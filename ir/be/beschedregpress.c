/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Register pressure node selector.
 * @author      Sebastian Hack
 * @date        29.08.2006
 */
#include <stdlib.h>

#include "iredges_t.h"
#include "irgwalk.h"
#include "irtools.h"
#include "util.h"

#include "besched.h"
#include "belistsched.h"
#include "benode.h"
#include "bemodule.h"


typedef struct usage_stats_t {
	ir_node *irn;
	struct usage_stats_t *next;
	int max_hops;
	int uses_in_block;      /**< Number of uses inside the current block. */
	int already_consumed;   /**< Number of insns using this value already
							scheduled. */
} usage_stats_t;

typedef struct {
	struct obstack obst;
	usage_stats_t *root;
	ir_nodeset_t already_scheduled;
} reg_pressure_selector_env_t;


static inline usage_stats_t *get_or_set_usage_stats(reg_pressure_selector_env_t *env, ir_node *irn)
{
	usage_stats_t *us = (usage_stats_t*)get_irn_link(irn);

	if (!us) {
		us                   = OALLOC(&env->obst, usage_stats_t);
		us->irn              = irn;
		us->already_consumed = 0;
		us->max_hops         = INT_MAX;
		us->next             = env->root;
		env->root            = us;
		set_irn_link(irn, us);
	}

	return us;
}

static int max_hops_walker(reg_pressure_selector_env_t *env, ir_node *irn, ir_node *curr_bl, int depth, unsigned visited_nr)
{
	ir_node *bl = get_nodes_block(irn);
	/*
	* If the reached node is not in the block desired,
	* return the value passed for this situation.
	*/
	if (get_nodes_block(irn) != bl)
		return block_dominates(bl, curr_bl) ? 0 : INT_MAX;

	/*
	* If the node is in the current block but not
	* yet scheduled, we keep on searching from that node.
	*/
	if (!ir_nodeset_contains(&env->already_scheduled, irn)) {
		int i, n;
		int res = 0;
		for (i = 0, n = get_irn_ins_or_deps(irn); i < n; ++i) {
			ir_node *operand = get_irn_in_or_dep(irn, i);

			if (get_irn_visited(operand) < visited_nr) {
				int tmp;

				set_irn_visited(operand, visited_nr);
				tmp = max_hops_walker(env, operand, bl, depth + 1, visited_nr);
				res = MAX(tmp, res);
			}
		}

		return res;
	}

	/*
	* If the node is in the current block and scheduled, return
	* the depth which indicates the number of steps to the
	* region of scheduled nodes.
	*/
	return depth;
}

static int compute_max_hops(reg_pressure_selector_env_t *env, ir_node *irn)
{
	ir_node *bl   = get_nodes_block(irn);
	ir_graph *irg = get_irn_irg(bl);
	int res       = 0;

	foreach_out_edge(irn, edge) {
		ir_node *user       = get_edge_src_irn(edge);
		unsigned visited_nr = get_irg_visited(irg) + 1;
		int max_hops;

		set_irg_visited(irg, visited_nr);
		max_hops = max_hops_walker(env, user, irn, 0, visited_nr);
		res      = MAX(res, max_hops);
	}

	return res;
}

static void *reg_pressure_graph_init(ir_graph *irg)
{
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);

	return NULL;
}

static void *reg_pressure_block_init(void *graph_env, ir_node *bl)
{
	reg_pressure_selector_env_t *env = XMALLOC(reg_pressure_selector_env_t);
	(void) graph_env;

	obstack_init(&env->obst);
	ir_nodeset_init(&env->already_scheduled);
	env->root              = NULL;

	/*
	* Collect usage statistics.
	*/
	sched_foreach(bl, irn) {
		for (int i = 0, n = get_irn_arity(irn); i < n; ++i) {
			usage_stats_t *us = get_or_set_usage_stats(env, irn);
			us->uses_in_block++;
		}
	}

	return env;
}

static void reg_pressure_block_free(void *block_env)
{
	reg_pressure_selector_env_t *env = (reg_pressure_selector_env_t*)block_env;
	usage_stats_t *us;

	for (us = env->root; us; us = us->next)
		set_irn_link(us->irn, NULL);

	obstack_free(&env->obst, NULL);
	ir_nodeset_destroy(&env->already_scheduled);
	free(env);
}

static int get_result_hops_sum(reg_pressure_selector_env_t *env, ir_node *irn)
{
	int res = 0;
	if (get_irn_mode(irn) == mode_T) {
		foreach_out_edge(irn, edge)
			res += get_result_hops_sum(env, get_edge_src_irn(edge));
	}

	else if (mode_is_data(get_irn_mode(irn)))
		res = compute_max_hops(env, irn);


	return res;
}

static inline int reg_pr_costs(reg_pressure_selector_env_t *env, ir_node *irn)
{
	int sum = 0;

	foreach_irn_in(irn, i, op) {
		if (is_Proj(op)
		    || (arch_get_irn_flags(op) & arch_irn_flag_not_scheduled))
			continue;

		sum += compute_max_hops(env, op);
	}

	sum += get_result_hops_sum(env, irn);

	return sum;
}

static ir_node *reg_pressure_select(void *block_env, ir_nodeset_t *ready_set)
{
	reg_pressure_selector_env_t *env = (reg_pressure_selector_env_t*)block_env;
	ir_node *res       = NULL;
	int      curr_cost = INT_MAX;

	assert(ir_nodeset_size(ready_set) > 0);

	foreach_ir_nodeset(ready_set, irn, iter) {
		/*
		Ignore branch instructions for the time being.
		They should only be scheduled if there is nothing else.
		*/
		if (!is_cfop(irn)) {
			int costs = reg_pr_costs(env, irn);
			if (costs <= curr_cost) {
				res       = irn;
				curr_cost = costs;
			}
		}
	}

	/*
	There was no result so we only saw a branch.
	Take it and finish.
	*/

	if (!res) {
		res = ir_nodeset_first(ready_set);
		assert(res && "There must be a node scheduled.");
	}

	ir_nodeset_insert(&env->already_scheduled, res);
	return res;
}

static void sched_reg_pressure(ir_graph *irg)
{
	static const list_sched_selector_t reg_pressure_selector = {
		reg_pressure_graph_init,
		reg_pressure_block_init,
		reg_pressure_select,
		NULL,                    /* node_ready */
		NULL,                    /* node_selected */
		reg_pressure_block_free,
		free
	};
	be_list_sched_graph(irg, &reg_pressure_selector);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_regpress)
void be_init_sched_regpress(void)
{
	be_register_scheduler("regpress", sched_reg_pressure);
}
