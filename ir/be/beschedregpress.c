/**
 * Regpressure node selector.
 * Originally implemented by Sebastian Hack.
 * @author Christian Wuerdig
 * @date   29.08.2006
 * @cvs-id $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "iredges_t.h"
#include "irgwalk.h"

#include "besched_t.h"
#include "belistsched.h"
#include "benode_t.h"


typedef struct _usage_stats_t {
	ir_node *irn;
	struct _usage_stats_t *next;
	int max_hops;
	int uses_in_block;      /**< Number of uses inside the current block. */
	int already_consumed;   /**< Number of insns using this value already
							scheduled. */
} usage_stats_t;

typedef struct {
	const list_sched_selector_t *vtab;
	const arch_env_t *arch_env;
} reg_pressure_main_env_t;

typedef struct {
	struct obstack obst;
	const reg_pressure_main_env_t *main_env;
	usage_stats_t *root;
	ir_nodeset_t already_scheduled;
} reg_pressure_selector_env_t;


#if 0
/*
* Ugly global variable for the compare function
* since qsort(3) does not pass an extra pointer.
*/
static ir_node *curr_bl = NULL;

static int cmp_usage(const void *a, const void *b)
{
	struct trivial_sched_env *env;
	const ir_node *p = a;
	const ir_node *q = b;
	int res = 0;

	res = is_live_end(env->curr_bl, a) - is_live_end(env->curr_bl, b);

	/*
	* One of them is live at the end of the block.
	* Then, that one shall be scheduled at after the other
	*/
	if(res != 0)
		return res;


	return res;
}
#endif

static INLINE usage_stats_t *get_or_set_usage_stats(reg_pressure_selector_env_t *env, ir_node *irn)
{
	usage_stats_t *us = get_irn_link(irn);

	if(!us) {
		us                   = obstack_alloc(&env->obst, sizeof(us[0]));
		us->irn              = irn;
		us->already_consumed = 0;
		us->max_hops         = INT_MAX;
		us->next             = env->root;
		env->root            = us;
		set_irn_link(irn, us);
	}

	return us;
}

static INLINE usage_stats_t *get_usage_stats(ir_node *irn)
{
	usage_stats_t *us = get_irn_link(irn);
	assert(us && "This node must have usage stats");
	return us;
}

static int max_hops_walker(reg_pressure_selector_env_t *env, ir_node *irn, ir_node *curr_bl, int depth, unsigned visited_nr)
{
	ir_node *bl = get_nodes_block(irn);
	/*
	* If the reached node is not in the block desired,
	* return the value passed for this situation.
	*/
	if(get_nodes_block(irn) != bl)
		return block_dominates(bl, curr_bl) ? 0 : INT_MAX;

	/*
	* If the node is in the current block but not
	* yet scheduled, we keep on searching from that node.
	*/
	if(!ir_nodeset_contains(&env->already_scheduled, irn)) {
		int i, n;
		int res = 0;
		for(i = 0, n = get_irn_ins_or_deps(irn); i < n; ++i) {
			ir_node *operand = get_irn_in_or_dep(irn, i);

			if(get_irn_visited(operand) < visited_nr) {
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

	const ir_edge_t *edge;

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

static void *reg_pressure_graph_init(const list_sched_selector_t *vtab, const arch_env_t *arch_env, ir_graph *irg)
{
	reg_pressure_main_env_t *main_env = xmalloc(sizeof(main_env[0]));

	main_env->arch_env = arch_env;
	main_env->vtab     = vtab;
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);

	return main_env;
}

static INLINE int must_appear_in_schedule(const list_sched_selector_t *sel, void *block_env, const ir_node *irn)
{
	int res = -1;

	if(sel->to_appear_in_schedule)
		res = sel->to_appear_in_schedule(block_env, irn);

	return res >= 0 ? res : (to_appear_in_schedule(irn) || be_is_Keep(irn) || be_is_CopyKeep(irn) || be_is_RegParams(irn));
}

static void *reg_pressure_block_init(void *graph_env, ir_node *bl)
{
	ir_node *irn;
	reg_pressure_selector_env_t *env  = xmalloc(sizeof(env[0]));

	obstack_init(&env->obst);
	ir_nodeset_init(&env->already_scheduled);
	env->root              = NULL;
	env->main_env          = graph_env;

	/*
	* Collect usage statistics.
	*/
	sched_foreach(bl, irn) {
		if(must_appear_in_schedule(env->main_env->vtab, env, irn)) {
			int i, n;

			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				//ir_node *op = get_irn_n(irn, i);
				if(must_appear_in_schedule(env->main_env->vtab, env, irn)) {
					usage_stats_t *us = get_or_set_usage_stats(env, irn);
#if 0 /* Liveness is not computed here! */
					if(is_live_end(bl, op))
						us->uses_in_block = 99999;
					else
#endif
						us->uses_in_block++;
				}
			}
		}
	}

	return env;
}

static void reg_pressure_block_free(void *block_env)
{
	reg_pressure_selector_env_t *env = block_env;
	usage_stats_t *us;

	for(us = env->root; us; us = us->next)
		set_irn_link(us->irn, NULL);

	obstack_free(&env->obst, NULL);
	ir_nodeset_destroy(&env->already_scheduled);
	free(env);
}

static int get_result_hops_sum(reg_pressure_selector_env_t *env, ir_node *irn)
{
	int res = 0;
	if(get_irn_mode(irn) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(irn, edge)
			res += get_result_hops_sum(env, get_edge_src_irn(edge));
	}

	else if(mode_is_data(get_irn_mode(irn)))
		res = compute_max_hops(env, irn);


	return res;
}

static INLINE int reg_pr_costs(reg_pressure_selector_env_t *env, ir_node *irn)
{
	int i, n;
	int sum = 0;

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(must_appear_in_schedule(env->main_env->vtab, env, op))
			sum += compute_max_hops(env, op);
	}

	sum += get_result_hops_sum(env, irn);

	return sum;
}

static ir_node *reg_pressure_select(void *block_env, ir_nodeset_t *ready_set,
                                    ir_nodeset_t *live_set)
{
	ir_nodeset_iterator_t iter;
	reg_pressure_selector_env_t *env = block_env;
	ir_node *irn, *res     = NULL;
	int curr_cost          = INT_MAX;

	assert(ir_nodeset_size(ready_set) > 0);

	ir_nodeset_iterator_init(&iter, ready_set);
	while( (irn = ir_nodeset_iterator_next(&iter)) != NULL) {
		/*
		Ignore branch instructions for the time being.
		They should only be scheduled if there is nothing else.
		*/
		if (! arch_irn_class_is(env->main_env->arch_env, irn, branch)) {
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

	if(!res) {
		ir_nodeset_iterator_init(&iter, ready_set);
		res = ir_nodeset_iterator_next(&iter);

		assert(res && "There must be a node scheduled.");
	}

	ir_nodeset_insert(&env->already_scheduled, res);
	return res;
}

static const list_sched_selector_t reg_pressure_selector_struct = {
	reg_pressure_graph_init,
	reg_pressure_block_init,
	reg_pressure_select,
	NULL,                    /* to_appear_in_schedule */
	NULL,                    /* node_ready */
	NULL,                    /* node_selected */
	NULL,                    /* exectime */
	NULL,                    /* latency */
	reg_pressure_block_free,
	free
};

const list_sched_selector_t *reg_pressure_selector = &reg_pressure_selector_struct;
