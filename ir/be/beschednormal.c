/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @brief   Use the strong normal form theorem (though it does not hold)
 * @author  Christoph Mallon
 */
#include <stdlib.h>

#include "besched.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "heights.h"
#include "irgwalk.h"
#include "benode.h"
#include "bemodule.h"
#include "util.h"
#include "array.h"

// XXX there is no one time init for schedulers
//#define NORMAL_DBG
#include "irprintf.h"

/** An instance of the normal scheduler. */
typedef struct instance_t {
	ir_graph*      irg;          /**< the IR graph of this instance */
	struct obstack obst;         /**< obstack for temporary data */
	ir_node*       curr_list;    /**< current block schedule list */
} instance_t;

static int must_be_scheduled(const ir_node* const irn)
{
	return !is_Proj(irn) && !arch_irn_is(irn, not_scheduled);
}


static ir_node *normal_select(void *block_env, ir_nodeset_t *ready_set)
{
	instance_t* inst = (instance_t*)block_env;
	ir_node*    irn;
	ir_node*    next;
	ir_node*    last = NULL;

	for (irn = inst->curr_list; irn != NULL; last = irn, irn = next) {
		next = (ir_node*)get_irn_link(irn);
		if (ir_nodeset_contains(ready_set, irn)) {
#if defined NORMAL_DBG
			ir_fprintf(stderr, "scheduling %+F\n", irn);
#endif
			if (last == NULL)
				inst->curr_list = next;
			else
				set_irn_link(last, next);
			return irn;
		}
	}

	return ir_nodeset_first(ready_set);
}


typedef struct irn_cost_pair {
	ir_node* irn;
	int      cost;
} irn_cost_pair;

static int cost_cmp(const void* a, const void* b)
{
	const irn_cost_pair* const a1 = (const irn_cost_pair*)a;
	const irn_cost_pair* const b1 = (const irn_cost_pair*)b;
	int ret = b1->cost - a1->cost;
	if (ret == 0)
		ret = (int)get_irn_idx(a1->irn) - (int)get_irn_idx(b1->irn);
#if defined NORMAL_DBG
	ir_fprintf(stderr, "cost %+F %s %+F\n", a1->irn, ret < 0 ? "<" : ret > 0 ? ">" : "=", b1->irn);
#endif
	return ret;
}


typedef struct flag_and_cost {
	int no_root;
	irn_cost_pair costs[];
} flag_and_cost;

#define get_irn_fc(irn)     ((flag_and_cost*)get_irn_link(irn))
#define set_irn_fc(irn, fc) set_irn_link(irn, fc)


static int count_result(const ir_node* irn)
{
	const ir_mode* mode = get_irn_mode(irn);

	if (mode == mode_M || mode == mode_X)
		return 0;

	if (mode == mode_T)
		return 1;

	arch_register_req_t const *const req = arch_get_irn_register_req(irn);
	if (arch_register_req_is(req, ignore))
		return 0;

	return 1;
}


/* TODO high cost for store trees
 */

static int normal_tree_cost(ir_node* irn, instance_t *inst)
{
	flag_and_cost* fc;
	int            arity;
	ir_node*       last;
	int            n_res;
	int            cost;
	int            n_op_res = 0;
	int            i;

	if (be_is_Keep(irn))
		return 0;

	if (is_Proj(irn)) {
		return normal_tree_cost(get_Proj_pred(irn), inst);
	}

	arity = get_irn_arity(irn);
	fc    = get_irn_fc(irn);

	if (fc == NULL) {
		irn_cost_pair* costs;
		ir_node*       block = get_nodes_block(irn);

		fc = OALLOCF(&inst->obst, flag_and_cost, costs, arity);
		fc->no_root = 0;
		costs = fc->costs;

		for (i = 0; i < arity; ++i) {
			ir_node* pred = get_irn_n(irn, i);

			if (is_Phi(irn) || get_irn_mode(pred) == mode_M) {
				cost = 0;
			} else if (get_nodes_block(pred) != block) {
				cost = 1;
			} else {
				flag_and_cost* pred_fc;
				ir_node*       real_pred;

				cost = normal_tree_cost(pred, inst);
				if (!arch_irn_is_ignore(pred)) {
					real_pred = (is_Proj(pred) ? get_Proj_pred(pred) : pred);
					pred_fc = get_irn_fc(real_pred);
					pred_fc->no_root = 1;
#if defined NORMAL_DBG
					ir_fprintf(stderr, "%+F says that %+F is no root\n", irn, real_pred);
#endif
				}
			}

			costs[i].irn  = pred;
			costs[i].cost = cost;
		}

		qsort(costs, arity, sizeof(*costs), cost_cmp);
		set_irn_link(irn, fc);
	}

	cost = 0;
	last = 0;
	for (i = 0; i < arity; ++i) {
		ir_node* op = fc->costs[i].irn;
		ir_mode* mode;
		if (op == last)
			continue;
		mode = get_irn_mode(op);
		if (mode == mode_M)
			continue;
		if (arch_irn_is_ignore(op))
			continue;
		cost = MAX(fc->costs[i].cost + n_op_res, cost);
		last = op;
		++n_op_res;
	}
	n_res = count_result(irn);
	cost = MAX(n_res, cost);

#if defined NORMAL_DBG
	ir_fprintf(stderr, "reguse of %+F is %d\n", irn, cost);
#endif

	return cost;
}


static void normal_cost_walker(ir_node* irn, void* env)
{
	instance_t *inst = (instance_t*)env;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "cost walking node %+F\n", irn);
#endif
	if (is_Block(irn)) {
		ir_node **const roots = NEW_ARR_F(ir_node*, 0);
		set_irn_link(irn, roots);
		return;
	}
	if (!must_be_scheduled(irn)) return;
	normal_tree_cost(irn, inst);
}


static void collect_roots(ir_node* irn, void* env)
{
	int is_root;

	(void)env;

	if (!must_be_scheduled(irn)) return;

	is_root = be_is_Keep(irn) || !get_irn_fc(irn)->no_root;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "%+F is %sroot\n", irn, is_root ? "" : "no ");
#endif

	if (is_root) {
		ir_node* block = get_nodes_block(irn);
		ir_node** roots = (ir_node**)get_irn_link(block);
		ARR_APP1(ir_node*, roots, irn);
		set_irn_link(block, roots);
	}
}


static ir_node** sched_node(ir_node** sched, ir_node* irn)
{
	if (irn_visited_else_mark(irn)) return sched;

	if (!is_Phi(irn) && !be_is_Keep(irn)) {
		ir_node*       block = get_nodes_block(irn);
		int            arity = get_irn_arity(irn);
		flag_and_cost* fc    = get_irn_fc(irn);
		irn_cost_pair* irns  = fc->costs;
		int            i;

		for (i = 0; i < arity; ++i) {
			ir_node* pred = irns[i].irn;
			if (get_nodes_block(pred) != block) continue;
			if (get_irn_mode(pred) == mode_M) continue;
			if (is_Proj(pred)) pred = get_Proj_pred(pred);
			sched = sched_node(sched, pred);
		}
	}

	ARR_APP1(ir_node*, sched, irn);
	return sched;
}


static int root_cmp(const void* a, const void* b)
{
	const irn_cost_pair* const a1 = (const irn_cost_pair*)a;
	const irn_cost_pair* const b1 = (const irn_cost_pair*)b;
	int ret;
	if (is_irn_forking(a1->irn) && !is_irn_forking(b1->irn)) {
		ret = 1;
	} else if (is_irn_forking(b1->irn) && !is_irn_forking(a1->irn)) {
		ret = -1;
	} else {
		ret = b1->cost - a1->cost;
		if (ret == 0) {
			/* place live-out nodes later */
			ret = (count_result(a1->irn) != 0) - (count_result(b1->irn) != 0);
			if (ret == 0) {
				/* compare node idx */
				ret = get_irn_idx(a1->irn) - get_irn_idx(b1->irn);
			}
		}
	}
#if defined NORMAL_DBG
	ir_fprintf(stderr, "root %+F %s %+F\n", a1->irn, ret < 0 ? "<" : ret > 0 ? ">" : "=", b1->irn);
#endif
	return ret;
}


static void normal_sched_block(ir_node* block, void* env)
{
	ir_node**      roots = (ir_node**)get_irn_link(block);
	ir_heights_t*  heights = (ir_heights_t*)env;
	int i;
	ir_node**      sched;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "sched walking block %+F\n", block);
#endif

	int const root_count = ARR_LEN(roots);
	if (root_count == 0) {
#if defined NORMAL_DBG
		fprintf(stderr, "has no roots\n");
#endif
		return;
	}

	irn_cost_pair *root_costs = ALLOCAN(irn_cost_pair, root_count);
	for (i = 0; i < root_count; ++i) {
		root_costs[i].irn  = roots[i];
		root_costs[i].cost = get_irn_height(heights, roots[i]);
#if defined NORMAL_DBG
		ir_fprintf(stderr, "height of %+F is %u\n", roots[i], root_costs[i].cost);
#endif
	}
	qsort(root_costs, root_count, sizeof(*root_costs), root_cmp);
#if defined NORMAL_DBG
	{
		int n = root_count;
		int i;

		ir_fprintf(stderr, "Root Scheduling of %+F:\n", block);
		for (i = 0; i < n; ++i) {
			ir_fprintf(stderr, "  %+F\n", root_costs[i].irn);
		}
		fprintf(stderr, "\n");
	}
#endif

	sched = NEW_ARR_F(ir_node*, 0);
	for (i = 0; i < root_count; ++i) {
		ir_node* irn = root_costs[i].irn;
		assert(must_be_scheduled(irn));
		sched = sched_node(sched, irn);
	}
	set_irn_link(block, sched);
	DEL_ARR_F(roots);

#if defined NORMAL_DBG
	{
		int n = ARR_LEN(sched);
		int i;

		ir_fprintf(stderr, "Scheduling of %+F:\n", block);
		for (i = 0; i < n; ++i) {
			ir_fprintf(stderr, "  %+F\n", sched[i]);
		}
		fprintf(stderr, "\n");
	}
#endif
}


static void *normal_init_graph(ir_graph *irg)
{
	instance_t   *inst = XMALLOC(instance_t);
	ir_heights_t *heights;

	be_clear_links(irg);

	obstack_init(&inst->obst);
	inst->irg         = irg;

	heights = heights_new(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, normal_cost_walker,  NULL, inst);
	irg_walk_graph(irg, collect_roots, NULL, NULL);
	inc_irg_visited(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	irg_block_walk_graph(irg, normal_sched_block, NULL, heights);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);

	heights_free(heights);

	return inst;
}

static void *normal_init_block(void *graph_env, ir_node *block)
{
	instance_t* inst  = (instance_t*)graph_env;
	ir_node**   sched = (ir_node**)get_irn_link(block);
	ir_node*    first = NULL;
	int         i;

	/* turn into a list, so we can easily remove nodes.
	   The link field is used anyway. */
	for (i = ARR_LEN(sched) - 1; i >= 0; --i) {
		ir_node* irn = sched[i];
		if (!is_cfop(irn)) {
			set_irn_link(irn, first);
			first = irn;
		}
	}
	/* note: we can free sched here, there should be no attempt to schedule
	   a block twice */
	DEL_ARR_F(sched);
	set_irn_link(block, sched);
	inst->curr_list = first;
	return inst;
}

static void normal_finish_graph(void *env)
{
	instance_t *inst = (instance_t*)env;

	/* block uses the link field to store the schedule */
	ir_free_resources(inst->irg, IR_RESOURCE_IRN_LINK);
	obstack_free(&inst->obst, NULL);
	free(inst);
}

static void sched_normal(ir_graph *irg)
{
	static const list_sched_selector_t normal_selector = {
		normal_init_graph,
		normal_init_block,
		normal_select,
		NULL,              /* node_ready */
		NULL,              /* node_selected */
		NULL,              /* finish_block */
		normal_finish_graph
	};
	be_list_sched_graph(irg, &normal_selector);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_normal)
void be_init_sched_normal(void)
{
	be_register_scheduler("normal", sched_normal);
}
