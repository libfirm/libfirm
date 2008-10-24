/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @brief   Use the strong normal form theorem (though it does not hold)
 * @author  Christoph Mallon
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "height.h"
#include "irtools.h"
#include "irgwalk.h"
#include "benode_t.h"
#include "array_t.h"

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
	return !is_Proj(irn) && !is_Sync(irn);
}


static ir_node *normal_select(void *block_env, ir_nodeset_t *ready_set,
                              ir_nodeset_t *live_set)
{
	instance_t* inst = block_env;
	ir_node*    irn;
	ir_node*    next;
	ir_node*    last = NULL;
	ir_nodeset_iterator_t iter;

	(void)live_set;

	for (irn = inst->curr_list; irn != NULL; last = irn, irn = next) {
		next = get_irn_link(irn);
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

	ir_nodeset_iterator_init(&iter, ready_set);
	irn = ir_nodeset_iterator_next(&iter);
	return irn;
}


typedef struct irn_cost_pair {
	ir_node* irn;
	int      cost;
} irn_cost_pair;

static int cost_cmp(const void* a, const void* b)
{
	const irn_cost_pair* const a1 = a;
	const irn_cost_pair* const b1 = b;
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

	if (arch_get_register_req_out(irn)->type & arch_register_req_type_ignore)
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
		int            i;
		ir_node*       block = get_nodes_block(irn);

		fc = obstack_alloc(&inst->obst, sizeof(*fc) + sizeof(*fc->costs) * arity);
		fc->no_root = 0;
		costs = fc->costs;

		for (i = 0; i < arity; ++i) {
			ir_node* pred = get_irn_n(irn, i);
			int cost;

			if (is_Phi(irn) || get_irn_mode(pred) == mode_M || is_Block(pred)) {
				cost = 0;
			} else if (get_nodes_block(pred) != block) {
				cost = 1;
			} else {
				flag_and_cost* pred_fc;
				ir_node*       real_pred;

				cost = normal_tree_cost(pred, inst);
				if (be_is_Barrier(pred)) cost = 1; // XXX hack: the barrier causes all users to have a reguse of #regs
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
		if (op == last)                 continue;
		if (get_irn_mode(op) == mode_M) continue;
		if (arch_irn_is_ignore(op))     continue;
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
	instance_t *inst = env;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "cost walking node %+F\n", irn);
#endif
	if (is_Block(irn)) return;
	if (!must_be_scheduled(irn)) return;
	normal_tree_cost(irn, inst);
}


static void collect_roots(ir_node* irn, void* env)
{
	int is_root;

	(void)env;

	if (is_Block(irn)) return;
	if (!must_be_scheduled(irn)) return;

	is_root = be_is_Keep(irn) || !get_irn_fc(irn)->no_root;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "%+F is %sroot\n", irn, is_root ? "" : "no ");
#endif

	if (is_root) {
		ir_node* block = get_nodes_block(irn);
		ir_node** roots = get_irn_link(block);
		if (roots == NULL) {
			roots = NEW_ARR_F(ir_node*, 0);
		}
		ARR_APP1(ir_node*, roots, irn);
		set_irn_link(block, roots);
	}
}


static ir_node** sched_node(ir_node** sched, ir_node* irn)
{
	if (irn_visited_else_mark(irn)) return sched;
	if (is_End(irn))                return sched;

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
	const irn_cost_pair* const a1 = a;
	const irn_cost_pair* const b1 = b;
	int ret;
	if (is_irn_forking(a1->irn)) {
		ret = 1;
	} else if (is_irn_forking(b1->irn)) {
		ret = -1;
	} else {
		ret = b1->cost - a1->cost;
		if (ret == 0) {
			/* place live-out nodes later */
			ret = (count_result(a1->irn) != 0) - (count_result(b1->irn) != 0);
		}
	}
#if defined NORMAL_DBG
	ir_fprintf(stderr, "root %+F %s %+F\n", a1->irn, ret < 0 ? "<" : ret > 0 ? ">" : "=", b1->irn);
#endif
	return ret;
}


static void normal_sched_block(ir_node* block, void* env)
{
	ir_node**      roots = get_irn_link(block);
	heights_t*     heights = env;
	int            root_count;
	irn_cost_pair* root_costs;
	int i;
	ir_node**      sched;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "sched walking block %+F\n", block);
#endif

	if (roots == NULL) {
#if defined NORMAL_DBG
		fprintf(stderr, "has no roots\n");
#endif
		return;
	}

	root_count = ARR_LEN(roots);
	NEW_ARR_A(irn_cost_pair, root_costs, root_count);
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


static void *normal_init_graph(const list_sched_selector_t *vtab,
                               const be_irg_t *birg)
{
	instance_t* inst = XMALLOC(instance_t);
	ir_graph*   irg = be_get_birg_irg(birg);
	heights_t*  heights;

	(void)vtab;

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
	instance_t* inst  = graph_env;
	ir_node**   sched = get_irn_link(block);
	ir_node*    first = NULL;
	int         i;

	/* turn into a list, so we can easily remove nodes.
	   The link field is used anyway. */
	for (i = ARR_LEN(sched) - 1; i >= 0; --i) {
		ir_node* irn = sched[i];
		if (!arch_irn_class_is(irn, branch)) {
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

void normal_finish_graph(void *env)
{
	instance_t *inst = env;

	/* block uses the link field to store the schedule */
	ir_free_resources(inst->irg, IR_RESOURCE_IRN_LINK);
	obstack_free(&inst->obst, NULL);
	xfree(inst);
}

const list_sched_selector_t normal_selector = {
	normal_init_graph,
	normal_init_block,
	normal_select,
	NULL,              /* to_appear_in_schedule */
	NULL,              /* node_ready */
	NULL,              /* node_selected */
	NULL,              /* exectime */
	NULL,              /* latency */
	NULL,              /* finish_block */
	normal_finish_graph
};
