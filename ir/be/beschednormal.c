/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @version $Id: beschedrand.c 14604 2007-06-18 14:07:07Z matze $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "beutil.h"
#include "irtools.h"
#include "irgwalk.h"


// XXX there is no one time init for schedulers
//#define NORMAL_DBG


static const arch_env_t *cur_arch_env;


static ir_node *normal_select(void *block_env, ir_nodeset_t *ready_set,
                              ir_nodeset_t *live_set)
{
	ir_nodeset_iterator_t iter;
	ir_node*  block;
	ir_node*  irn;
	ir_node** sched;
	int sched_count;

	(void)block_env;
	(void)live_set;

	ir_nodeset_iterator_init(&iter, ready_set);
	irn = ir_nodeset_iterator_next(&iter);
	block = get_nodes_block(irn);
	sched = get_irn_link(block);
	sched_count = ARR_LEN(sched);
	for (; sched_count-- != 0; ++sched) {
		ir_node* irn = *sched;
		if (ir_nodeset_contains(ready_set, irn) &&
				!arch_irn_class_is(cur_arch_env, irn, branch)) {
#if defined NORMAL_DBG
			ir_fprintf(stderr, "scheduling %+F\n", irn);
#endif
			return irn;
		}
	}

	return irn;
}


typedef struct irn_cost_pair {
	ir_node* irn;
	int      cost;
} irn_cost_pair;


static int cost_cmp(const void* a, const void* b)
{
	const irn_cost_pair* a1 = a;
	const irn_cost_pair* b1 = b;
	return b1->cost - a1->cost;
}


typedef struct flag_and_cost {
	int no_root;
	irn_cost_pair costs[];
} flag_and_cost;


static int count_result(const ir_node* irn)
{
	const ir_mode* mode = get_irn_mode(irn);
	return
		mode != mode_M &&
		mode != mode_X &&
		!arch_irn_is(cur_arch_env, irn, ignore);
}


static int normal_tree_cost(ir_node* irn)
{
	flag_and_cost* fc    = get_irn_link(irn);
	ir_node*       block = get_nodes_block(irn);
	int            arity = get_irn_arity(irn);
	int            cost_max  = 0;
	int            count_max = 0;
	int            n_res;
	int            cost;
	int            n_op_res = 0;
	int            i;

	if (fc == NULL) {
		irn_cost_pair* costs;
		int            i;

		fc = malloc(sizeof(*fc) + sizeof(*fc->costs) * arity);
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

				cost = normal_tree_cost(pred);
				if (be_is_Barrier(pred)) cost = 1; // XXX hack: the barrier causes all users to have a reguse of #regs
				pred_fc = get_irn_link(pred);
				pred_fc->no_root = 1;
#if defined NORMAL_DBG
				ir_fprintf(stderr, "%+F says that %+F is no root\n", irn, pred);
#endif
			}

			costs[i].irn  = pred;
			costs[i].cost = cost;

			if (cost > cost_max) {
				cost_max  = cost;
				count_max = 1;
			} else if (cost == cost_max) {
				++count_max;
			}
		}

		qsort(costs, arity, sizeof(*costs), cost_cmp);
		set_irn_link(irn, fc);
	} else {
		irn_cost_pair* costs = fc->costs;
		int            i;

		if (arity > 0) {
			cost_max = costs[0].cost;

			for (i = 0; i < arity; ++i) {
				if (costs[i].cost < cost_max) break;
				++count_max;
			}
		}
	}

	cost = 0;
	for (i = 0; i < arity; ++i) {
		if (get_irn_mode(fc->costs[i].irn) == mode_M) continue;
		if (arch_irn_is(cur_arch_env, fc->costs[i].irn, ignore)) continue;
		cost = MAX(fc->costs[i].cost + n_op_res, cost);
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
	(void)env;

#if defined NORMAL_DBG
	ir_fprintf(stderr, "cost walking node %+F\n", irn);
#endif
	if (is_Block(irn)) return;
	normal_tree_cost(irn);
}


static void collect_roots(ir_node* irn, void* env)
{
	flag_and_cost* fc;

	(void)env;

	if (is_Block(irn)) return;

	fc = get_irn_link(irn);

#if defined NORMAL_DBG
	ir_fprintf(stderr, "%+F is %sroot\n", irn, fc->no_root ? "no " : "");
#endif

	if (!fc->no_root) {
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
	ir_node*       block = get_nodes_block(irn);
	flag_and_cost* fc    = get_irn_link(irn);
	irn_cost_pair* irns  = fc->costs;
	int            arity = get_irn_arity(irn);
	int            i;

	if (irn_visited(irn)) return sched;

	if (is_End(irn)) return sched;

	if (!is_Phi(irn)) {
		for (i = 0; i < arity; ++i) {
			ir_node* pred = irns[i].irn;
			if (get_nodes_block(pred) != block) continue;
			if (get_irn_mode(pred) == mode_M) continue;
			sched = sched_node(sched, pred);
		}
	}

	mark_irn_visited(irn);
	ARR_APP1(ir_node*, sched, irn);
	return sched;
}


static void normal_sched_block(ir_node* block, void* env)
{
	ir_node** roots = get_irn_link(block);
	int            root_count;
	irn_cost_pair* root_costs;
	int i;
	ir_node**      sched;

	(void)env;

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
		root_costs[i].cost = normal_tree_cost(roots[i]);
	}
	qsort(root_costs, root_count, sizeof(*root_costs), cost_cmp);

	sched = NEW_ARR_F(ir_node*, 0);
	for (i = 0; i < root_count; ++i) {
		ir_node* irn = root_costs[i].irn;
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
	ir_graph* irg = be_get_birg_irg(birg);

	(void)vtab;

	cur_arch_env = be_get_birg_arch_env(birg);

	be_clear_links(irg);

	irg_walk_graph(irg, normal_cost_walker,  NULL, NULL);
	irg_walk_graph(irg, collect_roots, NULL, NULL);
	inc_irg_visited(irg);
	irg_block_walk_graph(irg, normal_sched_block, NULL, NULL);

	return NULL;
}


static void *normal_init_block(void *graph_env, ir_node *block)
{
	(void)graph_env;
	(void)block;

	return NULL;
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
	NULL               /* finish_graph */
};
