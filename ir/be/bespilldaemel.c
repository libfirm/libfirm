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
 * @file
 * @brief       Naiv spilling algorithm
 * @author      Matthias Braun
 * @date        20.09.2005
 * @version     $Id: bespillbelady.c 13913 2007-05-18 12:48:56Z matze $
 * @summary
 *   This implements a naiv spilling algorithm. It is design to produce similar
 *   effects to the spill decisions produced by traditional graph coloring
 *   register allocators that spill while they are coloring the graph.
 *
 *   This spiller walks over all blocks and looks for places with too high
 *   register pressure where it spills the values that are cheapest to spill.
 *   Spilling in this context means placing a spill instruction behind the
 *   definition of the value and a reload before each usage.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "debug.h"

#include "irnodeset.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "iredges_t.h"
#include "error.h"

#include "beirg.h"
#include "bespilloptions.h"
#include "bespill.h"
#include "bemodule.h"
#include "besched.h"
#include "bearch_t.h"
#include "be_t.h"
#include "benode_t.h"
#include "beirg.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static spill_env_t                 *spill_env;
static int                          n_regs;
static const arch_env_t            *arch_env;
static const arch_register_class_t *cls;
static const be_lv_t               *lv;
static bitset_t                    *spilled_nodes;

typedef struct spill_candidate_t spill_candidate_t;
struct spill_candidate_t {
	double   costs;
	ir_node *node;
};

static int compare_spill_candidates_desc(const void *d1, const void *d2)
{
	const spill_candidate_t *c1 = d1;
	const spill_candidate_t *c2 = d2;

	return (int) (c1->costs - c2->costs);
}

static double get_spill_costs(ir_node *node)
{
	const ir_edge_t *edge;
	ir_node         *spill_place = skip_Proj(node);
	double           costs       = be_get_spill_costs(spill_env, node,
	                                                  spill_place);

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		/* keeps should be directly below the node */
		if(be_is_Keep(use)) {
			continue;
		}

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			costs += be_get_reload_costs_on_edge(spill_env, node, block, in);
		} else {
			costs += be_get_reload_costs(spill_env, node, use);
		}
	}

	/* TODO cache costs? */

	return costs;
}

/**
 * spills a node by placing a reload before each usage
 */
static void spill_node(ir_node *node)
{
	const ir_edge_t *edge;

	DBG((dbg, LEVEL_3, "\tspilling %+F\n", node));

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);
		if(is_Anchor(use))
			continue;
		if(be_is_Keep(use))
			continue;

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			be_add_reload_on_edge(spill_env, node, block, in, cls, 1);
		} else {
			be_add_reload(spill_env, node, use, cls, 1);
		}
	}

	bitset_set(spilled_nodes, get_irn_idx(node));
}

/**
 * spill @p n nodes from a nodeset. Removes the nodes from the nodeset and
 * sets the spilled bits in spilled_nodes.
 */
static void do_spilling(ir_nodeset_t *live_nodes, ir_node *node)
{
	size_t                 n_live_nodes     = ir_nodeset_size(live_nodes);
	size_t                 values_defined   = 0;
	size_t                 free_regs_needed = 0;
	spill_candidate_t     *candidates;
	ir_nodeset_iterator_t  iter;
	size_t                 i, arity;
	int                    spills_needed;
	size_t                 cand_idx;
	ir_node               *n;

	/* mode_T nodes define several values at once. Count them */
	if(get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			const ir_node *proj = get_edge_src_irn(edge);

			if(arch_irn_consider_in_reg_alloc(arch_env, cls, proj)) {
				++values_defined;
			}
		}
	} else if(arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
		++values_defined;
	}

	/* we need registers for the non-live argument values */
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		if(arch_irn_consider_in_reg_alloc(arch_env, cls, pred)
				&& !ir_nodeset_contains(live_nodes, pred)) {
			++free_regs_needed;
		}
	}

	/* we can reuse all reloaded values for the defined values, but we might
	   need even more registers */
	if(values_defined > free_regs_needed)
		free_regs_needed = values_defined;

	spills_needed = (n_live_nodes + free_regs_needed) - n_regs;
	if(spills_needed <= 0)
		return;
	DBG((dbg, LEVEL_2, "\tspills needed after %+F: %d\n", node, spills_needed));

	candidates = alloca(n_live_nodes * sizeof(candidates[0]));

	/* construct array with spill candidates and calculate their costs */
	i = 0;
	foreach_ir_nodeset(live_nodes, n, iter) {
		spill_candidate_t *candidate = & candidates[i];

		assert(!bitset_is_set(spilled_nodes, get_irn_idx(n)));

		candidate->node  = n;
		candidate->costs = get_spill_costs(n);
		++i;
	}
	assert(i == n_live_nodes);

	/* sort spill candidates */
	qsort(candidates, n_live_nodes, sizeof(candidates[0]),
	      compare_spill_candidates_desc);

	/* spill cheapest ones */
	cand_idx = 0;
	while(spills_needed > 0) {
		spill_candidate_t *candidate;
		ir_node           *cand_node;
		int               is_use;

		if (cand_idx >= n_live_nodes) {
			panic("can't spill enough values for node %+F\n", node);
		}


		candidate = &candidates[cand_idx];
		cand_node = candidate->node;
		++cand_idx;

		if(arch_irn_is(arch_env, cand_node, dont_spill))
			continue;

		/* make sure the node is not an argument of the instruction */
		is_use = 0;
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			if(in == cand_node) {
				is_use = 1;
				break;
			}
		}
		if(is_use) {
			continue;
		}

		spill_node(cand_node);
		ir_nodeset_remove(live_nodes, cand_node);
		--spills_needed;
	}
}

/**
 * removes all values from the nodeset that are defined by node
 */
static void remove_defs(ir_node *node, ir_nodeset_t *nodeset)
{
	/* You should better break out of your loop when hitting the first phi
	 * function. */
	assert(!is_Phi(node) && "liveness_transfer produces invalid results for phi nodes");

	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			const ir_node *proj = get_edge_src_irn(edge);

			if (arch_irn_consider_in_reg_alloc(arch_env, cls, proj)) {
				ir_nodeset_remove(nodeset, proj);
			}
		}
	}

    if(arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
        ir_nodeset_remove(nodeset, node);
    }
}

static void add_uses(ir_node *node, ir_nodeset_t *nodeset)
{
	int i, arity;

    arity = get_irn_arity(node);
    for(i = 0; i < arity; ++i) {
        ir_node *op = get_irn_n(node, i);

        if(arch_irn_consider_in_reg_alloc(arch_env, cls, op)
		   && !bitset_is_set(spilled_nodes, get_irn_idx(op))) {
            ir_nodeset_insert(nodeset, op);
		}
    }
}

static __attribute__((unused))
void print_nodeset(ir_nodeset_t *nodeset)
{
	ir_nodeset_iterator_t  iter;
	ir_node               *node;

	foreach_ir_nodeset(nodeset, node, iter) {
		ir_fprintf(stderr, "%+F ", node);
	}
	fprintf(stderr, "\n");
}

/**
 * make sure register pressure in a block is always equal or below the number
 * of available registers
 */
static void spill_block(ir_node *block, void *data)
{
	ir_nodeset_t                 live_nodes;
	ir_nodeset_iterator_t        iter;
	ir_node                     *node;
	int                          n_phi_values_spilled;
	int                          regpressure;
	int                          phi_spills_needed;
	(void) data;

	DBG((dbg, LEVEL_1, "spilling block %+F\n", block));

	/* construct set of live nodes at end of block */
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, arch_env, cls, block, &live_nodes);

	/* remove already spilled nodes from liveset */
	foreach_ir_nodeset(&live_nodes, node, iter) {
		DBG((dbg, LEVEL_2, "\t%+F is live-end... ", node));
		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			DBG((dbg, LEVEL_2, "but spilled; removing.\n"));
			ir_nodeset_remove_iterator(&live_nodes, &iter);
		} else {
			DBG((dbg, LEVEL_2, "keeping.\n"));
		}
	}

	/* walk schedule backwards and spill until register pressure is fine at
	 * each node */
	sched_foreach_reverse(block, node) {
		if(is_Phi(node))
			break;

		remove_defs(node, &live_nodes);
		do_spilling(&live_nodes, node);
		add_uses(node, &live_nodes);
	}

	/* until now only the values of some phis have been spilled the phis itself
	 * are still there and occupy registers, so we need to count them and might
	 * have to spill some of them.
	 */
	n_phi_values_spilled = 0;
	sched_foreach(block, node) {
		if(!is_Phi(node))
			break;

		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			++n_phi_values_spilled;
		}
	}

	/* calculate how many of the phis need to be spilled */
	regpressure       = ir_nodeset_size(&live_nodes) + n_phi_values_spilled;
	phi_spills_needed = regpressure - n_regs;
	DBG((dbg, LEVEL_3, "Regpressure before phis: %d phispills: %d\n",
	     regpressure, phi_spills_needed));

	/* spill as many phis as needed */
	/* TODO: we should really estimate costs of the phi spill as well...
	 * and preferably spill phis with lower costs... */
	sched_foreach(block, node) {
		if(!is_Phi(node))
			break;
		if(phi_spills_needed <= 0)
			break;

		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			be_spill_phi(spill_env, node);
			--phi_spills_needed;
		}
	}
	assert(phi_spills_needed <= 0);

	ir_nodeset_destroy(&live_nodes);
}

void be_spill_daemel(be_irg_t *birg, const arch_register_class_t *new_cls)
{
	ir_graph     *irg    = be_get_birg_irg(birg);
	n_regs = new_cls->n_regs - be_put_ignore_regs(birg, new_cls, NULL);

	if(n_regs == 0)
		return;

	be_liveness_assure_sets(be_assure_liveness(birg));

	spill_env     = be_new_spill_env(birg);
	arch_env      = be_get_birg_arch_env(birg);
	cls           = new_cls;
	lv            = be_get_birg_liveness(birg);
	spilled_nodes = bitset_malloc(get_irg_last_idx(irg));

	DBG((dbg, LEVEL_1, "*** RegClass %s\n", cls->name));

	irg_block_walk_graph(irg, spill_block, NULL, NULL);

	bitset_free(spilled_nodes);
	spilled_nodes = NULL;

	be_insert_spills_reloads(spill_env);

	be_delete_spill_env(spill_env);
	spill_env = NULL;
}

void be_init_daemelspill(void)
{
	static be_spiller_t daemel_spiller = {
		be_spill_daemel
	};

	be_register_spiller("daemel", &daemel_spiller);
	FIRM_DBG_REGISTER(dbg, "ir.be.spilldaemel");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_doedelspill);
