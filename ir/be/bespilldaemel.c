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

typedef struct daemel_env_t daemel_env_t;
struct daemel_env_t {
	spill_env_t                 *spill_env;
	int                          n_regs;
	const arch_env_t            *arch_env;
	const arch_register_class_t *cls;
	const be_lv_t               *lv;
	bitset_t                    *spilled_nodes;
};

typedef struct spill_candidate_t spill_candidate_t;
struct spill_candidate_t {
	double   costs;
	ir_node *node;
};

static
int compare_spill_candidates_desc(const void *d1, const void *d2)
{
	const spill_candidate_t *c1 = d1;
	const spill_candidate_t *c2 = d2;

	return (int) (c1->costs - c2->costs);
}

static
double get_spill_costs(daemel_env_t *env, ir_node *node)
{
	const ir_edge_t *edge;
	spill_env_t     *spill_env = env->spill_env;
	double           costs     = be_get_spill_costs(spill_env, node, node);

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			costs += be_get_reload_costs_on_edge(spill_env, node, block, in);
		} else {
			costs += be_get_reload_costs(spill_env, node, use);
		}
	}

	return costs;
}

/**
 * spills a node by placing a reload before each usage
 */
static
void spill_node(daemel_env_t *env, ir_node *node, ir_nodeset_t *nodes)
{
	const ir_edge_t *edge;
	spill_env_t     *spill_env       = env->spill_env;
	const arch_register_class_t *cls = env->cls;

	DBG((dbg, LEVEL_3, "\tspilling %+F\n", node));

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);

		if(is_Phi(use)) {
			int      in         = get_edge_src_pos(edge);
			ir_node *block      = get_nodes_block(use);

			be_add_reload_on_edge(spill_env, node, block, in, cls, 1);
		} else if(!be_is_Keep(use)) {
			be_add_reload(spill_env, node, use, cls, 1);
		}
	}

	bitset_set(env->spilled_nodes, get_irn_idx(node));
	ir_nodeset_remove(nodes, node);
}

/**
 * spill @p n nodes from a nodeset. Removes the nodes from the nodeset and
 * sets the spilled bits in env->spilled_nodes.
 */
static
void do_spilling(daemel_env_t *env, ir_nodeset_t *nodes, ir_node *node)
{
	size_t                       node_count         = ir_nodeset_size(nodes);
	size_t                       additional_defines = 0;
	size_t                       reload_values      = 0;
	int                          registers          = env->n_regs;
	const arch_env_t            *arch_env           = env->arch_env;
	const arch_register_class_t *cls                = env->cls;
	spill_candidate_t           *candidates;
	ir_nodeset_iterator_t        iter;
	size_t                       i, arity;
	int                          spills_needed;
	size_t                       cand_idx;
	ir_node                     *n;
	const bitset_t              *spilled_nodes = env->spilled_nodes;

	/* mode_T nodes define several values at once. Count them */
	if(get_irn_mode(node) == mode_T) {
		ir_node *proj  = sched_next(node);

		while(is_Proj(proj)) {
			if(arch_irn_consider_in_reg_alloc(arch_env, cls, proj)) {
				++additional_defines;
			}
			proj = sched_next(proj);
		}
	}

	/* we might temporarily need registers for reloaded values */
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		if(bitset_is_set(spilled_nodes, get_irn_idx(pred)))
			++reload_values;
	}

	if(reload_values > additional_defines)
		additional_defines = reload_values;

	spills_needed = (node_count + additional_defines) - registers;
	if(spills_needed <= 0)
		return;
	DBG((dbg, LEVEL_2, "\tspills needed after %+F: %d\n", node, spills_needed));

	candidates = malloc(node_count * sizeof(candidates[0]));

	/* construct array with spill candidates and calculate their costs */
	i = 0;
	foreach_ir_nodeset(nodes, n, iter) {
		spill_candidate_t *candidate = & candidates[i];

		candidate->node  = n;
		candidate->costs = get_spill_costs(env, n);
		++i;
	}
	assert(i == node_count);

	/* sort spill candidates */
	qsort(candidates, node_count, sizeof(candidates[0]),
	      compare_spill_candidates_desc);

	/* spill cheapest ones */
	cand_idx = 0;
	while(spills_needed > 0) {
		spill_candidate_t *candidate = &candidates[cand_idx];
		ir_node           *cand_node = candidate->node;
		int                is_use;
		++cand_idx;

		if(cand_idx >= node_count) {
			panic("can't spill enough values for node %+F\n", node);
		}

		/* make sure the node is not a use of the instruction */
		is_use = 0;
		for(i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			if(in == cand_node) {
				is_use = 1;
				break;
			}
		}
		if(is_use) {
			continue;
		}

		spill_node(env, cand_node, nodes);
		--spills_needed;
	}

	free(candidates);
}

/**
 * similar to be_liveness_transfer.
 * custom liveness transfer function, that doesn't place already spilled values
 * into the liveness set
 */
static
void liveness_transfer(daemel_env_t *env, ir_node *node, ir_nodeset_t *nodeset)
{
	int i, arity;
	const arch_register_class_t *cls      = env->cls;
	const arch_env_t            *arch_env = env->arch_env;
	const bitset_t              *bitset   = env->spilled_nodes;

	/* You should better break out of your loop when hitting the first phi
	 * function. */
	assert(!is_Phi(node) && "liveness_transfer produces invalid results for phi nodes");

    if(arch_irn_consider_in_reg_alloc(arch_env, cls, node)) {
        ir_nodeset_remove(nodeset, node);
    }

    arity = get_irn_arity(node);
    for(i = 0; i < arity; ++i) {
        ir_node *op = get_irn_n(node, i);

        if(arch_irn_consider_in_reg_alloc(arch_env, cls, op)
		   && !bitset_is_set(bitset, get_irn_idx(op))) {
            ir_nodeset_insert(nodeset, op);
		}
    }
}

/**
 * make sure register pressure in a block is always equal or below the number
 * of available registers
 */
static
void spill_block(ir_node *block, void *data)
{
	daemel_env_t                *env           = data;
	const arch_env_t            *arch_env      = env->arch_env;
	const arch_register_class_t *cls           = env->cls;
	const be_lv_t               *lv            = env->lv;
	ir_nodeset_t                 live_nodes;
	ir_nodeset_iterator_t        iter;
	ir_node                     *node;
	bitset_t                    *spilled_nodes = env->spilled_nodes;
	int                          phi_count;

	DBG((dbg, LEVEL_1, "spilling block %+F\n", block));

	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block_ir_nodeset(lv, arch_env, cls, block, &live_nodes);

	foreach_ir_nodeset(&live_nodes, node, iter) {
		DBG((dbg, LEVEL_2, "\t%+F is live-in... ", node));
		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			DBG((dbg, LEVEL_2, "but spilled; removing.\n"));
		} else {
			DBG((dbg, LEVEL_2, "keeping.\n"));
		}
	}

	sched_foreach_reverse(block, node) {
		if(is_Phi(node))
			break;

		if(is_Proj(node) || be_is_Keep(node)) {
			liveness_transfer(env, node, &live_nodes);
			continue;
		}

		do_spilling(env, &live_nodes, node);
		liveness_transfer(env, node, &live_nodes);
	}
	do_spilling(env, &live_nodes, node);

	phi_count = 0;
	int spilled_phis = 0;
	sched_foreach(block, node) {
		if(!is_Phi(node))
			break;

		++phi_count;
		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			++spilled_phis;
		}
	}
	int regpressure       = ir_nodeset_size(&live_nodes) - spilled_phis;
	int phi_spills_needed = regpressure - env->n_regs;
	sched_foreach(block, node) {
		if(!is_Phi(node))
			break;
		if(phi_spills_needed <= 0)
			break;

		if(bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			be_spill_phi(env->spill_env, node);
			--phi_spills_needed;
		}
	}
	assert(phi_spills_needed <= 0);

	ir_nodeset_destroy(&live_nodes);
}

void be_spill_daemel(be_irg_t *birg, const arch_register_class_t *cls)
{
	daemel_env_t  env;
	ir_graph     *irg    = be_get_birg_irg(birg);
	int           n_regs = cls->n_regs - be_put_ignore_regs(birg, cls, NULL);

	if(n_regs == 0)
		return;

	be_invalidate_liveness(birg);
	be_assure_liveness(birg);

	env.spill_env     = be_new_spill_env(birg);
	env.n_regs        = n_regs;
	env.arch_env      = be_get_birg_arch_env(birg);
	env.cls           = cls;
	env.lv            = be_get_birg_liveness(birg);
	env.spilled_nodes = bitset_malloc(get_irg_last_idx(irg));

	irg_block_walk_graph(irg, spill_block, NULL, &env);

	bitset_free(env.spilled_nodes);

	be_insert_spills_reloads(env.spill_env);

	be_delete_spill_env(env.spill_env);
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
