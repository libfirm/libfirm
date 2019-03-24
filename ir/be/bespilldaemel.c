/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Naive spilling algorithm
 * @author      Matthias Braun
 * @date        20.09.2005
 * @brief
 *   This implements a naive spilling algorithm. It is designed to produce
 *   similar effects to the spill decisions produced by traditional graph
 *   coloring register allocators that spill while they are coloring the graph.
 *
 *   This spiller walks over all blocks and looks for places with too high
 *   register pressure where it spills the values that are cheapest to spill.
 *   Spilling in this context means placing a spill instruction behind the
 *   definition of the value and a reload before each usage.
 */
#include "be_t.h"
#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespill.h"
#include "bespillutil.h"
#include "debug.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnodeset.h"
#include "irprintf.h"
#include "panic.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static spill_env_t                 *spill_env;
static unsigned                     n_regs;
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
	const spill_candidate_t *c1 = (const spill_candidate_t*)d1;
	const spill_candidate_t *c2 = (const spill_candidate_t*)d2;
	return (int) (c1->costs - c2->costs);
}

static double get_spill_costs(ir_node *node)
{
	ir_node *spill_place = skip_Proj(node);
	double   costs       = be_get_spill_costs(spill_env, node, spill_place);

	foreach_out_edge(node, edge) {
		/* keeps should be directly below the node */
		ir_node *use = get_edge_src_irn(edge);
		if (be_is_Keep(use))
			continue;

		if (is_Phi(use)) {
			int      in    = get_edge_src_pos(edge);
			ir_node *block = get_nodes_block(use);

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
static void spill_node(ir_node *node)
{
	DBG((dbg, LEVEL_3, "\tspilling %+F\n", node));

	foreach_out_edge(node, edge) {
		ir_node *use = get_edge_src_irn(edge);
		if (is_Anchor(use))
			continue;
		if (be_is_Keep(use))
			continue;

		/* Ignore CopyKeeps, except for the operand to copy. */
		if (be_is_CopyKeep(use) && get_edge_src_pos(edge) != n_be_CopyKeep_op)
			continue;

		if (is_Phi(use)) {
			int      in    = get_edge_src_pos(edge);
			ir_node *block = get_nodes_block(use);

			be_add_reload_on_edge(spill_env, node, block, in);
		} else {
			be_add_reload(spill_env, node, use);
		}
	}

	bitset_set(spilled_nodes, get_irn_idx(node));
}

static unsigned get_value_width(const ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	return req->width;
}

/**
 * spill @p n nodes from a nodeset. Removes the nodes from the nodeset and
 * sets the spilled bits in spilled_nodes.
 */
static void do_spilling(ir_nodeset_t *live_nodes, ir_node *node)
{
	size_t values_defined = 0;
	be_foreach_definition(node, cls, value, req,
		(void)value;
		assert(req->width >= 1);
		values_defined += req->width;
	);

	/* we need registers for the non-live argument values */
	size_t free_regs_needed = 0;
	be_foreach_use(node, cls, in_req_, use, pred_req_,
		if (!ir_nodeset_contains(live_nodes, use)) {
			free_regs_needed += get_value_width(use);
		}
	);

	/* we may need additional free registers */
	be_add_pressure_t const add_pressure = arch_get_additional_pressure(node, cls);
	free_regs_needed += MAX( add_pressure, 0);
	values_defined   += MAX(-add_pressure, 0);

	/* we can reuse all reloaded values for the defined values, but we might
	 * need even more registers */
	free_regs_needed = MAX(free_regs_needed, values_defined);

	size_t n_live_nodes  = ir_nodeset_size(live_nodes);
	int    spills_needed = (n_live_nodes + free_regs_needed) - n_regs;
	if (spills_needed <= 0)
		return;
	DBG((dbg, LEVEL_2, "\tspills needed after %+F: %d\n", node, spills_needed));

	spill_candidate_t *candidates = ALLOCAN(spill_candidate_t, n_live_nodes);

	/* construct array with spill candidates and calculate their costs */
	size_t c = 0;
	foreach_ir_nodeset(live_nodes, n, iter) {
		assert(!bitset_is_set(spilled_nodes, get_irn_idx(n)));

		spill_candidate_t *candidate = &candidates[c++];
		candidate->node  = n;
		candidate->costs = get_spill_costs(n);
	}
	assert(c == n_live_nodes);

	/* sort spill candidates */
	QSORT(candidates, n_live_nodes, compare_spill_candidates_desc);

	/* spill cheapest ones */
	size_t cand_idx = 0;
	while (spills_needed > 0) {
		if (cand_idx >= n_live_nodes)
			panic("can't spill enough values for node %+F", node);

		spill_candidate_t *candidate = &candidates[cand_idx];
		ir_node           *cand_node = candidate->node;
		++cand_idx;

		if (arch_irn_is(skip_Proj_const(cand_node), dont_spill))
			continue;

		/* make sure the node is not an argument of the instruction */
		bool is_use = false;
		foreach_irn_in(node, i, in) {
			if (in == cand_node) {
				is_use = true;
				break;
			}
		}
		if (is_use)
			continue;

		spill_node(cand_node);
		ir_nodeset_remove(live_nodes, cand_node);
		spills_needed -= get_value_width(cand_node);
	}
}

/**
 * removes all values from the nodeset that are defined by node
 */
static void remove_defs(ir_node *node, ir_nodeset_t *nodeset)
{
	/* You must break out of your loop when hitting the first phi function. */
	assert(!is_Phi(node));

	be_foreach_definition(node, cls, value, req,
		ir_nodeset_remove(nodeset, value);
	);
}

static void add_uses(ir_node *node, ir_nodeset_t *nodeset)
{
	foreach_irn_in(node, i, op) {
		if (arch_irn_consider_in_reg_alloc(cls, op)
		    && !bitset_is_set(spilled_nodes, get_irn_idx(op)))
			ir_nodeset_insert(nodeset, op);
	}
}

static __attribute__((unused))
void print_nodeset(ir_nodeset_t *nodeset)
{
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
	(void)data;
	DBG((dbg, LEVEL_1, "spilling block %+F\n", block));

	/* construct set of live nodes at end of block */
	ir_nodeset_t live_nodes;
	ir_nodeset_init(&live_nodes);
	be_liveness_end_of_block(lv, cls, block, &live_nodes);

	/* remove already spilled nodes from liveset */
	foreach_ir_nodeset(&live_nodes, node, iter) {
		DBG((dbg, LEVEL_2, "\t%+F is live-end... ", node));
		if (bitset_is_set(spilled_nodes, get_irn_idx(node))) {
			DBG((dbg, LEVEL_2, "but spilled; removing.\n"));
			ir_nodeset_remove_iterator(&live_nodes, &iter);
		} else {
			DBG((dbg, LEVEL_2, "keeping.\n"));
		}
	}

	/* walk schedule backwards and spill until register pressure is fine at
	 * each node */
	sched_foreach_non_phi_reverse(block, node) {
		remove_defs(node, &live_nodes);
		do_spilling(&live_nodes, node);
		add_uses(node, &live_nodes);
	}

	/* until now only the values of some phis have been spilled the phis itself
	 * are still there and occupy registers, so we need to count them and might
	 * have to spill some of them. */
	int n_phi_values_spilled = 0;
	sched_foreach_phi(block, node) {
		if (bitset_is_set(spilled_nodes, get_irn_idx(node)))
			n_phi_values_spilled += get_value_width(node);
	}

	int live_nodes_pressure = 0;
	foreach_ir_nodeset(&live_nodes, node, iter) {
		live_nodes_pressure += get_value_width(node);
	}

	/* calculate how many of the phis need to be spilled */
	int regpressure       = live_nodes_pressure + n_phi_values_spilled;
	int phi_spills_needed = regpressure - n_regs;
	DBG((dbg, LEVEL_3, "Regpressure before phis: %d phispills: %d\n",
	     regpressure, phi_spills_needed));

	/* spill as many phis as needed */
	/* TODO: we should really estimate costs of the phi spill as well...
	 * and preferably spill phis with lower costs... */
	sched_foreach_phi(block, node) {
		if (phi_spills_needed <= 0)
			break;

		if (!bitset_is_set(spilled_nodes, get_irn_idx(node)))
			continue;

		be_spill_phi(spill_env, node);
		phi_spills_needed -= get_value_width(node);
	}
	assert(phi_spills_needed <= 0);

	ir_nodeset_destroy(&live_nodes);
}

static void be_spill_daemel(ir_graph *irg, const arch_register_class_t *new_cls,
                            const regalloc_if_t *regif)
{
	n_regs = be_get_n_allocatable_regs(irg, new_cls);

	be_assure_live_sets(irg);

	spill_env     = be_new_spill_env(irg, regif);
	cls           = new_cls;
	lv            = be_get_irg_liveness(irg);
	spilled_nodes = bitset_malloc(get_irg_last_idx(irg));

	DBG((dbg, LEVEL_1, "*** RegClass %s\n", cls->name));

	irg_block_walk_graph(irg, spill_block, NULL, NULL);

	free(spilled_nodes);

	be_insert_spills_reloads(spill_env);
	be_delete_spill_env(spill_env);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_daemelspill)
void be_init_daemelspill(void)
{
	be_register_spiller("daemel", be_spill_daemel);
	FIRM_DBG_REGISTER(dbg, "firm.be.spilldaemel");
}
