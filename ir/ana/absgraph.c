/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @brief
 *
 * Abstract graph implementations for the CFG of a ir_graph.
 */
#include "irgraph_t.h"
#include "iredges_t.h"
#include "absgraph.h"

static void *irg_cfg_succ_get_root(void *self)
{
	ir_graph *irg = (ir_graph*) self;
	assure_edges_kind(irg, EDGE_KIND_BLOCK);
	return get_irg_start_block(irg);
}

static void *irg_cfg_succ_get_end(void *self)
{
	ir_graph *irg = (ir_graph*) self;
	return get_irg_end_block(irg);
}

static void irg_cfg_succ_grow_succs(void *self, void *node, struct obstack *obst)
{
	ir_node *bl = (ir_node*) node;

	(void)self;
	foreach_block_succ(bl, edge) {
		obstack_ptr_grow(obst, get_edge_src_irn(edge));
	}
}

const absgraph_t absgraph_irg_cfg_succ = {
	irg_cfg_succ_get_root,
	irg_cfg_succ_grow_succs,
	irg_cfg_succ_get_end
};

static void *irg_cfg_pred_get_root(void *self)
{
	return get_irg_end_block((ir_graph*) self);
}

static void *irg_cfg_pred_get_end(void *self)
{
	return get_irg_start_block((ir_graph*) self);
}

static void irg_cfg_pred_grow_succs(void *self, void *node, struct obstack *obst)
{
	(void)self;
	foreach_irn_in((ir_node*)node, i, pred) {
		obstack_ptr_grow(obst, pred);
	}
}

const absgraph_t absgraph_irg_cfg_pred = {
	irg_cfg_pred_get_root,
	irg_cfg_pred_grow_succs,
	irg_cfg_pred_get_end
};
