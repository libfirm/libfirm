/**
 * @file   absgraph.c
 * @date   20.04.2007
 * @author Sebastian Hack
 *
 * Abstract graph implementations for the CFG of a ir_graph.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */

#include "irgraph_t.h"
#include "iredges_t.h"
#include "absgraph.h"

static void *irg_cfg_succ_get_root(void *self)
{
	ir_graph *irg = self;
	edges_activate_kind(irg, EDGE_KIND_BLOCK);
	return get_irg_start_block(irg);
}

static void irg_cfg_succ_grow_succs(void *self, void *node, struct obstack *obst)
{
	ir_node *bl = node;
	const ir_edge_t *edge;
	foreach_block_succ(bl, edge)
		obstack_ptr_grow(obst, get_edge_src_irn(edge));
}

const absgraph_t absgraph_irg_cfg_succ = {
	irg_cfg_succ_get_root,
	irg_cfg_succ_grow_succs
};

static void *irg_cfg_pred_get_root(void *self)
{
	return get_irg_end_block(self);
}

static void irg_cfg_pred_grow_succs(void *self, void *node, struct obstack *obst)
{
	int i, n;
	for (i = 0, n = get_irn_arity(node); i < n; ++i)
		obstack_ptr_grow(obst, get_irn_n(node, i));
}

const absgraph_t absgraph_irg_cfg_pred = {
	irg_cfg_pred_get_root,
	irg_cfg_pred_grow_succs
};
