/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Remove critical edges.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 */
#include "ircons.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irop_t.h"
#include <stdbool.h>

typedef struct cf_env {
	bool ignore_exc_edges; /**< set if exception edges should be ignored. */
	bool changed;          /**< indicate that the cf graph has changed. */
} cf_env;

/**
 * Called by walker of remove_critical_cf_edges().
 *
 * Place an empty block to an edge between a blocks of multiple
 * predecessors and a block of multiple successors.
 *
 * @param n   IR node
 * @param env Environment of walker.
 */
static void walk_critical_cf_edges(ir_node *block, void *env)
{
	if (get_Block_n_cfgpreds(block) <= 1)
		return;

	ir_graph *irg = get_irn_irg(block);
	if (block == get_irg_end_block(irg))
		return;

	cf_env *cenv = (cf_env*)env;
	foreach_irn_in(block, i, pre) {
		/* don't count Bad's */
		if (is_Bad(pre))
			continue;

		const ir_op *const cfop = get_irn_op(skip_Proj(pre));
		if (is_op_fragile(cfop)) {
			if (cenv->ignore_exc_edges && is_x_except_Proj(pre))
				continue;
			goto insert;
		}
		if (is_unknown_jump(pre)) {
			/* we can't add blocks in between ijmp and its destinations
			 * TODO: What now, we can't split all critical edges because of
			 * this... */
			fprintf(stderr, "libfirm warning: Couldn't split all critical edges (compiler will probably fail now)\n");
			continue;
		}
		/* Insert critical edge if predecessor has multiple successors. Also
		 * ensure that Start jumps into a block with exactly 1 predecessor. */
		if (is_op_forking(cfop)) {
			/* Predecessor has multiple successors. Insert new control flow
			 * edge edges. */
insert:;
			/* set predecessor of new block */
			ir_node *new_block = new_r_Block(irg, 1, &pre);
			/* insert new jmp node to new block */
			ir_node *jmp = new_r_Jmp(new_block);
			/* set successor of new block */
			set_irn_n(block, i, jmp);
			cenv->changed = true;
		}
	}
}

void remove_critical_cf_edges_ex(ir_graph *irg, int ignore_exception_edges)
{
	cf_env env;
	env.ignore_exc_edges = ignore_exception_edges;
	env.changed          = false;

	irg_block_walk_graph(irg, NULL, walk_critical_cf_edges, &env);
	if (env.changed) {
		/* control flow changed */
		clear_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL
			& ~(IR_GRAPH_PROPERTY_ONE_RETURN
				| IR_GRAPH_PROPERTY_MANY_RETURNS));
	}
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES);
}

void remove_critical_cf_edges(ir_graph *irg)
{
	remove_critical_cf_edges_ex(irg, true);
}
