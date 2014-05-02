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
#include <stdbool.h>

#include "ircons.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irop_t.h"

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
static void walk_critical_cf_edges(ir_node *n, void *env)
{
	cf_env *cenv = (cf_env*)env;

	if (get_irn_arity(n) <= 1)
		return;

	ir_graph *irg = get_irn_irg(n);
	if (n == get_irg_end_block(irg))
		return;  /*  No use to add a block here.      */

	foreach_irn_in(n, i, pre) {
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
		/* we don't want to place nodes in the start block, so handle it like
		 * forking */
		if (is_op_forking(cfop) || cfop == op_Start) {
			/* Predecessor has multiple successors. Insert new control flow
			 * edge edges. */
insert:;
			/* set predecessor of new block */
			ir_node *block = new_r_Block(irg, 1, &pre);
			/* insert new jmp node to new block */
			ir_node *jmp = new_r_Jmp(block);
			/* set successor of new block */
			set_irn_n(n, i, jmp);
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
