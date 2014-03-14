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
#include "irop_t.h"
#include "irnode_t.h"
#include "ircons.h"
#include "irgwalk.h"
#include "irgopt.h"

typedef struct cf_env {
	char ignore_exc_edges; /**< set if exception edges should be ignored. */
	char changed;          /**< flag indicates that the cf graphs has changed. */
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
	ir_node *block, *jmp;
	cf_env *cenv = (cf_env*)env;
	ir_graph *irg = get_irn_irg(n);

	/* Block has multiple predecessors */
	if (get_irn_arity(n) > 1) {
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
				 * TODO: What now, we can't split all critical edges because of this... */
				fprintf(stderr, "libfirm warning: Couldn't split all critical edges (compiler will probably fail now)\n");
				continue;
			}
			/* we don't want place nodes in the start block, so handle it like forking */
			if (is_op_forking(cfop) || cfop == op_Start) {
				/* Predecessor has multiple successors. Insert new control flow edge edges. */
insert:
				/* set predecessor of new block */
				block = new_r_Block(irg, 1, &pre);
				/* insert new jmp node to new block */
				jmp = new_r_Jmp(block);
				/* set successor of new block */
				set_irn_n(n, i, jmp);
				cenv->changed = 1;
			}
		}
	}
}

void remove_critical_cf_edges_ex(ir_graph *irg, int ignore_exception_edges)
{
	cf_env env;

	env.ignore_exc_edges = (char)ignore_exception_edges;
	env.changed          = 0;

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
	remove_critical_cf_edges_ex(irg, 1);
}
