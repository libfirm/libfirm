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
 * @file
 * @brief    Remove critical edges.
 * @author   Christian Schaefer, Goetz Lindenmaier, Sebastian Felis,
 *           Michael Beck
 * @version  $Id$
 */
#include "config.h"

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
static void walk_critical_cf_edges(ir_node *n, void *env) {
	int arity, i;
	ir_node *pre, *block, *jmp;
	cf_env *cenv = env;
	ir_graph *irg = get_irn_irg(n);

	/* Block has multiple predecessors */
	arity = get_irn_arity(n);
	if (arity > 1) {
		if (n == get_irg_end_block(irg))
			return;  /*  No use to add a block here.      */

		for (i = 0; i < arity; ++i) {
			const ir_op *cfop;

			pre = get_irn_n(n, i);
			/* don't count Bad's */
			if (is_Bad(pre))
				continue;

			cfop = get_irn_op(skip_Proj(pre));
			if (is_op_fragile(cfop)) {
				if (cenv->ignore_exc_edges && get_Proj_proj(pre) == pn_Generic_X_except)
					continue;
				goto insert;
			}
			if (is_IJmp(pre)) {
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
			} /* predecessor has multiple successors */
		} /* for all predecessors */
	} /* n is a multi-entry block */
}

void remove_critical_cf_edges_ex(ir_graph *irg, int ignore_exception_edges) {
	cf_env env;

	env.ignore_exc_edges = (char)ignore_exception_edges;
	env.changed          = 0;

	irg_block_walk_graph(irg, NULL, walk_critical_cf_edges, &env);
	if (env.changed) {
		/* control flow changed */
		set_irg_outs_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}
}

void remove_critical_cf_edges(ir_graph *irg) {
	remove_critical_cf_edges_ex(irg, 1);
}
