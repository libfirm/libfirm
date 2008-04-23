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
 * @brief   Instrumentation of graphs.
 * @date    14.4.2008
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irgraph_t.h"
#include "iredges.h"
#include "error.h"
#include "ircons.h"
#include "instrument.h"

/**
 * Adds a Call at the beginning of the given irg.
 */
void instrument_initcall(ir_graph *irg, ir_entity *ent) {
	const ir_edge_t *edge;
	ir_node         *initial_exec;
	ir_node         *initial_mem;
	ir_node         *start_block;
	ir_node         *adr, *call, *new_mem;
	ir_node         *first_block = NULL;
	int             i, idx, need_new_block;
	symconst_symbol sym;

	edges_assure(irg);

	/* find the first block */
	initial_exec = get_irg_initial_exec(irg);
	start_block  = get_irg_start_block(irg);

	foreach_out_edge(initial_exec, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (succ != start_block && is_Block(succ)) {
			/* found the first block */
			first_block = succ;
			break;
		}
	}
	if (first_block == NULL) {
		panic("Cannot find first block of irg %+F\n", irg);
	}

	/* check if this block has only one predecessor */
	idx = -1;
	need_new_block = 0;
	for (i = get_Block_n_cfgpreds(first_block) - 1; i >= 0; --i) {
		ir_node *p = get_Block_cfgpred(first_block, i);

		if (is_Bad(p))
			continue;
		if (p == initial_exec)
			idx = i;
		else
			need_new_block = 1;
	}

	if (need_new_block) {
		ir_node *blk = new_r_Block(irg, 1, &initial_exec);
		set_Block_cfgpred(first_block, idx, new_r_Jmp(irg, blk));
		first_block = blk;
	}

	/* place the call */
	sym.entity_p = ent;
	adr = new_r_SymConst(irg, start_block, mode_P_code, sym, symconst_addr_ent);

	call    = new_r_Call(irg, first_block, get_irg_no_mem(irg), adr, 0, NULL, get_entity_type(ent));
	new_mem = new_r_Proj(irg, first_block, call, mode_M, pn_Call_M_regular);

	initial_mem = get_irg_initial_mem(irg);
	edges_reroute(initial_mem, new_mem, irg);
	/* beware: reroute routes anchor edges also, revert this */
	set_irg_initial_mem(irg, initial_mem);
	set_Call_mem(call, initial_mem);
}
