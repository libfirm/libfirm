/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Instrumentation of graphs.
 * @date    14.4.2008
 * @author  Michael Beck
 */
#include "irgraph_t.h"
#include "iredges.h"
#include "error.h"
#include "ircons.h"
#include "instrument.h"

void instrument_initcall(ir_graph *irg, ir_entity *ent)
{
	ir_node *initial_exec;
	ir_node *first_block = NULL;
	int      i, idx, need_new_block;

	assure_edges(irg);

	/* find the first block */
	initial_exec = get_irg_initial_exec(irg);

	foreach_out_edge(initial_exec, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		if (is_Block(succ)) {
			/* found the first block */
			first_block = succ;
			break;
		}
	}
	if (first_block == NULL) {
		panic("Cannot find first block of irg %+F", irg);
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
		set_Block_cfgpred(first_block, idx, new_r_Jmp(blk));
		first_block = blk;
	}

	/* place the call */
	ir_node *const adr         = new_r_Address(irg, mode_P_code, ent);
	ir_node *const initial_mem = get_irg_initial_mem(irg);
	ir_node *const call        = new_r_Call(first_block, initial_mem, adr, 0, NULL, get_entity_type(ent));
	ir_node *const new_mem     = new_r_Proj(call, mode_M, pn_Call_M);

	edges_reroute_except(initial_mem, new_mem, call);
	/* beware: reroute routes anchor edges also, revert this */
	set_irg_initial_mem(irg, initial_mem);
}
