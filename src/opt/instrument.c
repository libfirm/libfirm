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
#include "instrument.h"

#include "ircons.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include <stdbool.h>

void instrument_initcall(ir_graph *irg, ir_entity *ent)
{
	assure_edges(irg);

	/* place the call */
	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const adr         = new_r_Address(irg, ent);
	ir_node *const initial_mem = get_irg_initial_mem(irg);
	ir_node *const call        = new_r_Call(start_block, initial_mem, adr, 0,
	                                        NULL, get_entity_type(ent));
	ir_node *const new_mem     = new_r_Proj(call, mode_M, pn_Call_M);

	edges_reroute_except(initial_mem, new_mem, call);
	/* beware: reroute routes anchor edges also, revert this */
	set_irg_initial_mem(irg, initial_mem);
}
