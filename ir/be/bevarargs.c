/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Default (pointer-based) implementation of variadic functions
 * @author  Andreas Fried
 */

#include "bevarargs.h"

#include "be.h"
#include "bitfiddle.h"
#include "ircons.h"
#include "irgmod.h"
#include "irnode_t.h"
#include "util.h"

void be_default_lower_va_arg(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_graph *irg   = get_irn_irg(node);

	ir_type *const aptype   = get_method_res_type(get_Builtin_type(node), 0);
	ir_node *const ap       = get_irn_n(node, 1);
	ir_node *const node_mem = get_Builtin_mem(node);

	ir_mode *apmode = get_type_mode(aptype);
	ir_node *res;
	ir_node *new_mem;
	if (apmode != NULL) {
		ir_node *const load = new_rd_Load(dbgi, block, node_mem, ap, apmode, aptype, cons_none);
		res     = new_r_Proj(load, apmode, pn_Load_res);
		new_mem = new_r_Proj(load, mode_M,pn_Load_M);
	} else {
		/* aptype has no associated mode, so it is represented as a pointer. */
		res     = ap;
		new_mem = node_mem;
	}

	backend_params const *const be_params = be_get_backend_param();
	unsigned       round_up    = round_up2(get_type_size_bytes(aptype), be_params->stack_param_align);
	ir_mode *const offset_mode = get_reference_offset_mode(mode_P);
	ir_node *const offset      = new_r_Const_long(irg, offset_mode, round_up);
	ir_node *const new_ap      = new_rd_Add(dbgi, block, ap, offset, mode_P);

	ir_node *const in[] = { new_mem, res, new_ap };
	turn_into_tuple(node, ARRAY_SIZE(in), in);
}
