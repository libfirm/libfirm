/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Helper functions for emitting assembly from a firm graph.
 * @author      Matthias Braun
 */
#include "beemithlp.h"

#include "bedwarf.h"
#include "beemitter.h"
#include "be_t.h"
#include "benode.h"
#include "dbginfo.h"
#include "debug.h"
#include "firm_types.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "util.h"

void be_init_emitters(void)
{
	ir_clear_opcodes_generic_func();
	be_set_emitter(op_Phi,      be_emit_nothing);
	be_set_emitter(op_be_Keep,  be_emit_nothing);
	be_set_emitter(op_be_Start, be_emit_nothing);
}

void be_emit_nothing(ir_node const *const node)
{
	(void)node;
}

void be_emit_node(ir_node const *const node)
{
	be_dwarf_location(get_irn_dbg_info(node));
	ir_op     *const op   = get_irn_op(node);
	emit_func *const emit = get_generic_function_ptr(emit_func, op);
	DEBUG_ONLY(if (!emit) panic("no emit handler for node %+F (%+G, graph %+F)", node, node, get_irn_irg(node));)
	emit(node);
}

void be_emit_pad_comment(void)
{
	size_t col = be_emit_get_column();
	col = MIN(col, 30);
	/* 34 spaces */
	be_emit_string_len("                                  ", 34 - col);
}
