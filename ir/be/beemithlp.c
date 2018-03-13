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

#include "be_t.h"
#include "bearch.h"
#include "bedwarf.h"
#include "beemitter.h"
#include "begnuas.h"
#include "benode.h"
#include "dbginfo.h"
#include "debug.h"
#include "firm_types.h"
#include "iredges.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "util.h"

static void be_emit_unknown(ir_node const *const node)
{
	if (be_options.verbose_asm) {
		arch_register_t const *const reg = arch_get_irn_register_out(node, 0);
		be_emit_irprintf("\t/* unknown in %s */", reg->name);
		be_emit_finish_line_gas(node);
	}
}

void be_init_emitters(void)
{
	ir_clear_opcodes_generic_func();
	be_set_emitter(op_Phi,        be_emit_nothing);
	be_set_emitter(op_be_Keep,    be_emit_nothing);
	be_set_emitter(op_be_Start,   be_emit_nothing);
	be_set_emitter(op_be_Unknown, be_emit_unknown);
	be_set_emitter(op_be_RegSplit, be_emit_nothing);
	be_set_emitter(op_be_RegJoin, be_emit_nothing);
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

void be_emit_init_cf_links(ir_node **const block_schedule)
{
	ir_node *prev = NULL;
	for (size_t i = 0, n = ARR_LEN(block_schedule); i < n; ++i) {
		ir_node *const block = block_schedule[i];

		/* Initialize cfop link */
		for (unsigned n = get_Block_n_cfgpreds(block); n-- > 0; ) {
			ir_node *pred = get_Block_cfgpred(block, n);
			set_irn_link(pred, block);
		}

		/* initialize pred block links */
		set_irn_link(block, prev);
		prev = block;
	}
}

be_cond_branch_projs_t be_get_cond_branch_projs(ir_node const *const node)
{
	be_cond_branch_projs_t projs = { NULL, NULL };
	foreach_out_edge(node, edge) {
		ir_node *const proj = get_edge_src_irn(edge);
		unsigned const pn   = get_Proj_num(proj);
		switch (pn) {
		case pn_Cond_false: projs.f = proj; continue;
		case pn_Cond_true:  projs.t = proj; continue;
		}
		panic("invalid Proj for branch");
	}
	assert(projs.f && projs.t);
	return projs;
}

void be_emit_cfop_target(ir_node const *const jmp)
{
	ir_node *const target = be_emit_get_cfop_target(jmp);
	be_gas_emit_block_name(target);
}

void be_emit_cfop_target_pos(ir_node const *const jmp, unsigned const pos)
{
	ir_node *const proj = get_Proj_for_pn(jmp, pos);
	be_emit_cfop_target(proj);
}

bool be_is_fallthrough(ir_node const *const jmp)
{
	ir_node *const block  = get_nodes_block(jmp);
	ir_node *const target = be_emit_get_cfop_target(jmp);
	return be_emit_get_prev_block(target) == block;
}
