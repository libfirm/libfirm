/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Peephole optimizations.
 */
#include "amd64_optimize.h"

#include "amd64_new_nodes.h"
#include "benode.h"
#include "bepeephole.h"
#include "besched.h"
#include "gen_amd64_regalloc_if.h"

static void peephole_amd64_mov_imm(ir_node *const node)
{
	if (be_peephole_get_value(REG_EFLAGS))
		return;

	amd64_movimm_attr_t const *const attr = get_amd64_movimm_attr_const(node);
	amd64_imm64_t       const *const imm  = &attr->immediate;
	if (imm->kind == X86_IMM_VALUE && imm->offset == 0) {
		/* mov $0, %reg -> xorl %reg, %reg */
		dbg_info              *const dbgi  = get_irn_dbg_info(node);
		ir_node               *const block = get_nodes_block(node);
		ir_node               *const xor   = new_bd_amd64_xor_0(dbgi, block);
		arch_register_t const *const reg   = arch_get_irn_register_out(node, pn_amd64_mov_imm_res);
		arch_set_irn_register_out(xor, pn_amd64_xor_0_res, reg);
		sched_add_before(node, xor);
		ir_node *const res = be_new_Proj(xor, pn_amd64_xor_0_res);
		be_peephole_exchange(node, res);
	}
}

void amd64_peephole_optimization(ir_graph *const irg)
{
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_amd64_mov_imm, peephole_amd64_mov_imm);
	be_peephole_opt(irg);
}
