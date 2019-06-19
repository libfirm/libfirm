/*
 * This file is part of libFirm.
 * Copyright (C) 2018 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Peephole optimization and legalization of a RISC-V function
 * @author   Johannes Bucher
 */
#include "riscv_bearch_t.h"


#include "besched.h"
#include "gen_riscv_new_nodes.h"
#include "be2addr.h"
#include "beirg.h"
#include "bepeephole.h"
#include "riscv_transform.h"
#include "gen_riscv_regalloc_if.h"


/**
 * Creates a constant from an immediate value.
 */
static ir_node *create_constant_from_immediate(ir_node *node, int val)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);

	riscv_hi_lo_imm imm = calc_hi_lo(val);
	ir_node      *res;
	if (imm.hi != 0) {
		res = new_bd_riscv_lui(dbgi, block, NULL, imm.hi);
		arch_set_irn_register(res, &riscv_registers[REG_T0]);
		sched_add_before(node, res);
	} else {
		ir_graph *const irg = get_irn_irg(node);
		res = get_Start_zero(irg);
	}
	if (imm.lo != 0) {
		res = new_bd_riscv_addi(dbgi, block, res, NULL, imm.lo);
		arch_set_irn_register(res, &riscv_registers[REG_T0]);
		sched_add_before(node, res);
	}
	return res;
}

/**
 * Adjust sp-relative offsets.
 *
 * Split into multiple instructions if offset exceeds RISC-V 12 bit immediate range.
 */
static void finish_riscv_FrameAddr(ir_node *node)
{
	riscv_immediate_attr_t *attr   = get_riscv_immediate_attr(node);
	int           offset = attr->val;

	if (!is_simm12(offset)) {
		ir_node               *base          = get_irn_n(node, n_riscv_FrameAddr_base);
		dbg_info              *dbgi          = get_irn_dbg_info(node);
		ir_node               *block         = get_nodes_block(node);
		ir_node               *constant      = create_constant_from_immediate(node, offset);
		ir_node               *new_frameaddr = new_bd_riscv_add(dbgi, block, base, constant);
		const arch_register_t *reg           = arch_get_irn_register(node);

		arch_set_irn_register(new_frameaddr, reg);
		be_peephole_replace(node, new_frameaddr);
	}
}

/**
 * The address offset immediate at load and store instructions is limited to 12 bit.
 * Adds an additional lui and add nodes to support large offsets.
 */
static void finish_riscv_load_store_offsets(ir_node *node) {
	riscv_immediate_attr_t *const imm = get_riscv_immediate_attr(node);
	if (!is_simm12(imm->val)) {
		dbg_info *dbgi = get_irn_dbg_info(node);
		ir_node *block = get_nodes_block(node);
		ir_node *res;

		riscv_hi_lo_imm hi_lo_imm = calc_hi_lo(imm->val);
		if (hi_lo_imm.hi != 0) {
			ir_node *lui = new_bd_riscv_lui(dbgi, block, NULL, hi_lo_imm.hi);
			arch_set_irn_register(lui, &riscv_registers[REG_T0]);
			sched_add_before(node, lui);
			ir_node *const base = get_irn_n(node, n_riscv_sw_base);
			res = new_bd_riscv_add(dbgi, block, base, lui);
			arch_set_irn_register(res, &riscv_registers[REG_T0]);
			sched_add_before(node, res);
		} else {
			res = node;
		}
		set_irn_n(node, n_riscv_sw_base, res);
		imm->val = hi_lo_imm.lo;
	}
}

/**
 * RISC-V immediates are limited. Split IncSP with bigger immediates if necessary.
 */
static void finish_be_IncSP(ir_node *node)
{
	int offset = -be_get_IncSP_offset(node);

	/* we might have to break the IncSP apart if the constant has become too big */
	if (!is_simm12(offset)) {
		ir_node  *sp       = be_get_IncSP_pred(node);
		dbg_info *dbgi     = get_irn_dbg_info(node);
		ir_node  *block    = get_nodes_block(node);
		ir_node  *constant = create_constant_from_immediate(node, offset);
		ir_node  *add      = new_bd_riscv_add(dbgi, block, sp, constant);

		arch_set_irn_register(add, &riscv_registers[REG_SP]);
		be_peephole_replace(node, add);
	}
}

static void peephole_be_IncSP(ir_node *const node)
{
	be_peephole_IncSP_IncSP(node);
}

/**
 * Optimize consecutive shift immediate operations by combining them in a single operation
 */
static void peephole_riscv_shift(ir_node *const node)
{
	int opcode = get_riscv_irn_opcode(node);
	ir_node *const pred = get_irn_n(node, n_riscv_srli_left);
	if (!is_riscv_irn(pred) || !be_has_only_one_user(pred) || sched_prev(node) != pred) {
		return;
	}
	if (get_riscv_irn_opcode(pred) == opcode) {
		riscv_immediate_attr_t *const node_attr_imm = get_riscv_immediate_attr(node);
		riscv_immediate_attr_t *const pred_attr_imm = get_riscv_immediate_attr(pred);
		if (!is_uimm5(node_attr_imm->val + pred_attr_imm->val)) {
			return;
		}
		int new_val = node_attr_imm->val + pred_attr_imm->val;

		dbg_info *dbgi  = get_irn_dbg_info(node);
		ir_node  *block = get_nodes_block(node);
		ir_node  *left  = get_irn_n(pred, n_riscv_srli_left);
		ir_node  *new;
		switch (opcode) {
			case iro_riscv_srli:
				new = new_bd_riscv_srli(dbgi, block, left, NULL, new_val);
				break;
			case iro_riscv_srai:
				new = new_bd_riscv_srai(dbgi, block, left, NULL, new_val);
				break;
			case iro_riscv_slli:
				new = new_bd_riscv_slli(dbgi, block, left, NULL, new_val);
				break;
			default:
				return;
		}
		arch_set_irn_register(new, arch_get_irn_register(node));
		be_peephole_replace(node, new);
		sched_remove(pred);
	}
}

void riscv_finish_graph(ir_graph *irg)
{
	/* perform peephole optimizations */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_be_IncSP, peephole_be_IncSP);
	register_peephole_optimization(op_riscv_srli, peephole_riscv_shift);
	register_peephole_optimization(op_riscv_srai, peephole_riscv_shift);
	register_peephole_optimization(op_riscv_slli, peephole_riscv_shift);
	be_peephole_opt(irg);

	/* perform legalizations (mostly fix nodes with too big immediates) */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_riscv_FrameAddr, finish_riscv_FrameAddr);
	register_peephole_optimization(op_riscv_lb, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_lbu, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_lh, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_lhu, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_lw, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_sb, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_sh, finish_riscv_load_store_offsets);
	register_peephole_optimization(op_riscv_sw, finish_riscv_load_store_offsets);

	register_peephole_optimization(op_be_IncSP, finish_be_IncSP);
	be_peephole_opt(irg);
}
