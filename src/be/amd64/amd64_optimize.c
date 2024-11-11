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
#include "amd64_transform.h"
#include "benode.h"
#include "bepeephole.h"
#include "besched.h"
#include "gen_amd64_regalloc_if.h"
#include "iredges_t.h"
#include "util.h"

static void peephole_amd64_cmp(ir_node *const node)
{
	/* cmp $0, %reg -> test %reg, %reg */
	amd64_binop_addr_attr_t const *const attr = get_amd64_binop_addr_attr_const(node);
	if (attr->base.base.op_mode == AMD64_OP_REG_IMM) {
		x86_imm32_t const *const imm = &attr->u.immediate;
		if (imm->kind == X86_IMM_VALUE && imm->offset == 0) {
			amd64_binop_addr_attr_t test_attr = *attr;
			test_attr.base.base.op_mode = AMD64_OP_REG_REG;

			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = get_nodes_block(node);
			ir_node  *const base  = get_irn_n(node, attr->base.addr.base_input);
			ir_node  *const in[]  = { base, base };
			ir_node  *const test  = new_bd_amd64_test(dbgi, block, ARRAY_SIZE(in), in, amd64_reg_reg_reqs, &test_attr);

			arch_register_t const *const oreg = arch_get_irn_register_out(node, pn_amd64_cmp_flags);
			arch_set_irn_register_out(test, pn_amd64_test_flags, oreg);

			be_peephole_replace(node, test);
		}
	}
}

static void make_add(ir_node *const node, size_t const n_in, ir_node *const *const in, arch_register_req_t const **const reqs, amd64_binop_addr_attr_t const *const attr, arch_register_t const *const oreg)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node  *const add   = new_bd_amd64_add(dbgi, block, n_in, in, reqs, attr);
	arch_set_irn_register_req_out(add, 0, &amd64_requirement_gp_same_0);
	sched_add_before(node, add);
	ir_node *const res = be_new_Proj_reg(add, pn_amd64_add_res, oreg);
	be_peephole_exchange(node, res);
}

static void peephole_amd64_lea(ir_node *const node)
{
	if (be_peephole_get_value(REG_EFLAGS))
		return;

	arch_register_t   const *const oreg = arch_get_irn_register_out(node, pn_amd64_lea_res);
	amd64_addr_attr_t const *const attr = get_amd64_addr_attr_const(node);
	x86_addr_t        const *const addr = &attr->addr;
	if (addr->variant == X86_ADDR_BASE) {
		/* lea c(%r), %r -> add $c, %r */
		ir_node *const base = get_irn_n(node, addr->base_input);
		if (oreg == arch_get_irn_register(base)) {
			amd64_binop_addr_attr_t const add_attr = {
				.base = {
					.base = {
						.op_mode = AMD64_OP_REG_IMM,
						.size    = attr->base.size,
					},
					.addr = {
						.base_input = 0,
						.variant    = X86_ADDR_REG,
					},
				},
				.u.immediate = addr->immediate,
			};
			ir_node *const in[] = { base };
			make_add(node, ARRAY_SIZE(in), in, reg_reqs, &add_attr, oreg);
		}
	} else if (addr->variant == X86_ADDR_BASE_INDEX && addr->log_scale == 0 && !addr->immediate.entity && addr->immediate.offset == 0) {
		ir_node       *l;
		ir_node       *r;
		ir_node *const base = get_irn_n(node, addr->base_input);
		ir_node *const idx  = get_irn_n(node, addr->index_input);
		if (oreg == arch_get_irn_register(base)) {
			/* lea (%r1, %r2), %r1 -> add %r2, %r1 */
			l = base;
			r = idx;
			goto add_reg_reg;
		} else if (oreg == arch_get_irn_register(idx)) {
			/* lea (%r1, %r2), %r2 -> add %r1, %r2 */
			l = idx;
			r = base;
add_reg_reg:;
			amd64_binop_addr_attr_t const add_attr = {
				.base = {
					.base = {
						.op_mode = AMD64_OP_REG_REG,
						.size    = attr->base.size,
					},
					.addr = {
						.base_input = 0,
						.variant    = X86_ADDR_REG,
					},
				},
			};
			ir_node *const in[] = { l, r };
			make_add(node, ARRAY_SIZE(in), in, amd64_reg_reg_reqs, &add_attr, oreg);
		}
	}
}

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
		ir_node               *const xor   = new_bd_amd64_xor_0(dbgi, block, X86_SIZE_32);
		arch_register_t const *const reg   = arch_get_irn_register_out(node, pn_amd64_mov_imm_res);
		arch_set_irn_register_out(xor, pn_amd64_xor_0_res, reg);
		sched_add_before(node, xor);
		ir_node *const res = be_new_Proj(xor, pn_amd64_xor_0_res);
		be_peephole_exchange(node, res);
	}
}

static void peephole_amd64_mov_gp(ir_node *const node)
{
	/*
	 * amd64_mov_gp instructions always perform a mov with zero extension (or the mov instruction overwrites the whole
	 * quadword anyway).
	 * This optimization removes unnecessary zero extensions, for example after a load:
	 * movzwl (%r1), %r2
	 * movzwl %r2, %r2    <-- remove this instruction
	 */
	ir_node *const pred = get_irn_n(node, 0);
	if (!is_Proj(pred)) {
		return;
	}
	amd64_addr_attr_t const *const attr = get_amd64_addr_attr_const(node);
	x86_addr_t        const *const addr = &attr->addr;
	if (addr->variant != X86_ADDR_REG) {
		return;
	}
	ir_node *const operand = get_Proj_pred(pred);
	if (is_amd64_mov_gp(operand)) {
		if (get_amd64_attr_const(node)->size < get_amd64_attr_const(operand)->size) {
			return;
		}
		arch_register_t const *const operand_out   = arch_get_irn_register_out(operand, pn_amd64_mov_gp_res);
		arch_register_t const *const node_out   = arch_get_irn_register_out(node, pn_amd64_mov_gp_res);
		arch_register_t const *const node_in   = arch_get_irn_register_in(node, 0);
		if (operand_out == node_out && node_in == node_out) {
			ir_node *const node_proj = be_get_or_make_Proj_for_pn(node, pn_amd64_mov_gp_res);
			be_peephole_exchange_using_proj(node_proj, pred);
		}
	}
}

static void peephole_amd64_cvtsi2sX(ir_node *const node)
{
	/**
	 * cvtsi2sd / cvtsi2ss instructions have a dependency on the destination register, as the upper part of the xmm
	 * register remains unmodified, but this dependency is not present in the amd64 backend.
	 * XORing the register to zero before the convert instruction breaks the
	 * dependency chain seen by the processor -> faster out-of-order execution
	 */
	arch_register_t const *const reg = arch_get_irn_register_out(node, pn_amd64_cvtsi2sd_res);
	dbg_info              *const dbgi  = get_irn_dbg_info(node);
	ir_node               *const block = get_nodes_block(node);
	ir_node *pxor = new_bd_amd64_pxor_0(dbgi, block, X86_SIZE_64);
	arch_set_irn_register_out(pxor, pn_amd64_pxor_0_res, reg);
	ir_node *keep = be_new_Keep_one(pxor);
	sched_add_before(node, pxor);
	sched_add_after(pxor, keep);
}

static void peephole_be_IncSP(ir_node *const node)
{
	be_peephole_IncSP_IncSP(node);
}

void amd64_peephole_optimization(ir_graph *const irg)
{
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_amd64_cmp,     peephole_amd64_cmp);
	register_peephole_optimization(op_amd64_lea,     peephole_amd64_lea);
	register_peephole_optimization(op_amd64_mov_imm, peephole_amd64_mov_imm);
	register_peephole_optimization(op_amd64_mov_gp,  peephole_amd64_mov_gp);
	register_peephole_optimization(op_amd64_cvtsi2sd, peephole_amd64_cvtsi2sX);
	register_peephole_optimization(op_amd64_cvtsi2ss, peephole_amd64_cvtsi2sX);
	register_peephole_optimization(op_be_IncSP,      peephole_be_IncSP);
	be_peephole_opt(irg);
}
