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
#include "util.h"

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
		ir_node               *const xor   = new_bd_amd64_xor_0(dbgi, block, INSN_SIZE_32);
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
	register_peephole_optimization(op_amd64_lea,     peephole_amd64_lea);
	register_peephole_optimization(op_amd64_mov_imm, peephole_amd64_mov_imm);
	be_peephole_opt(irg);
}
