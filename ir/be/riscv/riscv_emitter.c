/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "riscv_emitter.h"

#include <inttypes.h>

#include "be_t.h"
#include "bearch.h"
#include "beblocksched.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "besched.h"
#include "gen_riscv_new_nodes.h"
#include "gen_riscv_emitter.h"
#include "gen_riscv_regalloc_if.h"
#include "riscv_nodes_attr.h"
#include "riscv_bearch_t.h"
#include "util.h"

static void emit_immediate_val(char const *const prefix, ir_entity *const ent, int32_t const val)
{
	if (ent) {
		if (prefix)
			be_emit_irprintf("%s(", prefix);
		be_gas_emit_entity(ent);
		if (val != 0)
			be_emit_irprintf("%+" PRId32, val);
		if (prefix)
			be_emit_char(')');
	} else {
		be_emit_irprintf("%" PRId32, val);
	}
}

static void emit_immediate(char const *const prefix, ir_node const *const node)
{
	riscv_immediate_attr_t const *const imm = get_riscv_immediate_attr_const(node);
	emit_immediate_val(prefix, imm->ent, imm->val);
}

static void emit_register(arch_register_t const *const reg)
{
	be_emit_string(reg->name);
}

void riscv_emitf(ir_node const *const node, char const *fmt, ...)
{
	BE_EMITF(node, fmt, ap, false) {
		switch (*fmt++) {
		case 'A':
			emit_immediate("%lo", node);
			be_emit_char('(');
			emit_register(arch_get_irn_register_in(node, 1));
			be_emit_char(')');
			break;

		case 'C': {
			riscv_cond_t const cond = (riscv_cond_t)va_arg(ap, int);
			be_emit_string(riscv_get_cond_name(cond));
			break;
		}

		case 'D': {
			if (!is_digit(*fmt))
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			emit_register(arch_get_irn_register_out(node, pos));
			break;
		}

		case 'H': emit_immediate("%hi", node); break;
		case 'I': emit_immediate("%lo", node); break;
		case 'J': emit_immediate(NULL,  node); break;

		case 'R':
			emit_register(va_arg(ap, arch_register_t const*));
			break;

		case 'S': {
			if (!is_digit(*fmt))
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			emit_register(arch_get_irn_register_in(node, pos));
			break;
		}

		default:
unknown:
			panic("unknown format conversion");
		}
	}
}

static void emit_jmp(ir_node const *const node, ir_node const *const target)
{
	BE_EMIT_JMP(riscv, node, "j", target) {}
}

static void emit_riscv_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	be_asm_attr_t       const *const attr = get_be_asm_attr_const(node);
	riscv_asm_operand_t const *const op   = &((riscv_asm_operand_t const*)attr->operands)[pos];
	/* modifiers:
	 *   R: %lo of immediate
	 *   c: plain immediate
	 *   h: %hi of immediate
	 *   l: label only
	 *   z: print normally, except immediate 0 as 'zero' */
	if (!be_is_valid_asm_operand_kind(node, modifier, pos, op->op.kind, "z", "Rch", ""))
		return;

	switch (op->op.kind) {
	case BE_ASM_OPERAND_INVALID:
		panic("invalid asm operand");

	case BE_ASM_OPERAND_INPUT_VALUE:
		emit_register(arch_get_irn_register_in(node, op->op.pos));
		return;

	case BE_ASM_OPERAND_OUTPUT_VALUE:
		emit_register(arch_get_irn_register_out(node, op->op.pos));
		return;

	case BE_ASM_OPERAND_IMMEDIATE: {
		char const* prefix;
		switch (modifier) {
		case 'R': prefix = "%lo"; break;
		case 'h': prefix = "%hi"; break;

		case 'z':
			if (op->val == 0 && !op->ent) {
				emit_register(&riscv_registers[REG_ZERO]);
				return;
			} else {
				/* fallthrough */
		default:
				prefix = NULL;
				break;
			}
		}
		if (prefix)
			be_emit_irprintf("%s(", prefix);
		emit_immediate_val(NULL, op->ent, op->val);
		if (prefix)
			be_emit_char(')');
		return;
	}

	case BE_ASM_OPERAND_LABEL:
		be_emit_cfop_target_pos(node, op->op.pos);
		return;

	case BE_ASM_OPERAND_MEMORY:
		be_emit_char('(');
		emit_register(arch_get_irn_register_in(node, op->op.pos));
		be_emit_char(')');
		return;
	}
	panic("invalid asm operand kind");
}

static void emit_be_ASM(const ir_node *node)
{
	ir_node const *const fallthrough = be_emit_asm(node, &emit_riscv_asm_operand);
	if (fallthrough)
		emit_jmp(node, fallthrough);
}

static void emit_be_Copy(ir_node const *const node)
{
	ir_node               *const op  = be_get_Copy_op(node);
	arch_register_t const *const in  = arch_get_irn_register(op);
	arch_register_t const *const out = arch_get_irn_register(node);
	if (in == out)
		return;

	if (in->cls == &riscv_reg_classes[CLASS_riscv_gp]) {
		riscv_emitf(node, "mv\t%R, %R", out, in);
	} else {
		panic("unexpected register class");
	}
}

static void emit_be_IncSP(ir_node const *const node)
{
	int const offs = -be_get_IncSP_offset(node);
	if (offs == 0) {
		return;
	}
	assert(is_simm12(offs));
	riscv_emitf(node, "addi\t%D0, %S0, %d", offs);
}

static void emit_be_Perm(ir_node const *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	if (out->cls == &riscv_reg_classes[CLASS_riscv_gp]) {
		riscv_emitf(node,
			"xor\t%D0, %D0, %D1\n"
			"xor\t%D1, %D0, %D1\n"
			"xor\t%D0, %D0, %D1"
		);
	} else {
		panic("unexpected register class");
	}
}

static void emit_be_MemPerm(ir_node const *const node)
{
	/* TODO: this implementation is slower than necessary. */

	bool const omit_fp      = riscv_get_irg_data(get_irn_irg(node))->omit_fp;
	int const memperm_arity = be_get_MemPerm_entity_arity(node);
	int const max_arity     = 23;
	if (memperm_arity > max_arity)
		panic("memperm with more than %d inputs not supported yet", max_arity);

	char const *const frame_base = omit_fp ? "sp" : "fp";

	int const memperm_offset = be_get_MemPerm_offset(node);
	int ent_offset = memperm_offset;

	riscv_emitf(node, "addi\tsp, sp, -%d", memperm_arity * RISCV_REGISTER_SIZE);
	for (int i = 0; i < memperm_arity; ++i) {
		/* spill register */
		arch_register_t const *const reg = arch_register_for_index(&riscv_reg_classes[CLASS_riscv_gp], i + 9);

		riscv_emitf(node, "sw\t%s, %d(sp)", reg->name, i * RISCV_REGISTER_SIZE);

		/* load from entity */
		ir_entity *entity = be_get_MemPerm_in_entity(node, i);
		int        offset = get_entity_offset(entity) + ent_offset;
		if (omit_fp) {
			offset += (memperm_arity * RISCV_REGISTER_SIZE);
		}
		riscv_emitf(node, "lw\t%s, %d(%s)", reg->name, offset, frame_base);
		ent_offset += 4;
	}

	for (int i = memperm_arity; i-- > 0; ) {
		/* store to new entity */
		ent_offset -= 4;
		ir_entity *entity = be_get_MemPerm_out_entity(node, i);
		int        offset = get_entity_offset(entity) + ent_offset;
		if (omit_fp) {
			offset += (memperm_arity * RISCV_REGISTER_SIZE);
		}
		arch_register_t const *const reg = arch_register_for_index(&riscv_reg_classes[CLASS_riscv_gp], i + 9);
		riscv_emitf(node, "sw\t%s, %d(%s)", reg->name, offset, frame_base);
		/* restore register */
		riscv_emitf(node, "lw\t%s, %d(sp)", reg->name, i * RISCV_REGISTER_SIZE);
	}
	riscv_emitf(node, "addi\tsp, sp, %d", memperm_arity * RISCV_REGISTER_SIZE);
	assert(ent_offset == memperm_offset);
}

static void emit_riscv_bcc(ir_node const *const node)
{
	riscv_cond_t           const cond  = get_riscv_cond_attr_const(node)->cond;
	char            const *const fmt   = "b%C\t%S0, %S1, %L";
	be_cond_branch_projs_t const projs = be_get_cond_branch_projs(node);
	if (be_is_fallthrough(projs.t)) {
		riscv_emitf(node, fmt, riscv_negate_cond(cond), projs.f);
	} else {
		riscv_emitf(node, fmt, cond, projs.t);
		emit_jmp(node, projs.f);
	}
}

static void emit_riscv_j(ir_node const *const node)
{
	emit_jmp(node, node);
}

static void emit_jumptable_target(ir_entity const *const table, ir_node const *const proj_x)
{
  (void)table;
  be_emit_cfop_target(proj_x);
}

static void emit_riscv_switch(ir_node const *const node)
{
  riscv_emitf(node, "jr\t%S0");

  riscv_switch_attr_t const *const attr = get_riscv_switch_attr_const(node);
  be_emit_jump_table(node, &attr->swtch, mode_P, emit_jumptable_target);
}

static void emit_riscv_FrameAddr(const ir_node *node)
{
	const riscv_immediate_attr_t *attr   = get_riscv_immediate_attr_const(node);
	int32_t             offset = attr->val;

	assert(is_simm12((long)offset));
	riscv_emitf(node, "addi\t%D0, %S0, %d", (int)offset);
}

static void emit_riscv_SubSP(ir_node const *const node)
{
	riscv_emitf(node, "sub\t%D0, %S1, %S2");
	riscv_emitf(node, "mv\t%D1, %S1");
}

static void emit_riscv_SubSPimm(ir_node const *const node)
{
	const riscv_immediate_attr_t *attr   = get_riscv_immediate_attr_const(node);
	int32_t val = attr->val;
	assert(is_simm12((long)val));
	riscv_emitf(node, "addi\t%D0, %S1, %d", (int)val);
	riscv_emitf(node, "mv\t%D1, %S1");
}

static void riscv_register_emitters(void)
{
	be_init_emitters();
	riscv_register_spec_emitters();

	be_set_emitter(op_be_Asm,          emit_be_ASM);
	be_set_emitter(op_be_Copy,         emit_be_Copy);
	be_set_emitter(op_be_IncSP,        emit_be_IncSP);
	be_set_emitter(op_be_Perm,         emit_be_Perm);
	be_set_emitter(op_be_MemPerm,      emit_be_MemPerm);
	be_set_emitter(op_riscv_FrameAddr, emit_riscv_FrameAddr);
	be_set_emitter(op_riscv_bcc,       emit_riscv_bcc);
	be_set_emitter(op_riscv_j,         emit_riscv_j);
	be_set_emitter(op_riscv_SubSP,     emit_riscv_SubSP);
	be_set_emitter(op_riscv_SubSPimm,  emit_riscv_SubSPimm);
	be_set_emitter(op_riscv_switch,    emit_riscv_switch);
}

static void riscv_gen_block(ir_node *const block)
{
	be_gas_begin_block(block);
	sched_foreach(block, node) {
		be_emit_node(node);
	}
}

void riscv_emit_function(ir_graph *const irg)
{
	riscv_register_emitters();
	be_gas_elf_type_char = '@';

	ir_entity *const entity = get_irg_entity(irg);
	be_gas_emit_function_prolog(entity, 4, NULL);

	ir_node **const blk_sched = be_create_block_schedule(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	be_emit_init_cf_links(blk_sched);

	for (size_t i = 0, n_blocks = ARR_LEN(blk_sched); i != n_blocks; ++i) {
		riscv_gen_block(blk_sched[i]);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	be_gas_emit_function_epilog(entity);
}
