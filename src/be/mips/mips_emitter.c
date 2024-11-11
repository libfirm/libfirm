/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_emitter.h"

#include "be_t.h"
#include "bearch.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "benode.h"
#include "besched.h"
#include "gen_mips_emitter.h"
#include "gen_mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "mips_nodes_attr.h"
#include "panic.h"
#include "util.h"
#include <inttypes.h>

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
	mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(node);
	emit_immediate_val(prefix, imm->ent, imm->val);
}

static void emit_register(arch_register_t const *const reg)
{
	be_emit_char('$');
	be_emit_string(reg->name);
}

void mips_emitf(ir_node const *const node, char const *fmt, ...)
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
			mips_cond_t const cond = (mips_cond_t)va_arg(ap, int);
			be_emit_string(mips_get_cond_name(cond));
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

static void emit_mips_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	be_asm_attr_t      const *const attr = get_be_asm_attr_const(node);
	mips_asm_operand_t const *const op   = &((mips_asm_operand_t const*)attr->operands)[pos];
	/* modifiers:
	 *   c: plain immediate
	 *   l: label only
	 *   z: print normally, except immediate 0 as '$zero' */
	if (!be_is_valid_asm_operand_kind(node, modifier, pos, op->op.kind, "z", "c", ""))
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

	case BE_ASM_OPERAND_IMMEDIATE:
		if (modifier == 'z' && op->val == 0 && !op->ent)
			emit_register(&mips_registers[REG_ZERO]);
		else
			emit_immediate_val(NULL, op->ent, op->val);
		return;

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

static void emit_jmp(ir_node const *const node, ir_node const *const target)
{
	BE_EMIT_JMP(mips, node, "b", target) {
		mips_emitf(NULL, "nop");
	}
}

static void emit_be_ASM(const ir_node *node)
{
	ir_node const *const fallthrough = be_emit_asm(node, &emit_mips_asm_operand);
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

	if (in->cls == &mips_reg_classes[CLASS_mips_gp]) {
		mips_emitf(node, "move\t%R, %R", out, in);
	} else {
		panic("unexpected register class");
	}
}

static void emit_be_IncSP(ir_node const *const node)
{
	int const offs = -be_get_IncSP_offset(node);
	mips_emitf(node, "addiu\t%D0, %S0, %d", offs);
}

static void emit_be_Perm(ir_node const *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	if (out->cls == &mips_reg_classes[CLASS_mips_gp]) {
		mips_emitf(node,
			"xor\t%D0, %D0, %D1\n"
			"xor\t%D1, %D0, %D1\n"
			"xor\t%D0, %D0, %D1"
		);
	} else {
		panic("unexpected register class");
	}
}

static void emit_mips_b(ir_node const *const node)
{
	emit_jmp(node, node);
}

static void emit_mips_bcc(ir_node const *const node)
{
	mips_cond_t            const cond  = get_mips_cond_attr_const(node)->cond;
	char            const *const fmt   = cond == mips_cc_eq || cond == mips_cc_ne ? "b%C\t%S0, %S1, %L\nnop" : "b%C\t%S0, %L\nnop";
	be_cond_branch_projs_t const projs = be_get_cond_branch_projs(node);
	if (be_is_fallthrough(projs.t)) {
		mips_emitf(node, fmt, mips_negate_cond(cond), projs.f);
	} else {
		mips_emitf(node, fmt, cond, projs.t);
		emit_jmp(node, projs.f);
	}
}

static void emit_jumptable_target(ir_entity const *const table, ir_node const *const proj_x)
{
  (void)table;
  be_emit_cfop_target(proj_x);
}

static void emit_mips_switch(ir_node const *const node)
{
  mips_emitf(node, "jr\t%S0\nnop");

  mips_switch_attr_t const *const attr = get_mips_switch_attr_const(node);
  be_emit_jump_table(node, &attr->swtch, mode_P, emit_jumptable_target);
}

static void mips_register_emitters(void)
{
	be_init_emitters();
	mips_register_spec_emitters();

	be_set_emitter(op_be_Asm,      emit_be_ASM);
	be_set_emitter(op_be_Copy,     emit_be_Copy);
	be_set_emitter(op_be_IncSP,    emit_be_IncSP);
	be_set_emitter(op_be_Perm,     emit_be_Perm);
	be_set_emitter(op_mips_b,      emit_mips_b);
	be_set_emitter(op_mips_bcc,    emit_mips_bcc);
	be_set_emitter(op_mips_switch, emit_mips_switch);
}

static void mips_gen_block(ir_node *const block)
{
	be_gas_begin_block(block);
	sched_foreach(block, node) {
		be_emit_node(node);
	}
}

void mips_emit_function(ir_graph *const irg)
{
	mips_register_emitters();
	be_gas_elf_type_char = '@';

	ir_entity *const entity = get_irg_entity(irg);
	be_gas_emit_function_prolog(entity, 4, NULL);

	ir_node **const blk_sched = be_create_block_schedule(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	be_emit_init_cf_links(blk_sched);

	for (size_t i = 0, n_blocks = ARR_LEN(blk_sched); i != n_blocks; ++i) {
		mips_gen_block(blk_sched[i]);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	be_gas_emit_function_epilog(entity);
}
