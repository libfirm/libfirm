/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_emitter.h"

#include <inttypes.h>

#include "be_t.h"
#include "bearch.h"
#include "beblocksched.h"
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

static void emit_immediate(ir_node const *const node)
{
	mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(node);
	be_emit_irprintf("%" PRId32, imm->val);
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

		case 'I': emit_immediate(node); break;

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

static void emit_mips_bcc(ir_node const *const node)
{
	mips_cond_t            const cond  = get_mips_cond_attr_const(node)->cond;
	char            const *const fmt   = cond == mips_cc_eq || cond == mips_cc_ne ? "b%C\t%S0, %S1, %L\nnop" : "b%C\t%S0, %L\nnop";
	be_cond_branch_projs_t const projs = be_get_cond_branch_projs(node);
	if (be_is_fallthrough(projs.t)) {
		mips_emitf(node, fmt, mips_negate_cond(cond), projs.f);
	} else {
		mips_emitf(node, fmt, cond, projs.t);
		BE_EMIT_JMP(mips, node, "b", projs.f) {
			mips_emitf(NULL, "nop");
		}
	}
}

static void mips_register_emitters(void)
{
	be_init_emitters();
	mips_register_spec_emitters();

	be_set_emitter(op_be_Copy,  emit_be_Copy);
	be_set_emitter(op_mips_bcc, emit_mips_bcc);
}

static void mips_gen_block(ir_node *const block)
{
	be_gas_begin_block(block, true);
	sched_foreach(block, node) {
		be_emit_node(node);
	}
}

void mips_emit_function(ir_graph *const irg)
{
	mips_register_emitters();
	be_gas_elf_type_char = '@';

	ir_entity *const entity = get_irg_entity(irg);
	be_gas_emit_function_prolog(entity, 16, NULL);

	ir_node **const blk_sched = be_create_block_schedule(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	be_emit_init_cf_links(blk_sched);

	for (size_t i = 0, n_blocks = ARR_LEN(blk_sched); i != n_blocks; ++i) {
		mips_gen_block(blk_sched[i]);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	be_gas_emit_function_epilog(entity);
}
