/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_new_nodes_t.h"

#include "bearch.h"
#include "gen_mips_new_nodes.h"
#include "ircons_t.h"
#include <inttypes.h>

static int mips_attrs_equal_(mips_attr_t const *const a_attr, mips_attr_t const *const b_attr)
{
#if 0
	return except_attrs_equal(&a_attr->exc, &b_attr->exc); // TODO all other backends do not check this either
#else
	(void)a_attr, (void)b_attr;
	return 1;
#endif
}

int mips_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	mips_attr_t const *const a_attr = get_mips_attr_const(a);
	mips_attr_t const *const b_attr = get_mips_attr_const(b);
	return mips_attrs_equal_(a_attr, b_attr);
}

int mips_cond_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	mips_cond_attr_t const *const a_attr = get_mips_cond_attr_const(a);
	mips_cond_attr_t const *const b_attr = get_mips_cond_attr_const(b);
	return
		mips_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		a_attr->cond == b_attr->cond;
}

int mips_immediate_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	mips_immediate_attr_t const *const a_attr = get_mips_immediate_attr_const(a);
	mips_immediate_attr_t const *const b_attr = get_mips_immediate_attr_const(b);
	return
		mips_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		a_attr->ent == b_attr->ent &&
		a_attr->val == b_attr->val;
}

int mips_switch_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	mips_switch_attr_t const *const a_attr = get_mips_switch_attr_const(a);
	mips_switch_attr_t const *const b_attr = get_mips_switch_attr_const(b);
	return
		mips_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		be_switch_attrs_equal(&a_attr->swtch, &b_attr->swtch);
}

static void dump_immediate(FILE *const F, char const *const prefix, ir_node const *const n)
{
	mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(n);
	if (imm->ent) {
		fputc(' ', F);
		if (prefix)
			fprintf(F, "%s(", prefix);
		fputs(get_entity_name(imm->ent), F);
		if (imm->val != 0)
			fprintf(F, "%+" PRId32, imm->val);
		if (prefix)
			fputc(')', F);
	} else {
		fprintf(F, " %+" PRId32, imm->val);
	}
}

void mips_dump_node(FILE *const F, ir_node const *const n, dump_reason_t const reason)
{
	switch (reason) {
		case dump_node_info_txt:
		case dump_node_mode_txt:
		case dump_node_nodeattr_txt:
			break;

		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			switch ((mips_opcodes)get_mips_irn_opcode(n)) {
			case iro_mips_addiu:
			case iro_mips_lb:
			case iro_mips_lbu:
			case iro_mips_lh:
			case iro_mips_lhu:
			case iro_mips_lw:
			case iro_mips_sb:
			case iro_mips_sh:
			case iro_mips_sll:
			case iro_mips_sltiu:
			case iro_mips_sra:
			case iro_mips_srl:
			case iro_mips_sw:
				dump_immediate(F, "%lo", n);
				break;

			case iro_mips_andi:
			case iro_mips_ori:
			case iro_mips_xori: {
				mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(n);
				fprintf(F, " 0x%04" PRIX32, (uint32_t)imm->val);
				break;
			}

			case iro_mips_bcc: {
				mips_cond_attr_t const *const cond = get_mips_cond_attr_const(n);
				fprintf(F, " %s", mips_get_cond_name(cond->cond));
				break;
			}

			case iro_mips_jal:
				dump_immediate(F, NULL, n);
				break;

			case iro_mips_lui:
				dump_immediate(F, "%hi", n);
				break;

			case iro_mips_addu:
			case iro_mips_and:
			case iro_mips_b:
			case iro_mips_div_hi:
			case iro_mips_div_lo:
			case iro_mips_divu_hi:
			case iro_mips_divu_lo:
			case iro_mips_ijmp:
			case iro_mips_jalr:
			case iro_mips_last:
			case iro_mips_mult_hi:
			case iro_mips_mult_lo:
			case iro_mips_multu_hi:
			case iro_mips_nor:
			case iro_mips_or:
			case iro_mips_ret:
			case iro_mips_sllv:
			case iro_mips_slt:
			case iro_mips_sltu:
			case iro_mips_srav:
			case iro_mips_srlv:
			case iro_mips_subu:
			case iro_mips_switch:
			case iro_mips_xor:
				break;
			}
			break;
	}
}
