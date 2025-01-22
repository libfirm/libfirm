/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "riscv_new_nodes_t.h"

#include "gen_riscv_new_nodes.h"
#include "riscv_nodes_attr.h"

static int riscv_attrs_equal_(riscv_attr_t const *const a_attr, riscv_attr_t const *const b_attr)
{
#if 0
	return except_attrs_equal(&a_attr->exc, &b_attr->exc); // TODO all other backends do not check this either
#else
	(void)a_attr, (void)b_attr;
	return 1;
#endif
}

int riscv_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	riscv_attr_t const *const a_attr = get_riscv_attr_const(a);
	riscv_attr_t const *const b_attr = get_riscv_attr_const(b);
	return riscv_attrs_equal_(a_attr, b_attr);
}

int riscv_cond_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	riscv_cond_attr_t const *const a_attr = get_riscv_cond_attr_const(a);
	riscv_cond_attr_t const *const b_attr = get_riscv_cond_attr_const(b);
	return
		riscv_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		a_attr->cond == b_attr->cond;
}

int riscv_immediate_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	riscv_immediate_attr_t const *const a_attr = get_riscv_immediate_attr_const(a);
	riscv_immediate_attr_t const *const b_attr = get_riscv_immediate_attr_const(b);
	return
		riscv_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		a_attr->ent == b_attr->ent &&
		a_attr->val == b_attr->val;
}

int riscv_switch_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	riscv_switch_attr_t const *const a_attr = get_riscv_switch_attr_const(a);
	riscv_switch_attr_t const *const b_attr = get_riscv_switch_attr_const(b);
	return
		riscv_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		be_switch_attrs_equal(&a_attr->swtch, &b_attr->swtch);
}

static void dump_immediate(FILE *const F, char const *const prefix, ir_node const *const n)
{
	riscv_immediate_attr_t const *const imm = get_riscv_immediate_attr_const(n);
	if (imm->ent) {
		fputc(' ', F);
		if (prefix)
			fprintf(F, "%s(", prefix);
		const char *name = get_entity_name(imm->ent);
		if (name) {
			fputs(name, F);
		}
		if (imm->val != 0)
			fprintf(F, "%+" PRId32, imm->val);
		if (prefix)
			fputc(')', F);
	} else {
		fprintf(F, " %+" PRId32, imm->val);
	}
}

void riscv_dump_node(FILE *const F, ir_node const *const n, dump_reason_t const reason)
{
	switch (reason) {
		case dump_node_info_txt:
		case dump_node_mode_txt:
		case dump_node_nodeattr_txt:
			break;

		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			switch ((riscv_opcodes)get_riscv_irn_opcode(n)) {
			case iro_riscv_addi:
			case iro_riscv_lb:
			case iro_riscv_lbu:
			case iro_riscv_lh:
			case iro_riscv_lhu:
			case iro_riscv_lw:
			case iro_riscv_sb:
			case iro_riscv_sh:
			case iro_riscv_slli:
			case iro_riscv_sltiu:
			case iro_riscv_srai:
			case iro_riscv_srli:
			case iro_riscv_sw:
				dump_immediate(F, "%lo", n);
				break;

			case iro_riscv_andi:
			case iro_riscv_ori:
			case iro_riscv_xori: {
				riscv_immediate_attr_t const *const imm = get_riscv_immediate_attr_const(n);
				fprintf(F, " 0x%03" PRIX32, (uint32_t)imm->val);
				break;
			}

			case iro_riscv_bcc: {
				riscv_cond_attr_t const *const cond = get_riscv_cond_attr_const(n);
				fprintf(F, " %s", riscv_get_cond_name(cond->cond));
				break;
			}

			case iro_riscv_jal:
				dump_immediate(F, NULL, n);
				break;

			case iro_riscv_lui:
				dump_immediate(F, "%hi", n);
				break;

			case iro_riscv_add:
			case iro_riscv_and:
			case iro_riscv_div:
			case iro_riscv_divu:
			case iro_riscv_FrameAddr:
			case iro_riscv_ijmp:
			case iro_riscv_j:
			case iro_riscv_jalr:
			case iro_riscv_last:
			case iro_riscv_mul:
			case iro_riscv_mulh:
			case iro_riscv_mulhu:
			case iro_riscv_or:
			case iro_riscv_rem:
			case iro_riscv_remu:
			case iro_riscv_ret:
			case iro_riscv_sll:
			case iro_riscv_slt:
			case iro_riscv_sltu:
			case iro_riscv_sra:
			case iro_riscv_srl:
			case iro_riscv_sub:
			case iro_riscv_SubSP:
			case iro_riscv_SubSPimm:
			case iro_riscv_switch:
			case iro_riscv_xor:
				break;
			}
			break;
	}
}
