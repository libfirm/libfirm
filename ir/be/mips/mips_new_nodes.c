/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_new_nodes_t.h"

#include <inttypes.h>

#include "gen_mips_new_nodes.h"
#include "ircons_t.h"

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

int mips_immediate_attrs_equal(ir_node const *const a, ir_node const *const b)
{
	mips_immediate_attr_t const *const a_attr = get_mips_immediate_attr_const(a);
	mips_immediate_attr_t const *const b_attr = get_mips_immediate_attr_const(b);
	return
		mips_attrs_equal_(&a_attr->attr, &b_attr->attr) &&
		a_attr->val == b_attr->val;
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
			case iro_mips_addiu: {
				mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(n);
				fprintf(F, " %+" PRId32, imm->val);
				break;
			}

			case iro_mips_lui:
			case iro_mips_ori: {
				mips_immediate_attr_t const *const imm = get_mips_immediate_attr_const(n);
				fprintf(F, " 0x%04" PRIX32, (uint32_t)imm->val);
				break;
			}

			case iro_mips_addu:
			case iro_mips_last:
			case iro_mips_ret:
			case iro_mips_subu:
				break;
			}
			break;
	}
}
