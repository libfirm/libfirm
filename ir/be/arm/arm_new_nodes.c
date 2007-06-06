/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief  This file implements the creation of the architecture specific firm
 *         opcodes and the corresponding node constructors for the arm
 *         assembler irg.
 * @author Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "firm_common_t.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch_t.h"

#include "arm_nodes_attr.h"
#include "arm_new_nodes.h"
#include "gen_arm_regalloc_if_t.h"

#include "../beabi.h"
#include "bearch_arm_t.h"

/**
 * Returns the shift modifier string.
 */
const char *arm_shf_mod_name(arm_shift_modifier mod) {
  static const char *names[] = { NULL, NULL, "asr", "lsl", "lsr", "ror", "rrx" };
	return names[mod];
}

/***********************************************************************************
 *      _                                   _       _             __
 *     | |                                 (_)     | |           / _|
 *   __| |_   _ _ __ ___  _ __   ___ _ __   _ _ __ | |_ ___ _ __| |_ __ _  ___ ___
 *  / _` | | | | '_ ` _ \| '_ \ / _ \ '__| | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \
 * | (_| | |_| | | | | | | |_) |  __/ |    | | | | | ||  __/ |  | || (_| | (_|  __/
 *  \__,_|\__,_|_| |_| |_| .__/ \___|_|    |_|_| |_|\__\___|_|  |_| \__,_|\___\___|
 *                       | |
 *                       |_|
 ***********************************************************************************/

/**
 * Dumps the register requirements for either in or out.
 */
static void dump_reg_req(FILE *F, const ir_node *node,
                         const arch_register_req_t **reqs, int inout) {
	char *dir = inout ? "out" : "in";
	int   max = inout ? get_arm_n_res(node) : get_irn_arity(node);
	char  buf[1024];
	int   i;

	memset(buf, 0, sizeof(buf));

	if (reqs) {
		for (i = 0; i < max; i++) {
			fprintf(F, "%sreq #%d =", dir, i);

			if (reqs[i]->type == arch_register_req_type_none) {
				fprintf(F, " n/a");
			}

			if (reqs[i]->type & arch_register_req_type_normal) {
				fprintf(F, " %s", reqs[i]->cls->name);
			}

			if (reqs[i]->type & arch_register_req_type_limited) {
				fprintf(F, " %s",
				        arch_register_req_format(buf, sizeof(buf), reqs[i], node));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_same) {
				ir_fprintf(F, " same as %+F", get_irn_n(node, reqs[i]->other_same));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_different) {
				ir_fprintf(F, " different from %+F", get_irn_n(node, reqs[i]->other_different));
			}

			fprintf(F, "\n");
		}

		fprintf(F, "\n");
	} else {
		fprintf(F, "%sreq = N/A\n", dir);
	}
}

/**
 * Dumper interface for dumping arm nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int arm_dump_node(ir_node *n, FILE *F, dump_reason_t reason) {
	ir_mode     *mode = NULL;
	int          bad  = 0;
	int          i;
	arm_attr_t  *attr = get_arm_attr(n);
	const arch_register_req_t **reqs;
	const arch_register_t     **slots;
	arm_shift_modifier        mod;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			mode = get_irn_mode(n);

			if (mode) {
				fprintf(F, "[%s]", get_mode_name(mode));
			}
			else {
				fprintf(F, "[?NOMODE?]");
			}
			break;

		case dump_node_nodeattr_txt:
			mod = ARM_GET_SHF_MOD(attr);
			if (ARM_HAS_SHIFT(mod)) {
				fprintf(F, "[%s #%ld]", arm_shf_mod_name(mod), get_tarval_long(attr->value));
			}
			else if (mod == ARM_SHF_IMM) {
				/* immediate */
				fprintf(F, "[#0x%X]", arm_decode_imm_w_shift(attr->value));
			}
			break;

		case dump_node_info_txt:
			fprintf(F, "=== arm attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_arm_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			/* dump OUT requirements */
			if (ARR_LEN(attr->slots) > 0) {
				reqs = get_arm_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);
			}

			/* dump assigned registers */
			slots = get_arm_slots(n);
			if (slots && ARR_LEN(attr->slots) > 0) {
				for (i = 0; i < ARR_LEN(attr->slots); i++) {
					if (slots[i]) {
						fprintf(F, "reg #%d = %s\n", i, slots[i]->name);
					}
					else {
						fprintf(F, "reg #%d = n/a\n", i);
					}
				}
			}
			fprintf(F, "\n");

			/* dump n_res */
			fprintf(F, "n_res = %d\n", get_arm_n_res(n));

			/* dump flags */
			fprintf(F, "flags =");
			if (attr->flags == arch_irn_flags_none) {
				fprintf(F, " none");
			}
			else {
				if (attr->flags & arch_irn_flags_dont_spill) {
					fprintf(F, " unspillable");
				}
				if (attr->flags & arch_irn_flags_rematerializable) {
					fprintf(F, " remat");
				}
				if (attr->flags & arch_irn_flags_ignore) {
					fprintf(F, " ignore");
				}
			}
			fprintf(F, " (%d)\n", attr->flags);

			if (get_arm_value(n)) {
				if (is_arm_CopyB(n)) {
					fprintf(F, "size = %lu\n", get_tarval_long(get_arm_value(n)));
				} else {
					if (mode_is_float(get_irn_mode(n))) {
						fprintf(F, "float value = (%f)\n", (double) get_tarval_double(get_arm_value(n)));
					} else if (mode_is_int(get_irn_mode(n))) {
						long v =  get_tarval_long(get_arm_value(n));
						fprintf(F, "long value = %ld (0x%08lx)\n", v, v);
					} else if (mode_is_reference(get_irn_mode(n))) {
						fprintf(F, "pointer\n");
					} else {
						assert(0 && "unbehandelter Typ im const-Knoten");
					}
				}
			}
			if (get_arm_proj_num(n) >= 0) {
				fprintf(F, "proj_num = (%d)\n", get_arm_proj_num(n));
			}
			/* TODO: dump all additional attributes */

			fprintf(F, "=== arm attr end ===\n");
			/* end of: case dump_node_info_txt */
			break;
	}
	return bad;
}



/***************************************************************************************************
 *        _   _                   _       __        _                    _   _               _
 *       | | | |                 | |     / /       | |                  | | | |             | |
 *   __ _| |_| |_ _ __   ___  ___| |_   / /_ _  ___| |_   _ __ ___   ___| |_| |__   ___   __| |___
 *  / _` | __| __| '__| / __|/ _ \ __| / / _` |/ _ \ __| | '_ ` _ \ / _ \ __| '_ \ / _ \ / _` / __|
 * | (_| | |_| |_| |    \__ \  __/ |_ / / (_| |  __/ |_  | | | | | |  __/ |_| | | | (_) | (_| \__ \
 *  \__,_|\__|\__|_|    |___/\___|\__/_/ \__, |\___|\__| |_| |_| |_|\___|\__|_| |_|\___/ \__,_|___/
 *                                        __/ |
 *                                       |___/
 ***************************************************************************************************/

/**
 * Wraps get_irn_generic_attr() as it takes no const ir_node, so we need to do a cast.
 * Firm was made by people hating const :-(
 */
arm_attr_t *get_arm_attr(const ir_node *node) {
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return (arm_attr_t *)get_irn_generic_attr((ir_node *)node);
}

/**
 * Returns the argument register requirements of a arm node.
 */
const arch_register_req_t **get_arm_in_req_all(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an arm node.
 */
const arch_register_req_t **get_arm_out_req_all(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->out_req;
}

/**
 * Returns the argument register requirement at position pos of an arm node.
 */
const arch_register_req_t *get_arm_in_req(const ir_node *node, int pos) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an arm node.
 */
const arch_register_req_t *get_arm_out_req(const ir_node *node, int pos) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->out_req[pos];
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_arm_req_out(ir_node *node, const arch_register_req_t *req, int pos) {
	arm_attr_t *attr   = get_arm_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the complete OUT requirements of node.
 */
void set_arm_req_out_all(ir_node *node, const arch_register_req_t **reqs) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->out_req    = reqs;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_arm_req_in(ir_node *node, const arch_register_req_t *req, int pos) {
	arm_attr_t *attr  = get_arm_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the register flag of an arm node.
 */
arch_irn_flags_t get_arm_flags(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->flags;
}

/**
 * Sets the register flag of an arm node.
 */
void set_arm_flags(const ir_node *node, arch_irn_flags_t flags) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->flags      = flags;
}

/**
 * Returns the result register slots of an arm node.
 */
const arch_register_t **get_arm_slots(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_arm_out_reg_name(const ir_node *node, int pos) {
	arm_attr_t *attr = get_arm_attr(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_arm_out_regnr(const ir_node *node, int pos) {
	arm_attr_t *attr = get_arm_attr(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_arm_out_reg(const ir_node *node, int pos) {
	arm_attr_t *attr = get_arm_attr(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Returns the number of results.
 */
int get_arm_n_res(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return ARR_LEN(attr->slots);
}
/**
 * Returns the tarvalue
 */
tarval *get_arm_value(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->value;
}

/**
 * Sets the tarvalue
 */
void set_arm_value(ir_node *node, tarval *tv) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->value = tv;
}

/**
 * Returns the proj num
 */
int get_arm_proj_num(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->proj_num;
}

/**
 * Sets the proj num
 */
void set_arm_proj_num(ir_node *node, int proj_num) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->proj_num      = proj_num;
}

/**
 * Returns the SymConst label
 */
ident *get_arm_symconst_id(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->symconst_id;
}

/**
 * Sets the SymConst label
 */
void set_arm_symconst_id(ir_node *node, ident *symconst_id) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->symconst_id = symconst_id;
}


/**
 * Returns the number of projs.
 */
int get_arm_n_projs(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->n_projs;
}

/**
 * Sets the number of projs.
 */
void set_arm_n_projs(ir_node *node, int n_projs) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->n_projs = n_projs;
}

/**
 * Returns the default_proj_num.
 */
long get_arm_default_proj_num(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return attr->default_proj_num;
}

/**
 * Sets the default_proj_num.
 */
void set_arm_default_proj_num(ir_node *node, long default_proj_num) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->default_proj_num = default_proj_num;
}

/**
 * Gets the shift modifier attribute.
 */
arm_shift_modifier get_arm_shift_modifier(const ir_node *node) {
	arm_attr_t *attr = get_arm_attr(node);
	return ARM_GET_SHF_MOD(attr);
}

/* Set the ARM machine node attributes to default values. */
void init_arm_attributes(ir_node *node, int flags, const arch_register_req_t ** in_reqs,
						 const arch_register_req_t ** out_reqs, const be_execution_unit_t ***execution_units,
						 int n_res, unsigned latency) {
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	arm_attr_t     *attr = get_arm_attr(node);

	attr->in_req           = in_reqs;
	attr->out_req          = out_reqs;
	attr->flags            = flags;
	attr->instr_fl         = (ARM_COND_AL << 3) | ARM_SHF_NONE;
	attr->value            = NULL;
	attr->proj_num         = -42;
	attr->symconst_id      = NULL;
	attr->n_projs          = 0;
	attr->default_proj_num = 0;

	attr->slots = NEW_ARR_D(const arch_register_t*, obst, n_res);
	memset((arch_register_t **)attr->slots, 0, n_res * sizeof(attr->slots[0]));
}

static int arm_comp_condJmp(arm_attr_t *attr_a, arm_attr_t *attr_b) {
	return 1;
}


/***************************************************************************************
 *                  _                            _                   _
 *                 | |                          | |                 | |
 *  _ __   ___   __| | ___    ___ ___  _ __  ___| |_ _ __ _   _  ___| |_ ___  _ __ ___
 * | '_ \ / _ \ / _` |/ _ \  / __/ _ \| '_ \/ __| __| '__| | | |/ __| __/ _ \| '__/ __|
 * | | | | (_) | (_| |  __/ | (_| (_) | | | \__ \ |_| |  | |_| | (__| || (_) | |  \__ \
 * |_| |_|\___/ \__,_|\___|  \___\___/|_| |_|___/\__|_|   \__,_|\___|\__\___/|_|  |___/
 *
 ***************************************************************************************/

#ifdef BIT
#undef BIT
#endif
#define BIT(x)  (1 << (x % 32))

static unsigned arm_req_sp_limited[] = { BIT(REG_SP) };
static const arch_register_req_t _arm_req_sp = {
	arch_register_req_type_limited,
	&arm_reg_classes[CLASS_arm_gp],
	arm_req_sp_limited,
	-1,
	-1
};

/* construct Store: Store(ptr, val, mem) = ST ptr,val */
ir_node *new_r_arm_StoreStackMInc(ir_graph *irg, ir_node *block, ir_node *mem,
                                  ir_node *sp, int n_regs, ir_node **regs,
                                  ir_mode *mode) {
	ir_node *res;
	ir_node *in[16];
	int flags = 0;
	static const arch_register_req_t *_in_req_arm_StoreStackM4Inc[] =
	{
		&arm_StoreStackM4Inc_reg_req_in_0,
		&arm_StoreStackM4Inc_reg_req_in_1,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
		&arm_StoreStackM4Inc_reg_req_in_2,
	};

  	assert(n_regs <= 15);

	in[0] = mem;
	in[1] = sp;
	memcpy(&in[2], regs, n_regs * sizeof(in[0]));
	res = new_ir_node(NULL, irg, block, op_arm_StoreStackM4Inc, mode, 2 + n_regs, in);
	flags |= arch_irn_flags_rematerializable;   /* op can be easily recalculated */

	/* init node attributes */
	init_arm_attributes(res, flags, _in_req_arm_StoreStackM4Inc, NULL, NULL, 0, 1);

	res = optimize_node(res);
	irn_vrfy_irg(res, irg);

	return res;
}

/************************************************
 *   ___        _   _           _               *
 *  / _ \ _ __ | |_(_)_ __ ___ (_)_______ _ __  *
 * | | | | '_ \| __| | '_ ` _ \| |_  / _ \ '__| *
 * | |_| | |_) | |_| | | | | | | |/ /  __/ |    *
 *  \___/| .__/ \__|_|_| |_| |_|_/___\___|_|    *
 *       |_|                                    *
 ************************************************/

typedef struct _opt_tuple {
	ir_op *op_imm_left;		/**< immediate is left */
	ir_op *op_imm_right;	/**< immediate is right */
	ir_op *op_shf_left;		/**< shift operand on left */
	ir_op *op_shf_right;	/**< shift operand on right */
} opt_tuple;

//static const opt_tuple *opt_ops[iro_arm_last];

void arm_set_optimizers(void) {
	/*
#define STD(op)		p_##op = { op_arm_##op##_i, op_arm_##op##_i, op_arm_##op, op_arm_##op }
#define LEFT(op)	p_##op = { op_arm_##op##_i, NULL, op_arm_##op, NULL }
#define SET(op)   opt_ops[iro_arm_##op] = &p_##op;

	static const opt_tuple
		STD(Add),
		STD(And),
		STD(Or),
		STD(Eor),
		LEFT(Bic),
		LEFT(Shl),
		LEFT(Shr),
		LEFT(Shrs),
		p_Sub = { op_arm_Sub_i, op_arm_Rsb_i, op_arm_Sub, op_arm_Rsb },

	memset(opt_ops, 0, sizeof(opt_ops));
	SET(Add);
	SET(And);
	SET(Or);
	SET(Eor);
	SET(Sub);
	SET(Bic);
	SET(Shl);
	SET(Shr);
	SET(Shrs);
	*/
}

/* Include the generated constructor functions */
#include "gen_arm_new_nodes.c.inl"
