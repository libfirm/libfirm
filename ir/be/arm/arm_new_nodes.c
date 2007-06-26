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
			if (is_arm_CmpBra(n) && get_arm_CondJmp_proj_num(n) >= 0) {
				fprintf(F, "proj_num = (%d)\n", get_arm_CondJmp_proj_num(n));
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

/* Returns the attributes of a generic Arm node. */
arm_attr_t *get_arm_attr(ir_node *node) {
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return get_irn_generic_attr(node);
}

const arm_attr_t *get_arm_attr_const(const ir_node *node) {
	assert(is_arm_irn(node) && "need arm node to get attributes");
	return get_irn_generic_attr_const(node);
}

/**
 * Returns the attributes of an ARM SymConst node.
 */
arm_SymConst_attr_t *get_arm_SymConst_attr(ir_node *node) {
	assert(is_arm_SymConst(node));
	return get_irn_generic_attr(node);
}

const arm_SymConst_attr_t *get_arm_SymConst_attr_const(const ir_node *node) {
	assert(is_arm_SymConst(node));
	return get_irn_generic_attr_const(node);
}

/* Returns the attributes of a CondJmp node. */
arm_CondJmp_attr_t *get_arm_CmpBra_attr(ir_node *node) {
	assert(is_arm_CmpBra(node));
	return get_irn_generic_attr(node);
}

const arm_CondJmp_attr_t *get_arm_CmpBra_attr_const(const ir_node *node) {
	assert(is_arm_CmpBra(node));
	return get_irn_generic_attr_const(node);
}

/* Returns the attributes of a SwitchJmp node. */
arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr(ir_node *node) {
	assert(is_arm_SwitchJmp(node));
	return get_irn_generic_attr(node);
}

const arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr_const(const ir_node *node) {
	assert(is_arm_SwitchJmp(node));
	return get_irn_generic_attr_const(node);
}

/**
 * Returns the argument register requirements of a arm node.
 */
const arch_register_req_t **get_arm_in_req_all(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an arm node.
 */
const arch_register_req_t **get_arm_out_req_all(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->out_req;
}

/**
 * Returns the argument register requirement at position pos of an arm node.
 */
const arch_register_req_t *get_arm_in_req(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an arm node.
 */
const arch_register_req_t *get_arm_out_req(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);
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
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->flags;
}

/**
 * Sets the register flag of an arm node.
 */
void set_arm_flags(ir_node *node, arch_irn_flags_t flags) {
	arm_attr_t *attr = get_arm_attr(node);
	attr->flags      = flags;
}

/**
 * Returns the result register slots of an arm node.
 */
const arch_register_t **get_arm_slots(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_arm_out_reg_name(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_arm_out_regnr(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_arm_out_reg(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);

	assert(is_arm_irn(node) && "Not an arm node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Sets the flags for the n'th out.
 */
void set_arm_out_flags(ir_node *node, arch_irn_flags_t flags, int pos) {
	arm_attr_t *attr = get_arm_attr(node);
	assert(pos < ARR_LEN(attr->out_flags) && "Invalid OUT position.");
	attr->out_flags[pos] = flags;
}

/**
 * Gets the flags for the n'th out.
 */
arch_irn_flags_t get_arm_out_flags(const ir_node *node, int pos) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	assert(pos < ARR_LEN(attr->out_flags) && "Invalid OUT position.");
	return attr->out_flags[pos];
}

/**
 * Returns the number of results.
 */
int get_arm_n_res(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return ARR_LEN(attr->slots);
}
/**
 * Returns the tarvalue
 */
tarval *get_arm_value(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
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
int get_arm_CondJmp_proj_num(const ir_node *node) {
	const arm_CondJmp_attr_t *attr = get_arm_CmpBra_attr_const(node);
	return attr->proj_num;
}

/**
 * Sets the proj num
 */
void set_arm_CondJmp_proj_num(ir_node *node, int proj_num) {
	arm_CondJmp_attr_t *attr = get_arm_CmpBra_attr(node);
	attr->proj_num   = proj_num;
}

/**
 * Returns the SymConst label
 */
ident *get_arm_symconst_id(const ir_node *node) {
	const arm_SymConst_attr_t *attr = get_arm_SymConst_attr_const(node);
	return attr->symconst_id;
}

/**
 * Sets the SymConst label
 */
void set_arm_symconst_id(ir_node *node, ident *symconst_id) {
	arm_SymConst_attr_t *attr = get_arm_SymConst_attr(node);
	attr->symconst_id = symconst_id;
}

/**
 * Returns the number of projs of a SwitchJmp.
 */
int get_arm_SwitchJmp_n_projs(const ir_node *node) {
	const arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr_const(node);
	return attr->n_projs;
}

/**
 * Sets the number of projs.
 */
void set_arm_SwitchJmp_n_projs(ir_node *node, int n_projs) {
	arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr(node);
	attr->n_projs = n_projs;
}

/**
 * Returns the default_proj_num.
 */
long get_arm_SwitchJmp_default_proj_num(const ir_node *node) {
	const arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr_const(node);
	return attr->default_proj_num;
}

/**
 * Sets the default_proj_num.
 */
void set_arm_SwitchJmp_default_proj_num(ir_node *node, long default_proj_num) {
	arm_SwitchJmp_attr_t *attr = get_arm_SwitchJmp_attr(node);
	attr->default_proj_num = default_proj_num;
}

/**
 * Gets the shift modifier attribute.
 */
arm_shift_modifier get_arm_shift_modifier(const ir_node *node) {
	const arm_attr_t *attr = get_arm_attr_const(node);
	return ARM_GET_SHF_MOD(attr);
}

/* Set the ARM machine node attributes to default values. */
void init_arm_attributes(ir_node *node, int flags,
                         const arch_register_req_t ** in_reqs,
						 const arch_register_req_t ** out_reqs,
                         const be_execution_unit_t ***execution_units,
						 int n_res, unsigned latency) {
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	arm_attr_t     *attr = get_arm_attr(node);
	(void) execution_units;
	(void) latency;

	attr->in_req           = in_reqs;
	attr->out_req          = out_reqs;
	attr->flags            = flags;
	attr->instr_fl         = (ARM_COND_AL << 3) | ARM_SHF_NONE;
	attr->value            = NULL;

	attr->out_flags = NEW_ARR_D(int, obst, n_res);
	memset(attr->out_flags, 0, n_res * sizeof(attr->out_flags[0]));

	attr->slots = NEW_ARR_D(const arch_register_t*, obst, n_res);
	memset(attr->slots, 0, n_res * sizeof(attr->slots[0]));
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

static int cmp_attr_arm_SymConst(ir_node *a, ir_node *b) {
	const arm_SymConst_attr_t *attr_a = get_irn_generic_attr_const(a);
	const arm_SymConst_attr_t *attr_b = get_irn_generic_attr_const(b);
	return attr_a->symconst_id != attr_b->symconst_id;
}

static int cmp_attr_arm(ir_node *a, ir_node *b) {
	arm_attr_t *attr_a = get_irn_generic_attr(a);
	arm_attr_t *attr_b = get_irn_generic_attr(b);
	return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);
}

static int cmp_attr_arm_CondJmp(ir_node *a, ir_node *b) {
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

static int cmp_attr_arm_SwitchJmp(ir_node *a, ir_node *b) {
	(void) a;
	(void) b;
	/* never identical */
	return 1;
}

/** copies the ARM attributes of a node. */
static void arm_copy_attr(const ir_node *old_node, ir_node *new_node) {
	ir_graph          *irg     = get_irn_irg(new_node);
	struct obstack    *obst    = get_irg_obstack(irg);
	const arm_attr_t *attr_old = get_arm_attr_const(old_node);
	arm_attr_t       *attr_new = get_arm_attr(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	attr_new->out_flags =
		DUP_ARR_D(int, obst, attr_old->out_flags);
	/* copy register assignments */
	attr_new->slots =
		DUP_ARR_D(arch_register_t*, obst, attr_old->slots);
}



/* Include the generated constructor functions */
#include "gen_arm_new_nodes.c.inl"
