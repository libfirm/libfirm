/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Handling of ia32 specific firm opcodes.
 * @author      Christian Wuerdig
 *
 * This file implements the creation of the architecture specific firm opcodes
 * and the corresponding node constructors for the ia32 assembler irg.
 */
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>

#include "irargs_t.h"
#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irverify_t.h"
#include "irprintf.h"
#include "iredges.h"
#include "xmalloc.h"

#include "bearch.h"
#include "bedump.h"
#include "beinfo.h"

#include "bearch_ia32_t.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

struct obstack opcodes_obst;

static const char *condition_code_name(x86_condition_code_t cc)
{
	switch (cc) {
	case x86_cc_overflow:                    return "overflow";
	case x86_cc_not_overflow:                return "not overflow";
	case x86_cc_float_below:                 return "float below";
	case x86_cc_float_unordered_below:       return "float unordered or below";
	case x86_cc_below:                       return "below";
	case x86_cc_float_above_equal:           return "float above or equal";
	case x86_cc_float_unordered_above_equal: return "float unordered or above or equal";
	case x86_cc_above_equal:                 return "above or equal";
	case x86_cc_float_equal:                 return "float equal";
	case x86_cc_equal:                       return "equal";
	case x86_cc_float_not_equal:             return "float not equal";
	case x86_cc_not_equal:                   return "not equal";
	case x86_cc_float_below_equal:           return "float below or equal";
	case x86_cc_float_unordered_below_equal: return "float unordered or below or equal";
	case x86_cc_below_equal:                 return "below or equal";
	case x86_cc_float_above:                 return "float above";
	case x86_cc_float_unordered_above:       return "float unordered or above";
	case x86_cc_above:                       return "above";
	case x86_cc_sign:                        return "sign";
	case x86_cc_not_sign:                    return "no sign";
	case x86_cc_parity:                      return "parity";
	case x86_cc_not_parity:                  return "no parity";
	case x86_cc_less:                        return "less";
	case x86_cc_greater_equal:               return "greater or equal";
	case x86_cc_less_equal:                  return "less or equal";
	case x86_cc_greater:                     return "greater";
	case x86_cc_float_parity_cases:          return "float parity cases";
	case x86_cc_additional_float_cases:      return "additional float cases";
	default:                                 return NULL;
	}
}

static bool has_ia32_condcode_attr(const ir_node *node)
{
	return is_ia32_Setcc(node) || is_ia32_SetccMem(node) || is_ia32_CMovcc(node)
	    || is_ia32_Jcc(node) || is_ia32_Adc(node) || is_ia32_Sbb(node)
	    || is_ia32_Sbb0(node) || is_ia32_Cmc(node);
}

static bool has_ia32_x87_attr(ir_node const *const node)
{
	switch ((ia32_opcodes)get_ia32_irn_opcode(node)) {
	case iro_ia32_FucomFnstsw:
	case iro_ia32_Fucomi:
	case iro_ia32_FucomppFnstsw:
	case iro_ia32_fadd:
	case iro_ia32_fdiv:
	case iro_ia32_fdup:
	case iro_ia32_ffreep:
	case iro_ia32_fist:
	case iro_ia32_fisttp:
	case iro_ia32_fmul:
	case iro_ia32_fpop:
	case iro_ia32_fst:
	case iro_ia32_fsub:
	case iro_ia32_fxch:
		return true;

	default:
		return false;
	}
}

static char const *get_frame_use_str(ir_node const *const node)
{
	switch (get_ia32_frame_use(node)) {
	case IA32_FRAME_USE_NONE:  return "none";
	case IA32_FRAME_USE_32BIT: return "32bit";
	case IA32_FRAME_USE_64BIT: return "64bit";
	case IA32_FRAME_USE_AUTO:  return "auto";
	}
	return "invalid";
}

/**
 * Dumper interface for dumping ia32 nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static void ia32_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	ir_mode *mode = NULL;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));

			if (is_ia32_Immediate(n) || is_ia32_Const(n)) {
				const ia32_immediate_attr_t *attr
					= get_ia32_immediate_attr_const(n);

				fputc(' ', F);
				ir_entity *entity = attr->imm.entity;
				if (entity) {
					fputs(get_entity_name(entity), F);
				}
				int32_t offset = attr->imm.offset;
				if (offset != 0 || entity == NULL) {
					if (offset > 0 && entity != NULL) {
						fputc('+', F);
					}
					fprintf(F, "%"PRId32, offset);
					if (attr->no_pic_adjust) {
						fputs("(no_pic_adjust)", F);
					}
				}
			} else {
				const ia32_attr_t *attr = get_ia32_attr_const(n);

				int32_t    offset = attr->am_imm.offset;
				ir_entity *entity = attr->am_imm.entity;
				if (entity != NULL || offset != 0) {
					fputs(" [", F);

					if (entity != NULL) {
						fputs(get_entity_name(entity), F);
						if (attr->am_sc_no_pic_adjust) {
							fputs("(no_pic_adjust)", F);
						}
					}
					if (offset != 0) {
						if (offset > 0 && entity != NULL) {
							fputc('+', F);
						}
						fprintf(F, "%d", offset);
					}

					fputc(']', F);
				}
			}
			break;

		case dump_node_mode_txt:
			mode = get_ia32_ls_mode(n);
			if (mode != NULL)
				fprintf(F, "[%s]", get_mode_name(mode));
			break;

		case dump_node_nodeattr_txt:
			if (! is_ia32_Lea(n)) {
				switch (get_ia32_op_type(n)) {
				case ia32_Normal:    break;
				case ia32_AddrModeS: fprintf(F, "[AM S] "); break;
				case ia32_AddrModeD: fprintf(F, "[AM D] "); break;
				}
			}
			break;

		case dump_node_info_txt:
			be_dump_reqs_and_registers(F, n);

			/* dump op type */
			fprintf(F, "op = ");
			switch (get_ia32_op_type(n)) {
				case ia32_Normal:
					fprintf(F, "Normal");
					break;
				case ia32_AddrModeD:
					fprintf(F, "AM Dest (Load+Store)");
					break;
				case ia32_AddrModeS:
					fprintf(F, "AM Source (Load)");
					break;
				default:
					fprintf(F, "unknown (%d)", (int)get_ia32_op_type(n));
					break;
			}
			fprintf(F, "\n");

			/* dump supported am */
			fprintf(F, "AM support = ");
			switch (get_ia32_am_support(n)) {
				case ia32_am_none:   fputs("none\n",            F); break;
				case ia32_am_unary:  fputs("source (unary)\n",  F); break;
				case ia32_am_binary: fputs("source (binary)\n", F); break;

				default:
					fprintf(F, "unknown (%d)\n", (int)get_ia32_am_support(n));
					break;
			}

			/* dump AM offset */
			if (get_ia32_am_offs_int(n) != 0) {
				fprintf(F, "AM offset = %d\n", get_ia32_am_offs_int(n));
			}

			/* dump AM entity */
			ir_entity *ent = get_ia32_am_ent(n);
			if (ent != NULL) {
				ident *id = get_entity_ld_ident(ent);
				fprintf(F, "AM entity = %s\n", get_id_str(id));
			}

			/* dump AM scale */
			fprintf(F, "AM scale = %u\n", get_ia32_am_scale(n));

			/* dump pn code */
			if (has_ia32_condcode_attr(n)) {
				const ia32_attr_t *attr = get_ia32_attr_const(n);
				const char *cc_name = condition_code_name(get_ia32_condcode(n));
				if (cc_name) {
					fprintf(F, "condition_code = %s\n", cc_name);
				} else {
					fprintf(F, "condition_code = <invalid (0x%X)>\n",
					        (unsigned)get_ia32_condcode(n));
				}
				fprintf(F, "ins_permuted = %s\n", be_dump_yesno(attr->ins_permuted));
			} else if (is_ia32_CopyB(n) || is_ia32_CopyB_i(n)) {
				fprintf(F, "size = %u\n", get_ia32_copyb_size(n));
			} else if (has_ia32_x87_attr(n)) {
				ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(n);
				fprintf(F, "explicit operand = %s\n", be_dump_reg_name(attr->reg));
				fprintf(F, "result to explicit operand = %s\n", be_dump_yesno(attr->res_in_reg));
				fprintf(F, "pop = %s\n", be_dump_yesno(attr->pop));
			}

			fprintf(F, "commutative = %s\n", be_dump_yesno(is_ia32_commutative(n)));
			fprintf(F, "is reload = %s\n", be_dump_yesno(is_ia32_is_reload(n)));
			fprintf(F, "latency = %u\n", get_ia32_latency(n));

			/* dump frame entity */
			fprintf(F, "frame use = %s\n", get_frame_use_str(n));
			fprintf(F, "frame entity = ");
			if (get_ia32_frame_ent(n)) {
				ir_fprintf(F, "%+F", get_ia32_frame_ent(n));
			} else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

			/* dump modes */
			fprintf(F, "ls_mode = ");
			if (get_ia32_ls_mode(n)) {
				ir_fprintf(F, "%+F", get_ia32_ls_mode(n));
			} else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

#ifndef NDEBUG
			/* dump original ir node name */
			char const *orig = get_ia32_attr_const(n)->orig_node;
			fprintf(F, "orig node = %s\n", orig ? orig : "n/a");
#endif /* NDEBUG */

			break;
	}
}



ia32_attr_t *get_ia32_attr(ir_node *node)
{
	assert(is_ia32_irn(node) && "need ia32 node to get ia32 attributes");
	return (ia32_attr_t *)get_irn_generic_attr(node);
}

const ia32_attr_t *get_ia32_attr_const(const ir_node *node)
{
	assert(is_ia32_irn(node) && "need ia32 node to get ia32 attributes");
	return (const ia32_attr_t*) get_irn_generic_attr_const(node);
}

ia32_x87_attr_t *get_ia32_x87_attr(ir_node *node)
{
	ia32_attr_t     *attr     = get_ia32_attr(node);
	ia32_x87_attr_t *x87_attr = CAST_IA32_ATTR(ia32_x87_attr_t, attr);
	return x87_attr;
}

const ia32_x87_attr_t *get_ia32_x87_attr_const(const ir_node *node)
{
	const ia32_attr_t     *attr     = get_ia32_attr_const(node);
	const ia32_x87_attr_t *x87_attr = CONST_CAST_IA32_ATTR(ia32_x87_attr_t, attr);
	return x87_attr;
}

ia32_immediate_attr_t *get_ia32_immediate_attr(ir_node *node)
{
	ia32_attr_t           *attr      = get_ia32_attr(node);
	ia32_immediate_attr_t *imm_attr  = CAST_IA32_ATTR(ia32_immediate_attr_t, attr);

	return imm_attr;
}

const ia32_immediate_attr_t *get_ia32_immediate_attr_const(const ir_node *node)
{
	const ia32_attr_t           *attr     = get_ia32_attr_const(node);
	const ia32_immediate_attr_t *imm_attr = CONST_CAST_IA32_ATTR(ia32_immediate_attr_t, attr);

	return imm_attr;
}

ia32_condcode_attr_t *get_ia32_condcode_attr(ir_node *node)
{
	assert(has_ia32_condcode_attr(node));
	ia32_attr_t          *attr    = get_ia32_attr(node);
	ia32_condcode_attr_t *cc_attr = CAST_IA32_ATTR(ia32_condcode_attr_t, attr);

	return cc_attr;
}

const ia32_condcode_attr_t *get_ia32_condcode_attr_const(const ir_node *node)
{
	assert(has_ia32_condcode_attr(node));
	const ia32_attr_t          *attr    = get_ia32_attr_const(node);
	const ia32_condcode_attr_t *cc_attr = CONST_CAST_IA32_ATTR(ia32_condcode_attr_t, attr);

	return cc_attr;
}

ia32_switch_attr_t *get_ia32_switch_attr(ir_node *node)
{
	ia32_attr_t        *attr        = get_ia32_attr(node);
	ia32_switch_attr_t *switch_attr = CAST_IA32_ATTR(ia32_switch_attr_t, attr);
	return switch_attr;
}

const ia32_switch_attr_t *get_ia32_switch_attr_const(const ir_node *node)
{
	const ia32_attr_t        *attr        = get_ia32_attr_const(node);
	const ia32_switch_attr_t *switch_attr = CONST_CAST_IA32_ATTR(ia32_switch_attr_t, attr);
	return switch_attr;
}

ia32_return_attr_t *get_ia32_return_attr(ir_node *node)
{
	ia32_attr_t        *attr        = get_ia32_attr(node);
	ia32_return_attr_t *return_attr = CAST_IA32_ATTR(ia32_return_attr_t, attr);
	return return_attr;
}

const ia32_return_attr_t *get_ia32_return_attr_const(const ir_node *node)
{
	const ia32_attr_t        *attr        = get_ia32_attr_const(node);
	const ia32_return_attr_t *return_attr = CONST_CAST_IA32_ATTR(ia32_return_attr_t, attr);
	return return_attr;
}

ia32_call_attr_t *get_ia32_call_attr(ir_node *node)
{
	ia32_attr_t      *attr      = get_ia32_attr(node);
	ia32_call_attr_t *call_attr = CAST_IA32_ATTR(ia32_call_attr_t, attr);

	return call_attr;
}

const ia32_call_attr_t *get_ia32_call_attr_const(const ir_node *node)
{
	const ia32_attr_t      *attr      = get_ia32_attr_const(node);
	const ia32_call_attr_t *call_attr = CONST_CAST_IA32_ATTR(ia32_call_attr_t, attr);

	return call_attr;
}

ia32_copyb_attr_t *get_ia32_copyb_attr(ir_node *node)
{
	ia32_attr_t       *attr       = get_ia32_attr(node);
	ia32_copyb_attr_t *copyb_attr = CAST_IA32_ATTR(ia32_copyb_attr_t, attr);

	return copyb_attr;
}

const ia32_copyb_attr_t *get_ia32_copyb_attr_const(const ir_node *node)
{
	const ia32_attr_t       *attr       = get_ia32_attr_const(node);
	const ia32_copyb_attr_t *copyb_attr = CONST_CAST_IA32_ATTR(ia32_copyb_attr_t, attr);

	return copyb_attr;
}

ia32_climbframe_attr_t *get_ia32_climbframe_attr(ir_node *node)
{
	ia32_attr_t            *attr            = get_ia32_attr(node);
	ia32_climbframe_attr_t *climbframe_attr = CAST_IA32_ATTR(ia32_climbframe_attr_t, attr);

	return climbframe_attr;
}

const ia32_climbframe_attr_t *get_ia32_climbframe_attr_const(const ir_node *node)
{
	const ia32_attr_t            *attr            = get_ia32_attr_const(node);
	const ia32_climbframe_attr_t *climbframe_attr = CONST_CAST_IA32_ATTR(ia32_climbframe_attr_t, attr);

	return climbframe_attr;
}

/**
 * Gets the type of an ia32 node.
 */
ia32_op_type_t get_ia32_op_type(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return (ia32_op_type_t)attr->tp;
}

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->tp = tp;
}

ia32_am_type_t get_ia32_am_support(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return (ia32_am_type_t)attr->am_arity;
}

/**
 * Sets the supported address mode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t arity)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->am_arity = arity;
}

/**
 * Gets the address mode offset as int.
 */
int32_t get_ia32_am_offs_int(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_imm.offset;
}

/**
 * Sets the address mode offset from an int.
 */
void set_ia32_am_offs_int(ir_node *node, int32_t offset)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_imm.offset = offset;
}

void add_ia32_am_offs_int(ir_node *node, int32_t offset)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_imm.offset += offset;
}

ir_entity *get_ia32_am_ent(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_imm.entity;
}

void set_ia32_am_ent(ir_node *node, ir_entity *entity)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_imm.entity = entity;
}

void set_ia32_am_tls_segment(ir_node *node, bool value)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_tls_segment = value;
}

bool get_ia32_am_tls_segment(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_tls_segment;
}

/**
 * Gets the addr mode const.
 */
unsigned get_ia32_am_scale(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_scale;
}

/**
 * Sets the index register scale for address mode.
 */
void set_ia32_am_scale(ir_node *node, unsigned scale)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	assert(scale <= 3 && "AM scale out of range [0 ... 3]");
	attr->am_scale = scale;
}

void ia32_copy_am_attrs(ir_node *to, const ir_node *from)
{
	set_ia32_ls_mode(to, get_ia32_ls_mode(from));
	set_ia32_am_scale(to, get_ia32_am_scale(from));
	set_ia32_am_ent(to, get_ia32_am_ent(from));
	add_ia32_am_offs_int(to, get_ia32_am_offs_int(from));
	set_ia32_frame_ent(to, get_ia32_frame_ent(from));
	set_ia32_frame_use(to, get_ia32_frame_use(from));
}

/**
 * Sets node to commutative.
 */
void set_ia32_commutative(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->is_commutative = 1;
}

/**
 * Sets node to non-commutative.
 */
void clear_ia32_commutative(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->is_commutative = 0;
}

/**
 * Checks if node is commutative.
 */
int is_ia32_commutative(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->is_commutative;
}

void set_ia32_is_reload(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->is_reload = 1;
}

int is_ia32_is_reload(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->is_reload;
}

void set_ia32_is_spill(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->is_spill = 1;
}

int is_ia32_is_spill(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->is_spill;
}

void set_ia32_is_remat(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->is_remat = 1;
}

int is_ia32_is_remat(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->is_remat;
}

/**
 * Gets the frame entity assigned to this node.
 */
ir_entity *get_ia32_frame_ent(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->frame_ent;
}

/**
 * Sets the frame entity for this node.
 */
void set_ia32_frame_ent(ir_node *node, ir_entity *ent)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->frame_ent   = ent;
	if (!ent) {
		set_ia32_frame_use(node, IA32_FRAME_USE_NONE);
	} else if (get_ia32_frame_use(node) == IA32_FRAME_USE_NONE) {
		/* Only set frame use to auto, if it is not set to something more specific
		 * already. */
		set_ia32_frame_use(node, IA32_FRAME_USE_AUTO);
	}
}


/**
 * Gets the instruction latency.
 */
unsigned get_ia32_latency(const ir_node *node)
{
	assert(is_ia32_irn(node));
	const ir_op *op               = get_irn_op(node);
	const ia32_op_attr_t *op_attr = (ia32_op_attr_t*) get_op_attr(op);
	return op_attr->latency;
}

const ir_switch_table *get_ia32_switch_table(const ir_node *node)
{
	const ia32_switch_attr_t *attr = get_ia32_switch_attr_const(node);
	return attr->table;
}

x86_condition_code_t get_ia32_condcode(const ir_node *node)
{
	const ia32_condcode_attr_t *attr = get_ia32_condcode_attr_const(node);
	return attr->condition_code;
}

/**
 * Sets the condition code of a node
 */
void set_ia32_condcode(ir_node *node, x86_condition_code_t code)
{
	ia32_condcode_attr_t *attr = get_ia32_condcode_attr(node);
	attr->condition_code = code;
}

/**
 * Returns the condition code of a node.
 */
unsigned get_ia32_copyb_size(const ir_node *node)
{
	const ia32_copyb_attr_t *attr = get_ia32_copyb_attr_const(node);
	return attr->size;
}

/**
 * Get the exception label attribute.
 */
unsigned get_ia32_exc_label(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->has_except_label;
}

/**
 * Set the exception label attribute.
 */
void set_ia32_exc_label(ir_node *node, unsigned flag)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->has_except_label = flag;
}

/**
 * Return the exception label id.
 */
ir_label_t get_ia32_exc_label_id(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);

	assert(attr->has_except_label);
	return attr->exc_label;
}

/**
 * Assign the exception label id.
 */
void set_ia32_exc_label_id(ir_node *node, ir_label_t id)
{
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(attr->has_except_label);
	attr->exc_label = id;
}

#ifndef NDEBUG

static const char *ia32_get_old_node_name(const ir_node *irn)
{
	ir_graph       *irg  = get_irn_irg(irn);
	struct obstack *obst = be_get_be_obst(irg);

	lc_eoprintf(firm_get_arg_env(), obst, "%+F", irn);
	obstack_1grow(obst, 0);
	return (const char*)obstack_finish(obst);
}

/**
 * Sets the name of the original ir node.
 */
void set_ia32_orig_node(ir_node *node, const ir_node *old)
{
	const char  *name = ia32_get_old_node_name(old);
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->orig_node   = name;
}

#endif /* NDEBUG */

void ia32_swap_left_right(ir_node *node)
{
	ia32_attr_t *attr  = get_ia32_attr(node);
	ir_node     *left  = get_irn_n(node, n_ia32_binary_left);
	ir_node     *right = get_irn_n(node, n_ia32_binary_right);

	assert(is_ia32_commutative(node));
	attr->ins_permuted = !attr->ins_permuted;
	set_irn_n(node, n_ia32_binary_left,  right);
	set_irn_n(node, n_ia32_binary_right, left);
}

/**
 * Initializes the nodes attributes.
 */
static void init_ia32_attributes(ir_node *node, arch_irn_flags_t flags,
                                 const arch_register_req_t **in_reqs,
                                 int n_res)
{
	be_info_init_irn(node, flags, in_reqs, n_res);

#ifndef NDEBUG
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->attr_type   |= IA32_ATTR_ia32_attr_t;
#endif
}

static void init_ia32_x87_attributes(ir_node *res)
{
#ifndef NDEBUG
	ia32_attr_t *attr  = get_ia32_attr(res);
	attr->attr_type   |= IA32_ATTR_ia32_x87_attr_t;
#endif
	ir_graph *const irg = get_irn_irg(res);
	ia32_request_x87_sim(irg);
}

static void init_ia32_immediate_attributes(ir_node *res,
                                           x86_imm32_t const *const imm,
                                           bool no_pic_adjust)
{
	ia32_immediate_attr_t *attr = (ia32_immediate_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_immediate_attr_t;
#endif
	attr->imm           = *imm;
	attr->no_pic_adjust = no_pic_adjust;
}

static void init_ia32_call_attributes(ir_node* res, unsigned pop,
                                      ir_type* call_tp)
{
	ia32_call_attr_t *attr = (ia32_call_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_call_attr_t;
#endif
	attr->pop     = pop;
	attr->call_tp = call_tp;
}

static void init_ia32_copyb_attributes(ir_node *res, unsigned size)
{
	ia32_copyb_attr_t *attr = (ia32_copyb_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_copyb_attr_t;
#endif
	attr->size = size;
}

static void init_ia32_condcode_attributes(ir_node *res,
                                          x86_condition_code_t cc)
{
	ia32_condcode_attr_t *attr = (ia32_condcode_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_condcode_attr_t;
#endif
	attr->condition_code = cc;
}

static void init_ia32_climbframe_attributes(ir_node *res, unsigned count)
{
	ia32_climbframe_attr_t *attr = (ia32_climbframe_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_climbframe_attr_t;
#endif
	attr->count = count;
}

static void init_ia32_switch_attributes(ir_node *node,
                                        const ir_switch_table *table)
{
	ia32_switch_attr_t *attr = (ia32_switch_attr_t*) get_irn_generic_attr(node);
#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_switch_attr_t;
#endif
	attr->table = table;

	be_foreach_out(node, o) {
		arch_set_irn_register_req_out(node, o, arch_no_register_req);
	}
}

static void init_ia32_return_attributes(ir_node *node, uint16_t pop)
{
	ia32_return_attr_t *attr = (ia32_return_attr_t*)get_irn_generic_attr(node);
#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_return_attr_t;
#endif
	attr->pop = pop;
}

static int ia32_attrs_equal_(const ia32_attr_t *a, const ia32_attr_t *b)
{
	/* nodes with not yet assigned entities shouldn't be CSEd (important for
	 * unsigned int -> double conversions */
	if (a->frame_use != IA32_FRAME_USE_NONE && !a->frame_ent)
		return false;

	return a->tp == b->tp
	    && a->am_scale == b->am_scale
	    && x86_imm32_equal(&a->am_imm, &b->am_imm)
	    && a->am_sc_no_pic_adjust == b->am_sc_no_pic_adjust
	    && a->ls_mode == b->ls_mode
	    && a->frame_use == b->frame_use
	    && a->frame_ent == b->frame_ent
	    && a->has_except_label == b->has_except_label
	    && a->ins_permuted == b->ins_permuted;
}

/** Compare nodes attributes for all "normal" nodes. */
static int ia32_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_attr_t* attr_a = get_ia32_attr_const(a);
	const ia32_attr_t* attr_b = get_ia32_attr_const(b);
	return ia32_attrs_equal_(attr_a, attr_b);
}

/** Compare node attributes for nodes with condition code. */
static int ia32_condcode_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_condcode_attr_t *attr_a = get_ia32_condcode_attr_const(a);
	const ia32_condcode_attr_t *attr_b = get_ia32_condcode_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->condition_code == attr_b->condition_code;
}

/** Compare node attributes for call nodes. */
static int ia32_call_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_call_attr_t *attr_a = get_ia32_call_attr_const(a);
	const ia32_call_attr_t *attr_b = get_ia32_call_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->pop == attr_b->pop && attr_a->call_tp == attr_b->call_tp;
}

/** Compare node attributes for CopyB nodes. */
static int ia32_copyb_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_copyb_attr_t *attr_a = get_ia32_copyb_attr_const(a);
	const ia32_copyb_attr_t *attr_b = get_ia32_copyb_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->size == attr_b->size;
}

/**
 * Hash function for Immediates
 */
static unsigned ia32_hash_Immediate(const ir_node *irn)
{
	const ia32_immediate_attr_t *a = get_ia32_immediate_attr_const(irn);

	return hash_ptr(a->imm.entity) + (unsigned)a->imm.offset;
}

/** Compare node attributes for Immediates. */
static int ia32_immediate_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_immediate_attr_t *attr_a = get_ia32_immediate_attr_const(a);
	const ia32_immediate_attr_t *attr_b = get_ia32_immediate_attr_const(b);
	return x86_imm32_equal(&attr_a->imm, &attr_b->imm)
		&& attr_a->no_pic_adjust == attr_b->no_pic_adjust;
}

/** Compare node attributes for x87 nodes. */
static int ia32_x87_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_x87_attr_t *attr_a = get_ia32_x87_attr_const(a);
	const ia32_x87_attr_t *attr_b = get_ia32_x87_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr);
}

/** Compare node attributes for ClimbFrame nodes. */
static int ia32_climbframe_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_climbframe_attr_t *attr_a = get_ia32_climbframe_attr_const(a);
	const ia32_climbframe_attr_t *attr_b = get_ia32_climbframe_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->count == attr_b->count;
}

static int ia32_switch_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_switch_attr_t *attr_a = get_ia32_switch_attr_const(a);
	const ia32_switch_attr_t *attr_b = get_ia32_switch_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->table == attr_b->table
	    && attr_a->jump_table == attr_b->jump_table;
}

static int ia32_return_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_return_attr_t *attr_a = get_ia32_return_attr_const(a);
	const ia32_return_attr_t *attr_b = get_ia32_return_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->pop == attr_b->pop;
}

/* copies the ia32 attributes */
static void ia32_copy_attr(ir_graph *irg, const ir_node *old_node,
                           ir_node *new_node)
{
	struct obstack    *obst     = get_irg_obstack(irg);
	const ia32_attr_t *attr_old = get_ia32_attr_const(old_node);
	ia32_attr_t       *attr_new = get_ia32_attr(new_node);
	backend_info_t    *old_info = be_get_info(old_node);
	backend_info_t    *new_info = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy out flags */
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
	new_info->in_reqs = old_info->in_reqs;
	new_info->flags = old_info->flags;
}

static void ia32_init_op(ir_op *op, unsigned latency)
{
	ia32_op_attr_t *attr = OALLOCZ(&opcodes_obst, ia32_op_attr_t);
	attr->latency = latency;
	set_op_attr(op, attr);
}

/* Include the generated constructor functions */
#include "gen_ia32_new_nodes.c.inl"
