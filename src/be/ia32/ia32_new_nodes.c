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
#include "ia32_new_nodes_t.h"

#include "bearch.h"
#include "bedump.h"
#include "beinfo.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_bearch_t.h"
#include "ia32_nodes_attr.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "irverify_t.h"
#include "x86_x87.h"
#include "xmalloc.h"
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>

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
	case iro_ia32_fistp:
	case iro_ia32_fisttp:
	case iro_ia32_fmul:
	case iro_ia32_fpop:
	case iro_ia32_fst:
	case iro_ia32_fstp:
	case iro_ia32_fsub:
	case iro_ia32_fxch:
		return true;

	default:
		return false;
	}
}

#ifndef NDEBUG
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
#endif

static void ia32_dump_immediate(FILE *const F, ir_entity *const entity,
                                int32_t const offset)
{
	if (entity) {
		ir_fprintf(F, "%F", entity);
		if (offset != 0)
			fprintf(F, "%+" PRId32, offset);
	} else {
		fprintf(F, "%" PRId32, offset);
	}
}

void ia32_dump_node(FILE *F, const ir_node *n, dump_reason_t reason)
{
	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));

			if (is_ia32_Immediate(n) || is_ia32_Const(n)) {
				ia32_immediate_attr_t const *const attr
					= get_ia32_immediate_attr_const(n);
				fputc(' ', F);
				ia32_dump_immediate(F, attr->imm.entity, attr->imm.offset);
			} else {
				ia32_attr_t const *const attr   = get_ia32_attr_const(n);
				int32_t            const offset = attr->addr.immediate.offset;
				ir_entity         *const entity = attr->addr.immediate.entity;
				if (entity || offset != 0) {
					fputs(" [", F);
					ia32_dump_immediate(F, entity, offset);
					fputc(']', F);
				}
			}
			break;

		case dump_node_mode_txt:
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

			const ia32_attr_t *attr = get_ia32_attr_const(n);
			fputs("AM immediate = ", F);
			x86_dump_imm32(&attr->addr.immediate, F);
			fputc('\n', F);

			/* dump AM scale */
			fprintf(F, "AM log_scale = %u\n", attr->addr.log_scale);

			/* dump pn code */
			if (has_ia32_condcode_attr(n)) {
				const char *cc_name = condition_code_name(get_ia32_condcode(n));
				if (cc_name) {
					fprintf(F, "condition_code = %s\n", cc_name);
				} else {
					fprintf(F, "condition_code = <invalid (0x%X)>\n",
					        (unsigned)get_ia32_condcode(n));
				}
				fprintf(F, "ins_permuted = %s\n",
				        be_dump_yesno(attr->ins_permuted));
			} else if (is_ia32_CopyB(n) || is_ia32_CopyB_i(n)) {
				fprintf(F, "size = %u\n", get_ia32_copyb_size(n));
			} else if (has_ia32_x87_attr(n)) {
				ia32_x87_attr_t const *const attr = get_ia32_x87_attr_const(n);
				fprintf(F, "explicit operand = %s\n",
				        be_dump_reg_name(attr->x87.reg));
				fprintf(F, "result to explicit operand = %s\n",
				        be_dump_yesno(attr->x87.res_in_reg));
				fprintf(F, "pop = %s\n", be_dump_yesno(attr->x87.pop));
			}

			fprintf(F, "commutative = %s\n",
			        be_dump_yesno(is_ia32_commutative(n)));
			fprintf(F, "sign_extend = %s\n", be_dump_yesno(attr->sign_extend));
			fprintf(F, "use_8bit_high = %s\n",
			        be_dump_yesno(attr->use_8bit_high));
			fprintf(F, "is reload = %s\n", be_dump_yesno(is_ia32_is_reload(n)));
			fprintf(F, "latency = %u\n", get_ia32_latency(n));

			/* dump modes */
			fprintf(F, "size = %u\n", x86_bytes_from_size(attr->size) * 8);

#ifndef NDEBUG
			/* dump frame entity */
			fprintf(F, "frame use = %s\n", get_frame_use_str(n));
			if (attr->old_frame_ent != NULL) {
				fprintf(F, "frame entity = ");
				ir_entity *entity = attr->addr.immediate.entity;
				if (entity != NULL) {
					ir_fprintf(F, "%+F", entity);
				} else {
					fprintf(F, "n/a");
				}
				fprintf(F, "\n");
			}
#endif
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
	return (ia32_attr_t const*)get_irn_generic_attr_const(node);
}

ia32_x87_attr_t *get_ia32_x87_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_x87_attr_t, attr);
}

const ia32_x87_attr_t *get_ia32_x87_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_x87_attr_t, attr);
}

ia32_immediate_attr_t *get_ia32_immediate_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_immediate_attr_t, attr);
}

const ia32_immediate_attr_t *get_ia32_immediate_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_immediate_attr_t, attr);
}

ia32_condcode_attr_t *get_ia32_condcode_attr(ir_node *node)
{
	assert(has_ia32_condcode_attr(node));
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_condcode_attr_t, attr);
}

const ia32_condcode_attr_t *get_ia32_condcode_attr_const(const ir_node *node)
{
	assert(has_ia32_condcode_attr(node));
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_condcode_attr_t, attr);
}

ia32_switch_attr_t *get_ia32_switch_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_switch_attr_t, attr);
}

const ia32_switch_attr_t *get_ia32_switch_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_switch_attr_t, attr);
}

ia32_return_attr_t *get_ia32_return_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_return_attr_t, attr);
}

const ia32_return_attr_t *get_ia32_return_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_return_attr_t, attr);
}

ia32_call_attr_t *get_ia32_call_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_call_attr_t, attr);
}

const ia32_call_attr_t *get_ia32_call_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_call_attr_t, attr);
}

ia32_copyb_attr_t *get_ia32_copyb_attr(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	return CAST_IA32_ATTR(ia32_copyb_attr_t, attr);
}

const ia32_copyb_attr_t *get_ia32_copyb_attr_const(const ir_node *node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return CONST_CAST_IA32_ATTR(ia32_copyb_attr_t, attr);
}

ia32_op_type_t get_ia32_op_type(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return (ia32_op_type_t)attr->tp;
}

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

void set_ia32_am_support(ir_node *node, ia32_am_type_t arity)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->am_arity = arity;
}

void ia32_copy_am_attrs(ir_node *to, const ir_node *from)
{
	ia32_attr_t const *const from_attr = get_ia32_attr_const(from);
	ia32_attr_t       *const to_attr   = get_ia32_attr(to);
	to_attr->addr      = from_attr->addr;
	to_attr->frame_use = from_attr->frame_use;
	to_attr->size      = from_attr->size;

#ifndef NDEBUG
	to_attr->old_frame_ent = from_attr->old_frame_ent;
#endif
}

void set_ia32_commutative(ir_node *node)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->is_commutative = 1;
}

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

unsigned get_ia32_latency(const ir_node *node)
{
	assert(is_ia32_irn(node));
	const ir_op *op               = get_irn_op(node);
	const ia32_op_attr_t *op_attr = (ia32_op_attr_t*) get_op_attr(op);
	return op_attr->latency;
}

x86_condition_code_t get_ia32_condcode(const ir_node *node)
{
	const ia32_condcode_attr_t *attr = get_ia32_condcode_attr_const(node);
	return attr->condition_code;
}

void set_ia32_condcode(ir_node *node, x86_condition_code_t code)
{
	ia32_condcode_attr_t *attr = get_ia32_condcode_attr(node);
	attr->condition_code = code;
}

unsigned get_ia32_copyb_size(const ir_node *node)
{
	const ia32_copyb_attr_t *attr = get_ia32_copyb_attr_const(node);
	return attr->size;
}

unsigned get_ia32_exc_label(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->has_except_label;
}

void set_ia32_exc_label(ir_node *node, unsigned flag)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->has_except_label = flag;
}

ir_label_t get_ia32_exc_label_id(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);

	assert(attr->has_except_label);
	return attr->exc_label;
}

void set_ia32_exc_label_id(ir_node *node, ir_label_t id)
{
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(attr->has_except_label);
	attr->exc_label = id;
}

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

void init_ia32_attributes(ir_node *node, x86_insn_size_t const size)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->size             = size;
	attr->addr.base_input  = n_ia32_base;
	attr->addr.index_input = n_ia32_index;
	attr->addr.mem_input   = n_ia32_mem;

#ifndef NDEBUG
	attr->attr_type |= IA32_ATTR_ia32_attr_t;
#endif
}

void init_ia32_x87_attributes(ir_node *res)
{
#ifndef NDEBUG
	ia32_attr_t *attr = get_ia32_attr(res);
	attr->attr_type |= IA32_ATTR_ia32_x87_attr_t;
#endif
	ir_graph *const irg = get_irn_irg(res);
	ia32_request_x87_sim(irg);
}

void init_ia32_immediate_attributes(ir_node *res, x86_imm32_t const *const imm)
{
	ia32_immediate_attr_t *attr = (ia32_immediate_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_immediate_attr_t;
#endif
	attr->imm           = *imm;
}

void init_ia32_call_attributes(ir_node* res, uint8_t pop, uint8_t n_reg_results)
{
	ia32_call_attr_t *attr = (ia32_call_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_call_attr_t;
#endif
	attr->pop           = pop;
	attr->n_reg_results = n_reg_results;
}

void init_ia32_copyb_attributes(ir_node *res, unsigned size)
{
	ia32_copyb_attr_t *attr = (ia32_copyb_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_copyb_attr_t;
#endif
	attr->size = size;
}

void init_ia32_condcode_attributes(ir_node *res, x86_condition_code_t cc)
{
	ia32_condcode_attr_t *attr = (ia32_condcode_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_condcode_attr_t;
#endif
	attr->condition_code = cc;
}

void init_ia32_switch_attributes(ir_node *node,
                                 ir_switch_table const *const table,
                                 ir_entity const *const table_entity)
{
	ia32_switch_attr_t *attr = (ia32_switch_attr_t*) get_irn_generic_attr(node);
#ifndef NDEBUG
	attr->attr.attr_type |= IA32_ATTR_ia32_switch_attr_t;
#endif
	be_switch_attr_init(node, &attr->swtch, table, table_entity);
}

void init_ia32_return_attributes(ir_node *node, uint16_t pop)
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
	if (a->addr.immediate.kind == X86_IMM_FRAMEENT
	 && a->addr.immediate.entity == NULL)
		return false;

	return a->tp == b->tp
	    && x86_addrs_equal(&a->addr, &b->addr)
	    && a->size == b->size
	    && a->frame_use == b->frame_use
	    && a->has_except_label == b->has_except_label
	    && a->ins_permuted == b->ins_permuted
	    && a->sign_extend == b->sign_extend
	    && a->use_8bit_high == b->use_8bit_high;
}

int ia32_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_attr_t* attr_a = get_ia32_attr_const(a);
	const ia32_attr_t* attr_b = get_ia32_attr_const(b);
	return ia32_attrs_equal_(attr_a, attr_b);
}

int ia32_condcode_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_condcode_attr_t *attr_a = get_ia32_condcode_attr_const(a);
	const ia32_condcode_attr_t *attr_b = get_ia32_condcode_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->condition_code == attr_b->condition_code;
}

int ia32_call_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_call_attr_t *attr_a = get_ia32_call_attr_const(a);
	const ia32_call_attr_t *attr_b = get_ia32_call_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->pop == attr_b->pop
	    && attr_a->n_reg_results == attr_b->n_reg_results;
}

int ia32_copyb_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_copyb_attr_t *attr_a = get_ia32_copyb_attr_const(a);
	const ia32_copyb_attr_t *attr_b = get_ia32_copyb_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->size == attr_b->size;
}

unsigned ia32_hash_Immediate(const ir_node *irn)
{
	const ia32_immediate_attr_t *a = get_ia32_immediate_attr_const(irn);
	return hash_ptr(a->imm.entity) + (unsigned)a->imm.offset;
}

int ia32_immediate_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_immediate_attr_t *attr_a = get_ia32_immediate_attr_const(a);
	const ia32_immediate_attr_t *attr_b = get_ia32_immediate_attr_const(b);
	return x86_imm32_equal(&attr_a->imm, &attr_b->imm);
}

int ia32_x87_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_x87_attr_t *attr_a = get_ia32_x87_attr_const(a);
	const ia32_x87_attr_t *attr_b = get_ia32_x87_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr);
}

int ia32_switch_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_switch_attr_t *attr_a = get_ia32_switch_attr_const(a);
	const ia32_switch_attr_t *attr_b = get_ia32_switch_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && be_switch_attrs_equal(&attr_a->swtch, &attr_b->swtch);
}

int ia32_return_attrs_equal(const ir_node *a, const ir_node *b)
{
	const ia32_return_attr_t *attr_a = get_ia32_return_attr_const(a);
	const ia32_return_attr_t *attr_b = get_ia32_return_attr_const(b);
	return ia32_attrs_equal_(&attr_a->attr, &attr_b->attr)
	    && attr_a->pop == attr_b->pop;
}

void ia32_init_op(ir_op *op, unsigned latency)
{
	ia32_op_attr_t *attr = OALLOCZ(&opcodes_obst, ia32_op_attr_t);
	attr->latency = latency;
	set_op_attr(op, attr);
}
