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
#include "error.h"
#include "xmalloc.h"

#include "bearch.h"
#include "beinfo.h"

#include "bearch_ia32_t.h"
#include "ia32_common_transform.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"

struct obstack opcodes_obst;

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
				if (attr->symconst) {
					fputs(get_entity_name(attr->symconst), F);
				}
				if (attr->offset != 0 || attr->symconst == NULL) {
					if (attr->offset > 0 && attr->symconst != NULL) {
						fputc('+', F);
					}
					fprintf(F, "%ld", attr->offset);
					if (attr->no_pic_adjust) {
						fputs("(no_pic_adjust)", F);
					}
				}
			}
			else {
				const ia32_attr_t *attr = get_ia32_attr_const(n);

				if (attr->am_sc != NULL || attr->am_offs != 0)
					fputs(" [", F);

				if (attr->am_sc != NULL) {
					fputs(get_entity_name(attr->am_sc), F);
					if (attr->data.am_sc_no_pic_adjust) {
						fputs("(no_pic_adjust)", F);
					}
				}
				if (attr->am_offs != 0) {
					if (attr->am_offs > 0 && attr->am_sc != NULL) {
						fputc('+', F);
					}
					fprintf(F, "%d", attr->am_offs);
				}

				if (attr->am_sc != NULL || attr->am_offs != 0)
					fputc(']', F);
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
			arch_dump_reqs_and_registers(F, n);

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

			/* dump AM symconst */
			if (get_ia32_am_sc(n) != NULL) {
				ir_entity *ent = get_ia32_am_sc(n);
				ident *id = get_entity_ld_ident(ent);
				fprintf(F, "AM symconst = %s\n", get_id_str(id));
			}

			/* dump AM scale */
			fprintf(F, "AM scale = %u\n", get_ia32_am_scale(n));

			/* dump pn code */
			if (is_ia32_CMovcc(n) || is_ia32_Setcc(n) || is_ia32_Jcc(n)) {
				const ia32_attr_t *attr = get_ia32_attr_const(n);
				fprintf(F, "condition_code = 0x%X\n", (unsigned)get_ia32_condcode(n));
				fprintf(F, "ins_permuted = %u\n", (unsigned)attr->data.ins_permuted);
			}
			else if (is_ia32_CopyB(n) || is_ia32_CopyB_i(n)) {
				fprintf(F, "size = %u\n", get_ia32_copyb_size(n));
			}

			fprintf(F, "use_frame = %d\n",     is_ia32_use_frame(n));
			fprintf(F, "commutative = %d\n",   is_ia32_commutative(n));
			fprintf(F, "need stackent = %d\n", is_ia32_need_stackent(n));
			fprintf(F, "is reload = %d\n",     is_ia32_is_reload(n));
			fprintf(F, "latency = %u\n",       get_ia32_latency(n));

			/* dump frame entity */
			fprintf(F, "frame entity = ");
			if (get_ia32_frame_ent(n)) {
				ir_fprintf(F, "%+F", get_ia32_frame_ent(n));
			}
			else {
				fprintf(F, "n/a");
			}
			fprintf(F, "\n");

			/* dump modes */
			fprintf(F, "ls_mode = ");
			if (get_ia32_ls_mode(n)) {
				ir_fprintf(F, "%+F", get_ia32_ls_mode(n));
			}
			else {
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

const ia32_asm_attr_t *get_ia32_asm_attr_const(const ir_node *node)
{
	const ia32_attr_t     *attr     = get_ia32_attr_const(node);
	const ia32_asm_attr_t *asm_attr = CONST_CAST_IA32_ATTR(ia32_asm_attr_t, attr);

	return asm_attr;
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
	ia32_attr_t          *attr    = get_ia32_attr(node);
	ia32_condcode_attr_t *cc_attr = CAST_IA32_ATTR(ia32_condcode_attr_t, attr);

	return cc_attr;
}

const ia32_condcode_attr_t *get_ia32_condcode_attr_const(const ir_node *node)
{
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
	return (ia32_op_type_t)attr->data.tp;
}

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.tp     = tp;
}

ia32_am_type_t get_ia32_am_support(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return (ia32_am_type_t)attr->data.am_arity;
}

/**
 * Sets the supported address mode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t arity)
{
	ia32_attr_t *attr   = get_ia32_attr(node);
	attr->data.am_arity = arity;
}

/**
 * Gets the address mode offset as int.
 */
int get_ia32_am_offs_int(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_offs;
}

/**
 * Sets the address mode offset from an int.
 */
void set_ia32_am_offs_int(ir_node *node, int offset)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_offs = offset;
}

void add_ia32_am_offs_int(ir_node *node, int offset)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_offs += offset;
}

/**
 * Returns the symconst entity associated to address mode.
 */
ir_entity *get_ia32_am_sc(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->am_sc;
}

/**
 * Sets the symconst entity associated to address mode.
 */
void set_ia32_am_sc(ir_node *node, ir_entity *entity)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->am_sc       = entity;
}

void set_ia32_am_tls_segment(ir_node *node, bool value)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.am_tls_segment = value;
}

bool get_ia32_am_tls_segment(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.am_tls_segment;
}

/**
 * Gets the addr mode const.
 */
unsigned get_ia32_am_scale(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.am_scale;
}

/**
 * Sets the index register scale for address mode.
 */
void set_ia32_am_scale(ir_node *node, unsigned scale)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	assert(scale <= 3 && "AM scale out of range [0 ... 3]");
	attr->data.am_scale = scale;
}

void ia32_copy_am_attrs(ir_node *to, const ir_node *from)
{
	set_ia32_ls_mode(to, get_ia32_ls_mode(from));
	set_ia32_am_scale(to, get_ia32_am_scale(from));
	set_ia32_am_sc(to, get_ia32_am_sc(from));
	add_ia32_am_offs_int(to, get_ia32_am_offs_int(from));
	set_ia32_frame_ent(to, get_ia32_frame_ent(from));
	if (is_ia32_use_frame(from))
		set_ia32_use_frame(to);
}

/**
 * Sets the uses_frame flag.
 */
void set_ia32_use_frame(ir_node *node)
{
	ia32_attr_t *attr    = get_ia32_attr(node);
	attr->data.use_frame = 1;
}

/**
 * Clears the uses_frame flag.
 */
void clear_ia32_use_frame(ir_node *node)
{
	ia32_attr_t *attr    = get_ia32_attr(node);
	attr->data.use_frame = 0;
}

/**
 * Gets the uses_frame flag.
 */
int is_ia32_use_frame(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.use_frame;
}

/**
 * Sets node to commutative.
 */
void set_ia32_commutative(ir_node *node)
{
	ia32_attr_t *attr         = get_ia32_attr(node);
	attr->data.is_commutative = 1;
}

/**
 * Sets node to non-commutative.
 */
void clear_ia32_commutative(ir_node *node)
{
	ia32_attr_t *attr         = get_ia32_attr(node);
	attr->data.is_commutative = 0;
}

/**
 * Checks if node is commutative.
 */
int is_ia32_commutative(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.is_commutative;
}

void set_ia32_need_stackent(ir_node *node)
{
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.need_stackent = 1;
}

void clear_ia32_need_stackent(ir_node *node)
{
	ia32_attr_t *attr     = get_ia32_attr(node);
	attr->data.need_stackent = 0;
}

int is_ia32_need_stackent(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.need_stackent;
}

void set_ia32_is_reload(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.is_reload = 1;
}

int is_ia32_is_reload(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.is_reload;
}

void set_ia32_is_spill(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.is_spill = 1;
}

int is_ia32_is_spill(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.is_spill;
}

void set_ia32_is_remat(ir_node *node)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.is_remat = 1;
}

int is_ia32_is_remat(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->data.is_remat;
}

/**
 * Gets the mode of the stored/loaded value (only set for Store/Load)
 */
ir_mode *get_ia32_ls_mode(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);
	return attr->ls_mode;
}

/**
 * Sets the mode of the stored/loaded value (only set for Store/Load)
 */
void set_ia32_ls_mode(ir_node *node, ir_mode *mode)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->ls_mode     = mode;
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
	if (ent != NULL)
		set_ia32_use_frame(node);
	else
		clear_ia32_use_frame(node);
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
	return attr->data.has_except_label;
}

/**
 * Set the exception label attribute.
 */
void set_ia32_exc_label(ir_node *node, unsigned flag)
{
	ia32_attr_t *attr = get_ia32_attr(node);
	attr->data.has_except_label = flag;
}

/**
 * Return the exception label id.
 */
ir_label_t get_ia32_exc_label_id(const ir_node *node)
{
	const ia32_attr_t *attr = get_ia32_attr_const(node);

	assert(attr->data.has_except_label);
	return attr->exc_label;
}

/**
 * Assign the exception label id.
 */
void set_ia32_exc_label_id(ir_node *node, ir_label_t id)
{
	ia32_attr_t *attr = get_ia32_attr(node);

	assert(attr->data.has_except_label);
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
	attr->data.ins_permuted = !attr->data.ins_permuted;
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
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	backend_info_t *info;

	arch_set_irn_flags(node, flags);
	arch_set_irn_register_reqs_in(node, in_reqs);

#ifndef NDEBUG
	ia32_attr_t *attr  = get_ia32_attr(node);
	attr->attr_type   |= IA32_ATTR_ia32_attr_t;
#endif

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_DZ(reg_out_info_t, obst, n_res);
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

static void init_ia32_asm_attributes(ir_node *res)
{
#ifndef NDEBUG
	ia32_attr_t *attr  = get_ia32_attr(res);
	attr->attr_type   |= IA32_ATTR_ia32_asm_attr_t;
#endif

	ir_graph *const irg = get_irn_irg(res);
	ia32_request_x87_sim(irg); /* asm might have fp operands. */
}

static void init_ia32_immediate_attributes(ir_node *res, ir_entity *symconst,
                                           int no_pic_adjust, long offset)
{
	ia32_immediate_attr_t *attr = (ia32_immediate_attr_t*)get_irn_generic_attr(res);

#ifndef NDEBUG
	attr->attr.attr_type  |= IA32_ATTR_ia32_immediate_attr_t;
#endif
	attr->symconst      = symconst;
	attr->no_pic_adjust = no_pic_adjust;
	attr->offset        = offset;
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

/* default compare operation to compare attributes */
static int ia32_compare_attr(const ia32_attr_t *a, const ia32_attr_t *b)
{
	if (a->data.tp != b->data.tp)
		return 1;

	if (a->data.am_scale != b->data.am_scale
	    || a->am_offs != b->am_offs
	    || a->am_sc != b->am_sc
		|| a->data.am_sc_no_pic_adjust != b->data.am_sc_no_pic_adjust
	    || a->ls_mode != b->ls_mode)
		return 1;

	/* nodes with not yet assigned entities shouldn't be CSEd (important for
	 * unsigned int -> double conversions */
	if (a->data.use_frame && a->frame_ent == NULL)
		return 1;
	if (b->data.use_frame && b->frame_ent == NULL)
		return 1;

	if (a->data.use_frame != b->data.use_frame
	    || a->frame_ent != b->frame_ent)
		return 1;

	if (a->data.has_except_label != b->data.has_except_label)
		return 1;

	if (a->data.ins_permuted != b->data.ins_permuted)
		return 1;

	return 0;
}

/** Compare nodes attributes for all "normal" nodes. */
static int ia32_compare_nodes_attr(const ir_node *a, const ir_node *b)
{
	const ia32_attr_t* attr_a = get_ia32_attr_const(a);
	const ia32_attr_t* attr_b = get_ia32_attr_const(b);

	return ia32_compare_attr(attr_a, attr_b);
}

/** Compare node attributes for nodes with condition code. */
static int ia32_compare_condcode_attr(const ir_node *a, const ir_node *b)
{
	const ia32_condcode_attr_t *attr_a;
	const ia32_condcode_attr_t *attr_b;

	if (ia32_compare_nodes_attr(a, b))
		return 1;

	attr_a = get_ia32_condcode_attr_const(a);
	attr_b = get_ia32_condcode_attr_const(b);

	if (attr_a->condition_code != attr_b->condition_code)
		return 1;

	return 0;
}

/** Compare node attributes for call nodes. */
static int ia32_compare_call_attr(const ir_node *a, const ir_node *b)
{
	const ia32_call_attr_t *attr_a;
	const ia32_call_attr_t *attr_b;

	if (ia32_compare_nodes_attr(a, b))
		return 1;

	attr_a = get_ia32_call_attr_const(a);
	attr_b = get_ia32_call_attr_const(b);

	if (attr_a->pop != attr_b->pop)
		return 1;

	if (attr_a->call_tp != attr_b->call_tp)
		return 1;

	return 0;
}

/** Compare node attributes for CopyB nodes. */
static int ia32_compare_copyb_attr(const ir_node *a, const ir_node *b)
{
	const ia32_copyb_attr_t *attr_a;
	const ia32_copyb_attr_t *attr_b;

	if (ia32_compare_nodes_attr(a, b))
		return 1;

	attr_a = get_ia32_copyb_attr_const(a);
	attr_b = get_ia32_copyb_attr_const(b);

	if (attr_a->size != attr_b->size)
		return 1;

	return 0;
}


/** Compare ASM node attributes. */
static int ia32_compare_asm_attr(const ir_node *a, const ir_node *b)
{
	const ia32_asm_attr_t *attr_a;
	const ia32_asm_attr_t *attr_b;

	if (ia32_compare_nodes_attr(a, b))
		return 1;

	attr_a = get_ia32_asm_attr_const(a);
	attr_b = get_ia32_asm_attr_const(b);

	if (attr_a->asm_text != attr_b->asm_text)
		return 1;

	return 0;
}

/**
 * Hash function for Immediates
 */
static unsigned ia32_hash_Immediate(const ir_node *irn)
{
	const ia32_immediate_attr_t *a = get_ia32_immediate_attr_const(irn);

	return hash_ptr(a->symconst) + a->offset;
}

/** Compare node attributes for Immediates. */
static int ia32_compare_immediate_attr(const ir_node *a, const ir_node *b)
{
	const ia32_immediate_attr_t *attr_a = get_ia32_immediate_attr_const(a);
	const ia32_immediate_attr_t *attr_b = get_ia32_immediate_attr_const(b);

	if (attr_a->symconst != attr_b->symconst
		|| attr_a->no_pic_adjust != attr_b->no_pic_adjust
		|| attr_a->offset != attr_b->offset) {
		return 1;
	}

	return 0;
}

/** Compare node attributes for x87 nodes. */
static int ia32_compare_x87_attr(const ir_node *a, const ir_node *b)
{
	return ia32_compare_nodes_attr(a, b);
}

/** Compare node attributes for ClimbFrame nodes. */
static int ia32_compare_climbframe_attr(const ir_node *a, const ir_node *b)
{
	const ia32_climbframe_attr_t *attr_a;
	const ia32_climbframe_attr_t *attr_b;

	if (ia32_compare_nodes_attr(a, b))
		return 1;

	attr_a = get_ia32_climbframe_attr_const(a);
	attr_b = get_ia32_climbframe_attr_const(b);

	if (attr_a->count != attr_b->count)
		return 1;

	return 0;
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
