/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   emit assembler for a backend graph
 * @version $Id: amd64_emitter.c 26746 2009-11-27 08:53:15Z matze $
 */
#include "config.h"

#include <limits.h>

#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog.h"

#include "../besched.h"
#include "../begnuas.h"
#include "../beblocksched.h"
#include "../be_dbgout.h"

#include "amd64_emitter.h"
#include "gen_amd64_emitter.h"
#include "amd64_nodes_attr.h"
#include "amd64_new_nodes.h"

#define SNPRINTF_BUF_LEN 128

#include "../benode.h"

/**
 * Returns the register at in position pos.
 */
static const arch_register_t *get_in_reg(const ir_node *node, int pos)
{
	ir_node                *op;
	const arch_register_t  *reg = NULL;

	assert(get_irn_arity(node) > pos && "Invalid IN position");

	/* The out register of the operator at position pos is the
	   in register we need. */
	op = get_irn_n(node, pos);

	reg = arch_get_irn_register(op);

	assert(reg && "no in register found");
	return reg;
}

/**
 * Returns the register at out position pos.
 */
static const arch_register_t *get_out_reg(const ir_node *node, int pos)
{
	ir_node                *proj;
	const arch_register_t  *reg = NULL;

	/* 1st case: irn is not of mode_T, so it has only                 */
	/*           one OUT register -> good                             */
	/* 2nd case: irn is of mode_T -> collect all Projs and ask the    */
	/*           Proj with the corresponding projnum for the register */

	if (get_irn_mode(node) != mode_T) {
		reg = arch_get_irn_register(node);
	} else if (is_amd64_irn(node)) {
		reg = arch_irn_get_register(node, pos);
	} else {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			proj = get_edge_src_irn(edge);
			assert(is_Proj(proj) && "non-Proj from mode_T node");
			if (get_Proj_proj(proj) == pos) {
				reg = arch_get_irn_register(proj);
				break;
			}
		}
	}

	assert(reg && "no out register found");
	return reg;
}

/*************************************************************
 *             _       _    __   _          _
 *            (_)     | |  / _| | |        | |
 *  _ __  _ __ _ _ __ | |_| |_  | |__   ___| |_ __   ___ _ __
 * | '_ \| '__| | '_ \| __|  _| | '_ \ / _ \ | '_ \ / _ \ '__|
 * | |_) | |  | | | | | |_| |   | | | |  __/ | |_) |  __/ |
 * | .__/|_|  |_|_| |_|\__|_|   |_| |_|\___|_| .__/ \___|_|
 * | |                                       | |
 * |_|                                       |_|
 *************************************************************/

void amd64_emit_register(const arch_register_t *reg)
{
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

void amd64_emit_immediate(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const (node);
	be_emit_char('$');
	be_emit_irprintf("0x%X", attr->ext.imm_value);
}

void amd64_emit_source_register(const ir_node *node, int pos)
{
	amd64_emit_register(get_in_reg(node, pos));
}

void amd64_emit_dest_register(const ir_node *node, int pos)
{
	amd64_emit_register(get_out_reg(node, pos));
}

/**
 * Returns the target label for a control flow node.
 */
/*
static void amd64_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_irn_link(node);

	be_emit_irprintf("BLOCK_%ld", get_irn_node_nr(block));
}
*/

/***********************************************************************************
 *                  _          __                                             _
 *                 (_)        / _|                                           | |
 *  _ __ ___   __ _ _ _ __   | |_ _ __ __ _ _ __ ___   _____      _____  _ __| | __
 * | '_ ` _ \ / _` | | '_ \  |  _| '__/ _` | '_ ` _ \ / _ \ \ /\ / / _ \| '__| |/ /
 * | | | | | | (_| | | | | | | | | | | (_| | | | | | |  __/\ V  V / (_) | |  |   <
 * |_| |_| |_|\__,_|_|_| |_| |_| |_|  \__,_|_| |_| |_|\___| \_/\_/ \___/|_|  |_|\_\
 *
 ***********************************************************************************/

/**
 * Default emitter for anything that we don't want to generate code for.
 */
static void emit_nothing(const ir_node *node)
{
	(void) node;
}

/**
 * Emit a SymConst.
 */
static void emit_amd64_SymConst(const ir_node *irn)
{
	const amd64_SymConst_attr_t *attr = get_amd64_SymConst_attr_const(irn);
//	sym_or_tv_t key, *entry;
//	unsigned label;
//
//	key.u.id     = get_entity_ld_ident(attr->entity);
//	key.is_ident = 1;
//	key.label    = 0;
//	entry = (sym_or_tv_t *)set_insert(sym_or_tv, &key, sizeof(key), HASH_PTR(key.u.generic));
//	if (entry->label == 0) {
//		/* allocate a label */
//		entry->label = get_unique_label();
//	}
//	label = entry->label;

	be_gas_emit_entity(attr->entity);
	be_emit_char(':');
	be_emit_finish_line_gas(irn);
	be_emit_cstring("\t.long 0x0");
	be_emit_finish_line_gas(irn);
}

/**
 * Returns the next block in a block schedule.
 */
static ir_node *sched_next_block(const ir_node *block)
{
    return get_irn_link(block);
}

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn)
{
	return get_irn_link(irn);
}

/**
 * Emit the target label for a control flow node.
 */
static void amd64_emit_cfop_target(const ir_node *irn)
{
	ir_node *block = get_cfop_target_block(irn);

	be_gas_emit_block_name(block);
}

/**
 * Emit a Jmp.
 */
static void emit_amd64_Jmp(const ir_node *node)
{
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = sched_next_block(block);
	if (get_cfop_target_block(node) != next_block) {
		be_emit_cstring("\tjmp ");
		amd64_emit_cfop_target(node);
	} else {
		be_emit_cstring("\t/* fallthrough to ");
		amd64_emit_cfop_target(node);
		be_emit_cstring(" */");
	}
	be_emit_finish_line_gas(node);
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_amd64_Jcc(const ir_node *irn)
{
	const ir_edge_t      *edge;
	const ir_node        *proj_true  = NULL;
	const ir_node        *proj_false = NULL;
	const ir_node        *block;
	const ir_node        *next_block;
	const char           *suffix;
	const amd64_attr_t   *attr      = get_irn_generic_attr_const(irn);
	int                   proj_num  = attr->ext.pnc;
	ir_node              *op1       = get_irn_n(irn, 0);
	const amd64_attr_t   *cmp_attr  = get_irn_generic_attr_const(op1);
	bool                  is_signed = !cmp_attr->data.cmp_unsigned;

	assert(is_amd64_Cmp(op1));

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	if (cmp_attr->data.ins_permuted) {
		proj_num = get_mirrored_pnc(proj_num);
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_block = sched_next_block(block);

	assert(proj_num != pn_Cmp_False);
	assert(proj_num != pn_Cmp_True);

	if (get_cfop_target_block(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		proj_num   = get_negated_pnc(proj_num, mode_Lu);
	}

	switch (proj_num) {
		case pn_Cmp_Eq:  suffix = "e"; break;
		case pn_Cmp_Lt:  suffix = is_signed ? "l"  : "b"; break;
		case pn_Cmp_Le:  suffix = is_signed ? "le" : "be"; break;
		case pn_Cmp_Gt:  suffix = is_signed ? "g"  : "o"; break;
		case pn_Cmp_Ge:  suffix = is_signed ? "ge" : "oe"; break;
		case pn_Cmp_Lg:  suffix = "ne"; break;
		case pn_Cmp_Leg: suffix = "mp"; break;
		default: panic("Cmp has unsupported pnc");
	}

	/* emit the true proj */
	be_emit_irprintf("\tj%s ", suffix);
	amd64_emit_cfop_target(proj_true);
	be_emit_finish_line_gas(proj_true);

	if (get_cfop_target_block(proj_false) == next_block) {
		be_emit_cstring("\t/* fallthrough to ");
		amd64_emit_cfop_target(proj_false);
		be_emit_cstring(" */");
		be_emit_finish_line_gas(proj_false);
	} else {
		be_emit_cstring("\tjmp ");
		amd64_emit_cfop_target(proj_false);
		be_emit_finish_line_gas(proj_false);
	}
}

/**
 * Emits code for a call.
 */
static void emit_be_Call(const ir_node *node)
{
	ir_entity *entity = be_Call_get_entity (node);

	if (entity) {
		be_emit_cstring("\tcall ");
		be_gas_emit_entity (be_Call_get_entity(node));
		be_emit_finish_line_gas(node);
	} else {
		be_emit_pad_comment();
		be_emit_cstring("/* FIXME: call NULL entity?! */\n");
	}
}

/**
 * emit copy node
 */
static void emit_be_Copy(const ir_node *irn)
{
	ir_mode *mode = get_irn_mode(irn);

	if (get_in_reg(irn, 0) == get_out_reg(irn, 0)) {
		/* omitted Copy */
		return;
	}

	if (mode_is_float(mode)) {
		panic("emit_be_Copy: move not supported for FP");
	} else if (mode_is_data(mode)) {
		be_emit_cstring("\tmov ");
		amd64_emit_source_register(irn, 0);
		be_emit_cstring(", ");
		amd64_emit_dest_register(irn, 0);
		be_emit_finish_line_gas(irn);
	} else {
		panic("emit_be_Copy: move not supported for this mode");
	}
}

static void emit_amd64_FrameAddr(const ir_node *irn)
{
	const amd64_SymConst_attr_t *attr = get_irn_generic_attr_const(irn);

	be_emit_cstring("\tmov ");
	amd64_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	amd64_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);

	be_emit_cstring("\tadd ");
	be_emit_irprintf("$0x%X", attr->fp_offset);
	be_emit_cstring(", ");
	amd64_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}

/**
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *node)
{
	int offs = be_get_IncSP_offset(node);

	if (offs == 0)
		return;

	if (offs > 0) {
		be_emit_irprintf("\tsub ");
		amd64_emit_dest_register(node, 0);
		be_emit_irprintf(", $%u", offs);
		be_emit_finish_line_gas(node);
	} else {
		be_emit_irprintf("\tadd ");
		amd64_emit_dest_register(node, 0);
		be_emit_irprintf(", $%u", -offs);
		be_emit_finish_line_gas(node);
	}
}

/**
 * Emits code for a return.
 */
static void emit_be_Return(const ir_node *node)
{
	be_emit_cstring("\tret");
	be_emit_finish_line_gas(node);
}


static void emit_amd64_binop_op(const ir_node *irn, int second_op)
{
	if (irn->op == op_amd64_Add) {
		be_emit_cstring("\tadd ");
	} else if (irn->op == op_amd64_Mul) {
		be_emit_cstring("\tmul ");
	} else if (irn->op == op_amd64_Sub) {
		be_emit_cstring("\tsub ");
	}

	amd64_emit_dest_register(irn, 0);
	be_emit_cstring(", ");
	amd64_emit_source_register(irn, second_op);
	be_emit_finish_line_gas(irn);
}

/**
 * Emits an arithmetic operation that handles arbitraty input registers.
 */
static void emit_amd64_binop(const ir_node *irn)
{
	const arch_register_t *reg_s1 = get_in_reg(irn, 0);
	const arch_register_t *reg_s2 = get_in_reg(irn, 1);
	const arch_register_t *reg_d1 = get_out_reg(irn, 0);

	int second_op = 0;

	if (reg_d1 != reg_s1 && reg_d1 != reg_s2) {
		be_emit_cstring("\tmov ");
		amd64_emit_register(reg_s1);
		be_emit_cstring(", ");
		amd64_emit_register(reg_d1);
		be_emit_finish_line_gas(irn);
		second_op = 1;

	} else if (reg_d1 == reg_s2 && reg_d1 != reg_s1) {
		second_op = 0;

	}

	emit_amd64_binop_op(irn, second_op);

}

/**
 * The type of a emitter function.
 */
typedef void (emit_func)(const ir_node *irn);

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static inline void set_emitter(ir_op *op, emit_func arm_emit_node)
{
	op->ops.generic = (op_func)arm_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void amd64_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	amd64_register_spec_emitters();

	set_emitter(op_amd64_SymConst,   emit_amd64_SymConst);
	set_emitter(op_amd64_Jmp,        emit_amd64_Jmp);
	set_emitter(op_amd64_Jcc,        emit_amd64_Jcc);
	set_emitter(op_amd64_FrameAddr,  emit_amd64_FrameAddr);
	set_emitter(op_be_Return,        emit_be_Return);
	set_emitter(op_be_Call,          emit_be_Call);
	set_emitter(op_be_Copy,          emit_be_Copy);
	set_emitter(op_be_IncSP,         emit_be_IncSP);

	set_emitter(op_amd64_Add,        emit_amd64_binop);
	set_emitter(op_amd64_Mul,        emit_amd64_binop);

	set_emitter(op_be_Start,         emit_nothing);
	set_emitter(op_be_Keep,          emit_nothing);
	set_emitter(op_be_Barrier,       emit_nothing);
	set_emitter(op_Phi,              emit_nothing);
}

typedef void (*emit_func_ptr) (const ir_node *);

/**
 * Emits code for a node.
 */
static void amd64_emit_node(const ir_node *node)
{
	ir_op               *op       = get_irn_op(node);

	if (op->ops.generic) {
		emit_func_ptr func = (emit_func_ptr) op->ops.generic;
		(*func) (node);
	} else {
		ir_fprintf(stderr, "No emitter for node %+F\n", node);
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void amd64_gen_block(ir_node *block, void *data)
{
	ir_node *node;
	(void) data;

	if (! is_Block(block))
		return;

	be_gas_emit_block_name(block);
	be_emit_char(':');

	be_emit_write_line();

	sched_foreach(block, node) {
		amd64_emit_node(node);
	}
}


/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
static void amd64_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

/**
 * Main driver
 */
void amd64_gen_routine(const amd64_code_gen_t *cg, ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_node  **blk_sched;
	int i, n;
	(void)cg;

	/* register all emitter functions */
	amd64_register_emitters();

	blk_sched = be_create_block_schedule(irg);

	be_dbg_method_begin(entity, be_abi_get_stack_layout(cg->birg->abi));
	be_gas_emit_function_prolog(entity, 4);

	irg_block_walk_graph(irg, amd64_gen_labels, NULL, NULL);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n; i++) {
		ir_node *block = blk_sched[i];
		ir_node *next  = (i + 1) < n ? blk_sched[i+1] : NULL;

		set_irn_link(block, next);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];

		amd64_gen_block(block, 0);
	}

	be_gas_emit_function_epilog(entity);
	be_dbg_method_end();
	be_emit_char('\n');
	be_emit_write_line();
}
