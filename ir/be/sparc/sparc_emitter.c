/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @version $Id$
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
#include "irargs_t.h"
#include "error.h"
#include "raw_bitset.h"
#include "dbginfo.h"
#include "heights.h"

#include "../besched.h"
#include "../beblocksched.h"
#include "../beirg.h"
#include "../begnuas.h"
#include "../be_dbgout.h"
#include "../benode.h"
#include "../bestack.h"

#include "sparc_emitter.h"
#include "gen_sparc_emitter.h"
#include "sparc_nodes_attr.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_heights_t  *heights;
static const ir_node *delay_slot_filler; /**< this node has been choosen to fill
                                              the next delay slot */

static void sparc_emit_node(const ir_node *node);

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
	} else if (is_sparc_irn(node)) {
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

static bool is_valid_immediate(int32_t value)
{
	return -4096 <= value && value < 4096;
}

void sparc_emit_immediate(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	if (entity == NULL) {
		int32_t value = attr->immediate_value;
		assert(is_valid_immediate(value));
		be_emit_irprintf("%d", value);
	} else {
		be_emit_cstring("%lo(");
		be_gas_emit_entity(entity);
		if (attr->immediate_value != 0) {
			be_emit_irprintf("%+d", attr->immediate_value);
		}
		be_emit_char(')');
	}
}

void sparc_emit_high_immediate(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	be_emit_cstring("%hi(");
	if (entity == NULL) {
		uint32_t value = (uint32_t) attr->immediate_value;
		be_emit_irprintf("0x%X", value);
	} else {
		be_gas_emit_entity(entity);
		if (attr->immediate_value != 0) {
			be_emit_irprintf("%+d", attr->immediate_value);
		}
	}
	be_emit_char(')');
}

void sparc_emit_source_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_in_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

void sparc_emit_dest_register(const ir_node *node, int pos)
{
	const arch_register_t *reg = get_out_reg(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

/**
 * Emits either a imm or register depending on arity of node
 * @param node
 * @param register no (-1 if no register)
 */
void sparc_emit_reg_or_imm(const ir_node *node, int pos)
{
	if (get_irn_arity(node) > pos) {
		// we have reg input
		sparc_emit_source_register(node, pos);
	} else {
		// we have a imm input
		sparc_emit_immediate(node);
	}
}

static bool is_stack_pointer_relative(const ir_node *node)
{
	const arch_register_t *sp = &sparc_gp_regs[REG_SP];
	return (is_sparc_St(node) && get_in_reg(node, n_sparc_St_ptr) == sp)
	    || (is_sparc_Ld(node) && get_in_reg(node, n_sparc_Ld_ptr) == sp);
}

/**
 * emit SP offset
 */
void sparc_emit_offset(const ir_node *node, int offset_node_pos)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);

	if (attr->is_reg_reg) {
		assert(!attr->is_frame_entity);
		assert(attr->base.immediate_value == 0);
		assert(attr->base.immediate_value_entity == NULL);
		be_emit_char('+');
		sparc_emit_source_register(node, offset_node_pos);
	} else if (attr->is_frame_entity) {
		int32_t offset = attr->base.immediate_value;
		/* bad hack: the real stack stuff is behind the always-there spill
		 * space for the register window and stack */
		if (is_stack_pointer_relative(node))
			offset += SPARC_MIN_STACKSIZE;
		if (offset != 0) {
			assert(is_valid_immediate(offset));
			be_emit_irprintf("%+ld", offset);
		}
	} else if (attr->base.immediate_value != 0
			|| attr->base.immediate_value_entity != NULL) {
		be_emit_char('+');
		sparc_emit_immediate(node);
	}
}

void sparc_emit_float_load_store_mode(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	ir_mode *mode = attr->load_store_mode;
	int      bits = get_mode_size_bits(mode);

	assert(mode_is_float(mode));

	switch (bits) {
	case 32:  return;
	case 64:  be_emit_char('d'); return;
	case 128: be_emit_char('q'); return;
	}
	panic("invalid flaot load/store mode %+F", mode);
}

/**
 *  Emit load mode char
 */
void sparc_emit_load_mode(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	ir_mode *mode      = attr->load_store_mode;
	int      bits      = get_mode_size_bits(mode);
	bool     is_signed = mode_is_signed(mode);

	if (bits == 16) {
		be_emit_string(is_signed ? "sh" : "uh");
	} else if (bits == 8) {
		be_emit_string(is_signed ? "sb" : "ub");
	} else if (bits == 64) {
		be_emit_char('d');
	} else {
		assert(bits == 32);
	}
}

/**
 * Emit store mode char
 */
void sparc_emit_store_mode(const ir_node *node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	ir_mode *mode      = attr->load_store_mode;
	int      bits      = get_mode_size_bits(mode);

	if (bits == 16) {
		be_emit_string("h");
	} else if (bits == 8) {
		be_emit_string("b");
	} else if (bits == 64) {
		be_emit_char('d');
	} else {
		assert(bits == 32);
	}
}

/**
 * emit integer signed/unsigned prefix char
 */
void sparc_emit_mode_sign_prefix(const ir_node *node)
{
	ir_mode *mode      = get_irn_mode(node);
	bool     is_signed = mode_is_signed(mode);
	be_emit_string(is_signed ? "s" : "u");
}

static void emit_fp_suffix(const ir_mode *mode)
{
	unsigned bits = get_mode_size_bits(mode);
	assert(mode_is_float(mode));

	if (bits == 32) {
		be_emit_char('s');
	} else if (bits == 64) {
		be_emit_char('d');
	} else if (bits == 128) {
		be_emit_char('q');
	} else {
		panic("invalid FP mode");
	}
}

void sparc_emit_fp_conv_source(const ir_node *node)
{
	const sparc_fp_conv_attr_t *attr = get_sparc_fp_conv_attr_const(node);
	emit_fp_suffix(attr->src_mode);
}

void sparc_emit_fp_conv_destination(const ir_node *node)
{
	const sparc_fp_conv_attr_t *attr = get_sparc_fp_conv_attr_const(node);
	emit_fp_suffix(attr->dest_mode);
}

/**
 * emits the FP mode suffix char
 */
void sparc_emit_fp_mode_suffix(const ir_node *node)
{
	const sparc_fp_attr_t *attr = get_sparc_fp_attr_const(node);
	emit_fp_suffix(attr->fp_mode);
}

static ir_node *get_jump_target(const ir_node *jump)
{
	return get_irn_link(jump);
}

/**
 * Returns the target label for a control flow node.
 */
static void sparc_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_jump_target(node);
	be_gas_emit_block_name(block);
}

static int get_sparc_Call_dest_addr_pos(const ir_node *node)
{
	return get_irn_arity(node)-1;
}

static bool ba_is_fallthrough(const ir_node *node)
{
	ir_node *block      = get_nodes_block(node);
	ir_node *next_block = get_irn_link(block);
	return get_irn_link(node) == next_block;
}

static bool is_no_instruction(const ir_node *node)
{
	/* copies are nops if src_reg == dest_reg */
	if (be_is_Copy(node) || be_is_CopyKeep(node)) {
		const arch_register_t *src_reg  = get_in_reg(node, 0);
		const arch_register_t *dest_reg = get_out_reg(node, 0);

		if (src_reg == dest_reg)
			return true;
	}
	/* Ba is not emitted if it is a simple fallthrough */
	if (is_sparc_Ba(node) && ba_is_fallthrough(node))
		return true;

	return be_is_Keep(node) || be_is_Barrier(node) || be_is_Start(node)
		|| is_Phi(node);
}

static bool has_delay_slot(const ir_node *node)
{
	if (is_sparc_Ba(node) && ba_is_fallthrough(node))
		return false;

	return is_sparc_Bicc(node) || is_sparc_fbfcc(node) || is_sparc_Ba(node)
		|| is_sparc_SwitchJmp(node) || is_sparc_Call(node)
		|| is_sparc_SDiv(node) || is_sparc_UDiv(node);
}

/** returns true if the emitter for this sparc node can produce more than one
 * actual sparc instruction.
 * Usually it is a bad sign if we have to add instructions here. We should
 * rather try to get them lowered down. So we can actually put them into
 * delay slots and make them more accessible to the scheduler.
 */
static bool emits_multiple_instructions(const ir_node *node)
{
	if (has_delay_slot(node))
		return true;

	return is_sparc_Mulh(node) || is_sparc_SDiv(node) || is_sparc_UDiv(node)
		|| be_is_MemPerm(node) || be_is_Perm(node) || be_is_Return(node);
}

/**
 * search for an instruction that can fill the delay slot of @p node
 */
static const ir_node *pick_delay_slot_for(const ir_node *node)
{
	const ir_node *check      = node;
	const ir_node *schedpoint = node;
	unsigned       tries      = 0;
	/* currently we don't track which registers are still alive, so we can't
	 * pick any other instructions other than the one directly preceding */
	static const unsigned PICK_DELAY_SLOT_MAX_DISTANCE = 1;

	assert(has_delay_slot(node));

	if (is_sparc_Call(node)) {
		const sparc_attr_t *attr   = get_sparc_attr_const(node);
		ir_entity          *entity = attr->immediate_value_entity;
		if (entity != NULL) {
			check = NULL; /* pick any instruction, dependencies on Call
			                 don't matter */
		} else {
			/* we only need to check the value for the call destination */
			check = get_irn_n(node, get_sparc_Call_dest_addr_pos(node));
		}

		/* the Call also destroys the value of %o7, but since this is currently
		 * marked as ignore register in the backend, it should never be used by
		 * the instruction in the delay slot. */
	} else {
		check = node;
	}

	while (sched_has_prev(schedpoint)) {
		schedpoint = sched_prev(schedpoint);

		if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
			break;

		if (has_delay_slot(schedpoint))
			break;

		/* skip things which don't really result in instructions */
		if (is_no_instruction(schedpoint))
			continue;

		if (emits_multiple_instructions(schedpoint))
			continue;

		/* allowed for delayslot: any instruction which is not necessary to
		 * compute an input to the branch. */
		if (check != NULL
				&& heights_reachable_in_block(heights, check, schedpoint))
			continue;

		/* found something */
		return schedpoint;
	}

	return NULL;
}

/**
 * Emits code for stack space management
 */
static void emit_be_IncSP(const ir_node *irn)
{
	int offs = -be_get_IncSP_offset(irn);

	if (offs == 0)
		return;

	/* SPARC stack grows downwards */
	if (offs < 0) {
		be_emit_cstring("\tsub ");
		offs = -offs;
	} else {
		be_emit_cstring("\tadd ");
	}

	sparc_emit_source_register(irn, 0);
	be_emit_irprintf(", %d", offs);
	be_emit_cstring(", ");
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}

/**
 * emits code for save instruction with min. required stack space
 */
static void emit_sparc_Save(const ir_node *irn)
{
	const sparc_save_attr_t *save_attr = get_sparc_save_attr_const(irn);
	be_emit_cstring("\tsave ");
	sparc_emit_source_register(irn, 0);
	be_emit_irprintf(", %d, ", -save_attr->initial_stacksize);
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}

/**
 * emits code for mulh
 */
static void emit_sparc_Mulh(const ir_node *irn)
{
	be_emit_cstring("\t");
	sparc_emit_mode_sign_prefix(irn);
	be_emit_cstring("mul ");

	sparc_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	sparc_emit_reg_or_imm(irn, 1);
	be_emit_cstring(", ");
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);

	// our result is in the y register now
	// we just copy it to the assigned target reg
	be_emit_cstring("\tmov %y, ");
	sparc_emit_dest_register(irn, 0);
	be_emit_finish_line_gas(irn);
}

static void fill_delay_slot(void)
{
	if (delay_slot_filler != NULL) {
		sparc_emit_node(delay_slot_filler);
		delay_slot_filler = NULL;
	} else {
		be_emit_cstring("\tnop\n");
		be_emit_write_line();
	}
}

static void emit_sparc_Div(const ir_node *node, bool is_signed)
{
	/* can we get the delay count of the wr instruction somewhere? */
	unsigned wry_delay_count = 3;
	unsigned i;

	be_emit_cstring("\twr ");
	sparc_emit_source_register(node, 0);
	be_emit_cstring(", 0, %y");
	be_emit_finish_line_gas(node);

	for (i = 0; i < wry_delay_count; ++i) {
		fill_delay_slot();
	}

	be_emit_irprintf("\t%s ", is_signed ? "sdiv" : "udiv");
	sparc_emit_source_register(node, 1);
	be_emit_cstring(", ");
	sparc_emit_reg_or_imm(node, 2);
	be_emit_cstring(", ");
	sparc_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

static void emit_sparc_SDiv(const ir_node *node)
{
	emit_sparc_Div(node, true);
}

static void emit_sparc_UDiv(const ir_node *node)
{
	emit_sparc_Div(node, false);
}

/**
 * Emits code for return node
 */
static void emit_be_Return(const ir_node *irn)
{
	be_emit_cstring("\tret");
	//be_emit_cstring("\tjmp %i7+8");
	be_emit_finish_line_gas(irn);
	be_emit_cstring("\trestore");
	be_emit_finish_line_gas(irn);
}

/**
 * Emits code for Call node
 */
static void emit_sparc_Call(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	be_emit_cstring("\tcall ");
	if (entity != NULL) {
	    be_gas_emit_entity(entity);
	    if (attr->immediate_value != 0) {
			be_emit_irprintf("%+d", attr->immediate_value);
		}
		be_emit_cstring(", 0");
	} else {
		int dest_addr = get_sparc_Call_dest_addr_pos(node);
		sparc_emit_source_register(node, dest_addr);
	}
	be_emit_finish_line_gas(node);

	fill_delay_slot();
}

/**
 * Emit code for Perm node
 */
static void emit_be_Perm(const ir_node *irn)
{
	be_emit_cstring("\txor ");
	sparc_emit_source_register(irn, 1);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 0);
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\txor ");
	sparc_emit_source_register(irn, 1);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 1);
	be_emit_finish_line_gas(NULL);

	be_emit_cstring("\txor ");
	sparc_emit_source_register(irn, 1);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 0);
	be_emit_cstring(", ");
	sparc_emit_source_register(irn, 0);
	be_emit_finish_line_gas(irn);
}

static void emit_be_MemPerm(const ir_node *node)
{
	int i;
	int memperm_arity;
	int sp_change = 0;
	ir_graph          *irg    = get_irn_irg(node);
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);

	/* this implementation only works with frame pointers currently */
	assert(layout->sp_relative == false);

	/* TODO: this implementation is slower than necessary.
	   The longterm goal is however to avoid the memperm node completely */

	memperm_arity = be_get_MemPerm_entity_arity(node);
	// we use our local registers - so this is limited to 8 inputs !
	if (memperm_arity > 8)
		panic("memperm with more than 8 inputs not supported yet");

	be_emit_irprintf("\tsub %%sp, %d, %%sp", memperm_arity*4);
	be_emit_finish_line_gas(node);

	for (i = 0; i < memperm_arity; ++i) {
		ir_entity *entity = be_get_MemPerm_in_entity(node, i);
		int        offset = be_get_stack_entity_offset(layout, entity, 0);

		/* spill register */
		be_emit_irprintf("\tst %%l%d, [%%sp%+d]", i, sp_change + SPARC_MIN_STACKSIZE);
		be_emit_finish_line_gas(node);

		/* load from entity */
		be_emit_irprintf("\tld [%%fp%+d], %%l%d", offset, i);
		be_emit_finish_line_gas(node);
		sp_change += 4;
	}

	for (i = memperm_arity-1; i >= 0; --i) {
		ir_entity *entity = be_get_MemPerm_out_entity(node, i);
		int        offset = be_get_stack_entity_offset(layout, entity, 0);

		sp_change -= 4;

		/* store to new entity */
		be_emit_irprintf("\tst %%l%d, [%%fp%+d]", i, offset);
		be_emit_finish_line_gas(node);
		/* restore register */
		be_emit_irprintf("\tld [%%sp%+d], %%l%d", sp_change + SPARC_MIN_STACKSIZE, i);
		be_emit_finish_line_gas(node);
	}

	be_emit_irprintf("\tadd %%sp, %d, %%sp", memperm_arity*4);
	be_emit_finish_line_gas(node);

	assert(sp_change == 0);
}

static void emit_sparc_FrameAddr(const ir_node *node)
{
	const sparc_attr_t *attr = get_sparc_attr_const(node);

	// no need to fix offset as we are adressing via the framepointer
	if (attr->immediate_value >= 0) {
		be_emit_cstring("\tadd ");
		sparc_emit_source_register(node, 0);
		be_emit_cstring(", ");
		be_emit_irprintf("%ld", attr->immediate_value);
	} else {
		be_emit_cstring("\tsub ");
		sparc_emit_source_register(node, 0);
		be_emit_cstring(", ");
		be_emit_irprintf("%ld", -attr->immediate_value);
	}

	be_emit_cstring(", ");
	sparc_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

static const char *get_icc_unsigned(pn_Cmp pnc)
{
	switch (pnc) {
	case pn_Cmp_False: return "bn";
	case pn_Cmp_Eq:    return "be";
	case pn_Cmp_Lt:    return "blu";
	case pn_Cmp_Le:    return "bleu";
	case pn_Cmp_Gt:    return "bgu";
	case pn_Cmp_Ge:    return "bgeu";
	case pn_Cmp_Lg:    return "bne";
	case pn_Cmp_Leg:   return "ba";
	default: panic("Cmp has unsupported pnc");
	}
}

static const char *get_icc_signed(pn_Cmp pnc)
{
	switch (pnc) {
	case pn_Cmp_False: return "bn";
	case pn_Cmp_Eq:    return "be";
	case pn_Cmp_Lt:    return "bl";
	case pn_Cmp_Le:    return "ble";
	case pn_Cmp_Gt:    return "bg";
	case pn_Cmp_Ge:    return "bge";
	case pn_Cmp_Lg:    return "bne";
	case pn_Cmp_Leg:   return "ba";
	default: panic("Cmp has unsupported pnc");
	}
}

static const char *get_fcc(pn_Cmp pnc)
{
	switch (pnc) {
	case pn_Cmp_False: return "fbn";
	case pn_Cmp_Eq:    return "fbe";
	case pn_Cmp_Lt:    return "fbl";
	case pn_Cmp_Le:    return "fble";
	case pn_Cmp_Gt:    return "fbg";
	case pn_Cmp_Ge:    return "fbge";
	case pn_Cmp_Lg:    return "fblg";
	case pn_Cmp_Leg:   return "fbo";
	case pn_Cmp_Uo:    return "fbu";
	case pn_Cmp_Ue:    return "fbue";
	case pn_Cmp_Ul:    return "fbul";
	case pn_Cmp_Ule:   return "fbule";
	case pn_Cmp_Ug:    return "fbug";
	case pn_Cmp_Uge:   return "fbuge";
	case pn_Cmp_Ne:    return "fbne";
	case pn_Cmp_True:  return "fba";
	case pn_Cmp_max:
		break;
	}
	panic("invalid pnc");
}

typedef const char* (*get_cc_func)(pn_Cmp pnc);

static void emit_sparc_branch(const ir_node *node, get_cc_func get_cc)
{
	const sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr_const(node);
	pn_Cmp           pnc         = attr->pnc;
	const ir_node   *proj_true   = NULL;
	const ir_node   *proj_false  = NULL;
	const ir_edge_t *edge;
	const ir_node   *block;
	const ir_node   *next_block;

	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = get_irn_link(block);

	if (get_irn_link(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		if (is_sparc_fbfcc(node)) {
			pnc = get_negated_pnc(pnc, mode_F);
		} else {
			pnc = get_negated_pnc(pnc, mode_Iu);
		}
	}

	/* emit the true proj */
	be_emit_cstring("\t");
	be_emit_string(get_cc(pnc));
	be_emit_char(' ');
	sparc_emit_cfop_target(proj_true);
	be_emit_finish_line_gas(proj_true);

	fill_delay_slot();

	if (get_irn_link(proj_false) == next_block) {
		be_emit_cstring("\t/* fallthrough to ");
		sparc_emit_cfop_target(proj_false);
		be_emit_cstring(" */");
		be_emit_finish_line_gas(proj_false);
	} else {
		be_emit_cstring("\tba ");
		sparc_emit_cfop_target(proj_false);
		be_emit_finish_line_gas(proj_false);
		fill_delay_slot();
	}
}

static void emit_sparc_Bicc(const ir_node *node)
{
	const sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr_const(node);
	bool             is_unsigned = attr->is_unsigned;
	emit_sparc_branch(node, is_unsigned ? get_icc_unsigned : get_icc_signed);
}

static void emit_sparc_fbfcc(const ir_node *node)
{
	emit_sparc_branch(node, get_fcc);
}

static void emit_sparc_Ba(const ir_node *node)
{
	if (ba_is_fallthrough(node)) {
		be_emit_cstring("\t/* fallthrough to ");
		sparc_emit_cfop_target(node);
		be_emit_cstring(" */");
	} else {
		be_emit_cstring("\tba ");
		sparc_emit_cfop_target(node);
		be_emit_finish_line_gas(node);
		fill_delay_slot();
	}
	be_emit_finish_line_gas(node);
}

static void emit_jump_table(const ir_node *node)
{
	const sparc_switch_jmp_attr_t *attr = get_sparc_switch_jmp_attr_const(node);
	long             switch_max    = LONG_MIN;
	long             default_pn    = attr->default_proj_num;
	ir_entity       *entity        = attr->jump_table;
	ir_node         *default_block = NULL;
	unsigned long    length;
	const ir_edge_t *edge;
	unsigned         i;
	ir_node        **table;

	/* go over all proj's and collect them */
	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long     pn   = get_Proj_proj(proj);

		/* check for default proj */
		if (pn == default_pn) {
			assert(default_block == NULL); /* more than 1 default_pn? */
			default_block = get_jump_target(proj);
		} else {
			switch_max = pn > switch_max ? pn : switch_max;
		}
	}
	assert(switch_max > LONG_MIN);

	length = (unsigned long) switch_max + 1;
	/* the 16000 isn't a real limit of the architecture. But should protect us
	 * from seamingly endless compiler runs */
	if (length > 16000) {
		/* switch lowerer should have broken this monster to pieces... */
		panic("too large switch encountered");
	}

	table = XMALLOCNZ(ir_node*, length);
	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long     pn   = get_Proj_proj(proj);
		if (pn == default_pn)
			continue;

		table[pn] = get_jump_target(proj);
	}

	/* emit table */
	be_gas_emit_switch_section(GAS_SECTION_RODATA);
	be_emit_cstring("\t.align 4\n");
	be_gas_emit_entity(entity);
	be_emit_cstring(":\n");
	for (i = 0; i < length; ++i) {
		ir_node *block = table[i];
		if (block == NULL)
			block = default_block;
		be_emit_cstring("\t.long ");
		be_gas_emit_block_name(block);
		be_emit_char('\n');
		be_emit_write_line();
	}
	be_gas_emit_switch_section(GAS_SECTION_TEXT);

	xfree(table);
}

static void emit_sparc_SwitchJmp(const ir_node *node)
{
	be_emit_cstring("\tjmp ");
	sparc_emit_source_register(node, 0);
	be_emit_finish_line_gas(node);
	fill_delay_slot();

	emit_jump_table(node);
}

static void emit_fmov(const ir_node *node, const arch_register_t *src_reg,
                      const arch_register_t *dst_reg)
{
	be_emit_cstring("\tfmovs %");
	be_emit_string(arch_register_get_name(src_reg));
	be_emit_cstring(", %");
	be_emit_string(arch_register_get_name(dst_reg));
	be_emit_finish_line_gas(node);
}

static const arch_register_t *get_next_fp_reg(const arch_register_t *reg)
{
	unsigned index = reg->index;
	assert(reg == &sparc_fp_regs[index]);
	index++;
	assert(index < N_sparc_fp_REGS);
	return &sparc_fp_regs[index];
}

static void emit_be_Copy(const ir_node *node)
{
	ir_mode               *mode    = get_irn_mode(node);
	const arch_register_t *src_reg = get_in_reg(node, 0);
	const arch_register_t *dst_reg = get_out_reg(node, 0);

	if (src_reg == dst_reg)
		return;

	if (mode_is_float(mode)) {
		unsigned bits = get_mode_size_bits(mode);
		int      n    = bits > 32 ? bits > 64 ? 3 : 1 : 0;
		int      i;
		emit_fmov(node, src_reg, dst_reg);
		for (i = 0; i < n; ++i) {
			src_reg = get_next_fp_reg(src_reg);
			dst_reg = get_next_fp_reg(dst_reg);
			emit_fmov(node, src_reg, dst_reg);
		}
	} else if (mode_is_data(mode)) {
		be_emit_cstring("\tmov ");
		sparc_emit_source_register(node, 0);
		be_emit_cstring(", ");
		sparc_emit_dest_register(node, 0);
		be_emit_finish_line_gas(node);
	} else {
		panic("emit_be_Copy: invalid mode");
	}
}

static void emit_nothing(const ir_node *irn)
{
	(void) irn;
}

typedef void (*emit_func) (const ir_node *);

static inline void set_emitter(ir_op *op, emit_func sparc_emit_node)
{
	op->ops.generic = (op_func)sparc_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void sparc_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();
	/* register all emitter functions defined in spec */
	sparc_register_spec_emitters();

	/* custom emitter */
	set_emitter(op_be_Copy,         emit_be_Copy);
	set_emitter(op_be_CopyKeep,     emit_be_Copy);
	set_emitter(op_be_IncSP,        emit_be_IncSP);
	set_emitter(op_be_MemPerm,      emit_be_MemPerm);
	set_emitter(op_be_Perm,         emit_be_Perm);
	set_emitter(op_be_Return,       emit_be_Return);
	set_emitter(op_sparc_Ba,        emit_sparc_Ba);
	set_emitter(op_sparc_Bicc,      emit_sparc_Bicc);
	set_emitter(op_sparc_Call,      emit_sparc_Call);
	set_emitter(op_sparc_fbfcc,     emit_sparc_fbfcc);
	set_emitter(op_sparc_FrameAddr, emit_sparc_FrameAddr);
	set_emitter(op_sparc_Mulh,      emit_sparc_Mulh);
	set_emitter(op_sparc_Save,      emit_sparc_Save);
	set_emitter(op_sparc_SDiv,      emit_sparc_SDiv);
	set_emitter(op_sparc_SwitchJmp, emit_sparc_SwitchJmp);
	set_emitter(op_sparc_UDiv,      emit_sparc_UDiv);

	/* no need to emit anything for the following nodes */
	set_emitter(op_be_Barrier, emit_nothing);
	set_emitter(op_be_Keep,    emit_nothing);
	set_emitter(op_be_Start,   emit_nothing);
	set_emitter(op_Phi,        emit_nothing);
}

/**
 * Emits code for a node.
 */
static void sparc_emit_node(const ir_node *node)
{
	ir_op *op = get_irn_op(node);

	if (op->ops.generic) {
		emit_func func = (emit_func) op->ops.generic;
		be_dbg_set_dbg_info(get_irn_dbg_info(node));
		(*func) (node);
	} else {
		panic("No emit handler for node %+F (graph %+F)\n", node,
		      current_ir_graph);
	}
}

static ir_node *find_next_delay_slot(ir_node *from)
{
	ir_node *schedpoint = from;
	while (!has_delay_slot(schedpoint)) {
		if (!sched_has_next(schedpoint))
			return NULL;
		schedpoint = sched_next(schedpoint);
	}
	return schedpoint;
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void sparc_emit_block(ir_node *block)
{
	ir_node *node;
	ir_node *next_delay_slot;

	assert(is_Block(block));

	be_gas_emit_block_name(block);
	be_emit_cstring(":\n");
	be_emit_write_line();

	next_delay_slot = find_next_delay_slot(sched_first(block));
	if (next_delay_slot != NULL)
		delay_slot_filler = pick_delay_slot_for(next_delay_slot);

	sched_foreach(block, node) {
		if (node == delay_slot_filler) {
			continue;
		}

		sparc_emit_node(node);

		if (node == next_delay_slot) {
			assert(delay_slot_filler == NULL);
			next_delay_slot = find_next_delay_slot(sched_next(node));
			if (next_delay_slot != NULL)
				delay_slot_filler = pick_delay_slot_for(next_delay_slot);
		}
	}
}

/**
 * Emits code for function start.
 */
static void sparc_emit_func_prolog(ir_graph *irg)
{
	ir_entity *ent = get_irg_entity(irg);
	be_gas_emit_function_prolog(ent, 4);
	be_emit_write_line();
}

/**
 * Emits code for function end
 */
static void sparc_emit_func_epilog(ir_graph *irg)
{
	ir_entity *ent = get_irg_entity(irg);
	const char *irg_name = get_entity_ld_name(ent);
	be_emit_write_line();
	be_emit_irprintf("\t.size  %s, .-%s\n", irg_name, irg_name);
	be_emit_cstring("# -- End ");
	be_emit_string(irg_name);
	be_emit_cstring("\n");
	be_emit_write_line();
}

static void sparc_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block); // link the pred of a block (which is a jmp)
	}
}

void sparc_emit_routine(ir_graph *irg)
{
	ir_entity  *entity = get_irg_entity(irg);
	ir_node   **block_schedule;
	int         i;
	int         n;

	be_gas_elf_type_char      = '#';
	be_gas_object_file_format = OBJECT_FILE_FORMAT_ELF_SPARC;

	heights = heights_new(irg);

	/* register all emitter functions */
	sparc_register_emitters();
	be_dbg_method_begin(entity);

	/* create the block schedule. For now, we don't need it earlier. */
	block_schedule = be_create_block_schedule(irg);

	sparc_emit_func_prolog(irg);
	irg_block_walk_graph(irg, sparc_gen_labels, NULL, NULL);

	/* inject block scheduling links & emit code of each block */
	n = ARR_LEN(block_schedule);
	for (i = 0; i < n; ++i) {
		ir_node *block      = block_schedule[i];
		ir_node *next_block = i+1 < n ? block_schedule[i+1] : NULL;
		set_irn_link(block, next_block);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = block_schedule[i];
		if (block == get_irg_end_block(irg))
			continue;
		sparc_emit_block(block);
	}

	/* emit function epilog */
	sparc_emit_func_epilog(irg);

	heights_free(heights);
}

void sparc_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.emit");
}
