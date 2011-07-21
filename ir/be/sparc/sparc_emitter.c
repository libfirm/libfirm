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
 * @author  Hannes Rapp, Matthias Braun
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
#include "sparc_architecture.h"

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

void sparc_emit_immediate(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	if (entity == NULL) {
		int32_t value = attr->immediate_value;
		assert(sparc_is_value_imm_encodeable(value));
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
	if (arch_irn_get_flags(node) & ((arch_irn_flags_t)sparc_arch_irn_flag_immediate_form)) {
		// we have a imm input
		sparc_emit_immediate(node);
	} else {
		// we have reg input
		sparc_emit_source_register(node, pos);
	}
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
		if (offset != 0) {
			assert(sparc_is_value_imm_encodeable(offset));
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
	return (ir_node*)get_irn_link(jump);
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
	ir_node *next_block = (ir_node*)get_irn_link(block);
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
	if (be_is_IncSP(node) && be_get_IncSP_offset(node) == 0)
		return true;
	/* Ba is not emitted if it is a simple fallthrough */
	if (is_sparc_Ba(node) && ba_is_fallthrough(node))
		return true;

	return be_is_Keep(node) || be_is_Start(node) || is_Phi(node);
}

static bool has_delay_slot(const ir_node *node)
{
	if (is_sparc_Ba(node) && ba_is_fallthrough(node))
		return false;

	return is_sparc_Bicc(node) || is_sparc_fbfcc(node) || is_sparc_Ba(node)
		|| is_sparc_SwitchJmp(node) || is_sparc_Call(node)
		|| is_sparc_SDiv(node) || is_sparc_UDiv(node)
		|| be_is_Return(node);
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
		|| be_is_MemPerm(node) || be_is_Perm(node);
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
	} else if (be_is_Return(node)) {
		/* we only have to check the jump destination value */
		int arity = get_irn_arity(node);
		int i;

		check = NULL;
		for (i = 0; i < arity; ++i) {
			ir_node               *in  = get_irn_n(node, i);
			const arch_register_t *reg = arch_get_irn_register(in);
			if (reg == &sparc_registers[REG_O7]) {
				check = skip_Proj(in);
				break;
			}
		}
	} else {
		check = node;
	}

	while (sched_has_prev(schedpoint)) {
		schedpoint = sched_prev(schedpoint);

		if (has_delay_slot(schedpoint))
			break;

		/* skip things which don't really result in instructions */
		if (is_no_instruction(schedpoint))
			continue;

		if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
			break;

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
	int offset = be_get_IncSP_offset(irn);

	if (offset == 0)
		return;

	/* SPARC stack grows downwards */
	if (offset < 0) {
		be_emit_cstring("\tsub ");
		offset = -offset;
	} else {
		be_emit_cstring("\tadd ");
	}

	sparc_emit_source_register(irn, 0);
	be_emit_irprintf(", %d", -offset);
	be_emit_cstring(", ");
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

static void emit_be_Perm_xor(const ir_node *irn)
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

static int map_reg_to_permi_num(const arch_register_t *reg)
{
	return reg->index;
}

static int permi5(int reg0, int reg1, int reg2, int reg3, int reg4)
{
	return (reg0 << 20) | (reg1 << 15) | (reg2 << 10) | (reg3 <<  5) | reg4;
}

static void emit_be_Perm_permi(const ir_node *irn)
{
	const arch_register_t *reg1 = get_in_reg(irn, 0);
	const arch_register_t *reg2 = get_in_reg(irn, 1);
	const int pn1 = map_reg_to_permi_num(reg1);
	const int pn2 = map_reg_to_permi_num(reg2);
	const int n = permi5(pn1, pn2, pn2, pn2, pn2);

	be_emit_irprintf("\tpermi %d", n);
	be_emit_finish_line_gas(irn);
}

/**
 * Emit code for Perm node
 */
static void emit_be_Perm(const ir_node *irn)
{
	if (sparc_cg_config.use_permi)
		emit_be_Perm_permi(irn);
	else
		emit_be_Perm_xor(irn);
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

static void emit_be_Return(const ir_node *node)
{
	const char *destreg = "%o7";

	/* hack: we don't explicitely model register changes because of the
	 * restore node. So we have to do it manually here */
	if (delay_slot_filler != NULL &&
			(is_sparc_Restore(delay_slot_filler)
			 || is_sparc_RestoreZero(delay_slot_filler))) {
		destreg = "%i7";
	}
	be_emit_cstring("\tjmp ");
	be_emit_string(destreg);
	be_emit_cstring("+8");
	be_emit_finish_line_gas(node);
	fill_delay_slot();
}

static void emit_sparc_FrameAddr(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	int32_t             offset = attr->immediate_value;

	if (offset < 0) {
		be_emit_cstring("\tadd ");
		sparc_emit_source_register(node, 0);
		be_emit_cstring(", ");
		assert(sparc_is_value_imm_encodeable(offset));
		be_emit_irprintf("%ld", offset);
	} else {
		be_emit_cstring("\tsub ");
		sparc_emit_source_register(node, 0);
		be_emit_cstring(", ");
		assert(sparc_is_value_imm_encodeable(-offset));
		be_emit_irprintf("%ld", -offset);
	}

	be_emit_cstring(", ");
	sparc_emit_dest_register(node, 0);
	be_emit_finish_line_gas(node);
}

static const char *get_icc_unsigned(ir_relation relation)
{
	switch (relation & (ir_relation_less_equal_greater)) {
	case ir_relation_false:              return "bn";
	case ir_relation_equal:              return "be";
	case ir_relation_less:               return "blu";
	case ir_relation_less_equal:         return "bleu";
	case ir_relation_greater:            return "bgu";
	case ir_relation_greater_equal:      return "bgeu";
	case ir_relation_less_greater:       return "bne";
	case ir_relation_less_equal_greater: return "ba";
	default: panic("Cmp has unsupported relation");
	}
}

static const char *get_icc_signed(ir_relation relation)
{
	switch (relation & (ir_relation_less_equal_greater)) {
	case ir_relation_false:              return "bn";
	case ir_relation_equal:              return "be";
	case ir_relation_less:               return "bl";
	case ir_relation_less_equal:         return "ble";
	case ir_relation_greater:            return "bg";
	case ir_relation_greater_equal:      return "bge";
	case ir_relation_less_greater:       return "bne";
	case ir_relation_less_equal_greater: return "ba";
	default: panic("Cmp has unsupported relation");
	}
}

static const char *get_fcc(ir_relation relation)
{
	switch (relation) {
	case ir_relation_false:                   return "fbn";
	case ir_relation_equal:                   return "fbe";
	case ir_relation_less:                    return "fbl";
	case ir_relation_less_equal:              return "fble";
	case ir_relation_greater:                 return "fbg";
	case ir_relation_greater_equal:           return "fbge";
	case ir_relation_less_greater:            return "fblg";
	case ir_relation_less_equal_greater:      return "fbo";
	case ir_relation_unordered:               return "fbu";
	case ir_relation_unordered_equal:         return "fbue";
	case ir_relation_unordered_less:          return "fbul";
	case ir_relation_unordered_less_equal:    return "fbule";
	case ir_relation_unordered_greater:       return "fbug";
	case ir_relation_unordered_greater_equal: return "fbuge";
	case ir_relation_unordered_less_greater:  return "fbne";
	case ir_relation_true:                    return "fba";
	}
	panic("invalid relation");
}

typedef const char* (*get_cc_func)(ir_relation relation);

static void emit_sparc_branch(const ir_node *node, get_cc_func get_cc)
{
	const sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr_const(node);
	ir_relation      relation    = attr->relation;
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
	next_block = (ir_node*)get_irn_link(block);

	if (get_irn_link(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		relation   = get_negated_relation(relation);
	}

	/* emit the true proj */
	be_emit_cstring("\t");
	be_emit_string(get_cc(relation));
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

static void emit_sparc_SwitchJmp(const ir_node *node)
{
	const sparc_switch_jmp_attr_t *attr = get_sparc_switch_jmp_attr_const(node);

	be_emit_cstring("\tjmp ");
	sparc_emit_source_register(node, 0);
	be_emit_finish_line_gas(node);
	fill_delay_slot();

	emit_jump_table(node, attr->default_proj_num, attr->jump_table,
	                get_jump_target);
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
	unsigned idx = reg->global_index;
	assert(reg == &sparc_registers[idx]);
	idx++;
	assert(idx - REG_F0 < N_sparc_fp_REGS);
	return &sparc_registers[idx];
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
	set_emitter(op_sparc_SDiv,      emit_sparc_SDiv);
	set_emitter(op_sparc_SwitchJmp, emit_sparc_SwitchJmp);
	set_emitter(op_sparc_UDiv,      emit_sparc_UDiv);

	/* no need to emit anything for the following nodes */
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
	size_t      i;
	size_t      n;

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
