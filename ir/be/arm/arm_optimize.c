/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements several optimizations for ARM.
 * @author      Michael Beck
 */
#include "arm_optimize.h"

#include "arm_new_nodes.h"
#include "arm_nodes_attr.h"
#include "bediagnostic.h"
#include "benode.h"
#include "bepeephole.h"
#include "besched.h"
#include "gen_arm_new_nodes.h"
#include "gen_arm_regalloc_if.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"

static uint32_t arm_ror(uint32_t v, uint32_t ror)
{
	return (v << (32 - ror)) | (v >> ror);
}

/**
 * Returns non.zero if the given offset can be directly encoded into an ARM
 * instruction.
 */
static bool allowed_arm_immediate(int offset, arm_vals *result)
{
	arm_gen_vals_from_word(offset, result);
	return result->ops <= 1;
}

/**
 * Fix an IncSP node if the offset gets too big
 */
static void peephole_be_IncSP(ir_node *node)
{
	/* first optimize incsp->incsp combinations */
	if (be_peephole_IncSP_IncSP(node))
		return;

	int offset = be_get_IncSP_offset(node);
	/* can be transformed into Add OR Sub */
	int sign = 1;
	if (offset < 0) {
		sign = -1;
		offset = -offset;
	}
	arm_vals v;
	if (allowed_arm_immediate(offset, &v))
		return;

	be_set_IncSP_offset(node, sign * arm_ror(v.values[0], v.rors[0]));

	ir_node *first = node;
	ir_node *block = get_nodes_block(node);
	for (unsigned cnt = 1; cnt < v.ops; ++cnt) {
		int      const value = sign * arm_ror(v.values[cnt], v.rors[cnt]);
		ir_node *const incsp = be_new_IncSP(block, node, value, be_get_IncSP_no_align(node));
		sched_add_after(node, incsp);
		node = incsp;
	}

	/* reattach IncSP users */
	edges_reroute_except(first, node, sched_next(first));
}

typedef ir_node *new_binop_imm(dbg_info*, ir_node*, ir_node*, unsigned char, unsigned char);

static ir_node *gen_ptr(new_binop_imm *const cons, ir_node *const node, ir_node *const frame, arm_vals const *const v)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node        *ptr   = frame;
	for (unsigned cnt = 0; cnt < v->ops; ++cnt) {
		ptr = cons(dbgi, block, ptr, v->values[cnt], v->rors[cnt]);
		arch_set_irn_register(ptr, &arm_registers[REG_R12]);
		sched_add_before(node, ptr);
	}
	return ptr;
}

/**
 * creates the address by Adds
 */
static ir_node *gen_ptr_add(ir_node *node, ir_node *frame, const arm_vals *v)
{
	return gen_ptr(&new_bd_arm_Add_imm, node, frame, v);
}

/**
 * creates the address by Subs
 */
static ir_node *gen_ptr_sub(ir_node *node, ir_node *frame, const arm_vals *v)
{
	return gen_ptr(&new_bd_arm_Sub_imm, node, frame, v);
}

/** fix frame addresses which are too big */
static void peephole_arm_FrameAddr(ir_node *node)
{
	arm_Address_attr_t *attr   = get_arm_Address_attr(node);
	int                 offset = attr->fp_offset;
	arm_vals            v;
	if (allowed_arm_immediate(offset, &v))
		return;

	ir_node *base = get_irn_n(node, n_arm_FrameAddr_base);
	/* TODO: suboptimal */
	ir_node *ptr = gen_ptr_add(node, base, &v);

	attr->fp_offset = 0;
	set_irn_n(node, n_arm_FrameAddr_base, ptr);
}

/**
 * Fix stackpointer relative stores if the offset gets too big
 */
static void peephole_arm_Str_Ldr(ir_node *node)
{
	arm_load_store_attr_t *attr    = get_arm_load_store_attr(node);
	const int              offset  = attr->offset;
	if (arm_is_valid_offset(offset, attr->load_store_mode, is_arm_Str(node)))
		return;

	/* we should only have too big offsets for frame entities */
	if (!attr->is_frame_entity)
		be_errorf(node, "POSSIBLE ARM BACKEND PROBLEM: offset in Store too big");
	bool use_add = offset >= 0;

	ir_node *ptr;
	if (is_arm_Str(node)) {
		ptr = get_irn_n(node, n_arm_Str_ptr);
	} else {
		assert(is_arm_Ldr(node));
		ptr = get_irn_n(node, n_arm_Ldr_ptr);
	}

	arm_vals v;
	arm_gen_vals_from_word(offset, &v);
	if (use_add) {
		ptr = gen_ptr_add(node, ptr, &v);
	} else {
		ptr = gen_ptr_sub(node, ptr, &v);
	}

	/* TODO: sub-optimal, the last offset could probably be left inside the
	   store */
	if (is_arm_Str(node)) {
		set_irn_n(node, n_arm_Str_ptr, ptr);
	} else {
		assert(is_arm_Ldr(node));
		set_irn_n(node, n_arm_Ldr_ptr, ptr);
	}
	attr->offset = 0;
}

/* Perform peephole-optimizations. */
void arm_peephole_optimization(ir_graph *irg)
{
	/* register peephole optimizations */
	ir_clear_opcodes_generic_func();
	register_peephole_optimization(op_be_IncSP,      peephole_be_IncSP);
	register_peephole_optimization(op_arm_Str,       peephole_arm_Str_Ldr);
	register_peephole_optimization(op_arm_Ldr,       peephole_arm_Str_Ldr);
	register_peephole_optimization(op_arm_FrameAddr, peephole_arm_FrameAddr);

	be_peephole_opt(irg);
}

static inline bool arm_is_offset8(int32_t const v)
{
	/* Symmetrical, because ARM uses sign+magnitude for offset. */
	return -255 <= v && v <= 255;
}

static inline bool arm_is_offset12(int32_t const v)
{
	/* Symmetrical, because ARM uses sign+magnitude for offset. */
	return -4095 <= v && v <= 4095;
}

bool arm_is_valid_offset(int32_t const v, ir_mode *const mode, bool const is_store)
{
	switch (get_mode_size_bits(mode)) {
	case  8:
		if (is_store || !mode_is_signed(mode)) {
			/* FALLTHROUGH */
	case 32:
			return arm_is_offset12(v);
		} else {
			/* FALLTHROUGH */
	default:
			return arm_is_offset8(v);
		}
	}
}
