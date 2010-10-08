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
 * @brief   code selection (transform FIRM into SPARC FIRM)
 * @version $Id$
 */
#include "config.h"

#include <stdint.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "ircons.h"
#include "irprintf.h"
#include "iroptimize.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"

#include "../benode.h"
#include "../beirg.h"
#include "../beutil.h"
#include "../betranshlp.h"
#include "../beabihelper.h"
#include "bearch_sparc_t.h"

#include "sparc_nodes_attr.h"
#include "sparc_transform.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_new_nodes.h"

#include "gen_sparc_regalloc_if.h"
#include "sparc_cconv.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static beabi_helper_env_t    *abihelper;
static const arch_register_t *sp_reg = &sparc_registers[REG_SP];
static const arch_register_t *fp_reg = &sparc_registers[REG_FRAME_POINTER];
static calling_convention_t  *cconv  = NULL;
static ir_mode               *mode_gp;
static ir_mode               *mode_fp;
static ir_mode               *mode_fp2;
//static ir_mode               *mode_fp4;
static pmap                  *node_to_stack;

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/**
 * Create an And that will zero out upper bits.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param src_bits  number of lower bits that will remain
 */
static ir_node *gen_zero_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	if (src_bits == 8) {
		return new_bd_sparc_And_imm(dbgi, block, op, NULL, 0xFF);
	} else if (src_bits == 16) {
		ir_node *lshift = new_bd_sparc_Sll_imm(dbgi, block, op, NULL, 16);
		ir_node *rshift = new_bd_sparc_Srl_imm(dbgi, block, lshift, NULL, 16);
		return rshift;
	} else {
		panic("zero extension only supported for 8 and 16 bits");
	}
}

/**
 * Generate code for a sign extension.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param src_bits  number of lower bits that will remain
 */
static ir_node *gen_sign_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	int shift_width = 32 - src_bits;
	ir_node *lshift_node = new_bd_sparc_Sll_imm(dbgi, block, op, NULL, shift_width);
	ir_node *rshift_node = new_bd_sparc_Sra_imm(dbgi, block, lshift_node, NULL, shift_width);
	return rshift_node;
}

/**
 * returns true if it is assured, that the upper bits of a node are "clean"
 * which means for a 16 or 8 bit value, that the upper bits in the register
 * are 0 for unsigned and a copy of the last significant bit for signed
 * numbers.
 */
static bool upper_bits_clean(ir_node *transformed_node, ir_mode *mode)
{
	(void) transformed_node;
	(void) mode;
	/* TODO */
	return false;
}

/**
 * Extend a value to 32 bit signed/unsigned depending on its mode.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param orig_mode the original mode of op
 */
static ir_node *gen_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                              ir_mode *orig_mode)
{
	int bits = get_mode_size_bits(orig_mode);
	if (bits == 32)
		return op;

	if (mode_is_signed(orig_mode)) {
		return gen_sign_extension(dbgi, block, op, bits);
	} else {
		return gen_zero_extension(dbgi, block, op, bits);
	}
}

typedef enum {
	MATCH_NONE         = 0,
	MATCH_COMMUTATIVE  = 1U << 0, /**< commutative operation. */
	MATCH_MODE_NEUTRAL = 1U << 1, /**< the higher bits of the inputs don't
	                                   influence the significant lower bit at
	                                   all (for cases where mode < 32bit) */
} match_flags_t;

typedef ir_node* (*new_binop_reg_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2);
typedef ir_node* (*new_binop_fp_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode);
typedef ir_node* (*new_binop_imm_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_entity *entity, int32_t immediate);
typedef ir_node* (*new_unop_fp_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_mode *mode);

static bool is_value_imm_encodeable(int32_t value)
{
	return -4096 <= value && value <= 4095;
}

/**
 * checks if a node's value can be encoded as a immediate
 */
static bool is_imm_encodeable(const ir_node *node)
{
	long value;
	if (!is_Const(node))
		return false;

	value = get_tarval_long(get_Const_tarval(node));
	return is_value_imm_encodeable(value);
}

static bool needs_extension(ir_mode *mode)
{
	return get_mode_size_bits(mode) < get_mode_size_bits(mode_gp);
}

/**
 * Check, if a given node is a Down-Conv, ie. a integer Conv
 * from a mode with a mode with more bits to a mode with lesser bits.
 * Moreover, we return only true if the node has not more than 1 user.
 *
 * @param node   the node
 * @return non-zero if node is a Down-Conv
 */
static bool is_downconv(const ir_node *node)
{
	ir_mode *src_mode;
	ir_mode *dest_mode;

	if (!is_Conv(node))
		return false;

	src_mode  = get_irn_mode(get_Conv_op(node));
	dest_mode = get_irn_mode(node);
	return
		mode_needs_gp_reg(src_mode)  &&
		mode_needs_gp_reg(dest_mode) &&
		get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static ir_node *sparc_skip_downconv(ir_node *node)
{
	while (is_downconv(node)) {
		node = get_Conv_op(node);
	}
	return node;
}

/**
 * helper function for binop operations
 *
 * @param new_reg  register generation function ptr
 * @param new_imm  immediate generation function ptr
 */
static ir_node *gen_helper_binop_args(ir_node *node,
                                      ir_node *op1, ir_node *op2,
                                      match_flags_t flags,
                                      new_binop_reg_func new_reg,
                                      new_binop_imm_func new_imm)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *new_op1;
	ir_node  *new_op2;
	ir_mode  *mode1;
	ir_mode  *mode2;

	if (flags & MATCH_MODE_NEUTRAL) {
		op1 = sparc_skip_downconv(op1);
		op2 = sparc_skip_downconv(op2);
	}
	mode1 = get_irn_mode(op1);
	mode2 = get_irn_mode(op2);

	if (is_imm_encodeable(op2)) {
		ir_node *new_op1   = be_transform_node(op1);
		int32_t  immediate = get_tarval_long(get_Const_tarval(op2));
		if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(mode1)) {
			new_op1 = gen_extension(dbgi, block, new_op1, mode1);
		}
		return new_imm(dbgi, block, new_op1, NULL, immediate);
	}
	new_op2 = be_transform_node(op2);
	if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(mode2)) {
		new_op2 = gen_extension(dbgi, block, new_op2, mode2);
	}

	if ((flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		int32_t immediate = get_tarval_long(get_Const_tarval(op1));
		return new_imm(dbgi, block, new_op2, NULL, immediate);
	}

	new_op1 = be_transform_node(op1);
	if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(mode1)) {
		new_op1 = gen_extension(dbgi, block, new_op1, mode1);
	}
	return new_reg(dbgi, block, new_op1, new_op2);
}

static ir_node *gen_helper_binop(ir_node *node, match_flags_t flags,
                                 new_binop_reg_func new_reg,
                                 new_binop_imm_func new_imm)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	return gen_helper_binop_args(node, op1, op2, flags, new_reg, new_imm);
}

/**
 * helper function for FP binop operations
 */
static ir_node *gen_helper_binfpop(ir_node *node, ir_mode *mode,
                                   new_binop_fp_func new_func_single,
                                   new_binop_fp_func new_func_double,
                                   new_binop_fp_func new_func_quad)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_binop_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	unsigned  bits    = get_mode_size_bits(mode);

	switch (bits) {
	case 32:
		return new_func_single(dbgi, block, new_op1, new_op2, mode);
	case 64:
		return new_func_double(dbgi, block, new_op1, new_op2, mode);
	case 128:
		return new_func_quad(dbgi, block, new_op1, new_op2, mode);
	default:
		break;
	}
	panic("unsupported mode %+F for float op", mode);
}

static ir_node *gen_helper_unfpop(ir_node *node, ir_mode *mode,
                                  new_unop_fp_func new_func_single,
                                  new_unop_fp_func new_func_double,
                                  new_unop_fp_func new_func_quad)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	unsigned  bits    = get_mode_size_bits(mode);

	switch (bits) {
	case 32:
		return new_func_single(dbgi, block, new_op1, mode);
	case 64:
		return new_func_double(dbgi, block, new_op1, mode);
	case 128:
		return new_func_quad(dbgi, block, new_op1, mode);
	default:
		break;
	}
	panic("unsupported mode %+F for float op", mode);
}

static ir_node *get_g0(void)
{
	return be_prolog_get_reg_value(abihelper, &sparc_registers[REG_G0]);
}

typedef struct address_t {
	ir_node   *ptr;
	ir_node   *ptr2;
	ir_entity *entity;
	int32_t    offset;
} address_t;

/**
 * Match a load/store address
 */
static void match_address(ir_node *ptr, address_t *address, bool use_ptr2)
{
	ir_node   *base   = ptr;
	ir_node   *ptr2   = NULL;
	int32_t    offset = 0;
	ir_entity *entity = NULL;

	if (is_Add(base)) {
		ir_node *add_right = get_Add_right(base);
		if (is_Const(add_right)) {
			base    = get_Add_left(base);
			offset += get_tarval_long(get_Const_tarval(add_right));
		}
	}
	/* Note that we don't match sub(x, Const) or chains of adds/subs
	 * because this should all be normalized by now */

	/* we only use the symconst if we're the only user otherwise we probably
	 * won't save anything but produce multiple sethi+or combinations with
	 * just different offsets */
	if (is_SymConst(base) && get_irn_n_edges(base) == 1) {
		dbg_info *dbgi      = get_irn_dbg_info(ptr);
		ir_node  *block     = get_nodes_block(ptr);
		ir_node  *new_block = be_transform_node(block);
		entity = get_SymConst_entity(base);
		base   = new_bd_sparc_SetHi(dbgi, new_block, entity, offset);
	} else if (use_ptr2 && is_Add(base) && entity == NULL && offset == 0) {
		ptr2 = be_transform_node(get_Add_right(base));
		base = be_transform_node(get_Add_left(base));
	} else {
		if (is_value_imm_encodeable(offset)) {
			base = be_transform_node(base);
		} else {
			base   = be_transform_node(ptr);
			offset = 0;
		}
	}

	address->ptr    = base;
	address->ptr2   = ptr2;
	address->entity = entity;
	address->offset = offset;
}

/**
 * Creates an sparc Add.
 *
 * @param node   FIRM node
 * @return the created sparc Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	ir_node *right;

	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fadd_s,
		                          new_bd_sparc_fadd_d, new_bd_sparc_fadd_q);
	}

	/* special case: + 0x1000 can be represented as - 0x1000 */
	right = get_Add_right(node);
	if (is_Const(right)) {
		ir_node   *left = get_Add_left(node);
		ir_tarval *tv;
		uint32_t   val;
		/* is this simple address arithmetic? then we can let the linker do
		 * the calculation. */
		if (is_SymConst(left) && get_irn_n_edges(left) == 1) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = be_transform_node(get_nodes_block(node));
			address_t address;

			/* the value of use_ptr2 shouldn't matter here */
			match_address(node, &address, false);
			assert(is_sparc_SetHi(address.ptr));
			return new_bd_sparc_Or_imm(dbgi, block, address.ptr,
			                           address.entity, address.offset);
		}

		tv  = get_Const_tarval(right);
		val = get_tarval_long(tv);
		if (val == 0x1000) {
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_node(get_nodes_block(node));
			ir_node  *op     = get_Add_left(node);
			ir_node  *new_op = be_transform_node(op);
			return new_bd_sparc_Sub_imm(dbgi, block, new_op, NULL, -0x1000);
		}
	}

	return gen_helper_binop(node, MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                        new_bd_sparc_Add_reg, new_bd_sparc_Add_imm);
}

/**
 * Creates an sparc Sub.
 *
 * @param node       FIRM node
 * @return the created sparc Sub node
 */
static ir_node *gen_Sub(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fsub_s,
		                          new_bd_sparc_fsub_d, new_bd_sparc_fsub_q);
	}

	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Sub_reg, new_bd_sparc_Sub_imm);
}

static ir_node *create_ldf(dbg_info *dbgi, ir_node *block, ir_node *ptr,
                           ir_node *mem, ir_mode *mode, ir_entity *entity,
                           long offset, bool is_frame_entity)
{
	unsigned bits = get_mode_size_bits(mode);
	assert(mode_is_float(mode));
	if (bits == 32) {
		return new_bd_sparc_Ldf_s(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else if (bits == 64) {
		return new_bd_sparc_Ldf_d(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else {
		assert(bits == 128);
		return new_bd_sparc_Ldf_q(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	}
}

static ir_node *create_stf(dbg_info *dbgi, ir_node *block, ir_node *value,
                           ir_node *ptr, ir_node *mem, ir_mode *mode,
                           ir_entity *entity, long offset,
                           bool is_frame_entity)
{
	unsigned bits = get_mode_size_bits(mode);
	assert(mode_is_float(mode));
	if (bits == 32) {
		return new_bd_sparc_Stf_s(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else if (bits == 64) {
		return new_bd_sparc_Stf_d(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else {
		assert(bits == 128);
		return new_bd_sparc_Stf_q(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	}
}

/**
 * Transforms a Load.
 *
 * @param node    the ir Load node
 * @return the created sparc Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_mode  *mode     = get_Load_mode(node);
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *new_load = NULL;
	address_t address;

	if (mode_is_float(mode)) {
		match_address(ptr, &address, false);
		new_load = create_ldf(dbgi, block, address.ptr, new_mem, mode,
		                      address.entity, address.offset, false);
	} else {
		match_address(ptr, &address, true);
		if (address.ptr2 != NULL) {
			assert(address.entity == NULL && address.offset == 0);
			new_load = new_bd_sparc_Ld_reg(dbgi, block, address.ptr,
			                               address.ptr2, new_mem, mode);
		} else {
			new_load = new_bd_sparc_Ld_imm(dbgi, block, address.ptr, new_mem,
			                               mode, address.entity, address.offset,
			                               false);
		}
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

/**
 * Transforms a Store.
 *
 * @param node    the ir Store node
 * @return the created sparc Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Store_ptr(node);
	ir_node  *mem      = get_Store_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *val      = get_Store_value(node);
	ir_node  *new_val  = be_transform_node(val);
	ir_mode  *mode     = get_irn_mode(val);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_store = NULL;
	address_t address;

	if (mode_is_float(mode)) {
		/* TODO: variants with reg+reg address mode */
		match_address(ptr, &address, false);
		new_store = create_stf(dbgi, block, new_val, address.ptr, new_mem,
		                       mode, address.entity, address.offset, false);
	} else {
		match_address(ptr, &address, true);
		if (address.ptr2 != NULL) {
			assert(address.entity == NULL && address.offset == 0);
			new_store = new_bd_sparc_St_reg(dbgi, block, new_val, address.ptr,
			                                address.ptr2, new_mem, mode);
		} else {
			new_store = new_bd_sparc_St_imm(dbgi, block, new_val, address.ptr,
			                                new_mem, mode, address.entity,
			                                address.offset, false);
		}
	}
	set_irn_pinned(new_store, get_irn_pinned(node));

	return new_store;
}

/**
 * Creates an sparc Mul.
 * returns the lower 32bits of the 64bit multiply result
 *
 * @return the created sparc Mul node
 */
static ir_node *gen_Mul(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fmul_s,
		                          new_bd_sparc_fmul_d, new_bd_sparc_fmul_q);
	}

	return gen_helper_binop(node, MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                        new_bd_sparc_Mul_reg, new_bd_sparc_Mul_imm);
}

/**
 * Creates an sparc Mulh.
 * Mulh returns the upper 32bits of a mul instruction
 *
 * @return the created sparc Mulh node
 */
static ir_node *gen_Mulh(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	ir_node *mul;

	if (mode_is_float(mode))
		panic("FP not supported yet");

	mul = gen_helper_binop(node, MATCH_COMMUTATIVE, new_bd_sparc_Mulh_reg, new_bd_sparc_Mulh_imm);
	return new_r_Proj(mul, mode_gp, pn_sparc_Mulh_low);
}

static ir_node *gen_sign_extension_value(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	ir_node *new_node  = be_transform_node(node);
	/* TODO: we could do some shortcuts for some value types probably.
	 * (For constants or other cases where we know the sign bit in
	 *  advance) */
	return new_bd_sparc_Sra_imm(NULL, new_block, new_node, NULL, 31);
}

/**
 * Creates an sparc Div.
 *
 * @return the created sparc Div node
 */
static ir_node *gen_Div(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_mode  *mode      = get_Div_resmode(node);
	ir_node  *left      = get_Div_left(node);
	ir_node  *left_low  = be_transform_node(left);
	ir_node  *right     = get_Div_right(node);
	ir_node  *res;

	assert(!mode_is_float(mode));
	if (mode_is_signed(mode)) {
		ir_node *left_high = gen_sign_extension_value(left);

		if (is_imm_encodeable(right)) {
			int32_t immediate = get_tarval_long(get_Const_tarval(right));
			res = new_bd_sparc_SDiv_imm(dbgi, new_block, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_SDiv_reg(dbgi, new_block, left_high, left_low,
			                            new_right);
		}
	} else {
		ir_node *left_high = get_g0();
		if (is_imm_encodeable(right)) {
			int32_t immediate = get_tarval_long(get_Const_tarval(right));
			res = new_bd_sparc_UDiv_imm(dbgi, new_block, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_UDiv_reg(dbgi, new_block, left_high, left_low,
			                            new_right);
		}
	}

	return res;
}

static ir_node *gen_Quot(ir_node *node)
{
	ir_mode *mode = get_Quot_resmode(node);
	assert(mode_is_float(mode));
	return gen_helper_binfpop(node, mode, new_bd_sparc_fdiv_s,
	                          new_bd_sparc_fdiv_d, new_bd_sparc_fdiv_q);
}

#if 0
static ir_node *gen_Abs(ir_node *node)
{
	ir_mode *const mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_helper_unfpop(node, mode, new_bd_sparc_fabs_s,
		                         new_bd_sparc_fabs_d, new_bd_sparc_fabs_q);
	} else {
		ir_node  *const block  = be_transform_node(get_nodes_block(node));
		dbg_info *const dbgi   = get_irn_dbg_info(node);
		ir_node  *const op     = get_Abs_op(node);
		ir_node  *const new_op = be_transform_node(op);
		ir_node  *const sra    = new_bd_sparc_Sra_imm(dbgi, block, new_op, NULL, 31);
		ir_node  *const xor    = new_bd_sparc_Xor_reg(dbgi, block, new_op, sra);
		ir_node  *const sub    = new_bd_sparc_Sub_reg(dbgi, block, xor,    sra);
		return sub;
	}
}
#endif

/**
 * Transforms a Not node.
 *
 * @return the created sparc Not node
 */
static ir_node *gen_Not(ir_node *node)
{
	ir_node  *op     = get_Not_op(node);
	ir_node  *zero   = get_g0();
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *new_op = be_transform_node(op);

	/* Note: Not(Eor()) is normalize in firm locatopts already so
	 * we don't match it for xnor here */

	/* Not can be represented with xnor 0, n */
	return new_bd_sparc_XNor_reg(dbgi, block, zero, new_op);
}

static ir_node *gen_helper_bitop(ir_node *node,
                                 new_binop_reg_func new_reg,
                                 new_binop_imm_func new_imm,
                                 new_binop_reg_func new_not_reg,
                                 new_binop_imm_func new_not_imm)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	if (is_Not(op1)) {
		return gen_helper_binop_args(node, op2, get_Not_op(op1),
		                             MATCH_MODE_NEUTRAL,
		                             new_not_reg, new_not_imm);
	}
	if (is_Not(op2)) {
		return gen_helper_binop_args(node, op1, get_Not_op(op2),
		                             MATCH_MODE_NEUTRAL,
		                             new_not_reg, new_not_imm);
	}
	return gen_helper_binop_args(node, op1, op2,
								 MATCH_MODE_NEUTRAL | MATCH_COMMUTATIVE,
								 new_reg, new_imm);
}

static ir_node *gen_And(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_And_reg,
	                        new_bd_sparc_And_imm,
	                        new_bd_sparc_AndN_reg,
	                        new_bd_sparc_AndN_imm);
}

static ir_node *gen_Or(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_Or_reg,
	                        new_bd_sparc_Or_imm,
	                        new_bd_sparc_OrN_reg,
	                        new_bd_sparc_OrN_imm);
}

static ir_node *gen_Eor(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_Xor_reg,
	                        new_bd_sparc_Xor_imm,
	                        new_bd_sparc_XNor_reg,
	                        new_bd_sparc_XNor_imm);
}

static ir_node *gen_Shl(ir_node *node)
{
	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Sll_reg, new_bd_sparc_Sll_imm);
}

static ir_node *gen_Shr(ir_node *node)
{
	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Srl_reg, new_bd_sparc_Srl_imm);
}

static ir_node *gen_Shrs(ir_node *node)
{
	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Sra_reg, new_bd_sparc_Sra_imm);
}

/**
 * Transforms a Minus node.
 */
static ir_node *gen_Minus(ir_node *node)
{
	ir_mode  *mode = get_irn_mode(node);
	ir_node  *op;
	ir_node  *block;
	ir_node  *new_op;
	ir_node  *zero;
	dbg_info *dbgi;

	if (mode_is_float(mode)) {
		return gen_helper_unfpop(node, mode, new_bd_sparc_fneg_s,
		                         new_bd_sparc_fneg_d, new_bd_sparc_fneg_q);
	}
	block  = be_transform_node(get_nodes_block(node));
	dbgi   = get_irn_dbg_info(node);
	op     = get_Minus_op(node);
	new_op = be_transform_node(op);
	zero   = get_g0();
	return new_bd_sparc_Sub_reg(dbgi, block, zero, new_op);
}

/**
 * Create an entity for a given (floating point) tarval
 */
static ir_entity *create_float_const_entity(ir_tarval *tv)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(current_ir_graph);
	sparc_isa_t      *isa      = (sparc_isa_t*) arch_env;
	ir_entity        *entity   = (ir_entity*) pmap_get(isa->constants, tv);
	ir_initializer_t *initializer;
	ir_mode          *mode;
	ir_type          *type;
	ir_type          *glob;

	if (entity != NULL)
		return entity;

	mode   = get_tarval_mode(tv);
	type   = get_type_for_mode(mode);
	glob   = get_glob_type();
	entity = new_entity(glob, id_unique("C%u"), type);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(isa->constants, tv, entity);
	return entity;
}

static ir_node *gen_float_const(dbg_info *dbgi, ir_node *block, ir_tarval *tv)
{
	ir_entity *entity = create_float_const_entity(tv);
	ir_node   *hi     = new_bd_sparc_SetHi(dbgi, block, entity, 0);
	ir_node   *mem    = new_r_NoMem(current_ir_graph);
	ir_mode   *mode   = get_tarval_mode(tv);
	ir_node   *new_op
		= create_ldf(dbgi, block, hi, mem, mode, entity, 0, false);
	ir_node   *proj   = new_Proj(new_op, mode, pn_sparc_Ldf_res);
	be_dep_on_frame(hi);

	set_irn_pinned(new_op, op_pin_state_floats);
	return proj;
}

static ir_node *gen_Const(ir_node *node)
{
	ir_node   *block = be_transform_node(get_nodes_block(node));
	ir_mode   *mode  = get_irn_mode(node);
	dbg_info  *dbgi  = get_irn_dbg_info(node);
	ir_tarval *tv    = get_Const_tarval(node);
	long       value;

	if (mode_is_float(mode)) {
		return gen_float_const(dbgi, block, tv);
	}

	value = get_tarval_long(tv);
	if (value == 0) {
		return get_g0();
	} else if (-4096 <= value && value <= 4095) {
		return new_bd_sparc_Or_imm(dbgi, block, get_g0(), NULL, value);
	} else {
		ir_node *hi = new_bd_sparc_SetHi(dbgi, block, NULL, value);
		be_dep_on_frame(hi);
		if ((value & 0x3ff) != 0) {
			return new_bd_sparc_Or_imm(dbgi, block, hi, NULL, value & 0x3ff);
		} else {
			return hi;
		}
	}
}

static ir_mode *get_cmp_mode(ir_node *b_value)
{
	ir_node *pred;
	ir_node *op;

	if (!is_Proj(b_value))
		panic("can't determine cond signednes");
	pred = get_Proj_pred(b_value);
	if (!is_Cmp(pred))
		panic("can't determine cond signednes (no cmp)");
	op = get_Cmp_left(pred);
	return get_irn_mode(op);
}

static ir_node *make_address(dbg_info *dbgi, ir_node *block, ir_entity *entity,
                             int32_t offset)
{
	ir_node *hi  = new_bd_sparc_SetHi(dbgi, block, entity, offset);
	ir_node *low = new_bd_sparc_Or_imm(dbgi, block, hi, entity, offset);
	be_dep_on_frame(hi);
	return low;
}

static ir_node *gen_SwitchJmp(ir_node *node)
{
	dbg_info        *dbgi         = get_irn_dbg_info(node);
	ir_node         *block        = be_transform_node(get_nodes_block(node));
	ir_node         *selector     = get_Cond_selector(node);
	ir_node         *new_selector = be_transform_node(selector);
	long             default_pn   = get_Cond_default_proj(node);
	ir_entity       *entity;
	ir_node         *table_address;
	ir_node         *index;
	ir_node         *load;
	ir_node         *address;

	/* switch with smaller mode not implemented yet */
	assert(get_mode_size_bits(get_irn_mode(selector)) == 32);

	entity = new_entity(NULL, id_unique("TBL%u"), get_unknown_type());
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	/* TODO: this code does not construct code to check for access
	 * out-of bounds of the jumptable yet. I think we should put this stuff
	 * into the switch_lowering phase to get some additional optimisations
	 * done. */

	/* construct base address */
	table_address = make_address(dbgi, block, entity, 0);
	/* scale index */
	index = new_bd_sparc_Sll_imm(dbgi, block, new_selector, NULL, 2);
	/* load from jumptable */
	load = new_bd_sparc_Ld_reg(dbgi, block, table_address, index,
	                           new_r_NoMem(current_ir_graph),
	                           mode_gp);
	address = new_r_Proj(load, mode_gp, pn_sparc_Ld_res);

	return new_bd_sparc_SwitchJmp(dbgi, block, address, default_pn, entity);
}

static ir_node *gen_Cond(ir_node *node)
{
	ir_node  *selector = get_Cond_selector(node);
	ir_mode  *mode     = get_irn_mode(selector);
	ir_mode  *cmp_mode;
	ir_node  *block;
	ir_node  *flag_node;
	bool      is_unsigned;
	pn_Cmp    pnc;
	dbg_info *dbgi;

	// switch/case jumps
	if (mode != mode_b) {
		return gen_SwitchJmp(node);
	}

	// regular if/else jumps
	assert(is_Proj(selector));
	assert(is_Cmp(get_Proj_pred(selector)));

	cmp_mode = get_cmp_mode(selector);

	block       = be_transform_node(get_nodes_block(node));
	dbgi        = get_irn_dbg_info(node);
	flag_node   = be_transform_node(get_Proj_pred(selector));
	pnc         = get_Proj_proj(selector);
	is_unsigned = !mode_is_signed(cmp_mode);
	if (mode_is_float(cmp_mode)) {
		assert(!is_unsigned);
		return new_bd_sparc_fbfcc(dbgi, block, flag_node, pnc);
	} else {
		return new_bd_sparc_Bicc(dbgi, block, flag_node, pnc, is_unsigned);
	}
}

/**
 * transform Cmp
 */
static ir_node *gen_Cmp(ir_node *node)
{
	ir_node *op1      = get_Cmp_left(node);
	ir_node *op2      = get_Cmp_right(node);
	ir_mode *cmp_mode = get_irn_mode(op1);
	assert(get_irn_mode(op2) == cmp_mode);

	if (mode_is_float(cmp_mode)) {
		ir_node  *block   = be_transform_node(get_nodes_block(node));
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		unsigned  bits    = get_mode_size_bits(cmp_mode);
		if (bits == 32) {
			return new_bd_sparc_fcmp_s(dbgi, block, new_op1, new_op2, cmp_mode);
		} else if (bits == 64) {
			return new_bd_sparc_fcmp_d(dbgi, block, new_op1, new_op2, cmp_mode);
		} else {
			assert(bits == 128);
			return new_bd_sparc_fcmp_q(dbgi, block, new_op1, new_op2, cmp_mode);
		}
	}

	/* when we compare a bitop like and,or,... with 0 then we can directly use
	 * the bitopcc variant.
	 * Currently we only do this when we're the only user of the node...
	 */
	if (is_Const(op2) && is_Const_null(op2) && get_irn_n_edges(op1) == 1) {
		if (is_And(op1)) {
			return gen_helper_bitop(op1,
			                        new_bd_sparc_AndCCZero_reg,
			                        new_bd_sparc_AndCCZero_imm,
			                        new_bd_sparc_AndNCCZero_reg,
			                        new_bd_sparc_AndNCCZero_imm);
		} else if (is_Or(op1)) {
			return gen_helper_bitop(op1,
			                        new_bd_sparc_OrCCZero_reg,
			                        new_bd_sparc_OrCCZero_imm,
			                        new_bd_sparc_OrNCCZero_reg,
			                        new_bd_sparc_OrNCCZero_imm);
		} else if (is_Eor(op1)) {
			return gen_helper_bitop(op1,
			                        new_bd_sparc_XorCCZero_reg,
			                        new_bd_sparc_XorCCZero_imm,
			                        new_bd_sparc_XNorCCZero_reg,
			                        new_bd_sparc_XNorCCZero_imm);
		}
	}

	/* integer compare */
	return gen_helper_binop_args(node, op1, op2, MATCH_NONE,
	                             new_bd_sparc_Cmp_reg, new_bd_sparc_Cmp_imm);
}

/**
 * Transforms a SymConst node.
 */
static ir_node *gen_SymConst(ir_node *node)
{
	ir_entity *entity    = get_SymConst_entity(node);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
	return make_address(dbgi, new_block, entity, 0);
}

static ir_node *create_fftof(dbg_info *dbgi, ir_node *block, ir_node *op,
                             ir_mode *src_mode, ir_mode *dst_mode)
{
	unsigned src_bits = get_mode_size_bits(src_mode);
	unsigned dst_bits = get_mode_size_bits(dst_mode);
	if (src_bits == 32) {
		if (dst_bits == 64) {
			return new_bd_sparc_fftof_s_d(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 128);
			return new_bd_sparc_fftof_s_q(dbgi, block, op, src_mode, dst_mode);
		}
	} else if (src_bits == 64) {
		if (dst_bits == 32) {
			return new_bd_sparc_fftof_d_s(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 128);
			return new_bd_sparc_fftof_d_q(dbgi, block, op, src_mode, dst_mode);
		}
	} else {
		assert(src_bits == 128);
		if (dst_bits == 32) {
			return new_bd_sparc_fftof_q_s(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 64);
			return new_bd_sparc_fftof_q_d(dbgi, block, op, src_mode, dst_mode);
		}
	}
}

static ir_node *create_ftoi(dbg_info *dbgi, ir_node *block, ir_node *op,
                            ir_mode *src_mode)
{
	ir_node  *ftoi;
	unsigned  bits = get_mode_size_bits(src_mode);
	if (bits == 32) {
		ftoi = new_bd_sparc_fftoi_s(dbgi, block, op, src_mode);
	} else if (bits == 64) {
		ftoi = new_bd_sparc_fftoi_d(dbgi, block, op, src_mode);
	} else {
		assert(bits == 128);
		ftoi = new_bd_sparc_fftoi_q(dbgi, block, op, src_mode);
	}

	{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = new_r_NoMem(irg);
	ir_node  *stf   = create_stf(dbgi, block, ftoi, sp, nomem, src_mode,
	                             NULL, 0, true);
	ir_node  *ld    = new_bd_sparc_Ld_imm(dbgi, block, sp, stf, mode_gp,
	                                      NULL, 0, true);
	ir_node  *res   = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);
	set_irn_pinned(stf, op_pin_state_floats);
	set_irn_pinned(ld, op_pin_state_floats);
	return res;
	}
}

static ir_node *create_itof(dbg_info *dbgi, ir_node *block, ir_node *op,
                            ir_mode *dst_mode)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = new_r_NoMem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, op, sp, nomem,
	                                      mode_gp, NULL, 0, true);
	ir_node  *ldf   = new_bd_sparc_Ldf_s(dbgi, block, sp, st, mode_fp,
	                                     NULL, 0, true);
	ir_node  *res   = new_r_Proj(ldf, mode_fp, pn_sparc_Ldf_res);
	unsigned  bits  = get_mode_size_bits(dst_mode);
	set_irn_pinned(st, op_pin_state_floats);
	set_irn_pinned(ldf, op_pin_state_floats);

	if (bits == 32) {
		return new_bd_sparc_fitof_s(dbgi, block, res, dst_mode);
	} else if (bits == 64) {
		return new_bd_sparc_fitof_d(dbgi, block, res, dst_mode);
	} else {
		assert(bits == 128);
		return new_bd_sparc_fitof_q(dbgi, block, res, dst_mode);
	}
}

static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbg      = get_irn_dbg_info(node);

	int src_bits = get_mode_size_bits(src_mode);
	int dst_bits = get_mode_size_bits(dst_mode);

	if (src_mode == dst_mode)
		return new_op;

	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		assert((src_bits <= 64 && dst_bits <= 64) && "quad FP not implemented");

		if (mode_is_float(src_mode)) {
			if (mode_is_float(dst_mode)) {
				/* float -> float conv */
				return create_fftof(dbg, block, new_op, src_mode, dst_mode);
			} else {
				/* float -> int conv */
				if (!mode_is_signed(dst_mode))
					panic("float to unsigned not implemented yet");
				return create_ftoi(dbg, block, new_op, src_mode);
			}
		} else {
			/* int -> float conv */
			if (src_bits < 32) {
				new_op = gen_extension(dbg, block, new_op, src_mode);
			} else if (src_bits == 32 && !mode_is_signed(src_mode)) {
				panic("unsigned to float not lowered!");
			}
			return create_itof(dbg, block, new_op, dst_mode);
		}
	} else { /* complete in gp registers */
		int min_bits;
		ir_mode *min_mode;

		if (src_bits == dst_bits) {
			/* kill unnecessary conv */
			return new_op;
		}

		if (src_bits < dst_bits) {
			min_bits = src_bits;
			min_mode = src_mode;
		} else {
			min_bits = dst_bits;
			min_mode = dst_mode;
		}

		if (upper_bits_clean(new_op, min_mode)) {
			return new_op;
		}

		if (mode_is_signed(min_mode)) {
			return gen_sign_extension(dbg, block, new_op, min_bits);
		} else {
			return gen_zero_extension(dbg, block, new_op, min_bits);
		}
	}
}

static ir_node *gen_Unknown(ir_node *node)
{
	/* just produce a 0 */
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		ir_node *block = be_transform_node(get_nodes_block(node));
		return gen_float_const(NULL, block, get_mode_null(mode));
	} else if (mode_needs_gp_reg(mode)) {
		return get_g0();
	}

	panic("Unexpected Unknown mode");
}

/**
 * Produces the type which sits between the stack args and the locals on the
 * stack.
 */
static ir_type *sparc_get_between_type(void)
{
	static ir_type *between_type = NULL;

	if (between_type == NULL) {
		between_type = new_type_class(new_id_from_str("sparc_between_type"));
		set_type_size_bytes(between_type, SPARC_MIN_STACKSIZE);
	}

	return between_type;
}

static void create_stacklayout(ir_graph *irg)
{
	ir_entity         *entity        = get_irg_entity(irg);
	ir_type           *function_type = get_entity_type(entity);
	be_stack_layout_t *layout        = be_get_irg_stack_layout(irg);
	ir_type           *arg_type;
	int                p;
	int                n_params;

	/* calling conventions must be decided by now */
	assert(cconv != NULL);

	/* construct argument type */
	arg_type = new_type_struct(id_mangle_u(get_entity_ident(entity), new_id_from_chars("arg_type", 8)));
	n_params = get_method_n_params(function_type);
	for (p = 0; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		char                buf[128];
		ident              *id;

		if (param->type == NULL)
			continue;

		snprintf(buf, sizeof(buf), "param_%d", p);
		id            = new_id_from_str(buf);
		param->entity = new_entity(arg_type, id, param->type);
		set_entity_offset(param->entity, param->offset);
	}

	memset(layout, 0, sizeof(*layout));

	layout->frame_type     = get_irg_frame_type(irg);
	layout->between_type   = sparc_get_between_type();
	layout->arg_type       = arg_type;
	layout->initial_offset = 0;
	layout->initial_bias   = 0;
	layout->stack_dir      = -1;
	layout->sp_relative    = false;

	assert(N_FRAME_TYPES == 3);
	layout->order[0] = layout->frame_type;
	layout->order[1] = layout->between_type;
	layout->order[2] = layout->arg_type;
}

/**
 * transform the start node to the prolog code + initial barrier
 */
static ir_node *gen_Start(ir_node *node)
{
	ir_graph  *irg           = get_irn_irg(node);
	ir_entity *entity        = get_irg_entity(irg);
	ir_type   *function_type = get_entity_type(entity);
	ir_node   *block         = get_nodes_block(node);
	ir_node   *new_block     = be_transform_node(block);
	dbg_info  *dbgi          = get_irn_dbg_info(node);
	ir_node   *mem;
	ir_node   *start;
	ir_node   *sp;
	ir_node   *fp;
	ir_node   *barrier;
	ir_node   *save;
	int        i;

	/* stackpointer is important at function prolog */
	be_prolog_add_reg(abihelper, sp_reg,
			arch_register_req_type_produces_sp | arch_register_req_type_ignore);
	be_prolog_add_reg(abihelper, &sparc_registers[REG_G0],
	        arch_register_req_type_ignore);
	/* function parameters in registers */
	for (i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &cconv->parameters[i];
		if (param->reg0 != NULL)
			be_prolog_add_reg(abihelper, param->reg0, 0);
		if (param->reg1 != NULL)
			be_prolog_add_reg(abihelper, param->reg1, 0);
	}

	start = be_prolog_create_start(abihelper, dbgi, new_block);

	mem  = be_prolog_get_memory(abihelper);
	sp   = be_prolog_get_reg_value(abihelper, sp_reg);
	save = new_bd_sparc_Save(NULL, block, sp, mem, SPARC_MIN_STACKSIZE);
	fp   = new_r_Proj(save, mode_gp, pn_sparc_Save_frame);
	sp   = new_r_Proj(save, mode_gp, pn_sparc_Save_stack);
	mem  = new_r_Proj(save, mode_M, pn_sparc_Save_mem);
	arch_set_irn_register(fp, fp_reg);
	arch_set_irn_register(sp, sp_reg);

	be_prolog_add_reg(abihelper, fp_reg, arch_register_req_type_ignore);
	be_prolog_set_reg_value(abihelper, fp_reg, fp);

	sp = be_new_IncSP(sp_reg, new_block, sp, BE_STACK_FRAME_SIZE_EXPAND, 0);
	be_prolog_set_reg_value(abihelper, sp_reg, sp);
	be_prolog_set_memory(abihelper, mem);

	barrier = be_prolog_create_barrier(abihelper, new_block);

	return barrier;
}

static ir_node *get_stack_pointer_for(ir_node *node)
{
	/* get predecessor in stack_order list */
	ir_node *stack_pred = be_get_stack_pred(abihelper, node);
	ir_node *stack_pred_transformed;
	ir_node *stack;

	if (stack_pred == NULL) {
		/* first stack user in the current block. We can simply use the
		 * initial sp_proj for it */
		ir_node *sp_proj = be_prolog_get_reg_value(abihelper, sp_reg);
		return sp_proj;
	}

	stack_pred_transformed = be_transform_node(stack_pred);
	stack                  = pmap_get(node_to_stack, stack_pred);
	if (stack == NULL) {
		return get_stack_pointer_for(stack_pred);
	}

	return stack;
}

/**
 * transform a Return node into epilogue code + return statement
 */
static ir_node *gen_Return(ir_node *node)
{
	ir_node  *block          = get_nodes_block(node);
	ir_node  *new_block      = be_transform_node(block);
	dbg_info *dbgi           = get_irn_dbg_info(node);
	ir_node  *mem            = get_Return_mem(node);
	ir_node  *new_mem        = be_transform_node(mem);
	ir_node  *sp_proj        = get_stack_pointer_for(node);
	int       n_res          = get_Return_n_ress(node);
	ir_node  *bereturn;
	ir_node  *incsp;
	int       i;

	be_epilog_begin(abihelper);
	be_epilog_set_memory(abihelper, new_mem);
	/* connect stack pointer with initial stack pointer. fix_stack phase
	   will later serialize all stack pointer adjusting nodes */
	be_epilog_add_reg(abihelper, sp_reg,
			arch_register_req_type_produces_sp | arch_register_req_type_ignore,
			sp_proj);

	/* result values */
	for (i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &cconv->results[i];
		const arch_register_t    *reg           = slot->reg0;
		assert(slot->reg1 == NULL);
		be_epilog_add_reg(abihelper, reg, 0, new_res_value);
	}

	/* create the barrier before the epilog code */
	be_epilog_create_barrier(abihelper, new_block);

	/* epilog code: an incsp */
	sp_proj = be_epilog_get_reg_value(abihelper, sp_reg);
	incsp   = be_new_IncSP(sp_reg, new_block, sp_proj,
	                       BE_STACK_FRAME_SIZE_SHRINK, 0);
	be_epilog_set_reg_value(abihelper, sp_reg, incsp);

	bereturn = be_epilog_create_return(abihelper, dbgi, new_block);

	return bereturn;
}

static ir_node *bitcast_int_to_float(dbg_info *dbgi, ir_node *block,
                                     ir_node *value0, ir_node *value1)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = new_r_NoMem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, value0, sp, nomem,
	                                      mode_gp, NULL, 0, true);
	ir_mode  *mode;
	ir_node  *ldf;
	ir_node  *mem;
	set_irn_pinned(st, op_pin_state_floats);

	if (value1 != NULL) {
		ir_node *st1 = new_bd_sparc_St_imm(dbgi, block, value1, sp, nomem,
		                                   mode_gp, NULL, 4, true);
		ir_node *in[2] = { st, st1 };
		ir_node *sync  = new_r_Sync(block, 2, in);
		set_irn_pinned(st1, op_pin_state_floats);
		mem  = sync;
		mode = mode_fp2;
	} else {
		mem  = st;
		mode = mode_fp;
	}

	ldf = create_ldf(dbgi, block, sp, mem, mode, NULL, 0, true);
	set_irn_pinned(ldf, op_pin_state_floats);

	return new_r_Proj(ldf, mode, pn_sparc_Ldf_res);
}

static void bitcast_float_to_int(dbg_info *dbgi, ir_node *block,
                                 ir_node *node, ir_mode *float_mode,
                                 ir_node **result)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *stack = get_irg_frame(irg);
	ir_node  *nomem = new_r_NoMem(irg);
	ir_node  *stf   = create_stf(dbgi, block, node, stack, nomem, float_mode,
	                             NULL, 0, true);
	int       bits  = get_mode_size_bits(float_mode);
	ir_node  *ld;
	set_irn_pinned(stf, op_pin_state_floats);

	ld = new_bd_sparc_Ld_imm(dbgi, block, stack, stf, mode_gp, NULL, 0, true);
	set_irn_pinned(ld, op_pin_state_floats);
	result[0] = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);

	if (bits == 64) {
		ir_node *ld2 = new_bd_sparc_Ld_imm(dbgi, block, stack, stf, mode_gp,
		                                   NULL, 4, true);
		set_irn_pinned(ld, op_pin_state_floats);
		result[1] = new_r_Proj(ld2, mode_gp, pn_sparc_Ld_res);

		arch_irn_add_flags(ld, sparc_arch_irn_flag_needs_64bit_spillslot);
		arch_irn_add_flags(ld2, sparc_arch_irn_flag_needs_64bit_spillslot);
	} else {
		assert(bits == 32);
		result[1] = NULL;
	}
}

static ir_node *gen_Call(ir_node *node)
{
	ir_graph        *irg          = get_irn_irg(node);
	ir_node         *callee       = get_Call_ptr(node);
	ir_node         *block        = get_nodes_block(node);
	ir_node         *new_block    = be_transform_node(block);
	ir_node         *mem          = get_Call_mem(node);
	ir_node         *new_mem      = be_transform_node(mem);
	dbg_info        *dbgi         = get_irn_dbg_info(node);
	ir_type         *type         = get_Call_type(node);
	int              n_params     = get_Call_n_params(node);
	int              n_param_regs = sizeof(param_regs)/sizeof(param_regs[0]);
	/* max inputs: memory, callee, register arguments */
	int              max_inputs   = 2 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	ir_node        **sync_ins     = ALLOCAN(ir_node*, n_params);
	struct obstack  *obst         = be_get_be_obst(irg);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	calling_convention_t *cconv
		= sparc_decide_calling_convention(type, true);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	int              n_caller_saves
		= sizeof(caller_saves)/sizeof(caller_saves[0]);
	ir_entity       *entity       = NULL;
	ir_node         *new_frame    = get_stack_pointer_for(node);
	ir_node         *incsp;
	int              mem_pos;
	ir_node         *res;
	int              p;
	int              i;
	int              o;
	int              out_arity;

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* memory input */
	in_req[in_arity] = arch_no_register_req;
	mem_pos          = in_arity;
	++in_arity;

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	incsp = be_new_IncSP(sp_reg, new_block, new_frame,
	                     cconv->param_stack_size, 1);
	in_req[in_arity] = sp_reg->single_req;
	in[in_arity]     = incsp;
	++in_arity;

	/* parameters */
	for (p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		ir_node                  *new_value  = be_transform_node(value);
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);
		ir_node                  *new_values[2];
		ir_node                  *str;

		if (mode_is_float(mode) && param->reg0 != NULL) {
			unsigned size_bits = get_mode_size_bits(mode);
			assert(size_bits <= 64);
			bitcast_float_to_int(dbgi, new_block, new_value, mode, new_values);
		} else {
			new_values[0] = new_value;
			new_values[1] = NULL;
		}

		/* put value into registers */
		if (param->reg0 != NULL) {
			in[in_arity]     = new_values[0];
			in_req[in_arity] = param->reg0->single_req;
			++in_arity;
			if (new_values[1] == NULL)
				continue;
		}
		if (param->reg1 != NULL) {
			assert(new_values[1] != NULL);
			in[in_arity]     = new_values[1];
			in_req[in_arity] = param->reg1->single_req;
			++in_arity;
			continue;
		}

		/* we need a store if we're here */
		if (new_values[1] != NULL) {
			new_value = new_values[1];
			mode      = mode_gp;
		}

		/* create a parameter frame if necessary */
		if (mode_is_float(mode)) {
			str = create_stf(dbgi, new_block, new_value, incsp, new_mem,
			                 mode, NULL, param->offset, true);
		} else {
			str = new_bd_sparc_St_imm(dbgi, new_block, new_value, incsp,
			                          new_mem, mode, NULL, param->offset, true);
		}
		set_irn_pinned(str, op_pin_state_floats);
		sync_ins[sync_arity++] = str;
	}
	assert(in_arity <= max_inputs);

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else if (sync_arity == 1) {
		in[mem_pos] = sync_ins[0];
	} else {
		in[mem_pos] = new_rd_Sync(NULL, new_block, sync_arity, sync_ins);
	}

	if (is_SymConst(callee)) {
		entity = get_SymConst_entity(callee);
	} else {
		in[in_arity]     = be_transform_node(callee);
		in_req[in_arity] = sparc_reg_classes[CLASS_sparc_gp].class_req;
		++in_arity;
	}

	/* outputs:
	 *  - memory
	 *  - caller saves
	 */
	out_arity = 1 + n_caller_saves;

	/* create call node */
	if (entity != NULL) {
		res = new_bd_sparc_Call_imm(dbgi, new_block, in_arity, in, out_arity,
		                            entity, 0);
	} else {
		res = new_bd_sparc_Call_reg(dbgi, new_block, in_arity, in, out_arity);
	}
	arch_set_in_register_reqs(res, in_req);

	/* create output register reqs */
	o = 0;
	arch_set_out_register_req(res, o++, arch_no_register_req);
	for (i = 0; i < n_caller_saves; ++i) {
		const arch_register_t *reg = caller_saves[i];
		arch_set_out_register_req(res, o++, reg->single_req);
	}
	assert(o == out_arity);

	/* copy pinned attribute */
	set_irn_pinned(res, get_irn_pinned(node));

	/* IncSP to destroy the call stackframe */
	incsp = be_new_IncSP(sp_reg, new_block, incsp, -cconv->param_stack_size, 0);
	/* if we are the last IncSP producer in a block then we have to keep
	 * the stack value.
	 * Note: This here keeps all producers which is more than necessary */
	add_irn_dep(incsp, res);
	keep_alive(incsp);

	pmap_insert(node_to_stack, node, incsp);

	sparc_free_calling_convention(cconv);
	return res;
}

static ir_node *gen_Sel(ir_node *node)
{
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
	ir_node   *ptr       = get_Sel_ptr(node);
	ir_node   *new_ptr   = be_transform_node(ptr);
	ir_entity *entity    = get_Sel_entity(node);

	/* must be the frame pointer all other sels must have been lowered
	 * already */
	assert(is_Proj(ptr) && is_Start(get_Proj_pred(ptr)));
	/* we should not have value types from parameters anymore - they should be
	   lowered */
	assert(get_entity_owner(entity) !=
			get_method_value_param_type(get_entity_type(get_irg_entity(get_irn_irg(node)))));

	return new_bd_sparc_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
}

static const arch_register_req_t float1_req = {
	arch_register_req_type_normal,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	1
};
static const arch_register_req_t float2_req = {
	arch_register_req_type_normal | arch_register_req_type_aligned,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	2
};
static const arch_register_req_t float4_req = {
	arch_register_req_type_normal | arch_register_req_type_aligned,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	4
};


static const arch_register_req_t *get_float_req(ir_mode *mode)
{
	unsigned bits = get_mode_size_bits(mode);

	assert(mode_is_float(mode));
	if (bits == 32) {
		return &float1_req;
	} else if (bits == 64) {
		return &float2_req;
	} else {
		assert(bits == 128);
		return &float4_req;
	}
}

/**
 * Transform some Phi nodes
 */
static ir_node *gen_Phi(ir_node *node)
{
	const arch_register_req_t *req;
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *phi;

	if (mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		mode = mode_gp;
		req  = sparc_reg_classes[CLASS_sparc_gp].class_req;
	} else if (mode_is_float(mode)) {
		mode = mode;
		req  = get_float_req(mode);
	} else {
		req = arch_no_register_req;
	}

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	phi = new_ir_node(dbgi, irg, block, op_Phi, mode, get_irn_arity(node), get_irn_in(node) + 1);
	copy_node_attr(irg, node, phi);
	be_duplicate_deps(node, phi);
	arch_set_out_register_req(phi, 0, req);
	be_enqueue_preds(node);
	return phi;
}

/**
 * Transform a Proj from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      pn       = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_load)) {
	case iro_sparc_Ld:
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_gp, pn_sparc_Ld_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_sparc_Ld_M);
		}
		break;
	case iro_sparc_Ldf:
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_fp, pn_sparc_Ldf_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_sparc_Ld_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Load");
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node  *store     = get_Proj_pred(node);
	ir_node  *new_store = be_transform_node(store);
	long      pn        = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_store)) {
	case iro_sparc_St:
		if (pn == pn_Store_M) {
			return new_store;
		}
		break;
	case iro_sparc_Stf:
		if (pn == pn_Store_M) {
			return new_store;
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Store");
}

/**
 * Transform the Projs from a Cmp.
 */
static ir_node *gen_Proj_Cmp(ir_node *node)
{
	(void) node;
	panic("not implemented");
}

/**
 * transform Projs from a Div
 */
static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	long      pn       = get_Proj_proj(node);

	assert(is_sparc_SDiv(new_pred) || is_sparc_UDiv(new_pred));
	assert(pn_sparc_SDiv_res == pn_sparc_UDiv_res);
	assert(pn_sparc_SDiv_M == pn_sparc_UDiv_M);
	switch (pn) {
	case pn_Div_res:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_SDiv_res);
	case pn_Div_M:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_SDiv_M);
	default:
		break;
	}
	panic("Unsupported Proj from Div");
}

static ir_node *gen_Proj_Quot(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);
	long      pn       = get_Proj_proj(node);

	assert(is_sparc_fdiv(new_pred));
	switch (pn) {
	case pn_Quot_res:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_fdiv_res);
	case pn_Quot_M:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_fdiv_M);
	default:
		break;
	}
	panic("Unsupported Proj from Quot");
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	ir_node *barrier   = be_transform_node(get_Proj_pred(node));
	long     pn        = get_Proj_proj(node);

	switch ((pn_Start) pn) {
	case pn_Start_X_initial_exec:
		/* exchange ProjX with a jump */
		return new_bd_sparc_Ba(NULL, new_block);
	case pn_Start_M:
		return new_r_Proj(barrier, mode_M, 0);
	case pn_Start_T_args:
		return barrier;
	case pn_Start_P_frame_base:
		return be_prolog_get_reg_value(abihelper, fp_reg);
	case pn_Start_P_tls:
		return new_r_Bad(current_ir_graph);
	case pn_Start_max:
		break;
	}
	panic("Unexpected start proj: %ld\n", pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	long       pn          = get_Proj_proj(node);
	ir_node   *block       = get_nodes_block(node);
	ir_node   *new_block   = be_transform_node(block);
	ir_entity *entity      = get_irg_entity(current_ir_graph);
	ir_type   *method_type = get_entity_type(entity);
	ir_type   *param_type  = get_method_param_type(method_type, pn);
	const reg_or_stackslot_t *param;

	/* Proj->Proj->Start must be a method argument */
	assert(get_Proj_proj(get_Proj_pred(node)) == pn_Start_T_args);

	param = &cconv->parameters[pn];

	if (param->reg0 != NULL) {
		/* argument transmitted in register */
		ir_mode               *mode  = get_type_mode(param_type);
		const arch_register_t *reg   = param->reg0;
		ir_node               *value = be_prolog_get_reg_value(abihelper, reg);

		if (mode_is_float(mode)) {
			ir_node *value1 = NULL;

			if (param->reg1 != NULL) {
				value1 = be_prolog_get_reg_value(abihelper, param->reg1);
			} else if (param->entity != NULL) {
				ir_node *fp  = be_prolog_get_reg_value(abihelper, fp_reg);
				ir_node *mem = be_prolog_get_memory(abihelper);
				ir_node *ld  = new_bd_sparc_Ld_imm(NULL, new_block, fp, mem,
				                                   mode_gp, param->entity,
				                                   0, true);
				value1 = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);
			}

			/* convert integer value to float */
			value = bitcast_int_to_float(NULL, new_block, value, value1);
		}
		return value;
	} else {
		/* argument transmitted on stack */
		ir_node  *fp   = be_prolog_get_reg_value(abihelper, fp_reg);
		ir_node  *mem  = be_prolog_get_memory(abihelper);
		ir_mode  *mode = get_type_mode(param->type);
		ir_node  *load;
		ir_node  *value;

		if (mode_is_float(mode)) {
			load  = create_ldf(NULL, new_block, fp, mem, mode,
			                   param->entity, 0, true);
			value = new_r_Proj(load, mode_fp, pn_sparc_Ldf_res);
		} else {
			load  = new_bd_sparc_Ld_imm(NULL, new_block, fp, mem, mode,
			                            param->entity, 0, true);
			value = new_r_Proj(load, mode_gp, pn_sparc_Ld_res);
		}
		set_irn_pinned(load, op_pin_state_floats);

		return value;
	}
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	long     pn        = get_Proj_proj(node);
	ir_node *call      = get_Proj_pred(node);
	ir_node *new_call  = be_transform_node(call);

	switch ((pn_Call) pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, 0);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
	case pn_Call_P_value_res_base:
	case pn_Call_max:
		break;
	}
	panic("Unexpected Call proj %ld\n", pn);
}

/**
 * Finds number of output value of a mode_T node which is constrained to
 * a single specific register.
 */
static int find_out_for_reg(ir_node *node, const arch_register_t *reg)
{
	int n_outs = arch_irn_get_n_outs(node);
	int o;

	for (o = 0; o < n_outs; ++o) {
		const arch_register_req_t *req = arch_get_out_register_req(node, o);
		if (req == reg->single_req)
			return o;
	}
	return -1;
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	long                  pn            = get_Proj_proj(node);
	ir_node              *call          = get_Proj_pred(get_Proj_pred(node));
	ir_node              *new_call      = be_transform_node(call);
	ir_type              *function_type = get_Call_type(call);
	calling_convention_t *cconv
		= sparc_decide_calling_convention(function_type, true);
	const reg_or_stackslot_t *res = &cconv->results[pn];
	const arch_register_t    *reg = res->reg0;
	ir_mode                  *mode;
	int                       regn;

	assert(res->reg0 != NULL && res->reg1 == NULL);
	regn = find_out_for_reg(new_call, reg);
	if (regn < 0) {
		panic("Internal error in calling convention for return %+F", node);
	}
	mode = res->reg0->reg_class->mode;

	sparc_free_calling_convention(cconv);

	return new_r_Proj(new_call, mode, regn);
}

/**
 * Transform a Proj node.
 */
static ir_node *gen_Proj(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);

	switch (get_irn_opcode(pred)) {
	case iro_Store:
		return gen_Proj_Store(node);
	case iro_Load:
		return gen_Proj_Load(node);
	case iro_Call:
		return gen_Proj_Call(node);
	case iro_Cmp:
		return gen_Proj_Cmp(node);
	case iro_Cond:
		return be_duplicate_node(node);
	case iro_Div:
		return gen_Proj_Div(node);
	case iro_Quot:
		return gen_Proj_Quot(node);
	case iro_Start:
		return gen_Proj_Start(node);
	case iro_Proj: {
		ir_node *pred_pred = get_Proj_pred(pred);
		if (is_Call(pred_pred)) {
			return gen_Proj_Proj_Call(node);
		} else if (is_Start(pred_pred)) {
			return gen_Proj_Proj_Start(node);
		}
		/* FALLTHROUGH */
	}
	default:
		panic("code selection didn't expect Proj after %+F\n", pred);
	}
}

/**
 * transform a Jmp
 */
static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_sparc_Ba(dbgi, new_block);
}

/**
 * configure transformation callbacks
 */
static void sparc_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,          gen_Add);
	be_set_transform_function(op_And,          gen_And);
	be_set_transform_function(op_Call,         gen_Call);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_Div,          gen_Div);
	be_set_transform_function(op_Eor,          gen_Eor);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Minus,        gen_Minus);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_Mulh,         gen_Mulh);
	be_set_transform_function(op_Not,          gen_Not);
	be_set_transform_function(op_Or,           gen_Or);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Proj,         gen_Proj);
	be_set_transform_function(op_Quot,         gen_Quot);
	be_set_transform_function(op_Return,       gen_Return);
	be_set_transform_function(op_Sel,          gen_Sel);
	be_set_transform_function(op_Shl,          gen_Shl);
	be_set_transform_function(op_Shr,          gen_Shr);
	be_set_transform_function(op_Shrs,         gen_Shrs);
	be_set_transform_function(op_Start,        gen_Start);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_SymConst,     gen_SymConst);
	be_set_transform_function(op_Unknown,      gen_Unknown);

	be_set_transform_function(op_sparc_Save,   be_duplicate_node);
}

/* hack to avoid unused fp proj at start barrier */
static void assure_fp_keep(void)
{
	unsigned         n_users = 0;
	const ir_edge_t *edge;
	ir_node         *fp_proj = be_prolog_get_reg_value(abihelper, fp_reg);

	foreach_out_edge(fp_proj, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		if (is_End(succ) || is_Anchor(succ))
			continue;
		++n_users;
	}

	if (n_users == 0) {
		ir_node *block = get_nodes_block(fp_proj);
		ir_node *in[1] = { fp_proj };
		be_new_Keep(block, 1, in);
	}
}

/**
 * Transform a Firm graph into a SPARC graph.
 */
void sparc_transform_graph(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *frame_type;

	sparc_register_transformers();

	node_to_stack = pmap_create();

	mode_gp  = mode_Iu;
	mode_fp  = mode_F;
	mode_fp2 = mode_D;
	//mode_fp4 = ?

	abihelper = be_abihelper_prepare(irg);
	be_collect_stacknodes(abihelper);
	cconv = sparc_decide_calling_convention(get_entity_type(entity), false);
	create_stacklayout(irg);

	be_transform_graph(irg, NULL);
	assure_fp_keep();

	be_abihelper_finish(abihelper);
	sparc_free_calling_convention(cconv);

	frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined)
		default_layout_compound_type(frame_type);

	pmap_destroy(node_to_stack);
	node_to_stack = NULL;

	be_add_missing_keeps(irg);

	/* do code placement, to optimize the position of constants */
	place_code(irg);
}

void sparc_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.transform");
}
