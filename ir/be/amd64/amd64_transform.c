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
 * @brief   code selection (transform FIRM into amd64 FIRM)
 * @version $Id: amd64_transform.c 26673 2009-10-01 16:43:13Z matze $
 */
#include "config.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "iropt_t.h"
#include "error.h"
#include "debug.h"

#include "../benode.h"
#include "../betranshlp.h"
#include "bearch_amd64_t.h"

#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "amd64_new_nodes.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** holds the current code generator during transformation */
static amd64_code_gen_t *env_cg;

///* its enough to have those once */
//static ir_node *nomem, *noreg_GP;

/* Some support functions: */

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/**
 * Create a DAG constructing a given Const.
 *
 * @param irn  a Firm const
 */
static ir_node *create_const_graph(ir_node *irn, ir_node *block)
{
	tarval  *tv    = get_Const_tarval(irn);
	ir_mode *mode  = get_tarval_mode(tv);
	dbg_info *dbgi = get_irn_dbg_info(irn);
	unsigned value;

	if (mode_is_reference(mode)) {
		/* AMD64 is 64bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(mode_Iu));
		tv = tarval_convert_to(tv, mode_Iu);
	}

	value = get_tarval_long(tv);
	//d// printf ("TEST GENERATE %d\n", value);

	return new_bd_amd64_Immediate(dbgi, block, value);
}

/* Op transformers: */

/**
 * Transforms a Const node.
 *
 * @return The transformed AMD64 node.
 */
static ir_node *gen_Const(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode  *mode  = get_irn_mode(node);
	ir_node *res = create_const_graph(node, block);
	(void) mode;

	be_dep_on_frame(res);

	return res;
}

/**
 * Transforms a SymConst node.
 *
 * @return The transformed ARM node.
 */
static ir_node *gen_SymConst(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *entity = get_SymConst_entity(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_amd64_SymConst(dbgi, block, entity);
	be_dep_on_frame(new_node);
	return new_node;
}

/**
 * Transforms an Add node.
 *
 * @return The transformed AMD64 node.
 */
static ir_node *gen_Add(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	/* ir_mode  *mode  = get_irn_mode(node); */
	ir_node  *op1   = get_Add_left(node);
	ir_node  *op2   = get_Add_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	ir_node *res = new_bd_amd64_Add(dbgi, block, new_op1, new_op2);
	be_dep_on_frame (res);
	return res;
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_amd64_Jmp(dbgi, new_block);
}

static ir_node *gen_be_Call(ir_node *node)
{
	ir_node *res = be_duplicate_node(node);
	arch_irn_add_flags(res, arch_irn_flags_modify_flags);

	return res;
}

static ir_node *gen_Cmp(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op1      = get_Cmp_left(node);
	ir_node  *op2      = get_Cmp_right(node);
	ir_mode  *cmp_mode = get_irn_mode(op1);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_op1;
	ir_node  *new_op2;
	bool      is_unsigned;

	if (mode_is_float(cmp_mode)) {
		panic("Floating point not implemented yet (in gen_Cmp)!");
	}

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

	new_op1 = be_transform_node(op1);
//	new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
	new_op2 = be_transform_node(op2);
//	new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
	return new_bd_amd64_Cmp(dbgi, block, new_op1, new_op2, false,
	                        is_unsigned);
}

/**
 * Transforms a Cond.
 *
 * @return the created ARM Cond node
 */
static ir_node *gen_Cond(ir_node *node)
{
	ir_node  *selector = get_Cond_selector(node);
	ir_mode  *mode     = get_irn_mode(selector);
	ir_node  *block;
	ir_node  *flag_node;
	dbg_info *dbgi;

	if (mode != mode_b) {
		panic ("create_Switch not implemented yet!");
		// return gen_SwitchJmp(node);
	}
	assert(is_Proj(selector));

	block     = be_transform_node(get_nodes_block(node));
	dbgi      = get_irn_dbg_info(node);
	flag_node = be_transform_node(get_Proj_pred(selector));

	return new_bd_amd64_Jcc(dbgi, block, flag_node, get_Proj_proj(selector));
}

///**
// * Create an And that will zero out upper bits.
// *
// * @param dbgi     debug info
// * @param block    the basic block
// * @param op       the original node
// * param src_bits  number of lower bits that will remain
// */
//static ir_node *gen_zero_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
//                                   int src_bits)
//{
//	if (src_bits == 8) {
//		return new_bd_arm_And_imm(dbgi, block, op, 0xFF, 0);
//	} else if (src_bits == 16) {
//		ir_node *lshift = new_bd_arm_Mov_reg_shift_imm(dbgi, block, op, ARM_SHF_LSL_IMM, 16);
//		ir_node *rshift = new_bd_arm_Mov_reg_shift_imm(dbgi, block, lshift, ARM_SHF_LSR_IMM, 16);
//		return rshift;
//	} else {
//		panic("zero extension only supported for 8 and 16 bits");
//	}
//}
//
///**
// * Generate code for a sign extension.
// */
//static ir_node *gen_sign_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
//                                   int src_bits)
//{
//	int shift_width = 32 - src_bits;
//	ir_node *lshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, op, ARM_SHF_LSL_IMM, shift_width);
//	ir_node *rshift_node = new_bd_arm_Mov_reg_shift_imm(dbgi, block, lshift_node, ARM_SHF_ASR_IMM, shift_width);
//	return rshift_node;
//}
//
//static ir_node *gen_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
//                              ir_mode *orig_mode)
//{
//	int bits = get_mode_size_bits(orig_mode);
//	if (bits == 32)
//		return op;
//
//	if (mode_is_signed(orig_mode)) {
//		return gen_sign_extension(dbgi, block, op, bits);
//	} else {
//		return gen_zero_extension(dbgi, block, op, bits);
//	}
//}
//
///**
// * returns true if it is assured, that the upper bits of a node are "clean"
// * which means for a 16 or 8 bit value, that the upper bits in the register
// * are 0 for unsigned and a copy of the last significant bit for signed
// * numbers.
// */
//static bool upper_bits_clean(ir_node *transformed_node, ir_mode *mode)
//{
//	(void) transformed_node;
//	(void) mode;
//	/* TODO */
//	return false;
//}

/**
 * Change some phi modes
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
		/* all integer operations are on 32bit registers now */
		mode = mode_Iu;
		req  = amd64_reg_classes[CLASS_amd64_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	phi = new_ir_node(dbgi, irg, block, op_Phi, mode, get_irn_arity(node),
	                  get_irn_in(node) + 1);
	copy_node_attr(irg, node, phi);
	be_duplicate_deps(node, phi);

	arch_set_out_register_req(phi, 0, req);

	be_enqueue_preds(node);

	return phi;
}



/**
 * Transforms a Conv node.
 *
 * @return The created ia32 Conv node
 */
static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);

	if (src_mode == dst_mode)
		return new_op;

	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		panic("float not supported yet");
	} else { /* complete in gp registers */
		int src_bits = get_mode_size_bits(src_mode);
		int dst_bits = get_mode_size_bits(dst_mode);
		int min_bits;
		ir_mode *min_mode;

		if (src_bits == dst_bits) {
			/* kill unneccessary conv */
			return new_op;
		}

		if (src_bits < dst_bits) {
			min_bits = src_bits;
			min_mode = src_mode;
		} else {
			min_bits = dst_bits;
			min_mode = dst_mode;
		}

		return new_bd_amd64_Conv(dbgi, block, new_op, min_mode);

		//if (upper_bits_clean(new_op, min_mode)) {
		//	return new_op;
		//}

		//if (mode_is_signed(min_mode)) {
		//	return gen_sign_extension(dbg, block, new_op, min_bits);
		//} else {
		//	return gen_zero_extension(dbg, block, new_op, min_bits);
		//}
	}
}

/* Boilerplate code for transformation: */

static void amd64_pretransform_node(void)
{
	amd64_code_gen_t *cg = env_cg;
	(void) cg;

//	nomem = get_irg_no_mem(current_ir_graph);
}

static void set_transformer(ir_op *op, be_transform_func amd64_transform_func)
{
	op->ops.generic = (op_func)amd64_transform_func;
}

static void amd64_register_transformers(void)
{
	clear_irp_opcodes_generic_func();

	set_transformer(op_Const,        gen_Const);
	set_transformer(op_SymConst,     gen_SymConst);
	set_transformer(op_Add,          gen_Add);
	set_transformer(op_be_Call,      gen_be_Call);
	set_transformer(op_Conv,         gen_Conv);
	set_transformer(op_Jmp,          gen_Jmp);
	set_transformer(op_Cmp,          gen_Cmp);
	set_transformer(op_Cond,         gen_Cond);
	set_transformer(op_Phi,          gen_Phi);
}


void amd64_transform_graph(amd64_code_gen_t *cg)
{
	amd64_register_transformers();
	env_cg = cg;
	be_transform_graph(cg->irg, amd64_pretransform_node);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
