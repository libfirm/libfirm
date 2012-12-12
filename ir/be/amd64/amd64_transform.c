/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into amd64 FIRM)
 */
#include "config.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "ircons.h"
#include "iropt_t.h"
#include "error.h"
#include "debug.h"

#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "bearch_amd64_t.h"

#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "amd64_new_nodes.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

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
	ir_tarval *tv   = get_Const_tarval(irn);
	ir_mode   *mode = get_tarval_mode(tv);
	dbg_info  *dbgi = get_irn_dbg_info(irn);
	unsigned   value;

	if (mode_is_reference(mode)) {
		/* AMD64 is 64bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(mode_Lu));
		tv = tarval_convert_to(tv, mode_Lu);
	}

	value = get_tarval_long(tv);

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
	return new_node;
}

/**
 * Transforms an Add node.
 *
 * @return The transformed AMD64 node.
 */
static ir_node *gen_Add(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *op1   = get_Add_left(node);
	ir_node  *op2   = get_Add_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	ir_node *res = new_bd_amd64_Add(dbgi, block, new_op1, new_op2);
	return res;
}

/**
 * Transforms an Sub node.
 *
 * @return The transformed AMD64 node.
 */
static ir_node *gen_Sub(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *op1   = get_Sub_left(node);
	ir_node  *op2   = get_Sub_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	ir_node *res = new_bd_amd64_Sub(dbgi, block, new_op1, new_op2);
	return res;
}

static ir_node *gen_Mul(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *op1   = get_Mul_left(node);
	ir_node  *op2   = get_Mul_right(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	ir_node *res = new_bd_amd64_Mul(dbgi, block, new_op1, new_op2);
	return res;
}

static ir_node *gen_Minus(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *val      = be_transform_node(get_Minus_op(node));
	dbg_info *dbgi     = get_irn_dbg_info(node);

	return new_bd_amd64_Neg(dbgi, block, val);
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
	arch_add_irn_flags(res, arch_irn_flags_modify_flags);

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
		panic("Floating point not implemented yet!");
	}

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

	new_op1 = be_transform_node(op1);
	/* new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode); */
	new_op2 = be_transform_node(op2);
	/* new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode); */
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
	ir_node    *const block     = be_transform_node(get_nodes_block(node));
	dbg_info   *const dbgi      = get_irn_dbg_info(node);
	ir_node    *const selector  = get_Cond_selector(node);
	ir_node    *const flag_node = be_transform_node(selector);
	ir_relation const relation  = get_Cmp_relation(selector);
	return new_bd_amd64_Jcc(dbgi, block, flag_node, relation);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (mode_needs_gp_reg(mode)) {
		/* all integer operations are on 64bit registers now */
		mode = mode_Lu;
		req  = amd64_reg_classes[CLASS_amd64_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	return be_transform_phi(node, req);
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
		ir_mode *min_mode;

		if (src_bits == dst_bits) {
			/* kill unnecessary conv */
			return new_op;
		}

		if (src_bits < dst_bits) {
			min_mode = src_mode;
		} else {
			min_mode = dst_mode;
		}


		return new_bd_amd64_Conv(dbgi, block, new_op, min_mode);
	}
}

/**
 * Transforms a Store.
 *
 * @return the created AMD64 Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Store_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Store_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *val      = get_Store_value(node);
	ir_node  *new_val  = be_transform_node(val);
	ir_mode  *mode     = get_irn_mode(val);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node *new_store = NULL;

	if (mode_is_float(mode)) {
		panic("Float not supported yet");
	} else {
		assert(mode_is_data(mode) && "unsupported mode for Store");
		new_store = new_bd_amd64_Store(dbgi, block, new_ptr, new_val, new_mem, 0);
	}
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
}

/**
 * Transforms a Load.
 *
 * @return the created AMD64 Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_mode  *mode     = get_Load_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_load = NULL;

	if (mode_is_float(mode)) {
		panic("Float not supported yet");
	} else {
		assert(mode_is_data(mode) && "unsupported mode for Load");
		new_load = new_bd_amd64_Load(dbgi, block, new_ptr, new_mem, 0);
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

/**
 * Transform a Proj from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_amd64_irn_opcode(new_load)) {
		case iro_amd64_Load:
			/* handle all gp loads equal: they have the same proj numbers. */
			if (proj == pn_Load_res) {
				return new_rd_Proj(dbgi, new_load, mode_Lu, pn_amd64_Load_res);
			} else if (proj == pn_Load_M) {
				return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_Load_M);
			}
		break;
		default:
			panic("Unsupported Proj from Load");
	}

    return be_duplicate_node(node);
}

/**
 * Transform a Proj node.
 */
static ir_node *gen_Proj(ir_node *node)
{
	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node  *pred = get_Proj_pred(node);
	long     proj  = get_Proj_proj(node);

    (void) dbgi;

	if (is_Store(pred)) {
		if (proj == pn_Store_M) {
			return be_transform_node(pred);
		} else {
			panic("Unsupported Proj from Store");
		}
	} else if (is_Load(pred)) {
		return gen_Proj_Load(node);
	} else if (is_Start(pred)) {
	}

    return be_duplicate_node(node);
}

/**
 * Transforms a FrameAddr into an AMD64 Add.
 */
static ir_node *gen_be_FrameAddr(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *ent    = be_get_frame_entity(node);
	ir_node   *fp     = be_get_FrameAddr_frame(node);
	ir_node   *new_fp = be_transform_node(fp);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_amd64_FrameAddr(dbgi, block, new_fp, ent);
	return new_node;
}

/* Boilerplate code for transformation: */

static void amd64_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_SymConst,     gen_SymConst);
	be_set_transform_function(op_Add,          gen_Add);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_be_Call,      gen_be_Call);
	be_set_transform_function(op_be_FrameAddr, gen_be_FrameAddr);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Proj,         gen_Proj);
	be_set_transform_function(op_Minus,        gen_Minus);
}

void amd64_transform_graph(ir_graph *irg)
{
	amd64_register_transformers();
	be_transform_graph(irg, NULL);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
