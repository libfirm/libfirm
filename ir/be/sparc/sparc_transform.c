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
 * @brief   code selection (transform FIRM into SPARC FIRM)
 * @version $Id: TEMPLATE_transform.c 26673 2009-10-01 16:43:13Z matze $
 */

#include "config.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "irprintf.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"

#include "../benode.h"
#include "../beirg.h"
#include "../beutil.h"
#include "../betranshlp.h"
#include "bearch_sparc_t.h"

#include "sparc_nodes_attr.h"
#include "sparc_transform.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_new_nodes.h"

#include "gen_sparc_regalloc_if.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static sparc_code_gen_t *env_cg;

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/**
 * Creates a possible DAG for a constant.
 */
static ir_node *create_const_graph_value(dbg_info *dbgi, ir_node *block,
				long value)
{
	ir_node *result;

	// TODO: find a better solution for this
	if (value < -4096 || value > 4096) {
		panic("FIXME: immediate value exceeds max. size of simm13 (13 bits signed)");
	}

	result = new_bd_sparc_Mov_imm(dbgi, block, (int) value);
	return result;
}


/**
 * Create a DAG constructing a given Const.
 *
 * @param irn  a Firm const
 */
static ir_node *create_const_graph(ir_node *irn, ir_node *block)
{
	tarval  *tv = get_Const_tarval(irn);
	ir_mode *mode = get_tarval_mode(tv);
	long value;

	if (mode_is_reference(mode)) {
		/* SPARC V8 is 32bit, so we can safely convert a reference tarval into Iu */
		assert(get_mode_size_bits(mode) == get_mode_size_bits(mode_Iu));
		tv = tarval_convert_to(tv, mode_Iu);
	}
	value = get_tarval_long(tv);
	return create_const_graph_value(get_irn_dbg_info(irn), block, value);
}


typedef enum {
	MATCH_NONE         = 0,
	MATCH_COMMUTATIVE  = 1 << 0,
	MATCH_SIZE_NEUTRAL = 1 << 1,
} match_flags_t;

typedef ir_node* (*new_binop_reg_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2);
typedef ir_node* (*new_binop_imm_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, int simm13);

/**
 * checks wether a node's value can be encoded as a immediate
 * TODO: pass a result pointer to fetch the encoded immediate
 *
 */
static bool is_imm_encodeable(const ir_node *node)
{
	long val;

	if (!is_Const(node))
		return false;

	val = get_tarval_long(get_Const_tarval(node));

	return !(val < -4096 || val > 4096);
}

/**
 * helper function for binop operations
 *
 * @param new_binop_reg_func register generation function ptr
 * @param new_binop_imm_func immediate generation function ptr
 */
static ir_node *gen_helper_binop(ir_node *node, match_flags_t flags,
				new_binop_reg_func new_reg, new_binop_imm_func new_imm)
{
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1;
	ir_node  *op2     = get_binop_right(node);
	ir_node  *new_op2;
	dbg_info *dbgi    = get_irn_dbg_info(node);

/*
    if (flags & MATCH_SIZE_NEUTRAL) {
        op1 = arm_skip_downconv(op1);
        op2 = arm_skip_downconv(op2);
    } else {
        assert(get_mode_size_bits(get_irn_mode(node)) == 32);
    }
*/
	if (is_imm_encodeable(op2)) {
		ir_node *new_op1 = be_transform_node(op1);
		return new_imm(dbgi, block, new_op1, get_tarval_long(get_Const_tarval(node)));
	}

	new_op2 = be_transform_node(op2);

	if ((flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		return new_imm(dbgi, block, new_op2, get_tarval_long(get_Const_tarval(op1)) );
	}

	new_op1 = be_transform_node(op1);

	return new_reg(dbgi, block, new_op1, new_op2);
}

/**
 * Creates an sparc Add.
 *
 * @param node   FIRM node
 * @return the created sparc Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_mode  *mode    = get_irn_mode(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Add_left(node);
	ir_node  *op2     = get_Add_right(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	(void) new_op1;
	(void) new_op2;
	(void) block;
	(void) dbgi;

	if (mode_is_float(mode))
		panic("FP not implemented yet");

	return gen_helper_binop(node, MATCH_COMMUTATIVE | MATCH_SIZE_NEUTRAL, new_bd_sparc_Add_reg, new_bd_sparc_Add_imm);
}


/**
 * Creates an sparc Sub.
 *
 * @param node       FIRM node
 * @return the created sparc Sub node
 */
static ir_node *gen_Sub(ir_node *node)
{
	ir_mode  *mode    = get_irn_mode(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *op1     = get_Add_left(node);
	ir_node  *op2     = get_Add_right(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *new_op2 = be_transform_node(op2);

	(void) new_op1;
    (void) new_op2;
    (void) block;
    (void) dbgi;

	if (mode_is_float(mode))
		panic("FP not implemented yet");

	return gen_helper_binop(node, MATCH_SIZE_NEUTRAL, new_bd_sparc_Sub_reg, new_bd_sparc_Sub_imm);
}


/**
 * Transforms a Load.
 *
 * @param node    the ir Load node
 * @return the created sparc Load node
 */
static ir_node *gen_Load(ir_node *node)
{

	if (mode_is_float(get_irn_mode(node)))
		panic("SPARC: no fp implementation yet");

	ir_mode  *mode     = get_Load_mode(node);
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_load = NULL;

	new_load = new_bd_sparc_Load(dbgi, block, new_ptr, new_mem, mode, NULL, 0, 0, false);
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
	if (mode_is_float(get_irn_mode(node)))
		panic("SPARC: no fp implmentation yet");

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

	new_store = new_bd_sparc_Store(dbgi, block, new_ptr, new_val, new_mem, mode, NULL, 0, 0, false);

	return new_store;
}



/****** TRANSFORM GENERAL BACKEND NODES ********/

/**
 * Transforms a Const node.
 *
 * @param node    the ir Store node
 * @return The transformed sparc node.
 */
static ir_node *gen_Const(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode *mode = get_irn_mode(node);
	dbg_info *dbg = get_irn_dbg_info(node);

	(void) dbg;

	if (mode_is_float(mode)) {
		panic("FP not supported yet");
	}
	return create_const_graph(node, block);
}

/**
 * AddSP
 * @param node the ir AddSP node
 * @return transformed sparc SAVE node
 */
static ir_node *gen_be_AddSP(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *sz     = get_irn_n(node, be_pos_AddSP_size);
	ir_node  *new_sz = be_transform_node(sz);
	ir_node  *sp     = get_irn_n(node, be_pos_AddSP_old_sp);
	ir_node  *new_sp = be_transform_node(sp);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *nomem  = new_NoMem();
	ir_node  *new_op;

	/* SPARC stack grows in reverse direction */
	new_op = new_bd_sparc_AddSP(dbgi, block, new_sp, new_sz, nomem);

	return new_op;
}


/**
 * SubSP
 * @param node the ir SubSP node
 * @return transformed sparc SAVE node
 */
static ir_node *gen_be_SubSP(ir_node *node)
{
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *sz     = get_irn_n(node, be_pos_SubSP_size);
	ir_node  *new_sz = be_transform_node(sz);
	ir_node  *sp     = get_irn_n(node, be_pos_SubSP_old_sp);
	ir_node  *new_sp = be_transform_node(sp);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *nomem  = new_NoMem();
	ir_node  *new_op;

	/* SPARC stack grows in reverse direction */
	new_op = new_bd_sparc_SubSP(dbgi, block, new_sp, new_sz, nomem);
	return new_op;
}

/**
 * transform FrameAddr
 */
static ir_node *gen_be_FrameAddr(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *ent    = be_get_frame_entity(node);
	ir_node   *fp     = be_get_FrameAddr_frame(node);
	ir_node   *new_fp = be_transform_node(fp);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;
	new_node = new_bd_sparc_FrameAddr(dbgi, block, new_fp, ent);
	return new_node;
}

/**
 * Transform a be_Copy.
 */
static ir_node *gen_be_Copy(ir_node *node) {
	ir_node *result = be_duplicate_node(node);
	ir_mode *mode   = get_irn_mode(result);

	if (mode_needs_gp_reg(mode)) {
		set_irn_mode(node, mode_Iu);
	}

	return result;
}

/**
 * Transform a Call
 */
static ir_node *gen_be_Call(ir_node *node)
{
	ir_node *res = be_duplicate_node(node);
	arch_irn_add_flags(res, arch_irn_flags_modify_flags);
	return res;
}

static ir_node *gen_SwitchJmp(ir_node *node)
{
	panic("TODO: not implemented yet");
}

/**
 * Transform Cond nodes
 */
static ir_node *gen_Cond(ir_node *node)
{
	ir_node  *selector = get_Cond_selector(node);
	ir_mode  *mode     = get_irn_mode(selector);
	ir_node  *block;
	ir_node  *flag_node;
	dbg_info *dbgi;

	// switch/case jumps
	if (mode != mode_b) {
		//return gen_SwitchJmp(node);
		panic("TODO: switchJmp not implemented yet");
		return node;
	}

	// regular if/else jumps
	assert(is_Proj(selector));

	block     = be_transform_node(get_nodes_block(node));
	dbgi      = get_irn_dbg_info(node);
	flag_node = be_transform_node(get_Proj_pred(selector));
	return new_bd_sparc_Branch(dbgi, block, flag_node, get_Proj_proj(selector));
}

/**
 * transform Cmp
 */
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
		panic("FloatCmp not implemented");
	}

	if (get_mode_size_bits(cmp_mode) != 32) {
		panic("CmpMode != 32bit not supported yet");
	}

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

	/* compare with 0 can be done with Tst */
	if (is_Const(op2) && tarval_is_null(get_Const_tarval(op2))) {
		new_op1 = be_transform_node(op1);
		//new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
		//panic("TODO: implement Tst instruction");
		return new_bd_sparc_Tst(dbgi, block, new_op1, false,
		                          is_unsigned);
	}

	if (is_Const(op1) && tarval_is_null(get_Const_tarval(op1))) {
		new_op2 = be_transform_node(op2);
		//new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
		//panic("TODO: implement Tst instruction");
		return new_bd_sparc_Tst(dbgi, block, new_op2, true,
		                          is_unsigned);
	}

	/* integer compare */
	new_op1 = be_transform_node(op1);
	//new_op1 = gen_extension(dbgi, block, new_op1, cmp_mode);
	new_op2 = be_transform_node(op2);
	//new_op2 = gen_extension(dbgi, block, new_op2, cmp_mode);
	return new_bd_sparc_Cmp_reg(dbgi, block, new_op1, new_op2, false, is_unsigned);
}

/**
 * Transforms a SymConst node.
 */
static ir_node *gen_SymConst(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *entity = get_SymConst_entity(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_sparc_SymConst(dbgi, block, entity);
	be_dep_on_frame(new_node);
	return new_node;
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
		mode = mode_Iu;
		req  = sparc_reg_classes[CLASS_sparc_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	phi = new_ir_node(dbgi, irg, block, op_Phi, mode, get_irn_arity(node),
	get_irn_in(node) + 1);
	copy_node_attr(node, phi);
	be_duplicate_deps(node, phi);
	arch_set_out_register_req(phi, 0, req);
	be_enqueue_preds(node);
	return phi;
}

/**
 * the BAD transformer.
 */
static ir_node *bad_transform(ir_node *irn)
{
	panic("SPARC backend: Not implemented: %+F", irn);
}

/**
 * Set a node emitter. Make it a bit more type safe.
 */
static void set_transformer(ir_op *op, be_transform_func sparc_transform_func)
{
	op->ops.generic = (op_func)sparc_transform_func;
}

/**
 * configure transformation callbacks
 */
void sparc_register_transformers(void)
{
	clear_irp_opcodes_generic_func();
	set_transformer(op_Add,				gen_Add);
	set_transformer(op_Store,			gen_Store);
	set_transformer(op_Const,			gen_Const);
	set_transformer(op_Load,			gen_Load);
	set_transformer(op_Sub,				gen_Sub);

	set_transformer(op_be_AddSP,     gen_be_AddSP);
	set_transformer(op_be_SubSP,     gen_be_SubSP);
	set_transformer(op_be_Copy,      gen_be_Copy);
	set_transformer(op_be_Call,      gen_be_Call);
	set_transformer(op_be_FrameAddr, gen_be_FrameAddr);

	set_transformer(op_Cond,         gen_Cond);
	set_transformer(op_Cmp,          gen_Cmp);

	set_transformer(op_SymConst,     gen_SymConst);

	set_transformer(op_Phi,          gen_Phi);

	/* node list */
	/*
	set_transformer(op_Abs,          gen_Abs);
	set_transformer(op_Add,          gen_Add);
	set_transformer(op_And,          gen_And);
	set_transformer(op_Const,        gen_Const);
	set_transformer(op_Conv,         gen_Conv);
	set_transformer(op_CopyB,        gen_CopyB);
	set_transformer(op_Eor,          gen_Eor);
	set_transformer(op_Jmp,          gen_Jmp);
	set_transformer(op_Load,         gen_Load);
	set_transformer(op_Minus,        gen_Minus);
	set_transformer(op_Mul,          gen_Mul);
	set_transformer(op_Not,          gen_Not);
	set_transformer(op_Or,           gen_Or);
	set_transformer(op_Proj,         gen_Proj);
	set_transformer(op_Quot,         gen_Quot);
	set_transformer(op_Rotl,         gen_Rotl);
	set_transformer(op_Shl,          gen_Shl);
	set_transformer(op_Shr,          gen_Shr);
	set_transformer(op_Shrs,         gen_Shrs);
	set_transformer(op_Store,        gen_Store);
	set_transformer(op_Sub,          gen_Sub);
	set_transformer(op_Unknown,      gen_Unknown);
	*/

	set_transformer(op_ASM,       bad_transform);
	set_transformer(op_Builtin,   bad_transform);
	set_transformer(op_CallBegin, bad_transform);
	set_transformer(op_Cast,      bad_transform);
	set_transformer(op_Confirm,   bad_transform);
	set_transformer(op_DivMod,    bad_transform);
	set_transformer(op_EndExcept, bad_transform);
	set_transformer(op_EndReg,    bad_transform);
	set_transformer(op_Filter,    bad_transform);
	set_transformer(op_Free,      bad_transform);
	set_transformer(op_Id,        bad_transform);
	set_transformer(op_InstOf,    bad_transform);
	set_transformer(op_Mulh,      bad_transform);
	set_transformer(op_Mux,       bad_transform);
	set_transformer(op_Raise,     bad_transform);
	set_transformer(op_Sel,       bad_transform);
	set_transformer(op_Tuple,     bad_transform);
}


/**
  * Pre-transform all unknown nodes.
  */
static void sparc_pretransform_node(void)
{
	sparc_code_gen_t *cg = env_cg;
	(void) cg;
	//cg->unknown_gp  = be_pre_transform_node(cg->unknown_gp);
	//cg->unknown_fpa = be_pre_transform_node(cg->unknown_fpa);
}

/**
 * Transform a Firm graph into a SPARC graph.
 */
void sparc_transform_graph(sparc_code_gen_t *cg)
{
	sparc_register_transformers();
	env_cg = cg;
	be_transform_graph(cg->birg, sparc_pretransform_node);
}

void sparc_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.transform");
}
