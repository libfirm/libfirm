/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   The codegenrator (transform FIRM into mips FIRM
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "irprintf.h"
#include "irop.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"

#include "../benode_t.h"
#include "../beabi.h"
#include "../besched.h"
#include "../besched_t.h"
#include "../beirg_t.h"
#include "../betranshlp.h"
#include "bearch_mips_t.h"

#include "mips_nodes_attr.h"
#include "archop.h"
#include "mips_transform.h"
#include "mips_new_nodes.h"
#include "mips_map_regs.h"
#include "mips_util.h"
#include "mips_emitter.h"

#include "gen_mips_regalloc_if.h"

/** hold the current code generator during transformation */
static mips_code_gen_t *env_cg = NULL;

/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

typedef ir_node *construct_binop_func(dbg_info *db, ir_graph *irg,
	ir_node *block, ir_node *left, ir_node *right);

static INLINE int mode_needs_gp_reg(ir_mode *mode) {
	return mode_is_int(mode) || mode_is_character(mode)
		|| mode_is_reference(mode);
}

ir_node *mips_create_Immediate(long val)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *block = get_irg_start_block(irg);
	const arch_register_t **slots;
	ir_node  *res;

	assert(val >=  -32768 && val <= 32767);
	res      = new_rd_mips_Immediate(NULL, irg, block, MIPS_IMM_CONST, NULL,
	                                 val);
	slots    = get_mips_slots(res);
	slots[0] = &mips_gp_regs[REG_GP_NOREG];

	return res;
}

ir_node* mips_create_zero(void)
{
	ir_graph *irg   = current_ir_graph;
	ir_node  *block = get_irg_start_block(irg);
	ir_node  *zero  = new_rd_mips_zero(NULL, irg, block);
	const arch_register_t **slots = get_mips_slots(zero);

	slots[0] = &mips_gp_regs[REG_ZERO];

	return zero;
}

static ir_node *try_create_Immediate(ir_node *node)
{
	tarval   *tv;
	long      val;
	ir_mode  *mode;

	if(!is_Const(node))
		return NULL;

	mode = get_irn_mode(node);
	if(!mode_needs_gp_reg(mode))
		return NULL;

	tv = get_Const_tarval(node);
	if(tarval_is_long(tv)) {
		val = get_tarval_long(tv);
	} else if(tarval_is_null(tv)) {
		val = 0;
	} else {
		ir_fprintf(stderr, "Optimisation Warning: tarval %+F is not a long?\n",
		           node);
		return NULL;
	}

	if(val < -32768 || val > 32767)
		return NULL;

	return mips_create_Immediate(val);
}

static void create_binop_operands(ir_node **new_left, ir_node **new_right,
                                  ir_node *left, ir_node *right,
                                  int is_commutative)
{
	*new_right = try_create_Immediate(right);
	if(*new_right != NULL) {
		*new_left = be_transform_node(left);
		return;
	}
	if(is_commutative) {
		*new_right = try_create_Immediate(left);
		if(*new_right != NULL) {
			*new_left = be_transform_node(right);
			return;
		}
	}

	*new_left  = be_transform_node(left);
	*new_right = be_transform_node(right);
}

static ir_node *gen_binop(ir_node *node, ir_node *left, ir_node *right,
                          construct_binop_func func, int supports_immediate)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *res;
	ir_node  *new_left, *new_right;

	assert(mode_needs_gp_reg(get_irn_mode(node)));

	if(supports_immediate) {
		int is_commutative = is_op_commutative(get_irn_op(node));
		create_binop_operands(&new_left, &new_right, left, right,
		                      is_commutative);
	} else {
		new_left  = be_transform_node(left);
		new_right = be_transform_node(right);
	}

	res = func(dbgi, irg, block, new_left, new_right);

	return res;
}

static ir_node *gen_Add(ir_node *node)
{
	/* TODO: match add(symconst, const) */
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_addu, 1);
}

static ir_node *gen_Sub(ir_node *node)
{
	return gen_binop(node, get_Sub_left(node), get_Sub_right(node),
	                 new_rd_mips_addu, 0);
}

static ir_node *gen_And(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_and, 1);
}

static ir_node *gen_Or(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_or, 1);
}

static ir_node *gen_Eor(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_xor, 1);
}

static ir_node *gen_Shl(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_sll, 1);
}

static ir_node *gen_Shr(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_srl, 1);
}

static ir_node *gen_Shrs(ir_node *node)
{
	return gen_binop(node, get_Add_left(node), get_Add_right(node),
	                 new_rd_mips_sra, 1);
}

static ir_node *gen_Not(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_node  *op    = get_Not_op(node);
	ir_node  *new_op;
	ir_node  *res;
	ir_node  *one;

	/* we can transform not->or to nor */
	if(is_Or(op)) {
		return gen_binop(op, get_Or_left(op), get_Or_right(op),
		                 new_rd_mips_nor, 1);
	}

	/* construct (op < 1) */
	one    = mips_create_Immediate(1);
	new_op = be_transform_node(op);
	res    = new_rd_mips_sltu(dbgi, irg, block, new_op, one);

	return res;
}

static ir_node *gen_Minus(ir_node *node)
{
	ir_graph *irg    = current_ir_graph;
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *op     = get_Minus_op(node);
	ir_node  *new_op = be_transform_node(op);
	ir_node  *res;
	ir_node  *zero;

	/* construct (0 - op) */
	zero = mips_create_zero();
	res  = new_rd_mips_subu(dbgi, irg, block, zero, new_op);

	return res;
}

static ir_node *gen_Abs(ir_node *node)
{
	ir_graph *irg    = current_ir_graph;
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *block  = be_transform_node(get_nodes_block(node));
	ir_node  *op     = get_Abs_op(node);
	ir_node  *new_op = be_transform_node(op);
	ir_node  *sra_const, *sra, *add, *xor;

	/* TODO: support other bit sizes... */
	assert(get_mode_size_bits(get_irn_mode(node)) == 32);
	sra_const = mips_create_Immediate(31);
	sra       = new_rd_mips_sra(dbgi, irg, block, new_op, sra_const);
	add       = new_rd_mips_addu(dbgi, irg, block, new_op, sra);
	xor       = new_rd_mips_xor(dbgi, irg, block, sra, add);

	return xor;
}

static ir_node* gen_Const(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	tarval   *tv    = get_Const_tarval(node);
	ir_node  *upper_node;
	ir_node  *lower_node;
	ir_node  *or_const;
	unsigned long val, lower, upper;

	if(tarval_is_long(tv)) {
		val = get_tarval_long(tv);
	} else if(tarval_is_null(tv)) {
		val = 0;
	} else {
		panic("Can't get value of tarval %+F\n", node);
	}

	val = get_tarval_long(tv);

	lower = val & 0xffff;
	upper = (val >> 16) & 0xffff;
	if(upper == 0) {
		upper_node = mips_create_zero();
	} else {
		upper_node = new_rd_mips_lui(dbgi, irg, block, MIPS_IMM_CONST, NULL,
		                             upper);
	}

	if(lower == 0)
		return upper_node;

	or_const   = mips_create_Immediate(lower);
	lower_node = new_rd_mips_or(dbgi, irg, block, upper_node, or_const);

	return lower_node;
}

static ir_node* gen_SymConst(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_entity *entity;
	ir_node *lui, *or_const, *or;

	if(get_SymConst_kind(node) != symconst_addr_ent) {
		panic("Only address entity symconsts supported in mips backend");
	}

	entity = get_SymConst_entity(node);

	lui            = new_rd_mips_lui(dbgi, irg, block, MIPS_IMM_SYMCONST_HI,
	                                 entity, 0);
	or_const       = new_rd_mips_Immediate(dbgi, irg, block,
	                                       MIPS_IMM_SYMCONST_LO, entity, 0);
	or             = new_rd_mips_or(dbgi, irg, block, lui, or_const);

	return or;
}

typedef ir_node* (*gen_load_func) (dbg_info *dbg, ir_graph *irg,
                                   ir_node *block, ir_node *ptr, ir_node *mem,
                                   ir_entity *entity, long offset);

/**
 * Generates a mips node for a firm Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *mem     = get_Load_mem(node);
	ir_node  *new_mem = be_transform_node(mem);
	ir_node  *ptr     = get_Load_ptr(node);
	ir_node  *new_ptr = be_transform_node(ptr);
	ir_mode  *mode    = get_Load_mode(node);
	int       sign    = get_mode_sign(mode);
	ir_node  *res;
	gen_load_func func;

	ASSERT_NO_FLOAT(mode);
	assert(mode_needs_gp_reg(mode));

	/* TODO: make use of offset in ptrs */

	switch(get_mode_size_bits(mode)) {
	case 32:
		func = new_rd_mips_lw;
		break;
	case 16:
		func = sign ? new_rd_mips_lh : new_rd_mips_lhu;
		break;
	case 8:
		func = sign ? new_rd_mips_lb : new_rd_mips_lbu;
		break;
	default:
		panic("mips backend only support 32, 16, 8 bit loads");
	}

	res = func(dbgi, irg, block, new_ptr, new_mem, NULL, 0);
	set_irn_pinned(res, get_irn_pinned(node));

	return res;
}

typedef ir_node* (*gen_store_func) (dbg_info *dbg, ir_graph *irg,
                                    ir_node *block, ir_node *ptr, ir_node *val,
                                    ir_node *mem, ir_entity *ent, long offset);

/**
 * Generates a mips node for a firm Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_graph    *irg     = current_ir_graph;
	dbg_info    *dbgi    = get_irn_dbg_info(node);
	ir_node     *block   = be_transform_node(get_nodes_block(node));
	ir_node     *mem     = get_Store_mem(node);
	ir_node     *new_mem = be_transform_node(mem);
	ir_node     *ptr     = get_Store_ptr(node);
	ir_node     *new_ptr = be_transform_node(ptr);
	ir_node     *val     = get_Store_value(node);
	ir_node     *new_val = be_transform_node(val);
	ir_mode     *mode    = get_irn_mode(val);
	gen_store_func func;
	ir_node     *res;

	assert(mode_needs_gp_reg(mode));

	switch(get_mode_size_bits(mode)) {
	case 32:
		func = new_rd_mips_sw;
		break;
	case 16:
		func = new_rd_mips_sh;
		break;
	case 8:
		func = new_rd_mips_sb;
		break;
	default:
		panic("store only supported for 32, 16, 8 bit values in mips backend");
	}

	res = func(dbgi, irg, block, new_ptr, new_val, new_mem, NULL, 0);
	set_irn_pinned(res, get_irn_pinned(node));

	return res;
}

static ir_node *gen_Proj_DivMod(ir_node *node)
{
	ir_graph *irg     = current_ir_graph;
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *divmod  = get_Proj_pred(node);
	ir_node  *new_div = be_transform_node(divmod);
	long      pn      = get_Proj_proj(node);
	ir_node  *proj;

	assert(is_mips_div(new_div) || is_mips_divu(new_div));

	switch(get_irn_opcode(divmod)) {
	case iro_Div:
		switch(pn) {
		case pn_Div_M:
			return new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_M);
		case pn_Div_res:
			proj = new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_lohi);
			return new_rd_mips_mflo(dbgi, irg, block, proj);
		default:
			break;
		}
	case iro_Mod:
		switch(pn) {
		case pn_Mod_M:
			return new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_M);
		case pn_Mod_res:
			proj = new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_lohi);
			return new_rd_mips_mfhi(dbgi, irg, block, proj);
		default:
			break;
		}

	case iro_DivMod:
		switch(pn) {
		case pn_Div_M:
			return new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_M);
		case pn_DivMod_res_div:
			proj = new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_lohi);
			return new_rd_mips_mflo(dbgi, irg, block, proj);
		case pn_DivMod_res_mod:
			proj = new_rd_Proj(dbgi, irg, block, new_div, mode_M,
			                   pn_mips_div_lohi);
			return new_rd_mips_mfhi(dbgi, irg, block, proj);
		default:
			break;
		}
	default:
		break;
	}

	panic("invalid proj attached to %+F\n", divmod);
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	long      pn    = get_Proj_proj(node);

	if(pn == pn_Start_X_initial_exec) {
		/* we exchange the projx with a jump */
		ir_node *jump = new_rd_Jmp(dbgi, irg, block);
		return jump;
	}
	if(node == be_get_old_anchor(anchor_tls)) {
		/* TODO... */
		return be_duplicate_node(node);
	}
	return be_duplicate_node(node);
}

static ir_node *gen_Proj(ir_node *node)
{
	ir_graph *irg  = current_ir_graph;
	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node  *pred = get_Proj_pred(node);

	switch(get_irn_opcode(pred)) {
	case iro_Load:
		break;
	case iro_Store:
		break;
	case iro_Div:
	case iro_Mod:
	case iro_DivMod:
		return gen_Proj_DivMod(node);

	case iro_Start:
		return gen_Proj_Start(node);

	default:
		assert(get_irn_mode(node) != mode_T);
		if(mode_needs_gp_reg(get_irn_mode(node))) {
			ir_node *new_pred = be_transform_node(pred);
			ir_node *block    = be_transform_node(get_nodes_block(node));
			long     pn       = get_Proj_proj(node);

			return new_rd_Proj(dbgi, irg, block, new_pred, mode_Iu, pn);
		}
		break;
	}

	return be_duplicate_node(node);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *phi;

	if(mode_needs_gp_reg(mode)) {
		assert(get_mode_size_bits(mode) <= 32);
		mode = mode_Iu;
	}

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	phi = new_ir_node(dbgi, irg, block, op_Phi, mode, get_irn_arity(node),
	                  get_irn_in(node) + 1);
	copy_node_attr(node, phi);
	be_duplicate_deps(node, phi);

	be_set_transformed_node(node, phi);
	be_enqueue_preds(node);

	return phi;
}

#if 0
static
ir_node *gen_node_for_SwitchCond(mips_transform_env_t *env)
{
	ir_node *selector = get_Cond_selector(env->irn);
	ir_mode *selector_mode = get_irn_mode(selector);
	ir_node *node = env->irn;
	dbg_info *dbg = env->dbg;
	ir_graph *irg = env->irg;
	ir_node *block = env->block;
	ir_node *sub, *sltu, *minval_const, *max_const, *switchjmp;
	ir_node *defaultproj, *defaultproj_succ;
	ir_node *beq, *sl;
	long pn, minval, maxval, defaultprojn;
	const ir_edge_t *edge;
	ir_node *zero, *two_const, *add, *la, *load, *proj;
	ir_mode *unsigned_mode;
	mips_attr_t *attr;

	// mode_b conds are handled by gen_node_for_Proj
	if(get_mode_sort(selector_mode) != irms_int_number)
		return env->irn;

	assert(get_mode_size_bits(selector_mode) == 32);

	defaultproj = NULL;
	defaultprojn = get_Cond_defaultProj(node);

	// go over all projs to find min-&maxval of the switch
	minval = INT_MAX;
	maxval = INT_MIN;
	foreach_out_edge(node, edge) {
		ir_node* proj = get_edge_src_irn(edge);
		assert(is_Proj(proj) && "Only proj allowed at SwitchJmp");

		pn = get_Proj_proj(proj);
		if(pn == defaultprojn) {
			defaultproj = proj;
			continue;
		}

		if(pn < minval)
			minval = pn;
		if(pn > maxval)
			maxval = pn;
	}
	assert(defaultproj != NULL);

	// subtract minval from the switch value

	if(minval != 0) {
		minval_const = new_rd_Const(dbg, irg, block, selector_mode, new_tarval_from_long(minval, selector_mode));
		minval_const = gen_node_for_Const(env, dbg, irg, block, minval_const);
		sub = new_rd_mips_sub(dbg, irg, block, selector, minval_const);
	} else {
		sub = selector;
	}

	// compare if we're above maxval-minval or below zero.
	// we can do this with 1 compare because we use unsigned mode
	unsigned_mode = new_ir_mode(get_mode_name(selector_mode),
			get_mode_sort(selector_mode), get_mode_size_bits(selector_mode),
			0, get_mode_arithmetic(selector_mode), get_mode_modulo_shift(selector_mode));

	max_const = new_rd_Const(dbg, irg, block, unsigned_mode, new_tarval_from_long(maxval - minval + 1, unsigned_mode));
	max_const = gen_node_for_Const(env, dbg, irg, block, max_const);
	sltu = new_rd_mips_slt(dbg, irg, block, sub, max_const);

	zero = gen_zero_node(env, dbg, irg, block);
	beq = new_rd_mips_beq(dbg, irg, block, sltu, zero, mode_T);

	// attach defaultproj to beq now
	set_irn_n(defaultproj, 0, beq);
	set_Proj_proj(defaultproj, 1);

	two_const = new_rd_Const(dbg, irg, block, unsigned_mode, new_tarval_from_long(2, unsigned_mode));
	two_const = gen_node_for_Const(env, dbg, irg, block, two_const);
	sl = new_rd_mips_sl(dbg, irg, block, sub, two_const);

	la = new_rd_mips_la(dbg, irg, block);
	add = new_rd_mips_addu(dbg, irg, block, sl, la);
	load = new_rd_mips_load_r(dbg, irg, block, new_rd_NoMem(irg), add, mode_T);
	attr = get_mips_attr(load);
	attr->modes.load_store_mode = mode_Iu;
	attr->tv = new_tarval_from_long(0, mode_Iu);

	proj = new_rd_Proj(dbg, irg, block, load, mode_Iu, pn_Load_res);

	switchjmp = new_rd_mips_SwitchJump(dbg, irg, block, proj, mode_T);
	attr = get_mips_attr(switchjmp);
	attr->switch_default_pn = defaultprojn;

	edge = get_irn_out_edge_first(defaultproj);
	defaultproj_succ = get_edge_src_irn(edge);
	attr->symconst_id = new_id_from_str(mips_get_block_label(defaultproj_succ));

	attr = get_mips_attr(la);
	attr->symconst_id = new_id_from_str(mips_get_jumptbl_label(switchjmp));

	return switchjmp;
}
#endif

static ir_node *gen_Cond(ir_node *node)
{
	ir_graph *irg       = current_ir_graph;
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *sel_proj  = get_Cond_selector(node);
	ir_node  *cmp       = get_Proj_pred(sel_proj);
	ir_node  *left      = get_Cmp_left(cmp);
	ir_node  *new_left  = be_transform_node(left);
	ir_node  *right     = get_Cmp_right(cmp);
	ir_node  *new_right = be_transform_node(right);
	long      pnc       = get_Proj_proj(sel_proj);
	ir_node  *res;
	ir_node  *slt;
	ir_node  *zero;

	/* TODO: use blez & co. when possible */

	switch(pnc) {
	case pn_Cmp_False:
	case pn_Cmp_True:
	case pn_Cmp_Leg:
		panic("mips backend can't handle unoptimized constant Cond");

	case pn_Cmp_Eq:
		res = new_rd_mips_beq(dbgi, irg, block, new_left, new_right);
		break;

	case pn_Cmp_Lt:
		zero = mips_create_zero();
		slt  = new_rd_mips_slt(dbgi, irg, block, new_left, new_right);
		res  = new_rd_mips_bne(dbgi, irg, block, slt, zero);
		break;

	case pn_Cmp_Le:
		zero = mips_create_zero();
		slt  = new_rd_mips_slt(dbgi, irg, block, new_right, new_left);
		res  = new_rd_mips_beq(dbgi, irg, block, slt, zero);
		break;

	case pn_Cmp_Gt:
		zero = mips_create_zero();
		slt  = new_rd_mips_slt(dbgi, irg, block, new_right, new_left);
		res  = new_rd_mips_bne(dbgi, irg, block, slt, zero);
		break;

	case pn_Cmp_Ge:
		zero = mips_create_zero();
		slt  = new_rd_mips_slt(dbgi, irg, block, new_right, new_left);
		res  = new_rd_mips_bne(dbgi, irg, block, slt, zero);
		break;

	case pn_Cmp_Lg:
		res = new_rd_mips_bne(dbgi, irg, block, new_left, new_right);
		break;

	default:
		panic("mips backend doesn't handle unordered compares yet");
	}

	return res;
}

static ir_node *gen_Conv(ir_node *node)
{
	ir_graph *irg      = current_ir_graph;
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	int       src_size = get_mode_size_bits(src_mode);
	int       dst_size = get_mode_size_bits(dst_mode);
	ir_node  *res;

	assert(mode_needs_gp_reg(src_mode));
	assert(mode_needs_gp_reg(dst_mode));

	/* we only need to do something on upconvs */
	if(src_size >= dst_size) {
		/* unnecessary conv */
		return new_op;
	}

	if(mode_is_signed(src_mode)) {
		if(src_size == 8) {
			res = new_rd_mips_seb(dbgi, irg, block, new_op);
		} else if(src_size == 16) {
			res = new_rd_mips_seh(dbgi, irg, block, new_op);
		} else {
			panic("invalid conv %+F\n", node);
		}
	} else {
		ir_node *and_const;

		if(src_size == 8) {
			and_const = mips_create_Immediate(0xff);
		} else if(src_size == 16) {
			and_const = mips_create_Immediate(0xffff);
		} else {
			panic("invalid conv %+F\n", node);
		}
		res = new_rd_mips_and(dbgi, irg, block, new_op, and_const);
	}

	return res;
}

static ir_node *create_div(ir_node *node, ir_node *left, ir_node *right,
                           ir_mode *mode)
{
	ir_graph *irg       = current_ir_graph;
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_node(get_nodes_block(node));
	ir_node  *new_left  = be_transform_node(left);
	ir_node  *new_right = be_transform_node(right);
	ir_node  *res;

	if(mode_is_signed(mode)) {
		res = new_rd_mips_div(dbgi, irg, block, new_left, new_right);
	} else {
		res = new_rd_mips_divu(dbgi, irg, block, new_left, new_right);
	}

	set_irn_pinned(res, get_irn_pinned(node));

	return res;
}

static ir_node *gen_DivMod(ir_node *node)
{
	return create_div(node, get_DivMod_left(node), get_DivMod_right(node),
	                  get_DivMod_resmode(node));
}

static ir_node *gen_Div(ir_node *node)
{
	return create_div(node, get_Div_left(node), get_Div_right(node),
	                  get_Div_resmode(node));
}

static ir_node *gen_Mod(ir_node *node)
{
	return create_div(node, get_Mod_left(node), get_Mod_right(node),
	                  get_Mod_resmode(node));
}

#if 0
static ir_node *gen_node_for_Mul(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *mul;
	ir_node *mflo;
	ir_node *op1, *op2;
	ir_mode *mode = get_irn_mode(node);

	op1 = get_Mul_left(node);
	op2 = get_Mul_right(node);

	assert(get_mode_size_bits(env->mode) == 32);
	assert(get_mode_size_bits(get_irn_mode(op1)) == get_mode_size_bits(env->mode));
	assert(get_mode_size_bits(get_irn_mode(op2)) == get_mode_size_bits(env->mode));

	if(mode_is_signed(mode)) {
		mul = new_rd_mips_mult(env->dbg, env->irg, env->block, get_Mul_left(node), get_Mul_right(node));
	} else {
		mul = new_rd_mips_multu(env->dbg, env->irg, env->block, get_Mul_left(node), get_Mul_right(node));
	}
	mflo = new_rd_mips_mflo(env->dbg, env->irg, env->block, mul);

	return mflo;
}

static
ir_node *gen_node_for_IJmp(mips_transform_env_t *env) {
	ir_graph *irg    = env->irg;
	ir_node  *node   = env->irn;
	dbg_info *dbg    = get_irn_dbg_info(node);
	ir_node  *block  = get_nodes_block(node);
	ir_node  *target = get_IJmp_target(node);

	return new_rd_mips_jr(dbg, irg, block, target);
}

static
ir_node *gen_node_for_Rot(mips_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *subu, *srlv, *sllv, *or;

	subu = new_rd_mips_subuzero(env->dbg, env->irg, env->block, get_Rot_right(node));
	srlv = new_rd_mips_srlv(env->dbg, env->irg, env->block, get_Rot_left(node), subu);
	sllv = new_rd_mips_sllv(env->dbg, env->irg, env->block, get_Rot_left(node), get_Rot_right(node));
	or = new_rd_mips_or(env->dbg, env->irg, env->block, sllv, srlv);

	return or;
}
#endif

static ir_node *gen_Unknown(ir_node *node)
{
	(void) node;
	assert(mode_needs_gp_reg(get_irn_mode(node)));
	return mips_create_zero();
}

#if 0
/*
 * lower a copyB into standard Firm assembler :-)
 */
ir_node *gen_code_for_CopyB(ir_node *block, ir_node *node) {
	ir_node *cnt, *sub;
	ir_node *dst = get_CopyB_dst(node);
	ir_node *src = get_CopyB_src(node);
	ir_type *type = get_CopyB_type(node);
	ir_node *mem = get_CopyB_mem(node);
	ir_node *mm[4];
	ir_node *result = NULL;
	int size = get_type_size_bytes(type);
	dbg_info *dbg = get_irn_dbg_info(node);
	ir_graph *irg = get_irn_irg(block);
	mips_attr_t *attr;
	int i, n;

	if (size > 16) {
		ir_node     *phi, *projT, *projF, *cmp, *proj, *cond, *jmp, *in[2];
		ir_node     *new_bl, *src_phi, *dst_phi, *mem_phi, *add;
		ir_mode     *p_mode = get_irn_mode(src);
		ir_node     *ld[4];

		/* build the control loop */
		in[0] = in[1] = new_r_Unknown(irg, mode_X);

		new_bl = new_r_Block(irg, 2, in);

		in[0] = cnt = new_Const_long(mode_Is, (size >> 4));
        in[1] = new_r_Unknown(irg, mode_Is);
		phi   = new_r_Phi(irg, new_bl, 2, in, mode_Is);

		sub = new_rd_Sub(dbg, irg, new_bl, phi, new_Const_long(mode_Is, -1), mode_Is);
		set_Phi_pred(phi, 1, sub);

		cmp = new_rd_Cmp(dbg, irg, new_bl, sub, new_Const_long(mode_Is, 0));
		proj = new_r_Proj(irg, new_bl, cmp, mode_b, pn_Cmp_Lg);
		cond = new_rd_Cond(dbg, irg, new_bl, proj);

		projT = new_r_Proj(irg, new_bl, cond, mode_X, pn_Cond_true);
		projF = new_r_Proj(irg, new_bl, cond, mode_X, pn_Cond_false);

		jmp = get_Block_cfgpred(block, 0);
		set_Block_cfgpred(block, 0, projF);

		set_Block_cfgpred(new_bl, 0, jmp);
		set_Block_cfgpred(new_bl, 1, projT);

		size &= 0xF;

		/* build the copy */
		in[0]   = src;
        in[1]   = new_r_Unknown(irg, p_mode);
		src_phi = new_r_Phi(irg, new_bl, 2, in, p_mode);

		in[0]   = dst;
		dst_phi = new_r_Phi(irg, new_bl, 2, in, p_mode);

		add = new_rd_Add(dbg, irg, new_bl, src_phi, new_Const_long(mode_Is, 16), p_mode);
		set_Phi_pred(src_phi, 1, add);
		add = new_rd_Add(dbg, irg, new_bl, dst_phi, new_Const_long(mode_Is, 16), p_mode);
		set_Phi_pred(dst_phi, 1, add);

		in[0]   = mem;
        in[1]   = new_r_Unknown(irg, mode_M);
		mem_phi = new_r_Phi(irg, new_bl, 2, in, mode_M);

		src = src_phi;
		dst = dst_phi;

		/* create 4 parallel loads */
		for (i = 0; i < 4; ++i) {
			ir_node *load;

			load = new_rd_mips_load_r(dbg, irg, new_bl, mem_phi, src, mode_T);
			attr = get_mips_attr(load);
			attr->modes.load_store_mode = mode_Iu;
			attr->tv = new_tarval_from_long(i * 4, mode_Iu);

			ld[i] = new_rd_Proj(dbg, irg, new_bl, load, mode_Iu, pn_Load_res);
		}

		/* create 4 parallel stores */
		for (i = 0; i < 4; ++i) {
			ir_node *store;

			store = new_rd_mips_store_r(dbg, irg, new_bl, mem_phi, dst, ld[i], mode_T);
			attr = get_mips_attr(store);
			attr->modes.load_store_mode = mode_Iu;
			attr->tv = new_tarval_from_long(i * 4, mode_Iu);

			mm[i] = new_rd_Proj(dbg, irg, new_bl, store, mode_M, pn_Store_M);
		}
		mem = new_r_Sync(irg, new_bl, 4, mm);
		result = mem;
		set_Phi_pred(mem_phi, 1, mem);
	}

	// output store/loads manually
	n = 0;
	for(i = size; i > 0; ) {
		ir_mode *mode;
		ir_node *load, *store, *projv;
		int offset = size - i;
		if(i >= 4) {
			mode = mode_Iu;
			i -= 4;
		} else if(i >= 2) {
			mode = mode_Hu;
			i -= 2;
		} else {
			mode = mode_Bu;
			i -= 1;
		}

		load = new_rd_mips_load_r(dbg, irg, block, mem, src, mode_T);
		attr = get_mips_attr(load);
		attr->modes.load_store_mode = mode;
		attr->tv = new_tarval_from_long(offset, mode_Iu);

		projv = new_rd_Proj(dbg, irg, block, load, mode, pn_Load_res);

		store = new_rd_mips_store_r(dbg, irg, block, mem, dst, projv, mode_T);
		attr = get_mips_attr(store);
		attr->modes.load_store_mode = mode;
		attr->tv = new_tarval_from_long(offset, mode_Iu);

		mm[n] = new_rd_Proj(dbg, irg, block, store, mode_M, pn_Store_M);
		n++;
	}

	if(n > 0) {
		result = new_r_Sync(irg, block, n, mm);
	} else if(n == 1) {
		result = mm[0];
	}

	return result;
}

static void mips_fix_CopyB_Proj(mips_transform_env_t* env) {
	ir_node *node = env->irn;
	long n = get_Proj_proj(node);

	if(n == pn_CopyB_M_except) {
		assert(0);
	} else if(n == pn_CopyB_M_regular) {
		set_Proj_proj(node, pn_Store_M);
	} else if(n == pn_CopyB_M_except) {
		set_Proj_proj(node, pn_Store_X_except);
	}
}
#endif

static void mips_transform_Spill(mips_transform_env_t* env) {
	ir_node   *node = env->irn;
	ir_node   *sched_point = NULL;
	ir_node   *store;
	ir_node   *nomem = new_rd_NoMem(env->irg);
	ir_node   *ptr   = get_irn_n(node, 0);
	ir_node   *val   = get_irn_n(node, 1);
	ir_entity *ent   = be_get_frame_entity(node);

	if(sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	store = new_rd_mips_sw(env->dbg, env->irg, env->block, ptr, val, nomem,
	                       ent, 0);

	if (sched_point) {
		sched_add_after(sched_point, store);
		sched_remove(node);
	}

	exchange(node, store);
}

static void mips_transform_Reload(mips_transform_env_t* env) {
	ir_node   *node = env->irn;
	ir_node   *sched_point = NULL;
	ir_node   *load, *proj;
	ir_node   *ptr   = get_irn_n(node, 0);
	ir_node   *mem   = get_irn_n(node, 1);
	ir_entity *ent   = be_get_frame_entity(node);
	const arch_register_t* reg;

	if(sched_is_scheduled(node)) {
		sched_point = sched_prev(node);
	}

	load = new_rd_mips_lw(env->dbg, env->irg, env->block, ptr, mem, ent, 0);

	proj = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_Iu, pn_mips_lw_res);

	if (sched_point) {
		sched_add_after(sched_point, load);

		sched_remove(node);
	}

	/* copy the register from the old node to the new Load */
	reg = arch_get_irn_register(env->cg->arch_env, node);
	arch_set_irn_register(env->cg->arch_env, proj, reg);

	exchange(node, proj);
}

#if 0
static ir_node *gen_node_for_StackParam(mips_transform_env_t *env)
{
	ir_node *node = env->irn;
	ir_node *sp = get_irn_n(node, 0);
	ir_node *load;
	ir_node *nomem = new_rd_NoMem(env->irg);
	ir_node *proj;
	mips_attr_t *attr;

	load = new_rd_mips_load_r(env->dbg, env->irg, env->block, nomem, sp, mode_T);
	attr = get_mips_attr(load);
	attr->stack_entity = be_get_frame_entity(node);
	attr->modes.load_store_mode = env->mode;

	proj = new_rd_Proj(env->dbg, env->irg, env->block, load, env->mode, pn_Load_res);

	return proj;
}
#endif

#if 0
static ir_node *gen_AddSP(ir_node *node)
{
	ir_node *node = env->irn;
	ir_node *op1, *op2;
	ir_node *add;
	const arch_register_t *reg;

	op1 = get_irn_n(node, 0);
	op2 = get_irn_n(node, 1);

	add = new_rd_mips_addu(env->dbg, env->irg, env->block, op1, op2);

	/* copy the register requirements from the old node to the new node */
	reg = arch_get_irn_register(env->cg->arch_env, node);
	arch_set_irn_register(env->cg->arch_env, add, reg);

	return add;
}
#endif

/*********************************************************
 *                  _             _      _
 *                 (_)           | |    (_)
 *  _ __ ___   __ _ _ _ __     __| |_ __ ___   _____ _ __
 * | '_ ` _ \ / _` | | '_ \   / _` | '__| \ \ / / _ \ '__|
 * | | | | | | (_| | | | | | | (_| | |  | |\ V /  __/ |
 * |_| |_| |_|\__,_|_|_| |_|  \__,_|_|  |_| \_/ \___|_|
 *
 *********************************************************/

static ir_node *gen_Bad(ir_node *node)
{
	panic("Unexpected node %+F found in mips transform phase.\n", node);
	return NULL;
}

static void register_transformers(void)
{
	clear_irp_opcodes_generic_func();

	op_Add->ops.generic      = (op_func) gen_Add;
	op_Sub->ops.generic      = (op_func) gen_Sub;
	op_And->ops.generic      = (op_func) gen_And;
	op_Or->ops.generic       = (op_func) gen_Or;
	op_Eor->ops.generic      = (op_func) gen_Eor;
	op_Shl->ops.generic      = (op_func) gen_Shl;
	op_Shr->ops.generic      = (op_func) gen_Shr;
	op_Shrs->ops.generic     = (op_func) gen_Shrs;
	op_Not->ops.generic      = (op_func) gen_Not;
	op_Minus->ops.generic    = (op_func) gen_Minus;
	op_Div->ops.generic      = (op_func) gen_Div;
	op_Mod->ops.generic      = (op_func) gen_Mod;
	op_DivMod->ops.generic   = (op_func) gen_DivMod;
	op_Abs->ops.generic      = (op_func) gen_Abs;
	op_Load->ops.generic     = (op_func) gen_Load;
	op_Store->ops.generic    = (op_func) gen_Store;
	op_Cond->ops.generic     = (op_func) gen_Cond;
	op_Conv->ops.generic     = (op_func) gen_Conv;
	op_Const->ops.generic    = (op_func) gen_Const;
	op_SymConst->ops.generic = (op_func) gen_SymConst;
	op_Unknown->ops.generic  = (op_func) gen_Unknown;
	op_Proj->ops.generic     = (op_func) gen_Proj;
	op_Phi->ops.generic      = (op_func) gen_Phi;

	op_Raise->ops.generic     = (op_func) gen_Bad;
	op_Sel->ops.generic       = (op_func) gen_Bad;
	op_InstOf->ops.generic    = (op_func) gen_Bad;
	op_Cast->ops.generic      = (op_func) gen_Bad;
	op_Free->ops.generic      = (op_func) gen_Bad;
	op_Tuple->ops.generic     = (op_func) gen_Bad;
	op_Id->ops.generic        = (op_func) gen_Bad;
	op_Confirm->ops.generic   = (op_func) gen_Bad;
	op_Filter->ops.generic    = (op_func) gen_Bad;
	op_CallBegin->ops.generic = (op_func) gen_Bad;
	op_EndReg->ops.generic    = (op_func) gen_Bad;
	op_EndExcept->ops.generic = (op_func) gen_Bad;
}

#if 0
/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void mips_transform_node(ir_node *node, void *env) {
	mips_code_gen_t *cgenv = (mips_code_gen_t *)env;
	ir_opcode code         = get_irn_opcode(node);
	ir_node *asm_node      = node;
	mips_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
	tenv.mode     = get_irn_mode(node);
	tenv.cg		  = cgenv;

#define UNOP(firm_opcode, mips_nodetype)        case iro_##firm_opcode: asm_node = mips_gen_##mips_nodetype(&tenv, get_##firm_opcode##_op(node)); break
#define BINOP(firm_opcode, mips_nodetype)       case iro_##firm_opcode: asm_node = mips_gen_##mips_nodetype(&tenv, get_##firm_opcode##_left(node), get_##firm_opcode##_right(node)); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	switch (code) {
 		BINOP(Add, addu);
		BINOP(Sub, sub);
		BINOP(And, and);
		BINOP(Or, or);
		BINOP(Eor, xor);
		UNOP(Not, not);
		BINOP(Shl, sl);
		BINOP(Shr, sr);
		BINOP(Shrs, sra);

	case iro_Abs:
		asm_node = gen_node_for_Abs(&tenv);
		break;

	case iro_Rot:
		asm_node = gen_node_for_Rot(&tenv);
		break;

	case iro_Div:
		asm_node = gen_node_for_Div(&tenv);
		break;

	case iro_Mod:
		asm_node = gen_node_for_Mod(&tenv);
		break;

	case iro_Load:
		asm_node = gen_node_for_Load(&tenv);
		break;

	case iro_Store:
		asm_node = gen_node_for_Store(&tenv);
		break;

	case iro_Proj:
		asm_node = gen_node_for_Proj(&tenv);
		break;

	case iro_Conv:
		asm_node = gen_node_for_Conv(&tenv);
		break;

	case iro_DivMod:
		asm_node = gen_node_for_DivMod(&tenv);
		break;

	case iro_Mul:
		asm_node = gen_node_for_Mul(&tenv);
		break;

	case iro_Jmp:
		asm_node = gen_node_for_Jmp(&tenv);
		break;

	case iro_IJmp:
		asm_node = gen_node_for_IJmp(&tenv);
		break;

	case iro_Unknown:
		asm_node = gen_node_for_Unknown(&tenv);
		break;

	case iro_Cond:
		asm_node = gen_node_for_Cond(&tenv);
		break;

	case iro_Phi:
		asm_node = gen_node_for_Phi(&tenv);
		break;

		/* TODO: implement these nodes */
		BAD(Mux);

		/* You probably don't need to handle the following nodes */

		// call is handled in the emit phase
		IGN(Call);
		// Cmp is handled together with Cond
		IGN(Cmp);
		IGN(Alloc);

		IGN(Block);
		IGN(Start);
		IGN(End);
		IGN(NoMem);
		IGN(Break);
		IGN(Sync);

		IGN(Const);
		IGN(SymConst);

		BAD(Raise);
		BAD(Sel);
		BAD(InstOf);
		BAD(Cast);
		BAD(Free);
		BAD(Tuple);
		BAD(Id);
		BAD(Bad);
		BAD(Confirm);
		BAD(Filter);
		BAD(CallBegin);
		BAD(EndReg);
		BAD(EndExcept);

		default:
			if(be_is_StackParam(node)) {
				//asm_node = gen_node_for_StackParam(&tenv);
			} else if(be_is_AddSP(node)) {
				asm_node = gen_node_for_AddSP(&tenv);
			}
			break;

bad:
		fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
		assert(0);
	}

	if (asm_node != node) {
		exchange(node, asm_node);
	}
}
#endif

void mips_transform_graph(mips_code_gen_t *cg)
{
	env_cg = cg;
	register_transformers();
	be_transform_graph(cg->birg, NULL, cg);
}

/**
 * Calls the transform functions for Spill and Reload.
 */
void mips_after_ra_walker(ir_node *node, void *env) {
	mips_code_gen_t *cg = env;
	mips_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block = get_nodes_block(node);
	tenv.dbg   = get_irn_dbg_info(node);
	tenv.irg   = current_ir_graph;
	tenv.irn   = node;
	tenv.mode  = get_irn_mode(node);
	tenv.cg    = cg;

	if (be_is_Reload(node)) {
		mips_transform_Reload(&tenv);
	} else if (be_is_Spill(node)) {
		mips_transform_Spill(&tenv);
	}
}
