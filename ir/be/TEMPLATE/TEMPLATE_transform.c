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
 * @brief   code selection (transform FIRM into TEMPLATE FIRM)
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "irvrfy.h"
#include "ircons.h"
#include "iropt_t.h"
#include "debug.h"

#include "../benode_t.h"
#include "bearch_TEMPLATE_t.h"

#include "TEMPLATE_nodes_attr.h"
#include "archop.h"
#include "TEMPLATE_transform.h"
#include "TEMPLATE_new_nodes.h"
#include "TEMPLATE_map_regs.h"

#include "gen_TEMPLATE_regalloc_if.h"

extern ir_op *get_op_Mulh(void);



/****************************************************************************************************
 *                  _        _                        __                           _   _
 *                 | |      | |                      / _|                         | | (_)
 *  _ __   ___   __| | ___  | |_ _ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___   __ _| |_ _  ___  _ __
 * | '_ \ / _ \ / _` |/ _ \ | __| '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \ / _` | __| |/ _ \| '_ \
 * | | | | (_) | (_| |  __/ | |_| | | (_| | | | \__ \ || (_) | |  | | | | | | (_| | |_| | (_) | | | |
 * |_| |_|\___/ \__,_|\___|  \__|_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|\__,_|\__|_|\___/|_| |_|
 *
 ****************************************************************************************************/

/**
 * Creates an TEMPLATE Add.
 *
 * @param env   The transformation environment
 * @param op1   first operator
 * @param op2   second operator
 * @return the created TEMPLATE Add node
 */
static ir_node *gen_Add(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_Add(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Mul.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Mul node
 */
static ir_node *gen_Mul(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return new_rd_TEMPLATE_fMul(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	else {
		return new_rd_TEMPLATE_Mul(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
}



/**
 * Creates an TEMPLATE And.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE And node
 */
static ir_node *gen_And(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_And(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Or.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Or node
 */
static ir_node *gen_Or(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_Or(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Eor.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Eor node
 */
static ir_node *gen_Eor(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_Eor(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Sub.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Sub node
 */
static ir_node *gen_Sub(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	if (mode_is_float(env->mode)) {
		return new_rd_TEMPLATE_fSub(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
	else {
		return new_rd_TEMPLATE_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode);
	}
}



/**
 * Creates an TEMPLATE floating Div.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE fDiv node
 */
static ir_node *gen_Quot(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_fDiv(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Shl.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Shl node
 */
static ir_node *gen_Shl(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_Shl(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Creates an TEMPLATE Shr.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created TEMPLATE Shr node
 */
static ir_node *gen_Shr(TEMPLATE_transform_env_t *env, ir_node *op1, ir_node *op2) {
	return new_rd_TEMPLATE_Shr(env->dbg, env->irg, env->block, op1, op2, env->mode);
}



/**
 * Transforms a Minus node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Minus node
 * @param op      operator
 * @param mode    node mode
 * @return the created TEMPLATE Minus node
 */
static ir_node *gen_Minus(TEMPLATE_transform_env_t *env, ir_node *op) {
	if (mode_is_float(env->mode)) {
		return new_rd_TEMPLATE_fMinus(env->dbg, env->irg, env->block, op, env->mode);
	}
	return new_rd_TEMPLATE_Minus(env->dbg, env->irg, env->block, op, env->mode);
}



/**
 * Transforms a Not node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Not node
 * @param op      operator
 * @param mode    node mode
 * @return the created TEMPLATE Not node
 */
static ir_node *gen_Not(TEMPLATE_transform_env_t *env, ir_node *op) {
	return new_rd_TEMPLATE_Not(env->dbg, env->irg, env->block, op, env->mode);
}



/**
 * Transforms a Load.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Load node
 * @param mode    node mode
 * @return the created TEMPLATE Load node
 */
static ir_node *gen_Load(TEMPLATE_transform_env_t *env) {
	ir_node *node = env->irn;

	if (mode_is_float(env->mode)) {
		return new_rd_TEMPLATE_fLoad(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
	}
	return new_rd_TEMPLATE_Load(env->dbg, env->irg, env->block, get_Load_ptr(node), get_Load_mem(node), env->mode);
}



/**
 * Transforms a Store.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Store node
 * @param mode    node mode
 * @return the created TEMPLATE Store node
 */
static ir_node *gen_Store(TEMPLATE_transform_env_t *env) {
	ir_node *node = env->irn;

	if (mode_is_float(env->mode)) {
		return new_rd_TEMPLATE_fStore(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
	}
	return new_rd_TEMPLATE_Store(env->dbg, env->irg, env->block, get_Store_ptr(node), get_Store_value(node), get_Store_mem(node), env->mode);
}



/*********************************************************
 *                  _             _      _
 *                 (_)           | |    (_)
 *  _ __ ___   __ _ _ _ __     __| |_ __ ___   _____ _ __
 * | '_ ` _ \ / _` | | '_ \   / _` | '__| \ \ / / _ \ '__|
 * | | | | | | (_| | | | | | | (_| | |  | |\ V /  __/ |
 * |_| |_| |_|\__,_|_|_| |_|  \__,_|_|  |_| \_/ \___|_|
 *
 *********************************************************/



/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void TEMPLATE_transform_node(ir_node *node, void *env) {
	TEMPLATE_code_gen_t *cgenv = (TEMPLATE_code_gen_t *)env;
	ir_opcode code             = get_irn_opcode(node);
	ir_node *asm_node          = NULL;
	TEMPLATE_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block    = get_nodes_block(node);
	tenv.dbg      = get_irn_dbg_info(node);
	tenv.irg      = current_ir_graph;
	tenv.irn      = node;
#ifdef DEBUG_libfirm
	tenv.mod      = cgenv->mod;
#endif
	tenv.mode     = get_irn_mode(node);

#define UNOP(a)        case iro_##a: asm_node = gen_##a(&tenv, get_##a##_op(node)); break
#define BINOP(a)       case iro_##a: asm_node = gen_##a(&tenv, get_##a##_left(node), get_##a##_right(node)); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(&tenv); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	switch (code) {
		BINOP(Add);
		BINOP(Mul);
		BINOP(And);
		BINOP(Or);
		BINOP(Eor);

		BINOP(Sub);
		BINOP(Shl);
		BINOP(Shr);
		BINOP(Quot);


		UNOP(Minus);
		UNOP(Not);

		GEN(Load);
		GEN(Store);

		/* TODO: implement these nodes */
		IGN(Shrs);
		IGN(Div);
		IGN(Mod);
		IGN(DivMod);
		IGN(Const);
		IGN(SymConst);
		IGN(Conv);
		IGN(Abs);
		IGN(Cond);
		IGN(Mux);
		IGN(CopyB);
		IGN(Unknown);
		IGN(Cmp);

		/* You probably don't need to handle the following nodes */

		IGN(Call);
		IGN(Proj);
		IGN(Alloc);

		IGN(Block);
		IGN(Start);
		IGN(End);
		IGN(NoMem);
		IGN(Phi);
		IGN(IJmp);
		IGN(Jmp);
		IGN(Break);
		IGN(Sync);

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
			if (get_irn_op(node) == get_op_Max() ||
				get_irn_op(node) == get_op_Min() ||
				get_irn_op(node) == get_op_Mulh())
			{
				/* TODO: implement */
				/* ignore for now  */
			}
			break;
bad:
		fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
		assert(0);
	}

	if (asm_node) {
		exchange(node, asm_node);
		DB((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((tenv.mod, LEVEL_1, "ignored\n"));
	}
}
