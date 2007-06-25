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
 * @brief   The codegenerator (transform FIRM into ppc FIRM)
 * @author  Moritz Kroll, Jens Mueller
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
#include "iredges_t.h"
#include "irvrfy.h"
#include "ircons.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "irprintf.h"
#include "debug.h"

#include "../benode_t.h"
#include "bearch_ppc32_t.h"

#include "ppc32_nodes_attr.h"
#include "archop.h"
#include "ppc32_transform.h"
#include "ppc32_new_nodes.h"
#include "ppc32_map_regs.h"

#include "gen_ppc32_regalloc_if.h"

extern ir_op *get_op_Mulh(void);

int is_direct_entity(ir_entity *ent);

ir_mode* ppc32_mode_Cond = NULL;

/**
 * Returns the proj of a given node with the given proj number
 */
static INLINE ir_node *get_succ_Proj(ir_node *node, long proj)
{
	const ir_edge_t *edge;
	foreach_out_edge(node, edge)
	{
		if (is_Proj(edge->src) && get_Proj_proj(edge->src) == proj)
			return edge->src;
	}
	return NULL;
}

/**
 * Returns a singleton condition mode
 */
ir_mode *get_ppc32_mode_Cond(void) {
	if (ppc32_mode_Cond)
		return ppc32_mode_Cond;
	else {
		ppc32_mode_Cond = new_ir_mode("mode_Cond", irms_character, 4, 0, irma_none, 0);
		return ppc32_mode_Cond;
	}
}

/**
 * Calculates the modecode with size, sort and signed attributes
 */
modecode get_nice_modecode(ir_mode *irmode)
{
	modecode mode = irm_max;
	int sign = mode_is_signed(irmode);
	int bits = get_mode_size_bits(irmode);
	if(mode_is_int(irmode))
	{
		switch(bits)
		{
			case 8:
				mode = sign ? irm_Bs : irm_Bu;
				break;
			case 16:
				mode = sign ? irm_Hs : irm_Hu;
				break;
			case 32:
				mode = sign ? irm_Is : irm_Iu;
				break;
		}
	}
	else if(mode_is_float(irmode))
	{
		switch(bits)
		{
			case 32:
				mode = irm_F;
				break;
			case 64:
				mode = irm_D;
				break;
		}
	}
	else if(mode_is_reference(irmode))
	{
		switch(bits)
		{
			case 32:
				mode = irm_P;
				break;
		}
	}
	return mode;
}

/**
 * Returns true, if the given node is a Const node and it's value fits into
 * a signed 16-bit variable
 */
int is_16bit_signed_const(ir_node *node)
{
	tarval *tv_const;

	if(!is_ppc32_Const(node)) return 0;

	tv_const = get_ppc32_constant_tarval(node);

	switch(get_nice_modecode(get_irn_mode(node)))
	{
		case irm_Bu:
		case irm_Bs:
		case irm_Hs:
			return 1;
		case irm_Iu:
		case irm_P:
		{
			unsigned char val2 = get_tarval_sub_bits(tv_const, 2);
			unsigned char val3 = get_tarval_sub_bits(tv_const, 3);
			if(val2 || val3)
				return 0;

			// fall through
		}
		case irm_Hu:
		{
			unsigned char val1 = get_tarval_sub_bits(tv_const, 1);
			if(val1&0x80)
				return 0;
			return 1;
		}

		case irm_Is:
		{
			unsigned char val2 = get_tarval_sub_bits(tv_const, 2);
			unsigned char val3 = get_tarval_sub_bits(tv_const, 3);
			if(val2==0 && val3==0)
			{
				unsigned char val1 = get_tarval_sub_bits(tv_const, 1);
				if(val1&0x80)
					return 0;
				return 1;
			}
			if(!(val2==0xff && val3==0xff))
			{
				unsigned char val1 = get_tarval_sub_bits(tv_const, 1);
				if(!(val1&0x80))
					return 0;
				return 1;
			}
			return 0;
		}
		default:
			fprintf(stderr, "is_16bit_signed_const(): Mode not supported: %s\n", get_mode_name(get_irn_mode(node)));
			assert(0);
			return 0;
	}
}

/**
 * Returns true, if the given node is a Const node and it's value fits into
 * a unsigned 16-bit variable
 */
int is_16bit_unsigned_const(ir_node *node)
{
	tarval *tv_const;

	if(!is_ppc32_Const(node)) return 0;

	tv_const = get_ppc32_constant_tarval(node);
	switch(get_nice_modecode(get_irn_mode(node)))
	{
		case irm_Bu:
		case irm_Bs:
		case irm_Hs:
		case irm_Hu:
			return 1;
		case irm_Iu:
		case irm_P:
		case irm_Is:
		{
			unsigned char val2 = get_tarval_sub_bits(tv_const, 2);
			unsigned char val3 = get_tarval_sub_bits(tv_const, 3);
			if(val2 || val3)
				return 0;
			return 1;
		}
		default:
			fprintf(stderr, "is_16bit_unsigned_const(): Mode not supported: %s\n", get_mode_name(get_irn_mode(node)));
			assert(0);
			return 0;
	}
}


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
 * Creates an ppc Add.
 *
 * @param env   The transformation environment
 * @return the created ppc Add node
 */
static ir_node *gen_Add(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Add_left(env->irn);
	ir_node *op2 = get_Add_right(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
			return new_rd_ppc32_fAdd(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_F:
			return new_rd_ppc32_fAdds(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_Is:
		case irm_Iu:
		case irm_Hs:
		case irm_Hu:
		case irm_Bs:
		case irm_Bu:
		case irm_P:
			if(is_16bit_signed_const(op1))
			{
				ir_node *addnode = new_rd_ppc32_Addi(env->dbg, env->irg, env->block, op2, env->mode);
				set_ppc32_constant_tarval(addnode, get_ppc32_constant_tarval(op1));
				set_ppc32_offset_mode(addnode, ppc32_ao_None);
				return addnode;
			}
			if(is_16bit_signed_const(op2))
			{
				ir_node *addnode = new_rd_ppc32_Addi(env->dbg, env->irg, env->block, op1, env->mode);
				set_ppc32_constant_tarval(addnode, get_ppc32_constant_tarval(op2));
				set_ppc32_offset_mode(addnode, ppc32_ao_None);
				return addnode;
			}

			return new_rd_ppc32_Add(env->dbg, env->irg, env->block, op1, op2, env->mode);

		default:
			fprintf(stderr, "Mode for Add not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc Mul.
 *
 * @param env   The transformation environment
 * @return the created ppc Mul node
 */
static ir_node *gen_Mul(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Mul_left(env->irn);
	ir_node *op2 = get_Mul_right(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
			return new_rd_ppc32_fMul(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_F:
			return new_rd_ppc32_fMuls(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_Is:
		case irm_Iu:
		case irm_Hs:
		case irm_Hu:
		case irm_Bs:
		case irm_Bu:
			return new_rd_ppc32_Mullw(env->dbg, env->irg, env->block, op1, op2, env->mode);

		case irm_P:
		default:
			fprintf(stderr, "Mode for Mul not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc Mulh.
 *
 * @param env   The transformation environment
 * @return the created ppc Mulh node
 */
static ir_node *gen_Mulh(ppc32_transform_env_t *env) {
	ir_node *op1 = get_irn_n(env->irn, 0);
	ir_node *op2 = get_irn_n(env->irn, 1);

	switch(get_nice_modecode(env->mode)){
		case irm_Is:
		case irm_Hs:
		case irm_Bs:
			return new_rd_ppc32_Mulhw(env->dbg, env->irg, env->block, op1, op2, env->mode);

		case irm_Iu:
		case irm_Hu:
		case irm_Bu:
			return new_rd_ppc32_Mulhwu(env->dbg, env->irg, env->block, op1, op2, env->mode);

		case irm_D:
		case irm_F:
		case irm_P:
		default:
			fprintf(stderr, "Mode for Mulh not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc And.
 *
 * @param env   The transformation environment
 * @return the created ppc And node
 */
static ir_node *gen_And(ppc32_transform_env_t *env) {
	ir_node *op1 = get_And_left(env->irn);
	ir_node *op2 = get_And_right(env->irn);

	return new_rd_ppc32_And(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Or.
 *
 * @param env   The transformation environment
 * @return the created ppc Or node
 */
static ir_node *gen_Or(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Or_left(env->irn);
	ir_node *op2 = get_Or_right(env->irn);

	return new_rd_ppc32_Or(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Xor.
 *
 * @param env   The transformation environment
 * @return the created ppc Xor node
 */
static ir_node *gen_Eor(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Eor_left(env->irn);
	ir_node *op2 = get_Eor_right(env->irn);

	return new_rd_ppc32_Xor(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Sub.
 *
 * @param env   The transformation environment
 * @return the created ppc Sub node
 */
static ir_node *gen_Sub(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Sub_left(env->irn);
	ir_node *op2 = get_Sub_right(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
			return new_rd_ppc32_fSub(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_F:
			return new_rd_ppc32_fSubs(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_Is:
		case irm_Iu:
		case irm_Hs:
		case irm_Hu:
		case irm_Bs:
		case irm_Bu:
		case irm_P:
			return new_rd_ppc32_Sub(env->dbg, env->irg, env->block, op1, op2, env->mode);

		default:
			fprintf(stderr, "Mode for Sub not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc floating Div.
 *
 * @param env   The transformation environment
 * @return the created ppc fDiv node
 */
static ir_node *gen_Quot(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Quot_left(env->irn);
	ir_node *op2 = get_Quot_right(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
			return new_rd_ppc32_fDiv(env->dbg, env->irg, env->block, op1, op2, env->mode);
		case irm_F:
			return new_rd_ppc32_fDivs(env->dbg, env->irg, env->block, op1, op2, env->mode);

		default:
			fprintf(stderr, "Mode for Quot not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc integer Div.
 *
 * @param env   The transformation environment
 * @return the created ppc Div node
 */
static ir_node *gen_Div(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Div_left(env->irn);
	ir_node *op2 = get_Div_right(env->irn);

	switch(get_nice_modecode(get_irn_mode(op1))){
		case irm_Is:
		case irm_Hs:
		case irm_Bs:
			return new_rd_ppc32_Divw(env->dbg, env->irg, env->block, op1, op2, mode_T);

		case irm_Iu:
		case irm_Hu:
		case irm_Bu:
			return new_rd_ppc32_Divwu(env->dbg, env->irg, env->block, op1, op2, mode_T);

		default:
			fprintf(stderr, "Mode for Div not supported: %s\n", get_mode_name(get_irn_mode(op1)));
			assert(0);
			return NULL;
	}
}

/**
 * Creates an ppc integer Div and Mod.
 *
 * @param env   The transformation environment
 * @return the created ppc Div node
 */
static ir_node *gen_DivMod(ppc32_transform_env_t *env) {
	ir_node *op1 = get_DivMod_left(env->irn);
	ir_node *op2 = get_DivMod_right(env->irn);
	ir_node *proj_div = NULL, *proj_mod = NULL;
	ir_node *div_result;
	const ir_edge_t *edge;
	ir_mode *res_mode;

	foreach_out_edge(env->irn, edge)
	{
		if (is_Proj(edge->src))
		{
			switch(get_Proj_proj(edge->src)){
				case pn_DivMod_res_div:
					proj_div = edge->src;
					break;
				case pn_DivMod_res_mod:
					proj_mod = edge->src;
					break;
				default:
					break;
			}
		}
	}

	assert(proj_div!=NULL || proj_mod!=NULL);

	res_mode = get_irn_mode(proj_div);

	switch(get_nice_modecode(res_mode))
	{
		case irm_Is:
		case irm_Hs:
		case irm_Bs:
			div_result = new_rd_ppc32_Divw(env->dbg, env->irg, env->block, op1, op2, mode_T);
			break;

		case irm_Iu:
		case irm_Hu:
		case irm_Bu:
			div_result = new_rd_ppc32_Divwu(env->dbg, env->irg, env->block, op1, op2, mode_T);
			break;

		default:
			fprintf(stderr, "Mode for DivMod not supported: %s\n", get_mode_name(res_mode));
			assert(0);
			return 0;

	}

	if (proj_div == NULL)
		proj_div = new_rd_Proj(env->dbg, env->irg, env->block, div_result, get_irn_mode(proj_mod), pn_DivMod_res_div);

	if (proj_mod!=NULL){
		ir_node *mul_result;
		ir_node *mod_result;

		mul_result = new_rd_ppc32_Mullw(env->dbg, env->irg, env->block, proj_div, op2, res_mode);
		mod_result = new_rd_ppc32_Sub(env->dbg, env->irg, env->block, op1, mul_result, res_mode);

		exchange(proj_mod, mod_result);
	}

	return div_result;
}

/**
 * Creates an ppc integer Mod.
 *
 * @param env   The transformation environment
 * @return the created ppc Mod result node
 */
static ir_node *gen_Mod(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Mod_left(env->irn);
	ir_node *op2 = get_Mod_right(env->irn);
	ir_node *proj_div = NULL, *proj_mod = NULL;
	ir_node *div_result;
	ir_mode *res_mode;
	ir_node *mul_result;
	ir_node *mod_result;

	proj_mod = get_succ_Proj(env->irn, pn_Mod_res);

	assert(proj_mod != NULL);
	res_mode = get_irn_mode(proj_mod);

	switch(get_nice_modecode(res_mode))
	{
		case irm_Is:
		case irm_Hs:
		case irm_Bs:
			div_result = new_rd_ppc32_Divw(env->dbg, env->irg, env->block, op1, op2, mode_T);
			break;

		case irm_Iu:
		case irm_Hu:
		case irm_Bu:
			div_result = new_rd_ppc32_Divwu(env->dbg, env->irg, env->block, op1, op2, mode_T);
			break;

		default:
			fprintf(stderr, "Mode for Mod not supported: %s\n", get_mode_name(res_mode));
			assert(0);
			return NULL;

	}

	proj_div = new_rd_Proj(env->dbg, env->irg, env->block, div_result, res_mode, pn_DivMod_res_div);

	mul_result = new_rd_ppc32_Mullw(env->dbg, env->irg, env->block, proj_div, op2, res_mode);
	mod_result = new_rd_ppc32_Sub(env->dbg, env->irg, env->block, op1, mul_result, res_mode);

	exchange(proj_mod, mod_result);

	return div_result;
}

/**
 * Creates an ppc Shl.
 *
 * @param env   The transformation environment
 * @return the created ppc Shl node
 */
static ir_node *gen_Shl(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Shl_left(env->irn);
	ir_node *op2 = get_Shl_right(env->irn);

	if(is_ppc32_Const(op2))
	{
		ir_node *shift = new_rd_ppc32_Rlwinm(env->dbg, env->irg, env->block, op1, env->mode);
		tarval *tv_const = get_ppc32_constant_tarval(op2);
		int sh = get_tarval_long(tv_const);
		assert(0<=sh && sh<=31);
		set_ppc32_rlwimi_const(shift, sh, 0, 31-sh);
		return shift;
	}
	return new_rd_ppc32_Slw(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Srw.
 *
 * @param env   The transformation environment
 * @return the created ppc Shr node
 */
static ir_node *gen_Shr(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Shr_left(env->irn);
	ir_node *op2 = get_Shr_right(env->irn);

	if(is_ppc32_Const(op2))
	{
		ir_node *shift = new_rd_ppc32_Rlwinm(env->dbg, env->irg, env->block, op1, env->mode);
		tarval *tv_const = get_ppc32_constant_tarval(op2);
		int sh = get_tarval_long(tv_const);
		assert(0<=sh && sh<=31);
		set_ppc32_rlwimi_const(shift, 32-sh, sh, 31);
		return shift;
	}
	return new_rd_ppc32_Srw(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Sraw.
 *
 * @param env   The transformation environment
 * @return the created ppc Sraw node
 */
static ir_node *gen_Shrs(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Shrs_left(env->irn);
	ir_node *op2 = get_Shrs_right(env->irn);

	if(is_ppc32_Const(op2))
	{
		ir_node *shift = new_rd_ppc32_Srawi(env->dbg, env->irg, env->block, op1, env->mode);
		tarval *tv_const = get_ppc32_constant_tarval(op2);
		int sh = get_tarval_long(tv_const);
		assert(0<=sh && sh<=31);
		set_ppc32_constant_tarval(shift, tv_const);
		set_ppc32_offset_mode(shift, ppc32_ao_None);
		return shift;
	}
	return new_rd_ppc32_Sraw(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc RotL.
 *
 * @param env   The transformation environment
 * @return the created ppc RotL node
 */
static ir_node *gen_Rot(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Rot_left(env->irn);
	ir_node *op2 = get_Rot_right(env->irn);

	if(is_ppc32_Const(op2))
	{
		ir_node *rot = new_rd_ppc32_Rlwinm(env->dbg, env->irg, env->block, op1, env->mode);
		tarval *tv_const = get_ppc32_constant_tarval(op2);
		int sh = get_tarval_long(tv_const);
		assert(0<=sh && sh<=31);
		set_ppc32_rlwimi_const(rot, sh, 0, 31);
		return rot;
	}
	return new_rd_ppc32_Rlwnm(env->dbg, env->irg, env->block, op1, op2, env->mode);
}

/**
 * Creates an ppc Cmp.
 *
 * @param env   The transformation environment
 * @return the created ppc Cmp node
 */
static ir_node *gen_Cmp(ppc32_transform_env_t *env) {
	ir_node *op1 = get_Cmp_left(env->irn);
	ir_node *op2 = get_Cmp_right(env->irn);

	const ir_edge_t *edge;
	foreach_out_edge(env->irn, edge)
	{
		if (is_Proj(edge->src))
			set_irn_mode(edge->src, get_ppc32_mode_Cond());
	}

	if(mode_is_float(env->mode))
		return new_rd_ppc32_fCmpu(env->dbg, env->irg, env->block, op1, op2, env->mode);
	else if(mode_is_signed(env->mode))
	{
		if(is_16bit_signed_const(op2))
		{
			ir_node *cmp = new_rd_ppc32_Cmpi(env->dbg, env->irg, env->block, op1, env->mode);
			tarval *tv_const = get_ppc32_constant_tarval(op2);
			set_ppc32_constant_tarval(cmp, tv_const);
			set_ppc32_offset_mode(cmp, ppc32_ao_None);
			return cmp;
		}
		else
		{
			return new_rd_ppc32_Cmp(env->dbg, env->irg, env->block, op1, op2, env->mode);
		}
	}
	else
	{
		if(is_16bit_unsigned_const(op2))
		{
			ir_node *cmp = new_rd_ppc32_Cmpli(env->dbg, env->irg, env->block, op1, env->mode);
			tarval *tv_const = get_ppc32_constant_tarval(op2);
			set_ppc32_constant_tarval(cmp, tv_const);
			set_ppc32_offset_mode(cmp, ppc32_ao_None);

			return cmp;
		}
		else
		{
			return new_rd_ppc32_Cmpl(env->dbg, env->irg, env->block, op1, op2, env->mode);
		}
	}
}

/**
 * Transforms a Minus node.
 *
 * @param env   The transformation environment
 * @return the created ppc Minus node
 */
static ir_node *gen_Minus(ppc32_transform_env_t *env) {
	ir_node *op = get_Minus_op(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
		case irm_F:
			return new_rd_ppc32_fNeg(env->dbg, env->irg, env->block, op, env->mode);
		case irm_Is:
		case irm_Iu:
		case irm_Hs:
		case irm_Hu:
		case irm_Bs:
		case irm_Bu:
		case irm_P:
			return new_rd_ppc32_Neg(env->dbg, env->irg, env->block, op, env->mode);

		default:
			fprintf(stderr, "Mode for Neg not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return NULL;
	}
}

/**
 * Transforms a Not node.
 *
 * @param env   The transformation environment
 * @return the created ppc Not node
 */
static ir_node *gen_Not(ppc32_transform_env_t *env) {
	return new_rd_ppc32_Not(env->dbg, env->irg, env->block, get_Not_op(env->irn), env->mode);
}


static ir_node *own_gen_Andi_dot_lo16(ppc32_transform_env_t *env, ir_node *op, int mask)
{
	ir_node *andi = new_rd_ppc32_Andi_dot(env->dbg, env->irg, env->block, op, mode_T);
	ir_node* in[1];
	set_ppc32_offset_mode(andi, ppc32_ao_Lo16);
	set_ppc32_constant_tarval(andi, new_tarval_from_long(mask, mode_Is));
	in[0] = new_rd_Proj(env->dbg, env->irg, env->block, andi, env->mode,1);
	be_new_Keep(&ppc32_reg_classes[CLASS_ppc32_condition], env->irg, env->block, 1, in);
	return new_rd_Proj(env->dbg, env->irg, env->block, andi, env->mode,0);
}

/**
 * Transforms a Conv node.
 *
 * @param env   The transformation environment
 * @return the created ppc Conv node
 */
static ir_node *gen_Conv(ppc32_transform_env_t *env) {
	ir_node *op = get_Conv_op(env->irn);
	modecode from_mode=get_nice_modecode(get_irn_mode(op));
	modecode to_mode=get_nice_modecode(env->mode);

#define SKIP return op

	if(from_mode == to_mode) SKIP;

	switch(from_mode){
		case irm_F:
			switch(to_mode)
			{
				case irm_D: SKIP;
				default:
					break;
			}
			break;

		case irm_D:
			switch(to_mode)
			{
				case irm_F:
					return new_rd_ppc32_fRsp(env->dbg, env->irg, env->block, op, env->mode);
				default:
					break;
			}
			break;

		case irm_Is:
		case irm_Iu:
			switch(to_mode)
			{
				case irm_Hs:
					return new_rd_ppc32_Extsh(env->dbg, env->irg, env->block, op, env->mode);
				case irm_Hu:
					return own_gen_Andi_dot_lo16(env, op, 0xffff);
				case irm_Bs:
					return new_rd_ppc32_Extsb(env->dbg, env->irg, env->block, op, env->mode);
				case irm_Bu:
					return own_gen_Andi_dot_lo16(env, op, 0xff);
				case irm_Is:
				case irm_Iu:
					SKIP;
				default:
					break;
			}
			break;

		case irm_Hs:
		case irm_Hu:
			switch(to_mode)
			{
				case irm_Iu:
					if(from_mode==irm_Hu)
				case irm_Hu:
						return own_gen_Andi_dot_lo16(env, op, 0xffff);
				case irm_Is:
					SKIP;
				case irm_Bs:
					return new_rd_ppc32_Extsb(env->dbg, env->irg, env->block, op, env->mode);
				case irm_Bu:
					return own_gen_Andi_dot_lo16(env, op, 0xff);
				case irm_Hs:
					return new_rd_ppc32_Extsh(env->dbg, env->irg, env->block, op, env->mode);
				default:
					break;
			}
			break;

		case irm_Bs:
		case irm_Bu:
			switch(to_mode)
			{
				case irm_Iu:
				case irm_Hu:
					if(from_mode==irm_Bs)
				case irm_Bu:
						return own_gen_Andi_dot_lo16(env, op, 0xff);
				case irm_Is:
				case irm_Hs:
					SKIP;
				case irm_Bs:
					return new_rd_ppc32_Extsb(env->dbg, env->irg, env->block, op, env->mode);
				default:
					break;
			}
			break;
		case irm_P:
			if(to_mode==irm_Is || to_mode==irm_Iu) SKIP;
			break;
		default:
			break;
	}

	fprintf(stderr, "Mode for Conv not supported: %s -> %s\n",
		get_mode_name(get_irn_mode(get_irn_n(env->irn,0))),get_mode_name(env->mode));
	assert(0);
	return NULL;

#undef SKIP
}

/**
 * Transforms an Abs node.
 *
 * @param env   The transformation environment
 * @return the ppc node generating the absolute value
 */
static ir_node *gen_Abs(ppc32_transform_env_t *env) {
	ir_node *op = get_Abs_op(env->irn);
	int shift = 7;
	ir_node *n1,*n2;

	switch(get_nice_modecode(env->mode))
	{
		case irm_F:
		case irm_D:
			return new_rd_ppc32_fAbs(env->dbg, env->irg, env->block, op, env->mode);
		case irm_Is:
			shift += 16;
		case irm_Hs:
			shift += 8;
		case irm_Bs:
			n1 = new_rd_ppc32_Srawi(env->dbg, env->irg, env->block, op, env->mode);
			set_ppc32_constant_tarval(n1, new_tarval_from_long(shift, mode_Is));
			set_ppc32_offset_mode(n1, ppc32_ao_None);
			n2 = new_rd_ppc32_Add(env->dbg, env->irg, env->block, op, n1, env->mode);
			return new_rd_ppc32_Xor(env->dbg, env->irg, env->block, n2, n1, env->mode);
		default:
			break;
	}
	fprintf(stderr, "Mode for Abs not supported: %s\n", get_mode_name(env->mode));
	assert(0);
	return NULL;
}

/**
 * Transforms an Cond node.
 *
 * @param env   The transformation environment
 * @return a ppc branch node
 */
static ir_node *gen_Cond(ppc32_transform_env_t *env) {
	ir_node *selector = get_Cond_selector(env->irn);
	ir_mode *projmode = get_irn_mode(selector);
	if(is_Proj(selector) && projmode==get_ppc32_mode_Cond())
	{
		int projnum = get_Proj_proj(selector);
		ir_node *branch = new_rd_ppc32_Branch(env->dbg, env->irg, env->block, selector, env->mode);
		set_ppc32_proj_nr(branch, projnum);
		return branch;
	}
	else
	{
		ir_node *unknown_gpr = new_rd_ppc32_Unknown(env->dbg, env->irg, env->block, mode_Is);
		ir_node *unknown_cond = new_rd_ppc32_cUnknown(env->dbg, env->irg, env->block, get_ppc32_mode_Cond());

		ir_node *switch_node = new_rd_ppc32_Switch(env->dbg, env->irg, env->block, selector,
			unknown_gpr, unknown_cond, env->mode);
		set_ppc32_proj_nr(switch_node, get_Cond_defaultProj(env->irn));

		return switch_node;
	}
}

/**
 * Transforms an Unknown node.
 *
 * @param env   The transformation environment
 * @return a ppc Unknown node
 */
static ir_node *gen_Unknown(ppc32_transform_env_t *env) {
	if(mode_is_float(env->mode))
		return new_rd_ppc32_fUnknown(env->dbg, env->irg, env->block, env->mode);
	else if (mode_is_int(env->mode))
		return new_rd_ppc32_Unknown(env->dbg, env->irg, env->block, env->mode);
	else
	{
		fprintf(stderr, "Mode %s for unknown value not supported.\n", get_mode_name(env->mode));
		assert(0);
		return 0;
	}
}

static ir_node *ldst_insert_const(ir_node *ptr, tarval **ptv, ident **pid, ppc32_transform_env_t *env) {
	tarval *tv_const = NULL;
	ident *id_symconst = NULL;

	if(is_ppc32_Const(ptr))
	{
		tv_const = get_ppc32_constant_tarval(ptr);
		ptr = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, mode_P, ppc32_ao_Ha16, tv_const, NULL);
	}
	else if(is_ppc32_SymConst(ptr))
	{
		ir_entity *ent = get_ppc32_frame_entity(ptr);
		if(is_direct_entity(ent))
		{
			id_symconst = get_entity_ident(ent);
			ptr = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, mode_P, ppc32_ao_Ha16, NULL, id_symconst);
		}
	}
	*ptv = tv_const;
	*pid = id_symconst;
	return ptr;
}

/**
 * Transforms a Load.
 *
 * @param env   The transformation environment
 * @return the created ppc Load node
 */
static ir_node *gen_Load(ppc32_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *loadptr = get_Load_ptr(node);
	ir_node *load;
	ir_mode *mode = get_Load_mode(node);
	tarval *tv_const = NULL;
	ident *id_symconst = NULL;

	loadptr = ldst_insert_const(loadptr, &tv_const, &id_symconst, env);
	switch(get_nice_modecode(mode)){
		case irm_Bu:
			load = new_rd_ppc32_Lbz(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;

		case irm_Bs:
		{
			ir_node *proj_load, *extsb_node;
			load =  new_rd_ppc32_Lbz(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			proj_load = new_rd_Proj(env->dbg, env->irg, env->block, load, mode, pn_Load_res);
			extsb_node = new_rd_ppc32_Extsb(env->dbg, env->irg, env->block, proj_load, mode);
			exchange(get_succ_Proj(env->irn, pn_Load_res), extsb_node);
			break;
		}


		case irm_Hu:
			load = new_rd_ppc32_Lhz(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;
		case irm_Hs:
			load =new_rd_ppc32_Lha(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;
		case irm_Is:
		case irm_Iu:
		case irm_P:
			load = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;

		case irm_D:
			load = new_rd_ppc32_Lfd(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;
		case irm_F:
			load = new_rd_ppc32_Lfs(env->dbg, env->irg, env->block, loadptr, get_Load_mem(node));
			break;

		default:
			fprintf(stderr, "Mode for Load not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return 0;
	}

	if(tv_const)
	{
		set_ppc32_offset_mode(load, ppc32_ao_Lo16);
		set_ppc32_constant_tarval(load, tv_const);
	}
	else if(id_symconst)
	{
		set_ppc32_offset_mode(load, ppc32_ao_Lo16);
		set_ppc32_symconst_ident(load, id_symconst);
	}
	return load;
}



/**
 * Transforms a Store.
 *
 * @param env   The transformation environment
 * @return the created ppc Store node
 */
static ir_node *gen_Store(ppc32_transform_env_t *env) {
	ir_node *node = env->irn;
	ir_node *storeptr = get_Store_ptr(node);
	ir_node *valuenode = get_Store_value(node);
	ir_mode *mode = get_irn_mode(valuenode);
	ir_node *store;
	tarval *tv_const = NULL;
	ident *id_symconst = NULL;

	storeptr = ldst_insert_const(storeptr, &tv_const, &id_symconst, env);

	switch(get_nice_modecode(mode)){
		case irm_Bu:
		case irm_Bs:
			store = new_rd_ppc32_Stb(env->dbg, env->irg, env->block, storeptr, get_Store_value(node), get_Store_mem(node));
			break;

		case irm_Hu:
		case irm_Hs:
			store = new_rd_ppc32_Sth(env->dbg, env->irg, env->block, storeptr, get_Store_value(node), get_Store_mem(node));
			break;
		case irm_Is:
		case irm_Iu:
		case irm_P:
			store = new_rd_ppc32_Stw(env->dbg, env->irg, env->block, storeptr, get_Store_value(node), get_Store_mem(node));
			break;

		case irm_D:
			store = new_rd_ppc32_Stfd(env->dbg, env->irg, env->block, storeptr, get_Store_value(node), get_Store_mem(node));
			break;
		case irm_F:
			store = new_rd_ppc32_Stfs(env->dbg, env->irg, env->block, storeptr, get_Store_value(node), get_Store_mem(node));
			break;

		default:
			fprintf(stderr, "Mode for Store not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return 0;
	}
	if(tv_const)
	{
		set_ppc32_offset_mode(store, ppc32_ao_Lo16);
		set_ppc32_constant_tarval(store, tv_const);
	}
	else if(id_symconst)
	{
		set_ppc32_offset_mode(store, ppc32_ao_Lo16);
		set_ppc32_symconst_ident(store, id_symconst);
	}
	return store;
}

/**
 * Transforms a CopyB.
 *
 * @param env   The transformation environment
 * @return the created ppc CopyB node
 */
static ir_node *gen_CopyB(ppc32_transform_env_t *env) {
	ir_node *mem = get_CopyB_mem(env->irn);
	ir_node *src = get_CopyB_src(env->irn);
	ir_node *dest = get_CopyB_dst(env->irn);
	ir_type *type = get_CopyB_type(env->irn);
	int size = get_type_size_bytes(type);
	int offset = 0;

	ir_node *load, *store = NULL;

	if(size/4 >= 1)
	{
		ir_node *res;
		tarval *offset0 = new_tarval_from_long(0, mode_Is);
		tarval *offset4 = new_tarval_from_long(4, mode_Is);

		load = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, src, mem);
		set_ppc32_constant_tarval(load, offset0);
		set_ppc32_offset_mode(load, ppc32_ao_None);
		mem = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_M, pn_Load_M);
		res = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_Is, pn_Load_res);

		store = new_rd_ppc32_Stw(env->dbg, env->irg, env->block, dest, res, mem);
		set_ppc32_constant_tarval(store, offset0);
		set_ppc32_offset_mode(store, ppc32_ao_None);
		mem = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, pn_Store_M);

		if(size/4==2)
		{
			load = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, src, mem);
			set_ppc32_constant_tarval(load, offset4);
			set_ppc32_offset_mode(load, ppc32_ao_None);
			mem = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_M, pn_Load_M);
			res = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_Is, pn_Load_res);

			store = new_rd_ppc32_Stw(env->dbg, env->irg, env->block, dest, res, mem);
			set_ppc32_constant_tarval(store, offset4);
			set_ppc32_offset_mode(store, ppc32_ao_None);
			mem = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, pn_Store_M);

			offset = 8;
		}
		else
		{
			ir_node *ornode, *mtctrnode;
			ir_node* in[3];
			assert(size/4-1<=0xffff);
			if(size/4-1<0x8000)
			{
				ornode = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, mode_Is);
				set_ppc32_offset_mode(ornode, ppc32_ao_None);
			}
			else
			{
				ir_node *zeroreg = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, mode_Is);
				set_ppc32_offset_mode(zeroreg, ppc32_ao_None);
				set_ppc32_constant_tarval(zeroreg, new_tarval_from_long(0, mode_Is));
				ornode = new_rd_ppc32_Ori(env->dbg, env->irg, env->block, zeroreg, mode_Is);
				set_ppc32_offset_mode(ornode, ppc32_ao_Lo16);
			}

			set_ppc32_constant_tarval(ornode, new_tarval_from_long(size/4-1, mode_Is));
			mtctrnode = new_rd_ppc32_Mtctr(env->dbg, env->irg, env->block, ornode, mode_Is);
			store = new_rd_ppc32_LoopCopy(env->dbg, env->irg, env->block, src, dest, mtctrnode, mem, mode_T);

			in[0] = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_Is, 1); // src
			in[1] =	new_rd_Proj(env->dbg, env->irg, env->block, store, mode_Is, 2);	// dest
			in[2] =	new_rd_Proj(env->dbg, env->irg, env->block, store, mode_Is, 4);	// temp
			be_new_Keep(&ppc32_reg_classes[CLASS_ppc32_gp], env->irg, env->block, 3, in);
			in[0] = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_Is, 3); // ctr
			be_new_Keep(&ppc32_reg_classes[CLASS_ppc32_count], env->irg, env->block, 1, in);

			mem = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, 0);

			offset = 4;
		}
	}

	if(size & 2)
	{
		ir_node *res;
		tarval* offset_tarval = new_tarval_from_long(offset, mode_Is);
		load = new_rd_ppc32_Lhz(env->dbg, env->irg, env->block, src, mem);
		set_ppc32_constant_tarval(load, offset_tarval);
		set_ppc32_offset_mode(load, ppc32_ao_None);
		mem = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_M, pn_Load_M);
		res = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_Is, pn_Load_res);

		store = new_rd_ppc32_Sth(env->dbg, env->irg, env->block, dest, res, mem);
		set_ppc32_constant_tarval(store, offset_tarval);
		set_ppc32_offset_mode(store, ppc32_ao_None);
		mem = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, pn_Store_M);

		offset += 2;
	}

	if(size & 1)
	{
		ir_node *res;
		tarval* offset_tarval = new_tarval_from_long(offset, mode_Is);
		load = new_rd_ppc32_Lbz(env->dbg, env->irg, env->block, src, mem);
		set_ppc32_constant_tarval(load, offset_tarval);
		set_ppc32_offset_mode(load, ppc32_ao_None);
		mem = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_M, pn_Load_M);
		res = new_rd_Proj(env->dbg, env->irg, env->block, load, mode_Is, pn_Load_res);

		store = new_rd_ppc32_Stb(env->dbg, env->irg, env->block, dest, res, mem);
		set_ppc32_constant_tarval(store, offset_tarval);
		set_ppc32_offset_mode(store, ppc32_ao_None);
		// mem = new_rd_Proj(env->dbg, env->irg, env->block, store, mode_M, pn_Store_M);
	}

	return store;
}

/**
 * Transforms a FrameAddr into a ppc Add.
 *
 * @param env   The transformation environment
 */
static ir_node *gen_be_FrameAddr(ppc32_transform_env_t *env) {
	ir_node *op = get_irn_n(env->irn, 0);
	ir_node *add = new_rd_ppc32_Addi(env->dbg, env->irg, env->block, op, mode_P);
	set_ppc32_frame_entity(add, be_get_frame_entity(env->irn));
	return add;
}

/**
 * Transforms a StackParam into a ppc Load
 *
 * @param env   The transformation environment
 */
static ir_node *gen_be_StackParam(ppc32_transform_env_t *env) {
	ir_node *load = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, get_irn_n(env->irn, 0), new_NoMem());
	ir_node *proj = new_rd_Proj(env->dbg, env->irg, env->block, load, env->mode, pn_Load_res);
	set_ppc32_frame_entity(load, be_get_frame_entity(env->irn));
	return proj;
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
 * the BAD transformer.
 */
static ir_node *bad_transform(ppc32_transform_env_t *env) {
	ir_fprintf(stderr, "Not implemented: %+F\n", env->irn);
	assert(0);
	return NULL;
}

/**
 * Enters all transform functions into the generic pointer
 */
void ppc32_register_transformers(void) {
	ir_op *op_Max, *op_Min, *op_Mulh;

	/* first clear the generic function pointer for all ops */
	clear_irp_opcodes_generic_func();

#define FIRM_OP(a)     op_##a->ops.generic = (op_func)gen_##a
#define BAD(a)         op_##a->ops.generic = (op_func)bad_transform
#define IGN(a)

	FIRM_OP(Add);
	FIRM_OP(Mul);
	FIRM_OP(And);
	FIRM_OP(Or);
	FIRM_OP(Eor);

	FIRM_OP(Sub);
	FIRM_OP(Shl);
	FIRM_OP(Shr);
	FIRM_OP(Shrs);
	FIRM_OP(Rot);
	FIRM_OP(Quot);
	FIRM_OP(Div);
	FIRM_OP(DivMod);
	FIRM_OP(Mod);
	FIRM_OP(Cmp);

	FIRM_OP(Minus);
	FIRM_OP(Not);
	FIRM_OP(Conv);
	FIRM_OP(Abs);

	FIRM_OP(Load);
	FIRM_OP(Store);
	FIRM_OP(Cond);
	FIRM_OP(Unknown);
	FIRM_OP(CopyB);

	/* TODO: implement these nodes */
	BAD(Mux);

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

	FIRM_OP(be_FrameAddr);
	FIRM_OP(be_StackParam);
	op_Mulh = get_op_Mulh();
	if (op_Mulh)
		FIRM_OP(Mulh);
	op_Max = get_op_Max();
	if (op_Max)
		BAD(Max);
	op_Min = get_op_Min();
	if (op_Min)
		BAD(Min);
}

typedef ir_node *(transform_func)(ppc32_transform_env_t *env);

/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ppc32_transform_node(ir_node *node, void *env) {
	ppc32_code_gen_t *cg = (ppc32_code_gen_t *)env;
	ir_op *op            = get_irn_op(node);
	ir_node *asm_node    = NULL;

	if (op == op_Block)
		return;

	DBG((cg->mod, LEVEL_1, "check %+F ... ", node));

	if (op->ops.generic) {
		ppc32_transform_env_t tenv;
		transform_func *transform = (transform_func *)op->ops.generic;

		tenv.block    = get_nodes_block(node);
		tenv.dbg      = get_irn_dbg_info(node);
		tenv.irg      = current_ir_graph;
		tenv.irn      = node;
		tenv.mode     = get_irn_mode(node);
		DEBUG_ONLY(tenv.mod = cg->mod;)

		asm_node = (*transform)(&tenv);
	}

	if (asm_node) {
		exchange(node, asm_node);
		DB((cg->mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((cg->mod, LEVEL_1, "ignored\n"));
	}
}

/**
 * Constant generating code
 */

struct tv_ent {
	ir_entity *ent;
	tarval *tv;
};

/** Compares two (entity, tarval) combinations */
static int cmp_tv_ent(const void *a, const void *b, size_t len) {
	const struct tv_ent *e1 = a;
	const struct tv_ent *e2 = b;
	(void) len;

	return !(e1->tv == e2->tv);
}

/** Generates a SymConst node for a known FP const */
static ir_node *gen_fp_known_symconst(ppc32_transform_env_t *env, tarval *known_const) {
	static set    *const_set = NULL;
	static ir_type *tp = NULL;
	struct tv_ent  key;
	struct tv_ent *entry;
	ir_node       *cnst,*symcnst;
	ir_graph      *rem;
	ir_entity     *ent = NULL;

	if(!const_set)
		const_set = new_set(cmp_tv_ent, 10);
	if(!tp)
		tp = new_type_primitive(new_id_from_str("const_double_t"), env->mode);

	key.tv  = known_const;
	key.ent = NULL;

	entry = set_insert(const_set, &key, sizeof(key), HASH_PTR(key.tv));

	if(!entry->ent) {
		char buf[80];
		sprintf(buf, "const_%ld", get_irn_node_nr(env->irn));
		ent = new_entity(get_glob_type(), new_id_from_str(buf), tp);

		set_entity_ld_ident(ent, get_entity_ident(ent));
		set_entity_visibility(ent, visibility_local);
		set_entity_variability(ent, variability_constant);
		set_entity_allocation(ent, allocation_static);

		/* we create a new entity here: It's initialization must resist on the
		    const code irg */
		rem = current_ir_graph;
		current_ir_graph = get_const_code_irg();
		cnst = new_Const(env->mode, key.tv);
		current_ir_graph = rem;

		set_atomic_ent_value(ent, cnst);

		/* set the entry for hashmap */
		entry->ent = ent;
	}				 // TODO: Wird nicht richtig in global type gesteckt, ppc32_gen_decls.c findet ihn nicht

	symcnst = new_rd_ppc32_SymConst(env->dbg, env->irg, env->block, env->mode);
	set_ppc32_frame_entity(symcnst, ent);
	return symcnst;
}

static ir_node *gen_ppc32_SymConst(ppc32_transform_env_t *env);

/**
 * Transforms a Const.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    node mode
 * @return the created ppc Load immediate node
 */
static ir_node *gen_ppc32_Const(ppc32_transform_env_t *env) {
	tarval *tv_const = get_ppc32_constant_tarval(env->irn);
	ir_node *node;

	switch(get_nice_modecode(env->mode)){
		case irm_Hu:
		{
			unsigned char val1 = get_tarval_sub_bits(tv_const, 1);
			if(val1&0x80)
			{
				ir_node *zeroreg = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, mode_Is);
				set_ppc32_constant_tarval(zeroreg, new_tarval_from_long(0, mode_Is));
				set_ppc32_offset_mode(zeroreg, ppc32_ao_None);
				node = new_rd_ppc32_Ori(env->dbg, env->irg, env->block, zeroreg, mode_Is);
				set_ppc32_offset_mode(node, ppc32_ao_Lo16);
				break;
			}
		}
		case irm_Bu:
		case irm_Bs:
		case irm_Hs:
			node = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, env->mode);
			set_ppc32_offset_mode(node, ppc32_ao_None);
			break;
		case irm_Is:
		case irm_Iu:
		case irm_P:
		{
			unsigned char val2 = get_tarval_sub_bits(tv_const,2);
			unsigned char val3 = get_tarval_sub_bits(tv_const,3);
			if(!val2 && !val3)
			{
				unsigned char val1 = get_tarval_sub_bits(tv_const, 1);
				if(val1&0x80)
				{
					ir_node *zeroreg = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, mode_Is);
					set_ppc32_constant_tarval(zeroreg, new_tarval_from_long(0, mode_Is));
					set_ppc32_offset_mode(zeroreg, ppc32_ao_None);
					node = new_rd_ppc32_Ori(env->dbg, env->irg, env->block, zeroreg, mode_Is);
					set_ppc32_offset_mode(node, ppc32_ao_Lo16);
				}
				else
				{
					node = new_rd_ppc32_Addi_zero(env->dbg, env->irg, env->block, env->mode);
					set_ppc32_offset_mode(node, ppc32_ao_None);
				}
			}
			else
			{
				unsigned char val0 = get_tarval_sub_bits(tv_const,0);
				unsigned char val1 = get_tarval_sub_bits(tv_const,1);
				node = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, env->mode, ppc32_ao_Hi16, tv_const, NULL);
				if(val0 || val1)
				{
					set_ppc32_constant_tarval(node, tv_const);
					node = new_rd_ppc32_Ori(env->dbg, env->irg, env->block, node, env->mode);
					set_ppc32_offset_mode(node, ppc32_ao_Lo16);
				}
			}
			break;
		}

		default:
			fprintf(stderr, "Mode for Const not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return 0;
	}
	set_ppc32_constant_tarval(node, tv_const);
	return node;
}

/**
 * Transforms a fConst.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    node mode
 * @return the created ppc float Load node
 */
static ir_node *gen_ppc32_fConst(ppc32_transform_env_t *env) {
	tarval *tv_const = get_ppc32_constant_tarval(env->irn);

	switch(get_nice_modecode(env->mode)){
		case irm_D:
		case irm_F:
		{
			ir_node *addr, *load;
			ir_mode *mode = env->mode;
			ir_entity *ent;
			env->irn = gen_fp_known_symconst(env, tv_const);
			env->mode = mode_P;
			ent = get_ppc32_frame_entity(env->irn);
			if(is_direct_entity(ent))
			{
				ident *id_symconst = get_entity_ident(ent);
				ir_node *node_addis = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, env->mode, ppc32_ao_Ha16, NULL, id_symconst);

				if(mode==mode_D)
					load = new_rd_ppc32_Lfd(env->dbg, env->irg, env->block, node_addis, new_NoMem());
				else // mode_F
					load = new_rd_ppc32_Lfs(env->dbg, env->irg, env->block, node_addis, new_NoMem());

				set_ppc32_symconst_ident(load, id_symconst);
				set_ppc32_offset_mode(load, ppc32_ao_Lo16);
			}
			else
			{
				addr = gen_ppc32_SymConst (env);
				if(mode==mode_D)
					load = new_rd_ppc32_Lfd(env->dbg, env->irg, env->block, addr, new_NoMem());
				else // mode_F
					load = new_rd_ppc32_Lfs(env->dbg, env->irg, env->block, addr, new_NoMem());
			}
			return new_rd_Proj(env->dbg, env->irg, env->block, load, mode, pn_Load_res);
		}

		default:
			fprintf(stderr, "Mode for fConst not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return 0;
	}
	assert(0 && "Dead end!");
}


/**
 * Returns true, if the entity can be accessed directly,
 * or false, if the address must be loaded first
 */
int is_direct_entity(ir_entity *ent) {
	return get_entity_visibility(ent) != visibility_external_allocated;
/*	visibility vis = get_entity_visibility(ent);
	if(is_Method_type(get_entity_type(ent)))
	{
		return (vis!=visibility_external_allocated);
	}
	else
	{
		return (vis==visibility_local);
	}*/
}

/**
 * Transforms a SymConst.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    node mode
 * @return the created ppc Load immediate node
 */
static ir_node *gen_ppc32_SymConst(ppc32_transform_env_t *env) {
	ir_entity *ent = get_ppc32_frame_entity(env->irn);
	ident *id_symconst = get_entity_ident(ent);
	ir_node *node;
	switch(get_nice_modecode(env->mode)){
		case irm_P:
		{
			if (is_direct_entity(ent))
			{
				ir_node *node_addis = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, env->mode, ppc32_ao_Hi16, NULL, id_symconst);
				node = new_rd_ppc32_Ori(env->dbg, env->irg, env->block, node_addis, env->mode);
				set_ppc32_symconst_ident(node, id_symconst);
				set_ppc32_offset_mode(node, ppc32_ao_Lo16);
			}
			else
			{
				ir_node *node_addis = new_rd_ppc32_Addis_zero(env->dbg, env->irg, env->block, env->mode, ppc32_ao_Ha16, NULL, id_symconst);
				node = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, node_addis, new_NoMem());
				set_ppc32_symconst_ident(node, id_symconst);
				set_ppc32_offset_mode(node, ppc32_ao_Lo16);
				node = new_rd_Proj(env->dbg, env->irg, env->block, node, env->mode, pn_Load_res);
			}
			break;
		}

		default:
			fprintf(stderr, "Mode for SymConst not supported: %s\n", get_mode_name(env->mode));
			assert(0);
			return 0;
	}
	return node;
}

/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ppc32_transform_const(ir_node *node, void *env) {
	ppc32_code_gen_t *cgenv    = (ppc32_code_gen_t *)env;
	ir_node          *asm_node = NULL;
	ppc32_transform_env_t tenv;

	if (is_Block(node))
		return;

	tenv.block = get_nodes_block(node);
	tenv.dbg   = get_irn_dbg_info(node);
	tenv.irg   = current_ir_graph;
	tenv.irn   = node;
	DEBUG_ONLY(tenv.mod   = cgenv->mod;)
	tenv.mode  = get_irn_mode(node);

#define OTHER_GEN(a)                        \
	if (get_irn_op(node) == get_op_##a()) { \
		asm_node = gen_##a(&tenv);          \
	}

	DBG((tenv.mod, LEVEL_1, "check %+F ... ", node));

	OTHER_GEN(ppc32_Const)
	else OTHER_GEN(ppc32_fConst)
	else OTHER_GEN(ppc32_SymConst)

	if (asm_node) {
		exchange(node, asm_node);
		DB((tenv.mod, LEVEL_1, "created node %+F[%p]\n", asm_node, asm_node));
	}
	else {
		DB((tenv.mod, LEVEL_1, "ignored\n"));
	}
#undef OTHER_GEN
}
