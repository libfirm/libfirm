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
 * @brief   The codegenerator (transform FIRM Conv nodes into ppc FIRM)
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
#include "iropt_t.h"
#include "debug.h"

#include "../benode_t.h"
#include "bearch_ppc32_t.h"

#include "ppc32_nodes_attr.h"
//#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "ppc32_transform_conv.h"
#include "ppc32_new_nodes.h"
#include "ppc32_map_regs.h"

#include "gen_ppc32_regalloc_if.h"

typedef struct
{
	ir_node *first_conv;
	ir_node **convs;
	int conv_count;
} cw_block_attr;


ir_node *current_block;
int conv_nodes_found;
ir_entity *memslot;
ir_node *memory;

/**
 * Conv walker initialization
 */
void ppc32_init_conv_walk(void)
{
	current_block = NULL;
	conv_nodes_found = 0;
	memslot = NULL;
}

static ir_node *own_gen_convert_call(ppc32_transform_env_t *env, ir_node *op, const char *funcname,
									 ir_mode *from_mode, ir_mode *to_mode)
{
	ir_type *method_type;
	ir_entity *method_ent;
	ir_node *in[1];
	ir_node *call, *callee, *call_results;

	in[0] = op;

	method_type = new_type_method(new_id_from_str("convert_call_type"), 1, 1);
	set_method_param_type(method_type, 0, new_type_primitive(new_id_from_str("conv_param"), from_mode));
	set_method_res_type(method_type, 0, new_type_primitive(new_id_from_str("conv_result"), to_mode));

	method_ent   = new_entity(get_glob_type(), new_id_from_str(funcname), method_type);
	callee       = new_rd_SymConst_addr_ent(env->dbg, env->irg, method_ent, method_type);
	call         = new_rd_Call(env->dbg, env->irg, env->block, memory, callee, 1, in, method_type);
	call_results = new_rd_Proj(env->dbg, env->irg, env->block, call, mode_T, pn_Call_T_result);
	memory       = new_rd_Proj(env->dbg, env->irg, env->block, call, mode_M, pn_Call_M_regular);

	return new_rd_Proj(env->dbg, env->irg, env->block, call_results, to_mode, 0);
}

/**
 * Transforms a Conv node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Conv node
 * @param op      operator
 * @param mode    node mode
 * @return the created ppc Conv node
 */
static ir_node *gen_Conv(ppc32_transform_env_t *env, ir_node *op) {
	ir_mode *from_mode = get_irn_mode(get_irn_n(env->irn,0));
	ir_mode *to_mode = env->mode;
	modecode from_modecode=get_mode_modecode(from_mode);
	modecode to_modecode=get_mode_modecode(to_mode);

	switch(from_modecode){
		case irm_F:
			op = new_rd_Conv(env->dbg, env->irg, env->block, op, mode_D);
			// fall through
		case irm_D:
		{
			ir_node *res;
			if (mode_is_signed(to_mode))  // Float to integer
			{
				ir_node *fctiw = new_rd_ppc32_fCtiw(env->dbg, env->irg, env->block, op, from_mode);
				ir_node *stfd = new_rd_ppc32_Stfd(env->dbg, env->irg, env->block, get_irg_frame(env->irg),
					fctiw, memory);
				ir_node *storememproj = new_rd_Proj(env->dbg, env->irg, env->block, stfd, mode_M, pn_Store_M);
				ir_node *lwz = new_rd_ppc32_Lwz(env->dbg, env->irg, env->block, get_irg_frame(env->irg),
					storememproj);
				set_ppc32_frame_entity(stfd, memslot);
				set_ppc32_offset_mode(stfd, ppc32_ao_Lo16);	// TODO: only allows a 16-bit offset on stack
				set_ppc32_frame_entity(lwz, memslot);
				set_ppc32_offset_mode(stfd, ppc32_ao_Lo16);	// TODO: only allows a 16-bit offset on stack
				memory = new_rd_Proj(env->dbg, env->irg, env->block, lwz, mode_M, pn_Store_M);
				res = new_rd_Proj(env->dbg, env->irg, env->block, lwz, to_mode, pn_Load_res);

			}
			else
			{
				res = own_gen_convert_call(env, op, "conv_double_to_unsigned_int", mode_D, mode_Iu);
			}

			switch(to_modecode)
			{
				case irm_Bs:
				case irm_Hs:
				case irm_Bu:
				case irm_Hu:
					return new_rd_Conv(env->dbg, env->irg, env->block, res, to_mode);
				case irm_Is:
				case irm_Iu:
					return res;
				default:
					break;
			}
			break;
		}
		case irm_Hs:
		case irm_Bs:
			op = new_rd_Conv(env->dbg, env->irg, env->block, op, mode_Is);
		case irm_Is:
			return own_gen_convert_call(env, op, (to_mode == mode_D) ? "conv_int_to_double" : "conv_int_to_single", mode_Is, to_mode);


		case irm_Hu:
		case irm_Bu:
			op = new_rd_Conv(env->dbg, env->irg, env->block, op, mode_Iu);
		case irm_Iu:
			return own_gen_convert_call(env, op, (to_mode == mode_D) ? "conv_unsigned_int_to_double": "conv_unsigned_int_to_single", mode_Iu, to_mode);

		case irm_P:
			break;

		default:
			break;
	}
	fprintf(stderr, "Mode for Conv not supported: %s -> %s\n", get_mode_name(from_mode), get_mode_name(to_mode));
	assert(0);
	return 0;

	// return op;
}

int search_from_node_in_block(ir_node *from, ir_node *to)
{
	int n = get_irn_arity(from), i;
	for(i=0;i<n;i++)
	{
		ir_node *pred = get_irn_n(from, i);
		if(pred==to) return 1;
		if(get_irn_n(pred, -1)==current_block)
		{
			if(search_from_node_in_block(pred, to)) return 1;
		}
	}
	return 0;
}

int nodes_dependency_order(ir_node **a, ir_node **b)
{
	if(search_from_node_in_block(*a,*b)) return 1;
	if(search_from_node_in_block(*b,*a)) return -1;
	return 0;
}

void finalize_block(ppc32_code_gen_t *cgenv)
{
	int i;
	ir_node *current_conv;
	cw_block_attr *attr = current_block->link;
	ppc32_transform_env_t tenv;

	if(!attr->conv_count) return;

	if(!memslot)
	{
		ir_type *frame_type = get_irg_frame_type(cgenv->irg);
		memslot = frame_alloc_area(frame_type, get_mode_size_bytes(mode_D), 4, 0);
	}

	attr->convs = xmalloc(attr->conv_count * sizeof(ir_node *));

	for (i = 0, current_conv = attr->first_conv; i < attr->conv_count; i++, current_conv = current_conv->link)
	{
		attr->convs[i] = current_conv;
	}

	qsort(attr->convs, attr->conv_count, sizeof(ir_node *),
		(int (*)(const void *, const void *)) nodes_dependency_order);

	tenv.block    = current_block;
	tenv.irg      = current_ir_graph;
	DEBUG_ONLY(tenv.mod      = cgenv->mod;)

	memory = get_irg_no_mem(current_ir_graph);
	for(i = 0; i < attr->conv_count; i++)
	{
		tenv.dbg      = get_irn_dbg_info(attr->convs[i]);
		tenv.irn      = attr->convs[i];
		tenv.mode     = get_irn_mode(attr->convs[i]);

		exchange(attr->convs[i], gen_Conv(&tenv, get_Conv_op(attr->convs[i])));
	}
}

void init_block(void)
{
	cw_block_attr *attr = xmalloc(sizeof(cw_block_attr));
	attr->first_conv    = NULL;
	attr->convs         = NULL; /* attr->convs is set in finalize_block() */
	attr->conv_count    = 0;
	current_block->link = attr;
}

/**
 * Constant generating code
 */

#if 0
struct tv_ent {
	ir_entity *ent;
	tarval *tv;
};

/* Compares two (entity, tarval) combinations */
static int cmp_tv_ent(const void *a, const void *b, size_t len) {
	const struct tv_ent *e1 = a;
	const struct tv_ent *e2 = b;

	return !(e1->tv == e2->tv);
}

/* Generates a SymConst node for a known FP const */
static ir_node *gen_fp_known_symconst(ppc32_transform_env_t *env, tarval *known_const) {
	static set    *const_set = NULL;
	static ir_type *tp = NULL;
	struct tv_ent  key;
	struct tv_ent *entry;
	ir_node       *cnst;
	ir_graph      *rem;
	ir_entity     *ent;

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
	}

	return new_rd_SymConst_addr_ent(env->dbg, env->irg, ent, tp);
}
#endif

/**
 * Transforms a Const
 *
 * @param env transformation environment
 * @return the created ppc Const node
 */
static ir_node *gen_Const(ppc32_transform_env_t *env) {
	tarval *tv_const = get_Const_tarval(env->irn);
	ir_node *constant;

	if (mode_is_float(env->mode))
		constant = new_rd_ppc32_fConst(env->dbg, env->irg, env->block, env->mode);
	else
		constant = new_rd_ppc32_Const(env->dbg, env->irg, env->block, env->mode);
	set_ppc32_constant_tarval(constant, tv_const);
	return constant;
}

/**
 * Transforms a SymConst.
 *
 * @param env transformation environment
 * @return the created ppc SymConst node
 */
static ir_node *gen_SymConst(ppc32_transform_env_t *env) {
	ir_node *symconst;
	symconst = new_rd_ppc32_SymConst(env->dbg, env->irg, env->block, env->mode);
	set_ppc32_frame_entity(symconst, get_SymConst_entity(env->irn));
	return symconst;
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
 * Transforms all conv nodes into ppc convs before abi
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ppc32_conv_walk(ir_node *node, void *env) {
	ppc32_code_gen_t *cgenv = (ppc32_code_gen_t *)env;
	ir_opcode  code         = get_irn_opcode(node);
	ppc32_transform_env_t tenv;

	if (is_Block(node))
	{
		if(current_block != NULL)
			finalize_block(cgenv);

		current_block = node;
		init_block();

		return;
	}

	tenv.irg = current_ir_graph;
	DEBUG_ONLY(tenv.mod = cgenv->mod;)

	if (code == iro_Conv)
	{
		modecode from_mode=get_mode_modecode(get_irn_mode(get_irn_n(node,0)));
		modecode to_mode=get_mode_modecode(get_irn_mode(node));
		cw_block_attr *attr;

		if(from_mode == to_mode) return;
		if(from_mode == irm_F || from_mode == irm_D)
		{
			switch(to_mode)
			{
				case irm_Bs:
				case irm_Bu:
				case irm_Hs:
				case irm_Hu:
				case irm_Is:
				case irm_Iu:
					break;
				default:
					return;

			}
		}
		else if(to_mode == irm_F || to_mode == irm_D)
		{
			switch(from_mode)
			{
				case irm_Bs:
				case irm_Bu:
				case irm_Hs:
				case irm_Hu:
				case irm_Is:
				case irm_Iu:
					break;
				default:
					return;
			}
		}
		else return;

		/* in Liste eintragen */
		attr = get_irn_link(current_block);
		set_irn_link(node, attr->first_conv);
		attr->first_conv = node;
		attr->conv_count++;
		conv_nodes_found++;
	}
	else if (code == iro_Call) {
		int i, size = 0;
		ir_type *tp = get_Call_type(node);
		ir_type *ptp;
		int stack_alignment = 4;

		for (i = get_Call_n_params(node) - 1; i >= 0; --i) {
			ir_mode *mode = get_irn_mode(get_Call_param(node, i));
			int s;

			if (mode_is_reference(mode)) {
				/* might be a compound parameter */
				ptp = get_method_param_type(tp, i);

				if (is_compound_type(ptp)) {
					s = (get_type_size_bytes(ptp) + stack_alignment - 1) & -stack_alignment;

					size += s;
					continue;
				}
			}
			s = (get_mode_size_bytes(mode) + stack_alignment - 1) & -stack_alignment;
			size += s;
		}
		if ((unsigned) size > cgenv->area_size)
			cgenv->area_size = size;
	}
}

/**
 * Transforms all const nodes into ppc const nodes inside the using block
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ppc32_pretransform_walk(ir_node *node, void *env) {
	ppc32_code_gen_t *cgenv = (ppc32_code_gen_t *)env;
	ir_opcode  code         = get_irn_opcode(node);
	ppc32_transform_env_t tenv;

	if (is_Block(node))
	{
		current_block = node;
		return;
	}

	tenv.irg = current_ir_graph;
	DEBUG_ONLY(tenv.mod = cgenv->mod;)

	if(code == iro_Const || code == iro_SymConst)
	{
		ir_node *newconst;

		tenv.block    = cgenv->start_succ_block;
		tenv.irn      = node;
		tenv.mode     = get_irn_mode(node);
		tenv.dbg      = get_irn_dbg_info(node);

		if(code == iro_Const)
			newconst = gen_Const(&tenv);
		else
			newconst = gen_SymConst(&tenv);

		exchange(node, newconst);
	}
}
