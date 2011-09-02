/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Lowering of Calls with compound parameters and return types.
 * @author  Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <stdbool.h>

#include "lower_calls.h"
#include "lowering.h"
#include "irprog_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "irmode_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irtools.h"
#include "iroptimize.h"
#include "array_t.h"
#include "pmap.h"
#include "error.h"

static pmap *pointer_types;
static pmap *lowered_mtps;

/**
 * Default implementation for finding a pointer type for a given element type.
 * Simple create a new one.
 */
static ir_type *get_pointer_type(ir_type *dest_type)
{
	ir_type *res = (ir_type*)pmap_get(pointer_types, dest_type);
	if (res == NULL) {
		res = new_type_pointer(dest_type);
		pmap_insert(pointer_types, dest_type, res);
	}
	return res;
}

static void fix_parameter_entities(ir_graph *irg, size_t n_compound_ret)
{
	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n_compound = get_compound_n_members(frame_type);
	size_t   i;

	if (n_compound_ret == 0)
		return;

	for (i = 0; i < n_compound; ++i) {
		ir_entity *member = get_compound_member(frame_type, i);
		size_t     num;
		if (!is_parameter_entity(member))
			continue;

		/* increase parameter number since we added a new parameter in front */
		num = get_entity_parameter_number(member);
		if (num == IR_VA_START_PARAMETER_NUMBER)
			continue;
		set_entity_parameter_number(member, num + n_compound_ret);
	}
}

/**
 * Creates a new lowered type for a method type with compound
 * arguments. The new type is associated to the old one and returned.
 */
static ir_type *lower_mtp(compound_call_lowering_flags flags, ir_type *mtp)
{
	bool      must_be_lowered = false;
	ir_type  *lowered;
	ir_type **params;
	ir_type **results;
	size_t    n_ress;
	size_t    n_params;
	size_t    nn_ress;
	size_t    nn_params;
	size_t    i;

	if (!is_Method_type(mtp))
		return mtp;

	lowered = (ir_type*)pmap_get(lowered_mtps, mtp);
	if (lowered != NULL)
		return lowered;

	/* check if the type has to be lowered at all */
	n_ress = get_method_n_ress(mtp);
	for (i = 0; i < n_ress; ++i) {
		ir_type *res_tp = get_method_res_type(mtp, i);
		if (is_compound_type(res_tp)) {
			must_be_lowered = true;
			break;
		}
	}
	if (!must_be_lowered)
		return mtp;

	n_params  = get_method_n_params(mtp);
	results   = ALLOCANZ(ir_type*, n_ress);
	params    = ALLOCANZ(ir_type*, n_params + n_ress);
	nn_ress   = 0;
	nn_params = 0;

	/* add a hidden parameter in front for every compound result */
	for (i = 0; i < n_ress; ++i) {
		ir_type *res_tp = get_method_res_type(mtp, i);

		if (is_compound_type(res_tp)) {
			/* this compound will be allocated on callers stack and its
			   address will be transmitted as a hidden parameter. */
			ir_type *ptr_tp = get_pointer_type(res_tp);
			params[nn_params++] = ptr_tp;
			if (flags & LF_RETURN_HIDDEN)
				results[nn_ress++] = ptr_tp;
		} else {
			/* scalar result */
			results[nn_ress++] = res_tp;
		}
	}
	/* copy over parameter types */
	for (i = 0; i < n_params; ++i) {
		params[nn_params++] = get_method_param_type(mtp, i);
	}
	assert(nn_ress <= n_ress);
	assert(nn_params <= n_params + n_ress);

	/* create the new type */
	lowered = new_d_type_method(nn_params, nn_ress, get_type_dbg_info(mtp));
	lowered->attr.ma.has_compound_ret_parameter = true;

	/* fill it */
	for (i = 0; i < nn_params; ++i)
		set_method_param_type(lowered, i, params[i]);
	for (i = 0; i < nn_ress; ++i)
		set_method_res_type(lowered, i, results[i]);

	set_method_variadicity(lowered, get_method_variadicity(mtp));

	/* associate the lowered type with the original one for easier access */
	set_method_calling_convention(lowered, get_method_calling_convention(mtp) | cc_compound_ret);
	set_method_additional_properties(lowered, get_method_additional_properties(mtp));

	set_lowered_type(mtp, lowered);
	pmap_insert(lowered_mtps, mtp, lowered);

	return lowered;
}

/**
 * A call list entry.
 */
typedef struct cl_entry cl_entry;
struct cl_entry {
	cl_entry *next;   /**< Pointer to the next entry. */
	ir_node  *call;   /**< Pointer to the Call node. */
	ir_node  *copyb;  /**< List of all CopyB nodes. */
};

/**
 * Walker environment for fix_args_and_collect_calls().
 */
typedef struct wlk_env_t {
	size_t               arg_shift;        /**< The Argument index shift for parameters. */
	struct obstack       obst;             /**< An obstack to allocate the data on. */
	cl_entry             *cl_list;         /**< The call list. */
	pmap                 *dummy_map;       /**< A map for finding the dummy arguments. */
	compound_call_lowering_flags flags;
	ir_type              *lowered_mtp;     /**< The lowered method type of the current irg if any. */
	unsigned             only_local_mem:1; /**< Set if only local memory access was found. */
	unsigned             changed:1;        /**< Set if the current graph was changed. */
} wlk_env;

/**
 * Return the call list entry of a call node.
 * If no entry exists yet, allocate one and enter the node into
 * the call list of the environment.
 *
 * @param call   A Call node.
 * @param env    The environment.
 */
static cl_entry *get_Call_entry(ir_node *call, wlk_env *env)
{
	cl_entry *res = (cl_entry*)get_irn_link(call);
	if (res == NULL) {
		res = OALLOC(&env->obst, cl_entry);
		res->next  = env->cl_list;
		res->call  = call;
		res->copyb = NULL;
		set_irn_link(call, res);
		env->cl_list = res;
	}
	return res;
}

/**
 * Finds the base address of an address by skipping Sel's and address
 * calculation.
 *
 * @param adr   the address
 * @param pEnt  points to the base entity if any
 */
static ir_node *find_base_adr(ir_node *ptr, ir_entity **pEnt)
{
	ir_entity *ent = NULL;
	assert(mode_is_reference(get_irn_mode(ptr)));

	for (;;) {
		if (is_Sel(ptr)) {
			ent = get_Sel_entity(ptr);
			ptr = get_Sel_ptr(ptr);
		}
		else if (is_Add(ptr)) {
			ir_node *left = get_Add_left(ptr);
			if (mode_is_reference(get_irn_mode(left)))
				ptr = left;
			else
				ptr = get_Add_right(ptr);
			ent = NULL;
		} else if (is_Sub(ptr)) {
			ptr = get_Sub_left(ptr);
			ent = NULL;
		} else {
			*pEnt = ent;
			return ptr;
		}
	}
}

/**
 * Check if a given pointer represents non-local memory.
 */
static void check_ptr(ir_node *ptr, wlk_env *env)
{
	ir_storage_class_class_t sc;
	ir_entity                *ent;

	/* still alias free */
	ptr = find_base_adr(ptr, &ent);
	sc  = get_base_sc(classify_pointer(ptr, ent));
	if (sc != ir_sc_localvar && sc != ir_sc_malloced) {
		/* non-local memory access */
		env->only_local_mem = 0;
	}
}

/*
 * Returns non-zero if a Call is surely a self-recursive Call.
 * Beware: if this functions returns 0, the call might be self-recursive!
 */
static bool is_self_recursive_Call(const ir_node *call)
{
	const ir_node *callee = get_Call_ptr(call);

	if (is_SymConst_addr_ent(callee)) {
		const ir_entity *ent = get_SymConst_entity(callee);
		const ir_graph  *irg = get_entity_irg(ent);
		if (irg == get_irn_irg(call))
			return 1;
	}
	return 0;
}

/**
 * Post walker: shift all parameter indexes
 * and collect Calls with compound returns in the call list.
 * If a non-alias free memory access is found, reset the alias free
 * flag.
 */
static void fix_args_and_collect_calls(ir_node *n, void *ctx)
{
	wlk_env *env = (wlk_env*)ctx;
	ir_type *ctp;
	ir_node *ptr;

	switch (get_irn_opcode(n)) {
	case iro_Load:
	case iro_Store:
		if (env->only_local_mem) {
			ptr = get_irn_n(n, 1);
			check_ptr(ptr, env);
		}
		break;
	case iro_Proj:
		if (env->arg_shift > 0) {
			ir_node *pred = get_Proj_pred(n);
			ir_graph *irg = get_irn_irg(n);

			/* Fix the argument numbers */
			if (pred == get_irg_args(irg)) {
				long pnr = get_Proj_proj(n);
				set_Proj_proj(n, pnr + env->arg_shift);
				env->changed = 1;
			}
		}
		break;
	case iro_Call: {
		size_t i;
		size_t n_res;
		if (! is_self_recursive_Call(n)) {
			/* any non self recursive call might access global memory */
			env->only_local_mem = 0;
		}

		ctp = get_Call_type(n);
		/* check for compound returns */
		for (i = 0, n_res = get_method_n_ress(ctp); i < n_res; ++i) {
			if (is_compound_type(get_method_res_type(ctp, i))) {
				/*
				 * This is a call with a compound return. As the result
				 * might be ignored, we must put it in the list.
				 */
				(void)get_Call_entry(n, env);
				break;
			}
		}
		break;
	}
	case iro_CopyB: {
		ir_node *src = get_CopyB_src(n);
		if (env->only_local_mem) {
			check_ptr(get_CopyB_src(n), env);
			if (env->only_local_mem)
				check_ptr(get_CopyB_dst(n), env);
		}
		/* check for compound returns */
		if (is_Proj(src)) {
			ir_node *proj = get_Proj_pred(src);
			if (is_Proj(proj) && get_Proj_proj(proj) == pn_Call_T_result) {
				ir_node *call = get_Proj_pred(proj);
				if (is_Call(call)) {
					ctp = get_Call_type(call);
					if (is_compound_type(get_method_res_type(ctp, get_Proj_proj(src)))) {
						/* found a CopyB from compound Call result */
						cl_entry *e = get_Call_entry(call, env);
						set_irn_link(n, e->copyb);
						e->copyb = n;
					}
				}
			}
		}
		break;
	}
	case iro_Sel: {
		ir_entity *ent  = get_Sel_entity(n);
		ir_type   *type = get_entity_type(ent);

		/* we need to copy compound parameters */
		if (is_parameter_entity(ent) && is_compound_type(type)) {
			env->only_local_mem = 0;
		}
		break;
	}
	default:
		/* do nothing */
		break;
	}
}

/**
 * Returns non-zero if a node is a compound address
 * of a frame-type entity.
 *
 * @param ft   the frame type
 * @param adr  the node
 */
static bool is_compound_address(ir_type *ft, ir_node *adr)
{
	ir_entity *ent;

	if (! is_Sel(adr))
		return false;
	if (get_Sel_n_indexs(adr) != 0)
		return false;
	ent = get_Sel_entity(adr);
	return get_entity_owner(ent) == ft;
}

/** A pair for the copy-return-optimization. */
typedef struct cr_pair {
	ir_entity *ent; /**< the entity than can be removed from the frame */
	ir_node *arg;   /**< the argument that replaces the entities address */
} cr_pair;

/**
 * Post walker: fixes all entities addresses for the copy-return
 * optimization.
 *
 * Note: We expect the length of the cr_pair array (i.e. number of compound
 * return values) to be 1 (C, C++) in almost all cases, so ignore the
 * linear search complexity here.
 */
static void do_copy_return_opt(ir_node *n, void *ctx)
{
	if (is_Sel(n)) {
		ir_entity *ent = get_Sel_entity(n);
		cr_pair   *arr = (cr_pair*)ctx;
		size_t    i, l;

		for (i = 0, l = ARR_LEN(arr); i < l; ++i) {
			if (ent == arr[i].ent) {
				exchange(n, arr[i].arg);
				break;
			}
		}
	}
}

/**
 * Return a Sel node that selects a dummy argument of type tp.
 * Dummy arguments are only needed once and we use a map
 * to store them.
 * We could even assign all dummy arguments the same offset
 * in the frame type ...
 *
 * @param irg    the graph
 * @param block  the block where a newly create Sel should be placed
 * @param tp     the type of the dummy entity that should be create
 * @param env    the environment
 */
static ir_node *get_dummy_sel(ir_graph *irg, ir_node *block, ir_type *tp,
                              wlk_env *env)
{
	ir_entity *ent;
	pmap_entry *e;

	/* use a map the check if we already create such an entity */
	e = pmap_find(env->dummy_map, tp);
	if (e) {
		ent = (ir_entity*)e->value;
	} else {
		ir_type *ft = get_irg_frame_type(irg);
		ident   *dummy_id = id_unique("dummy.%u");
		ent = new_entity(ft, dummy_id, tp);
		pmap_insert(env->dummy_map, tp, ent);

		if (get_type_state(ft) == layout_fixed) {
			/* Fix the layout again */
			panic("Fixed layout not implemented");
		}
	}
	return new_r_simpleSel(block, get_irg_no_mem(irg), get_irg_frame(irg), ent);
}

/**
 * Add the hidden parameter from the CopyB node to the Call node.
 *
 * @param irg    the graph
 * @param n_com  number of compound results (will be number of hidden parameters)
 * @param ins    in array to store the hidden parameters into
 * @param entry  the call list
 * @param env    the environment
 */
static void add_hidden_param(ir_graph *irg, size_t n_com, ir_node **ins,
                             cl_entry *entry, wlk_env *env)
{
	ir_node *p, *n, *mem, *blk;
	size_t n_args;

	n_args = 0;
	for (p = entry->copyb; p; p = n) {
		ir_node *src = get_CopyB_src(p);
		size_t   idx = get_Proj_proj(src);
		n = (ir_node*)get_irn_link(p);

		ins[idx] = get_CopyB_dst(p);
		mem      = get_CopyB_mem(p);
		blk      = get_nodes_block(p);

		/* get rid of the CopyB */
		turn_into_tuple(p, pn_CopyB_max+1);
		set_Tuple_pred(p, pn_CopyB_M,         mem);
		set_Tuple_pred(p, pn_CopyB_X_regular, new_r_Jmp(blk));
		set_Tuple_pred(p, pn_CopyB_X_except,  new_r_Bad(irg, mode_X));
		++n_args;
	}

	/* now create dummy entities for function with ignored return value */
	if (n_args < n_com) {
		ir_type *ctp = get_Call_type(entry->call);
		size_t   i;
		size_t   j;

		if (is_lowered_type(ctp))
			ctp = get_associated_type(ctp);

		for (j = i = 0; i < get_method_n_ress(ctp); ++i) {
			ir_type *rtp = get_method_res_type(ctp, i);
			if (is_compound_type(rtp)) {
				if (ins[j] == NULL)
					ins[j] = get_dummy_sel(irg, get_nodes_block(entry->call), rtp, env);
				++j;
			}
		}
	}
}

/**
 * Fix all calls on a call list by adding hidden parameters.
 *
 * @param irg  the graph
 * @param env  the environment
 */
static void fix_call_list(ir_graph *irg, wlk_env *env)
{
	cl_entry *p;
	ir_node *call, **new_in;
	ir_type *ctp, *lowered_mtp;
	size_t i, n_res, n_params, n_com, pos;

	new_in = NEW_ARR_F(ir_node *, 0);
	for (p = env->cl_list; p; p = p->next) {
		call = p->call;
		ctp = get_Call_type(call);
		lowered_mtp = lower_mtp(env->flags, ctp);
		set_Call_type(call, lowered_mtp);

		n_params = get_Call_n_params(call);

		n_com = 0;
		for (i = 0, n_res = get_method_n_ress(ctp); i < n_res; ++i) {
			if (is_compound_type(get_method_res_type(ctp, i)))
				++n_com;
		}
		pos = 2;
		ARR_RESIZE(ir_node *, new_in, n_params + n_com + pos);
		memset(new_in, 0, sizeof(*new_in) * (n_params + n_com + pos));
		add_hidden_param(irg, n_com, &new_in[pos], p, env);
		pos += n_com;
		/* copy all other parameters */
		for (i = 0; i < n_params; ++i)
			new_in[pos++] = get_Call_param(call, i);
		new_in[0] = get_Call_mem(call);
		new_in[1] = get_Call_ptr(call);

		set_irn_in(call, n_params + n_com + 2, new_in);
	}
}

/**
 * Transform a graph. If it has compound parameter returns,
 * remove them and use the hidden parameter instead.
 * If it calls methods with compound parameter returns, add hidden
 * parameters.
 *
 * @param irg  the graph to transform
 */
static void transform_irg(compound_call_lowering_flags flags, ir_graph *irg)
{
	ir_entity  *ent = get_irg_entity(irg);
	ir_type    *mtp, *lowered_mtp, *tp, *ft;
	size_t     i, j, k, n_ress = 0, n_ret_com = 0;
	size_t     n_cr_opt;
	ir_node    **new_in, *ret, *endbl, *bl, *mem, *copy;
	cr_pair    *cr_opt;
	wlk_env    env;

	mtp = get_entity_type(ent);

	/* calculate the number of compound returns */
	n_ress = get_method_n_ress(mtp);
	for (n_ret_com = i = 0; i < n_ress; ++i) {
		tp = get_method_res_type(mtp, i);

		if (is_compound_type(tp))
			++n_ret_com;
	}

	fix_parameter_entities(irg, n_ret_com);

	if (n_ret_com) {
		/* much easier if we have only one return */
		normalize_one_return(irg);

		/* This graph has a compound argument. Create a new type */
		lowered_mtp = lower_mtp(flags, mtp);
		set_entity_type(ent, lowered_mtp);

		/* hidden arguments are added first */
		env.arg_shift    = n_ret_com;
	} else {
		/* we must only search for calls */
		env.arg_shift = 0;
		lowered_mtp   = NULL;
	}
	obstack_init(&env.obst);
	env.cl_list        = NULL;
	env.dummy_map      = pmap_create_ex(8);
	env.flags          = flags;
	env.lowered_mtp    = lowered_mtp;
	env.only_local_mem = 1;
	env.changed        = 0;

	/* scan the code, fix argument numbers and collect calls. */
	irg_walk_graph(irg, firm_clear_link, fix_args_and_collect_calls, &env);

	/* fix all calls */
	if (env.cl_list) {
		fix_call_list(irg, &env);
		env.changed = 1;
	}

	if (n_ret_com) {
		int idx;

		/* STEP 1: find the return. This is simple, we have normalized the graph. */
		endbl = get_irg_end_block(irg);
		ret = NULL;
		for (idx = get_Block_n_cfgpreds(endbl) - 1; idx >= 0; --idx) {
			ir_node *pred = get_Block_cfgpred(endbl, idx);

			if (is_Return(pred)) {
				ret = pred;
				break;
			}
		}

		/* in case of infinite loops, there might be no return */
		if (ret != NULL) {
			/*
			 * Now fix the Return node of the current graph.
			 */
			env.changed = 1;

			/*
			 * STEP 2: fix it. For all compound return values add a CopyB,
			 * all others are copied.
			 */
			NEW_ARR_A(ir_node *, new_in, n_ress + 1);

			bl  = get_nodes_block(ret);
			mem = get_Return_mem(ret);

			ft = get_irg_frame_type(irg);
			NEW_ARR_A(cr_pair, cr_opt, n_ret_com);
			n_cr_opt = 0;
			for (j = 1, i = k = 0; i < n_ress; ++i) {
				ir_node *pred = get_Return_res(ret, i);
				tp = get_method_res_type(mtp, i);

				if (is_compound_type(tp)) {
					ir_node *arg = get_irg_args(irg);
					arg = new_r_Proj(arg, mode_P_data, k);
					++k;

					if (is_Unknown(pred)) {
						/* The Return(Unknown) is the Firm construct for a
						 * missing return. Do nothing. */
					} else {
						/**
						 * Sorrily detecting that copy-return is possible isn't
						 * that simple. We must check, that the hidden address
						 * is alias free during the whole function.
						 * A simple heuristic: all Loads/Stores inside
						 * the function access only local frame.
						 */
						if (env.only_local_mem && is_compound_address(ft, pred)) {
							/* we can do the copy-return optimization here */
							cr_opt[n_cr_opt].ent = get_Sel_entity(pred);
							cr_opt[n_cr_opt].arg = arg;
							++n_cr_opt;
						} else { /* copy-return optimization is impossible, do the copy. */
							copy = new_r_CopyB(
									bl,
									mem,
									arg,
									pred,
									tp
									);
							mem = new_r_Proj(copy, mode_M, pn_CopyB_M);
						}
					}
					if (flags & LF_RETURN_HIDDEN) {
						new_in[j] = arg;
						++j;
					}
				} else { /* scalar return value */
					new_in[j] = pred;
					++j;
				}
			}
			/* replace the in of the Return */
			new_in[0] = mem;
			set_irn_in(ret, j, new_in);

			if (n_cr_opt > 0) {
				size_t c;
				size_t n;

				irg_walk_graph(irg, NULL, do_copy_return_opt, cr_opt);

				for (c = 0, n = ARR_LEN(cr_opt); c < n; ++c) {
					free_entity(cr_opt[c].ent);
				}
			}
		}
	}

	pmap_destroy(env.dummy_map);
	obstack_free(&env.obst, NULL);
}

static void lower_method_types(type_or_ent tore, void *env)
{
	const compound_call_lowering_flags *flags
		= (const compound_call_lowering_flags*)env;

	/* fix method entities */
	if (is_entity(tore.ent)) {
		ir_entity *ent     = tore.ent;
		ir_type   *tp      = get_entity_type(ent);
		ir_type   *lowered = lower_mtp(*flags, tp);
		set_entity_type(ent, lowered);
	} else {
		ir_type *tp = tore.typ;

		/* fix pointer to methods */
		if (is_Pointer_type(tp)) {
			ir_type *points_to         = get_pointer_points_to_type(tp);
			ir_type *lowered_points_to = lower_mtp(*flags, points_to);
			set_pointer_points_to_type(tp, lowered_points_to);
		}
	}
}

void lower_calls_with_compounds(compound_call_lowering_flags flags)
{
	size_t i, n;

	pointer_types = pmap_create();
	lowered_mtps = pmap_create();

	/* first step: Transform all graphs */
	for (i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);
		transform_irg(flags, irg);
	}

	/* second step: Lower all method types of visible entities */
	type_walk(NULL, lower_method_types, &flags);

	pmap_destroy(lowered_mtps);
	pmap_destroy(pointer_types);
}
