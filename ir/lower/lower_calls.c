/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lowering of Calls with compound parameters and return types.
 * @author  Michael Beck, Matthias Braun
 */
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
#include "util.h"

static pmap *pointer_types;
static pmap *lowered_mtps;

static bool needs_lowering(const ir_type *type)
{
	return is_compound_type(type) || is_Array_type(type);
}

/**
 * Default implementation for finding a pointer type for a given element type.
 * Simply create a new one.
 */
static ir_type *get_pointer_type(ir_type *dest_type)
{
	ir_type *res = pmap_get(ir_type, pointer_types, dest_type);
	if (res == NULL) {
		res = new_type_pointer(dest_type);
		pmap_insert(pointer_types, dest_type, res);
	}
	return res;
}

static void fix_parameter_entities(ir_graph *irg, size_t n_compound_ret)
{
	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n_members  = get_compound_n_members(frame_type);
	size_t   i;

	for (i = 0; i < n_members; ++i) {
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

static void remove_compound_param_entities(ir_graph *irg)
{
	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n_members  = get_compound_n_members(frame_type);
	size_t   i;

	for (i = n_members; i > 0; ) {
		ir_entity *member = get_compound_member(frame_type, --i);
		ir_type   *type;
		if (!is_parameter_entity(member))
			continue;

		type = get_entity_type(member);
		if (needs_lowering(type)) {
			free_entity(member);
		}
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
	unsigned  cconv;
	mtp_additional_properties mtp_properties;

	if (!is_Method_type(mtp))
		return mtp;

	lowered = pmap_get(ir_type, lowered_mtps, mtp);
	if (lowered != NULL)
		return lowered;

	/* check if the type has to be lowered at all */
	n_params = get_method_n_params(mtp);
	n_ress   = get_method_n_ress(mtp);
	for (i = 0; i < n_ress; ++i) {
		ir_type *res_tp = get_method_res_type(mtp, i);
		if (needs_lowering(res_tp)) {
			must_be_lowered = true;
			break;
		}
	}
	if (!must_be_lowered && !(flags & LF_DONT_LOWER_ARGUMENTS)) {
		for (i = 0; i < n_params; ++i) {
			ir_type *param_type = get_method_param_type(mtp, i);
			if (needs_lowering(param_type)) {
				must_be_lowered = true;
				break;
			}
		}
	}
	if (!must_be_lowered)
		return mtp;

	results   = ALLOCANZ(ir_type*, n_ress);
	params    = ALLOCANZ(ir_type*, n_params + n_ress);
	nn_ress   = 0;
	nn_params = 0;

	/* add a hidden parameter in front for every compound result */
	for (i = 0; i < n_ress; ++i) {
		ir_type *res_tp = get_method_res_type(mtp, i);

		if (needs_lowering(res_tp)) {
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
		ir_type *param_type = get_method_param_type(mtp, i);
		if (! (flags & LF_DONT_LOWER_ARGUMENTS) && needs_lowering(param_type)) {
		    /* turn parameter into a pointer type */
		    param_type = new_type_pointer(param_type);
		}
		params[nn_params++] = param_type;
	}
	assert(nn_ress <= n_ress);
	assert(nn_params <= n_params + n_ress);

	/* create the new type */
	lowered = new_d_type_method(nn_params, nn_ress, get_type_dbg_info(mtp));

	/* fill it */
	for (i = 0; i < nn_params; ++i)
		set_method_param_type(lowered, i, params[i]);
	for (i = 0; i < nn_ress; ++i)
		set_method_res_type(lowered, i, results[i]);

	set_method_variadicity(lowered, get_method_variadicity(mtp));

	cconv = get_method_calling_convention(mtp);
	if (nn_params > n_params) {
		cconv |= cc_compound_ret;
	}
	set_method_calling_convention(lowered, cconv);

	mtp_properties = get_method_additional_properties(mtp);
	/* after lowering the call is not const anymore, since it writes to the
	 * memory for the return value passed to it */
	mtp_properties &= ~mtp_property_const;
	set_method_additional_properties(lowered, mtp_properties);

	/* associate the lowered type with the original one for easier access */
	set_higher_type(lowered, mtp);
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
	bool      has_compound_ret   : 1;
	bool      has_compound_param : 1;
};

/**
 * Walker environment for fix_args_and_collect_calls().
 */
typedef struct wlk_env_t {
	size_t               arg_shift;        /**< The Argument index shift for parameters. */
	struct obstack       obst;             /**< An obstack to allocate the data on. */
	cl_entry             *cl_list;         /**< The call list. */
	compound_call_lowering_flags flags;
	ir_type              *lowered_mtp;     /**< The lowered method type of the current irg if any. */
	bool                  only_local_mem:1;/**< Set if only local memory access was found. */
	bool                  changed:1;       /**< Set if the current graph was changed. */
} wlk_env;

/**
 * Return the call list entry of a call node.
 * If no entry exists yet, allocate one and enter the node into
 * the call list of the environment.
 *
 * @param call   A Call node.
 * @param env    The environment.
 */
static cl_entry *get_call_entry(ir_node *call, wlk_env *env)
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
		env->only_local_mem = false;
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
			return true;
	}
	return false;
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
				env->changed = true;
			}
		}
		break;
	case iro_Call: {
		ir_type *ctp      = get_Call_type(n);
		size_t   n_ress   = get_method_n_ress(ctp);
		size_t   n_params = get_method_n_params(ctp);
		size_t   i;
		if (! is_self_recursive_Call(n)) {
			/* any non self recursive call might access global memory */
			env->only_local_mem = false;
		}

		/* check for compound returns */
		for (i = 0; i < n_ress; ++i) {
			ir_type *type = get_method_res_type(ctp, i);
			if (needs_lowering(type)) {
				/*
				 * This is a call with a compound return. As the result
				 * might be ignored, we must put it in the list.
				 */
				cl_entry *entry = get_call_entry(n, env);
				entry->has_compound_ret = true;
				break;
			}
		}
		for (i = 0; i < n_params; ++i) {
			ir_type *type = get_method_param_type(ctp, i);
			if (needs_lowering(type)) {
				cl_entry *entry = get_call_entry(n, env);
				entry->has_compound_param = true;
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
					ir_type *ctp = get_Call_type(call);
					if (needs_lowering(get_method_res_type(ctp, get_Proj_proj(src)))) {
						/* found a CopyB from compound Call result */
						cl_entry *e = get_call_entry(call, env);
						set_irn_link(n, e->copyb);
						e->copyb = n;
					}
				}
			}
		}
		break;
	}
	case iro_Sel: {
		ir_entity *entity = get_Sel_entity(n);
		ir_type   *type   = get_entity_type(entity);

		if (is_parameter_entity(entity) && needs_lowering(type)) {
			if (! (env->flags & LF_DONT_LOWER_ARGUMENTS)) {
				/* note that num was already modified by fix_parameter_entities
				 * so no need to add env->arg_shift again */
				size_t num = get_entity_parameter_number(entity);
				ir_graph *irg  = get_irn_irg(n);
				ir_node  *args = get_irg_args(irg);
				ir_node  *ptr  = new_r_Proj(args, mode_P, num);
				exchange(n, ptr);
				/* hack to avoid us visiting the proj again */
				mark_irn_visited(ptr);
			}

			/* we need to copy compound parameters */
			env->only_local_mem = false;
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
 *
 * @param irg    the graph
 * @param block  the block where a newly create Sel should be placed
 * @param tp     the type of the dummy entity that should be create
 */
static ir_node *get_dummy_sel(ir_graph *irg, ir_node *block, ir_type *tp)
{
	ir_type   *ft       = get_irg_frame_type(irg);
	ident     *dummy_id = id_unique("dummy.%u");
	ir_entity *ent      = new_entity(ft, dummy_id, tp);

	if (get_type_state(ft) == layout_fixed) {
		/* Fix the layout again */
		panic("Fixed layout not implemented");
	}

	return new_r_simpleSel(block, get_irg_no_mem(irg), get_irg_frame(irg), ent);
}

/**
 * Add the hidden parameter from the CopyB node to the Call node.
 */
static void add_hidden_param(ir_graph *irg, size_t n_com, ir_node **ins,
                             cl_entry *entry, ir_type *ctp)
{
	ir_node *p, *n;
	size_t n_args;

	n_args = 0;
	for (p = entry->copyb; p; p = n) {
		ir_node *src = get_CopyB_src(p);
		size_t   idx = get_Proj_proj(src);
		n = (ir_node*)get_irn_link(p);

		/* consider only the first CopyB */
		if (ins[idx] == NULL) {
			ir_node *block = get_nodes_block(p);

			/* use the memory output of the call and not the input of the CopyB
			 * otherwise stuff breaks if the call was mtp_property_const, because
			 * then the copyb skips the call. But after lowering the call is not
			 * const anymore, and its memory has to be used */
			ir_node *mem = new_r_Proj(entry->call, mode_M, pn_Call_M);

			ins[idx] = get_CopyB_dst(p);

			/* get rid of the CopyB */
			if (ir_throws_exception(p)) {
				ir_node *const in[] = {
					[pn_CopyB_M]         = mem,
					[pn_CopyB_X_regular] = new_r_Jmp(block),
					[pn_CopyB_X_except]  = new_r_Bad(irg, mode_X),
				};
				turn_into_tuple(p, ARRAY_SIZE(in), in);
			} else {
				ir_node *const in[] = { mem };
				turn_into_tuple(p, ARRAY_SIZE(in), in);
			}
			++n_args;
		}
	}

	/* now create dummy entities for function with ignored return value */
	if (n_args < n_com) {
		size_t i;
		size_t j;

		for (j = i = 0; i < get_method_n_ress(ctp); ++i) {
			ir_type *rtp = get_method_res_type(ctp, i);
			if (needs_lowering(rtp)) {
				if (ins[j] == NULL)
					ins[j] = get_dummy_sel(irg, get_nodes_block(entry->call), rtp);
				++j;
			}
		}
	}
}

static void fix_compound_ret(cl_entry *entry, ir_type *ctp)
{
	ir_node  *call     = entry->call;
	ir_graph *irg      = get_irn_irg(call);
	size_t    n_params = get_Call_n_params(call);
	size_t    n_com    = 0;
	size_t    n_res    = get_method_n_ress(ctp);
	size_t    pos      = 0;
	ir_node **new_in;
	size_t    i;

	for (i = 0; i < n_res; ++i) {
		ir_type *type = get_method_res_type(ctp, i);
		if (needs_lowering(type))
			++n_com;
	}

	new_in = ALLOCANZ(ir_node*, n_params + n_com + (n_Call_max+1));
	new_in[pos++] = get_Call_mem(call);
	new_in[pos++] = get_Call_ptr(call);
	assert(pos == n_Call_max+1);
	add_hidden_param(irg, n_com, &new_in[pos], entry, ctp);
	pos += n_com;

	/* copy all other parameters */
	for (i = 0; i < n_params; ++i) {
		ir_node *param = get_Call_param(call, i);
		new_in[pos++] = param;
	}
	assert(pos == n_params+n_com+(n_Call_max+1));
	set_irn_in(call, pos, new_in);
}

static ir_entity *create_compound_arg_entity(ir_graph *irg, ir_type *type)
{
	ir_type   *frame  = get_irg_frame_type(irg);
	ident     *id     = id_unique("$compound_param.%u");
	ir_entity *entity = new_entity(frame, id, type);
	/* TODO:
	 * we could do some optimisations here and create a big union type for all
	 * different call types in a function */
	return entity;
}

static void fix_compound_params(cl_entry *entry, ir_type *ctp)
{
	ir_node  *call     = entry->call;
	dbg_info *dbgi     = get_irn_dbg_info(call);
	ir_node  *mem      = get_Call_mem(call);
	ir_graph *irg      = get_irn_irg(call);
	ir_node  *nomem    = new_r_NoMem(irg);
	ir_node  *frame    = get_irg_frame(irg);
	size_t    n_params = get_method_n_params(ctp);
	size_t    i;

	for (i = 0; i < n_params; ++i) {
		ir_type   *type = get_method_param_type(ctp, i);
		ir_node   *block;
		ir_node   *arg;
		ir_node   *sel;
		ir_node   *copyb;
		ir_entity *arg_entity;
		if (!needs_lowering(type))
			continue;

		arg        = get_Call_param(call, i);
		arg_entity = create_compound_arg_entity(irg, type);
		block      = get_nodes_block(call);
		sel        = new_rd_simpleSel(dbgi, block, nomem, frame, arg_entity);
		copyb      = new_rd_CopyB(dbgi, block, mem, sel, arg, type);
		mem        = new_r_Proj(copyb, mode_M, pn_CopyB_M);
		set_Call_param(call, i, sel);
	}
	set_Call_mem(call, mem);
}

static void fix_calls(wlk_env *env)
{
	cl_entry *entry;
	for (entry = env->cl_list; entry; entry = entry->next) {
		ir_node *call        = entry->call;
		ir_type *ctp         = get_Call_type(call);
		ir_type *lowered_mtp = lower_mtp(env->flags, ctp);
		set_Call_type(call, lowered_mtp);

		if (entry->has_compound_param) {
			fix_compound_params(entry, ctp);
		}
		if (entry->has_compound_ret) {
			fix_compound_ret(entry, ctp);
		}
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
	ir_entity *ent         = get_irg_entity(irg);
	ir_type   *mtp         = get_entity_type(ent);
	size_t     n_ress      = get_method_n_ress(mtp);
	size_t     n_params    = get_method_n_params(mtp);
	size_t     n_param_com = 0;

	ir_type   *lowered_mtp, *tp, *ft;
	size_t    i, j, k;
	size_t    n_cr_opt;
	ir_node   **new_in, *ret, *endbl, *bl, *mem, *copy;
	cr_pair   *cr_opt;
	wlk_env   env;

	/* calculate the number of compound returns */
	size_t n_ret_com = 0;
	for (i = 0; i < n_ress; ++i) {
		ir_type *type = get_method_res_type(mtp, i);
		if (needs_lowering(type))
			++n_ret_com;
	}
	for (i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(mtp, i);
		if (needs_lowering(type))
			++n_param_com;
	}

	if (n_ret_com > 0)
		fix_parameter_entities(irg, n_ret_com);

	if (n_ret_com) {
		/* much easier if we have only one return */
		normalize_one_return(irg);

		/* hidden arguments are added first */
		env.arg_shift = n_ret_com;
	} else {
		/* we must only search for calls */
		env.arg_shift = 0;
		lowered_mtp   = NULL;
	}

	lowered_mtp = lower_mtp(flags, mtp);
	set_entity_type(ent, lowered_mtp);

	obstack_init(&env.obst);
	env.cl_list        = NULL;
	env.flags          = flags;
	env.lowered_mtp    = lowered_mtp;
	env.only_local_mem = true;
	env.changed        = false;

	/* scan the code, fix argument numbers and collect calls. */
	irg_walk_graph(irg, firm_clear_link, NULL, &env);
	irg_walk_graph(irg, fix_args_and_collect_calls, NULL, &env);

	if (n_param_com > 0 && !(flags & LF_DONT_LOWER_ARGUMENTS))
		remove_compound_param_entities(irg);

	/* fix all calls */
	if (env.cl_list != NULL) {
		fix_calls(&env);
		env.changed = true;
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
			env.changed = true;

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

				if (needs_lowering(tp)) {
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
