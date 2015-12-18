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

#include "array.h"
#include "be.h"
#include "panic.h"
#include "firm_types.h"
#include "heights.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irmemory.h"
#include "irmemory_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "irtools.h"
#include "lower_calls.h"
#include "lowering.h"
#include "pmap.h"
#include "type_t.h"
#include "util.h"

static pmap    *pointer_types;
static pmap    *lowered_mtps;

typedef struct lowering_env_t {
	compound_call_lowering_flags flags;
	decide_aggregate_ret_func    aggregate_ret;
} lowering_env_t;

aggregate_spec_t const no_values_aggregate_spec = {
	.n_values = 0,
	.modes    = NULL,
};

static aggregate_spec_t const *get_no_values(ir_type const *const type)
{
	(void)type;
	return &no_values_aggregate_spec;
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

static void fix_parameter_entities(ir_graph *irg, unsigned arg_shift)
{
	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n_members  = get_compound_n_members(frame_type);

	for (size_t i = 0; i < n_members; ++i) {
		ir_entity *member = get_compound_member(frame_type, i);
		if (!is_parameter_entity(member))
			continue;

		/* increase parameter number since we added a new parameter in front */
		size_t num = get_entity_parameter_number(member);
		if (num == IR_VA_START_PARAMETER_NUMBER)
			continue;
		set_entity_parameter_number(member, num + arg_shift);
	}
}

static void remove_compound_param_entities(ir_graph *irg)
{
	ir_type *frame_type = get_irg_frame_type(irg);
	size_t   n_members  = get_compound_n_members(frame_type);

	for (size_t i = n_members; i-- > 0; ) {
		ir_entity *member = get_compound_member(frame_type, i);
		if (!is_parameter_entity(member))
			continue;

		ir_type *type = get_entity_type(member);
		if (is_aggregate_type(type)) {
			free_entity(member);
		}
	}
}

/**
 * Creates a new lowered type for a method type with compound
 * arguments. The new type is associated to the old one and returned.
 */
static ir_type *lower_mtp(lowering_env_t const *const env, ir_type *mtp)
{
	if (!is_Method_type(mtp))
		return mtp;

	ir_type *lowered = pmap_get(ir_type, lowered_mtps, mtp);
	if (lowered != NULL)
		return lowered;

	/* check if the type has to be lowered at all */
	bool   must_be_lowered = false;
	size_t n_params        = get_method_n_params(mtp);
	size_t n_ress          = get_method_n_ress(mtp);
	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *res_tp = get_method_res_type(mtp, i);
		if (is_aggregate_type(res_tp)) {
			must_be_lowered = true;
			break;
		}
	}
	if (!must_be_lowered && !(env->flags & LF_DONT_LOWER_ARGUMENTS)) {
		for (size_t i = 0; i < n_params; ++i) {
			ir_type *param_type = get_method_param_type(mtp, i);
			if (is_aggregate_type(param_type)) {
				must_be_lowered = true;
				break;
			}
		}
	}
	if (!must_be_lowered)
		return mtp;

	ir_type **params    = ALLOCANZ(ir_type*, n_params + n_ress);
	ir_type **results   = ALLOCANZ(ir_type*, n_ress * 2);
	size_t    nn_params = 0;
	size_t    nn_ress   = 0;

	/* add a hidden parameter in front for every compound result */
	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *const res_tp = get_method_res_type(mtp, i);

		if (is_aggregate_type(res_tp)) {
			aggregate_spec_t const *const ret_spec = env->aggregate_ret(res_tp);
			unsigned                const n_values = ret_spec->n_values;
			if (n_values > 0) {
				for (unsigned i = 0; i < n_values; ++i) {
					ir_mode *const mode = ret_spec->modes[i];
					results[nn_ress++] = get_type_for_mode(mode);
				}
			} else {
				/* this compound will be allocated on callers stack and its
				   address will be transmitted as a hidden parameter. */
				ir_type *ptr_tp = get_pointer_type(res_tp);
				params[nn_params++] = ptr_tp;
				if (env->flags & LF_RETURN_HIDDEN)
					results[nn_ress++] = ptr_tp;
			}
		} else {
			/* scalar result */
			results[nn_ress++] = res_tp;
		}
	}
	/* copy over parameter types */
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *param_type = get_method_param_type(mtp, i);
		if (!(env->flags & LF_DONT_LOWER_ARGUMENTS)
		    && is_aggregate_type(param_type)) {
		    /* turn parameter into a pointer type */
		    param_type = new_type_pointer(param_type);
		}
		params[nn_params++] = param_type;
	}
	assert(nn_ress <= n_ress*2);
	assert(nn_params <= n_params + n_ress);

	/* create the new type */
	lowered = new_type_method(nn_params, nn_ress);
	set_type_dbg_info(lowered, get_type_dbg_info(mtp));

	/* fill it */
	for (size_t i = 0; i < nn_params; ++i)
		set_method_param_type(lowered, i, params[i]);
	for (size_t i = 0; i < nn_ress; ++i)
		set_method_res_type(lowered, i, results[i]);

	set_method_variadic(lowered, is_method_variadic(mtp));

	unsigned cconv = get_method_calling_convention(mtp);
	if (nn_params > n_params) {
		cconv |= cc_compound_ret;
	}
	set_method_calling_convention(lowered, cconv);

	mtp_additional_properties mtp_properties = get_method_additional_properties(mtp);
	/* after lowering the call is not pure anymore, since it writes to the
	 * memory for the return value passed to it */
	mtp_properties &= ~(mtp_property_no_write | mtp_property_pure);
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
	ir_node  *proj_M;
	ir_node  *proj_res;
	unsigned  n_compound_ret;
	bool      has_compound_param;
};

/**
 * Walker environment for fix_args_and_collect_calls().
 */
typedef struct wlk_env {
	unsigned             arg_shift;        /**< The Argument index shift for parameters. */
	struct obstack       obst;             /**< An obstack to allocate the data on. */
	cl_entry             *cl_list;         /**< The call list. */
	compound_call_lowering_flags flags;
	lowering_env_t const *env;
	ir_type              *mtp;             /**< original mtp before lowering */
	ir_type              *lowered_mtp;     /**< The lowered method type of the current irg if any. */
	ir_heights_t         *heights;         /**< Heights for reachability check. */
	bool                  only_local_mem:1;/**< Set if only local memory access was found. */
	bool                  changed:1;       /**< Set if the current graph was changed. */
	ir_node             **param_members;
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
		res = OALLOCZ(&env->obst, cl_entry);
		res->next = env->cl_list;
		res->call = call;
		set_irn_link(call, res);
		env->cl_list = res;
	}
	return res;
}

/**
 * Finds the base address of an address by skipping Member's and address
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
		if (is_Member(ptr)) {
			ent = get_Member_entity(ptr);
			ptr = get_Member_ptr(ptr);
		} else if (is_Sel(ptr)) {
			ptr = get_Sel_ptr(ptr);
		} else if (is_Add(ptr)) {
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
	/* still alias free */
	ir_entity *ent;
	ir_node *base_ptr = find_base_adr(ptr, &ent);
	ir_storage_class_class_t sc
		= get_base_sc(classify_pointer(ptr, base_ptr));
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
	const ir_entity *callee = get_Call_callee(call);
	if (callee != NULL) {
		const ir_graph *irg = get_entity_linktime_irg(callee);
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

	switch (get_irn_opcode(n)) {
	case iro_Load:
	case iro_Store:
		if (env->only_local_mem) {
			ir_node *ptr = get_irn_n(n, 1);
			check_ptr(ptr, env);
		}
		break;
	case iro_Proj: {
		ir_node  *pred = get_Proj_pred(n);
		ir_graph *irg  = get_irn_irg(n);
		if (pred == get_irg_args(irg)) {
			unsigned arg_shift = env->arg_shift;
			if (arg_shift > 0) {
				unsigned pn = get_Proj_num(n);
				set_Proj_num(n, pn + arg_shift);
				env->changed = true;
			}
		} else if (is_Call(pred)) {
			unsigned pn = get_Proj_num(n);
			if (pn == pn_Call_M) {
				cl_entry *entry = get_call_entry(pred, env);
				entry->proj_M = n;
			} else if (pn == pn_Call_T_result) {
				cl_entry *entry = get_call_entry(pred, env);
				entry->proj_res = n;
			}
		}
		break;
	}
	case iro_Call: {
		ir_type *ctp      = get_Call_type(n);
		size_t   n_ress   = get_method_n_ress(ctp);
		size_t   n_params = get_method_n_params(ctp);
		if (! is_self_recursive_Call(n)) {
			/* any non self recursive call might access global memory */
			env->only_local_mem = false;
		}

		/* check for compound returns */
		for (size_t i = 0; i < n_ress; ++i) {
			ir_type *type = get_method_res_type(ctp, i);
			if (is_aggregate_type(type)) {
				/*
				 * This is a call with a compound return. As the result
				 * might be ignored, we must put it in the list.
				 */
				cl_entry *entry = get_call_entry(n, env);
				++entry->n_compound_ret;
			}
		}
		for (size_t i = 0; i < n_params; ++i) {
			ir_type *type = get_method_param_type(ctp, i);
			if (is_aggregate_type(type)) {
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
			if (is_Proj(proj) && get_Proj_num(proj) == pn_Call_T_result) {
				ir_node *call = get_Proj_pred(proj);
				if (is_Call(call)) {
					ir_type *ctp = get_Call_type(call);
					if (is_aggregate_type(get_method_res_type(ctp, get_Proj_num(src)))) {
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
	case iro_Member: {
		ir_entity *entity = get_Member_entity(n);
		if (!is_parameter_entity(entity))
			break;
		ir_type *type = get_entity_type(entity);
		if (is_aggregate_type(type)) {
			if (! (env->flags & LF_DONT_LOWER_ARGUMENTS))
				ARR_APP1(ir_node*, env->param_members, n);
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
 * Returns non-zero if a node is a compound address of a frame-type entity.
 *
 * @param ft   the frame type
 * @param adr  the node
 */
static bool is_compound_address(ir_type *ft, ir_node *adr)
{
	if (!is_Member(adr))
		return false;
	ir_entity *ent = get_Member_entity(adr);
	return get_entity_owner(ent) == ft;
}

/** A pair for the copy-return-optimization. */
typedef struct cr_pair {
	ir_entity *ent; /**< the entity than can be removed from the frame */
	ir_node *arg;   /**< the argument that replaces the entities address */
} cr_pair;

typedef struct copy_return_opt_env {
	cr_pair *arr;
	size_t   n_pairs;
} copy_return_opt_env;

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
	if (is_Member(n)) {
		copy_return_opt_env *env = (copy_return_opt_env*)ctx;
		ir_entity *ent = get_Member_entity(n);

		for (size_t i = 0, n_pairs = env->n_pairs; i < n_pairs; ++i) {
			if (ent == env->arr[i].ent) {
				exchange(n, env->arr[i].arg);
				break;
			}
		}
	}
}

/**
 * Return a Member node that selects a dummy argument of type tp.
 *
 * @param block  the block where a newly create Member should be placed
 * @param tp     the type of the dummy entity that should be create
 */
static ir_node *get_dummy_member(ir_node *block, ir_type *tp)
{
	ir_graph *irg = get_irn_irg(block);
	ir_type  *ft  = get_irg_frame_type(irg);
	if (get_type_state(ft) == layout_fixed) {
		/* Fix the layout again */
		panic("fixed layout not implemented");
	}

	ident     *dummy_id = id_unique("call_result.%u");
	ir_entity *ent      = new_entity(ft, dummy_id, tp);
	return new_r_Member(block, get_irg_frame(irg), ent);
}

/**
 * Add the hidden parameter from the CopyB node to the Call node.
 */
static void get_dest_addrs(const cl_entry *entry, ir_node **ins,
                           const ir_type *orig_ctp, wlk_env *env)
{
	unsigned n_args = 0;
	for (ir_node *next, *copyb = entry->copyb; copyb != NULL; copyb = next) {
		ir_node *src = get_CopyB_src(copyb);
		size_t   idx = get_Proj_num(src);
		next = (ir_node*)get_irn_link(copyb);

		/* consider only the first CopyB */
		if (ins[idx] != NULL)
			continue;

		ir_node *call       = entry->call;
		ir_node *call_block = get_nodes_block(call);
		ir_node *dst        = get_CopyB_dst(copyb);
		ir_node *dst_block  = get_nodes_block(dst);

		/* Check whether we can use the destination of the CopyB for the call. */
		if (!block_dominates(dst_block, call_block))
			continue;

		if (dst_block == call_block) {
			ir_heights_t *heights = env->heights;
			if (heights == NULL) {
				ir_graph *irg = get_irn_irg(call_block);
				heights = heights_new(irg);
				env->heights = heights;
			}

			/* Do not optimize the CopyB if the destination depends on the
			 * call. */
			if (heights_reachable_in_block(heights, dst, call))
				continue;
		}

		ir_graph *irg   = get_irn_irg(dst);
		ir_node  *frame = get_irg_frame(irg);
		if (!is_Member(dst) || get_Member_ptr(dst) != frame)
			continue;

		ir_entity *dst_ent = get_Member_entity(dst);
		if (get_entity_usage(dst_ent) & ir_usage_address_taken)
			continue;

		/* Special case for calls with NoMem memory input. This can happen
		 * for mtp_property_const & mtp_property_terminates functions.
		 * The call needs a memory input after lowering, so patch it here
		 * to be the input of the CopyB. Note that in case of multiple CopyB
		 * return values this code may break the order: fix it if you find a
		 * language that actually uses this. */
		ir_node *copyb_mem = get_CopyB_mem(copyb);
		ir_node *call_mem  = get_Call_mem(call);
		if (is_NoMem(call_mem)) {
			set_Call_mem(call, copyb_mem);
			copyb_mem = new_r_Proj(call, mode_M, pn_Call_M);
		}

		ins[idx] = dst;
		/* get rid of the CopyB */
		exchange(copyb, copyb_mem);
		++n_args;
	}

	/* now create dummy entities for function with ignored return value */
	unsigned n_compound_ret = entry->n_compound_ret;
	if (n_args < n_compound_ret) {
		for (size_t i = 0, j = 0, n_ress = get_method_n_ress(orig_ctp);
		     i < n_ress; ++i) {
			ir_type *rtp = get_method_res_type(orig_ctp, i);
			if (is_aggregate_type(rtp)) {
				if (ins[j] == NULL)
					ins[j] = get_dummy_member(get_nodes_block(entry->call), rtp);
				++j;
			}
		}
	}
}

static void fix_int_return(cl_entry const *const entry,
                           ir_node *const base_addr,
                           aggregate_spec_t const *const ret_spec,
                           long const orig_pn, long const pn)
{
	ir_node  *const call  = entry->call;
	ir_node  *const block = get_nodes_block(call);
	ir_graph *const irg   = get_irn_irg(base_addr);

	/* we need edges activated here */
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	/* if the Call throws an exception, then we cannot add instruction
	 * immediately behind it as the call ends the basic block */
	assert(!is_cfop(call));
	ir_mode *const mode_ref = get_irn_mode(base_addr);

	ir_node *proj_mem = entry->proj_M;
	if (proj_mem == NULL)
		proj_mem = new_r_Proj(call, mode_M, pn_Call_M);
	ir_node *proj_res = entry->proj_res;
	if (proj_res == NULL)
		proj_res = new_r_Proj(call, mode_T, pn_Call_T_result);
	/* reroute old users */
	ir_node *const res_user = get_Proj_for_pn(proj_res, orig_pn);
	if (res_user != NULL)
		edges_reroute(res_user, base_addr);

	/* very hacky: reroute all memory users to a dummy node, which we will
	 * later reroute to the new memory */
	ir_node *dummy = new_r_Dummy(irg, mode_M);
	edges_reroute(proj_mem, dummy);

	unsigned  const n_values = ret_spec->n_values;
	ir_node **const sync_in  = ALLOCAN(ir_node*, n_values);
	int             offset   = 0;
	for (unsigned i = 0; i < n_values; ++i) {
		ir_mode *const mode = ret_spec->modes[i];
		ir_node *      addr = base_addr;
		if (offset > 0) {
			ir_mode *mode_offset = get_reference_offset_mode(mode_ref);
			ir_node *offset_cnst = new_r_Const_long(irg, mode_offset, offset);
			addr = new_r_Add(block, addr, offset_cnst, mode_ref);
		}
		ir_node *const value     = new_r_Proj(proj_res, mode, pn+i);
		ir_type *const type      = get_type_for_mode(mode);
		ir_node *const store     = new_r_Store(block, proj_mem, addr, value,
		                                       type, cons_none);
		ir_node *const store_mem = new_r_Proj(store, mode_M, pn_Store_M);
		sync_in[i] = store_mem;
		offset += get_mode_size_bytes(mode);
	}

	ir_node *const sync = new_r_Sync(block, n_values, sync_in);
	edges_reroute(dummy, sync);
}

static void fix_call_compound_ret(const cl_entry *entry,
                                  const ir_type *orig_ctp, wlk_env *env)
{
	/* produce destination addresses */
	unsigned  n_compound_ret = entry->n_compound_ret;
	ir_node **dest_addrs     = ALLOCANZ(ir_node*, n_compound_ret);
	get_dest_addrs(entry, dest_addrs, orig_ctp, env);

	/* now add parameters for destinations or produce stores if compound is
	 * returned as values */
	ir_node  *call     = entry->call;
	size_t    n_params = get_Call_n_params(call);
	size_t    max_ins  = n_params + (n_Call_max+1) + n_compound_ret;
	ir_node **new_in   = NULL;
	size_t    pos      = (size_t)-1;
	long      pn       = 0;
	for (size_t i = 0, c = 0, n_ress = get_method_n_ress(orig_ctp);
	     i < n_ress; ++i) {
		ir_type *type = get_method_res_type(orig_ctp, i);
		if (!is_aggregate_type(type)) {
			++pn;
			continue;
		}

		ir_node *dest_addr = dest_addrs[c++];
		aggregate_spec_t const *const ret_spec = env->env->aggregate_ret(type);
		unsigned                const n_values = ret_spec->n_values;
		if (n_values > 0) {
			fix_int_return(entry, dest_addr, ret_spec, i, pn);
			pn += n_values;
		} else {
			/* add parameter with destination */
			/* lazily construct new_input list */
			if (new_in == NULL) {
				new_in = ALLOCAN(ir_node*, max_ins);
				new_in[n_Call_mem] = get_Call_mem(call);
				new_in[n_Call_ptr] = get_Call_ptr(call);
				pos = 2;
				assert(pos == n_Call_max+1);
			}
			new_in[pos++] = dest_addr;

			if (env->flags & LF_RETURN_HIDDEN)
				++pn;
		}
	}

	/* do we have new inputs? */
	if (new_in != NULL) {
		/* copy all other parameters */
		for (size_t i = 0; i < n_params; ++i) {
			ir_node *param = get_Call_param(call, i);
			new_in[pos++] = param;
		}
		assert(pos <= max_ins);
		set_irn_in(call, pos, new_in);
	}
}

static ir_entity *create_compound_arg_entity(ir_graph *irg, ir_type *type)
{
	ir_type   *frame  = get_irg_frame_type(irg);
	ident     *id     = id_unique("$compound_param.%u");
	ir_entity *entity = new_entity(frame, id, type);
	/* TODO: we could do some optimizations here and create a big union type
	 * for all different call types in a function */
	return entity;
}

static void fix_call_compound_params(const cl_entry *entry, const ir_type *ctp, wlk_env *env)
{
	ir_node  *call     = entry->call;
	dbg_info *dbgi     = get_irn_dbg_info(call);
	ir_node  *mem      = get_Call_mem(call);
	ir_graph *irg      = get_irn_irg(call);
	ir_node  *frame    = get_irg_frame(irg);
	size_t    n_params = get_method_n_params(ctp);

	for (size_t i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(ctp, i);
		if (!is_aggregate_type(type) || (env->flags & LF_DONT_LOWER_ARGUMENTS))
			continue;

		ir_node   *arg         = get_Call_param(call, i);
		ir_entity *arg_entity  = create_compound_arg_entity(irg, type);
		ir_node   *block       = get_nodes_block(call);
		ir_node   *sel         = new_rd_Member(dbgi, block, frame, arg_entity);
		bool       is_volatile = is_partly_volatile(arg);
		mem = new_rd_CopyB(dbgi, block, mem, sel, arg, type, is_volatile ? cons_volatile : cons_none);
		set_Call_param(call, i, sel);
	}
	set_Call_mem(call, mem);
}

static void fix_calls(wlk_env *env)
{
	for (const cl_entry *entry = env->cl_list; entry; entry = entry->next) {
		if (!entry->has_compound_param && entry->n_compound_ret == 0)
			continue;
		ir_node *call        = entry->call;
		ir_type *ctp         = get_Call_type(call);
		ir_type *lowered_mtp = lower_mtp(env->env, ctp);
		set_Call_type(call, lowered_mtp);

		if (entry->has_compound_param) {
			fix_call_compound_params(entry, ctp, env);
		}
		if (entry->n_compound_ret > 0) {
			fix_call_compound_ret(entry, ctp, env);
		}
		env->changed = true;
	}
}

static void transform_return(ir_node *ret, size_t n_ret_com, wlk_env *env)
{
	ir_node   *block      = get_nodes_block(ret);
	ir_graph  *irg        = get_irn_irg(ret);
	ir_type   *mtp        = env->mtp;
	size_t     n_ress     = get_method_n_ress(mtp);
	ir_node   *mem        = get_Return_mem(ret);
	ir_node   *args       = get_irg_args(irg);
	ir_type   *frame_type = get_irg_frame_type(irg);
	size_t     n_cr_opt   = 0;
	size_t     n_in       = 1;
	ir_node  **new_in     = ALLOCAN(ir_node*, n_ress*2 + 1);
	cr_pair   *cr_opt     = ALLOCAN(cr_pair, n_ret_com);

	for (size_t i = 0, k = 0; i < n_ress; ++i) {
		ir_node *pred = get_Return_res(ret, i);
		ir_type *type = get_method_res_type(mtp, i);
		if (!is_aggregate_type(type)) {
			new_in[n_in++] = pred;
			continue;
		}

		aggregate_spec_t const *const ret_spec = env->env->aggregate_ret(type);
		unsigned                const n_values = ret_spec->n_values;
		if (n_values > 0) {
			if (is_Unknown(pred)) {
				for (unsigned i = 0; i < n_values; ++i) {
					new_in[n_in++] = new_r_Unknown(irg, ret_spec->modes[i]);
				}
			} else {
				ir_node **const sync_in = ALLOCAN(ir_node*, n_values);
				int             offset  = 0;
				for (unsigned i = 0; i < n_values; ++i) {
					ir_node *      addr     = pred;
					ir_mode *const mode_ref = get_irn_mode(addr);
					if (offset > 0) {
						ir_mode *const mode_offset
							= get_reference_offset_mode(mode_ref);
						ir_node *const offset_cnst
							= new_r_Const_long(irg, mode_offset, offset);
						addr = new_r_Add(block, addr, offset_cnst, mode_ref);
					}
					ir_mode *const mode = ret_spec->modes[i];
					ir_node *const load = new_r_Load(block, mem, addr, mode,
					                                 type, cons_none);
					sync_in[i]     = new_r_Proj(load, mode_M, pn_Load_M);
					new_in[n_in++] = new_r_Proj(load, mode, pn_Load_res);
					offset += get_mode_size_bytes(mode);
				}
				mem = new_r_Sync(block, n_values, sync_in);
			}
			continue;
		}

		ir_node *arg = new_r_Proj(args, mode_P, k++);
		if (env->flags & LF_RETURN_HIDDEN)
			new_in[n_in++] = arg;

		/* nothing to do when returning an unknown value */
		if (is_Unknown(pred))
			continue;

		/**
		 * Sorrily detecting that copy-return is possible isn't
		 * that simple. We must check, that the hidden address
		 * is alias free during the whole function.
		 * A simple heuristic: all Loads/Stores inside
		 * the function access only local frame.
		 */
		if (env->only_local_mem && is_compound_address(frame_type, pred)) {
			/* we can do the copy-return optimization here */
			cr_opt[n_cr_opt].ent = get_Member_entity(pred);
			cr_opt[n_cr_opt].arg = arg;
			++n_cr_opt;
		} else {
			/* copy-return optimization is impossible, do the copy. */
			bool is_volatile = is_partly_volatile(pred);
			mem = new_r_CopyB(block, mem, arg, pred, type,
			                  is_volatile ? cons_volatile : cons_none);
		}
	}
	/* replace the in of the Return */
	assert(n_in <= n_ress*2 + 1);
	new_in[0] = mem;
	set_irn_in(ret, n_in, new_in);

	if (n_cr_opt > 0) {
		copy_return_opt_env env;
		env.arr     = cr_opt;
		env.n_pairs = n_cr_opt;
		irg_walk_graph(irg, NULL, do_copy_return_opt, &env);

		for (size_t c = 0; c < n_cr_opt; ++c)
			free_entity(cr_opt[c].ent);
	}

	env->changed = true;
}

/**
 * Transform a graph. If it has compound parameter returns,
 * remove them and use the hidden parameter instead.
 * If it calls methods with compound parameter returns, add hidden
 * parameters.
 */
static void transform_irg(lowering_env_t const *const env, ir_graph *const irg)
{
	ir_entity *ent         = get_irg_entity(irg);
	ir_type   *mtp         = get_entity_type(ent);
	size_t     n_ress      = get_method_n_ress(mtp);
	size_t     n_params    = get_method_n_params(mtp);
	size_t     n_param_com = 0;

	/* calculate the number of compound returns */
	size_t   n_ret_com = 0;
	unsigned arg_shift = 0;
	for (size_t i = 0; i < n_ress; ++i) {
		ir_type *type = get_method_res_type(mtp, i);
		if (is_aggregate_type(type)) {
			++n_ret_com;

			aggregate_spec_t const *const ret_spec = env->aggregate_ret(type);
			/* if we don't return it as values, then we will add a new parameter
			 * with the address of the destination memory */
			if (ret_spec->n_values == 0)
				++arg_shift;
		}
	}
	for (size_t i = 0; i < n_params; ++i) {
		ir_type *type = get_method_param_type(mtp, i);
		if (is_aggregate_type(type))
			++n_param_com;
	}

	if (arg_shift > 0)
		fix_parameter_entities(irg, arg_shift);

	/* much easier if we have only one return */
	if (n_ret_com != 0)
		assure_irg_properties(irg, IR_GRAPH_PROPERTY_ONE_RETURN);

	ir_type *lowered_mtp = lower_mtp(env, mtp);
	set_entity_type(ent, lowered_mtp);

	wlk_env walk_env = {
		.arg_shift      = arg_shift,
		.flags          = env->flags,
		.env            = env,
		.mtp            = mtp,
		.lowered_mtp    = lowered_mtp,
		.param_members  = NEW_ARR_F(ir_node*, 0),
		.only_local_mem = true,
	};
	obstack_init(&walk_env.obst);

	/* scan the code, fix argument numbers and collect calls. */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irg_walk_graph(irg, firm_clear_link, NULL, &walk_env);
	irg_walk_graph(irg, fix_args_and_collect_calls, NULL, &walk_env);

	/* fix parameter sels */
	ir_node *args = get_irg_args(irg);
	for (size_t i = 0, n = ARR_LEN(walk_env.param_members); i < n; ++i) {
		ir_node   *member = walk_env.param_members[i];
		ir_entity *entity = get_Member_entity(member);
		size_t     num    = get_entity_parameter_number(entity);
		ir_node   *ptr    = new_r_Proj(args, mode_P, num);
		exchange(member, ptr);
	}
	DEL_ARR_F(walk_env.param_members);

	if (n_param_com > 0 && !(env->flags & LF_DONT_LOWER_ARGUMENTS))
		remove_compound_param_entities(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	fix_calls(&walk_env);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* transform return nodes */
	if (n_ret_com > 0) {
		ir_node *endbl = get_irg_end_block(irg);
		foreach_irn_in(endbl, i, pred) {
			if (is_Return(pred)) {
				transform_return(pred, n_ret_com, &walk_env);
				break;
			}
		}
	}

	if (walk_env.heights != NULL)
		heights_free(walk_env.heights);
	obstack_free(&walk_env.obst, NULL);
	confirm_irg_properties(irg, walk_env.changed
		? IR_GRAPH_PROPERTIES_CONTROL_FLOW : IR_GRAPH_PROPERTIES_ALL);
}

static void lower_method_types(ir_type *const type, ir_entity *const entity,
                               void *const data)
{
	lowering_env_t const *const env = (lowering_env_t const*)data;

	/* fix method entities */
	if (entity != NULL) {
		ir_type *tp      = get_entity_type(entity);
		ir_type *lowered = lower_mtp(env, tp);
		set_entity_type(entity, lowered);
	} else {
		/* fix pointer to methods */
		if (is_Pointer_type(type)) {
			ir_type *points_to         = get_pointer_points_to_type(type);
			ir_type *lowered_points_to = lower_mtp(env, points_to);
			set_pointer_points_to_type(type, lowered_points_to);
		}
	}
}

void lower_calls_with_compounds(compound_call_lowering_flags flags,
                                decide_aggregate_ret_func aggregate_ret)
{
	pointer_types = pmap_create();
	lowered_mtps = pmap_create();

	lowering_env_t env = {
		.flags         = flags,
		.aggregate_ret = aggregate_ret != NULL ? aggregate_ret : get_no_values,
	};

	/* first step: Transform all graphs */
	foreach_irp_irg(i, irg) {
		transform_irg(&env, irg);
	}

	/* second step: Lower all method types of visible entities */
	type_walk(NULL, lower_method_types, &env);

	pmap_destroy(lowered_mtps);
	pmap_destroy(pointer_types);
}
