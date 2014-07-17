/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimization of function calls.
 * @author  Michael Beck
 */

#include "../adt/util.h"
#include "opt_init.h"
#include <stdbool.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "dbginfo_t.h"
#include "irflag_t.h"
#include "irloop_t.h"
#include "ircons.h"
#include "iredges_t.h"
#include "iroptimize.h"
#include "analyze_irg_args.h"
#include "irhooks.h"
#include "irprog_t.h"
#include "irtools.h"
#include "raw_bitset.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * The walker environment for updating function calls.
 */
typedef struct env_t {
	ir_node  **float_const_call_list;    /**< The list of all floating const function calls that will be changed. */
	ir_node  **nonfloat_const_call_list; /**< The list of all non-floating const function calls that will be changed. */
	ir_node  **pure_call_list;           /**< The list of all pure function calls that will be changed. */
	ir_node  **nothrow_call_list;        /**< The list of all nothrow function calls that will be changed. */
} env_t;

/** Ready IRG's are marked in the ready set. */
static unsigned *ready_set;

/** IRG's that are in progress are marked here. */
static unsigned *busy_set;

static bool method_type_contains_aggregate(const ir_type *type)
{
	for (size_t i = 0, n_params = get_method_n_params(type);
	     i < n_params; ++i) {
		ir_type *param = get_method_param_type(type, i);
		if (is_aggregate_type(param))
			return true;
	}
	for (size_t i = 0, n_ress = get_method_n_ress(type); i < n_ress; ++i) {
		ir_type *res = get_method_res_type(type, i);
		if (is_aggregate_type(res))
			return true;
	}
	return false;
}

/**
 * Walker: Collect all calls to const and pure functions
 * to lists. Collect all Proj(Call) nodes into a Proj list.
 */
static void collect_const_and_pure_calls(ir_node *node, void *env)
{
	env_t *ctx = (env_t*)env;

	if (is_Call(node)) {
		ir_type *type = get_Call_type(node);
		unsigned   prop   = get_method_additional_properties(type);
		ir_entity *callee = get_Call_callee(node);
		if (callee != NULL)
			prop |= get_entity_additional_properties(callee);
		/* stop on aggregates (see comment in check_const_or_pure_function()) */
		if ((prop & mtp_property_const) != 0
		    && method_type_contains_aggregate(type)) {
			prop &= ~mtp_property_const;
			if ((prop & (mtp_property_const|mtp_property_pure)) == 0)
				return;
		}

		if ((prop & (mtp_property_const|mtp_property_pure)) == 0)
			return;

		/* ok, if we get here we found a call to a const or a pure function */
		if (prop & mtp_property_pure) {
			ARR_APP1(ir_node*, ctx->pure_call_list, node);
		} else if (prop & mtp_property_has_loop) {
			ARR_APP1(ir_node*, ctx->nonfloat_const_call_list, node);
		} else {
			ARR_APP1(ir_node*, ctx->float_const_call_list, node);
		}
	} else if (is_Proj(node)) {
		/*
		 * Collect all memory and exception Proj's from
		 * calls.
		 */
		ir_node *call = get_Proj_pred(node);
		if (!is_Call(call))
			return;

		/* collect the Proj's in the Proj list */
		switch (get_Proj_proj(node)) {
		case pn_Call_M:
		case pn_Call_X_except:
		case pn_Call_X_regular:
			set_irn_link(node, get_irn_link(call));
			set_irn_link(call, node);
			break;

		default:
			break;
		}
	}
}

/**
 * Fix the list of collected Calls.
 *
 * @param irg  the graph that contained calls to pure functions
 * @param ctx  context
 */
static void fix_const_call_lists(ir_graph *irg, env_t *ctx)
{
	/* Fix all calls by removing their memory input, let them float and fix
	 * their Projs. */
	bool exc_changed = false;
	for (size_t i = ARR_LEN(ctx->float_const_call_list); i-- > 0;) {
		ir_node *const call = ctx->float_const_call_list[i];
		for (ir_node *next, *proj = (ir_node*)get_irn_link(call); proj != NULL;
		     proj = next) {
			next = (ir_node*)get_irn_link(proj);

			switch (get_Proj_proj(proj)) {
			case pn_Call_M: {
				ir_node *const mem = get_Call_mem(call);
				/* in dead code there might be cycles where proj == mem */
				if (proj != mem)
					exchange(proj, mem);
				break;
			}

			case pn_Call_X_except:
				exc_changed = true;
				exchange(proj, new_r_Bad(irg, mode_X));
				break;

			case pn_Call_X_regular: {
				ir_node *block = get_nodes_block(call);
				exc_changed = true;
				exchange(proj, new_r_Jmp(block));
				break;
			}

			default:
				break;
			}
		}

		set_Call_mem(call, get_irg_no_mem(irg));

		/*
		 * Unfortunately we cannot simply set the node to 'float'.
		 * There is a reason for that:
		 *
		 * - The call might be inside a loop/if that is NOT entered
		 *   and calls a endless function. Setting the call to float
		 *   would allow to move it out from the loop/if causing this
		 *   function be called even if the loop/if is not entered ...
		 *
		 * This could be fixed using post-dominators for calls and Pin nodes
		 * but need some more analyzes to ensure that a call that potential
		 * never returns is not executed before some code that generates
		 * observable states...
		 */

		/* finally, this call can float */
		set_irn_pinned(call, op_pin_state_floats);
		hook_func_call(irg, call);
	}

	if (exc_changed) {
		/* ... including exception edges */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                   | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	}
}

/**
 * Walker: Collect all calls to nothrow functions
 * to lists. Collect all Proj(Call) nodes into a Proj list.
 */
static void collect_nothrow_calls(ir_node *node, void *env)
{
	env_t *ctx = (env_t*)env;

	if (is_Call(node)) {
		ir_entity *callee = get_Call_callee(node);
		if (callee == NULL)
			return;
		unsigned prop = get_entity_additional_properties(callee);
		if ((prop & mtp_property_nothrow) == 0)
			return;
		/* ok, if we get here we found a call to a nothrow function */
		ARR_APP1(ir_node*, ctx->nothrow_call_list, node);
	} else if (is_Proj(node)) {
		/*
		 * Collect all memory and exception Proj's from
		 * calls.
		 */
		ir_node *call = get_Proj_pred(node);
		if (!is_Call(call))
			return;

		/* collect the Proj's in the Proj list */
		switch (get_Proj_proj(node)) {
		case pn_Call_M:
		case pn_Call_X_except:
		case pn_Call_X_regular:
			set_irn_link(node, get_irn_link(call));
			set_irn_link(call, node);
			break;

		default:
			break;
		}
	}
}

/**
 * Fix the list of collected nothrow Calls.
 *
 * @param irg        the graph that contained calls to pure functions
 * @param call_list  the list of all call sites of const functions
 */
static void fix_nothrow_call_list(ir_graph *irg, ir_node **call_list)
{
	bool exc_changed = false;

	/* Remove all exception Projs. */
	for (size_t i = ARR_LEN(call_list); i-- > 0;) {
		ir_node *const call = call_list[i];

		for (ir_node *next, *proj = (ir_node*)get_irn_link(call); proj != NULL; proj = next) {
			next = (ir_node*)get_irn_link(proj);

			/* kill any exception flow */
			switch (get_Proj_proj(proj)) {
			case pn_Call_X_except:
				exc_changed = true;
				exchange(proj, new_r_Bad(irg, mode_X));
				break;
			case pn_Call_X_regular: {
				ir_node *block = get_nodes_block(call);
				exc_changed = true;
				exchange(proj, new_r_Jmp(block));
				break;
			}
			default:
				break;
			}
		}

		hook_func_call(irg, call);
	}

	/* changes were done ... */
	if (exc_changed) {
		/* ... including exception edges */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                   | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	}
}

/* marking */
#define SET_IRG_READY(irg)  rbitset_set(ready_set, get_irg_idx(irg))
#define IS_IRG_READY(irg)   rbitset_is_set(ready_set, get_irg_idx(irg))
#define SET_IRG_BUSY(irg)   rbitset_set(busy_set, get_irg_idx(irg))
#define CLEAR_IRG_BUSY(irg) rbitset_clear(busy_set, get_irg_idx(irg))
#define IS_IRG_BUSY(irg)    rbitset_is_set(busy_set, get_irg_idx(irg))

/* forward */
static mtp_additional_properties check_const_or_pure_function(ir_graph *irg, bool top);

/**
 * Calculate the bigger property of two. Handle the temporary flag right.
 */
static mtp_additional_properties max_property(mtp_additional_properties a,
                                              mtp_additional_properties b)
{
	mtp_additional_properties t = (a | b) & mtp_temporary;
	a &= ~mtp_temporary;
	b &= ~mtp_temporary;

	if (a == mtp_no_property || b == mtp_no_property)
		return mtp_no_property;
	mtp_additional_properties r = MAX(a, b);
	return r | t;
}

/**
 * Follow the memory chain starting at node and determine
 * the mtp_property.
 *
 * @return mtp_property_const if only calls of const functions are detected
 *         mtp_property_pure  if only Loads and const/pure calls detected
 *         mtp_no_property    else
 */
static mtp_additional_properties follow_mem_(ir_node *node)
{
	mtp_additional_properties mode = mtp_property_const;

	for (;;) {
		if (mode == mtp_no_property)
			return mtp_no_property;

		if (irn_visited_else_mark(node))
			return mode;

		switch (get_irn_opcode(node)) {
		case iro_Proj:
			node = get_Proj_pred(node);
			break;

		case iro_NoMem:
			/* finish here */
			return mode;

		case iro_Phi:
		case iro_Sync:
			/* do a dfs search */
			foreach_irn_in_r(node, i, pred) {
				mtp_additional_properties const m = follow_mem_(pred);
				mode = max_property(mode, m);
				if (mode == mtp_no_property)
					return mtp_no_property;
			}
			return mode;

		case iro_Load:
			/* Beware volatile Loads are NOT allowed in pure functions. */
			if (get_Load_volatility(node) == volatility_is_volatile)
				return mtp_no_property;
			mode = max_property(mode, mtp_property_pure);
			node = get_Load_mem(node);
			break;

		case iro_Call: {
			/* A call is only tolerable if its either constant or pure. */
			ir_entity *callee = get_Call_callee(node);
			if (callee == NULL)
				return mtp_no_property;

			ir_graph *irg = get_entity_linktime_irg(callee);
			if (irg == NULL) {
				mtp_additional_properties m
					= get_entity_additional_properties(callee)
				    & (mtp_property_const|mtp_property_pure);
				mode = max_property(mode, m);
			} else {
				/* we have a graph, analyze it. */
				mtp_additional_properties m
					= check_const_or_pure_function(irg, false);
				mode = max_property(mode, m);
			}
			node = get_Call_mem(node);
			break;
		}

		default:
			if (is_irn_const_memory(node)) {
				node = get_memop_mem(node);
				break;
			}
			return mtp_no_property;
		}
	}
}

/**
 * Follow the memory chain starting at node and determine
 * the mtp_property.
 *
 * @return mtp_property_const if only calls of const functions are detected
 *         mtp_property_pure  if only Loads and const/pure calls detected
 *         mtp_no_property else
 */
static mtp_additional_properties follow_mem(ir_node *node, mtp_additional_properties mode)
{
	mtp_additional_properties m = follow_mem_(node);
	return max_property(mode, m);
}

/**
 * Check if a graph represents a const or a pure function.
 *
 * @param irg  the graph to check
 * @param top  if set, this is the top call
 */
static mtp_additional_properties check_const_or_pure_function(ir_graph *irg, bool top)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *type   = get_entity_type(entity);
	mtp_additional_properties may_be_const = mtp_property_const;
	mtp_additional_properties prop = get_entity_additional_properties(entity);

	/* libfirm handles aggregate parameters by passing around pointers to
	 * stuff in memory, so if we have compound parameters or return types
	 * we are never const */
	if (method_type_contains_aggregate(type)) {
		prop        &= ~mtp_property_const;
		may_be_const = mtp_no_property;
	}

	if (prop & mtp_property_const) {
		/* already marked as a const function */
		return mtp_property_const;
	}
	if (prop & mtp_property_pure) {
		/* already marked as a pure function */
		return mtp_property_pure;
	}

	if (IS_IRG_READY(irg)) {
		/* already checked */
		return mtp_no_property;
	}
	if (IS_IRG_BUSY(irg)) {
		/* We are still evaluate this method.
		 * The function (indirectly) calls itself and thus may not terminate. */
		return mtp_no_property;
	}
	SET_IRG_BUSY(irg);

	ir_node *end   = get_irg_end(irg);
	ir_node *endbl = get_nodes_block(end);
	prop = may_be_const;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	/* mark the initial mem: recursion of follow_mem() stops here */
	mark_irn_visited(get_irg_initial_mem(irg));

	/* visit every Return */
	for (int j = get_Block_n_cfgpreds(endbl); j-- > 0;) {
		ir_node  *node = get_Block_cfgpred(endbl, j);
		unsigned  code = get_irn_opcode(node);

		/* Bad nodes usually do NOT produce anything, so it's ok */
		if (code == iro_Bad)
			continue;

		if (code == iro_Return) {
			ir_node *mem = get_Return_mem(node);

			/* Bad nodes usually do NOT produce anything, so it's ok */
			if (is_Bad(mem))
				continue;

			if (mem != get_irg_initial_mem(irg))
				prop = max_property(prop, follow_mem(mem, prop));
		} else {
			/* Exception found. Cannot be const or pure. */
			prop = mtp_no_property;
			break;
		}
		if (prop == mtp_no_property)
			break;
	}

	if (prop != mtp_no_property) {
		/* check, if a keep-alive exists */
		for (int j = get_End_n_keepalives(end); j-- > 0;) {
			ir_node *kept = get_End_keepalive(end, j);

			if (is_Block(kept)) {
				prop = mtp_no_property;
				break;
			}

			if (mode_M != get_irn_mode(kept))
				continue;

			prop = max_property(prop, follow_mem(kept, prop));
			if (prop == mtp_no_property)
				break;
		}
	}

	if (top) {
		/* Set the property only if we are at top-level. */
		if (prop != mtp_no_property) {
			add_entity_additional_properties(entity, prop);
		}
		SET_IRG_READY(irg);
	}
	CLEAR_IRG_BUSY(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	return prop;
}

/**
 * Handle calls to const functions.
 *
 * @param ctx  context
 */
static void handle_const_Calls(env_t *ctx)
{
	/* all calls of const functions can be transformed */
	foreach_irp_irg(i, irg) {
		ctx->float_const_call_list    = NEW_ARR_F(ir_node*, 0);
		ctx->nonfloat_const_call_list = NEW_ARR_F(ir_node*, 0);
		ctx->pure_call_list           = NEW_ARR_F(ir_node*, 0);

		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		irg_walk_graph(irg, firm_clear_link, collect_const_and_pure_calls, ctx);
		fix_const_call_lists(irg, ctx);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

		DEL_ARR_F(ctx->pure_call_list);
		DEL_ARR_F(ctx->nonfloat_const_call_list);
		DEL_ARR_F(ctx->float_const_call_list);

		confirm_irg_properties(irg,
			IR_GRAPH_PROPERTIES_CONTROL_FLOW
			| IR_GRAPH_PROPERTY_ONE_RETURN
			| IR_GRAPH_PROPERTY_MANY_RETURNS);
	}
}

/**
 * Handle calls to nothrow functions.
 *
 * @param ctx  context
 */
static void handle_nothrow_Calls(env_t *ctx)
{
	/* all calls of const functions can be transformed */
	foreach_irp_irg(i, irg) {
		ctx->nothrow_call_list = NEW_ARR_F(ir_node*, 0);

		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		irg_walk_graph(irg, firm_clear_link, collect_nothrow_calls, ctx);

		fix_nothrow_call_list(irg, ctx->nothrow_call_list);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

		DEL_ARR_F(ctx->nothrow_call_list);
	}
}

/**
 * Check, whether a given node represents a return value of
 * a malloc like function (ie, new heap allocated memory).
 *
 * @param node  the node to check
 */
static bool is_malloc_call_result(const ir_node *node)
{
	/* TODO: check mtp_malloc */
	(void)node;
	return false;
}

/**
 * Update a property depending on a call property.
 */
static mtp_additional_properties update_property(mtp_additional_properties orig_prop, mtp_additional_properties call_prop)
{
	mtp_additional_properties t = (orig_prop | call_prop) & mtp_temporary;
	mtp_additional_properties r = orig_prop & call_prop;
	return r | t;
}

/**
 * Check if a node is stored.
 */
static bool is_stored(const ir_node *n)
{
	foreach_out_edge(n, edge) {
		const ir_node *succ = get_edge_src_irn(edge);

		switch (get_irn_opcode(succ)) {
		case iro_Return:
		case iro_Load:
		case iro_Cmp:
			/* ok */
			break;
		case iro_Store:
			if (get_Store_value(succ) == n)
				return true;
			/* ok if its only the address input */
			break;
		case iro_Sel:
		case iro_Confirm:
			if (is_stored(succ))
				return true;
			break;
		case iro_Call: {
			ir_entity *callee = get_Call_callee(succ);
			/* unknown call address */
			if (callee == NULL)
				return true;
			/* we know the called entity */
			for (size_t i = get_Call_n_params(succ); i > 0;) {
				if (get_Call_param(succ, --i) == n) {
					/* n is the i'th param of the call */
					if (get_method_param_access(callee, i) & ptr_access_store) {
						/* n is store in ent */
						return true;
					}
				}
			}
			break;
		}
		default:
			/* bad, potential alias */
			return true;
		}
	}
	return false;
}

/**
 * Check that the return value of an irg is not stored anywhere.
 *
 * return ~mtp_property_malloc if return values are stored, ~0 else
 */
static mtp_additional_properties check_stored_result(ir_graph *irg)
{
	ir_node *end_blk = get_irg_end_block(irg);
	mtp_additional_properties res = ~mtp_no_property;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	for (int i = get_Block_n_cfgpreds(end_blk); i-- > 0;) {
		ir_node *pred = get_Block_cfgpred(end_blk, i);

		if (!is_Return(pred))
			continue;
		for (size_t j = get_Return_n_ress(pred); j-- > 0;) {
			const ir_node *irn = get_Return_res(pred, j);

			if (is_stored(irn)) {
				/* bad, might create an alias */
				res = ~mtp_property_malloc;
				goto finish;
			}
		}
	}
finish:
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
	return res;
}

/**
 * Check if a graph represents a nothrow or a malloc function.
 *
 * @param irg  the graph to check
 * @param top  if set, this is the top call
 */
static mtp_additional_properties check_nothrow_or_malloc(ir_graph *irg, bool top)
{
	mtp_additional_properties curr_prop
		= mtp_property_malloc | mtp_property_nothrow;

	ir_entity *ent = get_irg_entity(irg);
	if (IS_IRG_READY(irg)) {
		/* already checked */
		return get_entity_additional_properties(ent);
	}
	if (IS_IRG_BUSY(irg)) {
		/* we are still evaluate this method. Be optimistic,
		return the best possible so far but mark the result as temporary. */
		return mtp_temporary | mtp_property_malloc | mtp_property_nothrow;
	}
	SET_IRG_BUSY(irg);

	ir_type *mtp = get_entity_type(ent);
	if (get_method_n_ress(mtp) <= 0)
		curr_prop &= ~mtp_property_malloc;

	ir_node *end_blk = get_irg_end_block(irg);
	for (int i = get_Block_n_cfgpreds(end_blk); i-- > 0;) {
		ir_node *pred = get_Block_cfgpred(end_blk, i);

		if (is_Return(pred)) {
			if (curr_prop & mtp_property_malloc) {
				/* check, if malloc is called here */
				for (size_t j = get_Return_n_ress(pred); j-- > 0;) {
					ir_node *res = get_Return_res(pred, j);

					/* skip Confirms */
					res = skip_HighLevel_ops(res);
					/* skip Proj's */
					while (is_Proj(res))
						res = get_Proj_pred(res);
					if (is_malloc_call_result(res)) {
						/* ok, this is a malloc */
					} else if (is_Call(res)) {
						ir_entity *callee = get_Call_callee(res);
						if (callee != NULL) {
							/* a direct call */
							ir_graph *callee_irg = get_entity_linktime_irg(callee);
							if (callee_irg == irg) {
								/* A self-recursive call. The property did not
								 * depend on this call. */
							} else if (callee_irg != NULL) {
								mtp_additional_properties prop
									= check_nothrow_or_malloc(callee_irg, false);
								curr_prop = update_property(curr_prop, prop);
							} else {
								mtp_additional_properties prop
									= get_entity_additional_properties(callee);
								curr_prop = update_property(curr_prop, prop);
							}
						} else {
							/* unknown call */
							curr_prop &= ~mtp_property_malloc;
						}
					} else {
						/* unknown return value */
						curr_prop &= ~mtp_property_malloc;
					}
				}
			}
		} else if (curr_prop & mtp_property_nothrow) {
			/* exception flow detected */
			pred = skip_Proj(pred);

			if (is_Call(pred)) {
				ir_entity *callee = get_Call_callee(pred);
				if (callee != NULL) {
					/* a direct call */
					ir_graph *called_irg = get_entity_linktime_irg(callee);
					if (called_irg == irg) {
						/* A self-recursive call. The property did not depend
						 * on this call. */
					} else if (called_irg != NULL) {
						/* Note: we check here for nothrow only, so do NOT
						 * reset the malloc property */
						mtp_additional_properties prop
							= check_nothrow_or_malloc(called_irg, false)
							| mtp_property_malloc;
						curr_prop = update_property(curr_prop, prop);
					} else {
						if ((get_entity_additional_properties(callee)
						     & mtp_property_nothrow) == 0)
							curr_prop &= ~mtp_property_nothrow;
					}
				} else {
					/* unknown call */
					curr_prop &= ~mtp_property_nothrow;
				}
			} else {
				/* real exception flow possible. */
				curr_prop &= ~mtp_property_nothrow;
			}
		}
		if ((curr_prop & ~mtp_temporary) == mtp_no_property) {
			/* no need to search further */
			break;
		}
	}

	if (curr_prop & mtp_property_malloc) {
		/* Note that the malloc property means not only return newly allocated
		 * memory, but also that this memory is ALIAS FREE.
		 * To ensure that, we do NOT allow that the returned memory is somewhere
		 * stored. */
		curr_prop &= check_stored_result(irg);
	}

	if (curr_prop != mtp_no_property
	    && (top || (curr_prop & mtp_temporary) == 0)) {
		/* We use the temporary flag here to mark an optimistic result.
		 * Set the property only if we are sure that it does NOT base on
		 * temporary results OR if we are at top-level. */
		add_entity_additional_properties(ent, curr_prop & ~mtp_temporary);
		SET_IRG_READY(irg);
	}
	if (top)
		SET_IRG_READY(irg);
	CLEAR_IRG_BUSY(irg);
	return curr_prop;
}

/**
 * When a function was detected as "const", it might be moved out of loops.
 * This might be dangerous if the graph can contain endless loops.
 */
static void check_for_possible_endless_loops(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

	ir_loop *loop = get_irg_loop(irg);
	for (size_t i = 0, n_elems = get_loop_n_elements(loop); i < n_elems; ++i) {
		loop_element e = get_loop_element(loop, i);
		if (*e.kind == k_ir_loop) {
			ir_entity *ent = get_irg_entity(irg);
			add_entity_additional_properties(ent, mtp_property_has_loop);
			break;
		}
	}

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_ALL);
}

void optimize_funccalls(void)
{
	/* prepare: mark all graphs as not analyzed */
	size_t last_idx = get_irp_last_idx();
	ready_set = rbitset_malloc(last_idx);
	busy_set  = rbitset_malloc(last_idx);

	/* first step: detect, which functions are nothrow or malloc */
	DB((dbg, LEVEL_2, "Detecting nothrow and malloc properties ...\n"));
	foreach_irp_irg(i, irg) {
		unsigned const prop = check_nothrow_or_malloc(irg, true);
		if (prop & mtp_property_nothrow) {
			DB((dbg, LEVEL_2, "%+F has the nothrow property\n", irg));
		} else if (prop & mtp_property_malloc) {
			DB((dbg, LEVEL_2, "%+F has the malloc property\n", irg));
		}
	}

	/* second step: remove exception edges: this must be done before the
	   detection of const and pure functions take place. */
	env_t ctx;
	handle_nothrow_Calls(&ctx);

	rbitset_clear_all(ready_set, last_idx);
	rbitset_clear_all(busy_set, last_idx);

	/* third step: detect, which functions are const or pure */
	DB((dbg, LEVEL_2, "Detecting const and pure properties ...\n"));
	foreach_irp_irg(i, irg) {
		unsigned const prop = check_const_or_pure_function(irg, true);
		if (prop & mtp_property_const) {
			DB((dbg, LEVEL_2, "%+F has the const property\n", irg));
			check_for_possible_endless_loops(irg);
		} else if (prop & mtp_property_pure) {
			DB((dbg, LEVEL_2, "%+F has the pure property\n", irg));
		}
	}

	handle_const_Calls(&ctx);

	free(busy_set);
	free(ready_set);
}

void firm_init_funccalls(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.funccalls");
}
