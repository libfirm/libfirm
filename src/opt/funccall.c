/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimization of function calls.
 * @author  Michael Beck
 */
#include "analyze_irg_args.h"
#include "dbginfo_t.h"
#include "debug.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irprog_t.h"
#include "irtools.h"
#include "opt_init.h"
#include "panic.h"
#include "raw_bitset.h"
#include "util.h"
#include <stdbool.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct env {
	ir_node                   **call_list;
	mtp_additional_properties   filter_property;
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
 * Walker: Collect all calls with a given property to lists.
 * Collect all Proj(Call) nodes into a Proj list.
 */
static void collect_calls(ir_node *node, void *ctx)
{
	env_t *env = (env_t *)ctx;

	if (is_Call(node)) {
		ir_type   *type   = get_Call_type(node);
		unsigned   prop   = get_method_additional_properties(type);
		ir_entity *callee = get_Call_callee(node);
		if (callee != NULL) {
			prop |= get_entity_additional_properties(callee);
		}

		mtp_additional_properties filter_property = env->filter_property;
		if ((filter_property & ~prop) == 0) {
			ARR_APP1(ir_node*, env->call_list, node);
		}
	} else if (is_Proj(node)) {
		/* Collect all memory and exception Proj's from
		 * calls. */
		ir_node *call = get_Proj_pred(node);
		if (!is_Call(call))
			return;

		/* collect the Proj's in the Proj list */
		switch (get_Proj_num(node)) {
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
 * @param irg        the graph that contained calls to const functions
 * @param call_list  the list of all call sites of const functions
 */
static void fix_const_call_lists(ir_graph *irg, ir_node **pure_call_list)
{
	/* Fix all calls by removing their memory input, let them float and fix
	 * their Projs. */
	bool exc_changed = false;
	for (size_t i = ARR_LEN(pure_call_list); i-- > 0;) {
		ir_node *const call = pure_call_list[i];
		/* currently we cannot change the call if it contains aggregate
		 * parameters or results (as firm uses memory with known addresses
		 * to transfer them. */
		ir_type *const type = get_Call_type(call);
		if (method_type_contains_aggregate(type))
			continue;

		for (ir_node *next, *proj = (ir_node*)get_irn_link(call); proj != NULL;
		     proj = next) {
			next = (ir_node*)get_irn_link(proj);

			switch (get_Proj_num(proj)) {
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

		/* finally, this call can float */
		set_irn_pinned(call, false);
	}

	if (exc_changed) {
		/* ... including exception edges */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		                   | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
	}
}

/**
 * Fix the list of collected nothrow Calls.
 *
 * @param irg        the graph that contained calls to nothrow functions
 * @param call_list  the list of all call sites of nothrow functions
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
			switch (get_Proj_num(proj)) {
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

static mtp_additional_properties analyze_irg(ir_graph *irg);

/**
 * Performs a depth-first traversal through the CFG to detect loops. We use 2
 * visited flags for this:
 * block_visited marks blocks that we have already checked before (but not
 * necessarily on the currently followed path in the search.
 * irn_visited is only set for blocks that are on the current path
 *   (a parent has_loop call on the stack works on it)
 * We have found a loop if we hit a block with irn_visited set.
 */
static bool has_loop(ir_node *cfgpred)
{
	ir_node *block = get_nodes_block(cfgpred);
	if (Block_block_visited(block)) {
		/* if the block is on the current path, then we have a loop */
		return irn_visited(block);
	}

	mark_Block_block_visited(block); /* block visited */
	mark_irn_visited(block);         /* block is on current path */

	foreach_irn_in(block, i, pred) {
		if (is_Bad(pred))
			continue;
		if (has_loop(pred))
			return true;
	}

	/* block is not on current path anymore */
	set_irn_visited(block, get_irg_visited(get_irn_irg(cfgpred))-1);
	return false;
}

/** Check if a function always terminates or never returns.
 * This function returns mtp_property_terminates if the graph contains no
 * control flow loops. Note that this is preliminary, it may still contain calls
 * to functions which do not terminate. */
static mtp_additional_properties check_termination(ir_graph *irg)
{
	/* the visited flags are used by has_loop */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED
	                        | IR_RESOURCE_IRN_VISITED);
	inc_irg_block_visited(irg);
	inc_irg_visited(irg);

	bool found_return = false;
	bool found_loop   = false;
	ir_node *end_block = get_irg_end_block(irg);
	foreach_irn_in(end_block, i, pred) {
		if (is_Bad(pred))
			continue;
		found_return = true;
		if (has_loop(pred)) {
			found_loop = true;
			break;
		}
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED | IR_RESOURCE_IRN_VISITED);

	if (!found_return)
		return mtp_property_noreturn;
	if (!found_loop)
		return mtp_property_terminates; /* TODO: disallow recursion */
	return mtp_no_property;
}

/**
 * Follow the memory chain starting at node and determine
 * the mtp_property.
 *
 * @return mtp_property_pure  if only Loads and pure calls detected
 *         mtp_no_property    otherwise
 */
static mtp_additional_properties follow_mem(ir_node *node,
                                            const mtp_additional_properties min_prop,
                                            mtp_additional_properties max_prop)
{
	do {
		if (irn_visited_else_mark(node))
			return max_prop;

		switch (get_irn_opcode(node)) {
		case iro_Proj:
			node = get_Proj_pred(node);
			continue;

		case iro_Start:
		case iro_NoMem:
			return max_prop;

		case iro_Phi:
		case iro_Sync:
			/* do a dfs search */
			foreach_irn_in_r(node, i, pred) {
				max_prop &= follow_mem(pred, min_prop, max_prop);
				if ((max_prop & ~min_prop) == mtp_no_property)
					return max_prop;
			}
			return max_prop;

		case iro_Store:
			max_prop &= ~(mtp_property_no_write | mtp_property_pure);
			node = get_Store_mem(node);
			break;

		case iro_Load:
			/* Beware volatile Loads are NOT allowed in pure functions. */
			max_prop &= ~mtp_property_pure;
			node = get_Load_mem(node);
			break;

		case iro_Builtin: {
			ir_builtin_kind kind = get_Builtin_kind(node);
			node = get_Builtin_mem(node);
			switch (kind) {
			case ir_bk_debugbreak:
			case ir_bk_trap:
				/* abort function in a special way -> no clean termination */
				max_prop &= ~mtp_property_terminates;
				break;
			case ir_bk_return_address:
			case ir_bk_frame_address:
				/* Access context information => not pure anymore */
				max_prop &= ~mtp_property_pure;
				break;
			case ir_bk_prefetch:
			case ir_bk_ffs:
			case ir_bk_clz:
			case ir_bk_ctz:
			case ir_bk_popcount:
			case ir_bk_parity:
			case ir_bk_bswap:
			case ir_bk_saturating_increment:
			case ir_bk_may_alias:
				/* just arithmetic/no semantic change => no problem */
				continue;
			case ir_bk_compare_swap:
				/* write access */
				max_prop &= ~(mtp_property_pure | mtp_property_no_write);
				break;
			case ir_bk_inport:
			case ir_bk_outport:
				/* anything can happen when accessing periphery... */
			case ir_bk_va_start:
			case ir_bk_va_arg:
				/* The va_list parameter might not be local to the function. */
				return mtp_no_property;
			}
			break;
		}

		case iro_Call: {
			/* A call is only tolerable if it is pure. */
			ir_type *type = get_Call_type(node);
			mtp_additional_properties callprops
				= get_method_additional_properties(type);
			/* shortcut */
			if ((max_prop & callprops) != max_prop) {
				ir_entity *callee = get_Call_callee(node);
				if (callee == NULL)
					return mtp_no_property;
				ir_graph *irg = get_entity_linktime_irg(callee);
				if (irg == NULL)
					return mtp_no_property;
				/* recursively analyze graph, unless we found a loop in which case
				 * we can't guarantee termination and have to live with the
				 * currently set flags */
				if (IS_IRG_BUSY(irg)) {
					mtp_additional_properties entprops
						= get_entity_additional_properties(callee);
					if (entprops & mtp_property_terminates) {
						entprops &= ~mtp_property_terminates;
						set_entity_additional_properties(callee, entprops);
					}
					max_prop &= entprops;
				} else {
					max_prop &= analyze_irg(irg);
				}
			}
			node = get_Call_mem(node);
			break;
		}

		default:
			if (is_irn_const_memory(node)) {
				node = get_memop_mem(node);
				continue;
			}
			return mtp_no_property;
		}
	} while ((max_prop & ~min_prop) != mtp_no_property);
	return max_prop;
}

/**
 * Check if a graph may be a pure function. This checks memory
 * accesses as well as calls to other functions. It does not check for
 * potentially endless loops yet.
 *
 * @param irg  the graph to check
 */
static mtp_additional_properties analyze_irg(ir_graph *irg)
{
	assert(!IS_IRG_BUSY(irg));
	ir_entity *entity = get_irg_entity(irg);
	const mtp_additional_properties prop
		= get_entity_additional_properties(entity);
	/* already checked? */
	if (IS_IRG_READY(irg))
		return prop;

	/* the minimum are the properties that are guaranteed by the programmer
	 * or previous analysis passes. We can't get worse than that. */
	const mtp_additional_properties min_prop
		= prop & (mtp_property_no_write | mtp_property_pure
		         | mtp_property_terminates);
	/* The maximum is the most precise properties we could achieve by our
	 * current knowledge, we'll remove properties from the max_prop until we
	 * reach min_prop or can't find any more reason to remove a property. */
	mtp_additional_properties max_prop
		= mtp_property_pure | mtp_property_no_write | mtp_property_terminates;
	/* we can stop if we can't get better than the minimum anyway */
	if ((max_prop & ~min_prop) == mtp_no_property)
		goto early_finish;

	/* check for termination and loops */
	const mtp_additional_properties termination_props = check_termination(irg);
	/* we can immediately set the noreturn property, as it does not depend on
	 * recursive function calls, bypassing the min_prop/max_prop logic */
	if (termination_props & mtp_property_noreturn) {
		DB((dbg, LEVEL_2, "%+F: set mtp_property_noreturn\n", irg));
		add_entity_additional_properties(entity, mtp_property_noreturn);
	}

	/* if check_termination found no loop then we can possibly add
	 * mtp_property_terminates */
	if (!(termination_props & mtp_property_terminates))
		max_prop &= ~mtp_property_terminates;
	/* we can stop if we can't get better than the minimum anyway */
	if ((max_prop & ~min_prop) == mtp_no_property)
		goto early_finish;

	/* be optimistic and set maximum properties now for the case of a loop
	 * in the callgraph */
	SET_IRG_BUSY(irg);
	set_entity_additional_properties(entity, prop | max_prop);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	/* visit every memory chain */
	ir_node *endbl = get_irg_end_block(irg);
	foreach_irn_in(endbl, i, pred) {
		if (is_Bad(pred))
			continue;
		if (is_Return(pred)) {
			ir_node *mem = get_Return_mem(pred);
			max_prop &= follow_mem(mem, min_prop, max_prop);
		} else if (is_Raise(pred)) {
			ir_node *mem = get_Raise_mem(pred);
			max_prop &= ~mtp_property_terminates;
			max_prop &= follow_mem(mem, min_prop, max_prop);
		} else {
			panic("unexpected end block predecessor %+F", pred);
		}
		/* if we can't get better than min_prop anymore, abort */
		if ((max_prop & ~min_prop) == mtp_no_property)
			goto finish;
	}
	ir_node *end = get_irg_end(irg);
	/* check, if a memory keep-alive exists */
	for (int j = get_End_n_keepalives(end); j-- > 0;) {
		ir_node *kept = get_End_keepalive(end, j);
		if (is_Bad(kept))
			continue;
		if (get_irn_mode(kept) != mode_M && !is_memop(kept))
			continue;

		max_prop &= follow_mem(kept, min_prop, max_prop);
		/* if we can't get better than min_prop anymore, abort */
		if ((max_prop & ~min_prop) == mtp_no_property)
			goto finish;
	}

finish:
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
early_finish:
	CLEAR_IRG_BUSY(irg);
	SET_IRG_READY(irg);
	const mtp_additional_properties final_prop = prop | max_prop;
	set_entity_additional_properties(entity, final_prop);

	const mtp_additional_properties new_prop = max_prop & ~prop;
	if (new_prop & mtp_property_pure) {
		assert(final_prop & mtp_property_no_write);
		DB((dbg, LEVEL_2, "%+F: set mtp_property_pure\n", irg));
	}
	if (new_prop & mtp_property_no_write)
		DB((dbg, LEVEL_2, "%+F: set mtp_property_no_write\n", irg));
	if (new_prop & mtp_property_noreturn)
		DB((dbg, LEVEL_2, "%+F: set mtp_property_noreturn\n", irg));
	if (new_prop & mtp_property_terminates)
		DB((dbg, LEVEL_2, "%+F: set mtp_property_terminates\n", irg));

	return final_prop;
}

/**
 * Handle calls to const functions.
 */
static void handle_const_Calls(void)
{
	/* all calls of pure functions can be transformed */
	foreach_irp_irg(i, irg) {
		env_t env = {
			.call_list       = NEW_ARR_F(ir_node*, 0),
			.filter_property = mtp_property_pure | mtp_property_terminates,
		};
		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		irg_walk_graph(irg, firm_clear_link, collect_calls, &env);
		fix_const_call_lists(irg, env.call_list);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

		DEL_ARR_F(env.call_list);

		confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW
		                          | IR_GRAPH_PROPERTY_ONE_RETURN
		                          | IR_GRAPH_PROPERTY_MANY_RETURNS);
	}
}

/**
 * Handle calls to nothrow functions.
 */
static void handle_nothrow_Calls(void)
{
	/* all calls of nothrow functions can be transformed */
	foreach_irp_irg(i, irg) {
		env_t env = {
			.call_list       = NEW_ARR_F(ir_node*, 0),
			.filter_property = mtp_property_nothrow,
		};
		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
		irg_walk_graph(irg, firm_clear_link, collect_calls, &env);

		fix_nothrow_call_list(irg, env.call_list);
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

		DEL_ARR_F(env.call_list);
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
static mtp_additional_properties update_property(mtp_additional_properties orig_prop,
                                                 mtp_additional_properties call_prop)
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
					res = skip_Confirm(res);
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

void optimize_funccalls(void)
{
	/* prepare: mark all graphs as not analyzed */
	size_t last_idx = get_irp_last_idx();
	ready_set = rbitset_malloc(last_idx);
	busy_set  = rbitset_malloc(last_idx);

	/* first step: detect, which functions are nothrow or malloc */
	foreach_irp_irg(i, irg) {
		const mtp_additional_properties prop
			= check_nothrow_or_malloc(irg, true);
		if (prop & mtp_property_nothrow) {
			DB((dbg, LEVEL_2, "%+F: set mtp_property_nothrow\n", irg));
		} else if (prop & mtp_property_malloc) {
			DB((dbg, LEVEL_2, "%+F: set mtp_property_malloc\n", irg));
		}
	}

	/* second step: remove exception edges: this must be done before the
	   detection of pure functions take place. */
	handle_nothrow_Calls();

	rbitset_clear_all(ready_set, last_idx);
	rbitset_clear_all(busy_set, last_idx);

	/* third step: detect, which functions are const */
	foreach_irp_irg(i, irg) {
		analyze_irg(irg);
	}
	handle_const_Calls();

	free(busy_set);
	free(ready_set);
}

void firm_init_funccalls(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.funccalls");
}
