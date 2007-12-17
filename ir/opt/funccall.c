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
 * @brief   Optimization of function calls.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <adt/raw_bitset.h>

#include "funccall_t.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irvrfy.h"
#include "dbginfo_t.h"
#include "irflag_t.h"
#include "ircons.h"
#include "irhooks.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * The walker environment for rem_mem_from_const_fkt_calls
 */
typedef struct _env_t {
	unsigned n_calls_SymConst;
	unsigned n_calls_Sel;
	ir_node  *const_call_list;       /**< The list of all const function calls that will be changed. */
	ir_node  *pure_call_list;        /**< The list of all pure function calls that will be changed. */
	ir_node  *proj_list;             /**< The list of all potential Proj nodes that must be fixed. */
} env_t;

/** Ready IRG's are marked in the ready set. */
static unsigned *ready_set;

/** IRG's that are in progress are marked here. */
static unsigned *busy_set;

/**
 * We misuse the mtp_temporary flag as temporary here.
 * The is ok, as we cannot set or get it anyway.
 */
#define mtp_temporary  mtp_property_inherited

/**
 * Collect all calls to const and pure functions
 * to lists. Collect all Proj(Call) nodes into a Proj list.
 */
static void collect_calls(ir_node *node, void *env) {
	env_t *ctx = env;
	ir_node *call, *ptr;
	ir_entity *ent;
	unsigned mode;

	if (is_Call(node)) {
		call = node;

		/* set the link to NULL for all non-const/pure calls */
		set_irn_link(call, NULL);
		ptr = get_Call_ptr(call);
		if (is_SymConst(ptr) && get_SymConst_kind(ptr) == symconst_addr_ent) {
			ent = get_SymConst_entity(ptr);

			mode = get_entity_additional_properties(ent);
			if ((mode & (mtp_property_const|mtp_property_pure)) == 0)
				return;
			++ctx->n_calls_SymConst;
		} else if (get_opt_closed_world() &&
		           is_Sel(ptr) &&
		           get_irg_callee_info_state(current_ir_graph) == irg_callee_info_consistent) {
			/* If all possible callees are const functions, we can remove the memory edge. */
			int i, n_callees = get_Call_n_callees(call);
			if (n_callees == 0) {
				/* This is kind of strange:  dying code or a Call that will raise an exception
				   when executed as there is no implementation to call.  So better not
				   optimize. */
				return;
			}

			/* note that const function are a subset of pure ones */
			mode = mtp_property_const | mtp_property_pure;
			for (i = 0; i < n_callees; ++i) {
				ent = get_Call_callee(call, i);
				if (ent == unknown_entity) {
					/* we don't know which entity is called here */
					return;
				}
				mode &= get_entity_additional_properties(ent);
				if (mode == 0)
					return;
			}
			++ctx->n_calls_Sel;
		} else
			return;

		/* ok, if we get here we found a call to a const or a pure function */
		if (mode & mtp_property_pure) {
			set_irn_link(call, ctx->pure_call_list);
			ctx->pure_call_list = call;
		} else {
			set_irn_link(call, ctx->const_call_list);
			ctx->const_call_list = call;
		}
	} else if (is_Proj(node)) {
		/*
		 * Collect all memory and exception Proj's from
		 * calls.
		 */
		call = get_Proj_pred(node);
		if (! is_Call(call))
			return;

		/* collect the Proj's in the Proj list */
		switch (get_Proj_proj(node)) {
		case pn_Call_M_regular:
		case pn_Call_X_except:
		case pn_Call_M_except:
			set_irn_link(node, ctx->proj_list);
			ctx->proj_list = node;
			break;
		default:
			break;
		}
	}
}  /* collect_calls */

/**
 * Fix the list of collected Calls.
 *
 * @param irg        the graph that contained calls to pure functions
 * @param call_list  the list of all call sites of const functions
 * @param proj_list  the list of all memory/exception Proj's of this call sites
 */
static void fix_const_call_list(ir_graph *irg, ir_node *call_list, ir_node *proj_list) {
	ir_node *call, *next, *mem, *proj;
	int exc_changed = 0;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	/* First step: fix all calls by removing it's memory input.
	   It's original memory input is preserved in their link fields. */
	for (call = call_list; call; call = next) {
		next = get_irn_link(call);
		mem  = get_Call_mem(call);

		set_irn_link(call, mem);
		set_Call_mem(call, get_irg_no_mem(irg));

		/*
		 * Sorrily we cannot simply set the node to 'float'.
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

		/* finally, this call can float
		set_irn_pinned(call, op_pin_state_floats); */
		hook_func_call(irg, call);
	}

	/* Second step: fix all Proj's */
	for (proj = proj_list; proj; proj = next) {
		next = get_irn_link(proj);
		call = get_Proj_pred(proj);
		mem  = get_irn_link(call);

		/* beware of calls in the pure call list */
		if (! mem || get_irn_op(mem) == op_Call)
			continue;
		assert(get_irn_mode(mem) == mode_M);

		switch (get_Proj_proj(proj)) {
		case pn_Call_M_regular: {
			/* in dead code there might be cycles where proj == mem */
			if (proj != mem)
				exchange(proj, mem);
		} break;
		case pn_Call_X_except:
		case pn_Call_M_except:
			exc_changed = 1;
			exchange(proj, get_irg_bad(irg));
			break;
		default:
			;
		}
	}

	/* changes were done ... */
	set_irg_outs_inconsistent(irg);
	set_irg_loopinfo_state(irg, loopinfo_cf_inconsistent);

	if (exc_changed) {
		/* ... including exception edges */
		set_irg_doms_inconsistent(irg);
	}
	current_ir_graph = rem;
}  /* fix_call_list */

/* marking */
#define SET_IRG_READY(irg)	rbitset_set(ready_set, get_irg_idx(irg))
#define IS_IRG_READY(irg)   rbitset_is_set(ready_set, get_irg_idx(irg))
#define SET_IRG_BUSY(irg)   rbitset_set(busy_set, get_irg_idx(irg))
#define CLEAR_IRG_BUSY(irg) rbitset_clear(busy_set, get_irg_idx(irg))
#define IS_IRG_BUSY(irg)    rbitset_is_set(busy_set, get_irg_idx(irg))

/* forward */
static unsigned check_const_or_pure_function(ir_graph *irg, int top);

/**
 * Calculate the bigger mode of two. Handle the temporary flag right.
 */
static unsigned mode_max(unsigned a, unsigned b) {
	unsigned r, t = (a | b) & mtp_temporary;
	a &= ~mtp_temporary;
	b &= ~mtp_temporary;

	if (a == mtp_no_property || b == mtp_no_property)
		return mtp_no_property;
	r = a > b ? a : b;
	return r | t;
}

/**
 * Follow the memory chain starting at node and determine
 * the mtp_property.
 *
 * @return mtp_property_const if only calls of const functions are detected
 *         mtp_property_pure if only Loads and const/pure
 *         calls detected
 *         mtp_no_property else
 */
static unsigned _follow_mem(ir_node *node) {
	unsigned m, mode = mtp_property_const;
	ir_node  *ptr;
	int i;

	for (;;) {
		if (mode == mtp_no_property)
			return mtp_no_property;

		if (irn_visited(node))
			return mode;

		mark_irn_visited(node);

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
			for (i = get_irn_arity(node) - 1; i >= 0; --i) {
				m    = _follow_mem(get_irn_n(node, i));
				mode = mode_max(mode, m);
				if (mode == mtp_no_property)
					return mtp_no_property;
			}
			return mode;

		case iro_Load:
			/* Beware volatile Loads are NOT allowed in pure functions. */
			if (get_Load_volatility(node) == volatility_is_volatile)
				return mtp_no_property;
			mode = mode_max(mode, mtp_property_pure);
			node = get_Load_mem(node);
			break;

		case iro_Call:
			/* A call is only tolerable if its either constant or pure. */
			ptr = get_Call_ptr(node);
			if (get_irn_op(ptr) == op_SymConst &&
				get_SymConst_kind(ptr) == symconst_addr_ent) {
				ir_entity *ent = get_SymConst_entity(ptr);
				ir_graph  *irg = get_entity_irg(ent);

				if (irg == current_ir_graph) {
					/* A self-recursive call. The mode did not depend on this call. */
				} else if (irg == NULL) {
					m = get_entity_additional_properties(ent) & (mtp_property_const|mtp_property_pure);
					mode = mode_max(mode, m);
				} else if (irg != NULL) {
					/* we have a graph, analyze it. */
					m = check_const_or_pure_function(irg, /*top=*/0);
					mode = mode_max(mode, m);
				}
			} else
				return mtp_no_property;
			node = get_Call_mem(node);
			break;

		default:
			return mtp_no_property;
		}
	}
}  /* _follow_mem */

/**
 * Follow the memory chain starting at node and determine
 * the mtp_property.
 *
 * @return mtp_property_const if only calls of const functions are detected
 *         mtp_property_pure  if only Loads and const/pure calls detected
 *         mtp_no_property else
 */
static unsigned follow_mem(ir_node *node, unsigned mode) {
	unsigned m;

	m = _follow_mem(node);
	return mode_max(mode, m);
}  /* follow_mem */

/*
 * Check if a graph represents a const function.
 *
 * @param irg  the graph
 * @param top  top call
 */
static unsigned check_const_or_pure_function(ir_graph *irg, int top) {
	ir_node *end, *endbl;
	int j;
	unsigned mode = get_irg_additional_properties(irg);
	ir_graph *rem = current_ir_graph;

	if (mode & mtp_property_const) {
		/* already marked as a const function */
		return mtp_property_const;
	}
	if (mode & mtp_property_pure) {
		/* already marked as a pure function */
		return mtp_property_pure;
	}

	if (IS_IRG_READY(irg)) {
		/* already checked */
		return mtp_no_property;
	}
	if (IS_IRG_BUSY(irg)) {
		/* we are still evaluate this method. Be optimistic,
		   return the best possible so far but mark the result as temporary. */
		return mtp_temporary | mtp_property_const;
	}
	SET_IRG_BUSY(irg);

	end   = get_irg_end(irg);
	endbl = get_nodes_block(end);
	mode  = mtp_property_const;

	current_ir_graph = irg;

	inc_irg_visited(irg);
	/* mark the initial mem: recursion of follow_mem stops here */
	mark_irn_visited(get_irg_initial_mem(irg));

	/* visit every Return */
	for (j = get_Block_n_cfgpreds(endbl) - 1; j >= 0; --j) {
		ir_node *node = get_Block_cfgpred(endbl, j);
		ir_op   *op   = get_irn_op(node);
		ir_node *mem;

		/* Bad nodes usually do NOT produce anything, so it's ok */
		if (op == op_Bad)
			continue;

		if (op == op_Return) {
			mem = get_Return_mem(node);

			/* Bad nodes usually do NOT produce anything, so it's ok */
			if (is_Bad(mem))
				continue;

			if (mem != get_irg_initial_mem(irg))
				mode = mode_max(mode, follow_mem(mem, mode));
		} else {
			/* Exception found. Cannot be const or pure. */
			mode = mtp_no_property;
			break;
		}
		if (mode == mtp_no_property)
			break;
	}

	if (mode != mtp_no_property) {
		/* check, if a keep-alive exists */
		for (j = get_End_n_keepalives(end) - 1; j >= 0; --j) {
			ir_node *mem = get_End_keepalive(end, j);

			if (mode_M != get_irn_mode(mem))
				continue;

			mode = mode_max(mode, follow_mem(mem, mode));
			if (mode == mtp_no_property)
				break;
		}
	}

	if (mode != mtp_no_property) {
		if (top || (mode & mtp_temporary) == 0) {
			/* We use the temporary flag here to mark optimistic result.
			   Set the property only if we are sure that it does NOT base on
			   temporary results OR if we are at top-level. */
			set_irg_additional_property(irg, mode & ~mtp_temporary);
			SET_IRG_READY(irg);
		}
	}
	if (top)
		SET_IRG_READY(irg);
	CLEAR_IRG_BUSY(irg);
	current_ir_graph = rem;
	return mode;
}  /* check_const_or_pure_function */

/**
 * Handle calls to const functions.
 */
static void handle_const_Calls(env_t *ctx) {
	int i;

	ctx->n_calls_SymConst = 0;
	ctx->n_calls_Sel      = 0;

	/* all calls of const functions can be transformed */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg  = get_irp_irg(i);

		ctx->const_call_list = NULL;
		ctx->pure_call_list  = NULL;
		ctx->proj_list = NULL;
		irg_walk_graph(irg, NULL, collect_calls, ctx);

		if (ctx->const_call_list)
			fix_const_call_list(irg, ctx->const_call_list, ctx->proj_list);
	}
}  /* handle_const_Calls */

/*
 * optimize function calls by handling const functions
 */
void optimize_funccalls(int force_run)
{
	int i, n;
	unsigned num_const = 0;
	unsigned num_pure  = 0;

	/* prepare: mark all graphs as not analyzed */
	n = get_irp_last_idx();
	ready_set = rbitset_malloc(n);
	busy_set  = rbitset_malloc(n);

	/* first step: detect, which functions are const, i.e. do NOT touch any memory */
	DBG((dbg, LEVEL_2, "Detecting const and pure properties ...\n"));
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);
		unsigned mode = check_const_or_pure_function(irg, /*top=*/1);

		if (mode & mtp_property_const) {
			++num_const;
			DBG((dbg, LEVEL_2, "%+F has the const property\n", irg));
		} else if (mode & mtp_property_pure) {
			++num_pure;
			DBG((dbg, LEVEL_2, "%+F has the pure property\n", irg));
		}
	}

	if (force_run || num_const > 0) {
		env_t ctx;

		handle_const_Calls(&ctx);
		if (get_firm_verbosity()) {
			DBG((dbg, LEVEL_1, "Detected %u const graphs, %u pure graphs.\n", num_const, num_pure));
			DBG((dbg, LEVEL_1, "Optimizes %u(SymConst) + %u(Sel) calls to const functions.\n",
			       ctx.n_calls_SymConst, ctx.n_calls_Sel));
		}
	} else {
		DBG((dbg, LEVEL_1, "No graphs without side effects detected\n"));
	}
	xfree(busy_set);
	xfree(ready_set);
}  /* optimize_funccalls */

/* initialize the funccall optimization */
void firm_init_funccalls(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.funccalls");
}  /* firm_init_funccalls */
