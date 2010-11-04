/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief    Dead node elimination and Procedure Inlining.
 * @author   Michael Beck, Goetz Lindenmaier
 * @version  $Id$
 */
#include "config.h"

#include <limits.h>
#include <stdbool.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"

#include "iroptimize.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "array_t.h"
#include "list.h"
#include "pset.h"
#include "pmap.h"
#include "pdeq.h"
#include "xmalloc.h"
#include "pqueue.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
#include "opt_init.h"
#include "cgana.h"
#include "trouts.h"
#include "error.h"

#include "analyze_irg_args.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "iropt_dbg.h"
#include "irpass_t.h"
#include "irphase_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/*------------------------------------------------------------------*/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/*------------------------------------------------------------------*/

/**
 * Remember the new node in the old node by using a field all nodes have.
 */
static void set_new_node(ir_node *node, ir_node *new_node)
{
	set_irn_link(node, new_node);
}

/**
 * Get this new node, before the old node is forgotten.
 */
static inline ir_node *get_new_node(ir_node *old_node)
{
	assert(irn_visited(old_node));
	return (ir_node*) get_irn_link(old_node);
}

/*--------------------------------------------------------------------*/
/*  Functionality for inlining                                         */
/*--------------------------------------------------------------------*/

/**
 * Copy node for inlineing.  Updates attributes that change when
 * inlineing but not for dead node elimination.
 *
 * Copies the node by calling copy_node() and then updates the entity if
 * it's a local one.  env must be a pointer of the frame type of the
 * inlined procedure. The new entities must be in the link field of
 * the entities.
 */
static void copy_node_inline(ir_node *node, void *env)
{
	ir_graph *new_irg  = (ir_graph*) env;
	ir_node  *new_node = irn_copy_into_irg(node, new_irg);

	set_new_node(node, new_node);
	if (is_Sel(node)) {
		ir_graph  *old_irg        = get_irn_irg(node);
		ir_type   *old_frame_type = get_irg_frame_type(old_irg);
		ir_entity *old_entity     = get_Sel_entity(node);
		assert(is_Sel(new_node));
		/* use copied entities from the new frame */
		if (get_entity_owner(old_entity) == old_frame_type) {
			ir_entity *new_entity = (ir_entity*)get_entity_link(old_entity);
			assert(new_entity != NULL);
			set_Sel_entity(new_node, new_entity);
		}
	} else if (is_Block(new_node)) {
		new_node->attr.block.irg.irg = new_irg;
	}
}

static void set_preds_inline(ir_node *node, void *env)
{
	ir_node *new_node;

	irn_rewire_inputs(node);

	/* move constants into start block */
	new_node = get_new_node(node);
	if (is_irn_constlike(new_node)) {
		ir_graph *new_irg     = (ir_graph *) env;
		ir_node  *start_block = get_irg_start_block(new_irg);
		set_nodes_block(new_node, start_block);
	}
}

/**
 * Walker: checks if P_value_arg_base is used.
 */
static void find_addr(ir_node *node, void *env)
{
	bool *allow_inline = (bool*)env;

	if (is_Sel(node)) {
		ir_graph *irg = current_ir_graph;
		if (get_Sel_ptr(node) == get_irg_frame(irg)) {
			/* access to frame */
			ir_entity *ent = get_Sel_entity(node);
			if (get_entity_owner(ent) != get_irg_frame_type(irg)) {
				/* access to value_type */
				*allow_inline = false;
			}
		}
	} else if (is_Alloc(node) && get_Alloc_where(node) == stack_alloc) {
		/* From GCC:
		 * Refuse to inline alloca call unless user explicitly forced so as this
		 * may change program's memory overhead drastically when the function
		 * using alloca is called in loop.  In GCC present in SPEC2000 inlining
		 * into schedule_block cause it to require 2GB of ram instead of 256MB.
		 *
		 * Sorrily this is true with our implementation also.
		 * Moreover, we cannot differentiate between alloca() and VLA yet, so
		 * this disables inlining of functions using VLA (which are completely
		 * save).
		 *
		 * 2 Solutions:
		 * - add a flag to the Alloc node for "real" alloca() calls
		 * - add a new Stack-Restore node at the end of a function using
		 *   alloca()
		 */
		*allow_inline = false;
	}
}

/**
 * Check if we can inline a given call.
 * Currently, we cannot inline two cases:
 * - call with compound arguments
 * - graphs that take the address of a parameter
 *
 * check these conditions here
 */
static bool can_inline(ir_node *call, ir_graph *called_graph)
{
	ir_entity          *called      = get_irg_entity(called_graph);
	ir_type            *called_type = get_entity_type(called);
	ir_type            *call_type   = get_Call_type(call);
	int                 n_params    = get_method_n_params(called_type);
	int                 n_arguments = get_method_n_params(call_type);
	int                 n_res       = get_method_n_ress(called_type);
	irg_inline_property prop        = get_irg_inline_property(called_graph);
	int                 i;
	bool                res;

	if (prop == irg_inline_forbidden)
		return false;

	if (n_arguments != n_params) {
		/* this is a bad feature of C: without a prototype, we can
		 * call a function with less parameters than needed. Currently
		 * we don't support this, although we could use Unknown than. */
		return false;
	}
	if (n_res != get_method_n_ress(call_type)) {
		return false;
	}

	/* Argh, compiling C has some bad consequences:
	 * It is implementation dependent what happens in that case.
	 * We support inlining, if the bitsize of the types matches AND
	 * the same arithmetic is used. */
	for (i = n_params - 1; i >= 0; --i) {
		ir_type *param_tp = get_method_param_type(called_type, i);
		ir_type *arg_tp   = get_method_param_type(call_type, i);

		if (param_tp != arg_tp) {
			ir_mode *pmode = get_type_mode(param_tp);
			ir_mode *amode = get_type_mode(arg_tp);

			if (pmode == NULL || amode == NULL)
				return false;
			if (get_mode_size_bits(pmode) != get_mode_size_bits(amode))
				return false;
			if (get_mode_arithmetic(pmode) != get_mode_arithmetic(amode))
				return false;
			/* otherwise we can simply "reinterpret" the bits */
		}
	}
	for (i = n_res - 1; i >= 0; --i) {
		ir_type *decl_res_tp = get_method_res_type(called_type, i);
		ir_type *used_res_tp = get_method_res_type(call_type, i);

		if (decl_res_tp != used_res_tp) {
			ir_mode *decl_mode = get_type_mode(decl_res_tp);
			ir_mode *used_mode = get_type_mode(used_res_tp);
			if (decl_mode == NULL || used_mode == NULL)
				return false;
			if (get_mode_size_bits(decl_mode) != get_mode_size_bits(used_mode))
				return false;
			if (get_mode_arithmetic(decl_mode) != get_mode_arithmetic(used_mode))
				return false;
			/* otherwise we can "reinterpret" the bits */
		}
	}

	/* check parameters for compound arguments */
	for (i = 0; i < n_params; ++i) {
		ir_type *p_type = get_method_param_type(call_type, i);

		if (is_compound_type(p_type))
			return false;
	}

	/* check results for compound arguments */
	for (i = 0; i < n_res; ++i) {
		ir_type *r_type = get_method_res_type(call_type, i);

		if (is_compound_type(r_type))
			return false;
	}

	res = true;
	irg_walk_graph(called_graph, find_addr, NULL, &res);

	return res;
}

enum exc_mode {
	exc_handler,    /**< There is a handler. */
	exc_no_handler  /**< Exception handling not represented. */
};

/**
 * copy all entities on the stack frame on 1 irg to the stackframe of another.
 * Sets entity links of the old entities to the copies
 */
static void copy_frame_entities(ir_graph *from, ir_graph *to)
{
	ir_type *from_frame = get_irg_frame_type(from);
	ir_type *to_frame   = get_irg_frame_type(to);
	int      n_members  = get_class_n_members(from_frame);
	int      i;
	assert(from_frame != to_frame);

	for (i = 0; i < n_members; ++i) {
		ir_entity *old_ent = get_class_member(from_frame, i);
		ir_entity *new_ent = copy_entity_own(old_ent, to_frame);
		set_entity_link(old_ent, new_ent);
	}
}

/* Inlines a method at the given call site. */
int inline_method(ir_node *call, ir_graph *called_graph)
{
	ir_node       *pre_call;
	ir_node       *post_call, *post_bl;
	ir_node       *in[pn_Start_max];
	ir_node       *end, *end_bl, *block;
	ir_node       **res_pred;
	ir_node       **cf_pred;
	ir_node       **args_in;
	ir_node       *ret, *phi;
	int           arity, n_ret, n_exc, n_res, i, j, rem_opt;
	int           irn_arity, n_params;
	int           n_mem_phi;
	enum exc_mode exc_handling;
	ir_type       *mtp;
	ir_type       *ctp;
	ir_entity     *ent;
	ir_graph      *rem;
	ir_graph      *irg = get_irn_irg(call);

	/* we cannot inline some types of calls */
	if (! can_inline(call, called_graph))
		return 0;

	/* We cannot inline a recursive call. The graph must be copied before
	 * the call the inline_method() using create_irg_copy(). */
	if (called_graph == irg)
		return 0;

	ent      = get_irg_entity(called_graph);
	mtp      = get_entity_type(ent);
	ctp      = get_Call_type(call);
	n_params = get_method_n_params(mtp);
	n_res    = get_method_n_ress(mtp);

	rem = current_ir_graph;
	current_ir_graph = irg;

	DB((dbg, LEVEL_1, "Inlining %+F(%+F) into %+F\n", call, called_graph, irg));

	/* optimizations can cause problems when allocating new nodes */
	rem_opt = get_opt_optimize();
	set_optimize(0);

	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);
	assert(get_irg_pinned(irg) == op_pin_state_pinned);
	assert(get_irg_pinned(called_graph) == op_pin_state_pinned);
	set_irg_outs_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);
	set_irg_callee_info_state(irg, irg_callee_info_inconsistent);
	set_irg_entity_usage_state(irg, ir_entity_usage_not_computed);
	edges_deactivate(irg);

	/* here we know we WILL inline, so inform the statistics */
	hook_inline(call, called_graph);

	/* -- Decide how to handle exception control flow: Is there a handler
	   for the Call node, or do we branch directly to End on an exception?
	   exc_handling:
	   0 There is a handler.
	   2 Exception handling not represented in Firm. -- */
	{
		ir_node *Xproj = NULL;
		ir_node *proj;
		for (proj = (ir_node*)get_irn_link(call); proj != NULL;
		     proj = (ir_node*)get_irn_link(proj)) {
			long proj_nr = get_Proj_proj(proj);
			if (proj_nr == pn_Call_X_except) Xproj = proj;
		}
		exc_handling = Xproj != NULL ? exc_handler : exc_no_handler;
	}

	/* create the argument tuple */
	args_in = ALLOCAN(ir_node*, n_params);

	block = get_nodes_block(call);
	for (i = n_params - 1; i >= 0; --i) {
		ir_node *arg      = get_Call_param(call, i);
		ir_type *param_tp = get_method_param_type(mtp, i);
		ir_mode *mode     = get_type_mode(param_tp);

		if (mode != get_irn_mode(arg)) {
			arg = new_r_Conv(block, arg, mode);
		}
		args_in[i] = arg;
	}

	/* the procedure and later replaces the Start node of the called graph.
	 * Post_call is the old Call node and collects the results of the called
	 * graph. Both will end up being a tuple. */
	post_bl = get_nodes_block(call);
	/* XxMxPxPxPxT of Start + parameter of Call */
	in[pn_Start_M]              = get_Call_mem(call);
	in[pn_Start_X_initial_exec] = new_r_Jmp(post_bl);
	in[pn_Start_P_frame_base]   = get_irg_frame(irg);
	in[pn_Start_P_tls]          = get_irg_tls(irg);
	in[pn_Start_T_args]         = new_r_Tuple(post_bl, n_params, args_in);
	pre_call = new_r_Tuple(post_bl, pn_Start_max, in);
	post_call = call;

	/* --
	   The new block gets the ins of the old block, pre_call and all its
	   predecessors and all Phi nodes. -- */
	part_block(pre_call);

	/* increment visited flag for later walk */
	inc_irg_visited(called_graph);

	/* link some nodes to nodes in the current graph so instead of copying
	 * the linked nodes will get used.
	 * So the copier will use the created Tuple instead of copying the start
	 * node, similar for singleton nodes like NoMem and Bad.
	 * Note: this will prohibit predecessors to be copied - only do it for
	 *       nodes without predecessors */
	{
		ir_node *start_block;
		ir_node *start;
		ir_node *bad;
		ir_node *nomem;

		start_block = get_irg_start_block(called_graph);
		set_new_node(start_block, get_nodes_block(pre_call));
		mark_irn_visited(start_block);

		start = get_irg_start(called_graph);
		set_new_node(start, pre_call);
		mark_irn_visited(start);

		bad = get_irg_bad(called_graph);
		set_new_node(bad, get_irg_bad(irg));
		mark_irn_visited(bad);

		nomem = get_irg_no_mem(called_graph);
		set_new_node(nomem, get_irg_no_mem(irg));
		mark_irn_visited(nomem);
	}

	/* entitiy link is used to link entities on old stackframe to the
	 * new stackframe */
	irp_reserve_resources(irp, IR_RESOURCE_ENTITY_LINK);

	/* copy entities and nodes */
	assert(!irn_visited(get_irg_end(called_graph)));
	copy_frame_entities(called_graph, irg);
	irg_walk_core(get_irg_end(called_graph), copy_node_inline, set_preds_inline,
	              irg);

	irp_free_resources(irp, IR_RESOURCE_ENTITY_LINK);

	/* -- Merge the end of the inlined procedure with the call site -- */
	/* We will turn the old Call node into a Tuple with the following
	   predecessors:
	   -1:  Block of Tuple.
	   0: Phi of all Memories of Return statements.
	   1: Jmp from new Block that merges the control flow from all exception
	   predecessors of the old end block.
	   2: Tuple of all arguments.
	   3: Phi of Exception memories.
	   In case the old Call directly branches to End on an exception we don't
	   need the block merging all exceptions nor the Phi of the exception
	   memories.
	*/

	/* Precompute some values */
	end_bl = get_new_node(get_irg_end_block(called_graph));
	end    = get_new_node(get_irg_end(called_graph));
	arity  = get_Block_n_cfgpreds(end_bl);    /* arity = n_exc + n_ret  */
	n_res  = get_method_n_ress(get_Call_type(call));

	res_pred = XMALLOCN(ir_node*, n_res);
	cf_pred  = XMALLOCN(ir_node*, arity);

	/* archive keepalives */
	irn_arity = get_irn_arity(end);
	for (i = 0; i < irn_arity; i++) {
		ir_node *ka = get_End_keepalive(end, i);
		if (! is_Bad(ka))
			add_End_keepalive(get_irg_end(irg), ka);
	}

	/* replace Return nodes by Jump nodes */
	n_ret = 0;
	for (i = 0; i < arity; i++) {
		ir_node *ret;
		ret = get_Block_cfgpred(end_bl, i);
		if (is_Return(ret)) {
			ir_node *block = get_nodes_block(ret);
			cf_pred[n_ret] = new_r_Jmp(block);
			n_ret++;
		}
	}
	set_irn_in(post_bl, n_ret, cf_pred);

	/* build a Tuple for all results of the method.
	 * add Phi node if there was more than one Return. */
	turn_into_tuple(post_call, pn_Call_max);
	/* First the Memory-Phi */
	n_mem_phi = 0;
	for (i = 0; i < arity; i++) {
		ret = get_Block_cfgpred(end_bl, i);
		if (is_Return(ret)) {
			cf_pred[n_mem_phi++] = get_Return_mem(ret);
		}
		/* memory output for some exceptions is directly connected to End */
		if (is_Call(ret)) {
			cf_pred[n_mem_phi++] = new_r_Proj(ret, mode_M, 3);
		} else if (is_fragile_op(ret)) {
			/* We rely that all cfops have the memory output at the same position. */
			cf_pred[n_mem_phi++] = new_r_Proj(ret, mode_M, 0);
		} else if (is_Raise(ret)) {
			cf_pred[n_mem_phi++] = new_r_Proj(ret, mode_M, 1);
		}
	}
	phi = new_r_Phi(post_bl, n_mem_phi, cf_pred, mode_M);
	set_Tuple_pred(call, pn_Call_M, phi);
	/* Conserve Phi-list for further inlinings -- but might be optimized */
	if (get_nodes_block(phi) == post_bl) {
		set_irn_link(phi, get_irn_link(post_bl));
		set_irn_link(post_bl, phi);
	}
	/* Now the real results */
	if (n_res > 0) {
		ir_node *result_tuple;
		for (j = 0; j < n_res; j++) {
			ir_type *res_type = get_method_res_type(ctp, j);
			ir_mode *res_mode = get_type_mode(res_type);
			n_ret = 0;
			for (i = 0; i < arity; i++) {
				ret = get_Block_cfgpred(end_bl, i);
				if (is_Return(ret)) {
					ir_node *res = get_Return_res(ret, j);
					if (get_irn_mode(res) != res_mode) {
						ir_node *block = get_nodes_block(res);
						res = new_r_Conv(block, res, res_mode);
					}
					cf_pred[n_ret] = res;
					n_ret++;
				}
			}
			if (n_ret > 0) {
				ir_mode *mode = get_irn_mode(cf_pred[0]);
				phi = new_r_Phi(post_bl, n_ret, cf_pred, mode);
			} else {
				phi = new_r_Bad(irg);
			}
			res_pred[j] = phi;
			/* Conserve Phi-list for further inlinings -- but might be optimized */
			if (get_nodes_block(phi) == post_bl) {
				set_Phi_next(phi, get_Block_phis(post_bl));
				set_Block_phis(post_bl, phi);
			}
		}
		result_tuple = new_r_Tuple(post_bl, n_res, res_pred);
		set_Tuple_pred(call, pn_Call_T_result, result_tuple);
	} else {
		set_Tuple_pred(call, pn_Call_T_result, new_r_Bad(irg));
	}
	/* handle the regular call */
	set_Tuple_pred(call, pn_Call_X_regular, new_r_Jmp(post_bl));

	/* For now, we cannot inline calls with value_base */
	set_Tuple_pred(call, pn_Call_P_value_res_base, new_r_Bad(irg));

	/* Finally the exception control flow.
	   We have two possible situations:
	   First if the Call branches to an exception handler:
	   We need to add a Phi node to
	   collect the memory containing the exception objects.  Further we need
	   to add another block to get a correct representation of this Phi.  To
	   this block we add a Jmp that resolves into the X output of the Call
	   when the Call is turned into a tuple.
	   Second: There is no exception edge. Just add all inlined exception
	   branches to the End node.
	 */
	if (exc_handling == exc_handler) {
		n_exc = 0;
		for (i = 0; i < arity; i++) {
			ir_node *ret, *irn;
			ret = get_Block_cfgpred(end_bl, i);
			irn = skip_Proj(ret);
			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				++n_exc;
			}
		}
		if (n_exc > 0) {
			if (n_exc == 1) {
				/* simple fix */
				set_Tuple_pred(call, pn_Call_X_except, cf_pred[0]);
			} else {
				ir_node *block = new_r_Block(irg, n_exc, cf_pred);
				set_Tuple_pred(call, pn_Call_X_except, new_r_Jmp(block));
			}
		} else {
			set_Tuple_pred(call, pn_Call_X_except, new_r_Bad(irg));
		}
	} else {
		ir_node *main_end_bl;
		int main_end_bl_arity;
		ir_node **end_preds;

		/* assert(exc_handling == 1 || no exceptions. ) */
		n_exc = 0;
		for (i = 0; i < arity; i++) {
			ir_node *ret = get_Block_cfgpred(end_bl, i);
			ir_node *irn = skip_Proj(ret);

			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				n_exc++;
			}
		}
		main_end_bl       = get_irg_end_block(irg);
		main_end_bl_arity = get_irn_arity(main_end_bl);
		end_preds         = XMALLOCN(ir_node*, n_exc + main_end_bl_arity);

		for (i = 0; i < main_end_bl_arity; ++i)
			end_preds[i] = get_irn_n(main_end_bl, i);
		for (i = 0; i < n_exc; ++i)
			end_preds[main_end_bl_arity + i] = cf_pred[i];
		set_irn_in(main_end_bl, n_exc + main_end_bl_arity, end_preds);
		set_Tuple_pred(call, pn_Call_X_except, new_r_Bad(irg));
		free(end_preds);
	}
	free(res_pred);
	free(cf_pred);

	/* --  Turn CSE back on. -- */
	set_optimize(rem_opt);
	current_ir_graph = rem;

	return 1;
}

/********************************************************************/
/* Apply inlining to small methods.                                 */
/********************************************************************/

static struct obstack  temp_obst;

/** Represents a possible inlinable call in a graph. */
typedef struct call_entry {
	ir_node    *call;       /**< The Call node. */
	ir_graph   *callee;     /**< The callee IR-graph. */
	list_head  list;        /**< List head for linking the next one. */
	int        loop_depth;  /**< The loop depth of this call. */
	int        benefice;    /**< The calculated benefice of this call. */
	unsigned   local_adr:1; /**< Set if this call gets an address of a local variable. */
	unsigned   all_const:1; /**< Set if this call has only constant parameters. */
} call_entry;

/**
 * environment for inlining small irgs
 */
typedef struct inline_env_t {
	struct obstack obst;  /**< An obstack where call_entries are allocated on. */
	list_head      calls; /**< The call entry list. */
} inline_env_t;

/**
 * Returns the irg called from a Call node. If the irg is not
 * known, NULL is returned.
 *
 * @param call  the call node
 */
static ir_graph *get_call_called_irg(ir_node *call)
{
	ir_node *addr;

	addr = get_Call_ptr(call);
	if (is_Global(addr)) {
		ir_entity *ent = get_Global_entity(addr);
		/* we don't know which function gets finally bound to a weak symbol */
		if (get_entity_linkage(ent) & IR_LINKAGE_WEAK)
			return NULL;

		return get_entity_irg(ent);
	}

	return NULL;
}

/**
 * Walker: Collect all calls to known graphs inside a graph.
 */
static void collect_calls(ir_node *call, void *env)
{
	(void) env;
	if (is_Call(call)) {
		ir_graph *called_irg = get_call_called_irg(call);

		if (called_irg != NULL) {
			/* The Call node calls a locally defined method.  Remember to inline. */
			inline_env_t *ienv  = (inline_env_t*)env;
			call_entry   *entry = OALLOC(&ienv->obst, call_entry);
			entry->call       = call;
			entry->callee     = called_irg;
			entry->loop_depth = 0;
			entry->benefice   = 0;
			entry->local_adr  = 0;
			entry->all_const  = 0;

			list_add_tail(&entry->list, &ienv->calls);
		}
	}
}

/**
 * Inlines all small methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_small_irgs(ir_graph *irg, int size)
{
	ir_graph *rem = current_ir_graph;
	inline_env_t env;
	call_entry *entry;

	current_ir_graph = irg;
	/* Handle graph state */
	assert(get_irg_phase_state(irg) != phase_building);
	free_callee_info(irg);

	/* Find Call nodes to inline.
	   (We can not inline during a walk of the graph, as inlining the same
	   method several times changes the visited flag of the walked graph:
	   after the first inlining visited of the callee equals visited of
	   the caller.  With the next inlining both are increased.) */
	obstack_init(&env.obst);
	INIT_LIST_HEAD(&env.calls);
	irg_walk_graph(irg, NULL, collect_calls, &env);

	if (! list_empty(&env.calls)) {
		/* There are calls to inline */
		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
		collect_phiprojs(irg);

		list_for_each_entry(call_entry, entry, &env.calls, list) {
			ir_graph            *callee = entry->callee;
			irg_inline_property prop    = get_irg_inline_property(callee);

			if (prop == irg_inline_forbidden) {
				continue;
			}

			if (prop >= irg_inline_forced ||
			    _obstack_memory_used(callee->obst) - (int)obstack_room(callee->obst) < size) {
				inline_method(entry->call, callee);
			}
		}
		ir_free_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
	}
	obstack_free(&env.obst, NULL);
	current_ir_graph = rem;
}

typedef struct inline_small_irgs_pass_t {
	ir_graph_pass_t pass;
	int            size;
} inline_small_irgs_pass_t;

/**
 * Wrapper to run inline_small_irgs() as a pass.
 */
static int inline_small_irgs_wrapper(ir_graph *irg, void *context)
{
	inline_small_irgs_pass_t *pass = (inline_small_irgs_pass_t*)context;

	inline_small_irgs(irg, pass->size);
	return 0;
}

/* create a pass for inline_small_irgs() */
ir_graph_pass_t *inline_small_irgs_pass(const char *name, int size)
{
	inline_small_irgs_pass_t *pass = XMALLOCZ(inline_small_irgs_pass_t);

	pass->size = size;
	return def_graph_pass_constructor(
		&pass->pass, name ? name : "inline_small_irgs", inline_small_irgs_wrapper);
}

/**
 * Environment for inlining irgs.
 */
typedef struct {
	list_head calls;             /**< List of of all call nodes in this graph. */
	unsigned  *local_weights;    /**< Once allocated, the beneficial weight for transmitting local addresses. */
	unsigned  n_nodes;           /**< Number of nodes in graph except Id, Tuple, Proj, Start, End. */
	unsigned  n_blocks;          /**< Number of Blocks in graph without Start and End block. */
	unsigned  n_nodes_orig;      /**< for statistics */
	unsigned  n_call_nodes;      /**< Number of Call nodes in the graph. */
	unsigned  n_call_nodes_orig; /**< for statistics */
	unsigned  n_callers;         /**< Number of known graphs that call this graphs. */
	unsigned  n_callers_orig;    /**< for statistics */
	unsigned  got_inline:1;      /**< Set, if at least one call inside this graph was inlined. */
	unsigned  recursive:1;       /**< Set, if this function is self recursive. */
} inline_irg_env;

/**
 * Allocate a new environment for inlining.
 */
static inline_irg_env *alloc_inline_irg_env(void)
{
	inline_irg_env *env    = OALLOC(&temp_obst, inline_irg_env);
	INIT_LIST_HEAD(&env->calls);
	env->local_weights     = NULL;
	env->n_nodes           = -2; /* do not count count Start, End */
	env->n_blocks          = -2; /* do not count count Start, End Block */
	env->n_nodes_orig      = -2; /* do not count Start, End */
	env->n_call_nodes      = 0;
	env->n_call_nodes_orig = 0;
	env->n_callers         = 0;
	env->n_callers_orig    = 0;
	env->got_inline        = 0;
	env->recursive         = 0;
	return env;
}

typedef struct walker_env {
	inline_irg_env *x;     /**< the inline environment */
	char ignore_runtime;   /**< the ignore runtime flag */
	char ignore_callers;   /**< if set, do change callers data */
} wenv_t;

/**
 * post-walker: collect all calls in the inline-environment
 * of a graph and sum some statistics.
 */
static void collect_calls2(ir_node *call, void *ctx)
{
	wenv_t         *env = (wenv_t*)ctx;
	inline_irg_env *x = env->x;
	unsigned        code = get_irn_opcode(call);
	ir_graph       *callee;
	call_entry     *entry;

	/* count meaningful nodes in irg */
	if (code != iro_Proj && code != iro_Tuple && code != iro_Sync) {
		if (code != iro_Block) {
			++x->n_nodes;
			++x->n_nodes_orig;
		} else {
			++x->n_blocks;
		}
	}

	if (code != iro_Call) return;

	/* check, if it's a runtime call */
	if (env->ignore_runtime) {
		ir_node *symc = get_Call_ptr(call);

		if (is_Global(symc)) {
			ir_entity *ent = get_Global_entity(symc);

			if (get_entity_additional_properties(ent) & mtp_property_runtime)
				return;
		}
	}

	/* collect all call nodes */
	++x->n_call_nodes;
	++x->n_call_nodes_orig;

	callee = get_call_called_irg(call);
	if (callee != NULL) {
		if (! env->ignore_callers) {
			inline_irg_env *callee_env = (inline_irg_env*)get_irg_link(callee);
			/* count all static callers */
			++callee_env->n_callers;
			++callee_env->n_callers_orig;
		}
		if (callee == current_ir_graph)
			x->recursive = 1;

		/* link it in the list of possible inlinable entries */
		entry = OALLOC(&temp_obst, call_entry);
		entry->call       = call;
		entry->callee     = callee;
		entry->loop_depth = get_irn_loop(get_nodes_block(call))->depth;
		entry->benefice   = 0;
		entry->local_adr  = 0;
		entry->all_const  = 0;

		list_add_tail(&entry->list, &x->calls);
	}
}

/**
 * Returns TRUE if the number of callers is 0 in the irg's environment,
 * hence this irg is a leave.
 */
inline static int is_leave(ir_graph *irg)
{
	inline_irg_env *env = (inline_irg_env*)get_irg_link(irg);
	return env->n_call_nodes == 0;
}

/**
 * Returns TRUE if the number of nodes in the callee is
 * smaller then size in the irg's environment.
 */
inline static int is_smaller(ir_graph *callee, unsigned size)
{
	inline_irg_env *env = (inline_irg_env*)get_irg_link(callee);
	return env->n_nodes < size;
}

/**
 * Duplicate a call entry.
 *
 * @param entry     the original entry to duplicate
 * @param new_call  the new call node
 * @param loop_depth_delta
 *                  delta value for the loop depth
 */
static call_entry *duplicate_call_entry(const call_entry *entry,
                                        ir_node *new_call, int loop_depth_delta)
{
	call_entry *nentry = OALLOC(&temp_obst, call_entry);
	nentry->call       = new_call;
	nentry->callee     = entry->callee;
	nentry->benefice   = entry->benefice;
	nentry->loop_depth = entry->loop_depth + loop_depth_delta;
	nentry->local_adr  = entry->local_adr;
	nentry->all_const  = entry->all_const;

	return nentry;
}

/**
 * Append all call nodes of the source environment to the nodes of in the destination
 * environment.
 *
 * @param dst         destination environment
 * @param src         source environment
 * @param loop_depth  the loop depth of the call that is replaced by the src list
 */
static void append_call_list(inline_irg_env *dst, inline_irg_env *src, int loop_depth)
{
	call_entry *entry, *nentry;

	/* Note that the src list points to Call nodes in the inlined graph, but
	   we need Call nodes in our graph. Luckily the inliner leaves this information
	   in the link field. */
	list_for_each_entry(call_entry, entry, &src->calls, list) {
		nentry = duplicate_call_entry(entry, (ir_node*)get_irn_link(entry->call), loop_depth);
		list_add_tail(&nentry->list, &dst->calls);
	}
	dst->n_call_nodes += src->n_call_nodes;
	dst->n_nodes      += src->n_nodes;
}

/*
 * Inlines small leave methods at call sites where the called address comes
 * from a Const node that references the entity representing the called
 * method.
 * The size argument is a rough measure for the code size of the method:
 * Methods where the obstack containing the firm graph is smaller than
 * size are inlined.
 */
void inline_leave_functions(unsigned maxsize, unsigned leavesize,
                            unsigned size, int ignore_runtime)
{
	inline_irg_env   *env;
	ir_graph         *irg;
	int              i, n_irgs;
	ir_graph         *rem;
	int              did_inline;
	wenv_t           wenv;
	call_entry       *entry, *next;
	const call_entry *centry;
	pmap             *copied_graphs;
	pmap_entry       *pm_entry;

	rem = current_ir_graph;
	obstack_init(&temp_obst);

	/* a map for the copied graphs, used to inline recursive calls */
	copied_graphs = pmap_create();

	/* extend all irgs by a temporary data structure for inlining. */
	n_irgs = get_irp_n_irgs();
	for (i = 0; i < n_irgs; ++i)
		set_irg_link(get_irp_irg(i), alloc_inline_irg_env());

	/* Pre-compute information in temporary data structure. */
	wenv.ignore_runtime = ignore_runtime;
	wenv.ignore_callers = 0;
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = get_irp_irg(i);

		assert(get_irg_phase_state(irg) != phase_building);
		free_callee_info(irg);

		assure_cf_loop(irg);
		wenv.x = (inline_irg_env*)get_irg_link(irg);
		irg_walk_graph(irg, NULL, collect_calls2, &wenv);
	}

	/* -- and now inline. -- */

	/* Inline leaves recursively -- we might construct new leaves. */
	do {
		did_inline = 0;

		for (i = 0; i < n_irgs; ++i) {
			ir_node *call;
			int phiproj_computed = 0;

			current_ir_graph = get_irp_irg(i);
			env              = (inline_irg_env*)get_irg_link(current_ir_graph);

			ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
			list_for_each_entry_safe(call_entry, entry, next, &env->calls, list) {
				ir_graph            *callee;
				irg_inline_property  prop;

				if (env->n_nodes > maxsize)
					break;

				call   = entry->call;
				callee = entry->callee;

				prop = get_irg_inline_property(callee);
				if (prop == irg_inline_forbidden) {
					continue;
				}

				if (is_leave(callee) && (
				    is_smaller(callee, leavesize) || prop >= irg_inline_forced)) {
					if (!phiproj_computed) {
						phiproj_computed = 1;
						collect_phiprojs(current_ir_graph);
					}
					did_inline = inline_method(call, callee);

					if (did_inline) {
						inline_irg_env *callee_env = (inline_irg_env*)get_irg_link(callee);

						/* call was inlined, Phi/Projs for current graph must be recomputed */
						phiproj_computed = 0;

						/* Do some statistics */
						env->got_inline = 1;
						--env->n_call_nodes;
						env->n_nodes += callee_env->n_nodes;
						--callee_env->n_callers;

						/* remove this call from the list */
						list_del(&entry->list);
						continue;
					}
				}
			}
			ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
		}
	} while (did_inline);

	/* inline other small functions. */
	for (i = 0; i < n_irgs; ++i) {
		ir_node *call;
		int phiproj_computed = 0;

		current_ir_graph = get_irp_irg(i);
		env              = (inline_irg_env*)get_irg_link(current_ir_graph);

		ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

		/* note that the list of possible calls is updated during the process */
		list_for_each_entry_safe(call_entry, entry, next, &env->calls, list) {
			irg_inline_property prop;
			ir_graph            *callee;
			pmap_entry          *e;

			call   = entry->call;
			callee = entry->callee;

			prop = get_irg_inline_property(callee);
			if (prop == irg_inline_forbidden) {
				continue;
			}

			e = pmap_find(copied_graphs, callee);
			if (e != NULL) {
				/*
				 * Remap callee if we have a copy.
				 * FIXME: Should we do this only for recursive Calls ?
				 */
				callee = (ir_graph*)e->value;
			}

			if (prop >= irg_inline_forced ||
			    (is_smaller(callee, size) && env->n_nodes < maxsize) /* small function */) {
				if (current_ir_graph == callee) {
					/*
					 * Recursive call: we cannot directly inline because we cannot walk
					 * the graph and change it. So we have to make a copy of the graph
					 * first.
					 */

					inline_irg_env *callee_env;
					ir_graph       *copy;

					ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

					/*
					 * No copy yet, create one.
					 * Note that recursive methods are never leaves, so it is sufficient
					 * to test this condition here.
					 */
					copy = create_irg_copy(callee);

					/* create_irg_copy() destroys the Proj links, recompute them */
					phiproj_computed = 0;

					ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

					/* allocate new environment */
					callee_env = alloc_inline_irg_env();
					set_irg_link(copy, callee_env);

					assure_cf_loop(copy);
					wenv.x              = callee_env;
					wenv.ignore_callers = 1;
					irg_walk_graph(copy, NULL, collect_calls2, &wenv);

					/*
					 * Enter the entity of the original graph. This is needed
					 * for inline_method(). However, note that ent->irg still points
					 * to callee, NOT to copy.
					 */
					set_irg_entity(copy, get_irg_entity(callee));

					pmap_insert(copied_graphs, callee, copy);
					callee = copy;

					/* we have only one caller: the original graph */
					callee_env->n_callers      = 1;
					callee_env->n_callers_orig = 1;
				}
				if (! phiproj_computed) {
					phiproj_computed = 1;
					collect_phiprojs(current_ir_graph);
				}
				did_inline = inline_method(call, callee);
				if (did_inline) {
					inline_irg_env *callee_env = (inline_irg_env *)get_irg_link(callee);

					/* call was inlined, Phi/Projs for current graph must be recomputed */
					phiproj_computed = 0;

					/* callee was inline. Append it's call list. */
					env->got_inline = 1;
					--env->n_call_nodes;
					append_call_list(env, callee_env, entry->loop_depth);
					--callee_env->n_callers;

					/* after we have inlined callee, all called methods inside callee
					   are now called once more */
					list_for_each_entry(call_entry, centry, &callee_env->calls, list) {
						inline_irg_env *penv = (inline_irg_env*)get_irg_link(centry->callee);
						++penv->n_callers;
					}

					/* remove this call from the list */
					list_del(&entry->list);
					continue;
				}
			}
		}
		ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
	}

	for (i = 0; i < n_irgs; ++i) {
		irg = get_irp_irg(i);
		env = (inline_irg_env*)get_irg_link(irg);

		if (env->got_inline) {
			optimize_graph_df(irg);
			optimize_cf(irg);
		}
		if (env->got_inline || (env->n_callers_orig != env->n_callers)) {
			DB((dbg, LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
			env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
			env->n_callers_orig, env->n_callers,
			get_entity_name(get_irg_entity(irg))));
		}
	}

	/* kill the copied graphs: we don't need them anymore */
	foreach_pmap(copied_graphs, pm_entry) {
		ir_graph *copy = (ir_graph*)pm_entry->value;

		/* reset the entity, otherwise it will be deleted in the next step ... */
		set_irg_entity(copy, NULL);
		free_ir_graph(copy);
	}
	pmap_destroy(copied_graphs);

	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

typedef struct inline_leave_functions_pass_t {
	ir_prog_pass_t pass;
	unsigned       maxsize;
	unsigned       leavesize;
	unsigned       size;
	int            ignore_runtime;
} inline_leave_functions_pass_t;

/**
 * Wrapper to run inline_leave_functions() as a ir_prog pass.
 */
static int inline_leave_functions_wrapper(ir_prog *irp, void *context)
{
	inline_leave_functions_pass_t *pass = (inline_leave_functions_pass_t*)context;

	(void)irp;
	inline_leave_functions(
		pass->maxsize, pass->leavesize,
		pass->size, pass->ignore_runtime);
	return 0;
}

/* create a pass for inline_leave_functions() */
ir_prog_pass_t *inline_leave_functions_pass(
	const char *name, unsigned maxsize, unsigned leavesize,
	unsigned size, int ignore_runtime)
{
	inline_leave_functions_pass_t *pass = XMALLOCZ(inline_leave_functions_pass_t);

	pass->maxsize        = maxsize;
	pass->leavesize      = leavesize;
	pass->size           = size;
	pass->ignore_runtime = ignore_runtime;

	return def_prog_pass_constructor(
		&pass->pass,
		name ? name : "inline_leave_functions",
		inline_leave_functions_wrapper);
}

/**
 * Calculate the parameter weights for transmitting the address of a local variable.
 */
static unsigned calc_method_local_weight(ir_node *arg)
{
	int      i, j, k;
	unsigned v, weight = 0;

	for (i = get_irn_n_outs(arg) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(arg, i);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
		case iro_Store:
			/* Loads and Store can be removed */
			weight += 3;
			break;
		case iro_Sel:
			/* check if all args are constant */
			for (j = get_Sel_n_indexs(succ) - 1; j >= 0; --j) {
				ir_node *idx = get_Sel_index(succ, j);
				if (! is_Const(idx))
					return 0;
			}
			/* Check users on this Sel. Note: if a 0 is returned here, there was
			   some unsupported node. */
			v = calc_method_local_weight(succ);
			if (v == 0)
				return 0;
			/* we can kill one Sel with constant indexes, this is cheap */
			weight += v + 1;
			break;
		case iro_Id:
			/* when looking backward we might find Id nodes */
			weight += calc_method_local_weight(succ);
			break;
		case iro_Tuple:
			/* unoptimized tuple */
			for (j = get_Tuple_n_preds(succ) - 1; j >= 0; --j) {
				ir_node *pred = get_Tuple_pred(succ, j);
				if (pred == arg) {
					/* look for Proj(j) */
					for (k = get_irn_n_outs(succ) - 1; k >= 0; --k) {
						ir_node *succ_succ = get_irn_out(succ, k);
						if (is_Proj(succ_succ)) {
							if (get_Proj_proj(succ_succ) == j) {
								/* found */
								weight += calc_method_local_weight(succ_succ);
							}
						} else {
							/* this should NOT happen */
							return 0;
						}
					}
				}
			}
			break;
		default:
			/* any other node: unsupported yet or bad. */
			return 0;
		}
	}
	return weight;
}

/**
 * Calculate the parameter weights for transmitting the address of a local variable.
 */
static void analyze_irg_local_weights(inline_irg_env *env, ir_graph *irg)
{
	ir_entity *ent = get_irg_entity(irg);
	ir_type  *mtp;
	int      nparams, i, proj_nr;
	ir_node  *irg_args, *arg;

	mtp      = get_entity_type(ent);
	nparams  = get_method_n_params(mtp);

	/* allocate a new array. currently used as 'analysed' flag */
	env->local_weights = NEW_ARR_D(unsigned, &temp_obst, nparams);

	/* If the method haven't parameters we have nothing to do. */
	if (nparams <= 0)
		return;

	assure_irg_outs(irg);
	irg_args = get_irg_args(irg);
	for (i = get_irn_n_outs(irg_args) - 1; i >= 0; --i) {
		arg     = get_irn_out(irg_args, i);
		proj_nr = get_Proj_proj(arg);
		env->local_weights[proj_nr] = calc_method_local_weight(arg);
	}
}

/**
 * Calculate the benefice for transmitting an local variable address.
 * After inlining, the local variable might be transformed into a
 * SSA variable by scalar_replacement().
 */
static unsigned get_method_local_adress_weight(ir_graph *callee, int pos)
{
	inline_irg_env *env = (inline_irg_env*)get_irg_link(callee);

	if (env->local_weights != NULL) {
		if (pos < ARR_LEN(env->local_weights))
			return env->local_weights[pos];
		return 0;
	}

	analyze_irg_local_weights(env, callee);

	if (pos < ARR_LEN(env->local_weights))
		return env->local_weights[pos];
	return 0;
}

/**
 * Calculate a benefice value for inlining the given call.
 *
 * @param call       the call node we have to inspect
 * @param callee     the called graph
 */
static int calc_inline_benefice(call_entry *entry, ir_graph *callee)
{
	ir_node   *call = entry->call;
	ir_entity *ent  = get_irg_entity(callee);
	ir_node   *frame_ptr;
	ir_type   *mtp;
	int       weight = 0;
	int       i, n_params, all_const;
	unsigned  cc, v;
	irg_inline_property prop;

	inline_irg_env *callee_env;

	prop = get_irg_inline_property(callee);
	if (prop == irg_inline_forbidden) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: inlining forbidden\n",
		    call, callee));
		return entry->benefice = INT_MIN;
	}

	if (get_irg_additional_properties(callee) & mtp_property_noreturn) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: not inlining noreturn or weak\n",
		    call, callee));
		return entry->benefice = INT_MIN;
	}

	/* costs for every passed parameter */
	n_params = get_Call_n_params(call);
	mtp      = get_entity_type(ent);
	cc       = get_method_calling_convention(mtp);
	if (cc & cc_reg_param) {
		/* register parameter, smaller costs for register parameters */
		int max_regs = cc & ~cc_bits;

		if (max_regs < n_params)
			weight += max_regs * 2 + (n_params - max_regs) * 5;
		else
			weight += n_params * 2;
	} else {
		/* parameters are passed an stack */
		weight += 5 * n_params;
	}

	/* constant parameters improve the benefice */
	frame_ptr = get_irg_frame(current_ir_graph);
	all_const = 1;
	for (i = 0; i < n_params; ++i) {
		ir_node *param = get_Call_param(call, i);

		if (is_Const(param)) {
			weight += get_method_param_weight(ent, i);
		} else {
			all_const = 0;
			if (is_SymConst(param))
				weight += get_method_param_weight(ent, i);
			else if (is_Sel(param) && get_Sel_ptr(param) == frame_ptr) {
				/*
				 * An address of a local variable is transmitted. After
				 * inlining, scalar_replacement might be able to remove the
				 * local variable, so honor this.
				 */
				v = get_method_local_adress_weight(callee, i);
				weight += v;
				if (v > 0)
					entry->local_adr = 1;
			}
		}
	}
	entry->all_const = all_const;

	callee_env = (inline_irg_env*)get_irg_link(callee);
	if (callee_env->n_callers == 1 &&
	    callee != current_ir_graph &&
	    !entity_is_externally_visible(ent)) {
		weight += 700;
	}

	/* give a bonus for functions with one block */
	if (callee_env->n_blocks == 1)
		weight = weight * 3 / 2;

	/* and one for small non-recursive functions: we want them to be inlined in mostly every case */
	if (callee_env->n_nodes < 30 && !callee_env->recursive)
		weight += 2000;

	/* and finally for leaves: they do not increase the register pressure
	   because of callee safe registers */
	if (callee_env->n_call_nodes == 0)
		weight += 400;

	/** it's important to inline inner loops first */
	if (entry->loop_depth > 30)
		weight += 30 * 1024;
	else
		weight += entry->loop_depth * 1024;

	/*
	 * All arguments constant is probably a good sign, give an extra bonus
	 */
	if (all_const)
		weight += 1024;

	return entry->benefice = weight;
}

static ir_graph **irgs;
static int      last_irg;

/**
 * Callgraph walker, collect all visited graphs.
 */
static void callgraph_walker(ir_graph *irg, void *data)
{
	(void) data;
	irgs[last_irg++] = irg;
}

/**
 * Creates an inline order for all graphs.
 *
 * @return the list of graphs.
 */
static ir_graph **create_irg_list(void)
{
	ir_entity **free_methods;
	int       arr_len;
	int       n_irgs = get_irp_n_irgs();

	cgana(&arr_len, &free_methods);
	xfree(free_methods);

	compute_callgraph();

	last_irg = 0;
	irgs     = XMALLOCNZ(ir_graph*, n_irgs);

	callgraph_walk(NULL, callgraph_walker, NULL);
	assert(n_irgs == last_irg);

	return irgs;
}

/**
 * Push a call onto the priority list if its benefice is big enough.
 *
 * @param pqueue   the priority queue of calls
 * @param call     the call entry
 * @param inlien_threshold
 *                 the threshold value
 */
static void maybe_push_call(pqueue_t *pqueue, call_entry *call,
                            int inline_threshold)
{
	ir_graph            *callee  = call->callee;
	irg_inline_property prop     = get_irg_inline_property(callee);
	int                 benefice = calc_inline_benefice(call, callee);

	DB((dbg, LEVEL_2, "In %+F Call %+F to %+F has benefice %d\n",
	    get_irn_irg(call->call), call->call, callee, benefice));

	if (prop < irg_inline_forced && benefice < inline_threshold) {
		return;
	}

	pqueue_put(pqueue, call, benefice);
}

/**
 * Try to inline calls into a graph.
 *
 * @param irg      the graph into which we inline
 * @param maxsize  do NOT inline if the size of irg gets
 *                 bigger than this amount
 * @param inline_threshold
 *                 threshold value for inline decision
 * @param copied_graphs
 *                 map containing copied of recursive graphs
 */
static void inline_into(ir_graph *irg, unsigned maxsize,
                        int inline_threshold, pmap *copied_graphs)
{
	int            phiproj_computed = 0;
	inline_irg_env *env = (inline_irg_env*)get_irg_link(irg);
	call_entry     *curr_call;
	wenv_t         wenv;
	pqueue_t       *pqueue;

	if (env->n_call_nodes == 0)
		return;

	if (env->n_nodes > maxsize) {
		DB((dbg, LEVEL_2, "%+F: too big (%d)\n", irg, env->n_nodes));
		return;
	}

	current_ir_graph = irg;
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

	/* put irgs into the pqueue */
	pqueue = new_pqueue();

	list_for_each_entry(call_entry, curr_call, &env->calls, list) {
		assert(is_Call(curr_call->call));
		maybe_push_call(pqueue, curr_call, inline_threshold);
	}

	/* note that the list of possible calls is updated during the process */
	while (!pqueue_empty(pqueue)) {
		int                 did_inline;
		call_entry          *curr_call  = (call_entry*)pqueue_pop_front(pqueue);
		ir_graph            *callee     = curr_call->callee;
		ir_node             *call_node  = curr_call->call;
		inline_irg_env      *callee_env = (inline_irg_env*)get_irg_link(callee);
		irg_inline_property prop        = get_irg_inline_property(callee);
		int                 loop_depth;
		const call_entry    *centry;
		pmap_entry          *e;

		if ((prop < irg_inline_forced) && env->n_nodes + callee_env->n_nodes > maxsize) {
			DB((dbg, LEVEL_2, "%+F: too big (%d) + %+F (%d)\n", irg,
						env->n_nodes, callee, callee_env->n_nodes));
			continue;
		}

		e = pmap_find(copied_graphs, callee);
		if (e != NULL) {
			int benefice = curr_call->benefice;
			/*
			 * Reduce the weight for recursive function IFF not all arguments are const.
			 * inlining recursive functions is rarely good.
			 */
			if (!curr_call->all_const)
				benefice -= 2000;
			if (benefice < inline_threshold)
				continue;

			/*
			 * Remap callee if we have a copy.
			 */
			callee     = (ir_graph*)e->value;
			callee_env = (inline_irg_env*)get_irg_link(callee);
		}

		if (current_ir_graph == callee) {
			/*
			 * Recursive call: we cannot directly inline because we cannot
			 * walk the graph and change it. So we have to make a copy of
			 * the graph first.
			 */
			int benefice = curr_call->benefice;
			ir_graph *copy;

			/*
			 * Reduce the weight for recursive function IFF not all arguments are const.
			 * inlining recursive functions is rarely good.
			 */
			if (!curr_call->all_const)
				benefice -= 2000;
			if (benefice < inline_threshold)
				continue;

			ir_free_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

			/*
			 * No copy yet, create one.
			 * Note that recursive methods are never leaves, so it is
			 * sufficient to test this condition here.
			 */
			copy = create_irg_copy(callee);

			/* create_irg_copy() destroys the Proj links, recompute them */
			phiproj_computed = 0;

			ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

			/* allocate a new environment */
			callee_env = alloc_inline_irg_env();
			set_irg_link(copy, callee_env);

			assure_cf_loop(copy);
			wenv.x              = callee_env;
			wenv.ignore_callers = 1;
			irg_walk_graph(copy, NULL, collect_calls2, &wenv);

			/*
			 * Enter the entity of the original graph. This is needed
			 * for inline_method(). However, note that ent->irg still points
			 * to callee, NOT to copy.
			 */
			set_irg_entity(copy, get_irg_entity(callee));

			pmap_insert(copied_graphs, callee, copy);
			callee = copy;

			/* we have only one caller: the original graph */
			callee_env->n_callers      = 1;
			callee_env->n_callers_orig = 1;
		}
		if (! phiproj_computed) {
			phiproj_computed = 1;
			collect_phiprojs(current_ir_graph);
		}
		did_inline = inline_method(call_node, callee);
		if (!did_inline)
			continue;

		/* call was inlined, Phi/Projs for current graph must be recomputed */
		phiproj_computed = 0;

		/* remove it from the caller list */
		list_del(&curr_call->list);

		/* callee was inline. Append it's call list. */
		env->got_inline = 1;
		--env->n_call_nodes;

		/* we just generate a bunch of new calls */
		loop_depth = curr_call->loop_depth;
		list_for_each_entry(call_entry, centry, &callee_env->calls, list) {
			inline_irg_env *penv = (inline_irg_env*)get_irg_link(centry->callee);
			ir_node        *new_call;
			call_entry     *new_entry;

			/* after we have inlined callee, all called methods inside
			 * callee are now called once more */
			++penv->n_callers;

			/* Note that the src list points to Call nodes in the inlined graph,
			 * but we need Call nodes in our graph. Luckily the inliner leaves
			 * this information in the link field. */
			new_call = (ir_node*)get_irn_link(centry->call);
			assert(is_Call(new_call));

			new_entry = duplicate_call_entry(centry, new_call, loop_depth);
			list_add_tail(&new_entry->list, &env->calls);
			maybe_push_call(pqueue, new_entry, inline_threshold);
		}

		env->n_call_nodes += callee_env->n_call_nodes;
		env->n_nodes += callee_env->n_nodes;
		--callee_env->n_callers;
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);
	del_pqueue(pqueue);
}

/*
 * Heuristic inliner. Calculates a benefice value for every call and inlines
 * those calls with a value higher than the threshold.
 */
void inline_functions(unsigned maxsize, int inline_threshold,
                      opt_ptr after_inline_opt)
{
	inline_irg_env   *env;
	int              i, n_irgs;
	ir_graph         *rem;
	wenv_t           wenv;
	pmap             *copied_graphs;
	pmap_entry       *pm_entry;
	ir_graph         **irgs;

	rem = current_ir_graph;
	obstack_init(&temp_obst);

	irgs = create_irg_list();

	/* a map for the copied graphs, used to inline recursive calls */
	copied_graphs = pmap_create();

	/* extend all irgs by a temporary data structure for inlining. */
	n_irgs = get_irp_n_irgs();
	for (i = 0; i < n_irgs; ++i)
		set_irg_link(irgs[i], alloc_inline_irg_env());

	/* Pre-compute information in temporary data structure. */
	wenv.ignore_runtime = 0;
	wenv.ignore_callers = 0;
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		free_callee_info(irg);

		wenv.x = (inline_irg_env*)get_irg_link(irg);
		assure_cf_loop(irg);
		irg_walk_graph(irg, NULL, collect_calls2, &wenv);
	}

	/* -- and now inline. -- */
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		inline_into(irg, maxsize, inline_threshold, copied_graphs);
	}

	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		env = (inline_irg_env*)get_irg_link(irg);
		if (env->got_inline && after_inline_opt != NULL) {
			/* this irg got calls inlined: optimize it */
			after_inline_opt(irg);
		}
		if (env->got_inline || (env->n_callers_orig != env->n_callers)) {
			DB((dbg, LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
			env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
			env->n_callers_orig, env->n_callers,
			get_entity_name(get_irg_entity(irg))));
		}
	}

	/* kill the copied graphs: we don't need them anymore */
	foreach_pmap(copied_graphs, pm_entry) {
		ir_graph *copy = (ir_graph*)pm_entry->value;

		/* reset the entity, otherwise it will be deleted in the next step ... */
		set_irg_entity(copy, NULL);
		free_ir_graph(copy);
	}
	pmap_destroy(copied_graphs);

	xfree(irgs);

	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

typedef struct inline_functions_pass_t {
	ir_prog_pass_t pass;
	unsigned       maxsize;
	int            inline_threshold;
	opt_ptr        after_inline_opt;
} inline_functions_pass_t;

/**
 * Wrapper to run inline_functions() as a ir_prog pass.
 */
static int inline_functions_wrapper(ir_prog *irp, void *context)
{
	inline_functions_pass_t *pass = (inline_functions_pass_t*)context;

	(void)irp;
	inline_functions(pass->maxsize, pass->inline_threshold,
	                 pass->after_inline_opt);
	return 0;
}

/* create a ir_prog pass for inline_functions */
ir_prog_pass_t *inline_functions_pass(
	  const char *name, unsigned maxsize, int inline_threshold,
	  opt_ptr after_inline_opt)
{
	inline_functions_pass_t *pass = XMALLOCZ(inline_functions_pass_t);

	pass->maxsize          = maxsize;
	pass->inline_threshold = inline_threshold;
	pass->after_inline_opt = after_inline_opt;

	return def_prog_pass_constructor(
		&pass->pass, name ? name : "inline_functions",
		inline_functions_wrapper);
}

void firm_init_inline(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.inline");
}
