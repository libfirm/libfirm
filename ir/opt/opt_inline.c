/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Dead node elimination and Procedure Inlining.
 * @author   Michael Beck, Goetz Lindenmaier
 */
#include <limits.h>
#include <stdbool.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"

#include "iroptimize.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"

#include "array.h"
#include "list.h"
#include "pmap.h"
#include "xmalloc.h"
#include "pqueue.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
#include "opt_init.h"
#include "cgana.h"
#include "trouts.h"
#include "error.h"
#include "irmemory_t.h"

#include "analyze_irg_args.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "iropt_dbg.h"
#include "irnodemap.h"

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
	if (is_irn_start_block_placed(new_node)) {
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

	if (is_Block(node) && get_Block_entity(node)) {
		/**
		 * Currently we can't handle blocks whose address was taken correctly
		 * when inlining
		 */
		*allow_inline = false;
	} else if (is_Sel(node)) {
		ir_graph *irg = current_ir_graph;
		if (get_Sel_ptr(node) == get_irg_frame(irg)) {
			/* access to frame */
			ir_entity *ent = get_Sel_entity(node);
			if (get_entity_owner(ent) != get_irg_frame_type(irg)) {
				/* access to value_type */
				*allow_inline = false;
			}
		}
	} else if (is_Alloc(node)) {
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
	ir_type            *frame_type  = get_irg_frame_type(called_graph);
	size_t              n_params    = get_method_n_params(called_type);
	size_t              n_arguments = get_method_n_params(call_type);
	size_t              n_res       = get_method_n_ress(called_type);
	size_t              n_entities  = get_class_n_members(frame_type);
	mtp_additional_properties props = get_entity_additional_properties(called);
	size_t              i;
	bool                res;

	if (props & mtp_property_noinline)
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
	for (i = 0; i < n_params; ++i) {
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
	for (i = 0; i < n_res; ++i) {
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

	/* check for nested functions and variable number of parameters */
	for (i = 0; i < n_entities; ++i) {
		ir_entity *ent = get_class_member(frame_type, i);
		if (is_method_entity(ent))
			return false;
		if (is_parameter_entity(ent) && (get_entity_parameter_number(ent) == IR_VA_START_PARAMETER_NUMBER))
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
	size_t   n_members  = get_class_n_members(from_frame);
	size_t   i;
	assert(from_frame != to_frame);

	for (i = 0; i < n_members; ++i) {
		ir_entity *old_ent = get_class_member(from_frame, i);

		// parameter entities are already copied and the link has been set
		if (!is_parameter_entity(old_ent)) {
			ir_entity *new_ent = copy_entity_own(old_ent, to_frame);
			set_entity_link(old_ent, new_ent);
		}
	}
}

/* Copies parameter entities from the given called graph */
static void copy_parameter_entities(ir_node *call, ir_graph *called_graph)
{
	dbg_info *dbgi         = get_irn_dbg_info(call);
	ir_graph *irg          = get_irn_irg(call);
	ir_node  *frame        = get_irg_frame(irg);
	ir_node  *block        = get_nodes_block(call);
	ir_type  *called_frame = get_irg_frame_type(called_graph);
	ir_type  *frame_type   = get_irg_frame_type(irg);
	ir_node  *call_mem     = get_Call_mem(call);
	ir_node **sync_mem     = NULL;
	bool      have_copyb   = false;

	for (size_t i = 0, n_entities = get_class_n_members(called_frame);
	     i < n_entities; ++i) {
		ir_entity *old_entity = get_class_member(called_frame, i);
		if (!is_parameter_entity(old_entity))
			continue;

		if (sync_mem == NULL) {
			sync_mem = NEW_ARR_F(ir_node*, 1);
			sync_mem[0] = get_Call_mem(call);
		}

		ir_type   *old_type    = get_entity_type(old_entity);
		dbg_info  *entity_dbgi = get_entity_dbg_info(old_entity);
		ident     *name        = get_entity_ident(old_entity);
		name = id_mangle3("", name, "$inlined");
		ir_entity *new_ent     = new_entity(frame_type, name, old_type);
		set_entity_dbg_info(new_ent, entity_dbgi);
		set_entity_link(old_entity, new_ent);

		size_t   n_param_pos = get_entity_parameter_number(old_entity);
		ir_node *param       = get_Call_param(call, n_param_pos);
		ir_node *nomem       = get_irg_no_mem(irg);
		ir_node *sel         = new_rd_simpleSel(dbgi, block, nomem, frame, new_ent);
		ir_node *new_mem;
		if (is_aggregate_type(old_type)) {
			/* Copy the compound parameter */

			bool is_volatile = is_partly_volatile(param) || is_partly_volatile(sel);

			new_mem = new_rd_CopyB(dbgi, block, call_mem, sel, param, old_type, is_volatile ? cons_volatile : cons_none);
			set_Call_param(call, n_param_pos, sel);
			if (have_copyb) {
				ARR_APP1(ir_node*, sync_mem, new_mem);
			} else {
				/*
				 * The first time a copyb node is added it may overwrite sync_mem[0],
				 * because the copyb node itself references it.
				 */
				sync_mem[0] = new_mem;
				have_copyb = true;
			}
		} else {
			/* Store the parameter onto the frame */
			ir_node *store = new_rd_Store(dbgi, block, nomem, sel, param, cons_none);
			new_mem = new_r_Proj(store, mode_M, pn_Store_M);
			ARR_APP1(ir_node*, sync_mem, new_mem);
		}
	}

	if (sync_mem != NULL) {
		int sync_arity = (int)ARR_LEN(sync_mem);
		if (sync_arity > 1) {
			ir_node *sync = new_r_Sync(block, sync_arity, sync_mem);
			set_Call_mem(call, sync);
		} else {
			set_Call_mem(call, sync_mem[0]);
		}
		DEL_ARR_F(sync_mem);
	}
}

/* Inlines a method at the given call site. */
static int inline_method(ir_node *const call, ir_graph *called_graph)
{
	/* we cannot inline some types of calls */
	if (! can_inline(call, called_graph))
		return 0;

	/* We cannot inline a recursive call. The graph must be copied before
	 * the call the inline_method() using create_irg_copy(). */
	ir_graph *irg = get_irn_irg(call);
	if (called_graph == irg)
		return 0;

	ir_entity *ent      = get_irg_entity(called_graph);
	ir_type   *mtp      = get_entity_type(ent);
	ir_type   *ctp      = get_Call_type(call);
	int        n_params = get_method_n_params(mtp);

	ir_graph *rem = current_ir_graph;
	current_ir_graph = irg;

	DB((dbg, LEVEL_1, "Inlining %+F(%+F) into %+F\n", call, called_graph, irg));

	/* optimizations can cause problems when allocating new nodes */
	int rem_opt = get_opt_optimize();
	set_optimize(0);

	/* Handle graph state */
	assert(get_irg_pinned(irg) == op_pin_state_pinned);
	assert(get_irg_pinned(called_graph) == op_pin_state_pinned);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
	                   | IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	set_irg_callee_info_state(irg, irg_callee_info_inconsistent);
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	edges_deactivate(irg);

	/* here we know we WILL inline, so inform the statistics */
	hook_inline(call, called_graph);

	/* -- Decide how to handle exception control flow: Is there a handler
	   for the Call node, or do we branch directly to End on an exception?
	   exc_handling:
	   0 There is a handler.
	   2 Exception handling not represented in Firm. -- */
	ir_node *Xproj = NULL;
	for (ir_node *proj = (ir_node*)get_irn_link(call); proj != NULL;
		 proj = (ir_node*)get_irn_link(proj)) {
		long proj_nr = get_Proj_proj(proj);
		if (proj_nr == pn_Call_X_except) Xproj = proj;
	}
	enum exc_mode exc_handling = Xproj != NULL ? exc_handler : exc_no_handler;

	/* entitiy link is used to link entities on old stackframe to the
	 * new stackframe */
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* If the call has parameters, copy all parameter entities */
	if (n_params != 0) {
		copy_parameter_entities(call, called_graph);
	}

	/* create the argument tuple */
	ir_node **args_in = ALLOCAN(ir_node*, n_params);

	ir_node *block = get_nodes_block(call);
	for (int i = n_params - 1; i >= 0; --i) {
		ir_node *arg      = get_Call_param(call, i);
		ir_type *param_tp = get_method_param_type(mtp, i);
		ir_mode *mode     = get_type_mode(param_tp);

		if (!is_aggregate_type(param_tp) && mode != get_irn_mode(arg)) {
			arg = new_r_Conv(block, arg, mode);
		}
		args_in[i] = arg;
	}

	/* the procedure and later replaces the Start node of the called graph.
	 * Post_call is the old Call node and collects the results of the called
	 * graph. Both will end up being a tuple. */
	ir_node *post_bl = get_nodes_block(call);
	/* XxMxPxPxPxT of Start + parameter of Call */
	ir_node *in[pn_Start_max+1];
	in[pn_Start_M]              = get_Call_mem(call);
	in[pn_Start_X_initial_exec] = new_r_Jmp(post_bl);
	in[pn_Start_P_frame_base]   = get_irg_frame(irg);
	in[pn_Start_T_args]         = new_r_Tuple(post_bl, n_params, args_in);
	ir_node *pre_call = new_r_Tuple(post_bl, pn_Start_max+1, in);

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
	ir_node *start_block = get_irg_start_block(called_graph);
	set_new_node(start_block, get_nodes_block(pre_call));
	mark_irn_visited(start_block);

	ir_node *start = get_irg_start(called_graph);
	set_new_node(start, pre_call);
	mark_irn_visited(start);

	ir_node *nomem = get_irg_no_mem(called_graph);
	set_new_node(nomem, get_irg_no_mem(irg));
	mark_irn_visited(nomem);

	/* copy entities and nodes */
	assert(!irn_visited(get_irg_end(called_graph)));
	copy_frame_entities(called_graph, irg);
	irg_walk_core(get_irg_end(called_graph), copy_node_inline, set_preds_inline,
	              irg);

	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

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
	ir_node *end_bl = get_new_node(get_irg_end_block(called_graph));
	ir_node *end    = get_new_node(get_irg_end(called_graph));
	int      arity  = get_Block_n_cfgpreds(end_bl); /* arity = n_exc + n_ret  */
	int      n_res  = get_method_n_ress(get_Call_type(call));

	ir_node **res_pred = XMALLOCN(ir_node*, n_res);
	ir_node **cf_pred  = XMALLOCN(ir_node*, arity);

	/* archive keepalives */
	int irn_arity = get_irn_arity(end);
	for (int i = 0; i < irn_arity; i++) {
		ir_node *ka = get_End_keepalive(end, i);
		if (! is_Bad(ka))
			add_End_keepalive(get_irg_end(irg), ka);
	}

	/* replace Return nodes by Jump nodes */
	int n_ret = 0;
	for (int i = 0; i < arity; i++) {
		ir_node *ret = get_Block_cfgpred(end_bl, i);
		if (is_Return(ret)) {
			ir_node *block = get_nodes_block(ret);
			cf_pred[n_ret] = new_r_Jmp(block);
			n_ret++;
		}
	}
	set_irn_in(post_bl, n_ret, cf_pred);

	/* build a Tuple for all results of the method.
	 * add Phi node if there was more than one Return. */
	/* First the Memory-Phi */
	int n_mem_phi = 0;
	for (int i = 0; i < arity; i++) {
		ir_node *ret = get_Block_cfgpred(end_bl, i);
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
	ir_node *const call_mem = new_r_Phi(post_bl, n_mem_phi, cf_pred, mode_M);
	/* Conserve Phi-list for further inlinings -- but might be optimized */
	if (get_nodes_block(call_mem) == post_bl) {
		set_irn_link(call_mem, get_irn_link(post_bl));
		set_irn_link(post_bl, call_mem);
	}
	/* Now the real results */
	ir_node *call_res;
	if (n_res > 0) {
		for (int j = 0; j < n_res; j++) {
			ir_type *res_type     = get_method_res_type(ctp, j);
			ir_mode *res_mode     = get_type_mode(res_type);
			bool     is_aggregate = is_aggregate_type(res_type);
			int n_ret = 0;
			for (int i = 0; i < arity; i++) {
				ir_node *ret = get_Block_cfgpred(end_bl, i);
				if (is_Return(ret)) {
					ir_node *res = get_Return_res(ret, j);
					if (is_aggregate) {
						res_mode = get_irn_mode(res);
					} else if (get_irn_mode(res) != res_mode) {
						ir_node *block = get_nodes_block(res);
						res = new_r_Conv(block, res, res_mode);
					}
					cf_pred[n_ret] = res;
					n_ret++;
				}
			}
			ir_node *const phi = n_ret > 0
				? new_r_Phi(post_bl, n_ret, cf_pred, res_mode)
				: new_r_Bad(irg, res_mode);
			res_pred[j] = phi;
			/* Conserve Phi-list for further inlinings -- but might be optimized */
			if (get_nodes_block(phi) == post_bl) {
				set_Phi_next(phi, get_Block_phis(post_bl));
				set_Block_phis(post_bl, phi);
			}
		}
		call_res = new_r_Tuple(post_bl, n_res, res_pred);
	} else {
		call_res = new_r_Bad(irg, mode_T);
	}
	/* handle the regular call */
	ir_node *const call_x_reg = new_r_Jmp(post_bl);

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
	ir_node *call_x_exc;
	if (exc_handling == exc_handler) {
		int n_exc = 0;
		for (int i = 0; i < arity; i++) {
			ir_node *ret = get_Block_cfgpred(end_bl, i);
			ir_node *irn = skip_Proj(ret);
			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				++n_exc;
			}
		}
		if (n_exc > 0) {
			if (n_exc == 1) {
				/* simple fix */
				call_x_exc = cf_pred[0];
			} else {
				ir_node *block = new_r_Block(irg, n_exc, cf_pred);
				call_x_exc = new_r_Jmp(block);
			}
		} else {
			call_x_exc = new_r_Bad(irg, mode_X);
		}
	} else {
		/* assert(exc_handling == 1 || no exceptions. ) */
		int n_exc = 0;
		for (int i = 0; i < arity; i++) {
			ir_node *ret = get_Block_cfgpred(end_bl, i);
			ir_node *irn = skip_Proj(ret);

			if (is_fragile_op(irn) || is_Raise(irn)) {
				cf_pred[n_exc] = ret;
				n_exc++;
			}
		}
		ir_node  *main_end_bl       = get_irg_end_block(irg);
		int       main_end_bl_arity = get_irn_arity(main_end_bl);
		ir_node **end_preds         = XMALLOCN(ir_node*, n_exc+main_end_bl_arity);

		for (int i = 0; i < main_end_bl_arity; ++i)
			end_preds[i] = get_irn_n(main_end_bl, i);
		for (int i = 0; i < n_exc; ++i)
			end_preds[main_end_bl_arity + i] = cf_pred[i];
		set_irn_in(main_end_bl, n_exc + main_end_bl_arity, end_preds);
		call_x_exc = new_r_Bad(irg, mode_X);
		free(end_preds);
	}
	free(res_pred);
	free(cf_pred);

	ir_node *const call_in[] = {
		[pn_Call_M]         = call_mem,
		[pn_Call_T_result]  = call_res,
		[pn_Call_X_regular] = call_x_reg,
		[pn_Call_X_except]  = call_x_exc,
	};
	turn_into_tuple(call, ARRAY_SIZE(call_in), call_in);

	/* --  Turn CSE back on. -- */
	set_optimize(rem_opt);
	current_ir_graph = rem;

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);

	return 1;
}

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
 * Returns the irg called from a Call node. If the irg is not
 * known, NULL is returned.
 *
 * @param call  the call node
 */
static ir_graph *get_call_called_irg(ir_node *call)
{
	ir_node *addr;

	addr = get_Call_ptr(call);
	if (is_SymConst_addr_ent(addr)) {
		ir_entity *ent = get_SymConst_entity(addr);
		/* we don't know which function gets finally bound to a weak symbol */
		if (get_entity_linkage(ent) & IR_LINKAGE_WEAK)
			return NULL;

		return get_entity_irg(ent);
	}

	return NULL;
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
 * Calculate the parameter weights for transmitting the address of a local variable.
 */
static unsigned calc_method_local_weight(ir_node *arg)
{
	int      j;
	unsigned v, weight = 0;

	for (unsigned i = get_irn_n_outs(arg); i-- > 0; ) {
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
					for (unsigned k = get_irn_n_outs(succ); k-- > 0; ) {
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
	size_t   nparams;
	long     proj_nr;
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
	for (unsigned i = get_irn_n_outs(irg_args); i-- > 0; ) {
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
static unsigned get_method_local_adress_weight(ir_graph *callee, size_t pos)
{
	inline_irg_env *env = (inline_irg_env*)get_irg_link(callee);

	if (env->local_weights == NULL)
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
	size_t    i, n_params;
	ir_node   *frame_ptr;
	ir_type   *mtp;
	int       weight = 0;
	int       all_const;
	unsigned  cc, v;

	inline_irg_env *callee_env;

	mtp_additional_properties props = get_entity_additional_properties(ent);
	if (props & mtp_property_noinline) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: inlining forbidden\n",
		    call, callee));
		return entry->benefice = INT_MIN;
	}

	if (props & mtp_property_noreturn) {
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
		size_t max_regs = cc & ~cc_bits;

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

	/* and finally for leafs: they do not increase the register pressure
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

typedef struct walk_env_t {
	ir_graph **irgs;
	size_t   last_irg;
} walk_env_t;

/**
 * Callgraph walker, collect all visited graphs.
 */
static void callgraph_walker(ir_graph *irg, void *data)
{
	walk_env_t *env = (walk_env_t *)data;
	env->irgs[env->last_irg++] = irg;
}

/**
 * Creates an inline order for all graphs.
 *
 * @return the list of graphs.
 */
static ir_graph **create_irg_list(void)
{
	ir_entity  **free_methods;
	size_t     n_irgs = get_irp_n_irgs();
	walk_env_t env;

	cgana(&free_methods);
	free(free_methods);

	compute_callgraph();

	env.irgs     = XMALLOCNZ(ir_graph*, n_irgs);
	env.last_irg = 0;

	callgraph_walk(NULL, callgraph_walker, &env);
	assert(n_irgs == env.last_irg);

	free_callgraph();

	return env.irgs;
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
	ir_graph *callee   = call->callee;
	int       benefice = calc_inline_benefice(call, callee);

	DB((dbg, LEVEL_2, "In %+F Call %+F to %+F has benefice %d\n",
	    get_irn_irg(call->call), call->call, callee, benefice));

	ir_entity                *ent   = get_irg_entity(callee);
	mtp_additional_properties props = get_entity_additional_properties(ent);
	if (!(props & mtp_property_always_inline) && benefice < inline_threshold) {
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
		ir_entity           *ent        = get_irg_entity(callee);
		mtp_additional_properties props
			= get_entity_additional_properties(ent);
		ir_graph            *calleee;
		int                 loop_depth;

		if (!(props & mtp_property_always_inline)
		    && env->n_nodes + callee_env->n_nodes > maxsize) {
			DB((dbg, LEVEL_2, "%+F: too big (%d) + %+F (%d)\n", irg,
						env->n_nodes, callee, callee_env->n_nodes));
			continue;
		}

		calleee = pmap_get(ir_graph, copied_graphs, callee);
		if (calleee != NULL) {
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
			callee     = calleee;
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
			 * Note that recursive methods are never leafs, so it is
			 * sufficient to test this condition here.
			 */
			copy = create_irg_copy(callee);

			/* create_irg_copy() destroys the Proj links, recompute them */
			phiproj_computed = 0;

			ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK|IR_RESOURCE_PHI_LIST);

			/* allocate a new environment */
			callee_env = alloc_inline_irg_env();
			set_irg_link(copy, callee_env);

			assure_irg_properties(copy, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
			memset(&wenv, 0, sizeof(wenv));
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

		/* callee was inline. Append its call list. */
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
			if (get_irn_irg(new_call) != irg) {
				/* centry->call has not been copied, which means it is dead.
				 * This might happen during inlining, if a const function,
				 * which cannot be inlined is only used as an unused argument
				 * of another function, which is inlined. */
				continue;
			}
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
	size_t           i, n_irgs;
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
	wenv.ignore_callers = 0;
	for (i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];

		free_callee_info(irg);

		wenv.x = (inline_irg_env*)get_irg_link(irg);
		assure_loopinfo(irg);
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

	free(irgs);

	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

void firm_init_inline(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.inline");
}
