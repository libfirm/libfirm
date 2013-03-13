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
#include <math.h>

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
#include "pdeq.h"

#include "irouts.h"
#include "irloop_t.h"
#include "irbackedge_t.h"
#include "opt_init.h"
#include "cgana.h"
#include "trouts.h"
#include "error.h"
#include "execfreq.h"
#include "irmemory_t.h"

#include "analyze_irg_args.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irhooks.h"
#include "irtools.h"
#include "iropt_dbg.h"
#include "irnodemap.h"
#include "apqueue.h"


DEBUG_ONLY(static firm_dbg_module_t *dbg;)

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

/**
 * Copy node for inlining.  Updates attributes that change when
 * inlining but not for dead node elimination.
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
	irn_rewire_inputs(node);

	/* move constants into start block */
	ir_node *new_node = get_new_node(node);
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
	ir_entity                *called = get_irg_entity(called_graph);
	mtp_additional_properties props  = get_entity_additional_properties(called);
	if (props & mtp_property_noinline)
		return false;

	ir_type *called_type = get_entity_type(called);
	size_t   n_params    = get_method_n_params(called_type);
	ir_type *call_type   = get_Call_type(call);
	size_t   n_arguments = get_method_n_params(call_type);
	if (n_arguments != n_params) {
		/* this is a bad feature of C: without a prototype, we can
		 * call a function with less parameters than needed. Currently
		 * we don't support this, although we could use Unknown than. */
		return false;
	}
	size_t n_res = get_method_n_ress(called_type);
	if (n_res != get_method_n_ress(call_type)) {
		return false;
	}

	/* Argh, compiling C has some bad consequences:
	 * It is implementation dependent what happens in that case.
	 * We support inlining, if the bitsize of the types matches AND
	 * the same arithmetic is used. */
	for (size_t i = 0; i < n_params; ++i) {
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
	for (size_t i = 0; i < n_res; ++i) {
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
	ir_type *frame_type = get_irg_frame_type(called_graph);
	for (size_t i = 0, n_entities = get_class_n_members(frame_type);
	     i < n_entities; ++i) {
		ir_entity *ent = get_class_member(frame_type, i);
		if (is_method_entity(ent))
			return false;
		if (is_parameter_entity(ent)
            && (get_entity_parameter_number(ent) == IR_VA_START_PARAMETER_NUMBER))
			return false;
	}

	bool res = true;
	irg_walk_graph(called_graph, find_addr, NULL, &res);

	return res;
}

enum exc_mode {
	exc_handler,    /**< There is a handler. */
	exc_no_handler  /**< Exception handling not represented. */
};

/**
 * copy all entities on the stack frame on 1 irg to the stack frame of another.
 * Sets entity links of the old entities to the copies
 */
static void copy_frame_entities(ir_graph *from, ir_graph *to)
{
	ir_type *from_frame = get_irg_frame_type(from);
	ir_type *to_frame   = get_irg_frame_type(to);
	assert(from_frame != to_frame);
	for (size_t i = 0, n_members = get_class_n_members(from_frame);
	     i < n_members; ++i) {
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
				 * The first time a CopyB node is added it may overwrite
				 * sync_mem[0], because the CopyB node itself references it.
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
static bool inline_method(ir_node *const call, ir_graph *called_graph)
{
	/* we cannot inline some types of calls */
	if (!can_inline(call, called_graph))
		return false;

	/* We cannot inline a recursive call. The graph must be copied before
	 * the call the inline_method() using create_irg_copy(). */
	ir_graph *irg = get_irn_irg(call);
	if (called_graph == irg)
		return false;

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

	/* entity link is used to link entities on old stack frame to the
	 * new stack frame */
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* If the call has parameters, copy all parameter entities */
	ir_entity *ent      = get_irg_entity(called_graph);
	ir_type   *mtp      = get_entity_type(ent);
	int        n_params = get_method_n_params(mtp);
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
		if (!is_Bad(ka))
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
			/* We rely that all cfops have the memory output at the same
			 * position. */
			cf_pred[n_mem_phi++] = new_r_Proj(ret, mode_M, 0);
		} else if (is_Raise(ret)) {
			cf_pred[n_mem_phi++] = new_r_Proj(ret, mode_M, 1);
		}
	}
	ir_node *const call_mem = new_r_Phi(post_bl, n_mem_phi, cf_pred, mode_M);
	/* Conserve Phi-list for further inlining -- but might be optimized */
	if (get_nodes_block(call_mem) == post_bl) {
		set_irn_link(call_mem, get_irn_link(post_bl));
		set_irn_link(post_bl, call_mem);
	}
	/* Now the real results */
	ir_type *ctp = get_Call_type(call);
	ir_node *call_res;
	if (n_res > 0) {
		for (int j = 0; j < n_res; j++) {
			ir_type *res_type     = get_method_res_type(ctp, j);
			bool     is_aggregate = is_aggregate_type(res_type);
			ir_mode *res_mode     = is_aggregate ? mode_P_data
			                                     : get_type_mode(res_type);
			int n_ret = 0;
			for (int i = 0; i < arity; i++) {
				ir_node *ret = get_Block_cfgpred(end_bl, i);
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
			ir_node *const phi = n_ret > 0
				? new_r_Phi(post_bl, n_ret, cf_pred, res_mode)
				: new_r_Bad(irg, res_mode);
			res_pred[j] = phi;
			/* Conserve Phi-list for further inlining -- but might be
			 * optimized */
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
	   We need to add a Phi node to collect the memory containing the exception
	   objects.  Further we need to add another block to get a correct
	   representation of this Phi.  To this block we add a Jmp that resolves
	   into the X output of the Call when the Call is turned into a tuple.
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

	return true;
}

/* Obstack for the inline pass */
static struct obstack  temp_obst;

/**
 * Environment for inlining, which will be set in the link of every irg
 */
typedef struct {
	list_head all_calls;         /**< All Calls in the graph which will be inlined. */
	list_head affected_calls;    /**< All calls that are affected by changing this irg. */
	unsigned  *local_weights;    /**< Once allocated, the beneficial weight for transmitting local addresses. */
	unsigned  n_nodes;           /**< Number of nodes in graph except Id, Tuple, Proj, Start, End. */
	unsigned  n_blocks;          /**< Number of Blocks in graph without Start and End block. */
	unsigned  n_nodes_orig;      /**< for statistics */
	unsigned  n_call_nodes;      /**< Number of Call nodes in the graph. */
	unsigned  n_call_nodes_orig; /**< for statistics */
	unsigned  n_callers;         /**< Number of known graphs that call this graphs. */
	unsigned  n_callers_orig;    /**< for statistics */
	unsigned  got_inline:1;      /**< Set, if at least one call inside this graph was inlined. */
	unsigned  recursive:1;       /**< Set, if the graph contains recursive calls. */
	unsigned  n_alloca;          /**< Number of alloca nodes in the graph. */
} irg_env_t;

/**
 * Allocate a new environment for inlining.
 */
static irg_env_t *alloc_irg_env(void)
{
	irg_env_t *env         = OALLOC(&temp_obst, irg_env_t);
	INIT_LIST_HEAD(&env->affected_calls);
	INIT_LIST_HEAD(&env->all_calls);
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
	env->n_alloca          = 0;
	return env;
}

/* Weights for the inline benefice */
enum inline_weights {
	register_param_weight     = 2,     /**< Factor for parameters in registers. */
	stack_param_weight        = 5,     /**< Factor for parameters on the stack. */
	all_const_param_weight    = 250,   /**< Addend, if all parameters are constant. */
	load_store_param_weight   = 12,    /**< Addend for load/store argument nodes. */
	const_sel_param_weight    = 6,     /**< Addend for constant Sel argument nodes. */
	one_call_to_static_weight = 750,   /**< Will be added, if the graph is only called once. */
	recursive_call_weight     = -150,  /**< Will be subtracted if the call is recursive. */
	leaf_function_weight      = 800,   /**< Addend, if the function is a leaf. */
	one_block_function_weight = 50,    /**< Will be added, if the function consists of one block. */
	block_function_weight     = -4,    /**< Factor for the number of blocks. */
	small_function_weight     = 1000,  /**< Addend for a small function. */
	inner_loop_weight         = 1000,  /**< Will be multiplied with the loop depth, then added. */
	large_function_weight     = -3,    /**< Will be multiplied with the number of nodes. */
	alloca_function_weight    = -350,  /**< Multiplied with the call/alloca frequency and subtracted. */
};

/**
 * Calculate the parameter weights for transmitting the address of a local
 * variable.
 */
static unsigned calc_method_local_weight(ir_node *arg)
{
	unsigned weight = 0;

	for (unsigned i = get_irn_n_outs(arg); i-- > 0; ) {
		ir_node *succ = get_irn_out(arg, i);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
		case iro_Store:
			/* Loads and Store can be removed */
			weight += load_store_param_weight;
			break;
		case iro_Sel:
			/* check if all args are constant */
			for (int j = get_Sel_n_indexs(succ); j-- > 0; ) {
				ir_node *idx = get_Sel_index(succ, j);
				if (!is_Const(idx))
					return 0;
			}
			/* Check users on this Sel. Note: if a 0 is returned here, there was
			   some unsupported node. */
			unsigned v = calc_method_local_weight(succ);
			if (v == 0)
				return 0;
			/* we can kill one Sel with constant indexes, this is cheap */
			weight += v + const_sel_param_weight;
			break;
		case iro_Id:
			/* when looking backward we might find Id nodes */
			weight += calc_method_local_weight(succ);
			break;
		case iro_Tuple:
			/* unoptimized tuple */
			for (int j = get_Tuple_n_preds(succ); j-- > 0; ) {
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
 * Calculate the parameter weights for transmitting the address of a local
 * variable.
 */
static void analyze_irg_local_weights(irg_env_t *env, ir_graph *irg)
{
	ir_entity *ent     = get_irg_entity(irg);
	ir_type   *mtp     = get_entity_type(ent);
	size_t     nparams = get_method_n_params(mtp);


	/* allocate a new array. currently used as 'analysed' flag */
	env->local_weights = NEW_ARR_DZ(unsigned, &temp_obst, nparams);

	/* If the method does not have parameters we have nothing to do. */
	if (nparams <= 0)
		return;

	assure_irg_outs(irg);
	ir_node  *irg_args   = get_irg_args(irg);
	unsigned  n_arg_outs = get_irn_n_outs(irg_args);

	for (int i = n_arg_outs - 1; i >= 0; --i) {
		ir_node *arg     = get_irn_out(irg_args, i);
		long     proj_nr = get_Proj_proj(arg);
		env->local_weights[proj_nr] = calc_method_local_weight(arg);
	}
}

/**
 * Calculate the benefice for transmitting an local variable address.
 * After inlining, the local variable might be transformed into a
 * SSA variable by scalar_replacement().
 */
static unsigned get_method_local_address_weight(ir_graph *called, size_t pos)
{
	irg_env_t *env = (irg_env_t*)get_irg_link(called);

	if (env->local_weights == NULL)
		analyze_irg_local_weights(env, called);

	assert(pos < get_method_n_params(get_entity_type(get_irg_entity(called))));
	return env->local_weights[pos];
}

/**
 * Environment for the heuristic analysis of a call.
 */
typedef struct {
	ir_node      *call;             /**< The Call node. */
	ir_graph     *called;           /**< The called IR-graph. */
	list_head     list_all;         /**< List head for linking the next call in the graph. */
	list_head     list_affected;    /**< List head for linking the next affected call. */
	apqueue_el_t *address;          /**< Address for the priority queue. */
	int           benefice_static;  /**< Calculated benefice which will not change. */
	int           benefice_dynamic; /**< Calculated benefice which could change. */
	double        execfreq;         /**< Execution frequency of this call. */
	int           loop_depth;       /**< Depth of the loop. */
} call_env_t;

/**
 * Calculates the static benefice for a given call.
 * The static benefice will only be calculated at most once for every call in the inliner.
 *
 * @param cenv The environment of a call.
 * @return A benefice value. A high benefice indicates a good call for inlining.
 */
static int calc_call_static_benefice(call_env_t *cenv)
{
	ir_node                   *call       = cenv->call;
	ir_graph                  *called     = cenv->called;
	ir_graph                  *callee     = get_irn_irg(call);
	ir_entity                 *called_ent = get_irg_entity(called);
	irg_env_t                 *called_env = get_irg_link(called);
	mtp_additional_properties  props      = get_entity_additional_properties(called_ent);
	if (props & mtp_property_noinline) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: inlining forbidden\n",
		    call, called));
		return cenv->benefice_static = INT_MIN;
	}

	if (props & mtp_property_noreturn) {
		DB((dbg, LEVEL_2, "In %+F Call to %+F: not inlining noreturn or weak\n",
		    call, called));
		return cenv->benefice_static = INT_MIN;
	}

	/* Costs for passed parameters */
	size_t    n_params           = get_Call_n_params(call);
	ir_type  *mtp                = get_entity_type(called_ent);
	unsigned  calling_convention = get_method_calling_convention(mtp);
	int64_t   weight             = 0;

	if (calling_convention & cc_reg_param) {
		/* Register parameter, the calling cost is lower and inlining less important */
		size_t max_regs = calling_convention & ~cc_bits;

		if (max_regs < n_params) {
			weight += max_regs * register_param_weight;
			weight += (n_params - max_regs) * stack_param_weight;
		} else {
			weight += n_params * register_param_weight;
		}
	} else {
		/* parameters are passed on stack */
		weight += stack_param_weight * n_params;
	}

	/* constant parameters improve the benefice */
	ir_node *frame_ptr = get_irg_frame(callee);
	bool     all_const = true;
	for (size_t i = 0; i < n_params; ++i) {
		ir_node *param = get_Call_param(call, i);

		if (is_Const(param)) {
			weight += get_method_param_weight(called_ent, i);
		} else {
			all_const = false;
			if (is_SymConst(param))
				weight += get_method_param_weight(called_ent, i);
			else if (is_Sel(param) && get_Sel_ptr(param) == frame_ptr) {
				/*
				 * An address of a local variable is transmitted. After
				 * inlining, scalar_replacement might be able to remove the
				 * local variable, so honor this.
				 */
				weight += get_method_local_address_weight(called, i);
			}
		}
	}

	/* Give an extra bonus, if all arguments are constant */
	if (all_const) {
		weight += all_const_param_weight;
	}

	/* One call, non-recursive with internal linkage: this is very good */
	if (called_env->n_callers == 1 &&
	    called != current_ir_graph &&
	    !entity_is_externally_visible(called_ent)) {
		weight += one_call_to_static_weight;
	}

	/* Recursive calls are rarely good to inline, but const params would be ok */
	if (callee == called && !all_const) {
		weight += recursive_call_weight;
	}

	/* Leafs do not increase the register pressure because of callee safe registers */
	if (called_env->n_call_nodes == 0) {
		weight += leaf_function_weight;
	}

	/* It's important to inline inner loops first */
	if (cenv->loop_depth > 30) {
		weight += 30 * inner_loop_weight;
	} else {
		weight += cenv->loop_depth * inner_loop_weight;
	}

	assert(weight < INT_MAX && "weight too big for int");
	return cenv->benefice_static = weight;
}

/**
 * Calculates the dynamic benefice for a given call.
 * The dynamic benefice can change during the inlining process and should thus be
 * fairly easy to calculate, as it is called more than once.
 *
 * @param cenv The environment of a call.
 * @return A benefice value. A high benefice indicates a good call for inlining.
 */
static int calc_call_dynamic_benefice(call_env_t *cenv)
{
	irg_env_t *called_env = (irg_env_t*)get_irg_link(cenv->called);
	int64_t   weight      = 0;

	/* Give a bonus for functions with one block,
	 * or a penalty if there are more than one    */
	if (called_env->n_blocks == 1) {
		weight += one_block_function_weight;
	} else {
		long n_blocks = called_env->n_blocks;
		weight += block_function_weight * n_blocks;
	}

	/* Give a bonus for small non-recursive functions:
	 * we want them to be inlined in almost every case */
	if (called_env->n_nodes < 30 && !called_env->recursive)
		weight += small_function_weight;

	/* Give a penalty for the called graph size */
	long n_nodes = called_env->n_nodes;
	weight += n_nodes * large_function_weight;

	if (called_env->n_alloca > 0) {
		/* This is overly conservative, because inlining of alloca calls can
		 * increase the program's memory overhead drastically.
		 * Therefore only inline if the circumstances are perfect, i.e. no loops
		 * in the callee (low execution frequency).
		 *
		 * Moreover, we cannot differentiate between alloca() and VLA yet, so
		 * this could disable inlining of functions using VLA (which are completely
		 * save).
		 *
		 * 2 Solutions:
		 * - add a flag to the Alloc node for "real" alloca() calls
		 * - add a new Stack-Restore node at the end of a functions using alloca()
		 */
		long n_alloca = called_env->n_alloca;
		weight += n_alloca * alloca_function_weight * cenv->execfreq;
	}

	assert(weight < INT_MAX && "weight too big for int");
	return cenv->benefice_dynamic = weight;
}

typedef struct {
	irg_env_t  *ienv;
} collect_wenv_t;

/**
 * Calculates a priority value from the inline benefice and the execution frequency.
 */
static int calc_priority(int benefice, double execfreq)
{
	return execfreq * benefice;
}

/**
 * post-walker: Calculate the priority for all calls and push them into a priority queue.
 * If the node is not a call node, then only sum some statistics.
 */
static void find_calls(ir_node *node, void *ctx)
{
	collect_wenv_t *wenv = (collect_wenv_t*)ctx;
	irg_env_t      *ienv = wenv->ienv;
	unsigned        code = get_irn_opcode(node);

	/* count meaningful nodes in irg */
	if (code != iro_Proj && code != iro_Tuple && code != iro_Sync) {
		if (code != iro_Block) {
			++ienv->n_nodes;
			++ienv->n_nodes_orig;
		} else {
			++ienv->n_blocks;
		}
	}

	if (is_Alloc(node)) {
		++ienv->n_alloca;
	}

	if (code != iro_Call) return;

	/* collect all call nodes */
	++ienv->n_call_nodes;
	++ienv->n_call_nodes_orig;

	ir_entity *called_ent = get_Call_callee(node);
	if (called_ent == NULL)
		return;
	ir_graph *called = get_entity_linktime_irg(called_ent);
	if (called != NULL) {
		irg_env_t *called_env = (irg_env_t*)get_irg_link(called);

		/* count alle static callers */
		++called_env->n_callers;
		++called_env->n_callers_orig;

		if (called == current_ir_graph)
			++ienv->recursive;

		/* Create call environment */
		call_env_t *cenv = OALLOC(&temp_obst, call_env_t);
		cenv->call = node;
		cenv->called = called;
		cenv->address = NULL;
		cenv->loop_depth = get_irn_loop(get_nodes_block(node))->depth;
		ir_node *block = get_nodes_block(node);

		/* Get the execution frequency */
		cenv->execfreq = get_block_execfreq(block);

		/* Add to list of calls of the callee graph */
		list_add_tail(&cenv->list_all, &ienv->all_calls);

		/* Add to list of affected calls of called graph*/
		list_add_tail(&cenv->list_affected, &called_env->affected_calls);
	}
}

/**
 * Calculates the priority of a given call and maybe pushes the call in the
 * priority queue.
 */
static void handle_call(call_env_t *cenv, apqueue_t *pq, int threshold)
{
	int benefice = calc_call_static_benefice(cenv);
	benefice += calc_call_dynamic_benefice(cenv);

	/* Calculate priority and return or add to the priority queue */
	int priority = calc_priority(benefice, cenv->execfreq);

	DB((dbg, LEVEL_2, "In %F %+F to %+F has priority %d\n",
	    get_irn_irg(cenv->call), cenv->call, cenv->called, priority));

	ir_entity                 *ent   = get_irg_entity(cenv->called);
	mtp_additional_properties  props = get_entity_additional_properties(ent);
	if (!(props & mtp_property_always_inline) && priority < threshold) {
		return;
	}

	apqueue_el_t *addr = apqueue_put(pq, cenv, priority);
	cenv->address = addr;
}

/**
 * Copies a call environment.
 */
static call_env_t *copy_call_env_t(call_env_t *cenv, ir_node *call, unsigned loop_delta,
                          unsigned execfreq_delta)
{
	call_env_t *new_env       = OALLOC(&temp_obst, call_env_t);
	new_env->call             = call;
	new_env->called           = cenv->called;
	new_env->benefice_static  = cenv->benefice_static;
	new_env->benefice_dynamic = cenv->benefice_dynamic;
	new_env->loop_depth       = cenv->loop_depth + loop_delta;
	new_env->execfreq         = cenv->execfreq * execfreq_delta;
	new_env->address          = NULL;

	/* Register by called graph as affected call */
	irg_env_t *called_env     = get_irg_link(cenv->called);
	list_add_tail(&new_env->list_affected, &called_env->affected_calls);

	return new_env;
}

/*
 * Heuristic inliner.
 */
void inline_functions(unsigned maxsize, int inline_threshold,
                       opt_ptr after_inline_opt)
{
	ir_graph *rem = current_ir_graph;

	obstack_init(&temp_obst);
	apqueue_t *pq = new_apqueue();

	/* A container for copied graphs, used to inline recursive calls */
	pmap *copied_graphs = pmap_create();

	/* Find all irgs */
	size_t     n_irgs = get_irp_n_irgs();
	ir_graph **irgs   = XMALLOCNZ(ir_graph*, n_irgs);
	for (size_t i = 0; i < n_irgs; ++i) {
		irgs[i] = get_irp_irg(i);
	}

	/* Extend all irgs by a temporary data structure for inlining */
	for (size_t i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];
		set_irg_link(irg, alloc_irg_env());

		/* Calculate execution frequency, deactivate edges or the inliner won't work*/
		ir_estimate_execfreq(irg);
		edges_deactivate(irg);
	}

	/* Find calls and calculate graph statistics */
	collect_wenv_t wenv;
	for (size_t i = 0; i < n_irgs; ++i) {
		ir_graph *irg = irgs[i];
		wenv.ienv = (irg_env_t*)get_irg_link(irg);

		free_callee_info(irg);
		assure_loopinfo(irg);

		ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

		irg_walk_graph(irg, NULL, find_calls, &wenv);
	}

	/* Calculate priority and push calls in the priority queue */
	for (size_t i = 0; i < n_irgs; ++i) {
		irg_env_t *env = (irg_env_t*)get_irg_link(irgs[i]);
		list_for_each_entry(call_env_t, env_it, &env->all_calls, list_all) {
			handle_call(env_it, pq, inline_threshold);
		}
	}

	/*
	 * All calls in the priority queue should be inlined,
	 * but the priority queue is updated constantly.
	 * Recursive function calls need special considerations.
	 */
	while (!apqueue_empty(pq)) {
		call_env_t *call_env       = apqueue_pop_front(pq);
		ir_node    *call           = call_env->call;
		ir_graph   *call_irg       = get_irn_irg(call);
		irg_env_t  *call_irg_env   = (irg_env_t*)get_irg_link(call_irg);
		ir_graph   *called_irg     = call_env->called;
		irg_env_t  *called_irg_env = (irg_env_t*)get_irg_link(called_irg);
		unsigned   did_inline      = 0;
		unsigned   recursive       = 0;

		/* Check for maxsize, but not when
		 * the called graph has the 'always inline' property. */
		ir_entity                 *ent   = get_irg_entity(called_irg);
		mtp_additional_properties  props = get_entity_additional_properties(ent);
		if (!(props & mtp_property_always_inline)
		    && call_irg_env->n_nodes + called_irg_env->n_nodes > maxsize) {
			DB((dbg, LEVEL_2, "%+F: too big (%d) + %+F (%d)\n", call_irg,
			    call_irg_env->n_nodes, called_irg, called_irg_env->n_nodes));
			continue;
		}

		/* It is not possible to directly inline a recursive call.
		 * So make a copy of the graph or use use an already created copy. */
		if (call_irg == called_irg) {
			recursive      = 1;
			ir_graph *copy = pmap_get(ir_graph, copied_graphs, called_irg);

			/* Copy graph */
			if (copy == NULL) {
				ir_free_resources(call_irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

				copy = create_irg_copy(called_irg);

				ir_reserve_resources(call_irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

				/* Allocate a new environment */
				irg_env_t *copy_env = alloc_irg_env();
				set_irg_link(copy, copy_env);

				assure_irg_properties(copy, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
				memset(&wenv, 0, sizeof(wenv));
				wenv.ienv = copy_env;
				irg_walk_graph(copy, NULL, find_calls, &wenv);

				/* Onle one caller: The original graph */
				copy_env->n_callers      = 1;
				copy_env->n_callers_orig = 1;

				/* Enter the entity of the original graph. This is needed for
				 * inline_method(). However, note that ent->irg still points to
				 * the called graph, NOT to copy. */
				set_irg_entity(copy, get_irg_entity(called_irg));

				pmap_insert(copied_graphs, called_irg, copy);
			}

			/* Remap called_irg and called_irg_env to the copy */
			called_irg     = copy;
			called_irg_env = (irg_env_t*)get_irg_link(copy);
		}

		collect_phiprojs(call_irg);

		did_inline = inline_method(call, called_irg);
		if (!did_inline)
			continue;

		/* Correct statistics */
		call_irg_env->got_inline = 1;

		--call_irg_env->n_call_nodes;
		--called_irg_env->n_callers;

		call_irg_env->n_call_nodes += called_irg_env->n_call_nodes;
		call_irg_env->n_nodes      += called_irg_env->n_nodes;

		/* Add all new calls to the priority queue */
		list_for_each_entry(call_env_t, env_it, &called_irg_env->all_calls,
                            list_all) {

			ir_node    *old_call   = env_it->call;
			ir_node    *new_call;
			call_env_t *new_cenv;

			/* Test if the old call is recursive */
			if (get_irn_irg(old_call) == env_it->called) {
				recursive = 1;
			}

			/* All called methods are now called once more */
			++called_irg_env->n_callers;

			/* We need the copied nodes, not the original nodes in the called graph.
			 * Luckily the inliner leaves this information in the link field. */
			new_call = (ir_node*)get_irn_link(env_it->call);

			if (get_irn_irg(new_call) != call_irg) {
				/* The call has not been copied, which means it is dead.
				 * This might happen during inlining, if a const functions,
				 * which cannot be inlined is only used as an unused argument
				 * of another function, which is inlined. */
				continue;
			}

			if (!is_Call(new_call))
				continue;

			/* Create/Copy call environment */
			if (recursive) {
				/* Do not increase loop depth and execfreq for recursive calls */
				new_cenv = copy_call_env_t(env_it, new_call, 0, 1);
			} else {
				new_cenv = copy_call_env_t(env_it, new_call, call_env->loop_depth,
                                           call_env->execfreq);
			}

			list_add_tail(&new_cenv->list_all, &call_irg_env->all_calls);

			/* Calculate priority and maybe put the call into the queue. */
			handle_call(new_cenv, pq, inline_threshold);
		}

		/* Reassess priority of all affected calls,
		 * because the dynamic benefice has changed. */
		list_for_each_entry(call_env_t, env_it, &call_irg_env->affected_calls,
		                    list_affected) {

			/* There are calls which have never been put in the priority queue. */
			if (env_it->address == NULL)
				continue;

			if (apqueue_contains(env_it->address)) {
				int benefice = calc_call_dynamic_benefice(env_it);
				benefice += env_it->benefice_static;
				int new_priority = calc_priority(benefice, env_it->execfreq);

				/* Check the inline_threshold: Remove call or change priority */
				if (!(props & mtp_property_always_inline)
                    && new_priority < inline_threshold) {
					apqueue_remove(pq, env_it->address);
					DB((dbg, LEVEL_2, "In %F priority of %+F changed to %d and has been removed\n",
                        call_irg, env_it->call, new_priority));
				} else {
					apqueue_change_priority(pq, env_it->address, new_priority);
				}
			}
		}
	}

	/* Do after inline optimization and print some statistics */
	for (size_t i = 0; i < n_irgs; ++i) {
		ir_graph *irg  = irgs[i];
		irg_env_t *env = (irg_env_t*)get_irg_link(irg);

		ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

		if (env->got_inline && after_inline_opt != NULL) {
			after_inline_opt(irg);
		}
		if (env->got_inline || (env->n_callers_orig != env->n_callers)) {
			DB((dbg, LEVEL_1, "Nodes:%3d ->%3d, calls:%3d ->%3d, callers:%3d ->%3d, -- %s\n",
			env->n_nodes_orig, env->n_nodes, env->n_call_nodes_orig, env->n_call_nodes,
			env->n_callers_orig, env->n_callers,
			get_entity_name(get_irg_entity(irg))));
		}
	}

	pmap_destroy(copied_graphs);
	del_apqueue(pq);
	free(irgs);
	obstack_free(&temp_obst, NULL);
	current_ir_graph = rem;
}

void firm_init_inline(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.inline");
}
