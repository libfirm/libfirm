/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       be transform helper extracted from the ia32 backend.
 * @author      Matthias Braun, Michael Beck
 * @date        14.06.2007
 */
#include "pdeq.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "ircons_t.h"
#include "irhooks.h"
#include "iredges.h"
#include "irouts.h"
#include "irgmod.h"
#include "cgana.h"
#include "debug.h"
#include "execfreq_t.h"

#include "bearch.h"
#include "beirg.h"
#include "betranshlp.h"
#include "belive.h"
#include "benode.h"

typedef struct be_transform_env_t {
	ir_graph *irg;         /**< The irg, the node should be created in */
	waitq    *worklist;    /**< worklist of nodes that still need to be
	                            transformed */
	ir_node  *old_anchor;  /**< the old anchor node in the old irg */
} be_transform_env_t;


static be_transform_env_t env;

void be_set_transformed_node(ir_node *old_node, ir_node *new_node)
{
	set_irn_link(old_node, new_node);
	mark_irn_visited(old_node);
}

int be_is_transformed(const ir_node *node)
{
	return irn_visited(node);
}

static inline ir_node *be_get_transformed_node(ir_node *old_node)
{
	if (irn_visited(old_node)) {
		ir_node *new_node = (ir_node*)get_irn_link(old_node);
		assert(new_node != NULL);
		return new_node;
	}
	return NULL;
}

void be_duplicate_deps(ir_node *old_node, ir_node *new_node)
{
	int deps = get_irn_n_deps(old_node);
	for (int i = 0; i < deps; ++i) {
		ir_node *dep     = get_irn_dep(old_node, i);
		ir_node *new_dep = be_transform_node(dep);

		add_irn_dep(new_node, new_dep);
	}
}

ir_node *be_transform_phi(ir_node *node, const arch_register_req_t *req)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_graph *irg   = get_irn_irg(block);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	ir_node **ins   = get_irn_in(node)+1;
	int       arity = get_irn_arity(node);
	ir_mode  *mode  = req->cls != NULL ? req->cls->mode : get_irn_mode(node);
	ir_node  *phi   = new_ir_node(dbgi, irg, block, op_Phi, mode, arity, ins);
	copy_node_attr(irg, node, phi);
	be_duplicate_deps(node, phi);

	backend_info_t *info = be_get_info(phi);
	struct obstack *obst = be_get_be_obst(irg);
	info->in_reqs = OALLOCN(obst, const arch_register_req_t*, arity);
	for (int i = 0; i < arity; ++i) {
		info->in_reqs[i] = req;
	}

	arch_set_irn_register_req_out(phi, 0, req);
	be_enqueue_preds(node);

	return phi;
}

void be_set_transform_function(ir_op *op, be_transform_func func)
{
	/* shouldn't be assigned twice (except for exchanging the default
	 * be_duplicate_node entries) */
	assert(op->ops.generic == NULL
			|| op->ops.generic == (op_func) be_duplicate_node);
	op->ops.generic = (op_func) func;
}

void be_set_transform_proj_function(ir_op *op, be_transform_func func)
{
	op->ops.generic1 = (op_func) func;
}

/**
 * Transform helper for blocks.
 */
static ir_node *transform_block(ir_node *node)
{
	ir_graph *irg   = get_irn_irg(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_node  *block = new_ir_node(dbgi, irg, NULL, get_irn_op(node), mode,
	                              get_irn_arity(node), get_irn_in(node) + 1);
	copy_node_attr(irg, node, block);
	block->node_nr = node->node_nr;

	/* transfer execfreq value */
	double execfreq = get_block_execfreq(node);
	set_block_execfreq(block, execfreq);

	/* put the preds in the worklist */
	be_enqueue_preds(node);

	return block;
}

static ir_node *transform_end(ir_node *node)
{
	/* end has to be duplicated manually because we need a dynamic in array */
	ir_graph *irg     = get_irn_irg(node);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
	ir_node  *new_end = new_ir_node(dbgi, irg, block, op_End, mode_X, -1, NULL);
	copy_node_attr(irg, node, new_end);
	be_duplicate_deps(node, new_end);

	set_irg_end(irg, new_end);

	/* do not transform predecessors yet to keep the pre-transform
	 * phase from visiting all the graph */
	foreach_irn_in(node, i, in) {
		add_End_keepalive(new_end, in);
	}
	be_enqueue_preds(node);

	return new_end;
}

static ir_node *transform_proj(ir_node *node)
{
	ir_node *pred    = get_Proj_pred(node);
	ir_op   *pred_op = get_irn_op(pred);
	be_transform_func *proj_transform
		= (be_transform_func*)pred_op->ops.generic1;
	/* we should have a Proj transformer registered */
	assert(proj_transform != NULL);
	return proj_transform(node);
}

ir_node *be_duplicate_node(ir_node *node)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_graph *irg   = env.irg;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_op    *op    = get_irn_op(node);

	ir_node *new_node;
	if (op->opar == oparity_dynamic) {
		new_node = new_ir_node(dbgi, irg, block, op, mode, -1, NULL);
		foreach_irn_in(node, i, in) {
			add_irn_n(new_node, be_transform_node(in));
		}
	} else {
		int       arity = get_irn_arity(node);
		ir_node **ins   = ALLOCAN(ir_node*, arity);
		foreach_irn_in(node, i, in) {
			ins[i] = be_transform_node(in);
		}

		new_node = new_ir_node(dbgi, irg, block, op, mode, arity, ins);
	}

	copy_node_attr(irg, node, new_node);
	be_duplicate_deps(node, new_node);

	new_node->node_nr = node->node_nr;
	return new_node;
}

ir_node *be_transform_node(ir_node *node)
{
	ir_node *new_node = be_get_transformed_node(node);
	if (new_node != NULL)
		return new_node;

	DEBUG_ONLY(be_set_transformed_node(node, NULL);)

	ir_op *op = get_irn_op(node);
	be_transform_func *transform = (be_transform_func *)op->ops.generic;

	new_node = transform(node);
	assert(new_node != NULL);

	be_set_transformed_node(node, new_node);
	return new_node;
}

void be_enqueue_preds(ir_node *node)
{
	/* put the preds in the worklist */
	foreach_irn_in(node, i, pred) {
		pdeq_putr(env.worklist, pred);
	}
}

/**
 * Rewire nodes which are potential loops (like Phis) to avoid endless loops.
 */
static void fix_loops(ir_node *node)
{
	if (irn_visited_else_mark(node))
		return;

	assert(node_is_in_irgs_storage(env.irg, node));

	bool changed = false;
	if (! is_Block(node)) {
		ir_node *block     = get_nodes_block(node);
		ir_node *new_block = (ir_node*)get_irn_link(block);

		if (new_block != NULL) {
			set_nodes_block(node, new_block);
			block = new_block;
			changed = true;
		}

		fix_loops(block);
	}

	foreach_irn_in(node, i, pred) {
		ir_node *in = pred;
		ir_node *nw = (ir_node*)get_irn_link(in);

		if (nw != NULL && nw != in) {
			set_irn_n(node, i, nw);
			in = nw;
			changed = true;
		}

		fix_loops(in);
	}
	/* fix proj block */
	if (is_Proj(node)) {
		set_nodes_block(node, get_nodes_block(get_Proj_pred(node)));
		changed = true;
	}

	for (int i = 0, arity = get_irn_n_deps(node); i < arity; ++i) {
		ir_node *in = get_irn_dep(node, i);
		ir_node *nw = (ir_node*)get_irn_link(in);

		if (nw != NULL && nw != in) {
			set_irn_dep(node, i, nw);
			in = nw;
			changed = true;
		}

		fix_loops(in);
	}

	if (changed) {
		identify_remember(node);
	}
}

ir_node *be_pre_transform_node(ir_node *place)
{
	if (place == NULL)
		return NULL;

	return be_transform_node(place);
}

static void pre_transform_anchor(ir_graph *irg, int anchor)
{
	ir_node *old_anchor_node = get_irn_n(env.old_anchor, anchor);
	ir_node *transformed     = be_transform_node(old_anchor_node);
	set_irg_anchor(irg, anchor, transformed);
}

/**
 * Transforms all nodes. Deletes the old obstack and creates a new one.
 */
static void transform_nodes(ir_graph *irg, arch_pretrans_nodes *pre_transform)
{
	hook_dead_node_elim(irg, 1);

	inc_irg_visited(irg);

	env.irg        = irg;
	env.worklist   = new_waitq();
	env.old_anchor = irg->anchor;

	ir_node *old_end = get_irg_end(irg);

	/* put all anchor nodes in the worklist */
	for (int i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *anchor = get_irg_anchor(irg, i);

		if (anchor == NULL)
			continue;
		waitq_put(env.worklist, anchor);
	}

	ir_node *new_anchor  = new_r_Anchor(irg);
	irg->anchor = new_anchor;

	/* pre transform some anchors (so they are available in the other transform
	 * functions) */
	pre_transform_anchor(irg, anchor_no_mem);
	pre_transform_anchor(irg, anchor_end_block);
	pre_transform_anchor(irg, anchor_end);
	pre_transform_anchor(irg, anchor_start_block);
	pre_transform_anchor(irg, anchor_start);
	pre_transform_anchor(irg, anchor_frame);

	if (pre_transform)
		pre_transform(irg);

	/* process worklist (this should transform all nodes in the graph) */
	while (! waitq_empty(env.worklist)) {
		ir_node *node = (ir_node*)waitq_get(env.worklist);
		be_transform_node(node);
	}

	/* fix loops and set new anchors*/
	inc_irg_visited(irg);
	for (int i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *anchor = get_irn_n(env.old_anchor, i);

		if (anchor == NULL)
			continue;

		anchor = (ir_node*)get_irn_link(anchor);
		fix_loops(anchor);
		set_irn_n(new_anchor, i, anchor);
	}

	del_waitq(env.worklist);
	free_End(old_end);
	hook_dead_node_elim(irg, 0);
}

void be_transform_graph(ir_graph *irg, arch_pretrans_nodes *func)
{
	/* create a new obstack */
	struct obstack old_obst = irg->obst;
	obstack_init(&irg->obst);
	irg->last_node_idx = 0;

	free_vrp_data(irg);

	/* create new value table for CSE */
	new_identities(irg);

	/* do the main transformation */
	transform_nodes(irg, func);

	/* free the old obstack */
	obstack_free(&old_obst, 0);

	/* most analysis info is wrong after transformation */
	be_invalidate_live_chk(irg);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);

	/* recalculate edges */
	edges_activate(irg);
}

bool be_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	ir_op *op = get_irn_op(node);
	if (op->ops.generic2 == NULL)
		return false;
	upper_bits_clean_func func = (upper_bits_clean_func)op->ops.generic2;
	return func(node, mode);
}

static bool bit_binop_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	return be_upper_bits_clean(get_binop_left(node), mode)
	    && be_upper_bits_clean(get_binop_right(node), mode);
}

static bool mux_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	return be_upper_bits_clean(get_Mux_true(node), mode)
	    && be_upper_bits_clean(get_Mux_false(node), mode);
}

static bool and_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	if (!mode_is_signed(mode)) {
		return be_upper_bits_clean(get_And_left(node), mode)
		    || be_upper_bits_clean(get_And_right(node), mode);
	} else {
		return bit_binop_upper_bits_clean(node, mode);
	}
}

static bool shr_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	if (mode_is_signed(mode)) {
		return false;
	} else {
		const ir_node *right = get_Shr_right(node);
		if (is_Const(right)) {
			ir_tarval *tv  = get_Const_tarval(right);
			long       val = get_tarval_long(tv);
			if (val >= 32 - (long)get_mode_size_bits(mode))
				return true;
		}
		return be_upper_bits_clean(get_Shr_left(node), mode);
	}
}

static bool shrs_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	return be_upper_bits_clean(get_Shrs_left(node), mode);
}

static bool const_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	ir_tarval *tv  = get_Const_tarval(node);
	long       val = get_tarval_long(tv);
	if (mode_is_signed(mode)) {
		long    shifted = val >> (get_mode_size_bits(mode)-1);
		return shifted == 0 || shifted == -1;
	} else {
		unsigned long shifted = (unsigned long)val;
		shifted >>= get_mode_size_bits(mode)-1;
		shifted >>= 1;
		return shifted == 0;
	}
}

static bool conv_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	ir_mode       *dest_mode = get_irn_mode(node);
	const ir_node *op        = get_Conv_op(node);
	ir_mode       *src_mode  = get_irn_mode(op);
	if (mode_is_float(src_mode))
		return true;

	unsigned src_bits  = get_mode_size_bits(src_mode);
	unsigned dest_bits = get_mode_size_bits(dest_mode);
	/* downconvs are a nop */
	if (src_bits >= dest_bits)
		return be_upper_bits_clean(op, mode);
	/* upconvs are fine if src is big enough or if sign matches */
	if (src_bits <= get_mode_size_bits(mode)
		&& mode_is_signed(src_mode) == mode_is_signed(mode))
		return true;
	return false;
}

static bool proj_upper_bits_clean(const ir_node *node, ir_mode *mode)
{
	const ir_node *pred = get_Proj_pred(node);
	switch (get_irn_opcode(pred)) {
	case iro_Load: {
		ir_mode *load_mode = get_Load_mode(pred);
		unsigned load_bits = get_mode_size_bits(load_mode);
		if (load_bits > get_mode_size_bits(mode))
			return false;
		if (mode_is_signed(load_mode) != mode_is_signed(mode))
			return false;
		return true;
	}
	default:
		break;
	}
	return false;
}

void be_set_upper_bits_clean_function(ir_op *op, upper_bits_clean_func func)
{
	op->ops.generic2 = (op_func)func;
}

void be_start_transform_setup(void)
{
	ir_clear_opcodes_generic_func();

	be_set_transform_function(op_Bad,         be_duplicate_node);
	be_set_transform_function(op_be_CopyKeep, be_duplicate_node);
	be_set_transform_function(op_be_IncSP,    be_duplicate_node);
	be_set_transform_function(op_be_Keep,     be_duplicate_node);
	be_set_transform_function(op_Block,       transform_block);
	be_set_transform_function(op_End,         transform_end);
	be_set_transform_function(op_NoMem,       be_duplicate_node);
	be_set_transform_function(op_Pin,         be_duplicate_node);
	be_set_transform_function(op_Proj,        transform_proj);
	be_set_transform_function(op_Start,       be_duplicate_node);
	be_set_transform_function(op_Sync,        be_duplicate_node);

	be_set_upper_bits_clean_function(op_And,   and_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Const, const_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Conv,  conv_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Eor,   bit_binop_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Mux,   mux_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Or,    bit_binop_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Proj,  proj_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Shr,   shr_upper_bits_clean);
	be_set_upper_bits_clean_function(op_Shrs,  shrs_upper_bits_clean);
}

bool be_pattern_is_rotl(ir_node const *const irn_or, ir_node **const left,
                        ir_node **const right)
{
	assert(is_Add(irn_or) || is_Or(irn_or));

	ir_mode *mode = get_irn_mode(irn_or);
	if (!mode_is_int(mode))
		return false;

	ir_node *shl = get_binop_left(irn_or);
	ir_node *shr = get_binop_right(irn_or);
	if (is_Shr(shl)) {
		if (!is_Shl(shr))
			return false;

		ir_node *tmp = shl;
		shl = shr;
		shr = tmp;
	} else if (!is_Shl(shl)) {
		return false;
	} else if (!is_Shr(shr)) {
		return false;
	}

	ir_node *x = get_Shl_left(shl);
	if (x != get_Shr_left(shr))
		return false;

	ir_node *c1 = get_Shl_right(shl);
	ir_node *c2 = get_Shr_right(shr);
	if (is_Const(c1) && is_Const(c2)) {
		ir_tarval *tv1 = get_Const_tarval(c1);
		if (!tarval_is_long(tv1))
			return false;

		ir_tarval *tv2 = get_Const_tarval(c2);
		if (!tarval_is_long(tv2))
			return false;

		if (get_tarval_long(tv1) + get_tarval_long(tv2)
		    != (long) get_mode_size_bits(mode))
			return false;

		*left  = x;
		*right = c1;
		return true;
	}

	/* Note: the obvious rot formulation (a << x) | (a >> (32-x)) gets
	 * transformed to (a << x) | (a >> -x) by transform_node_shift_modulo() */
	if (!ir_is_negated_value(c1, c2))
		return false;

	*left  = x;
	*right = c1;
	return true;
}

void be_map_exc_node_to_runtime_call(ir_node *node, ir_mode *res_mode,
                                     ir_entity *runtime_entity,
                                     long pn_M, long pn_X_regular,
                                     long pn_X_except, long pn_res)
{
	assert(is_memop(node));

	size_t    n_in = get_irn_arity(node)-1;
	ir_node **in   = ALLOCAN(ir_node*, n_in);
	ir_type  *mtp  = get_entity_type(runtime_entity);

	assert(get_method_n_params(mtp) == n_in);
	size_t p = 0;
	foreach_irn_in(node, i, n) {
		if (get_irn_mode(n) == mode_M)
			continue;
		in[p++] = n;
	}
	assert(p == n_in);

	ir_graph *irg   = get_irn_irg(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *addr  = new_r_Address(irg, runtime_entity);
	ir_node  *block = get_nodes_block(node);
	ir_node  *mem   = get_memop_mem(node);
	ir_node  *call  = new_rd_Call(dbgi, block, mem, addr, n_in, in, mtp);
	set_irn_pinned(call, get_irn_pinned(node));
	int throws_exception = ir_throws_exception(node);
	ir_set_throws_exception(call, throws_exception);

	assert(pn_M < 2 && pn_res < 2 && pn_X_regular < 4 && pn_X_except < 4);
	int const         n_proj     = 4;
	int               n_operands = 2;
	ir_node   **const tuple_in   = ALLOCAN(ir_node*, n_proj);
	tuple_in[pn_M] = new_r_Proj(call, mode_M, pn_Call_M);
	ir_node *ress = new_r_Proj(call, mode_T, pn_Call_T_result);
	tuple_in[pn_res] = new_r_Proj(ress, res_mode, 0);
	if (throws_exception) {
		tuple_in[pn_X_regular]  = new_r_Proj(call, mode_X, pn_Call_X_regular);
		tuple_in[pn_X_except]   = new_r_Proj(call, mode_X, pn_Call_X_except);
		n_operands             += 2;
	}

	turn_into_tuple(node, n_operands, tuple_in);
}
