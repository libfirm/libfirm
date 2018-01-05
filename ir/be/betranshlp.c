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
#include "betranshlp.h"

#include "bearch.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "beutil.h"
#include "cgana.h"
#include "debug.h"
#include "execfreq_t.h"
#include "heights.h"
#include "irargs_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irouts.h"
#include "irtools.h"
#include "panic.h"
#include "pdeq.h"
#include "util.h"
#include "vrp.h"

typedef struct be_transform_env_t {
	deq_t worklist;  /**< worklist of nodes that still need to be transformed */
} be_transform_env_t;

static be_transform_env_t env;

#ifndef NDEBUG
static void be_set_orig_node_rec(ir_node *const node, char const *const name)
{
	if (!is_Proj(node)) {
		char const **const orig = &be_get_info(node)->orig_node;
		if (*orig)
			return;
		*orig = name;
	}
	foreach_irn_in(node, i, in) {
		be_set_orig_node_rec(in, name);
	}
}

static void be_set_orig_node(ir_node *const new_node, ir_node const *const old_node)
{
	if (!is_Proj(old_node)) {
		ir_graph       *const irg  = get_irn_irg(old_node);
		struct obstack *const obst = be_get_be_obst(irg);
		lc_eoprintf(firm_get_arg_env(), obst, "%+F", old_node);
		obstack_1grow(obst, 0);
		char const *const name = (char const*)obstack_finish(obst);
		be_set_orig_node_rec(new_node, name);
	}
}
#endif

void be_set_transformed_node(ir_node *old_node, ir_node *new_node)
{
	set_irn_link(old_node, new_node);
	mark_irn_visited(old_node);
#ifndef NDEBUG
	be_set_orig_node(new_node, old_node);
#endif
}

bool be_is_transformed(const ir_node *node)
{
	return irn_visited(node);
}

ir_node *be_transform_phi(ir_node *node, const arch_register_req_t *req)
{
	ir_node  *block = be_transform_nodes_block(node);
	ir_graph *irg   = get_irn_irg(block);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	/* phi nodes allow loops, so we use the old arguments for now
	 * and fix this later */
	ir_node **ins   = get_irn_in(node);
	int       arity = get_irn_arity(node);
	ir_mode  *mode  = req->cls->mode;
	if (mode_is_float(mode) && req->width == 2) {
		mode = get_modeD();
	}
	ir_node  *phi   = new_ir_node(dbgi, irg, block, op_Phi, mode, arity, ins);
	copy_node_attr(irg, node, phi);

	backend_info_t *info = be_get_info(phi);
	info->in_reqs = be_allocate_in_reqs(irg, arity);
	for (int i = 0; i < arity; ++i) {
		info->in_reqs[i] = req;
	}

	arch_set_irn_register_req_out(phi, 0, req);
	be_enqueue_operands(node);

	return phi;
}

void be_set_transform_function(ir_op *op, be_transform_func func)
{
	/* Shouldn't be assigned twice. */
	assert(!op->ops.generic);
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
	ir_node *const block = exact_copy(node);
	block->node_nr = node->node_nr;

	/* put the preds in the worklist */
	be_enqueue_operands(node);

	return block;
}

static ir_node *transform_end(ir_node *node)
{
	/* Do not transform predecessors yet to keep the pre-transform
	 * phase from visiting all the graph. */
	ir_node *const block   = be_transform_nodes_block(node);
	ir_node *const new_end = exact_copy(node);
	set_nodes_block(new_end, block);

	ir_graph *const irg = get_irn_irg(new_end);
	set_irg_end(irg, new_end);

	be_enqueue_operands(node);

	return new_end;
}

static ir_node *transform_proj(ir_node *node)
{
	ir_node *pred    = get_Proj_pred(node);
	ir_op   *pred_op = get_irn_op(pred);
	be_transform_func *proj_transform
		= (be_transform_func*)pred_op->ops.generic1;
	/* we should have a Proj transformer registered */
#ifdef DEBUG_libfirm
	if (!proj_transform) {
		unsigned const node_pn = get_Proj_num(node);
		if (is_Proj(pred)) {
			unsigned const pred_pn   = get_Proj_num(pred);
			ir_node *const pred_pred = get_Proj_pred(pred);
			panic("no transformer for %+F (%u) -> %+F (%u) -> %+F", node, node_pn, pred, pred_pn, pred_pred);
		} else {
			panic("no transformer for %+F (%u) -> %+F", node, node_pn, pred);
		}
	}
#endif
	return proj_transform(node);
}

ir_node *be_duplicate_node(ir_node *const node)
{
	int       const arity = get_irn_arity(node);
	ir_node **const ins   = ALLOCAN(ir_node*, arity);
	foreach_irn_in(node, i, in) {
		ins[i] = be_transform_node(in);
	}

	ir_node *const block    = be_transform_nodes_block(node);
	ir_node *const new_node = new_similar_node(node, block, ins);

	new_node->node_nr = node->node_nr;
	return new_node;
}

ir_node *be_gen_Proj_default(ir_node *const node)
{
  ir_node *const pred     = get_Proj_pred(node);
  ir_node *const new_pred = be_transform_node(pred);
  unsigned const pn       = get_Proj_num(node);
  return be_new_Proj(new_pred, pn);
}

ir_node *be_transform_node(ir_node *node)
{
	ir_node *new_node;
	if (be_is_transformed(node)) {
		new_node = (ir_node*)get_irn_link(node);
	} else {
#ifdef DEBUG_libfirm
		set_irn_link(node, NULL);
#endif
		mark_irn_visited(node);

		ir_op             *const op        = get_irn_op(node);
		be_transform_func *const transform = (be_transform_func*)op->ops.generic;
#ifdef DEBUG_libfirm
		if (!transform)
			panic("no transformer for %+F", node);
#endif

		new_node = transform(node);
		be_set_transformed_node(node, new_node);
	}
	assert(new_node);
	return new_node;
}

ir_node *be_transform_nodes_block(ir_node const *const node)
{
	ir_node *const block = get_nodes_block(node);
	return be_transform_node(block);
}

void be_enqueue_operands(ir_node *node)
{
	/* put the preds in the worklist */
	foreach_irn_in(node, i, pred) {
		deq_push_pointer_right(&env.worklist, pred);
	}
}

/**
 * Rewire nodes which are potential loops (like Phis) to avoid endless loops.
 */
static void fix_loops(ir_node *node)
{
	if (irn_visited_else_mark(node))
		return;

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

	if (changed) {
		identify_remember(node);
	}
}

/**
 * Transforms all nodes. Deletes the old obstack and creates a new one.
 */
static void transform_nodes(ir_graph *irg, arch_pretrans_nodes *pre_transform)
{
	inc_irg_visited(irg);

	deq_init(&env.worklist);

	ir_node *const old_anchor = irg->anchor;
	ir_node *const new_anchor = new_r_Anchor(irg);
	ir_node *const old_end    = get_irg_end(irg);
	irg->anchor = new_anchor;

	/* Pre-transform all anchors (so they are available in the other transform
	 * functions) and put them into the worklist. */
	foreach_irn_in(old_anchor, i, old) {
		ir_node *const nw = be_transform_node(old);
		set_irn_n(new_anchor, i, nw);
	}

	if (pre_transform)
		pre_transform(irg);

	/* process worklist (this should transform all nodes in the graph) */
	while (!deq_empty(&env.worklist)) {
		ir_node *node = deq_pop_pointer_left(ir_node, &env.worklist);
		be_transform_node(node);
	}

	/* Fix loops. */
	inc_irg_visited(irg);
	foreach_irn_in_r(new_anchor, i, n) {
		fix_loops(n);
	}

	deq_free(&env.worklist);
	free_End(old_end);
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
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	transform_nodes(irg, func);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

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
			long const val = get_Const_long(right);
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
	long const val = get_Const_long(node);
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

	be_set_transform_function(op_Block, transform_block);
	be_set_transform_function(op_End,   transform_end);
	be_set_transform_function(op_NoMem, be_duplicate_node);
	be_set_transform_function(op_Pin,   be_duplicate_node);
	be_set_transform_function(op_Proj,  transform_proj);
	be_set_transform_function(op_Sync,  be_duplicate_node);

	be_set_transform_proj_function(op_ASM,    be_gen_Proj_default);
	be_set_transform_proj_function(op_Cond,   be_gen_Proj_default);
	be_set_transform_proj_function(op_Switch, be_gen_Proj_default);

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

static ir_heights_t *heights;

/**
 * Check if a node is somehow data dependent on another one.
 * both nodes must be in the same basic block.
 * @param n1 The first node.
 * @param n2 The second node.
 * @return 1, if n1 is data dependent (transitively) on n2, 0 if not.
 */
static int dependent_on(const ir_node *n1, const ir_node *n2)
{
	assert(get_nodes_block(n1) == get_nodes_block(n2));
	return heights_reachable_in_block(heights, n1, n2);
}

struct be_stack_change_t {
	ir_node  *before;
	unsigned  pos;
	ir_node  *after;
};

/**
 * Classical qsort() comparison function behavior:
 *
 *  0 if both elements are equal, no node depend on the other
 * +1 if first depends on second (first is greater)
 * -1 if second depends on first (second is greater)
 */
static int cmp_stack_dependency(const void *c1, const void *c2)
{
	be_stack_change_t const *const s1 = (be_stack_change_t const*)c1;
	be_stack_change_t const *const s2 = (be_stack_change_t const*)c2;

	/* Sort blockwise. */
	ir_node *const b1  = s1->before;
	ir_node *const b2  = s2->before;
	ir_node *const bl1 = get_nodes_block(b1);
	ir_node *const bl2 = get_nodes_block(b2);
	if (bl1 != bl2)
		return get_irn_idx(bl2) - get_irn_idx(bl1);

	/* If one change chain does not produce a new value, it must be the last. */
	ir_node *const n1 = s1->after;
	if (!n1)
		return 1;
	ir_node *const n2 = s2->after;
	if (!n2)
		return -1;

	/* If one change chain is data dependent on the other, it must come later.
	 * The after nodes cannot be dependent on each other, because they are unused.
	 * So compare after of one with before of the other. */
	if (dependent_on(n1, b2))
		return 1;
	if (dependent_on(n2, b1))
		return -1;

	/* The nodes have no depth order, but we need a total order because qsort()
	 * is not stable.
	 *
	 * Additionally, we need to respect transitive dependencies. Consider a
	 * Call a depending on Call b and an independent Call c.
	 * We MUST NOT order c > a and b > c. */
	unsigned h1 = get_irn_height(heights, b1);
	unsigned h2 = get_irn_height(heights, b2);
	if (h1 < h2)
		return 1;
	if (h1 > h2)
		return -1;
	/* Same height, so use a random (but stable) order */
	return get_irn_idx(n2) - get_irn_idx(n1);
}

void be_stack_init(be_stack_env_t *const env)
{
	env->changes = NEW_ARR_F(be_stack_change_t, 0);
}

void be_stack_record_chain(be_stack_env_t *const env, ir_node *const before, unsigned const pos, ir_node *const after)
{
	assert(!after || get_nodes_block(after) == get_nodes_block(before));

	be_stack_change_t const change = { before, pos, after };
	ARR_APP1(be_stack_change_t, env->changes, change);
	/* FIXME: This should be not be necessary, but not keeping the till now unused
	 * stack nodes triggers problems with out edges, because they get deactivated
	 * before be_stack_finish() is called. It should suffice to keep the last
	 * stack producer per block in be_stack_finish(). */
	if (after)
		keep_alive(after);
}

void be_stack_finish(be_stack_env_t *const env)
{
	be_stack_change_t *const changes = env->changes;
	env->changes = NULL;

	unsigned const n_changes = ARR_LEN(changes);
	if (n_changes != 0) {
		/* Order the stack changes according to their data dependencies. */
		ir_graph *const irg = get_irn_irg(changes[0].before);
		heights = heights_new(irg);
		QSORT(changes, n_changes, cmp_stack_dependency);
		heights_free(heights);

		/* Wire the stack change chains within each block, i.e. connect before of
		 * each change to after of its predecessor. */
		ir_node *prev_block = NULL;
		for (unsigned n = n_changes; n-- != 0;) {
			be_stack_change_t const *const c     = &changes[n];
			ir_node                 *const block = get_nodes_block(c->before);
			if (block == prev_block)
				set_irn_n(c[1].before, c[1].pos, c[0].after);
			prev_block = block;
		}
	}

	DEL_ARR_F(changes);
}

ir_entity **be_collect_parameter_entities(ir_graph *const irg)
{
	ir_entity  *const fun_ent    = get_irg_entity(irg);
	ir_type    *const fun_type   = get_entity_type(fun_ent);
	size_t      const n_params   = get_method_n_params(fun_type);
	ir_entity **const params     = XMALLOCNZ(ir_entity*, n_params);
	ir_type    *const frame_type = get_irg_frame_type(irg);
	for (size_t f = get_compound_n_members(frame_type); f-- > 0;) {
		ir_entity *const member = get_compound_member(frame_type, f);
		if (is_parameter_entity(member)) {
			size_t const num = get_entity_parameter_number(member);
			assert(num < n_params);
			if (params[num])
				panic("multiple entities for parameter %u in %+F found", f, irg);
			params[num] = member;
		}
	}
	return params;
}

void be_add_parameter_entity_stores_list(ir_graph *irg, unsigned n_entities,
                                         ir_entity **entities)
{
	if (n_entities == 0)
		return;

	ir_node *const frame       = get_irg_frame(irg);
	ir_node *const initial_mem = get_irg_initial_mem(irg);
	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const args        = get_irg_args(irg);

	/* all parameter entities left in the frame type require stores.
	 * (The ones passed on the stack have been moved to the arg type) */
	ir_node *first_store = NULL;
	ir_node *mem         = initial_mem;
	for (unsigned i = 0; i < n_entities; ++i) {
		ir_entity *const entity = entities[i];
		ir_type   *const tp     = get_entity_type(entity);
		size_t     const arg    = get_entity_parameter_number(entity);
		ir_node   *const addr   = new_r_Member(start_block, frame, entity);

		if (entity->attr.parameter.is_lowered_doubleword) {
			ir_type *mt          = get_entity_type(get_irg_entity(irg));
			ir_type *param_type0 = get_method_param_type(mt, arg);
			ir_type *param_type1 = get_method_param_type(mt, arg + 1);
			ir_mode *m0          = get_type_mode(param_type0);
			ir_mode *m1          = get_type_mode(param_type1);
			ir_node *val0        = new_r_Proj(args, m0, arg);
			ir_node *val1        = new_r_Proj(args, m1, arg + 1);
			ir_node *store0      = new_r_Store(start_block, mem, addr, val0,
			                                   tp, cons_none);
			ir_node *mem0        = new_r_Proj(store0, mode_M, pn_Store_M);
			size_t   offset      = get_mode_size_bytes(m0);
			ir_mode *mode_ref    = get_irn_mode(addr);
			ir_mode *mode_offs   = get_reference_offset_mode(mode_ref);
			ir_node *cnst        = new_r_Const_long(irg, mode_offs, offset);
			ir_node *next_addr   = new_r_Add(start_block, addr, cnst);
			ir_node *store1      = new_r_Store(start_block, mem0, next_addr, val1,
			                                   tp, cons_none);
			mem = new_r_Proj(store1, mode_M, pn_Store_M);
			if (first_store == NULL)
				first_store = store0;

		} else {
			ir_mode *const mode  = is_compound_type(tp) ? mode_P
			                                            : get_type_mode(tp);
			ir_node *const val   = new_r_Proj(args, mode, arg);
			ir_node *const store = new_r_Store(start_block, mem, addr, val, tp,
			                                   cons_none);
			mem = new_r_Proj(store, mode_M, pn_Store_M);
			if (first_store == NULL)
				first_store = store;
		}
	}

	edges_reroute_except(initial_mem, mem, first_store);
	set_irg_initial_mem(irg, initial_mem);
}

void be_add_parameter_entity_stores(ir_graph *irg)
{
	ir_type    *function_type = get_entity_type(get_irg_entity(irg));
	unsigned    n_parameters  = get_method_n_params(function_type);
	ir_entity **need_stores   = XMALLOCN(ir_entity*, n_parameters);
	unsigned    n_need_stores = 0;
	ir_type    *type          = get_irg_frame_type(irg);


	/* Assume that all parameter entities without an explicit offset set need a
	 * store. */
	for (size_t i = 0, n = get_compound_n_members(type); i < n; ++i) {
		ir_entity *entity = get_compound_member(type, i);
		if (!is_parameter_entity(entity))
			continue;
		if (get_entity_offset(entity) != INVALID_OFFSET)
			continue;

		assert(n_need_stores < n_parameters);
		need_stores[n_need_stores++] = entity;
	}
	be_add_parameter_entity_stores_list(irg, n_need_stores, need_stores);
	free(need_stores);
}

unsigned be_get_n_allocatable_regs(const ir_graph *irg,
                                   const arch_register_class_t *cls)
{
	unsigned *const bs = rbitset_alloca(cls->n_regs);
	be_get_allocatable_regs(irg, cls, bs);
	return rbitset_popcount(bs, cls->n_regs);
}

void be_get_allocatable_regs(ir_graph const *const irg,
                             arch_register_class_t const *const cls,
                             unsigned *const raw_bitset)
{
	be_irg_t *birg             = be_birg_from_irg(irg);
	unsigned *allocatable_regs = birg->allocatable_regs;

	rbitset_clear_all(raw_bitset, cls->n_regs);
	for (unsigned i = 0; i < cls->n_regs; ++i) {
		const arch_register_t *reg = &cls->regs[i];
		if (rbitset_is_set(allocatable_regs, reg->global_index))
			rbitset_set(raw_bitset, i);
	}
}

uint32_t be_get_tv_bits32(ir_tarval *const tv, unsigned const offset)
{
	uint32_t val;
	val  = (uint32_t)get_tarval_sub_bits(tv, offset);
	val |= (uint32_t)get_tarval_sub_bits(tv, offset + 1) <<  8;
	val |= (uint32_t)get_tarval_sub_bits(tv, offset + 2) << 16;
	val |= (uint32_t)get_tarval_sub_bits(tv, offset + 3) << 24;
	return val;
}

ir_node *be_skip_downconv(ir_node *node, bool const single_user)
{
	assert(be_mode_needs_gp_reg(get_irn_mode(node)));
	for (;;) {
		if (single_user && get_irn_n_edges(node) > 1) {
			/* we only want to skip the conv when we're the only user
			 * (because this test is used in the context of address-mode selection
			 *  and we don't want to use address mode for multiple users) */
			break;
		} else if (is_Conv(node)) {
			ir_node *const op       = get_Conv_op(node);
			ir_mode *const src_mode = get_irn_mode(op);
			if (!be_mode_needs_gp_reg(src_mode) || get_mode_size_bits(get_irn_mode(node)) > get_mode_size_bits(src_mode))
				break;
			node = op;
		} else {
			break;
		}
	}
	return node;
}

ir_node *be_skip_sameconv(ir_node *node)
{
	assert(be_mode_needs_gp_reg(get_irn_mode(node)));
	for (;;) {
		if (get_irn_n_edges(node) > 1) {
			/* we only want to skip the conv when we're the only user
			 * (because this test is used in the context of address-mode selection
			 *  and we don't want to use address mode for multiple users) */
			break;
		} else if (is_Conv(node)) {
			ir_node *const op       = get_Conv_op(node);
			ir_mode *const src_mode = get_irn_mode(op);
			if (!be_mode_needs_gp_reg(src_mode) || get_mode_size_bits(get_irn_mode(node)) != get_mode_size_bits(src_mode))
				break;
			node = op;
		} else {
			break;
		}
	}
	return node;
}

bool be_match_immediate(ir_node const *const node, ir_tarval **const tarval_out,
                        ir_entity **const entity_out, unsigned *reloc_kind_out)
{
	unsigned         reloc_kind;
	ir_entity       *entity;
	ir_node   const *cnst;
	if (is_Const(node)) {
		entity     = NULL;
		cnst       = node;
		reloc_kind = 0;
	} else if (is_Address(node)) {
		entity     = get_Address_entity(node);
		cnst       = NULL;
		reloc_kind = 0;
	} else if (be_is_Relocation(node)) {
		entity     = be_get_Relocation_entity(node);
		cnst       = NULL;
		reloc_kind = be_get_Relocation_kind(node);
	} else if (is_Add(node)) {
		ir_node const *l = get_Add_left(node);
		ir_node const *r = get_Add_right(node);
		if (be_is_Relocation(r) || is_Address(r)) {
			ir_node const *tmp = l;
			l = r;
			r = tmp;
		}
		if (!is_Const(r))
			return false;
		cnst = r;
		if (is_Address(l)) {
			entity     = get_Address_entity(l);
			reloc_kind = 0;
		} else if (be_is_Relocation(l)) {
			entity     = be_get_Relocation_entity(l);
			reloc_kind = be_get_Relocation_kind(l);
		} else {
			return false;
		}
	} else {
		return false;
	}

	if (entity && is_tls_entity(entity))
		return false;

	*tarval_out     = cnst ? get_Const_tarval(cnst) : NULL;
	*entity_out     = entity;
	*reloc_kind_out = reloc_kind;
	return true;
}

ir_node *be_make_Sync(ir_node *const block, int const arity, ir_node **const ins)
{
	return
		arity == 1 ? ins[0] :
		new_r_Sync(block, arity, ins);
}

unsigned be_get_out_for_reg(ir_node const *const node, arch_register_t const *const reg)
{
	be_foreach_out(node, o) {
		arch_register_req_t const *const req = arch_get_irn_register_req_out(node, o);
		if (req == reg->single_req)
			return o;
	}
	panic("register requirement not found");
}
