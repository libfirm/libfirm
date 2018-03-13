/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of an intermediate operation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "irnode_t.h"

#include "beinfo.h"
#include "ident.h"
#include "irbackedge_t.h"
#include "ircons.h"
#include "irdump.h"
#include "iredgekinds.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irmode_t.h"
#include "irop_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "irverify.h"
#include "panic.h"
#include "pset_new.h"
#include "util.h"
#include <string.h>

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define END_KEEPALIVE_OFFSET  0

const char *get_relation_string(ir_relation relation)
{
	static char const *const relation_names[] = {
		"false",
		"equal",
		"less",
		"less_equal",
		"greater",
		"greater_equal",
		"less_greater",
		"less_equal_greater",
		"unordered",
		"unordered_equal",
		"unordered_less",
		"unordered_less_equal",
		"unordered_greater",
		"unordered_greater_equal",
		"not_equal",
		"true"
	};

	assert((size_t)relation < ARRAY_SIZE(relation_names));
	return relation_names[relation];
}

ir_relation get_negated_relation(ir_relation relation)
{
	return relation ^ ir_relation_true;
}

ir_relation get_inversed_relation(ir_relation relation)
{
	ir_relation code    = relation & ~(ir_relation_less|ir_relation_greater);
	bool        less    = relation & ir_relation_less;
	bool        greater = relation & ir_relation_greater;
	code |= (less ? ir_relation_greater : ir_relation_false)
	      | (greater ? ir_relation_less : ir_relation_false);
	return code;
}

ir_node *new_ir_node(dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op,
                     ir_mode *mode, int arity, ir_node *const *in)
{
	assert(mode != NULL);

	size_t   const node_size = offsetof(ir_node, attr) + op->attr_size;
	ir_node *const res       = (ir_node*)OALLOCNZ(get_irg_obstack(irg), char, node_size);

	res->kind     = k_ir_node;
	res->op       = op;
	res->mode     = mode;
	res->irg      = irg;
	res->node_idx = irg_register_node_idx(irg, res);

	if (arity < 0) {
		res->in = NEW_ARR_F(ir_node *, 1);  /* 1: space for block */
	} else {
		/* Nodes with dynamic arity must always have a flexible array. */
		if (op->opar == oparity_dynamic)
			res->in = NEW_ARR_F(ir_node *, (arity+1));
		else
			res->in = NEW_ARR_D(ir_node*, get_irg_obstack(irg), arity + 1);
		MEMCPY(&res->in[1], in, arity);
	}

	res->in[0]   = block;
	set_irn_dbg_info(res, db);
	res->node_nr = get_irp_new_node_nr();

	for (ir_edge_kind_t i = EDGE_KIND_FIRST; i <= EDGE_KIND_LAST; ++i) {
		INIT_LIST_HEAD(&res->edge_info[i].outs_head);
		/* Edges will be built immediately. */
		res->edge_info[i].edges_built = 1;
		res->edge_info[i].out_count = 0;
	}

	/* don't put this into the for loop, arity is -1 for some nodes! */
	if (block != NULL)
		edges_notify_edge(res, -1, block, NULL, irg);
	for (int i = 0; i < arity; ++i)
		edges_notify_edge(res, i, res->in[i+1], NULL, irg);

	hook_new_node(res);
	if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_BACKEND))
		be_info_new_node(irg, res);

	return res;
}

ir_node *new_similar_node(ir_node *const old, ir_node *const block, ir_node **const in)
{
	dbg_info *const dbgi  = get_irn_dbg_info(old);
	ir_graph *const irg   = get_irn_irg(old);
	ir_op    *const op    = get_irn_op(old);
	ir_mode  *const mode  = get_irn_mode(old);
	int       const arity = get_irn_arity(old);
	ir_node  *const n     = new_ir_node(dbgi, irg, block, op, mode, arity, in);
	copy_node_attr(irg, old, n);
	return n;
}

int (get_irn_arity)(const ir_node *node)
{
	return get_irn_arity_(node);
}

void set_irn_in(ir_node *const node, int const arity, ir_node *const *const in)
{
	assert(node != NULL && node->kind == k_ir_node);
	assert(arity >= 0);
#ifndef NDEBUG
	for (int i = 0; i < arity; ++i) {
		assert(in[i] != NULL && in[i]->kind == k_ir_node);
	}
#endif

	ir_graph  *irg     = get_irn_irg(node);
	ir_node ***pOld_in = &node->in;
	int        i;
	for (i = 0; i < arity; i++) {
		if (i < (int)ARR_LEN(*pOld_in)-1)
			edges_notify_edge(node, i, in[i], (*pOld_in)[i+1], irg);
		else
			edges_notify_edge(node, i, in[i], NULL,            irg);
	}
	for (;i < (int)ARR_LEN(*pOld_in)-1; i++) {
		edges_notify_edge(node, i, NULL, (*pOld_in)[i+1], irg);
	}

	if (arity != (int)ARR_LEN(*pOld_in) - 1) {
		ir_node * block = (*pOld_in)[0];
		*pOld_in = NEW_ARR_D(ir_node*, get_irg_obstack(irg), arity + 1);
		(*pOld_in)[0] = block;
	}
	fix_backedges(get_irg_obstack(irg), node);

	MEMCPY(*pOld_in + 1, in, arity);

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
}

ir_node *(get_irn_n)(const ir_node *node, int n)
{
	return get_irn_n_(node, n);
}

void set_irn_n(ir_node *node, int n, ir_node *in)
{
	ir_graph *irg = get_irn_irg(node);
	assert(node && node->kind == k_ir_node);
	assert(-1 <= n);
	assert(n < get_irn_arity(node));
	assert(in && in->kind == k_ir_node);
	assert(!is_Deleted(in));

	/* Here, we rely on src and tgt being in the current ir graph */
	edges_notify_edge(node, n, in, node->in[n + 1], irg);

	node->in[n + 1] = in;

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS | IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);
}

int add_irn_n(ir_node *node, ir_node *in)
{
	ir_graph *irg = get_irn_irg(node);

	assert(is_irn_dynamic(node));
	int pos = ARR_LEN(node->in) - 1;
	ARR_APP1(ir_node *, node->in, in);
	edges_notify_edge(node, pos, node->in[pos + 1], NULL, irg);

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	return pos;
}

static void remove_irn_n(ir_node *node, int n)
{
	ir_graph *const irg   = get_irn_irg(node);
	int       const arity = get_irn_arity(node);
	ir_node  *const last  = node->in[arity];
	if (n != arity - 1) {
		/* Replace by last edge. */
		ir_node **const slot = &node->in[n + 1];
		ir_node  *const pred = *slot;
		*slot = last;
		edges_notify_edge(node, n, last, pred, irg);
	}
	/* Remove last edge. */
	edges_notify_edge(node, arity - 1, NULL, last, irg);
	ARR_SHRINKLEN(node->in, arity);

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
}

void remove_Sync_n(ir_node *n, int i)
{
	remove_irn_n(n, i);
}


ir_mode *(get_irn_mode)(const ir_node *node)
{
	return get_irn_mode_(node);
}

void (set_irn_mode)(ir_node *node, ir_mode *mode)
{
	set_irn_mode_(node, mode);
}

ir_op *(get_irn_op)(const ir_node *node)
{
	return get_irn_op_(node);
}

unsigned (get_irn_opcode)(const ir_node *node)
{
	return get_irn_opcode_(node);
}

const char *get_irn_opname(const ir_node *node)
{
	return get_id_str(node->op->name);
}

ident *get_irn_opident(const ir_node *node)
{
	assert(node);
	return node->op->name;
}

ir_visited_t (get_irn_visited)(const ir_node *node)
{
	return get_irn_visited_(node);
}

void (set_irn_visited)(ir_node *node, ir_visited_t visited)
{
	set_irn_visited_(node, visited);
}

void (mark_irn_visited)(ir_node *node)
{
	mark_irn_visited_(node);
}

int (irn_visited)(const ir_node *node)
{
	return irn_visited_(node);
}

int (irn_visited_else_mark)(ir_node *node)
{
	return irn_visited_else_mark_(node);
}

void (set_irn_link)(ir_node *node, void *link)
{
	set_irn_link_(node, link);
}

void *(get_irn_link)(const ir_node *node)
{
	return get_irn_link_(node);
}

int (get_irn_pinned)(const ir_node *node)
{
	return get_irn_pinned_(node);
}

void set_irn_pinned(ir_node *node, int pinned)
{
	assert(get_op_pinned(get_irn_op(node)) >= op_pin_state_exc_pinned);

	node->attr.except.pinned = (pinned != 0);
}

long get_irn_node_nr(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	return node->node_nr;
}

void *(get_irn_generic_attr)(ir_node *node)
{
	assert(node->kind == k_ir_node);
	return get_irn_generic_attr_(node);
}

const void *(get_irn_generic_attr_const)(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	return get_irn_generic_attr_const_(node);
}

unsigned (get_irn_idx)(const ir_node *node)
{
	assert(node->kind == k_ir_node);
	return get_irn_idx_(node);
}

ir_node *(get_nodes_block)(const ir_node *node)
{
	return get_nodes_block_(node);
}

void set_nodes_block(ir_node *node, ir_node *block)
{
	assert(!is_Block(node));
	set_irn_n(node, -1, block);
}

ir_node *(get_Block_cfgpred_block)(const ir_node *node, int pos)
{
	return get_Block_cfgpred_block_(node, pos);
}

int get_Block_matured(const ir_node *node)
{
	assert(is_Block(node));
	return (int)node->attr.block.is_matured;
}

void set_Block_matured(ir_node *node, int matured)
{
	assert(is_Block(node));
	node->attr.block.is_matured = matured;
}

ir_visited_t (get_Block_block_visited)(const ir_node *node)
{
	return get_Block_block_visited_(node);
}

void (set_Block_block_visited)(ir_node *node, ir_visited_t visit)
{
	set_Block_block_visited_(node, visit);
}

void (mark_Block_block_visited)(ir_node *node)
{
	mark_Block_block_visited_(node);
}

int (Block_block_visited)(const ir_node *node)
{
	return Block_block_visited_(node);
}

ir_entity *create_Block_entity(ir_node *block)
{
	ir_entity *entity;
	assert(is_Block(block));

	entity = block->attr.block.entity;
	if (entity == NULL) {
		ir_label_t nr = get_irp_next_label_nr();
		entity = new_label_entity(nr);
		set_entity_linkage(entity, IR_LINKAGE_CONSTANT);

		block->attr.block.entity = entity;
	}
	return entity;
}

ir_node *(get_Block_phis)(const ir_node *block)
{
	return get_Block_phis_(block);
}

void (set_Block_phis)(ir_node *block, ir_node *phi)
{
	set_Block_phis_(block, phi);
}

void (add_Block_phi)(ir_node *block, ir_node *phi)
{
	add_Block_phi_(block, phi);
}

unsigned (get_Block_mark)(const ir_node *block)
{
	return get_Block_mark_(block);
}

void (set_Block_mark)(ir_node *block, unsigned mark)
{
	set_Block_mark_(block, mark);
}

void add_End_keepalive(ir_node *end, ir_node *ka)
{
	assert(is_End(end));
	add_irn_n(end, ka);
}

void set_End_keepalives(ir_node *end, int n, ir_node *in[])
{
	/* notify that edges are deleted */
	ir_graph *irg = get_irn_irg(end);
	for (size_t e = END_KEEPALIVE_OFFSET; e < ARR_LEN(end->in) - 1; ++e) {
		edges_notify_edge(end, e, NULL, end->in[e + 1], irg);
	}
	ARR_RESIZE(ir_node *, end->in, n + 1 + END_KEEPALIVE_OFFSET);

	for (int i = 0; i < n; ++i) {
		end->in[1 + END_KEEPALIVE_OFFSET + i] = in[i];
		edges_notify_edge(end, END_KEEPALIVE_OFFSET + i, end->in[1 + END_KEEPALIVE_OFFSET + i], NULL, irg);
	}

	/* update irg flags */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
}

void remove_End_n(ir_node *n, int idx)
{
	remove_irn_n(n, idx);
}

void remove_End_keepalive(ir_node *end, const ir_node *irn)
{
	assert(END_KEEPALIVE_OFFSET == 0);
	foreach_irn_in_r(end, i, ka) {
		if (ka == irn) {
			remove_irn_n(end, i);
			return;
		}
	}
}

void remove_keep_alive(const ir_node *irn)
{
	ir_graph *irg = get_irn_irg(irn);
	ir_node  *end = get_irg_end(irg);

	for (int i = get_End_n_keepalives(end);;) {
		if (i-- == 0)
			return;

		ir_node *old_ka = end->in[1 + END_KEEPALIVE_OFFSET + i];

		/* find irn */
		if (old_ka == irn)
			set_irn_n(end, END_KEEPALIVE_OFFSET + i, new_r_Bad(irg, get_irn_mode(irn)));
	}
}

void remove_End_Bads_and_doublets(ir_node *end)
{
	int const n = get_End_n_keepalives(end);
	if (n <= 0)
		return;

	pset_new_t keeps;
	pset_new_init(&keeps);

	bool changed = false;
	for (int idx = n; idx-- > 0; ) {
		ir_node *ka = get_End_keepalive(end, idx);

		if (is_Bad(ka) || is_NoMem(ka) || pset_new_contains(&keeps, ka)) {
			changed = true;
			remove_irn_n(end, idx - END_KEEPALIVE_OFFSET);
		} else {
			pset_new_insert(&keeps, ka);
		}
	}
	pset_new_destroy(&keeps);

	if (changed) {
		ir_graph *const irg = get_irn_irg(end);
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	}
}

void free_End(ir_node *end)
{
	assert(is_End(end));
	end->kind = k_BAD;
	DEL_ARR_F(end->in);
	end->in = NULL;   /* @@@ make sure we get an error if we use the
	                     in array afterwards ... */
}

int (is_Const_null)(const ir_node *node)
{
	return is_Const_null_(node);
}

int (is_Const_one)(const ir_node *node)
{
	return is_Const_one_(node);
}

int (is_Const_all_one)(const ir_node *node)
{
	return is_Const_all_one_(node);
}

const char *get_builtin_kind_name(ir_builtin_kind kind)
{
#define X(a)    case a: return #a
	switch (kind) {
		X(ir_bk_trap);
		X(ir_bk_debugbreak);
		X(ir_bk_return_address);
		X(ir_bk_frame_address);
		X(ir_bk_prefetch);
		X(ir_bk_ffs);
		X(ir_bk_clz);
		X(ir_bk_ctz);
		X(ir_bk_popcount);
		X(ir_bk_parity);
		X(ir_bk_bswap);
		X(ir_bk_inport);
		X(ir_bk_outport);
		X(ir_bk_saturating_increment);
		X(ir_bk_compare_swap);
		X(ir_bk_may_alias);
		X(ir_bk_va_start);
		X(ir_bk_va_arg);
	}
	return "<unknown>";
#undef X
}

ir_entity *get_Call_callee(const ir_node *node)
{
	ir_node *ptr = get_Call_ptr(node);
	if (!is_Address(ptr))
		return NULL;
	ir_entity *entity = get_Address_entity(ptr);
	/* some (corner case/pointless) graphs can have non-method entities as
	 * call pointers */
	if (!is_method_entity(entity) && !is_alias_entity(entity))
		return NULL;
	return entity;
}

ir_node *get_binop_left(const ir_node *node)
{
	assert(is_binop(node));
	return get_irn_n(node, node->op->op_index);
}

void set_binop_left(ir_node *node, ir_node *left)
{
	assert(is_binop(node));
	set_irn_n(node, node->op->op_index, left);
}

ir_node *get_binop_right(const ir_node *node)
{
	assert(is_binop(node));
	return get_irn_n(node, node->op->op_index + 1);
}

void set_binop_right(ir_node *node, ir_node *right)
{
	assert(is_binop(node));
	set_irn_n(node, node->op->op_index + 1, right);
}

ir_node *(get_Phi_next)(const ir_node *phi)
{
	return get_Phi_next_(phi);
}

void (set_Phi_next)(ir_node *phi, ir_node *next)
{
	set_Phi_next_(phi, next);
}

int is_memop(const ir_node *node)
{
	return is_op_uses_memory(get_irn_op(node));
}

ir_node *get_memop_mem(const ir_node *node)
{
	const ir_op *op = get_irn_op(node);
	assert(is_memop(node));
	ir_node *const mem = get_irn_n(node, op->memory_index);
	assert(get_irn_mode(mem) == mode_M);
	return mem;
}

void set_memop_mem(ir_node *node, ir_node *mem)
{
	const ir_op *op = get_irn_op(node);
	assert(is_memop(node));
	set_irn_n(node, op->memory_index, mem);
}

void add_Sync_pred(ir_node *node, ir_node *pred)
{
	assert(is_Sync(node));
	add_irn_n(node, pred);
}

int is_x_except_Proj(const ir_node *node)
{
	if (!is_Proj(node))
		return false;
	ir_node *pred = get_Proj_pred(node);
	if (!is_fragile_op(pred))
		return false;
	return get_Proj_num(node) == pred->op->pn_x_except;
}

int is_x_regular_Proj(const ir_node *node)
{
	if (!is_Proj(node))
		return false;
	ir_node *pred = get_Proj_pred(node);
	if (!is_fragile_op(pred))
		return false;
	return get_Proj_num(node) == pred->op->pn_x_regular;
}

void ir_set_throws_exception(ir_node *node, int throws_exception)
{
	except_attr *attr = &node->attr.except;
	assert(is_fragile_op(node));
	attr->throws_exception = throws_exception;
}

int ir_throws_exception(const ir_node *node)
{
	const except_attr *attr = &node->attr.except;
	assert(is_fragile_op(node));
	return attr->throws_exception;
}

size_t get_ASM_n_output_constraints(const ir_node *node)
{
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.output_constraints);
}

size_t get_ASM_n_clobbers(const ir_node *node)
{
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.clobbers);
}

ir_graph *(get_irn_irg)(const ir_node *node)
{
	return get_irn_irg_(node);
}

ir_node *skip_Proj(ir_node *node)
{
	if (is_Proj(node))
		node = get_Proj_pred(node);
	return node;
}

const ir_node *skip_Proj_const(const ir_node *node)
{
	if (is_Proj(node))
		node = get_Proj_pred(node);
	return node;
}

ir_node *skip_Tuple(ir_node *node)
{
restart:
	if (is_Proj(node)) {
		ir_node *pred = get_Proj_pred(node);

		if (is_Proj(pred)) { /* nested Tuple ? */
		    pred = skip_Tuple(pred);

			if (is_Tuple(pred)) {
				node = get_Tuple_pred(pred, get_Proj_num(node));
				goto restart;
			}
		} else if (is_Tuple(pred)) {
			node = get_Tuple_pred(pred, get_Proj_num(node));
			goto restart;
		}
	}
	return node;
}

ir_node *skip_Pin(ir_node *node)
{
	if (is_Pin(node))
		return get_Pin_op(node);
	return node;
}

ir_node *skip_Confirm(ir_node *node)
{
	if (is_Confirm(node))
		return get_Confirm_value(node);
	return node;
}

ir_node *skip_Id(ir_node *node)
{
	/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
	 * than any other approach, as Id chains are resolved and all point to the real node, or
	 * all id's are self loops. */
	if (node->op != op_Id)
		return node;

	/* Don't use get_Id_pred():  We get into an endless loop for
	   self-referencing Ids. */
	ir_node *pred = node->in[0+1];
	if (pred->op != op_Id)
		return pred;

	if (node != pred) {  /* not a self referencing Id. Resolve Id chain. */
		if (pred->op != op_Id)
			return pred; /* shortcut */
		ir_node *rem_pred = pred;

		assert(get_irn_arity (node) > 0);

		node->in[0+1] = node;   /* turn us into a self referencing Id:  shorten Id cycles. */
		ir_node *res = skip_Id(rem_pred);
		/* self-loop */
		if (is_Id(res))
			return node;

		node->in[0+1] = res;    /* Turn Id chain into Ids all referencing the chain end. */
		return res;
	} else {
		return node;
	}
}

int is_cfop(const ir_node *node)
{
	if (is_fragile_op(node) && ir_throws_exception(node))
		return true;

	return is_op_cfopcode(get_irn_op(node));
}

int is_unknown_jump(const ir_node *node)
{
	return is_op_unknown_jump(get_irn_op(node));
}

int is_fragile_op(const ir_node *node)
{
	return is_op_fragile(get_irn_op(node));
}

int (is_irn_forking)(const ir_node *node)
{
	return is_irn_forking_(node);
}

static bool is_call_no_write(const ir_node *node)
{
	ir_type *call_tp = get_Call_type(node);
	unsigned prop    = get_method_additional_properties(call_tp);
	if (prop & mtp_property_no_write)
		return true;
	ir_entity *entity = get_Call_callee(node);
	if (entity == NULL)
		return false;
	return get_entity_additional_properties(entity) & mtp_property_no_write;
}

int is_irn_const_memory(const ir_node *node)
{
	const ir_op *op = get_irn_op(node);
	if (get_op_flags(op) & irop_flag_const_memory)
		return true;
	if (is_Builtin(node)) {
		switch (get_Builtin_kind(node)) {
		case ir_bk_trap:
		case ir_bk_debugbreak:
		case ir_bk_compare_swap:
		case ir_bk_va_start:
		case ir_bk_va_arg:
			return false;
		case ir_bk_return_address:
		case ir_bk_frame_address:
		case ir_bk_prefetch:
		case ir_bk_ffs:
		case ir_bk_clz:
		case ir_bk_ctz:
		case ir_bk_popcount:
		case ir_bk_parity:
		case ir_bk_bswap:
		case ir_bk_inport:
		case ir_bk_outport:
		case ir_bk_saturating_increment:
		case ir_bk_may_alias:
			return true;
		}
		panic("invalid Builtin %+F", node);
	}
	if (is_Call(node))
		return is_call_no_write(node);
	return false;
}

void (copy_node_attr)(ir_graph *irg, const ir_node *old_node, ir_node *new_node)
{
	copy_node_attr_(irg, old_node, new_node);
}

ir_type *(get_irn_type_attr)(ir_node *node)
{
	return get_irn_type_attr_(node);
}

ir_entity *(get_irn_entity_attr)(ir_node *node)
{
	return get_irn_entity_attr_(node);
}

int (is_irn_constlike)(const ir_node *node)
{
	return is_irn_constlike_(node);
}

int (is_irn_keep)(const ir_node *node)
{
	return is_irn_keep_(node);
}

int (is_irn_start_block_placed)(const ir_node *node)
{
	return is_irn_start_block_placed_(node);
}

const char *get_cond_jmp_predicate_name(cond_jmp_predicate pred)
{
#define X(a)    case a: return #a
	switch (pred) {
		X(COND_JMP_PRED_NONE);
		X(COND_JMP_PRED_TRUE);
		X(COND_JMP_PRED_FALSE);
	}
	return "<unknown>";
#undef X
}

void ir_register_getter_ops(void)
{
	set_op_get_type_attr(op_Align,     get_Align_type);
	set_op_get_type_attr(op_Builtin,   get_Builtin_type);
	set_op_get_type_attr(op_Call,      get_Call_type);
	set_op_get_type_attr(op_CopyB,     get_CopyB_type);
	set_op_get_type_attr(op_Sel,       get_Sel_type);
	set_op_get_type_attr(op_Size,      get_Size_type);

	set_op_get_entity_attr(op_Address, get_Address_entity);
	set_op_get_entity_attr(op_Block,   get_Block_entity);
	set_op_get_entity_attr(op_Member,  get_Member_entity);
	set_op_get_entity_attr(op_Offset,  get_Offset_entity);
}

void (set_irn_dbg_info)(ir_node *n, dbg_info *db)
{
	set_irn_dbg_info_(n, db);
}

dbg_info *(get_irn_dbg_info)(const ir_node *n)
{
	return get_irn_dbg_info_(n);
}

/**
 * A gdb helper function to print firm objects.
 */
const char *gdb_node_helper(const void *firm_object)
{
	static char buf[1024];
	ir_snprintf(buf, sizeof(buf), "%+F", firm_object);
	return buf;
}

ir_switch_table *ir_new_switch_table(ir_graph *irg, size_t n_entries)
{
	struct obstack *obst = get_irg_obstack(irg);
	ir_switch_table *res = OALLOCFZ(obst, ir_switch_table, entries, n_entries);
	res->n_entries = n_entries;
	return res;
}

void ir_switch_table_set(ir_switch_table *table, size_t n,
                         ir_tarval *min, ir_tarval *max, unsigned pn)
{
	ir_switch_table_entry *entry = ir_switch_table_get_entry(table, n);
	entry->min = min;
	entry->max = max;
	entry->pn  = pn;
}

size_t (ir_switch_table_get_n_entries)(const ir_switch_table *table)
{
	return ir_switch_table_get_n_entries_(table);
}

ir_tarval *ir_switch_table_get_max(const ir_switch_table *table, size_t e)
{
	return ir_switch_table_get_entry_const(table, e)->max;
}

ir_tarval *ir_switch_table_get_min(const ir_switch_table *table, size_t e)
{
	return ir_switch_table_get_entry_const(table, e)->min;
}

unsigned ir_switch_table_get_pn(const ir_switch_table *table, size_t e)
{
	return ir_switch_table_get_entry_const(table, e)->pn;
}

ir_switch_table *ir_switch_table_duplicate(ir_graph *irg,
                                           const ir_switch_table *table)
{
	size_t n_entries = ir_switch_table_get_n_entries(table);
	ir_switch_table *res = ir_new_switch_table(irg, n_entries);
	for (size_t e = 0; e < n_entries; ++e) {
		const ir_switch_table_entry *entry
			= ir_switch_table_get_entry_const(table, e);
		ir_switch_table_entry *new_entry = ir_switch_table_get_entry(res, e);
		*new_entry = *entry;
	}
	return res;
}

ir_node *get_Proj_for_pn(ir_node const *const irn, unsigned const pn)
{
	assert(get_irn_mode(irn) == mode_T && "need mode_T");
	foreach_out_edge(irn, edge) {
		ir_node *const proj = get_edge_src_irn(edge);
		if (is_Proj(proj) && get_Proj_num(proj) == pn)
			return proj;
	}
	return NULL;
}
