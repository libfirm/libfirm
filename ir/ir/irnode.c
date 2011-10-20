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
 * @brief   Representation of an intermediate operation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <string.h>

#include "pset_new.h"
#include "ident.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irbackedge_t.h"
#include "irdump.h"
#include "irop_t.h"
#include "irprog_t.h"
#include "iredgekinds.h"
#include "iredges_t.h"
#include "ircons.h"
#include "error.h"

#include "irhooks.h"
#include "irtools.h"
#include "util.h"

#include "beinfo.h"

/* some constants fixing the positions of nodes predecessors
   in the in array */
#define CALL_PARAM_OFFSET     (n_Call_max+1)
#define BUILTIN_PARAM_OFFSET  (n_Builtin_max+1)
#define SEL_INDEX_OFFSET      (n_Sel_max+1)
#define RETURN_RESULT_OFFSET  (n_Return_max+1)
#define END_KEEPALIVE_OFFSET  0

static const char *relation_names [] = {
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

const char *get_relation_string(ir_relation relation)
{
	assert(relation < (ir_relation)ARRAY_SIZE(relation_names));
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
	code |= (less ? ir_relation_greater : 0) | (greater ? ir_relation_less : 0);
	return code;
}

/**
 * Indicates, whether additional data can be registered to ir nodes.
 * If set to 1, this is not possible anymore.
 */
static int forbid_new_data = 0;

/**
 * The amount of additional space for custom data to be allocated upon
 * creating a new node.
 */
unsigned firm_add_node_size = 0;


/* register new space for every node */
unsigned firm_register_additional_node_data(unsigned size)
{
	assert(!forbid_new_data && "Too late to register additional node data");

	if (forbid_new_data)
		return 0;

	return firm_add_node_size += size;
}


void init_irnode(void)
{
	/* Forbid the addition of new data to an ir node. */
	forbid_new_data = 1;
}

struct struct_align {
	char c;
	struct s {
		int i;
		float f;
		double d;
	} s;
};

/*
 * irnode constructor.
 * Create a new irnode in irg, with an op, mode, arity and
 * some incoming irnodes.
 * If arity is negative, a node with a dynamic array is created.
 */
ir_node *new_ir_node(dbg_info *db, ir_graph *irg, ir_node *block, ir_op *op,
                     ir_mode *mode, int arity, ir_node *const *in)
{
	ir_node *res;
	unsigned align = offsetof(struct struct_align, s) - 1;
	unsigned add_node_size = (firm_add_node_size + align) & ~align;
	size_t node_size = offsetof(ir_node, attr) + op->attr_size + add_node_size;
	char *p;
	int i;

	assert(irg);
	assert(op);
	assert(mode);
	p = (char*)obstack_alloc(irg->obst, node_size);
	memset(p, 0, node_size);
	res = (ir_node *)(p + add_node_size);

	res->kind     = k_ir_node;
	res->op       = op;
	res->mode     = mode;
	res->visited  = 0;
	res->node_idx = irg_register_node_idx(irg, res);
	res->link     = NULL;
	res->deps     = NULL;

	if (arity < 0) {
		res->in = NEW_ARR_F(ir_node *, 1);  /* 1: space for block */
	} else {
		/* not nice but necessary: End and Sync must always have a flexible array */
		if (op == op_End || op == op_Sync)
			res->in = NEW_ARR_F(ir_node *, (arity+1));
		else
			res->in = NEW_ARR_D(ir_node *, irg->obst, (arity+1));
		memcpy(&res->in[1], in, sizeof(ir_node *) * arity);
	}

	res->in[0]   = block;
	set_irn_dbg_info(res, db);
	res->out     = NULL;
	res->node_nr = get_irp_new_node_nr();

	for (i = 0; i < EDGE_KIND_LAST; ++i) {
		INIT_LIST_HEAD(&res->edge_info[i].outs_head);
		/* edges will be build immediately */
		res->edge_info[i].edges_built = 1;
		res->edge_info[i].out_count = 0;
	}

	/* don't put this into the for loop, arity is -1 for some nodes! */
	edges_notify_edge(res, -1, res->in[0], NULL, irg);
	for (i = 1; i <= arity; ++i)
		edges_notify_edge(res, i - 1, res->in[i], NULL, irg);

	hook_new_node(irg, res);
	if (get_irg_phase_state(irg) == phase_backend) {
		be_info_new_node(res);
	}

	return res;
}

/*-- getting some parameters from ir_nodes --*/

int (is_ir_node)(const void *thing)
{
	return _is_ir_node(thing);
}

int (get_irn_arity)(const ir_node *node)
{
	return _get_irn_arity(node);
}

/* Returns the array with ins. This array is shifted with respect to the
   array accessed by get_irn_n: The block operand is at position 0 not -1.
   (@@@ This should be changed.)
   The order of the predecessors in this array is not guaranteed, except that
   lists of operands as predecessors of Block or arguments of a Call are
   consecutive. */
ir_node **get_irn_in(const ir_node *node)
{
	return node->in;
}

void set_irn_in(ir_node *node, int arity, ir_node **in)
{
	int i;
	ir_node *** pOld_in;
	ir_graph *irg = get_irn_irg(node);

	pOld_in = &node->in;


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
		*pOld_in = NEW_ARR_D(ir_node *, irg->obst, arity + 1);
		(*pOld_in)[0] = block;
	}
	fix_backedges(irg->obst, node);

	memcpy((*pOld_in) + 1, in, sizeof(ir_node *) * arity);

	/* update irg flags */
	clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS | IR_GRAPH_STATE_CONSISTENT_LOOPINFO);
}

ir_node *(get_irn_n)(const ir_node *node, int n)
{
	return _get_irn_n(node, n);
}

void set_irn_n(ir_node *node, int n, ir_node *in)
{
	ir_graph *irg = get_irn_irg(node);
	assert(node && node->kind == k_ir_node);
	assert(-1 <= n);
	assert(n < get_irn_arity(node));
	assert(in && in->kind == k_ir_node);

	/* Call the hook */
	hook_set_irn_n(node, n, in, node->in[n + 1]);

	/* Here, we rely on src and tgt being in the current ir graph */
	edges_notify_edge(node, n, in, node->in[n + 1], irg);

	node->in[n + 1] = in;

	/* update irg flags */
	clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS | IR_GRAPH_STATE_CONSISTENT_LOOPINFO);
}

int add_irn_n(ir_node *node, ir_node *in)
{
	int pos;
	ir_graph *irg = get_irn_irg(node);

	assert(node->op->opar == oparity_dynamic);
	pos = ARR_LEN(node->in) - 1;
	ARR_APP1(ir_node *, node->in, in);
	edges_notify_edge(node, pos, node->in[pos + 1], NULL, irg);

	/* Call the hook */
	hook_set_irn_n(node, pos, node->in[pos + 1], NULL);

	return pos;
}

void del_Sync_n(ir_node *n, int i)
{
	int      arity     = get_Sync_n_preds(n);
	ir_node *last_pred = get_Sync_pred(n, arity - 1);
	set_Sync_pred(n, i, last_pred);
	edges_notify_edge(n, arity - 1, NULL, last_pred, get_irn_irg(n));
	ARR_SHRINKLEN(get_irn_in(n), arity);
}

int (get_irn_deps)(const ir_node *node)
{
	return _get_irn_deps(node);
}

ir_node *(get_irn_dep)(const ir_node *node, int pos)
{
	return _get_irn_dep(node, pos);
}

void (set_irn_dep)(ir_node *node, int pos, ir_node *dep)
{
	_set_irn_dep(node, pos, dep);
}

int add_irn_dep(ir_node *node, ir_node *dep)
{
	int res = 0;

	/* DEP edges are only allowed in backend phase */
	assert(get_irg_phase_state(get_irn_irg(node)) == phase_backend);
	if (node->deps == NULL) {
		node->deps = NEW_ARR_F(ir_node *, 1);
		node->deps[0] = dep;
	} else {
		int i, n;
		int first_zero = -1;

		for (i = 0, n = ARR_LEN(node->deps); i < n; ++i) {
			if (node->deps[i] == NULL)
				first_zero = i;

			if (node->deps[i] == dep)
				return i;
		}

		if (first_zero >= 0) {
			node->deps[first_zero] = dep;
			res = first_zero;
		} else {
			ARR_APP1(ir_node *, node->deps, dep);
			res = n;
		}
	}

	edges_notify_edge_kind(node, res, dep, NULL, EDGE_KIND_DEP, get_irn_irg(node));

	return res;
}

void add_irn_deps(ir_node *tgt, ir_node *src)
{
	int i, n;

	for (i = 0, n = get_irn_deps(src); i < n; ++i)
		add_irn_dep(tgt, get_irn_dep(src, i));
}


ir_mode *(get_irn_mode)(const ir_node *node)
{
	return _get_irn_mode(node);
}

void (set_irn_mode)(ir_node *node, ir_mode *mode)
{
	_set_irn_mode(node, mode);
}

ir_op *(get_irn_op)(const ir_node *node)
{
	return _get_irn_op(node);
}

/* should be private to the library: */
void (set_irn_op)(ir_node *node, ir_op *op)
{
	_set_irn_op(node, op);
}

unsigned (get_irn_opcode)(const ir_node *node)
{
	return _get_irn_opcode(node);
}

const char *get_irn_opname(const ir_node *node)
{
	assert(node);
	if (is_Phi0(node)) return "Phi0";
	return get_id_str(node->op->name);
}

ident *get_irn_opident(const ir_node *node)
{
	assert(node);
	return node->op->name;
}

ir_visited_t (get_irn_visited)(const ir_node *node)
{
	return _get_irn_visited(node);
}

void (set_irn_visited)(ir_node *node, ir_visited_t visited)
{
	_set_irn_visited(node, visited);
}

void (mark_irn_visited)(ir_node *node)
{
	_mark_irn_visited(node);
}

int (irn_visited)(const ir_node *node)
{
	return _irn_visited(node);
}

int (irn_visited_else_mark)(ir_node *node)
{
	return _irn_visited_else_mark(node);
}

void (set_irn_link)(ir_node *node, void *link)
{
	_set_irn_link(node, link);
}

void *(get_irn_link)(const ir_node *node)
{
	return _get_irn_link(node);
}

op_pin_state (get_irn_pinned)(const ir_node *node)
{
	return _get_irn_pinned(node);
}

op_pin_state (is_irn_pinned_in_irg) (const ir_node *node)
{
	return _is_irn_pinned_in_irg(node);
}

void set_irn_pinned(ir_node *node, op_pin_state state)
{
	/* due to optimization an opt may be turned into a Tuple */
	if (is_Tuple(node))
		return;

	assert(node && get_op_pinned(get_irn_op(node)) >= op_pin_state_exc_pinned);
	assert(state == op_pin_state_pinned || state == op_pin_state_floats);

	node->attr.except.pin_state = state;
}

/* Outputs a unique number for this node */
long get_irn_node_nr(const ir_node *node)
{
	assert(node);
	return node->node_nr;
}

void *(get_irn_generic_attr)(ir_node *node)
{
	assert(is_ir_node(node));
	return _get_irn_generic_attr(node);
}

const void *(get_irn_generic_attr_const)(const ir_node *node)
{
	assert(is_ir_node(node));
	return _get_irn_generic_attr_const(node);
}

unsigned (get_irn_idx)(const ir_node *node)
{
	assert(is_ir_node(node));
	return _get_irn_idx(node);
}

int get_irn_pred_pos(ir_node *node, ir_node *arg)
{
	int i;
	for (i = get_irn_arity(node) - 1; i >= 0; i--) {
		if (get_irn_n(node, i) == arg)
			return i;
	}
	return -1;
}

/** manipulate fields of individual nodes **/

ir_node *(get_nodes_block)(const ir_node *node)
{
	return _get_nodes_block(node);
}

void set_nodes_block(ir_node *node, ir_node *block)
{
	assert(node->op != op_Block);
	set_irn_n(node, -1, block);
}

/* Test whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
ir_type *is_frame_pointer(const ir_node *n)
{
	if (is_Proj(n) && (get_Proj_proj(n) == pn_Start_P_frame_base)) {
		ir_node *start = get_Proj_pred(n);
		if (is_Start(start)) {
			return get_irg_frame_type(get_irn_irg(start));
		}
	}
	return NULL;
}

ir_node **get_Block_cfgpred_arr(ir_node *node)
{
	assert(is_Block(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int (get_Block_n_cfgpreds)(const ir_node *node)
{
	return _get_Block_n_cfgpreds(node);
}

ir_node *(get_Block_cfgpred)(const ir_node *node, int pos)
{
	return _get_Block_cfgpred(node, pos);
}

void set_Block_cfgpred(ir_node *node, int pos, ir_node *pred)
{
	assert(is_Block(node));
	set_irn_n(node, pos, pred);
}

int get_Block_cfgpred_pos(const ir_node *block, const ir_node *pred)
{
	int i;

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		if (get_Block_cfgpred_block(block, i) == pred)
			return i;
	}
	return -1;
}

ir_node *(get_Block_cfgpred_block)(const ir_node *node, int pos)
{
	return _get_Block_cfgpred_block(node, pos);
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
	return _get_Block_block_visited(node);
}

void (set_Block_block_visited)(ir_node *node, ir_visited_t visit)
{
	_set_Block_block_visited(node, visit);
}

void (mark_Block_block_visited)(ir_node *node)
{
	_mark_Block_block_visited(node);
}

int (Block_block_visited)(const ir_node *node)
{
	return _Block_block_visited(node);
}

ir_extblk *get_Block_extbb(const ir_node *block)
{
	ir_extblk *res;
	assert(is_Block(block));
	res = block->attr.block.extblk;
	assert(res == NULL || is_ir_extbb(res));
	return res;
}

void set_Block_extbb(ir_node *block, ir_extblk *extblk)
{
	assert(is_Block(block));
	assert(extblk == NULL || is_ir_extbb(extblk));
	block->attr.block.extblk = extblk;
}

/* returns the graph of a Block. */
ir_graph *(get_Block_irg)(const ir_node *block)
{
	return _get_Block_irg(block);
}

ir_entity *create_Block_entity(ir_node *block)
{
	ir_entity *entity;
	assert(is_Block(block));

	entity = block->attr.block.entity;
	if (entity == NULL) {
		ir_label_t  nr;
		ir_type   *glob;

		glob = get_glob_type();
		entity = new_entity(glob, id_unique("block_%u"), get_code_type());
		set_entity_visibility(entity, ir_visibility_local);
		set_entity_linkage(entity, IR_LINKAGE_CONSTANT);
		nr = get_irp_next_label_nr();
		set_entity_label(entity, nr);
		set_entity_compiler_generated(entity, 1);

		block->attr.block.entity = entity;
	}
	return entity;
}

ir_entity *get_Block_entity(const ir_node *block)
{
	assert(is_Block(block));
	return block->attr.block.entity;
}

void set_Block_entity(ir_node *block, ir_entity *entity)
{
	assert(is_Block(block));
	assert(get_entity_type(entity) == get_code_type());
	block->attr.block.entity = entity;
}

int has_Block_entity(const ir_node *block)
{
	return block->attr.block.entity != NULL;
}

ir_node *(get_Block_phis)(const ir_node *block)
{
	return _get_Block_phis(block);
}

void (set_Block_phis)(ir_node *block, ir_node *phi)
{
	_set_Block_phis(block, phi);
}

void (add_Block_phi)(ir_node *block, ir_node *phi)
{
	_add_Block_phi(block, phi);
}

/* Get the Block mark (single bit). */
unsigned (get_Block_mark)(const ir_node *block)
{
	return _get_Block_mark(block);
}

/* Set the Block mark (single bit). */
void (set_Block_mark)(ir_node *block, unsigned mark)
{
	_set_Block_mark(block, mark);
}

int get_End_n_keepalives(const ir_node *end)
{
	assert(is_End(end));
	return (get_irn_arity(end) - END_KEEPALIVE_OFFSET);
}

ir_node *get_End_keepalive(const ir_node *end, int pos)
{
	assert(is_End(end));
	return get_irn_n(end, pos + END_KEEPALIVE_OFFSET);
}

void add_End_keepalive(ir_node *end, ir_node *ka)
{
	assert(is_End(end));
	add_irn_n(end, ka);
}

void set_End_keepalive(ir_node *end, int pos, ir_node *ka)
{
	assert(is_End(end));
	set_irn_n(end, pos + END_KEEPALIVE_OFFSET, ka);
}

/* Set new keep-alives */
void set_End_keepalives(ir_node *end, int n, ir_node *in[])
{
	size_t e;
	int    i;
	ir_graph *irg = get_irn_irg(end);

	/* notify that edges are deleted */
	for (e = END_KEEPALIVE_OFFSET; e < ARR_LEN(end->in) - 1; ++e) {
		edges_notify_edge(end, e, NULL, end->in[e + 1], irg);
	}
	ARR_RESIZE(ir_node *, end->in, n + 1 + END_KEEPALIVE_OFFSET);

	for (i = 0; i < n; ++i) {
		end->in[1 + END_KEEPALIVE_OFFSET + i] = in[i];
		edges_notify_edge(end, END_KEEPALIVE_OFFSET + i, end->in[1 + END_KEEPALIVE_OFFSET + i], NULL, irg);
	}

	/* update irg flags */
	clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS);
}

/* Set new keep-alives from old keep-alives, skipping irn */
void remove_End_keepalive(ir_node *end, ir_node *irn)
{
	int      n = get_End_n_keepalives(end);
	int      i, idx;
	ir_graph *irg;

	idx = -1;
	for (i = n -1; i >= 0; --i) {
		ir_node *old_ka = end->in[1 + END_KEEPALIVE_OFFSET + i];

		/* find irn */
		if (old_ka == irn) {
			idx = i;
			goto found;
		}
	}
	return;
found:
	irg = get_irn_irg(end);

	/* remove the edge */
	edges_notify_edge(end, idx, NULL, irn, irg);

	if (idx != n - 1) {
		/* exchange with the last one */
		ir_node *old = end->in[1 + END_KEEPALIVE_OFFSET + n - 1];
		edges_notify_edge(end, n - 1, NULL, old, irg);
		end->in[1 + END_KEEPALIVE_OFFSET + idx] = old;
		edges_notify_edge(end, idx, old, NULL, irg);
	}
	/* now n - 1 keeps, 1 block input */
	ARR_RESIZE(ir_node *, end->in, (n - 1) + 1 + END_KEEPALIVE_OFFSET);

	/* update irg flags */
	clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS);
}

/* remove Bads, NoMems and doublets from the keep-alive set */
void remove_End_Bads_and_doublets(ir_node *end)
{
	pset_new_t keeps;
	int        idx, n = get_End_n_keepalives(end);
	ir_graph   *irg;
	bool       changed = false;

	if (n <= 0)
		return;

	irg = get_irn_irg(end);
	pset_new_init(&keeps);

	for (idx = n - 1; idx >= 0; --idx) {
		ir_node *ka = get_End_keepalive(end, idx);

		if (is_Bad(ka) || is_NoMem(ka) || pset_new_contains(&keeps, ka)) {
			changed = true;
			/* remove the edge */
			edges_notify_edge(end, idx, NULL, ka, irg);

			if (idx != n - 1) {
				/* exchange with the last one */
				ir_node *old = end->in[1 + END_KEEPALIVE_OFFSET + n - 1];
				edges_notify_edge(end, n - 1, NULL, old, irg);
				end->in[1 + END_KEEPALIVE_OFFSET + idx] = old;
				edges_notify_edge(end, idx, old, NULL, irg);
			}
			--n;
		} else {
			pset_new_insert(&keeps, ka);
		}
	}
	/* n keeps, 1 block input */
	ARR_RESIZE(ir_node *, end->in, n + 1 + END_KEEPALIVE_OFFSET);

	pset_new_destroy(&keeps);

	if (changed) {
		clear_irg_state(irg, IR_GRAPH_STATE_CONSISTENT_OUTS);
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

size_t get_Return_n_ress(const ir_node *node)
{
	assert(is_Return(node));
	return (size_t)(get_irn_arity(node) - RETURN_RESULT_OFFSET);
}

ir_node **get_Return_res_arr(ir_node *node)
{
	assert(is_Return(node));
	if (get_Return_n_ress(node) > 0)
		return (ir_node **)&(get_irn_in(node)[1 + RETURN_RESULT_OFFSET]);
	else
		return NULL;
}

ir_node *get_Return_res(const ir_node *node, int pos)
{
	assert(is_Return(node));
	assert(pos >= 0);
	assert(get_Return_n_ress(node) > (size_t)pos);
	return get_irn_n(node, pos + RETURN_RESULT_OFFSET);
}

void set_Return_res(ir_node *node, int pos, ir_node *res)
{
	assert(is_Return(node));
	set_irn_n(node, pos + RETURN_RESULT_OFFSET, res);
}

int (is_Const_null)(const ir_node *node)
{
	return _is_Const_null(node);
}

int (is_Const_one)(const ir_node *node)
{
	return _is_Const_one(node);
}

int (is_Const_all_one)(const ir_node *node)
{
	return _is_Const_all_one(node);
}



symconst_kind get_SymConst_kind(const ir_node *node)
{
	assert(is_SymConst(node));
	return node->attr.symc.kind;
}

void set_SymConst_kind(ir_node *node, symconst_kind kind)
{
	assert(is_SymConst(node));
	node->attr.symc.kind = kind;
}

ir_type *get_SymConst_type(const ir_node *node)
{
	/* the cast here is annoying, but we have to compensate for
	   the skip_tip() */
	ir_node *irn = (ir_node *)node;
	assert(is_SymConst(node) &&
	       (SYMCONST_HAS_TYPE(get_SymConst_kind(node))));
	return irn->attr.symc.sym.type_p;
}

void set_SymConst_type(ir_node *node, ir_type *tp)
{
	assert(is_SymConst(node) &&
	       (SYMCONST_HAS_TYPE(get_SymConst_kind(node))));
	node->attr.symc.sym.type_p = tp;
}


/* Only to access SymConst of kind symconst_addr_ent.  Else assertion: */
ir_entity *get_SymConst_entity(const ir_node *node)
{
	assert(is_SymConst(node) && SYMCONST_HAS_ENT(get_SymConst_kind(node)));
	return node->attr.symc.sym.entity_p;
}

void set_SymConst_entity(ir_node *node, ir_entity *ent)
{
	assert(is_SymConst(node) && SYMCONST_HAS_ENT(get_SymConst_kind(node)));
	node->attr.symc.sym.entity_p  = ent;
}

ir_enum_const *get_SymConst_enum(const ir_node *node)
{
	assert(is_SymConst(node) && SYMCONST_HAS_ENUM(get_SymConst_kind(node)));
	return node->attr.symc.sym.enum_p;
}

void set_SymConst_enum(ir_node *node, ir_enum_const *ec)
{
	assert(is_SymConst(node) && SYMCONST_HAS_ENUM(get_SymConst_kind(node)));
	node->attr.symc.sym.enum_p  = ec;
}

union symconst_symbol
get_SymConst_symbol(const ir_node *node)
{
	assert(is_SymConst(node));
	return node->attr.symc.sym;
}

void set_SymConst_symbol(ir_node *node, union symconst_symbol sym)
{
	assert(is_SymConst(node));
	node->attr.symc.sym = sym;
}

int get_Sel_n_indexs(const ir_node *node)
{
	assert(is_Sel(node));
	return (get_irn_arity(node) - SEL_INDEX_OFFSET);
}

ir_node **get_Sel_index_arr(ir_node *node)
{
	assert(is_Sel(node));
	if (get_Sel_n_indexs(node) > 0)
		return (ir_node **)& get_irn_in(node)[SEL_INDEX_OFFSET + 1];
	else
		return NULL;
}

ir_node *get_Sel_index(const ir_node *node, int pos)
{
	assert(is_Sel(node));
	return get_irn_n(node, pos + SEL_INDEX_OFFSET);
}

void set_Sel_index(ir_node *node, int pos, ir_node *index)
{
	assert(is_Sel(node));
	set_irn_n(node, pos + SEL_INDEX_OFFSET, index);
}

ir_node **get_Call_param_arr(ir_node *node)
{
	assert(is_Call(node));
	return &get_irn_in(node)[CALL_PARAM_OFFSET + 1];
}

size_t get_Call_n_params(const ir_node *node)
{
	assert(is_Call(node));
	return (size_t) (get_irn_arity(node) - CALL_PARAM_OFFSET);
}

ir_node *get_Call_param(const ir_node *node, int pos)
{
	assert(is_Call(node));
	return get_irn_n(node, pos + CALL_PARAM_OFFSET);
}

void set_Call_param(ir_node *node, int pos, ir_node *param)
{
	assert(is_Call(node));
	set_irn_n(node, pos + CALL_PARAM_OFFSET, param);
}

ir_node **get_Builtin_param_arr(ir_node *node)
{
	assert(is_Builtin(node));
	return &get_irn_in(node)[BUILTIN_PARAM_OFFSET + 1];
}

int get_Builtin_n_params(const ir_node *node)
{
	assert(is_Builtin(node));
	return (get_irn_arity(node) - BUILTIN_PARAM_OFFSET);
}

ir_node *get_Builtin_param(const ir_node *node, int pos)
{
	assert(is_Builtin(node));
	return get_irn_n(node, pos + BUILTIN_PARAM_OFFSET);
}

void set_Builtin_param(ir_node *node, int pos, ir_node *param)
{
	assert(is_Builtin(node));
	set_irn_n(node, pos + BUILTIN_PARAM_OFFSET, param);
}

/* Returns a human readable string for the ir_builtin_kind. */
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
		X(ir_bk_inner_trampoline);
	}
	return "<unknown>";
#undef X
}


int Call_has_callees(const ir_node *node)
{
	assert(is_Call(node));
	return ((get_irg_callee_info_state(get_irn_irg(node)) != irg_callee_info_none) &&
	        (node->attr.call.callee_arr != NULL));
}

size_t get_Call_n_callees(const ir_node *node)
{
  assert(is_Call(node) && node->attr.call.callee_arr);
  return ARR_LEN(node->attr.call.callee_arr);
}

ir_entity *get_Call_callee(const ir_node *node, size_t pos)
{
	assert(pos < get_Call_n_callees(node));
	return node->attr.call.callee_arr[pos];
}

void set_Call_callee_arr(ir_node *node, size_t n, ir_entity ** arr)
{
	ir_graph *irg = get_irn_irg(node);

	assert(is_Call(node));
	if (node->attr.call.callee_arr == NULL || get_Call_n_callees(node) != n) {
		node->attr.call.callee_arr = NEW_ARR_D(ir_entity *, irg->obst, n);
	}
	memcpy(node->attr.call.callee_arr, arr, n * sizeof(ir_entity *));
}

void remove_Call_callee_arr(ir_node *node)
{
	assert(is_Call(node));
	node->attr.call.callee_arr = NULL;
}

/* Checks for upcast.
 *
 * Returns true if the Cast node casts a class type to a super type.
 */
int is_Cast_upcast(ir_node *node)
{
	ir_type *totype   = get_Cast_type(node);
	ir_type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));

	assert(get_irg_typeinfo_state(get_irn_irg(node)) == ir_typeinfo_consistent);
	assert(fromtype);

	while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
		totype   = get_pointer_points_to_type(totype);
		fromtype = get_pointer_points_to_type(fromtype);
	}

	assert(fromtype);

	if (!is_Class_type(totype)) return 0;
	return is_SubClass_of(fromtype, totype);
}

/* Checks for downcast.
 *
 * Returns true if the Cast node casts a class type to a sub type.
 */
int is_Cast_downcast(ir_node *node)
{
	ir_type *totype   = get_Cast_type(node);
	ir_type *fromtype = get_irn_typeinfo_type(get_Cast_op(node));

	assert(get_irg_typeinfo_state(get_irn_irg(node)) == ir_typeinfo_consistent);
	assert(fromtype);

	while (is_Pointer_type(totype) && is_Pointer_type(fromtype)) {
		totype   = get_pointer_points_to_type(totype);
		fromtype = get_pointer_points_to_type(fromtype);
	}

	assert(fromtype);

	if (!is_Class_type(totype)) return 0;
	return is_SubClass_of(totype, fromtype);
}

int (is_unop)(const ir_node *node)
{
	return _is_unop(node);
}

ir_node *get_unop_op(const ir_node *node)
{
	if (node->op->opar == oparity_unary)
		return get_irn_n(node, node->op->op_index);

	assert(node->op->opar == oparity_unary);
	return NULL;
}

void set_unop_op(ir_node *node, ir_node *op)
{
	if (node->op->opar == oparity_unary)
		set_irn_n(node, node->op->op_index, op);

	assert(node->op->opar == oparity_unary);
}

int (is_binop)(const ir_node *node)
{
	return _is_binop(node);
}

ir_node *get_binop_left(const ir_node *node)
{
	assert(node->op->opar == oparity_binary);
	return get_irn_n(node, node->op->op_index);
}

void set_binop_left(ir_node *node, ir_node *left)
{
	assert(node->op->opar == oparity_binary);
	set_irn_n(node, node->op->op_index, left);
}

ir_node *get_binop_right(const ir_node *node)
{
	assert(node->op->opar == oparity_binary);
	return get_irn_n(node, node->op->op_index + 1);
}

void set_binop_right(ir_node *node, ir_node *right)
{
	assert(node->op->opar == oparity_binary);
	set_irn_n(node, node->op->op_index + 1, right);
}

int is_Phi0(const ir_node *n)
{
	assert(n);

	return ((get_irn_op(n) == op_Phi) &&
	        (get_irn_arity(n) == 0) &&
	        (get_irg_phase_state(get_irn_irg(n)) ==  phase_building));
}

ir_node **get_Phi_preds_arr(ir_node *node)
{
  assert(is_Phi(node));
  return (ir_node **)&(get_irn_in(node)[1]);
}

int get_Phi_n_preds(const ir_node *node)
{
	assert(is_Phi(node) || is_Phi0(node));
	return (get_irn_arity(node));
}

ir_node *get_Phi_pred(const ir_node *node, int pos)
{
	assert(is_Phi(node) || is_Phi0(node));
	return get_irn_n(node, pos);
}

void set_Phi_pred(ir_node *node, int pos, ir_node *pred)
{
	assert(is_Phi(node) || is_Phi0(node));
	set_irn_n(node, pos, pred);
}

ir_node *(get_Phi_next)(const ir_node *phi)
{
	return _get_Phi_next(phi);
}

void (set_Phi_next)(ir_node *phi, ir_node *next)
{
	_set_Phi_next(phi, next);
}

int is_memop(const ir_node *node)
{
	unsigned code = get_irn_opcode(node);
	return (code == iro_Load || code == iro_Store);
}

ir_node *get_memop_mem(const ir_node *node)
{
	assert(is_memop(node));
	assert(n_Load_mem == 0 && n_Store_mem == 0);
	return get_irn_n(node, 0);
}

void set_memop_mem(ir_node *node, ir_node *mem)
{
	assert(is_memop(node));
	assert(n_Load_mem == 0 && n_Store_mem == 0);
	set_irn_n(node, 0, mem);
}

ir_node *get_memop_ptr(const ir_node *node)
{
	assert(is_memop(node));
	assert(n_Load_mem == 1 && n_Store_mem == 1);
	return get_irn_n(node, 1);
}

void set_memop_ptr(ir_node *node, ir_node *ptr)
{
	assert(is_memop(node));
	assert(n_Load_mem == 1 && n_Store_mem == 1);
	set_irn_n(node, 1, ptr);
}


ir_node **get_Sync_preds_arr(ir_node *node)
{
	assert(is_Sync(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int get_Sync_n_preds(const ir_node *node)
{
	assert(is_Sync(node));
	return (get_irn_arity(node));
}

/*
void set_Sync_n_preds(ir_node *node, int n_preds)
{
	assert(is_Sync(node));
}
*/

ir_node *get_Sync_pred(const ir_node *node, int pos)
{
	assert(is_Sync(node));
	return get_irn_n(node, pos);
}

void set_Sync_pred(ir_node *node, int pos, ir_node *pred)
{
	assert(is_Sync(node));
	set_irn_n(node, pos, pred);
}

/* Add a new Sync predecessor */
void add_Sync_pred(ir_node *node, ir_node *pred)
{
	assert(is_Sync(node));
	add_irn_n(node, pred);
}

int (is_arg_Proj)(const ir_node *node)
{
	return _is_arg_Proj(node);
}

int is_x_except_Proj(const ir_node *node)
{
	ir_node *pred;
	if (!is_Proj(node))
		return false;
	pred = get_Proj_pred(node);
	if (!is_fragile_op(pred))
		return false;
	return get_Proj_proj(node) == pred->op->pn_x_except;
}

int is_x_regular_Proj(const ir_node *node)
{
	ir_node *pred;
	if (!is_Proj(node))
		return false;
	pred = get_Proj_pred(node);
	if (!is_fragile_op(pred))
		return false;
	return get_Proj_proj(node) == pred->op->pn_x_regular;
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

ir_node **get_Tuple_preds_arr(ir_node *node)
{
	assert(is_Tuple(node));
	return (ir_node **)&(get_irn_in(node)[1]);
}

int get_Tuple_n_preds(const ir_node *node)
{
	assert(is_Tuple(node));
	return get_irn_arity(node);
}

ir_node *get_Tuple_pred(const ir_node *node, int pos)
{
  assert(is_Tuple(node));
  return get_irn_n(node, pos);
}

void set_Tuple_pred(ir_node *node, int pos, ir_node *pred)
{
	assert(is_Tuple(node));
	set_irn_n(node, pos, pred);
}

int get_ASM_n_input_constraints(const ir_node *node)
{
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.input_constraints);
}

int get_ASM_n_output_constraints(const ir_node *node)
{
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.output_constraints);
}

int get_ASM_n_clobbers(const ir_node *node)
{
	assert(is_ASM(node));
	return ARR_LEN(node->attr.assem.clobbers);
}

/* returns the graph of a node */
ir_graph *(get_irn_irg)(const ir_node *node)
{
	return _get_irn_irg(node);
}


/*----------------------------------------------------------------*/
/*  Auxiliary routines                                            */
/*----------------------------------------------------------------*/

ir_node *skip_Proj(ir_node *node)
{
	/* don't assert node !!! */
	if (node == NULL)
		return NULL;

	if (is_Proj(node))
		node = get_Proj_pred(node);

	return node;
}

const ir_node *
skip_Proj_const(const ir_node *node)
{
	/* don't assert node !!! */
	if (node == NULL)
		return NULL;

	if (is_Proj(node))
		node = get_Proj_pred(node);

	return node;
}

ir_node *skip_Tuple(ir_node *node)
{
  ir_node *pred;

restart:
	if (is_Proj(node)) {
	    pred = get_Proj_pred(node);

		if (is_Proj(pred)) { /* nested Tuple ? */
		    pred = skip_Tuple(pred);

			if (is_Tuple(pred)) {
				node = get_Tuple_pred(pred, get_Proj_proj(node));
				goto restart;
			}
		} else if (is_Tuple(pred)) {
			node = get_Tuple_pred(pred, get_Proj_proj(node));
			goto restart;
		}
	}
	return node;
}

/* returns operand of node if node is a Cast */
ir_node *skip_Cast(ir_node *node)
{
	if (is_Cast(node))
		return get_Cast_op(node);
	return node;
}

/* returns operand of node if node is a Cast */
const ir_node *skip_Cast_const(const ir_node *node)
{
	if (is_Cast(node))
		return get_Cast_op(node);
	return node;
}

/* returns operand of node if node is a Pin */
ir_node *skip_Pin(ir_node *node)
{
	if (is_Pin(node))
		return get_Pin_op(node);
	return node;
}

/* returns operand of node if node is a Confirm */
ir_node *skip_Confirm(ir_node *node)
{
	if (is_Confirm(node))
		return get_Confirm_value(node);
	return node;
}

/* skip all high-level ops */
ir_node *skip_HighLevel_ops(ir_node *node)
{
	while (is_op_highlevel(get_irn_op(node))) {
		node = get_irn_n(node, 0);
	}
	return node;
}


/* This should compact Id-cycles to self-cycles. It has the same (or less?) complexity
 * than any other approach, as Id chains are resolved and all point to the real node, or
 * all id's are self loops.
 *
 * Note: This function takes 10% of mostly ANY the compiler run, so it's
 * a little bit "hand optimized".
 */
ir_node *skip_Id(ir_node *node)
{
	ir_node *pred;
	/* don't assert node !!! */

	if (!node || (node->op != op_Id)) return node;

	/* Don't use get_Id_pred():  We get into an endless loop for
	   self-referencing Ids. */
	pred = node->in[0+1];

	if (pred->op != op_Id) return pred;

	if (node != pred) {  /* not a self referencing Id. Resolve Id chain. */
		ir_node *rem_pred, *res;

		if (pred->op != op_Id) return pred; /* shortcut */
		rem_pred = pred;

		assert(get_irn_arity (node) > 0);

		node->in[0+1] = node;   /* turn us into a self referencing Id:  shorten Id cycles. */
		res = skip_Id(rem_pred);
		if (is_Id(res)) /* self-loop */ return node;

		node->in[0+1] = res;    /* Turn Id chain into Ids all referencing the chain end. */
		return res;
	} else {
		return node;
	}
}

int (is_strictConv)(const ir_node *node)
{
	return _is_strictConv(node);
}

/* Returns true if node is a SymConst node with kind symconst_addr_ent. */
int (is_SymConst_addr_ent)(const ir_node *node)
{
	return _is_SymConst_addr_ent(node);
}

/* Returns true if the operation manipulates control flow. */
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

/* Returns true if the operation can change the control flow because
   of an exception. */
int is_fragile_op(const ir_node *node)
{
	return is_op_fragile(get_irn_op(node));
}

/* Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node)
{
	assert(node && is_fragile_op(node));
	return get_irn_n(node, node->op->fragile_mem_index);
}

/* Returns true if the operation is a forking control flow operation. */
int (is_irn_forking)(const ir_node *node)
{
	return _is_irn_forking(node);
}

void (copy_node_attr)(ir_graph *irg, const ir_node *old_node, ir_node *new_node)
{
	_copy_node_attr(irg, old_node, new_node);
}

/* Return the type attribute of a node n (SymConst, Call, Alloc, Free,
   Cast) or NULL.*/
ir_type *(get_irn_type_attr)(ir_node *node)
{
	return _get_irn_type_attr(node);
}

/* Return the entity attribute of a node n (SymConst, Sel) or NULL. */
ir_entity *(get_irn_entity_attr)(ir_node *node)
{
	return _get_irn_entity_attr(node);
}

/* Returns non-zero for constant-like nodes. */
int (is_irn_constlike)(const ir_node *node)
{
	return _is_irn_constlike(node);
}

/*
 * Returns non-zero for nodes that are allowed to have keep-alives and
 * are neither Block nor PhiM.
 */
int (is_irn_keep)(const ir_node *node)
{
	return _is_irn_keep(node);
}

/*
 * Returns non-zero for nodes that are always placed in the start block.
 */
int (is_irn_start_block_placed)(const ir_node *node)
{
	return _is_irn_start_block_placed(node);
}

/* Returns non-zero for nodes that are machine operations. */
int (is_irn_machine_op)(const ir_node *node)
{
	return _is_irn_machine_op(node);
}

/* Returns non-zero for nodes that are machine operands. */
int (is_irn_machine_operand)(const ir_node *node)
{
	return _is_irn_machine_operand(node);
}

/* Returns non-zero for nodes that have the n'th user machine flag set. */
int (is_irn_machine_user)(const ir_node *node, unsigned n)
{
	return _is_irn_machine_user(node, n);
}

/* Returns non-zero for nodes that are CSE neutral to its users. */
int (is_irn_cse_neutral)(const ir_node *node)
{
	return _is_irn_cse_neutral(node);
}

/* Gets the string representation of the jump prediction .*/
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

/** Return the attribute type of a SymConst node if exists */
static ir_type *get_SymConst_attr_type(const ir_node *self)
{
	symconst_kind kind = get_SymConst_kind(self);
	if (SYMCONST_HAS_TYPE(kind))
		return get_SymConst_type(self);
	return NULL;
}

/** Return the attribute entity of a SymConst node if exists */
static ir_entity *get_SymConst_attr_entity(const ir_node *self)
{
	symconst_kind kind = get_SymConst_kind(self);
	if (SYMCONST_HAS_ENT(kind))
		return get_SymConst_entity(self);
	return NULL;
}

/** the get_type_attr operation must be always implemented */
static ir_type *get_Null_type(const ir_node *n)
{
	(void) n;
	return firm_unknown_type;
}

/* Sets the get_type operation for an ir_op_ops. */
ir_op_ops *firm_set_default_get_type_attr(unsigned code, ir_op_ops *ops)
{
	switch (code) {
	case iro_SymConst: ops->get_type_attr = get_SymConst_attr_type; break;
	case iro_Call:     ops->get_type_attr = get_Call_type; break;
	case iro_Alloc:    ops->get_type_attr = get_Alloc_type; break;
	case iro_Free:     ops->get_type_attr = get_Free_type; break;
	case iro_Cast:     ops->get_type_attr = get_Cast_type; break;
	default:
		/* not allowed to be NULL */
		if (! ops->get_type_attr)
			ops->get_type_attr = get_Null_type;
		break;
	}
	return ops;
}

/** the get_entity_attr operation must be always implemented */
static ir_entity *get_Null_ent(const ir_node *n)
{
	(void) n;
	return NULL;
}

/* Sets the get_type operation for an ir_op_ops. */
ir_op_ops *firm_set_default_get_entity_attr(unsigned code, ir_op_ops *ops)
{
	switch (code) {
	case iro_SymConst: ops->get_entity_attr = get_SymConst_attr_entity; break;
	case iro_Sel:      ops->get_entity_attr = get_Sel_entity; break;
	default:
		/* not allowed to be NULL */
		if (! ops->get_entity_attr)
			ops->get_entity_attr = get_Null_ent;
		break;
	}
	return ops;
}

/* Sets the debug information of a node. */
void (set_irn_dbg_info)(ir_node *n, dbg_info *db)
{
	_set_irn_dbg_info(n, db);
}

/**
 * Returns the debug information of an node.
 *
 * @param n   The node.
 */
dbg_info *(get_irn_dbg_info)(const ir_node *n)
{
	return _get_irn_dbg_info(n);
}

/*
 * Calculate a hash value of a node.
 */
unsigned firm_default_hash(const ir_node *node)
{
	unsigned h;
	int i, irn_arity;

	/* hash table value = 9*(9*(9*(9*(9*arity+in[0])+in[1])+ ...)+mode)+code */
	h = irn_arity = get_irn_arity(node);

	/* consider all in nodes... except the block if not a control flow. */
	for (i = is_cfop(node) ? -1 : 0;  i < irn_arity;  ++i) {
		ir_node *pred = get_irn_n(node, i);
		if (is_irn_cse_neutral(pred))
			h *= 9;
		else
			h = 9*h + HASH_PTR(pred);
	}

	/* ...mode,... */
	h = 9*h + HASH_PTR(get_irn_mode(node));
	/* ...and code */
	h = 9*h + HASH_PTR(get_irn_op(node));

	return h;
}  /* firm_default_hash */

/* include generated code */
#include "gen_irnode.c.inl"
