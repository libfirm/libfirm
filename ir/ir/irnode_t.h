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
 * @brief   Representation of an intermediate operation -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_IR_IRNODE_T_H
#define FIRM_IR_IRNODE_T_H

#include "irtypes.h"
#include "irnode.h"
#include "irop_t.h"
#include "irgraph_t.h"
#include "irflag_t.h"
#include "firm_common_t.h"
#include "array.h"

/**
 * Returns the array with the ins.  The content of the array may not be
 * changed.
 * Note that this function returns the whole in array including the
 * block predecessor. So, it is NOT symmetric with set_irn_in().
 */
ir_node     **get_irn_in            (const ir_node *node);

/** @{ */
/** access attributes directly */
const_attr    *get_irn_const_attr    (ir_node *node);
long          get_irn_proj_attr      (ir_node *node);
alloc_attr    *get_irn_alloc_attr    (ir_node *node);
free_attr     *get_irn_free_attr     (ir_node *node);
symconst_attr *get_irn_symconst_attr (ir_node *node);
ir_type       *get_irn_call_attr     (ir_node *node);
ir_type       *get_irn_funccall_attr (ir_node *node);
sel_attr      *get_irn_sel_attr      (ir_node *node);
phi_attr      *get_irn_phi_attr      (ir_node *node);
block_attr    *get_irn_block_attr    (ir_node *node);
load_attr     *get_irn_load_attr     (ir_node *node);
store_attr    *get_irn_store_attr    (ir_node *node);
except_attr   *get_irn_except_attr   (ir_node *node);
divmod_attr   *get_irn_divmod_attr   (ir_node *node);
builtin_attr  *get_irn_builtin_attr  (ir_node *node);
/** @} */

/**
 * The amount of additional space for custom data to be allocated upon creating a new node.
 */
extern unsigned firm_add_node_size;

/**
 * Sets the get_type operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_type(ir_opcode code, ir_op_ops *ops);

/**
 * Sets the get_type_attr operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_type_attr(ir_opcode code, ir_op_ops *ops);

/**
 * Sets the get_entity_attr operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_get_entity_attr(ir_opcode code, ir_op_ops *ops);

/*-------------------------------------------------------------------*/
/*  These function are most used in libfirm.  Give them as static    */
/*  functions so they can be inlined.                                */
/*-------------------------------------------------------------------*/

/**
 * Checks whether a pointer points to a ir node.
 * Intern version for libFirm.
 */
static inline int
_is_ir_node(const void *thing) {
	return (get_kind(thing) == k_ir_node);
}

/**
 * Gets the op of a node.
 * Intern version for libFirm.
 */
static inline ir_op *
_get_irn_op(const ir_node *node) {
	assert(node);
	return node->op;
}

static inline void
_set_irn_op(ir_node *node, ir_op *op) {
	assert(node);
	node->op = op;
}

/** Copies all attributes stored in the old node  to the new node.
    Assumes both have the same opcode and sufficient size. */
static inline void
copy_node_attr(const ir_node *old_node, ir_node *new_node) {
	ir_op *op = _get_irn_op(old_node);

	/* must always exist */
	op->ops.copy_attr(old_node, new_node);
}

/**
 * Gets the opcode of a node.
 * Intern version for libFirm.
 */
static inline unsigned
_get_irn_opcode(const ir_node *node) {
	assert(k_ir_node == get_kind(node));
	assert(node->op);
	return node->op->code;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static inline int
_get_irn_intra_arity(const ir_node *node) {
	assert(node);
	return ARR_LEN(node->in) - 1;
}

/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
static inline int
_get_irn_inter_arity(const ir_node *node) {
	assert(node);
	if (_get_irn_op(node) == op_Filter) {
		assert(node->attr.filter.in_cg);
		return ARR_LEN(node->attr.filter.in_cg) - 1;
	} else if (_get_irn_op(node) == op_Block && node->attr.block.in_cg) {
		return ARR_LEN(node->attr.block.in_cg) - 1;
	}
	return _get_irn_intra_arity(node);
}

#ifdef INTERPROCEDURAL_VIEW
/**
 * Returns the number of predecessors without the block predecessor.
 * Intern version for libFirm.
 */
extern int (*_get_irn_arity)(const ir_node *node);

#else

#define _get_irn_arity(n) _get_irn_intra_arity(n)
#endif

/**
 * Intern version for libFirm.
 */
static inline ir_node *
_get_irn_intra_n(const ir_node *node, int n) {
	ir_node *nn;

	assert(node);
	assert(-1 <= n && n < _get_irn_intra_arity(node));

	nn = node->in[n + 1];
	if (nn == NULL) {
		/* only block and Anchor inputs are allowed to be NULL */
		assert((node->op == op_Anchor || n == -1) && "NULL input of a node");
		return NULL;
	}
	if (nn->op != op_Id) return nn;

	return (node->in[n + 1] = skip_Id(nn));
}

/**
 * Intern version for libFirm.
 */
static inline ir_node*
_get_irn_inter_n(const ir_node *node, int n) {
	assert(node); assert(-1 <= n && n < _get_irn_inter_arity(node));

	/* handle Filter and Block specially */
	if (_get_irn_op(node) == op_Filter) {
		assert(node->attr.filter.in_cg);
		return (node->attr.filter.in_cg[n + 1] = skip_Id(node->attr.filter.in_cg[n + 1]));
	} else if (_get_irn_op(node) == op_Block && node->attr.block.in_cg) {
		return (node->attr.block.in_cg[n + 1] = skip_Id(node->attr.block.in_cg[n + 1]));
	}

	return _get_irn_intra_n(node, n);
}

/**
 * returns a hash value for a node
 */
static inline unsigned hash_irn(const ir_node *node)
{
	return (unsigned) get_irn_idx(node);
}

/**
 * Access to the predecessors of a node.
 * To iterate over the operands iterate from 0 to i < get_irn_arity(),
 * to iterate including the Block predecessor iterate from i = -1 to
 * i < get_irn_arity.
 * If it is a block, the entry -1 is NULL.
 * Intern version for libFirm.
 */
#ifdef INTERPROCEDURAL_VIEW
extern ir_node *(*_get_irn_n)(const ir_node *node, int n);
#else
#define _get_irn_n(n,i) _get_irn_intra_n(n,i)
#endif

static inline int _get_irn_deps(const ir_node *node) {
	return node->deps ? ARR_LEN(node->deps) : 0;
}

static inline ir_node *_get_irn_dep(const ir_node *node, int pos) {
	assert(node->deps && "dependency array node yet allocated. use add_irn_dep()");
	assert(pos >= 0 && pos < ARR_LEN(node->deps) && "dependency index out of range");
	return node->deps[pos];
}

static inline void
_set_irn_dep(ir_node *node, int pos, ir_node *dep) {
	ir_node *old;

	assert(node->deps && "dependency array node yet allocated. use add_irn_dep()");
	assert(pos >= 0 && pos < ARR_LEN(node->deps) && "dependency index out of range");
	old = node->deps[pos];
	node->deps[pos] = dep;
	edges_notify_edge_kind(node, pos, dep, old, EDGE_KIND_DEP, get_irn_irg(node));
}


static inline int
_get_irn_ins_or_deps(const ir_node *irn) {
	return _get_irn_deps(irn) + _get_irn_arity(irn);
}

static inline ir_node *
_get_irn_in_or_dep(const ir_node *irn, int pos) {
	int n_in = get_irn_arity(irn);
	return pos < n_in ? get_irn_n(irn, pos) : get_irn_dep(irn, pos - n_in);
}

/**
 * Gets the mode of a node.
 * Intern version for libFirm.
 */
static inline ir_mode *
_get_irn_mode(const ir_node *node) {
	assert(node);
	return node->mode;
}

/**
 * Sets the mode of a node.
 * Intern version of libFirm.
 */
static inline void
_set_irn_mode(ir_node *node, ir_mode *mode) {
	assert(node);
	node->mode = mode;
}

/**
 * Gets the visited counter of a node.
 * Intern version for libFirm.
 */
static inline ir_visited_t
_get_irn_visited(const ir_node *node) {
	assert(node);
	return node->visited;
}

/**
 * Sets the visited counter of a node.
 * Intern version for libFirm.
 */
static inline void
_set_irn_visited(ir_node *node, ir_visited_t visited) {
	assert(node);
	node->visited = visited;
}

/**
 * Mark a node as visited in a graph.
 * Intern version for libFirm.
 */
static inline void
_mark_irn_visited(ir_node *node) {
	assert(node);
	node->visited = current_ir_graph->visited;
}

/**
 * Returns non-zero if a node of was visited.
 * Intern version for libFirm.
 */
static inline int
_irn_visited(const ir_node *node) {
	assert(node);
	return (node->visited >= current_ir_graph->visited);
}

static inline int
_irn_visited_else_mark(ir_node *node) {
	if (_irn_visited(node))
		return 1;
	_mark_irn_visited(node);
	return 0;
}

/**
 * Sets the link of a node.
 * Intern version of libFirm.
 */
static inline void
_set_irn_link(ir_node *node, void *link) {
	assert(node);
	node->link = link;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static inline void *
_get_irn_link(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return node->link;
}

/**
 * Returns whether the node _always_ must be pinned.
 * I.e., the node is not floating after global cse.
 *
 * Intern version of libFirm.
 */
static inline op_pin_state
_get_irn_pinned(const ir_node *node) {
	op_pin_state state;
	assert(node && _is_ir_node(node));
	/* Check opcode */
	state = _get_op_pinned(_get_irn_op(node));

	if (state >= op_pin_state_exc_pinned)
		return get_opt_fragile_ops() ? node->attr.except.pin_state : op_pin_state_pinned;
	return state;
}

static inline op_pin_state
_is_irn_pinned_in_irg(const ir_node *node) {
	if (get_irg_pinned(get_irn_irg(node)) == op_pin_state_floats)
		return get_irn_pinned(node);
	return op_pin_state_pinned;
}

static inline int
_is_unop(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (node->op->opar == oparity_unary);
}

static inline int
_is_binop(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (node->op->opar == oparity_binary);
}

static inline int
_is_Phi(const ir_node *node) {
	ir_op *op;
	assert(node);

	op = get_irn_op(node);
#ifdef INTERPROCEDURAL_VIEW
	if (op == op_Filter) return get_interprocedural_view();
#endif
	return (op == op_Phi);
}

static inline int
_is_Proj(const ir_node *node) {
	ir_op *op;
	assert(node);

	op = _get_irn_op(node);
#ifdef INTERPROCEDURAL_VIEW
	if (op == op_Filter) return !get_interprocedural_view();
#endif
	return (op == op_Proj);
}

static inline int
_is_Filter(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Filter);
}

static inline int
_is_Bad(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Bad);
}

static inline int
_is_NoMem(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_NoMem);
}

static inline int
_is_Minus(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Minus);
}

static inline int
_is_Abs(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Abs);
}

static inline int
_is_Mod(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mod);
}

static inline int
_is_Div(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Div);
}

static inline int
_is_DivMod(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_DivMod);
}

static inline int
_is_Quot(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Quot);
}

static inline int
_is_Add(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Add);
}

static inline int
_is_Carry(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Carry);
}

static inline int
_is_And(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_And);
}

static inline int
_is_Or(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Or);
}

static inline int
_is_Eor(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Eor);
}

static inline int
_is_Sub(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sub);
}

static inline int
_is_Not(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Not);
}

static inline int
_is_Shl(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Shl);
}

static inline int
_is_Shr(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Shr);
}

static inline int
_is_Shrs(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Shrs);
}

static inline int
_is_Rotl(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Rotl);
}

static inline int
_is_Id(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Id);
}

static inline int
_is_Tuple(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Tuple);
}

static inline int
_is_Bound(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Bound);
}

static inline int
_is_Start(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Start);
}

static inline int
_is_End(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_End);
}

static inline int
_is_Const(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Const);
}

static inline int
_is_Conv(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Conv);
}

static inline int
_is_strictConv(const ir_node *node) {
	return _is_Conv(node) && get_Conv_strict(node);
}

static inline int
_is_Cast(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Cast);
}

static inline int
_is_CopyB(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_CopyB);
}

static inline int
_is_Unknown(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Unknown);
}

static inline int
_is_Return(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Return);
}

static inline int
_is_Call(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Call);
}

static inline int
_is_Builtin(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Builtin);
}

static inline int
_is_CallBegin(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_CallBegin);
}

static inline int
_is_Sel(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sel);
}

static inline int
_is_Mul(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mul);
}

static inline int
_is_Mulh(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mulh);
}

static inline int
_is_Mux(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Mux);
}

static inline int
_is_Load(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Load);
}

static inline int
_is_Store(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Store);
}

static inline int
_is_Sync(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Sync);
}

static inline int
_is_Confirm(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Confirm);
}

static inline int
_is_Pin(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Pin);
}

static inline int
_is_SymConst(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_SymConst);
}

static inline int
_is_SymConst_addr_ent(const ir_node *node) {
	return is_SymConst(node) && get_SymConst_kind(node) == symconst_addr_ent;
}

static inline int
_is_Cond(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Cond);
}

static inline int
_is_Cmp(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Cmp);
}

static inline int
_is_Alloc(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Alloc);
}

static inline int
_is_Free(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Free);
}

static inline int
_is_Jmp(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Jmp);
}

static inline int
_is_IJmp(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_IJmp);
}

static inline int
_is_Raise(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Raise);
}

static inline int
_is_ASM(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_ASM);
}

static inline int
_is_Dummy(const ir_node *node) {
	assert(node);
	return (_get_irn_op(node) == op_Dummy);
}

static inline int
_is_Anchor(const ir_node *node) {
	return (_get_irn_op(node) == op_Anchor);
}

static inline int
_is_no_Block(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (_get_irn_op(node) != op_Block);
}

static inline int
_is_Block(const ir_node *node) {
	assert(node && _is_ir_node(node));
	return (_get_irn_op(node) == op_Block);
}

static inline int
_get_Block_n_cfgpreds(const ir_node *node) {
	assert(_is_Block(node));
	return _get_irn_arity(node);
}

static inline ir_node *
_get_Block_cfgpred(const ir_node *node, int pos) {
	assert(0 <= pos && pos < get_irn_arity(node));
	assert(_is_Block(node));
	return _get_irn_n(node, pos);
}

/* Get the predecessor block.
 *
 *  Returns the block corresponding to the predecessor pos.
 *
 *  There are several ambiguities we resolve with this function:
 *  - The direct predecessor can be a Proj, which is not pinned.
 *    We walk from the predecessor to the next pinned node
 *    (skip_Proj) and return the block that node is in.
 *  - If we encounter the Bad node, this function does not return
 *    the Start block, but the Bad node.
 */
static inline ir_node  *
_get_Block_cfgpred_block(const ir_node *node, int pos) {
	ir_node *res = skip_Proj(get_Block_cfgpred(node, pos));
	if (!is_Bad(res))
		res = get_nodes_block(res);
	return res;
}

static inline ir_visited_t
_get_Block_block_visited(const ir_node *node) {
	assert(node->op == op_Block);
	return node->attr.block.block_visited;
}

static inline void
_set_Block_block_visited(ir_node *node, ir_visited_t visit) {
	assert(node->op == op_Block);
	node->attr.block.block_visited = visit;
}

/* For this current_ir_graph must be set. */
static inline void
_mark_Block_block_visited(ir_node *node) {
	assert(node->op == op_Block);
	node->attr.block.block_visited = get_irg_block_visited(current_ir_graph);
}

static inline int
_Block_block_visited(const ir_node *node) {
	assert(node->op == op_Block);
	return (node->attr.block.block_visited >= get_irg_block_visited(current_ir_graph));
}

static inline ir_node *
_set_Block_dead(ir_node *block) {
	assert(_get_irn_op(block) == op_Block);
	block->attr.block.dom.dom_depth = -1;
	block->attr.block.is_dead = 1;
	return block;
}

static inline int
_is_Block_dead(const ir_node *block) {
	ir_op *op = _get_irn_op(block);

	if (op == op_Bad)
		return 1;
	else {
		assert(op == op_Block);
		return block->attr.block.is_dead;
	}
}

static inline tarval *_get_Const_tarval(const ir_node *node) {
	assert(_get_irn_op(node) == op_Const);
	return node->attr.con.tv;
}

static inline int _is_Const_null(const ir_node *node) {
	return tarval_is_null(_get_Const_tarval(node));
}

static inline int _is_Const_one(const ir_node *node) {
	return tarval_is_one(_get_Const_tarval(node));
}

static inline int _is_Const_all_one(const ir_node *node) {
	return tarval_is_all_one(_get_Const_tarval(node));
}

static inline int _is_irn_forking(const ir_node *node) {
	return is_op_forking(_get_irn_op(node));
}

static inline ir_type *_get_irn_type(ir_node *node) {
	return _get_irn_op(node)->ops.get_type(node);
}

static inline ir_type *_get_irn_type_attr(ir_node *node) {
	return _get_irn_op(node)->ops.get_type_attr(node);
}

static inline ir_entity *_get_irn_entity_attr(ir_node *node) {
  return _get_irn_op(node)->ops.get_entity_attr(node);
}

static inline int _is_irn_constlike(const ir_node *node) {
	return is_op_constlike(_get_irn_op(node));
}

static inline int _is_irn_always_opt(const ir_node *node) {
	return is_op_always_opt(_get_irn_op(node));
}

static inline int _is_irn_keep(const ir_node *node) {
	return is_op_keep(_get_irn_op(node));
}

static inline int _is_irn_start_block_placed(const ir_node *node) {
	return is_op_start_block_placed(_get_irn_op(node));
}

static inline int _is_irn_machine_op(const ir_node *node) {
	return is_op_machine(_get_irn_op(node));
}

static inline int _is_irn_machine_operand(const ir_node *node) {
	return is_op_machine_operand(_get_irn_op(node));
}

static inline int _is_irn_machine_user(const ir_node *node, unsigned n) {
	return is_op_machine_user(_get_irn_op(node), n);
}

static inline cond_jmp_predicate _get_Cond_jmp_pred(const ir_node *node) {
	assert(_get_irn_op(node) == op_Cond);
	return node->attr.cond.jmp_pred;
}

static inline void _set_Cond_jmp_pred(ir_node *node, cond_jmp_predicate pred) {
	assert(_get_irn_op(node) == op_Cond);
	node->attr.cond.jmp_pred = pred;
}

static inline void *_get_irn_generic_attr(ir_node *node) {
	return &node->attr;
}

static inline const void *_get_irn_generic_attr_const(const ir_node *node) {
	return &node->attr;
}

static inline unsigned _get_irn_idx(const ir_node *node) {
	return node->node_idx;
}

static inline dbg_info *_get_irn_dbg_info(const ir_node *n) {
	return n->dbi;
}  /* get_irn_dbg_info */

static inline void _set_irn_dbg_info(ir_node *n, dbg_info *db) {
	n->dbi = db;
}

/**
 * Sets the Phi list of a block.
 */
static inline void
_set_Block_phis(ir_node *block, ir_node *phi) {
	assert(_is_Block(block));
	assert(phi == NULL || _is_Phi(phi));
	block->attr.block.phis = phi;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static inline ir_node *
_get_Block_phis(const ir_node *block) {
	assert(_is_Block(block));
	return block->attr.block.phis;
}

/**
 * Sets the next link of a Phi.
 */
static inline void
_set_Phi_next(ir_node *phi, ir_node *next) {
	assert(_is_Phi(phi));
	phi->attr.phi.next = next;
}

/**
 * Returns the link of a node.
 * Intern version of libFirm.
 */
static inline ir_node *
_get_Phi_next(const ir_node *phi) {
	assert(_is_Phi(phi));
	return phi->attr.phi.next;
}

/** Add a Phi node to the list of Block Phi's. */
static inline void
_add_Block_phi(ir_node *block, ir_node *phi) {
	_set_Phi_next(phi, _get_Block_phis(block));
	_set_Block_phis(block, phi);
}

/** Get the Block mark (single bit). */
static inline unsigned
_get_Block_mark(const ir_node *block) {
	assert(_is_Block(block));
	return block->attr.block.marked;
}

/** Set the Block mark (single bit). */
static inline void
_set_Block_mark(ir_node *block, unsigned mark) {
	assert(_is_Block(block));
	block->attr.block.marked = mark;
}

/** Returns non-zero if a node is a routine parameter. */
static inline int
_is_arg_Proj(const ir_node *node) {
	if (! is_Proj(node))
		return 0;
	node = get_Proj_pred(node);
	if (! is_Proj(node))
		return 0;
	return pn_Start_T_args == get_Proj_proj(node) && is_Start(get_Proj_pred(node));
}


/* this section MUST contain all inline functions */
#define is_ir_node(thing)                     _is_ir_node(thing)
#define get_irn_intra_arity(node)             _get_irn_intra_arity(node)
#define get_irn_inter_arity(node)             _get_irn_inter_arity(node)
#define get_irn_arity(node)                   _get_irn_arity(node)
#define get_irn_intra_n(node, n)              _get_irn_intra_n(node, n)
#define get_irn_inter_n(node, n)              _get_irn_inter_n(node, n)
#define get_irn_n(node, n)                    _get_irn_n(node, n)
#define get_irn_mode(node)                    _get_irn_mode(node)
#define set_irn_mode(node, mode)              _set_irn_mode(node, mode)
#define get_irn_op(node)                      _get_irn_op(node)
#define set_irn_op(node, op)                  _set_irn_op(node, op)
#define get_irn_opcode(node)                  _get_irn_opcode(node)
#define get_irn_visited(node)                 _get_irn_visited(node)
#define set_irn_visited(node, v)              _set_irn_visited(node, v)
#define mark_irn_visited(node)                _mark_irn_visited(node)
#define irn_visited(node)                     _irn_visited(node)
#define irn_visited_else_mark(node)           _irn_visited_else_mark(node)
#define set_irn_link(node, link)              _set_irn_link(node, link)
#define get_irn_link(node)                    _get_irn_link(node)
#define get_irn_pinned(node)                  _get_irn_pinned(node)
#define is_irn_pinned_in_irg(node)            _is_irn_pinned_in_irg(node)
#define is_unop(node)                         _is_unop(node)
#define is_binop(node)                        _is_binop(node)
#define is_Proj(node)                         _is_Proj(node)
#define is_Filter(node)                       _is_Filter(node)
#define is_Phi(node)                          _is_Phi(node)
#define is_Const(node)                        _is_Const(node)
#define is_Conv(node)                         _is_Conv(node)
#define is_strictConv(node)                   _is_strictConv(node)
#define is_Cast(node)                         _is_Cast(node)
#define is_Unknown(node)                      _is_Unknown(node)
#define is_Return(node)                       _is_Return(node)
#define is_Call(node)                         _is_Call(node)
#define is_Builtin(node)                      _is_Builtin(node)
#define is_CallBegin(node)                    _is_CallBegin(node)
#define is_Sel(node)                          _is_Sel(node)
#define is_Mul(node)                          _is_Mul(node)
#define is_Mulh(node)                         _is_Mulh(node)
#define is_Mux(node)                          _is_Mux(node)
#define is_Load(node)                         _is_Load(node)
#define is_Sync(node)                         _is_Sync(node)
#define is_Confirm(node)                      _is_Confirm(node)
#define is_Pin(node)                          _is_Pin(node)
#define is_SymConst(node)                     _is_SymConst(node)
#define is_SymConst_addr_ent(node)            _is_SymConst_addr_ent(node)
#define is_Cond(node)                         _is_Cond(node)
#define is_CopyB(node)                        _is_CopyB(node)
#define is_Cmp(node)                          _is_Cmp(node)
#define is_Alloc(node)                        _is_Alloc(node)
#define is_Free(node)                         _is_Free(node)
#define is_Jmp(node)                          _is_Jmp(node)
#define is_IJmp(node)                         _is_IJmp(node)
#define is_Raise(node)                        _is_Raise(node)
#define is_ASM(node)                          _is_ASM(node)
#define is_Dummy(node)                        _is_Dummy(node)
#define is_Anchor(node)                       _is_Anchor(node)
#define is_Bad(node)                          _is_Bad(node)
#define is_NoMem(node)                        _is_NoMem(node)
#define is_Start(node)                        _is_Start(node)
#define is_End(node)                          _is_End(node)
#define is_Minus(node)                        _is_Minus(node)
#define is_Abs(node)                          _is_Abs(node)
#define is_Mod(node)                          _is_Mod(node)
#define is_Div(node)                          _is_Div(node)
#define is_DivMod(node)                       _is_DivMod(node)
#define is_Quot(node)                         _is_Quot(node)
#define is_Add(node)                          _is_Add(node)
#define is_Carry(node)                        _is_Carry(node)
#define is_And(node)                          _is_And(node)
#define is_Or(node)                           _is_Or(node)
#define is_Eor(node)                          _is_Eor(node)
#define is_Sub(node)                          _is_Sub(node)
#define is_Not(node)                          _is_Not(node)
#define is_Shl(node)                          _is_Shl(node)
#define is_Shr(node)                          _is_Shr(node)
#define is_Shrs(node)                         _is_Shrs(node)
#define is_Rotl(node)                         _is_Rotl(node)
#define is_Id(node)                           _is_Id(node)
#define is_Tuple(node)                        _is_Tuple(node)
#define is_Bound(node)                        _is_Bound(node)
#define is_no_Block(node)                     _is_no_Block(node)
#define is_Block(node)                        _is_Block(node)
#define get_Block_n_cfgpreds(node)            _get_Block_n_cfgpreds(node)
#define get_Block_cfgpred(node, pos)          _get_Block_cfgpred(node, pos)
#define get_Block_cfgpred_block(node, pos)    _get_Block_cfgpred_block(node, pos)
#define get_Block_block_visited(node)         _get_Block_block_visited(node)
#define set_Block_block_visited(node, visit)  _set_Block_block_visited(node, visit)
#define mark_Block_block_visited(node)        _mark_Block_block_visited(node)
#define Block_block_visited(node)             _Block_block_visited(node)
#define set_Block_dead(block)                 _set_Block_dead(block)
#define is_Block_dead(block)                  _is_Block_dead(block)
#define get_Const_tarval(node)                _get_Const_tarval(node)
#define is_Const_null(node)                   _is_Const_null(node)
#define is_Const_one(node)                    _is_Const_one(node)
#define is_Const_all_one(node)                _is_Const_all_one(node)
#define is_irn_forking(node)                  _is_irn_forking(node)
#define get_irn_type(node)                    _get_irn_type(node)
#define get_irn_type_attr(node)               _get_irn_type_attr(node)
#define get_irn_entity_attr(node)             _get_irn_entity_attr(node)
#define is_irn_constlike(node)                _is_irn_constlike(node)
#define is_irn_always_opt(node)               _is_irn_always_opt(node)
#define is_irn_keep(node)                     _is_irn_keep(node)
#define is_irn_start_block_placed(node)       _is_irn_start_block_placed(node)
#define is_irn_machine_op(node)               _is_irn_machine_op(node)
#define is_irn_machine_operand(node)          _is_irn_machine_operand(node)
#define is_irn_machine_user(node, n)          _is_irn_machine_user(node, n)
#define get_Cond_jmp_pred(node)               _get_Cond_jmp_pred(node)
#define set_Cond_jmp_pred(node, pred)         _set_Cond_jmp_pred(node, pred)
#define get_irn_generic_attr(node)            _get_irn_generic_attr(node)
#define get_irn_generic_attr_const(node)      _get_irn_generic_attr_const(node)
#define get_irn_idx(node)                     _get_irn_idx(node)

#define get_irn_deps(node)                    _get_irn_deps(node)
#define set_irn_dep(node, pos, dep)           _set_irn_dep(node, pos, dep)
#define get_irn_dep(node, pos)                _get_irn_dep(node, pos)

#define get_irn_ins_or_deps(node)             _get_irn_ins_or_deps(node)
#define get_irn_in_or_dep(node, pos)          _get_irn_in_or_dep(node, pos)

#define get_irn_dbg_info(node)                _get_irn_dbg_info(node)
#define set_irn_dbg_info(node, db)            _set_irn_dbg_info(node, db)

#define set_Block_phis(block, phi)            _set_Block_phis(block, phi)
#define get_Block_phis(block)                 _get_Block_phis(block)
#define add_Block_phi(block, phi)             _add_Block_phi(block, phi)
#define get_Block_mark(block)                 _get_Block_mark(block)
#define set_Block_mark(block, mark)           _set_Block_mark(block, mark)

#define set_Phi_next(node, phi)               _set_Phi_next(node, phi)
#define get_Phi_next(node)                    _get_Phi_next(node)

#define is_arg_Proj(node)                     _is_arg_Proj(node)

#endif
