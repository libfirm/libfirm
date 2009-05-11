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
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <string.h>

#include "irop_t.h"
#include "irnode_t.h"
#include "irhooks.h"
#include "irbackedge_t.h"

#include "iropt_t.h"             /* for firm_set_default_operations */
#include "irvrfy_t.h"
#include "reassoc_t.h"

#include "xmalloc.h"

void be_init_op(void);

/** the available next opcode */
static unsigned next_iro = iro_MaxOpcode;

/*
 * Copies all attributes stored in the old node to the new node.
 * Assumes both have the same opcode and sufficient size.
 */
void default_copy_attr(const ir_node *old_node, ir_node *new_node) {
	unsigned size = firm_add_node_size;

	assert(get_irn_op(old_node) == get_irn_op(new_node));
	memcpy(&new_node->attr, &old_node->attr, get_op_attr_size(get_irn_op(old_node)));

	if (size > 0) {
		/* copy additional node data */
		memcpy(get_irn_data(new_node, void, size), get_irn_data(old_node, void, size), size);
	}
}  /* default_copy_attr */

/**
 * Copies all Call attributes stored in the old node to the new node.
 */
static void
call_copy_attr(const ir_node *old_node, ir_node *new_node) {
	default_copy_attr(old_node, new_node);
	remove_Call_callee_arr(new_node);
}  /* call_copy_attr */

/**
 * Copies all Block attributes stored in the old node to the new node.
 */
static void
block_copy_attr(const ir_node *old_node, ir_node *new_node) {
	ir_graph *irg = current_ir_graph;

	default_copy_attr(old_node, new_node);
	new_node->attr.block.phis        = NULL;
	new_node->attr.block.cg_backedge = NULL;
	new_node->attr.block.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
	INIT_LIST_HEAD(&new_node->attr.block.succ_head);
}  /* block_copy_attr */

/**
 * Copies all phi attributes stored in old node to the new node
 */
static void
phi_copy_attr(const ir_node *old_node, ir_node *new_node) {
	ir_graph *irg = current_ir_graph;

	default_copy_attr(old_node, new_node);
	new_node->attr.phi.next       = NULL;
	new_node->attr.phi.u.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
}

/**
 * Copies all filter attributes stored in old node to the new node
 */
static void
filter_copy_attr(const ir_node *old_node, ir_node *new_node) {
	ir_graph *irg = current_ir_graph;

	default_copy_attr(old_node, new_node);
	new_node->attr.filter.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
}

/**
 * Copies all ASM attributes stored in old node to the new node
 */
static void
ASM_copy_attr(const ir_node *old_node, ir_node *new_node) {
	ir_graph *irg = current_ir_graph;

	default_copy_attr(old_node, new_node);
	new_node->attr.assem.inputs  = DUP_ARR_D(ir_asm_constraint, irg->obst, old_node->attr.assem.inputs);
	new_node->attr.assem.outputs = DUP_ARR_D(ir_asm_constraint, irg->obst, old_node->attr.assem.outputs);
	new_node->attr.assem.clobber = DUP_ARR_D(ir_asm_constraint, irg->obst, old_node->attr.assem.clobber);
}

/**
 * Sets the default copy_attr operation for an ir_ops
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
static ir_op_ops *firm_set_default_copy_attr(ir_opcode code, ir_op_ops *ops) {
	switch(code) {
	case iro_Call:
		ops->copy_attr = call_copy_attr;
		break;
	case iro_Block:
		ops->copy_attr = block_copy_attr;
		break;
	case iro_Phi:
		ops->copy_attr = phi_copy_attr;
		break;
	case iro_Filter:
		ops->copy_attr = filter_copy_attr;
		break;
	case iro_ASM:
		ops->copy_attr = ASM_copy_attr;
		break;
	default:
		/* not allowed to be NULL */
		if (! ops->copy_attr)
			ops->copy_attr = default_copy_attr;
	}
	return ops;
}  /* firm_set_default_copy_attr */

/* Creates a new ir operation. */
ir_op *
new_ir_op(unsigned code, const char *name, op_pin_state p,
          unsigned flags, op_arity opar, int op_index, size_t attr_size,
          const ir_op_ops *ops)
{
	ir_op *res = XMALLOCZ(ir_op);

	res->code      = code;
	res->name      = new_id_from_chars(name, strlen(name));
	res->pin_state = p;
	res->attr_size = attr_size;
	res->flags     = flags;
	res->opar      = opar;
	res->op_index  = op_index;
	res->tag       = 0;

	if (ops)
		memcpy(&res->ops, ops, sizeof(res->ops));
	else /* no given ops, set all operations to NULL */
		memset(&res->ops, 0, sizeof(res->ops));

	firm_set_default_operations(code, &res->ops);
	firm_set_default_copy_attr(code, &res->ops);
	firm_set_default_verifyer(code, &res->ops);
	firm_set_default_reassoc(code, &res->ops);

	add_irp_opcode(res);

	hook_new_ir_op(res);
	return res;
}  /* new_ir_op */

void free_ir_op(ir_op *code) {
	hook_free_ir_op(code);

	remove_irp_opcode(code);
	free(code);
}  /* free_ir_op */

/* Returns the string for the opcode. */
const char *get_op_name (const ir_op *op) {
	return get_id_str(op->name);
}  /* get_op_name */

unsigned (get_op_code)(const ir_op *op){
  return _get_op_code(op);
}  /* get_op_code */

ident *(get_op_ident)(const ir_op *op){
  return _get_op_ident(op);
}  /* get_op_ident */

const char *get_op_pin_state_name(op_pin_state s) {
	switch(s) {
#define XXX(s) case s: return #s
	XXX(op_pin_state_floats);
	XXX(op_pin_state_pinned);
	XXX(op_pin_state_exc_pinned);
	XXX(op_pin_state_mem_pinned);
#undef XXX
	}
	return "<none>";
}  /* get_op_pin_state_name */

op_pin_state (get_op_pinned)(const ir_op *op) {
	return _get_op_pinned(op);
}  /* get_op_pinned */

/* Sets op_pin_state_pinned in the opcode.  Setting it to floating has no effect
   for Phi, Block and control flow nodes. */
void set_op_pinned(ir_op *op, op_pin_state pinned) {
	if (op == op_Block || op == op_Phi || is_op_cfopcode(op)) return;
	op->pin_state = pinned;
}  /* set_op_pinned */

/* retrieve the next free opcode */
unsigned get_next_ir_opcode(void) {
	return next_iro++;
}  /* get_next_ir_opcode */

/* Returns the next free n IR opcode number, allows to register a bunch of user ops */
unsigned get_next_ir_opcodes(unsigned num) {
	unsigned base = next_iro;
	next_iro += num;
	return base;
}  /* get_next_ir_opcodes */

/* Returns the generic function pointer from an ir operation. */
op_func (get_generic_function_ptr)(const ir_op *op) {
	return _get_generic_function_ptr(op);
}  /* get_generic_function_ptr */

/* Store a generic function pointer into an ir operation. */
void (set_generic_function_ptr)(ir_op *op, op_func func) {
	_set_generic_function_ptr(op, func);
}  /* set_generic_function_ptr */

/* Returns the ir_op_ops of an ir_op. */
const ir_op_ops *(get_op_ops)(const ir_op *op) {
	return _get_op_ops(op);
}  /* get_op_ops */

irop_flags get_op_flags(const ir_op *op) {
	return op->flags;
}

#include "gen_irop.c.inl"
