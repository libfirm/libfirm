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
 * @brief    Representation of opcode of intermediate operation -- private header.
 * @author   Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version  $Id$
 */
#ifndef FIRM_IR_IROP_T_H
#define FIRM_IR_IROP_T_H

#include "firm_config.h"
#include "irop.h"
#include "tv.h"
#include "irnode.h"


/** The type of an ir_op. */
struct ir_op {
  ir_opcode code;         /**< The unique opcode of the op. */
  ident *name;            /**< The name of the op. */
  size_t attr_size;       /**< Space needed in memory for private attributes. */
  op_pin_state op_pin_state_pinned; /**< How to deal with the node in CSE, PRE. */
  op_arity opar;          /**< The arity of operator. */
  int op_index;           /**< The index of the first data operand, 0 for most cases, 1 for Div etc. */
  unsigned flags;         /**< Flags describing the behavior of the ir_op, a bitmasks of irop_flags. */
  void *tag;              /**< Some custom pointer the op's creator can attach stuff to. */

  ir_op_ops ops;          /**< The operations of the this op. */
};

/**
 * Frees a newly created ir operation.
 */
void free_ir_op(ir_op *code);

/** Initialize the irop module. */
void init_op(void);

/** Free memory used by irop module. */
void finish_op(void);

/**
 * Copies simply all attributes stored in the old node to the new node.
 * Assumes both have the same opcode and sufficient size.
 *
 * @param old_node  the old node from which the attributes are read
 * @param new_node  the new node to which the attributes are written
 */
void default_copy_attr(const ir_node *old_node, ir_node *new_node);

/**
 * Returns the attribute size of nodes of this opcode.
 * @note Use not encouraged, internal feature.
 */
static INLINE size_t get_op_attr_size (const ir_op *op) {
  return op->attr_size;
}

/**
 * Returns non-zero if op is a control flow opcode,
 * like Start, End, Jmp, Cond, Return, Raise or Bad.
 */
static INLINE int is_cfopcode(const ir_op *op) {
  return op->flags & irop_flag_cfopcode;
}

/**
 * Returns non-zero if the operation manipulates interprocedural control flow:
 * CallBegin, EndReg, EndExcept
 */
static INLINE int is_ip_cfopcode(const ir_op *op) {
  return op->flags & irop_flag_ip_cfopcode;
}

/** Returns non-zero if operation is commutative */
static INLINE int is_op_commutative(const ir_op *op) {
  return op->flags & irop_flag_commutative;
}

/** Returns non-zero if operation is fragile */
static INLINE int is_op_fragile(const ir_op *op) {
  return op->flags & irop_flag_fragile;
}

/** Returns non-zero if operation is forking control flow */
static INLINE int is_op_forking(const ir_op *op) {
  return op->flags & irop_flag_forking;
}

/** Returns non-zero if operation is a high-level op */
static INLINE int is_op_highlevel(const ir_op *op) {
  return op->flags & irop_flag_highlevel;
}

/** Returns non-zero if operation is a const-like op */
static INLINE int is_op_constlike(const ir_op *op) {
  return op->flags & irop_flag_constlike;
}

/** Returns non-zero if operation must always be optimized */
static INLINE int is_op_always_opt(const ir_op *op) {
  return op->flags & irop_flag_always_opt;
}

/** Returns non-zero if operation is a keep-like op */
static INLINE int is_op_keep(const ir_op *op) {
  return op->flags & irop_flag_keep;
}

/** Returns non-zero if operation must always be placed in the start block. */
static INLINE int is_op_start_block_placed(const ir_op *op) {
  return op->flags & irop_flag_start_block;
}

/** Returns non-zero if operation is a machine operation */
static INLINE int is_op_machine(const ir_op *op) {
  return op->flags & irop_flag_machine;
}

/** Returns non-zero if operation is a machine operand */
static INLINE int is_op_machine_operand(const ir_op *op) {
  return op->flags & irop_flag_machine_op;
}

/** Returns non-zero if operation is a machine user op number n */
static INLINE int is_op_machine_user(const ir_op *op, unsigned n) {
  return op->flags & (irop_flag_user << n);
}

static INLINE ir_opcode _get_op_code(const ir_op *op) {
  return op->code;
}

static INLINE ident *_get_op_ident(const ir_op *op){
  return op->name;
}

static INLINE op_pin_state _get_op_pinned(const ir_op *op) {
  return op->op_pin_state_pinned;
}

static INLINE void _set_generic_function_ptr(ir_op *op, op_func func) {
  op->ops.generic = func;
}

static INLINE op_func _get_generic_function_ptr(const ir_op *op) {
  return op->ops.generic;
}

static INLINE const ir_op_ops *_get_op_ops(const ir_op *op) {
  return &op->ops;
}

static INLINE void _set_op_tag(ir_op *op, void *tag) {
	op->tag = tag;
}

static INLINE void *_get_op_tag(const ir_op *op) {
	return op->tag;
}

#define get_op_code(op)         _get_op_code(op)
#define get_op_ident(op)        _get_op_ident(op)
#define get_op_pinned(op)       _get_op_pinned(op)
#define get_op_ops(op)          _get_op_ops(op)
#define set_op_tag(op, tag)     _set_op_tag((op), (tag))
#define get_op_tag(op)          _get_op_tag(op)

#endif
