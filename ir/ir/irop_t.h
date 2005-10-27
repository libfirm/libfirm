/*
 * Project:     libFIRM
 * File name:   ir/ir/irop_t.h
 * Purpose:     Representation of opcode of intermediate operation -- private header.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _IROP_T_H_
#define _IROP_T_H_

#include "firm_config.h"
#include "irop.h"
#include "tv.h"
#include "irnode.h"


/** The type of an ir_op. */
struct ir_op {
  opcode code;            /**< the unique opcode of the op */
  ident *name;            /**< the name of the op */
  size_t attr_size;       /**< Space needed in memory for private attributes */
  op_pin_state op_pin_state_pinned; /**< How to deal with the node in cse, pre. */
  op_arity opar;          /**< arity of operator. */
  int op_index;           /**< the index of the first data operand, 0 for most cases, 1 for Div etc. */
  unsigned flags;         /**< flags describing the behavior of the ir_op, a bitmaks of irop_flags */

  ir_op_ops ops;          /**< the operations of the this op. */
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
static INLINE int get_op_attr_size (const ir_op *op) {
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

static INLINE opcode _get_op_code(const ir_op *op) {
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

#define get_op_code(op)         _get_op_code(op)
#define get_op_ident(op)        _get_op_ident(op)
#define get_op_pinned(op)       _get_op_pinned(op)
#define get_op_ops(op)          _get_op_ops(op)


#endif /* _IROP_T_H_ */
