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


/**
 * The compute value operation.
 * This operation evaluates an IR node into a tarval if possible,
 * returning tarval_bad otherwise.
 */
typedef tarval *(*computed_value_func)(ir_node *self);

/**
 * The equivalent node operation.
 * This operation returns an equivalent node for the input node.
 * It does not create new nodes.  It is therefore safe to free self
 * if the node returned is not self.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., possible for Div).
 */
typedef ir_node *(*equivalent_node_func)(ir_node *self);

/**
 * The transform node operation.
 * This operation tries several [inplace] [optimizing] transformations
 * and returns an equivalent node.
 * The difference to equivalent_node() is that these
 * transformations _do_ generate new nodes, and thus the old node must
 * not be freed even if the equivalent node isn't the old one.
 */
typedef ir_node *(*transform_node_func)(ir_node *self);

/**
 * The node attribute compare operation.
 * Compares the nodes attributes of two nodes of identical opcode
 * and returns 0 if the attributes are identical, 1 if they differ.
 */
typedef int (*node_cmp_attr_func)(ir_node *a, ir_node *b);

/**
 * The reassociation operation.
 * Called from a walker.  Returns non-zero if
 * a reassociation rule was applied.
 * The pointer n is set to the newly created node, if some reassociation
 * was applied.
 */
typedef int (*reassociate_func)(ir_node **n);

/**
 * The copy attribute operation.
 * Copy the node attributes from a 'old' node to a 'new' one.
 */
typedef void (*copy_attr_func)(const ir_node *old_node, ir_node *new_node);

/**
 * The get_type operation.
 * Return the type of the node self.
 */
typedef type *(*get_type_func)(ir_node *self);

/**
 * The verify_node operation.
 * Return non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_node_func)(ir_node *self, ir_graph *irg);

/**
 * The verify_node operation for Proj(X).
 * Return non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_proj_node_func)(ir_node *self, ir_node *proj);

/** The type of an ir_op. */
struct ir_op {
  opcode code;            /**< the unique opcode of the op */
  ident *name;            /**< the name of the op */
  size_t attr_size;       /**< Space needed in memory for private attributes */
  op_pin_state op_pin_state_pinned; /**< How to deal with the node in cse, pre. */
  op_arity opar;          /**< arity of operator. */
  int op_index;           /**< the index of the first data operand, 0 for most cases, 1 for Div etc. */
  unsigned flags;         /**< flags describing the behavior of the ir_op, a bitmaks of irop_flags */

  /* CallBacks */
  computed_value_func	  computed_value;		/**< evaluates a node into a tarval if possible. */
  equivalent_node_func  equivalent_node;	/**< optimizes the node by returning an equivalent one. */
  transform_node_func   transform_node;		/**< optimizes the node by transforming it. */
  node_cmp_attr_func    node_cmp_attr;		/**< compares two node attributes. */
  reassociate_func      reassociate;            /**< reassociate a tree */
  copy_attr_func        copy_attr;              /**< copy node attributes */
  get_type_func         get_type;               /**< return the type of a node */
  verify_node_func      verify_node;            /**< verify the node */
  verify_proj_node_func verify_proj_node;       /**< verify the Proj node */
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
 */
void default_copy_attr(const ir_node *old_node, ir_node *new_node);

/** Returns the attribute size of nodes of this opcode.
   @note Use not encouraged, internal feature. */
static INLINE int get_op_attr_size (const ir_op *op) {
  return op->attr_size;
}

/** Returns non-zero if op is one of Start, End, Jmp, Cond, Return, Raise or Bad. */
static INLINE int is_cfopcode(const ir_op *op) {
  return op->flags & irop_flag_cfopcode;
}

/** Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
static INLINE int is_ip_cfopcode(const ir_op *op) {
  return op->flags & irop_flag_ip_cfopcode;
}

/* Returns non-zero if operation is commutative */
static INLINE int is_op_commutative(const ir_op *op) {
  return op->flags & irop_flag_commutative;
}

/* Returns non-zero if operation is fragile */
static INLINE int is_op_fragile(const ir_op *op) {
  return op->flags & irop_flag_fragile;
}

/* Returns non-zero if operation is forking control flow */
static INLINE int is_op_forking(const ir_op *op) {
  return op->flags & irop_flag_forking;
}

/* Returns non-zero if operation is a high-level op */
static INLINE int is_op_highlevel(const ir_op *op) {
  return op->flags & irop_flag_highlevel;
}

/* Returns non-zero if operation is a const-like op */
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


#define get_op_code(op)         _get_op_code(op)
#define get_op_ident(op)        _get_op_ident(op)
#define get_op_pinned(op)       _get_op_pinned(op)


#endif /* _IROP_T_H_ */
