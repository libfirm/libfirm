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


# ifndef _IROP_T_H_
# define _IROP_T_H_

# include "irop.h"
# include "tv.h"
# include "irnode.h"

/** The allowed parities */
typedef enum {
  oparity_invalid = 0,
  oparity_unary,              /**< an unary operator -- considering 'numeric' arguments. */
  oparity_binary,             /**< an binary operator  -- considering 'numeric' arguments.*/
  oparity_trinary,            /**< an trinary operator  -- considering 'numeric' arguments.*/
  oparity_zero,               /**< no operators, as e.g. Const. */
  oparity_variable,           /**< arity not fixed by opcode, but statically
				 known.  E.g., number of arguments to call. */
  oparity_dynamic,            /**< arity depends on state of firm representation.
				 Can change by optimizations...
				 We must allocate a dynamic in array for the node! */
  oparity_any,                /**< other arity */
} op_arity;


/** The irop flags */
typedef enum {
  irop_flag_none        = 0x00000000,   /**< nothing */
  irop_flag_labeled     = 0x00000001,	/**< if set, Output edge labels on in-edges in vcg graph */
  irop_flag_commutative = 0x00000002,	/**< operation is commutative */
  irop_flag_cfopcode    = 0x00000004,   /**< is a control flow operation */
  irop_flag_ip_cfopcode = 0x00000008,	/**< operation manipulates interprocedural control flow */
  irop_flag_fragile     = 0x00000010,	/**< set if the operation can change the control flow because
                                             of an exception */
  irop_flag_forking     = 0x00000020,   /**< the operation is a forking control flow */
} irop_flags;


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
  computed_value_func	computed_value;		/**< evaluates a node into a tarval if possible. */
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
 * Creates a new ir operation.
 *
 * @param code      the opcode, one of type \c opcode
 * @param name      the printable name of this opcode
 * @param p         wheater operations of this opcode are op_pin_state_pinned or floating
 * @param flags     a bitmask of irop_flags describing the behavior of the ir operation
 * @param opar      the parity of this ir operation
 * @param op_index  if the parity is oparity_unary, oparity_binary or oparity_trinary the index
 *                  of the left operand
 * @param attr_size attribute size for this ir operation
 *
 * @return The genenerated ir operation.
 */
ir_op * new_ir_op(opcode code, const char *name, op_pin_state p,
		   unsigned flags, op_arity opar, int op_index, size_t attr_size);

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

static INLINE opcode __get_op_code(const ir_op *op) {
  return op->code;
}

static INLINE ident *__get_op_ident(const ir_op *op){
  return op->name;
}

static INLINE op_pin_state __get_op_pinned(const ir_op *op) {
  return op->op_pin_state_pinned;
}


#define get_op_code(op)         __get_op_code(op)
#define get_op_ident(op)        __get_op_ident(op)
#define get_op_pinned(op)       __get_op_pinned(op)


#endif /* _IROP_T_H_ */
