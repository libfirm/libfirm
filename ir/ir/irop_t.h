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
  irop_flag_labeled     = 0x00000001,	/**< if set, Output edge labels on in-edges in vcg graph */
  irop_flag_commutative = 0x00000002,	/**< operation is commutative */
  irop_flag_cfopcode    = 0x00000004,   /**< is a control flow operation */
  irop_flag_ip_cfopcode = 0x00000008,	/**< operation manipulates interprocedural control flow */
  irop_flag_fragile     = 0x00000010,	/**< set if the operation can change the control flow because
                                             of an exception */
} irop_flags;


/** the type of an ir_op */
struct ir_op {
  opcode code;            /**< the unique opcode of the op */
  ident *name;            /**< the name of the op */
  size_t attr_size;       /**< Space needed in memory for private attributes */
  op_pinned pinned;       /**< How to deal with the node in cse, pre. */
  op_arity opar;          /**< arity of operator. */
  int op_index;           /**< the index of the first data operand, 0 for most cases, 1 for Div etc. */
  unsigned flags;         /**< flags describing the behavior of the ir_op, a bitmaks of irop_flags */
};

/**
 * Creates a new ir operation.
 *
 * @param code      the opcode, one of type \c opcode
 * @param name      the printable name of this opcode
 * @param p         wheater operations of this opcode are pinned or floating
 * @param flags     a bitmask of irop_flags describing the behavior of the ir operation
 * @param opar      the parity of this ir operation
 * @param op_index  if the parity is oparity_unary, oparity_binary or oparity_trinary the index
 *                  of the left operand
 * @param attr_size attribute size for this ir operation
 *
 * @return The genenerated ir operation.
 */
ir_op * new_ir_op(opcode code, const char *name, op_pinned p,
		   unsigned flags, op_arity opar, int op_index, size_t attr_size);

/**
 * Frees a newly created ir operation.
 */
void free_ir_op(ir_op *code);

/** Initialize the irop module. */
void init_op(void);

/** Free memory used by irop module. */
void finish_op(void);

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

#endif /* _IROP_T_H_ */
