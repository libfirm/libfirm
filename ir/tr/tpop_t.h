/*
 * Project:     libFIRM
 * File name:   ir/tr/tpop_t.h
 * Purpose:     Opcode of types -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifndef _TPOP_T_H_
# define _TPOP_T_H_

# include <stddef.h>
# include "tpop.h"
/**
 * @file tpop_t.h
 *
 * This file contains the datatypes hidden in tpop.h.
 *
 * @autor Goetz Lindenmaier
 * @see  tpop.h
 */

/** possible flags for a type opcode */
enum tp_op_flags_t {
  TP_OP_FLAG_COMPOUND = 1   /**< is a compound type */
};

/** The type opcode */
struct tp_op {
  tp_opcode code;       /**< the tpop code */
  ident *name;          /**< the name of the type opcode */
  size_t attr_size;     /**< the attribute size for a type of this opcode */
  unsigned flags;       /**< flags for this opcode */
};

/**
 *   Returns a new type opcode.
 *
 *   Allocates a new tp_op struct and initializes it's fields with
 *   the passed values.  This function is only to be used during
 *   initialization of the library.
 *
 *   @param code        the enum for this type opcode.
 *   @param name        an ident for the name of the type opcode.
 *   @param flags       additional flags
 *   @param attr_size   the size of the attributes necessary for a type with
 *                      this opcode
 *   @return A new type opcode.
 */
tp_op *new_tpop (tp_opcode code, ident *name, unsigned flags, size_t attr_size);

/**
 * Free a tpop datastructure.
 */
void free_tpop(tp_op* tpop);

/**
 *   Initialize the tpop module.
 *
 *   Must be called during the initialization of the library. Allocates
 *   opcodes and sets the globals that are external visible as specified
 *   in tpop.h.
 *   Allocates opcodes for classes, struct, method, union, array,
 *   enumeration, pointer and primitive and sets the according values.
 */
void init_tpop (void);

/**
 *  Finalize the tpop module.
 *
 *  Frees all type opcodes.
 */
void finish_tpop(void);

/**
 *   Returns the size of the attribute to this kind
 *   of type.
 *
 *   Internal feature.
 *
 *   @param op  The type opcode to get the size for.
 *   @return The size of the attribute of types with this opcode.
 *
 */
int get_tpop_attr_size (const tp_op *op);


/* ---------------- *
 * inline functions *
 * -----------------*/

static INLINE tp_opcode
__get_tpop_code(const tp_op *op) {
  return op->code;
}

static INLINE ident *
__get_tpop_ident(const tp_op *op){
  return op->name;
}

static INLINE int
__get_tpop_attr_size(const tp_op *op) {
  return op->attr_size;
}

#define get_tpop_code(op)      __get_tpop_code(op)
#define get_tpop_ident(op)     __get_tpop_ident(op)
#define get_tpop_attr_size(op) __get_tpop_attr_size(op)

#endif /* _TPOP_T_H_ */
