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

/** The type opcode */
struct tp_op {
  tp_opcode code;
  ident *name;
  size_t attr_size;
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
 *   @param attr_size   the size of the attributes necessary for a type with
 *                      this opcode
 *   @return A new type opcode.
 *
 */
tp_op *new_tpop (tp_opcode code, ident *name, size_t attr_size);

/**
 *   Initialize the tpop module.
 *
 *   Must be called during the initizlization of the library. Allocates
 *   opcodes and sets the globals that are external visible as specified
 *   in tpop.h.
 *   Allocates opcodes for classes, struct, method, union, array,
 *   enumeration, pointer and primitive and sets the according values.
 */
void init_tpop (void);

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
int get_tpop_attr_size (tp_op *op);

#endif /* _TPOP_T_H_ */
