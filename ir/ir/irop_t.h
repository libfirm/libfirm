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

/** the type of an ir_op */
struct ir_op {
  opcode code;
  ident *name;
  size_t attr_size;     /**< Space needed in memory for private attributes */
  int labeled;          /**< Output edge labels on in-edges in vcg graph */
  int pinned;           /**< How to deal with the node in cse, pre. */
};

/**
 * Create a new ir operation.
 *
 * @param code     the opcode, one of type \c opcode
 * @param name     the printable name of this opcode
 * @param p        wheater operations of this opcode are pinned or floating
 * @param labeled  if set, output edge labels on in-edges in vcg graph wil be generated
 *
 * @return The genenerated ir operation.
 */
ir_op * new_ir_op (opcode code, const char *name, op_pinned p,
		   int labeled, size_t attr_size);

/** initialize the irop module */
void init_op (void);

#endif /* _IROP_T_H_ */
