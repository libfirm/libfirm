
/* $Id$ */

# ifndef _IROP_T_H_
# define _IROP_T_H_

# include <stddef.h>
# include "tpop.h"
/****h* libfirm/tpop_t.h
 *
 * NAME
 *   file tpop_t.h
 * COPYRIGHT
 *   (C) 2001 by Universitaet Karlsruhe
 * AUTHORS
 *   Goetz Lindenmaier
 * NOTES
 *   This file contains the datatypes hidden in tpop.h.
 * SEE ALSO
 *   tpop.h
 *****
 */

struct tp_op {
  tp_opcode code;
  ident *name;
  size_t attr_size;
};

/****f* tpop/new_tpop
 *
 * NAME
 *   new_tpop - Returns a new type opcode.
 * NOTE
 *   Allocates a new tp_op struct and initializes it's fields with
 *   the passed values.  This function is only to be used during
 *   initialization of the library.
 * SYNOPSIS
 *   tp_op * new_tpop (tp_opcode code, ident *name, size_t attr_size);
 * INPUTS
 *   code      - the enum for this type opcode.
 *   name      - an ident for the name of the type opcode.
 *   attr_size - the size of the attributes necessary for a type with
 *               this opcode
 * RESULT
 *   A new type opcode.
 ***
 */
tp_op * new_tpop (tp_opcode code, ident *name, size_t attr_size);

/****f* tpop/new_tpop
 *
 * NAME
 *   init_tpop - Initialize the tpop module.
 * NOTE
 *   Must be called during the initizlization of the library. Allocates
 *   opcodes and sets the globals that are external visible as specified
 *   in tpop.h.
 * SYNOPSIS
 *   void init_tpop (void);
 * INPUTS
 * RESULT
 * SIDE EFFECTS
 *   Allocates opcodes for classes, struct, method, union, array,
 *   enumeration, pointer and primitive and sets the according values.
 ***
 */
void init_tpop (void);

/****f* tpop/get_tpop_attr_size
 *
 * NAME
 *   get_tpop_attr_size - Returns the size of the attribute to this kind
 *   of type.
 * NOTE
 *   Internal feature.
 * SYNOPSIS
 *   int get_tpop_attr_size (tp_op *op)
 * INPUTS
 *   op - The type opcode to get the size for.
 * RESULT
 *   The size of the attribute of types with this opcode.
 * SIDE EFFECTS
 ***
 */
int get_tpop_attr_size (tp_op *op);

#endif /* _IROP_T_H_ */
