
/* $Id$ */

# ifndef _IROP_T_H_
# define _IROP_T_H_

# include "irop.h"

struct ir_op {
  opcode code;
  ident *name;
  size_t attr_size;     /* Space needed in memory for private attributes */
  int labeled;          /* Output edge labels on in-edges in vcg graph */
  int pinned;           /* How to deal with the node in cse, pre. */
};

/* create a new ir operation */
ir_op * new_ir_op (opcode code, char *name, op_pinned p,
		   int labeled, size_t attr_size);

/* initialize the irop module */
void init_op (void);

#endif
