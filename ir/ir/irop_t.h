
# ifndef _IROP_T_H_
# define _IROP_T_H_

# include "irop.h"

struct ir_op {
  opcode code;
  ident *name;
  size_t attr_size;
  int labeled;
};

/* create a new ir operation */
ir_op * new_ir_op (opcode code, ident *name, size_t attr_size, int labeled);

/* initialize the irop module */
void init_op (void);

#endif
