/*
**  Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
**  All rights reserved.
**
**  Authors: Christian Schaefer
**
**  irop.h  operators of firm nodes
**
**  This module specifies the opcodes possible for ir nodes.  Their
**  definition is close to the operations specified in UKA Tech-Report
**  1999-14
**
*/

/* $Id$ */

# ifndef _IROP_H_
# define _IROP_H_

# include <stddef.h>
# include "ident.h"

typedef enum {
  iro_Block,
  iro_Start, iro_End, iro_Jmp, iro_Cond, iro_Return, iro_Raise,
  iro_Const, iro_SymConst,
  iro_Sel,  iro_InstOf,
  iro_Call, iro_Add, iro_Sub, iro_Minus, iro_Mul, iro_Quot, iro_DivMod,
  iro_Div,  iro_Mod, iro_Abs, iro_And, iro_Or, iro_Eor, iro_Not,
  iro_Cmp,  iro_Shl, iro_Shr, iro_Shrs, iro_Rot, iro_Conv,
  iro_Phi,
  iro_Load, iro_Store, iro_Alloc, iro_Free, iro_Sync,
  iro_Proj, iro_Tuple, iro_Id, iro_Bad,
  iro_Unknown, iro_Filter, iro_Break, iro_CallBegin, iro_EndReg, iro_EndExcept
} opcode;

typedef struct ir_op ir_op;

extern ir_op *op_Block;

extern ir_op *op_Start;
extern ir_op *op_End;
extern ir_op *op_Jmp;
extern ir_op *op_Cond;
extern ir_op *op_Return;
extern ir_op *op_Raise;
extern ir_op *op_Sel;
extern ir_op *op_InstOf;

extern ir_op *op_Const;
extern ir_op *op_SymConst;

extern ir_op *op_Call;
extern ir_op *op_Add;
extern ir_op *op_Sub;
extern ir_op *op_Minus;
extern ir_op *op_Mul;
extern ir_op *op_Quot;
extern ir_op *op_DivMod;
extern ir_op *op_Div;
extern ir_op *op_Mod;
extern ir_op *op_Abs;
extern ir_op *op_And;
extern ir_op *op_Or;
extern ir_op *op_Eor;
extern ir_op *op_Not;
extern ir_op *op_Cmp;
extern ir_op *op_Shl;
extern ir_op *op_Shr;
extern ir_op *op_Shrs;
extern ir_op *op_Rot;
extern ir_op *op_Conv;

extern ir_op *op_Phi;

extern ir_op *op_Load;
extern ir_op *op_Store;
extern ir_op *op_Alloc;
extern ir_op *op_Free;

extern ir_op *op_Sync;

extern ir_op *op_Tuple;
extern ir_op *op_Proj;
extern ir_op *op_Id;
extern ir_op *op_Bad;

extern ir_op *op_Unknown;
extern ir_op *op_Filter;
extern ir_op *op_Break;
extern ir_op *op_CallBegin;
extern ir_op *op_EndReg;
extern ir_op *op_EndExcept;


/* Returns the string for the opcode. */
const char *get_op_name      (ir_op *op);

/* Returns the enum for the opcode */
opcode get_op_code      (ir_op *op);

/* Returns the ident for the opcode name */
ident *get_op_ident     (ir_op *op);

typedef enum {
  floats = 0,    /* Nodes of this opcode can be placed in any basic block. */
  pinned           /* Nodes must remain in this basic block. */
} op_pinned;

op_pinned get_op_pinned (ir_op *op);
/* Sets pinned in the opcode.  Setting it to floating has no effect
   for Block, Phi and control flow nodes. */
void      set_op_pinned(ir_op *op, op_pinned pinned);

/* Returns the attribute size of nodes of this opcode.
   Use not encouraged, internal feature. */
int    get_op_attr_size (ir_op *op);

/* Returns true if op is one of Start, End, Jmp, Cond, Return, Raise or Bad. */
int is_cfopcode(ir_op *op);


# endif /* _IROP_H_ */
