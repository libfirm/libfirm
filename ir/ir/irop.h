/*
*  Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
*  All rights reserved.
*
*  Authors: Christian Schaefer
*
*  irop.h  operators of firm nodes
*
*  This module specifies the opcodes possible for ir nodes.  Their
*  definition is close to the operations specified in UKA Tech-Report
*  1999-14
*
*/

/* $Id$ */

# ifndef _IROP_H_
# define _IROP_H_

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

extern ir_op *op_Block;           ir_op *get_op_Block     ();

extern ir_op *op_Start;           ir_op *get_op_Start     ();
extern ir_op *op_End;             ir_op *get_op_End       ();
extern ir_op *op_Jmp;             ir_op *get_op_Jmp       ();
extern ir_op *op_Cond;            ir_op *get_op_Cond      ();
extern ir_op *op_Return;          ir_op *get_op_Return    ();
extern ir_op *op_Raise;           ir_op *get_op_Raise     ();
extern ir_op *op_Sel;             ir_op *get_op_Sel       ();
extern ir_op *op_InstOf;          ir_op *get_op_InstOf    ();

extern ir_op *op_Const;           ir_op *get_op_Const     ();
extern ir_op *op_SymConst;        ir_op *get_op_SymConst  ();

extern ir_op *op_Call;            ir_op *get_op_Call      ();
extern ir_op *op_Add;		  ir_op *get_op_Add       ();
extern ir_op *op_Sub;		  ir_op *get_op_Sub       ();
extern ir_op *op_Minus;		  ir_op *get_op_Minus     ();
extern ir_op *op_Mul;		  ir_op *get_op_Mul       ();
extern ir_op *op_Quot;		  ir_op *get_op_Quot      ();
extern ir_op *op_DivMod;	  ir_op *get_op_DivMod    ();
extern ir_op *op_Div;		  ir_op *get_op_Div       ();
extern ir_op *op_Mod;		  ir_op *get_op_Mod       ();
extern ir_op *op_Abs;		  ir_op *get_op_Abs       ();
extern ir_op *op_And;		  ir_op *get_op_And       ();
extern ir_op *op_Or;              ir_op *get_op_Or        ();
extern ir_op *op_Eor;		  ir_op *get_op_Eor       ();
extern ir_op *op_Not;		  ir_op *get_op_Not       ();
extern ir_op *op_Cmp;		  ir_op *get_op_Cmp       ();
extern ir_op *op_Shl;		  ir_op *get_op_Shl       ();
extern ir_op *op_Shr;		  ir_op *get_op_Shr       ();
extern ir_op *op_Shrs;		  ir_op *get_op_Shrs      ();
extern ir_op *op_Rot;		  ir_op *get_op_Rot       ();
extern ir_op *op_Conv;		  ir_op *get_op_Conv      ();

extern ir_op *op_Phi;		  ir_op *get_op_Phi       ();

extern ir_op *op_Load;            ir_op *get_op_Load      ();
extern ir_op *op_Store;		  ir_op *get_op_Store     ();
extern ir_op *op_Alloc;		  ir_op *get_op_Alloc     ();
extern ir_op *op_Free;		  ir_op *get_op_Free      ();

extern ir_op *op_Sync;		  ir_op *get_op_Sync      ();

extern ir_op *op_Tuple;		  ir_op *get_op_Tuple     ();
extern ir_op *op_Proj;		  ir_op *get_op_Proj      ();
extern ir_op *op_Id;		  ir_op *get_op_Id        ();
extern ir_op *op_Bad;		  ir_op *get_op_Bad       ();

extern ir_op *op_Unknown;         ir_op *get_op_Unknown   ();
extern ir_op *op_Filter;	  ir_op *get_op_Filter    ();
extern ir_op *op_Break;		  ir_op *get_op_Break     ();
extern ir_op *op_CallBegin;	  ir_op *get_op_CallBegin ();
extern ir_op *op_EndReg;	  ir_op *get_op_EndReg    ();
extern ir_op *op_EndExcept;  	  ir_op *get_op_EndExcept ();


/* Returns the ident for the opcode name */
ident *get_op_ident     (ir_op *op);
/* Returns the string for the opcode. */
const char *get_op_name (ir_op *op);

/* Returns the enum for the opcode */
opcode get_op_code      (ir_op *op);

typedef enum {
  floats = 0,    /* Nodes of this opcode can be placed in any basic block. */
  pinned           /* Nodes must remain in this basic block. */
} op_pinned;

op_pinned get_op_pinned (ir_op *op);
/* Sets pinned in the opcode.  Setting it to floating has no effect
   for Block, Phi and control flow nodes. */
void      set_op_pinned(ir_op *op, op_pinned pinned);

/* Returns true if op is one of Start, End, Jmp, Cond, Return, Raise or Bad. */
int is_cfopcode(ir_op *op);

/* Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
int is_ip_cfopcode(ir_op *op);

/* Returns the attribute size of nodes of this opcode.
   @@@ Use not encouraged, internal feature. */
int    get_op_attr_size (ir_op *op);

# endif /* _IROP_H_ */
