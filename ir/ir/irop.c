/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <string.h>

# include "irop_t.h"
# include "irnode_t.h"
# include "misc.h"

ir_op *op_Block;

ir_op *op_Start;
ir_op *op_End;
ir_op *op_Jmp;
ir_op *op_Cond;
ir_op *op_Return;
ir_op *op_Raise;

ir_op *op_Sel;
ir_op *op_InstOf;

ir_op *op_Const;
ir_op *op_SymConst;

ir_op *op_Call;
ir_op *op_Add;
ir_op *op_Sub;
ir_op *op_Minus;
ir_op *op_Mul;
ir_op *op_Quot;
ir_op *op_DivMod;
ir_op *op_Div;
ir_op *op_Mod;
ir_op *op_Abs;
ir_op *op_And;
ir_op *op_Or;
ir_op *op_Eor;
ir_op *op_Not;
ir_op *op_Cmp;
ir_op *op_Shl;
ir_op *op_Shr;
ir_op *op_Shrs;
ir_op *op_Rot;
ir_op *op_Conv;

ir_op *op_Phi;

ir_op *op_Load;
ir_op *op_Store;
ir_op *op_Alloc;
ir_op *op_Free;
ir_op *op_Sync;

ir_op *op_Tuple;
ir_op *op_Proj;
ir_op *op_Id;
ir_op *op_Bad;

ir_op *op_Unknown;
ir_op *op_Filter;
ir_op *op_Break;
ir_op *op_CallBegin;
ir_op *op_EndReg;
ir_op *op_EndExcept;


ir_op *
new_ir_op (opcode code, char *name, op_pinned p, int labeled, size_t attr_size)
{
  ir_op *res;

  res = (ir_op *) xmalloc (sizeof (ir_op));
  res->code = code;
  res->name = id_from_str(name, strlen(name));
  res->pinned = p;
  res->attr_size = attr_size;
  res->labeled = labeled;   /* For vcg dumping.
                               Set labeled = 1 if the edges should be
			       enumarated in vcg output, otherwise set
			       labeled = 0. */
  return res;
}


void
init_op(void)
{
  op_Block = new_ir_op (iro_Block, "Block",  pinned, 1, sizeof (block_attr));

  op_Start = new_ir_op (iro_Start, "Start",  pinned, 0, 0);
  op_End   = new_ir_op (iro_End,   "End",    pinned, 0, 0);
  op_Jmp   = new_ir_op (iro_Jmp,   "Jmp",    pinned, 0, 0);
  op_Cond  = new_ir_op (iro_Cond,  "Cond",   pinned, 1, sizeof(cond_attr));
  op_Return= new_ir_op (iro_Return,"Return", pinned, 1, 0);
  op_Raise = new_ir_op (iro_Raise, "Raise",  pinned, 1, 0);

  op_Const = new_ir_op (iro_Const, "Const",  floats, 0, sizeof (struct tarval *));
  op_SymConst = new_ir_op (iro_SymConst, "SymConst",
			                     floats, 0, sizeof (symconst_attr));

  op_Sel   = new_ir_op (iro_Sel,   "Sel",    floats, 1, sizeof (sel_attr));
  op_InstOf= new_ir_op (iro_InstOf,"InstOf", floats, 1, sizeof (sel_attr));

  op_Call  = new_ir_op (iro_Call,  "Call",   pinned, 1, sizeof (call_attr));
  op_Add   = new_ir_op (iro_Add,   "Add",    floats, 0, 0);
  op_Minus = new_ir_op (iro_Minus, "Minus",  floats, 0, 0);
  op_Sub   = new_ir_op (iro_Sub,   "Sub",    floats, 1, 0);
  op_Mul   = new_ir_op (iro_Mul,   "Mul",    floats, 0, 0);
  op_Quot  = new_ir_op (iro_Quot,  "Quot",   pinned, 1, sizeof(struct irnode **));
  op_DivMod= new_ir_op (iro_DivMod,"DivMod", pinned, 1, sizeof(struct irnode **));
  op_Div   = new_ir_op (iro_Div,   "Div",    pinned, 1, sizeof(struct irnode **));
  op_Mod   = new_ir_op (iro_Mod,   "Mod",    pinned, 1, sizeof(struct irnode **));
  op_Abs   = new_ir_op (iro_Abs,   "Abs",    floats, 0, 0);
  op_And   = new_ir_op (iro_And,   "And",    floats, 0, 0);
  op_Or    = new_ir_op (iro_Or,    "Or",     floats, 0, 0);
  op_Eor   = new_ir_op (iro_Eor,   "Eor",    floats, 0, 0);
  op_Not   = new_ir_op (iro_Not,   "Not",    floats, 0, 0);
  op_Cmp   = new_ir_op (iro_Cmp,   "Cmp",    floats, 1, 0);
  op_Shl   = new_ir_op (iro_Shl,   "Shl",    floats, 1, 0);
  op_Shr   = new_ir_op (iro_Shr,   "Shr",    floats, 1, 0);
  op_Shrs  = new_ir_op (iro_Shrs,  "Shrs",   floats, 1, 0);
  op_Rot   = new_ir_op (iro_Rot,   "Rot",    floats, 1, 0);
  op_Conv  = new_ir_op (iro_Conv,  "Conv",   floats, 0, 0);

  op_Phi   = new_ir_op (iro_Phi,   "Phi",    pinned, 1, sizeof (int));

  op_Load  = new_ir_op (iro_Load,  "Load",   pinned, 1, sizeof(struct irnode **));
  op_Store = new_ir_op (iro_Store, "Store",  pinned, 1, sizeof(struct irnode **));
  op_Alloc = new_ir_op (iro_Alloc, "Alloc",  pinned, 1, sizeof (alloc_attr));
  op_Free  = new_ir_op (iro_Free,  "Free",   pinned, 1, sizeof (type *));
  op_Sync  = new_ir_op (iro_Sync,  "Sync",   pinned, 0, 0);

  op_Proj  = new_ir_op (iro_Proj,  "Proj",   floats, 0, sizeof (long));
  op_Tuple = new_ir_op (iro_Tuple, "Tuple",  floats, 1, 0);
  op_Id    = new_ir_op (iro_Id,    "Id",     floats, 0, 0);
  op_Bad   = new_ir_op (iro_Bad,   "Bad",    floats, 0, 0);

  op_Unknown   = new_ir_op (iro_Unknown,   "Unknown",   floats, 0, 0);
  op_Filter    = new_ir_op (iro_Filter,    "Filter",    pinned, 0, sizeof(filter_attr));
  op_Break     = new_ir_op (iro_Break,     "Break",     pinned, 0, 0);
  op_CallBegin = new_ir_op (iro_CallBegin, "CallBegin", pinned, 0, sizeof(callbegin_attr));
  op_EndReg    = new_ir_op (iro_EndReg,    "EndReg",    pinned, 0, sizeof(end_attr));
  op_EndExcept = new_ir_op (iro_EndExcept, "EndExcept", pinned, 0, sizeof(end_attr));
}

/* Returns the string for the opcode. */
const char  *get_op_name      (ir_op *op) {
  return id_to_str(op->name);
}

opcode get_op_code (ir_op *op){
  return op->code;
}

ident *get_op_ident(ir_op *op){
  return op->name;
}

op_pinned get_op_pinned (ir_op *op){
  return op->pinned;
}

/* Sets pinned in the opcode.  Setting it to floating has no effect
   for Phi, Block and control flow nodes. */
void      set_op_pinned(ir_op *op, op_pinned pinned) {
  if (op == op_Block || op == op_Phi || is_cfopcode(op)) return;
  op->pinned = pinned;
}


/* returns the attribute size of the operator. */
int get_op_attr_size (ir_op *op) {
  return op->attr_size;
}

int is_cfopcode(ir_op *op) {
  return ((op == op_Start)
          || (op == op_Jmp)
          || (op == op_Cond)
          || (op == op_Return)
          || (op == op_Raise)
          || (op == op_Bad)
	  || (op == op_End)
          || (op == op_Unknown)
          || (op == op_Break)
	  || (op == op_CallBegin)
	  || (op == op_EndReg)
	  || (op == op_EndExcept));
}

/* Returns true if the operation manipulates interprocedural control flow:
   CallBegin, EndReg, EndExcept */
int is_ip_cfopcode(ir_op *op) {
  return ((op == op_CallBegin)
	  || (op == op_EndReg)
	  || (op == op_EndExcept));

}
