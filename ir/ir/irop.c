/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

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


ir_op *
new_ir_op (opcode code, ident *name, size_t attr_size, int labeled)
{
  ir_op *res;

  res = (ir_op *) xmalloc (sizeof (ir_op));
  res->code = code;
  res->name = name;
  res->attr_size = attr_size;
  res->labeled = labeled;   /* For vcg dumping.
                               Set labeled = 1 if the edges shuld be
			       enumarated, otherwise set labeled = 0. */
  return res;
}


void
init_op(void)
{
  op_Block = new_ir_op (iro_Block, id_from_str ("Block", 5), sizeof (block_attr), 1);

  op_Start = new_ir_op (iro_Start, id_from_str ("Start", 5), sizeof (block_attr), 1);
  op_End = new_ir_op (iro_End, id_from_str ("End", 3), sizeof (block_attr), 1);
  op_Jmp    = new_ir_op (iro_Jmp, id_from_str ("Jmp", 3), 0, 0);
  op_Cond   = new_ir_op (iro_Cond, id_from_str ("Cond", 4), 0, 1);
  op_Return = new_ir_op (iro_Return, id_from_str ("Return", 6), 0, 1);
  op_Raise  = new_ir_op (iro_Raise, id_from_str ("Raise", 5), 0, 1);

  op_Const = new_ir_op (iro_Const, id_from_str ("Const", 5), sizeof (struct tarval *), 0);
  op_SymConst = new_ir_op (iro_SymConst, id_from_str ("SymConst", 8),
			   sizeof (symconst_attr), 0);

  op_Sel = new_ir_op (iro_Sel, id_from_str ("Sel", 3), sizeof (sel_attr), 1);

  op_Call = new_ir_op (iro_Call, id_from_str ("Call", 4), sizeof (type_method *), 1);
  op_Add = new_ir_op (iro_Add, id_from_str ("Add", 3), 0, 0);
  op_Minus = new_ir_op (iro_Minus, id_from_str ("Minus", 5), 0, 0);
  op_Sub = new_ir_op (iro_Sub, id_from_str ("Sub", 3), 0, 1);
  op_Mul = new_ir_op (iro_Mul, id_from_str ("Mul", 3), 0, 0);
  op_Quot = new_ir_op (iro_Quot, id_from_str ("Quot", 4), 0, 1);
  op_DivMod = new_ir_op (iro_DivMod, id_from_str ("DivMod", 6), 0, 1);
  op_Div = new_ir_op (iro_Div, id_from_str ("Div", 3), 0, 1);
  op_Mod = new_ir_op (iro_Mod, id_from_str ("Mod", 3), 0, 1);
  op_Abs = new_ir_op (iro_Abs, id_from_str ("Abs", 3), 0, 0);
  op_And = new_ir_op (iro_And, id_from_str ("And", 3), 0, 0);
  op_Or  = new_ir_op (iro_Or,  id_from_str ("Or", 2), 0, 0);
  op_Eor = new_ir_op (iro_Eor, id_from_str ("Eor", 3), 0, 0);
  op_Not = new_ir_op (iro_Not, id_from_str ("Not", 3), 0, 0);
  op_Cmp = new_ir_op (iro_Cmp, id_from_str ("Cmp", 3), 0, 1);
  op_Shl = new_ir_op (iro_Shl, id_from_str ("Shl", 3), 0, 1);
  op_Shr = new_ir_op (iro_Shr, id_from_str ("Shr", 3), 0, 1);
  op_Shrs  = new_ir_op (iro_Shrs, id_from_str ("Shrs", 3), 0, 0);
  op_Rot   = new_ir_op (iro_Rot, id_from_str ("Rot", 3), 0, 0);
  op_Conv  = new_ir_op (iro_Conv, id_from_str ("Conv", 4), 0, 1);

  op_Phi   = new_ir_op (iro_Phi,   id_from_str ("Phi", 3),   sizeof (int), 1);

  op_Load  = new_ir_op (iro_Load,  id_from_str ("Load", 4),  0, 1);
  op_Store = new_ir_op (iro_Store, id_from_str ("Store", 5), 0, 1);
  op_Alloc = new_ir_op (iro_Alloc, id_from_str ("Alloc", 5), sizeof (alloc_attr), 1);
  op_Free  = new_ir_op (iro_Free,  id_from_str ("Free", 4),  sizeof (type *),     1);
  op_Sync  = new_ir_op (iro_Sync,  id_from_str ("Sync", 4),  0, 0);

  op_Proj  = new_ir_op (iro_Proj,  id_from_str ("Proj", 4), sizeof (long), 1);
  op_Tuple = new_ir_op (iro_Tuple, id_from_str ("Tuple", 5), 0, 1);
  op_Id    = new_ir_op (iro_Id,    id_from_str ("Id", 2), 0, 0);
  op_Bad   = new_ir_op (iro_Bad,   id_from_str ("Bad", 3), 0, 0);
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

/* returns the attribute size of the operator. */
int get_op_attr_size (ir_op *op) {
  return op->attr_size;
}
