/*
 * Project:     libFIRM
 * File name:   ir/ir/irop.c
 * Purpose:     Representation of opcode of intermediate operation.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <string.h>

# include "irop_t.h"
# include "irnode_t.h"
# include "firmstat.h"

# include "iropt.h"             /* for firm_set_default_operations */

# include "xmalloc.h"

/** the available next opcode */
static unsigned next_iro = iro_MaxOpcode;

ir_op *op_Block;       ir_op *get_op_Block     (void) { return op_Block;     }

ir_op *op_Start;       ir_op *get_op_Start     (void) { return op_Start;     }
ir_op *op_End;         ir_op *get_op_End       (void) { return op_End;       }
ir_op *op_Jmp;         ir_op *get_op_Jmp       (void) { return op_Jmp;       }
ir_op *op_Cond;        ir_op *get_op_Cond      (void) { return op_Cond;      }
ir_op *op_Return;      ir_op *get_op_Return    (void) { return op_Return;    }
ir_op *op_Raise;       ir_op *get_op_Raise     (void) { return op_Raise;     }

ir_op *op_Sel;         ir_op *get_op_Sel       (void) { return op_Sel;       }
ir_op *op_InstOf;      ir_op *get_op_InstOf    (void) { return op_InstOf;    }

ir_op *op_Const;       ir_op *get_op_Const     (void) { return op_Const;     }
ir_op *op_SymConst;    ir_op *get_op_SymConst  (void) { return op_SymConst;  }

ir_op *op_Call;        ir_op *get_op_Call      (void) { return op_Call;      }
ir_op *op_Add;         ir_op *get_op_Add       (void) { return op_Add;       }
ir_op *op_Sub;         ir_op *get_op_Sub       (void) { return op_Sub;       }
ir_op *op_Minus;       ir_op *get_op_Minus     (void) { return op_Minus;     }
ir_op *op_Mul;         ir_op *get_op_Mul       (void) { return op_Mul;       }
ir_op *op_Quot;        ir_op *get_op_Quot      (void) { return op_Quot;      }
ir_op *op_DivMod;      ir_op *get_op_DivMod    (void) { return op_DivMod;    }
ir_op *op_Div;         ir_op *get_op_Div       (void) { return op_Div;       }
ir_op *op_Mod;         ir_op *get_op_Mod       (void) { return op_Mod;       }
ir_op *op_Abs;         ir_op *get_op_Abs       (void) { return op_Abs;       }
ir_op *op_And;         ir_op *get_op_And       (void) { return op_And;       }
ir_op *op_Or;          ir_op *get_op_Or        (void) { return op_Or;        }
ir_op *op_Eor;         ir_op *get_op_Eor       (void) { return op_Eor;       }
ir_op *op_Not;         ir_op *get_op_Not       (void) { return op_Not;       }
ir_op *op_Cmp;         ir_op *get_op_Cmp       (void) { return op_Cmp;       }
ir_op *op_Shl;         ir_op *get_op_Shl       (void) { return op_Shl;       }
ir_op *op_Shr;         ir_op *get_op_Shr       (void) { return op_Shr;       }
ir_op *op_Shrs;        ir_op *get_op_Shrs      (void) { return op_Shrs;      }
ir_op *op_Rot;         ir_op *get_op_Rot       (void) { return op_Rot;       }
ir_op *op_Conv;        ir_op *get_op_Conv      (void) { return op_Conv;      }
ir_op *op_Cast;        ir_op *get_op_Cast      (void) { return op_Cast;      }

ir_op *op_Phi;         ir_op *get_op_Phi       (void) { return op_Phi;       }

ir_op *op_Load;        ir_op *get_op_Load      (void) { return op_Load;      }
ir_op *op_Store;       ir_op *get_op_Store     (void) { return op_Store;     }
ir_op *op_Alloc;       ir_op *get_op_Alloc     (void) { return op_Alloc;     }
ir_op *op_Free;        ir_op *get_op_Free      (void) { return op_Free;      }
ir_op *op_Sync;        ir_op *get_op_Sync      (void) { return op_Sync;      }

ir_op *op_Tuple;       ir_op *get_op_Tuple     (void) { return op_Tuple;     }
ir_op *op_Proj;        ir_op *get_op_Proj      (void) { return op_Proj;      }
ir_op *op_Id;          ir_op *get_op_Id        (void) { return op_Id;        }
ir_op *op_Bad;         ir_op *get_op_Bad       (void) { return op_Bad;       }
ir_op *op_Confirm;     ir_op *get_op_Confirm   (void) { return op_Confirm;   }

ir_op *op_Unknown;     ir_op *get_op_Unknown   (void) { return op_Unknown;   }
ir_op *op_Filter;      ir_op *get_op_Filter    (void) { return op_Filter;    }
ir_op *op_Break;       ir_op *get_op_Break     (void) { return op_Break;     }
ir_op *op_CallBegin;   ir_op *get_op_CallBegin (void) { return op_CallBegin; }
ir_op *op_EndReg;      ir_op *get_op_EndReg    (void) { return op_EndReg;    }
ir_op *op_EndExcept;   ir_op *get_op_EndExcept (void) { return op_EndExcept; }

ir_op *op_NoMem;       ir_op *get_op_NoMem     (void) { return op_NoMem; }


ir_op *
new_ir_op(opcode code, const char *name, op_pin_state p, unsigned flags, op_arity opar, int op_index, size_t attr_size)
{
  ir_op *res;

  res = (ir_op *)xmalloc(sizeof(*res));
  memset(res, 0, sizeof(*res));

  res->code      = code;
  res->name      = new_id_from_chars(name, strlen(name));
  res->op_pin_state_pinned = p;
  res->attr_size = attr_size;
  res->flags     = flags;
  res->opar      = opar;
  res->op_index  = op_index;

  firm_set_default_operations(res);
  stat_new_ir_op(res);
  return res;
}

void free_ir_op(ir_op *code) {
  stat_free_ir_op(code);
  free(code);
}

void
init_op(void)
{
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking

  op_Block     = new_ir_op(iro_Block,     "Block",     op_pin_state_pinned, L,       oparity_variable, -1, sizeof(block_attr));

  op_Start     = new_ir_op(iro_Start,     "Start",     op_pin_state_pinned, X,       oparity_zero,     -1, sizeof(start_attr));
  op_End       = new_ir_op(iro_End,       "End",       op_pin_state_pinned, X,       oparity_dynamic,  -1, 0);
  op_Jmp       = new_ir_op(iro_Jmp,       "Jmp",       op_pin_state_pinned, X,       oparity_zero,     -1, 0);
  op_Cond      = new_ir_op(iro_Cond,      "Cond",      op_pin_state_pinned, L|X|Y,   oparity_any,      -1, sizeof(cond_attr));
  op_Return    = new_ir_op(iro_Return,    "Return",    op_pin_state_pinned, L|X,     oparity_zero,     -1, 0);
  op_Raise     = new_ir_op(iro_Raise,     "Raise",     op_pin_state_pinned, L|X,     oparity_any,      -1, 0);

  op_Const     = new_ir_op(iro_Const,     "Const",     op_pin_state_floats, 0,       oparity_zero,     -1, sizeof(const_attr));
  op_SymConst  = new_ir_op(iro_SymConst,  "SymConst",  op_pin_state_floats, 0,       oparity_zero,     -1, sizeof(symconst_attr));

  op_Sel       = new_ir_op(iro_Sel,       "Sel",       op_pin_state_floats, L,       oparity_any,      -1, sizeof(sel_attr));
  op_InstOf    = new_ir_op(iro_InstOf,    "InstOf",    op_pin_state_floats, L,       oparity_any,      -1, sizeof(sel_attr));

  op_Call      = new_ir_op(iro_Call,      "Call",      op_pin_state_mem_pinned, L|F, oparity_variable, -1, sizeof(call_attr));
  op_Add       = new_ir_op(iro_Add,       "Add",       op_pin_state_floats, C,       oparity_binary,    0, 0);
  op_Minus     = new_ir_op(iro_Minus,     "Minus",     op_pin_state_floats, 0,       oparity_unary,     0, 0);
  op_Sub       = new_ir_op(iro_Sub,       "Sub",       op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Mul       = new_ir_op(iro_Mul,       "Mul",       op_pin_state_floats, C,       oparity_binary,    0, 0);
  op_Quot      = new_ir_op(iro_Quot,      "Quot",      op_pin_state_exc_pinned, L|F, oparity_binary,    1, sizeof(except_attr));
  op_DivMod    = new_ir_op(iro_DivMod,    "DivMod",    op_pin_state_exc_pinned, L|F, oparity_binary,    1, sizeof(except_attr));
  op_Div       = new_ir_op(iro_Div,       "Div",       op_pin_state_exc_pinned, L|F, oparity_binary,    1, sizeof(except_attr));
  op_Mod       = new_ir_op(iro_Mod,       "Mod",       op_pin_state_exc_pinned, L|F, oparity_binary,    1, sizeof(except_attr));
  op_Abs       = new_ir_op(iro_Abs,       "Abs",       op_pin_state_floats, 0,       oparity_unary,     0, 0);
  op_And       = new_ir_op(iro_And,       "And",       op_pin_state_floats, C,       oparity_binary,    0, 0);
  op_Or        = new_ir_op(iro_Or,        "Or",        op_pin_state_floats, C,       oparity_binary,    0, 0);
  op_Eor       = new_ir_op(iro_Eor,       "Eor",       op_pin_state_floats, C,       oparity_binary,    0, 0);
  op_Not       = new_ir_op(iro_Not,       "Not",       op_pin_state_floats, 0,       oparity_unary,     0, 0);
  op_Cmp       = new_ir_op(iro_Cmp,       "Cmp",       op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Shl       = new_ir_op(iro_Shl,       "Shl",       op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Shr       = new_ir_op(iro_Shr,       "Shr",       op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Shrs      = new_ir_op(iro_Shrs,      "Shrs",      op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Rot       = new_ir_op(iro_Rot,       "Rot",       op_pin_state_floats, L,       oparity_binary,    0, 0);
  op_Conv      = new_ir_op(iro_Conv,      "Conv",      op_pin_state_floats, 0,       oparity_unary,     0, 0);
  op_Cast      = new_ir_op(iro_Cast,      "Cast",      op_pin_state_floats, 0,       oparity_unary,     0, sizeof(cast_attr));

  op_Phi       = new_ir_op(iro_Phi,       "Phi",       op_pin_state_pinned, L,       oparity_variable, -1, sizeof(int));

  op_Load      = new_ir_op(iro_Load,      "Load",      op_pin_state_exc_pinned, L|F, oparity_any,      -1, sizeof(load_attr));
  op_Store     = new_ir_op(iro_Store,     "Store",     op_pin_state_exc_pinned, L|F, oparity_any,      -1, sizeof(store_attr));
  op_Alloc     = new_ir_op(iro_Alloc,     "Alloc",     op_pin_state_pinned, L|F,     oparity_any,      -1, sizeof(alloc_attr));
  op_Free      = new_ir_op(iro_Free,      "Free",      op_pin_state_pinned, L,       oparity_any,      -1, sizeof(type *));
  op_Sync      = new_ir_op(iro_Sync,      "Sync",      op_pin_state_pinned, 0,       oparity_any,      -1, 0);

  op_Proj      = new_ir_op(iro_Proj,      "Proj",      op_pin_state_floats, 0,       oparity_any,      -1, sizeof(long));
  op_Tuple     = new_ir_op(iro_Tuple,     "Tuple",     op_pin_state_floats, L,       oparity_variable, -1, 0);
  op_Id        = new_ir_op(iro_Id,        "Id",        op_pin_state_floats, 0,       oparity_any,      -1, 0);
  op_Bad       = new_ir_op(iro_Bad,       "Bad",       op_pin_state_pinned, X|F,     oparity_zero,     -1, 0);
  op_Confirm   = new_ir_op(iro_Confirm,   "Confirm",   op_pin_state_floats, L,       oparity_any,      -1, sizeof(confirm_attr));

  op_Unknown   = new_ir_op(iro_Unknown,   "Unknown",   op_pin_state_pinned, X|F,     oparity_zero,     -1, 0);
  op_Filter    = new_ir_op(iro_Filter,    "Filter",    op_pin_state_pinned, L,       oparity_variable, -1, sizeof(filter_attr));
  op_Break     = new_ir_op(iro_Break,     "Break",     op_pin_state_pinned, X,       oparity_zero,     -1, 0);
  op_CallBegin = new_ir_op(iro_CallBegin, "CallBegin", op_pin_state_pinned, X|I,     oparity_any,      -1, sizeof(callbegin_attr));
  op_EndReg    = new_ir_op(iro_EndReg,    "EndReg",    op_pin_state_pinned, X|I,     oparity_any,      -1, sizeof(end_attr));
  op_EndExcept = new_ir_op(iro_EndExcept, "EndExcept", op_pin_state_pinned, X|I,     oparity_any,      -1, sizeof(end_attr));

  op_NoMem     = new_ir_op(iro_NoMem,     "NoMem",     op_pin_state_pinned, 0,       oparity_zero,     -1, 0);

#undef Y
#undef F
#undef I
#undef X
#undef C
#undef L
}


/* free memory used by irop module. */
void finish_op(void) {
  free_ir_op (op_Block    ); op_Block     = NULL;

  free_ir_op (op_Start    ); op_Start     = NULL;
  free_ir_op (op_End      ); op_End       = NULL;
  free_ir_op (op_Jmp      ); op_Jmp       = NULL;
  free_ir_op (op_Cond     ); op_Cond      = NULL;
  free_ir_op (op_Return   ); op_Return    = NULL;
  free_ir_op (op_Raise    ); op_Raise     = NULL;

  free_ir_op (op_Const    ); op_Const     = NULL;
  free_ir_op (op_SymConst ); op_SymConst  = NULL;

  free_ir_op (op_Sel      ); op_Sel       = NULL;
  free_ir_op (op_InstOf   ); op_InstOf    = NULL;

  free_ir_op (op_Call     ); op_Call      = NULL;
  free_ir_op (op_Add      ); op_Add       = NULL;
  free_ir_op (op_Minus    ); op_Minus     = NULL;
  free_ir_op (op_Sub      ); op_Sub       = NULL;
  free_ir_op (op_Mul      ); op_Mul       = NULL;
  free_ir_op (op_Quot     ); op_Quot      = NULL;
  free_ir_op (op_DivMod   ); op_DivMod    = NULL;
  free_ir_op (op_Div      ); op_Div       = NULL;
  free_ir_op (op_Mod      ); op_Mod       = NULL;
  free_ir_op (op_Abs      ); op_Abs       = NULL;
  free_ir_op (op_And      ); op_And       = NULL;
  free_ir_op (op_Or       ); op_Or        = NULL;
  free_ir_op (op_Eor      ); op_Eor       = NULL;
  free_ir_op (op_Not      ); op_Not       = NULL;
  free_ir_op (op_Cmp      ); op_Cmp       = NULL;
  free_ir_op (op_Shl      ); op_Shl       = NULL;
  free_ir_op (op_Shr      ); op_Shr       = NULL;
  free_ir_op (op_Shrs     ); op_Shrs      = NULL;
  free_ir_op (op_Rot      ); op_Rot       = NULL;
  free_ir_op (op_Conv     ); op_Conv      = NULL;
  free_ir_op (op_Cast     ); op_Cast      = NULL;

  free_ir_op (op_Phi      ); op_Phi       = NULL;

  free_ir_op (op_Load     ); op_Load      = NULL;
  free_ir_op (op_Store    ); op_Store     = NULL;
  free_ir_op (op_Alloc    ); op_Alloc     = NULL;
  free_ir_op (op_Free     ); op_Free      = NULL;
  free_ir_op (op_Sync     ); op_Sync      = NULL;

  free_ir_op (op_Proj     ); op_Proj      = NULL;
  free_ir_op (op_Tuple    ); op_Tuple     = NULL;
  free_ir_op (op_Id       ); op_Id        = NULL;
  free_ir_op (op_Bad      ); op_Bad       = NULL;
  free_ir_op (op_Confirm  ); op_Confirm   = NULL;

  free_ir_op (op_Unknown  ); op_Unknown   = NULL;
  free_ir_op (op_Filter   ); op_Filter    = NULL;
  free_ir_op (op_Break    ); op_Break     = NULL;
  free_ir_op (op_CallBegin); op_CallBegin = NULL;
  free_ir_op (op_EndReg   ); op_EndReg    = NULL;
  free_ir_op (op_EndExcept); op_EndExcept = NULL;

  free_ir_op (op_NoMem    ); op_NoMem     = NULL;
}

/* Returns the string for the opcode. */
const char *get_op_name (const ir_op *op) {
  return get_id_str(op->name);
}

opcode (get_op_code)(const ir_op *op){
  return __get_op_code(op);
}

ident *(get_op_ident)(ir_op *op){
  return __get_op_ident(op);
}

op_pin_state (get_op_pinned)(const ir_op *op){
  return __get_op_pinned(op);
}

/* Sets op_pin_state_pinned in the opcode.  Setting it to floating has no effect
   for Phi, Block and control flow nodes. */
void      set_op_pinned(ir_op *op, op_pin_state op_pin_state_pinned) {
  if (op == op_Block || op == op_Phi || is_cfopcode(op)) return;
  op->op_pin_state_pinned = op_pin_state_pinned;
}

unsigned get_next_ir_opcode(void) {
  return next_iro++;
}
