/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

#include "irop_t.h"
#include "irnode_t.h"
#include "irhooks.h"
#include "irbackedge_t.h"

#include "iropt_t.h"             /* for firm_set_default_operations */
#include "irvrfy_t.h"
#include "reassoc_t.h"

#include "xmalloc.h"

/** the available next opcode */
static unsigned next_iro = iro_MaxOpcode;

ir_op *op_Block;       ir_op *get_op_Block     (void) { return op_Block;     }

ir_op *op_Start;       ir_op *get_op_Start     (void) { return op_Start;     }
ir_op *op_End;         ir_op *get_op_End       (void) { return op_End;       }
ir_op *op_Jmp;         ir_op *get_op_Jmp       (void) { return op_Jmp;       }
ir_op *op_IJmp;        ir_op *get_op_IJmp      (void) { return op_IJmp;      }
ir_op *op_Cond;        ir_op *get_op_Cond      (void) { return op_Cond;      }
ir_op *op_Return;      ir_op *get_op_Return    (void) { return op_Return;    }

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
ir_op *op_Carry;       ir_op *get_op_Carry     (void) { return op_Carry;     }
ir_op *op_Borrow;      ir_op *get_op_Borrow    (void) { return op_Borrow;    }

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

ir_op *op_NoMem;       ir_op *get_op_NoMem     (void) { return op_NoMem;     }
ir_op *op_Mux;         ir_op *get_op_Mux       (void) { return op_Mux;       }
ir_op *op_Psi;         ir_op *get_op_Psi       (void) { return op_Psi;       }
ir_op *op_CopyB;       ir_op *get_op_CopyB     (void) { return op_CopyB;     }

ir_op *op_Raise;       ir_op *get_op_Raise     (void) { return op_Raise;     }
ir_op *op_Bound;       ir_op *get_op_Bound     (void) { return op_Bound;     }

ir_op *op_Pin;         ir_op *get_op_Pin       (void) { return op_Pin;       }

/*
 * Copies all attributes stored in the old node to the new node.
 * Assumes both have the same opcode and sufficient size.
 */
void default_copy_attr(const ir_node *old_node, ir_node *new_node) {
  unsigned size = firm_add_node_size;

  assert(get_irn_op(old_node) == get_irn_op(new_node));
  memcpy(&new_node->attr, &old_node->attr, get_op_attr_size(get_irn_op(old_node)));

  if (size > 0) {
    /* copy additional node data */
    memcpy(get_irn_data(new_node, void, size), get_irn_data(old_node, void, size), size);
  }
}  /* default_copy_attr */

/**
 * Copies all Call attributes stored in the old node to the new node.
 */
static void
call_copy_attr(const ir_node *old_node, ir_node *new_node) {
  default_copy_attr(old_node, new_node);
  remove_Call_callee_arr(new_node);
}  /* call_copy_attr */

/**
 * Copies all Block attributes stored in the old node to the new node.
 */
static void
block_copy_attr(const ir_node *old_node, ir_node *new_node) {
  ir_graph *irg = current_ir_graph;

  default_copy_attr(old_node, new_node);
  new_node->attr.block.cg_backedge = NULL;
  new_node->attr.block.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
  INIT_LIST_HEAD(&new_node->attr.block.succ_head);
}  /* block_copy_attr */

/**
 * Copies all phi attributes stored in old node to the new node
 */
static void
phi_copy_attr(const ir_node *old_node, ir_node *new_node) {
  ir_graph *irg = current_ir_graph;

  default_copy_attr(old_node, new_node);
  new_node->attr.phi_backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
}

/**
 * Copies all filter attributes stored in old node to the new node
 */
static void
filter_copy_attr(const ir_node *old_node, ir_node *new_node) {
  ir_graph *irg = current_ir_graph;

  default_copy_attr(old_node, new_node);
  new_node->attr.filter.backedge = new_backedge_arr(irg->obst, get_irn_arity(new_node));
}

/**
 * Sets the default copy_attr operation for an ir_ops
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
static ir_op_ops *firm_set_default_copy_attr(ir_opcode code, ir_op_ops *ops) {
  if (code == iro_Call)
    ops->copy_attr = call_copy_attr;
  else if (code == iro_Block)
    ops->copy_attr = block_copy_attr;
  else if (code == iro_Phi)
    ops->copy_attr = phi_copy_attr;
  else if (code == iro_Filter)
    ops->copy_attr = filter_copy_attr;
  else {
    /* not allowed to be NULL */
    if (! ops->copy_attr)
      ops->copy_attr = default_copy_attr;
  }
  return ops;
}  /* firm_set_default_copy_attr */

/* Creates a new ir operation. */
ir_op *
new_ir_op(ir_opcode code, const char *name, op_pin_state p,
          unsigned flags, op_arity opar, int op_index, size_t attr_size,
          const ir_op_ops *ops)
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
  res->tag       = NULL;

  if (ops)
    memcpy(&res->ops, ops, sizeof(res->ops));
  else /* no given ops, set all operations to NULL */
    memset(&res->ops, 0, sizeof(res->ops));

  firm_set_default_operations(code, &res->ops);
  firm_set_default_copy_attr(code, &res->ops);
  firm_set_default_verifyer(code, &res->ops);
  firm_set_default_reassoc(code, &res->ops);

  add_irp_opcode(res);

  hook_new_ir_op(res);
  return res;
}  /* new_ir_op */

void free_ir_op(ir_op *code) {
  hook_free_ir_op(code);

  remove_irp_opcode(code);
  free(code);
}  /* free_ir_op */

void
init_op(void)
{
#define N   irop_flag_none
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking
#define H   irop_flag_highlevel
#define c   irop_flag_constlike
#define K   irop_flag_keep
#define S   irop_flag_start_block

  /* Caution: A great deal of Firm optimizations depend an right operations flags. */
  op_Block     = new_ir_op(iro_Block,     "Block",     op_pin_state_pinned, L,       oparity_variable, -1, sizeof(block_attr), NULL);

  op_Start     = new_ir_op(iro_Start,     "Start",     op_pin_state_pinned, X,       oparity_zero,     -1, 0, NULL);
  op_End       = new_ir_op(iro_End,       "End",       op_pin_state_pinned, X,       oparity_dynamic,  -1, 0, NULL);
  op_Jmp       = new_ir_op(iro_Jmp,       "Jmp",       op_pin_state_pinned, X,       oparity_zero,     -1, 0, NULL);
  op_IJmp      = new_ir_op(iro_IJmp,      "IJmp",      op_pin_state_pinned, X|K,     oparity_unary,    -1, 0, NULL);
  op_Cond      = new_ir_op(iro_Cond,      "Cond",      op_pin_state_pinned, X|Y,     oparity_any,      -1, sizeof(cond_attr), NULL);
  op_Return    = new_ir_op(iro_Return,    "Return",    op_pin_state_pinned, X,       oparity_variable, -1, 0, NULL);

  op_Const     = new_ir_op(iro_Const,     "Const",     op_pin_state_floats, c|S,     oparity_zero,     -1, sizeof(const_attr), NULL);
  op_SymConst  = new_ir_op(iro_SymConst,  "SymConst",  op_pin_state_floats, c|S,     oparity_zero,     -1, sizeof(symconst_attr), NULL);

  op_Sel       = new_ir_op(iro_Sel,       "Sel",       op_pin_state_floats, H,       oparity_any,      -1, sizeof(sel_attr), NULL);

  op_Call      = new_ir_op(iro_Call,      "Call",      op_pin_state_mem_pinned, F,   oparity_variable, -1, sizeof(call_attr), NULL);
  op_Add       = new_ir_op(iro_Add,       "Add",       op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Minus     = new_ir_op(iro_Minus,     "Minus",     op_pin_state_floats, N,       oparity_unary,     0, 0, NULL);
  op_Sub       = new_ir_op(iro_Sub,       "Sub",       op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Mul       = new_ir_op(iro_Mul,       "Mul",       op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Quot      = new_ir_op(iro_Quot,      "Quot",      op_pin_state_exc_pinned, F,   oparity_binary,    1, sizeof(except_attr), NULL);
  op_DivMod    = new_ir_op(iro_DivMod,    "DivMod",    op_pin_state_exc_pinned, F,   oparity_binary,    1, sizeof(except_attr), NULL);
  op_Div       = new_ir_op(iro_Div,       "Div",       op_pin_state_exc_pinned, F,   oparity_binary,    1, sizeof(except_attr), NULL);
  op_Mod       = new_ir_op(iro_Mod,       "Mod",       op_pin_state_exc_pinned, F,   oparity_binary,    1, sizeof(except_attr), NULL);
  op_Abs       = new_ir_op(iro_Abs,       "Abs",       op_pin_state_floats, N,       oparity_unary,     0, 0, NULL);
  op_And       = new_ir_op(iro_And,       "And",       op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Or        = new_ir_op(iro_Or,        "Or",        op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Eor       = new_ir_op(iro_Eor,       "Eor",       op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Not       = new_ir_op(iro_Not,       "Not",       op_pin_state_floats, N,       oparity_unary,     0, 0, NULL);
  op_Cmp       = new_ir_op(iro_Cmp,       "Cmp",       op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Shl       = new_ir_op(iro_Shl,       "Shl",       op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Shr       = new_ir_op(iro_Shr,       "Shr",       op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Shrs      = new_ir_op(iro_Shrs,      "Shrs",      op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Rot       = new_ir_op(iro_Rot,       "Rot",       op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);
  op_Conv      = new_ir_op(iro_Conv,      "Conv",      op_pin_state_floats, N,       oparity_unary,     0, sizeof(conv_attr), NULL);
  op_Cast      = new_ir_op(iro_Cast,      "Cast",      op_pin_state_floats, N|H,     oparity_unary,     0, sizeof(cast_attr), NULL);
  op_Carry     = new_ir_op(iro_Carry,     "Carry",     op_pin_state_floats, C,       oparity_binary,    0, 0, NULL);
  op_Borrow    = new_ir_op(iro_Borrow,    "Borrow",    op_pin_state_floats, N,       oparity_binary,    0, 0, NULL);

  op_Phi       = new_ir_op(iro_Phi,       "Phi",       op_pin_state_pinned, N,       oparity_variable, -1, sizeof(int), NULL);

  op_Load      = new_ir_op(iro_Load,      "Load",      op_pin_state_exc_pinned, F,   oparity_any,      -1, sizeof(load_attr), NULL);
  op_Store     = new_ir_op(iro_Store,     "Store",     op_pin_state_exc_pinned, F,   oparity_any,      -1, sizeof(store_attr), NULL);
  op_Alloc     = new_ir_op(iro_Alloc,     "Alloc",     op_pin_state_pinned, F,       oparity_any,      -1, sizeof(alloc_attr), NULL);
  op_Free      = new_ir_op(iro_Free,      "Free",      op_pin_state_pinned, N,       oparity_any,      -1, sizeof(free_attr), NULL);
  op_Sync      = new_ir_op(iro_Sync,      "Sync",      op_pin_state_pinned, N,       oparity_dynamic,  -1, 0, NULL);

  op_Proj      = new_ir_op(iro_Proj,      "Proj",      op_pin_state_floats, N,       oparity_unary,    -1, sizeof(long), NULL);
  op_Tuple     = new_ir_op(iro_Tuple,     "Tuple",     op_pin_state_floats, L,       oparity_variable, -1, 0, NULL);
  op_Id        = new_ir_op(iro_Id,        "Id",        op_pin_state_floats, N,       oparity_any,      -1, 0, NULL);
  op_Bad       = new_ir_op(iro_Bad,       "Bad",       op_pin_state_pinned, X|F|S,   oparity_zero,     -1, 0, NULL);
  op_Confirm   = new_ir_op(iro_Confirm,   "Confirm",   op_pin_state_pinned, H,       oparity_any,      -1, sizeof(confirm_attr), NULL);

  op_Unknown   = new_ir_op(iro_Unknown,   "Unknown",   op_pin_state_pinned, X|F|S,   oparity_zero,     -1, 0, NULL);
  op_Filter    = new_ir_op(iro_Filter,    "Filter",    op_pin_state_pinned, N,       oparity_variable, -1, sizeof(filter_attr), NULL);
  op_Break     = new_ir_op(iro_Break,     "Break",     op_pin_state_pinned, X,       oparity_zero,     -1, 0, NULL);
  op_CallBegin = new_ir_op(iro_CallBegin, "CallBegin", op_pin_state_pinned, X|I,     oparity_any,      -1, sizeof(callbegin_attr), NULL);
  op_EndReg    = new_ir_op(iro_EndReg,    "EndReg",    op_pin_state_pinned, X|I,     oparity_dynamic,  -1, sizeof(end_attr), NULL);
  op_EndExcept = new_ir_op(iro_EndExcept, "EndExcept", op_pin_state_pinned, X|I,     oparity_dynamic,  -1, sizeof(end_attr), NULL);

  op_NoMem     = new_ir_op(iro_NoMem,     "NoMem",     op_pin_state_pinned, N,       oparity_zero,     -1, 0, NULL);
  op_Mux       = new_ir_op(iro_Mux,       "Mux",       op_pin_state_floats, N,       oparity_trinary,  -1, 0, NULL);
  op_Psi       = new_ir_op(iro_Psi,       "Psi",       op_pin_state_floats, N,       oparity_variable, -1, 0, NULL);
  op_CopyB     = new_ir_op(iro_CopyB,     "CopyB",     op_pin_state_mem_pinned, F|H, oparity_trinary,  -1, sizeof(copyb_attr), NULL);

  op_InstOf    = new_ir_op(iro_InstOf,    "InstOf",    op_pin_state_mem_pinned, H,   oparity_unary,    -1, sizeof(io_attr), NULL);
  op_Raise     = new_ir_op(iro_Raise,     "Raise",     op_pin_state_pinned,     H|X, oparity_any,      -1, 0, NULL);
  op_Bound     = new_ir_op(iro_Bound,     "Bound",     op_pin_state_exc_pinned, F|H, oparity_trinary,  -1, sizeof(bound_attr), NULL);

  op_Pin       = new_ir_op(iro_Pin,       "Pin",       op_pin_state_pinned, H,       oparity_unary,    -1, 0, NULL);

#undef S
#undef H
#undef Y
#undef F
#undef I
#undef X
#undef C
#undef L
}  /* init_op */


/* free memory used by irop module. */
void finish_op(void) {
  free_ir_op (op_Block    ); op_Block     = NULL;

  free_ir_op (op_Start    ); op_Start     = NULL;
  free_ir_op (op_End      ); op_End       = NULL;
  free_ir_op (op_Jmp      ); op_Jmp       = NULL;
  free_ir_op (op_Cond     ); op_Cond      = NULL;
  free_ir_op (op_Return   ); op_Return    = NULL;

  free_ir_op (op_Const    ); op_Const     = NULL;
  free_ir_op (op_SymConst ); op_SymConst  = NULL;

  free_ir_op (op_Sel      ); op_Sel       = NULL;

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
  free_ir_op (op_Carry    ); op_Carry     = NULL;
  free_ir_op (op_Borrow   ); op_Borrow    = NULL;

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
  free_ir_op (op_Psi      ); op_Psi       = NULL;
  free_ir_op (op_Mux      ); op_Mux       = NULL;
  free_ir_op (op_CopyB    ); op_CopyB     = NULL;

  free_ir_op (op_InstOf   ); op_InstOf    = NULL;
  free_ir_op (op_Raise    ); op_Raise     = NULL;
  free_ir_op (op_Bound    ); op_Bound     = NULL;

  free_ir_op (op_Pin      ); op_Pin       = NULL;
}

/* Returns the string for the opcode. */
const char *get_op_name (const ir_op *op) {
  return get_id_str(op->name);
}  /* get_op_name */

ir_opcode (get_op_code)(const ir_op *op){
  return _get_op_code(op);
}  /* get_op_code */

ident *(get_op_ident)(const ir_op *op){
  return _get_op_ident(op);
}  /* get_op_ident */

const char *get_op_pin_state_name(op_pin_state s) {
  switch(s) {
#define XXX(s) case s: return #s
  XXX(op_pin_state_floats);
  XXX(op_pin_state_pinned);
  XXX(op_pin_state_exc_pinned);
  XXX(op_pin_state_mem_pinned);
#undef XXX
  }
  return "<none>";
}  /* get_op_pin_state_name */

op_pin_state (get_op_pinned)(const ir_op *op) {
  return _get_op_pinned(op);
}  /* get_op_pinned */

/* Sets op_pin_state_pinned in the opcode.  Setting it to floating has no effect
   for Phi, Block and control flow nodes. */
void set_op_pinned(ir_op *op, op_pin_state op_pin_state_pinned) {
  if (op == op_Block || op == op_Phi || is_cfopcode(op)) return;
  op->op_pin_state_pinned = op_pin_state_pinned;
}  /* set_op_pinned */

/* retrieve the next free opcode */
unsigned get_next_ir_opcode(void) {
  return next_iro++;
}  /* get_next_ir_opcode */

/* Returns the next free n IR opcode number, allows to register a bunch of user ops */
unsigned get_next_ir_opcodes(unsigned num) {
  unsigned base = next_iro;
  next_iro += num;
  return base;
}  /* get_next_ir_opcodes */

/* Returns the generic function pointer from an ir operation. */
op_func (get_generic_function_ptr)(const ir_op *op) {
  return _get_generic_function_ptr(op);
}  /* get_generic_function_ptr */

/* Store a generic function pointer into an ir operation. */
void (set_generic_function_ptr)(ir_op *op, op_func func) {
  _set_generic_function_ptr(op, func);
}  /* set_generic_function_ptr */

/* Returns the ir_op_ops of an ir_op. */
const ir_op_ops *(get_op_ops)(const ir_op *op) {
  return _get_op_ops(op);
}  /* get_op_ops */
