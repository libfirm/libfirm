/*
 * Project:     libFIRM
 * File name:   ir/ir/irop.h
 * Purpose:     Representation of opcode of intermediate operation.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file irop.h
*
* Operators of firm nodes.
*
* @author Christian Schaefer
*
*  This module specifies the opcodes possible for ir nodes.  Their
*  definition is close to the operations specified in UKA Tech-Report
*  1999-14
*/

# ifndef _IROP_H_
# define _IROP_H_

# include "ident.h"

/** The opcodes of the libFirm predefined operations. */
typedef enum {
  iro_Block,
  iro_Start, iro_End, iro_Jmp, iro_Cond, iro_Return, iro_Raise,
  iro_Const, iro_SymConst,
  iro_Sel,  iro_InstOf,
  iro_Call, iro_Add, iro_Sub, iro_Minus, iro_Mul, iro_Quot, iro_DivMod,
  iro_Div,  iro_Mod, iro_Abs, iro_And, iro_Or, iro_Eor, iro_Not,
  iro_Cmp,  iro_Shl, iro_Shr, iro_Shrs, iro_Rot, iro_Conv, iro_Cast,
  iro_Phi,
  iro_Load, iro_Store, iro_Alloc, iro_Free, iro_Sync,
  iro_Proj, iro_Tuple, iro_Id, iro_Bad, iro_Confirm,
  iro_Unknown, iro_Filter, iro_Break, iro_CallBegin, iro_EndReg, iro_EndExcept,
  iro_NoMem,
  iro_MaxOpcode
} opcode;

typedef struct ir_op ir_op;

extern ir_op *op_Block;           ir_op *get_op_Block     (void);

extern ir_op *op_Start;           ir_op *get_op_Start     (void);
extern ir_op *op_End;             ir_op *get_op_End       (void);
extern ir_op *op_Jmp;             ir_op *get_op_Jmp       (void);
extern ir_op *op_Cond;            ir_op *get_op_Cond      (void);
extern ir_op *op_Return;          ir_op *get_op_Return    (void);
extern ir_op *op_Raise;           ir_op *get_op_Raise     (void);
extern ir_op *op_Sel;             ir_op *get_op_Sel       (void);
extern ir_op *op_InstOf;          ir_op *get_op_InstOf    (void);

extern ir_op *op_Const;           ir_op *get_op_Const     (void);
extern ir_op *op_SymConst;        ir_op *get_op_SymConst  (void);

extern ir_op *op_Call;            ir_op *get_op_Call      (void);
extern ir_op *op_Add;             ir_op *get_op_Add       (void);
extern ir_op *op_Sub;             ir_op *get_op_Sub       (void);
extern ir_op *op_Minus;           ir_op *get_op_Minus     (void);
extern ir_op *op_Mul;             ir_op *get_op_Mul       (void);
extern ir_op *op_Quot;            ir_op *get_op_Quot      (void);
extern ir_op *op_DivMod;          ir_op *get_op_DivMod    (void);
extern ir_op *op_Div;             ir_op *get_op_Div       (void);
extern ir_op *op_Mod;             ir_op *get_op_Mod       (void);
extern ir_op *op_Abs;             ir_op *get_op_Abs       (void);
extern ir_op *op_And;             ir_op *get_op_And       (void);
extern ir_op *op_Or;              ir_op *get_op_Or        (void);
extern ir_op *op_Eor;             ir_op *get_op_Eor       (void);
extern ir_op *op_Not;             ir_op *get_op_Not       (void);
extern ir_op *op_Cmp;             ir_op *get_op_Cmp       (void);
extern ir_op *op_Shl;             ir_op *get_op_Shl       (void);
extern ir_op *op_Shr;             ir_op *get_op_Shr       (void);
extern ir_op *op_Shrs;            ir_op *get_op_Shrs      (void);
extern ir_op *op_Rot;             ir_op *get_op_Rot       (void);
extern ir_op *op_Conv;            ir_op *get_op_Conv      (void);
extern ir_op *op_Cast;            ir_op *get_op_Cast      (void);

extern ir_op *op_Phi;             ir_op *get_op_Phi       (void);

extern ir_op *op_Load;            ir_op *get_op_Load      (void);
extern ir_op *op_Store;           ir_op *get_op_Store     (void);
extern ir_op *op_Alloc;           ir_op *get_op_Alloc     (void);
extern ir_op *op_Free;            ir_op *get_op_Free      (void);

extern ir_op *op_Sync;            ir_op *get_op_Sync      (void);

extern ir_op *op_Tuple;           ir_op *get_op_Tuple     (void);
extern ir_op *op_Proj;            ir_op *get_op_Proj      (void);
extern ir_op *op_Id;              ir_op *get_op_Id        (void);
extern ir_op *op_Bad;             ir_op *get_op_Bad       (void);
extern ir_op *op_Confirm;         ir_op *get_op_Confirm   (void);

extern ir_op *op_Unknown;         ir_op *get_op_Unknown   (void);
extern ir_op *op_Filter;          ir_op *get_op_Filter    (void);
extern ir_op *op_Break;           ir_op *get_op_Break     (void);
extern ir_op *op_CallBegin;       ir_op *get_op_CallBegin (void);
extern ir_op *op_EndReg;          ir_op *get_op_EndReg    (void);
extern ir_op *op_EndExcept;       ir_op *get_op_EndExcept (void);

extern ir_op *op_NoMem;           ir_op *get_op_NoMem     (void);

/** Returns the ident for the opcode name */
ident *get_op_ident(ir_op *op);

/** Returns the string for the opcode. */
const char *get_op_name(const ir_op *op);

/** Returns the enum for the opcode */
opcode get_op_code(const ir_op *op);

/** op_pin_state_pinned states */
typedef enum {
  op_pin_state_floats = 0,    /**< Nodes of this opcode can be placed in any basic block. */
  op_pin_state_pinned,        /**< Nodes must remain in this basic block. */
  op_pin_state_exc_pinned,    /**< Node must be remain in this basic block if it can throw an
                                   exception, else can float. Used internally. */
  op_pin_state_mem_pinned     /**< Node must be remain in this basic block if it can throw an
                                   exception or uses memory, else can float. Used internally. */
} op_pin_state;

const char *get_op_pin_state_name(op_pin_state s);

/** gets pinned state of an opcode */
op_pin_state get_op_pinned(const ir_op *op);

/** Sets pinned in the opcode.  Setting it to floating has no effect
   for Block, Phi and control flow nodes. */
void set_op_pinned(ir_op *op, op_pin_state pinned);

/** Returns the next free IR opcode number, allows to register user ops */
unsigned get_next_ir_opcode(void);

# endif /* _IROP_H_ */
