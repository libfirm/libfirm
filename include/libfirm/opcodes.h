
/* Warning: automatically generated code */
#ifndef FIRM_IR_OPCODES_H
#define FIRM_IR_OPCODES_H

/** The opcodes of the libFirm predefined operations. */
typedef enum ir_opcode {
	iro_ASM,
	iro_Abs,
	iro_Add,
	iro_Alloc,
	iro_Anchor,
	iro_And,
	iro_Bad,
	iro_Block,
	iro_Borrow,
	iro_Bound,
	iro_Builtin,
	iro_Call,
	iro_Carry,
	iro_Cast,
	iro_Cmp,
	iro_Cond,
	iro_Confirm,
	iro_Const,
	iro_Conv,
	iro_CopyB,
	iro_Div,
	iro_DivMod,
	iro_Dummy,
	iro_End,
	iro_Eor,
	iro_Free,
	iro_IJmp,
	iro_Id,
	iro_InstOf,
	iro_Jmp,
	iro_Load,
	iro_Minus,
	iro_Mod,
	iro_Mul,
	iro_Mulh,
	iro_Mux,
	iro_NoMem,
	iro_Not,
	iro_Or,
	iro_Phi,
	iro_Pin,
	iro_Proj,
	iro_Quot,
	iro_Raise,
	iro_Return,
	iro_Rotl,
	iro_Sel,
	iro_Shl,
	iro_Shr,
	iro_Shrs,
	iro_Start,
	iro_Store,
	iro_Sub,
	iro_SymConst,
	iro_Sync,
	iro_Tuple,
	iro_Unknown,
	iro_Unreachable,
	iro_First = iro_ASM,
	iro_Last = iro_Unreachable,

	beo_First,
	/* backend specific nodes */
	beo_Spill = beo_First,
	beo_Reload,
	beo_Perm,
	beo_MemPerm,
	beo_Copy,
	beo_Keep,
	beo_CopyKeep,
	beo_Call,
	beo_Return,
	beo_AddSP,
	beo_SubSP,
	beo_IncSP,
	beo_Start,
	beo_FrameAddr,
	beo_Barrier,
	/* last backend node number */
	beo_Last = beo_Barrier,
	iro_MaxOpcode
} ir_opcode;


FIRM_API ir_op *op_ASM;
FIRM_API ir_op *op_Abs;
FIRM_API ir_op *op_Add;
FIRM_API ir_op *op_Alloc;
FIRM_API ir_op *op_Anchor;
FIRM_API ir_op *op_And;
FIRM_API ir_op *op_Bad;
FIRM_API ir_op *op_Block;
FIRM_API ir_op *op_Borrow;
FIRM_API ir_op *op_Bound;
FIRM_API ir_op *op_Builtin;
FIRM_API ir_op *op_Call;
FIRM_API ir_op *op_Carry;
FIRM_API ir_op *op_Cast;
FIRM_API ir_op *op_Cmp;
FIRM_API ir_op *op_Cond;
FIRM_API ir_op *op_Confirm;
FIRM_API ir_op *op_Const;
FIRM_API ir_op *op_Conv;
FIRM_API ir_op *op_CopyB;
FIRM_API ir_op *op_Div;
FIRM_API ir_op *op_DivMod;
FIRM_API ir_op *op_Dummy;
FIRM_API ir_op *op_End;
FIRM_API ir_op *op_Eor;
FIRM_API ir_op *op_Free;
FIRM_API ir_op *op_IJmp;
FIRM_API ir_op *op_Id;
FIRM_API ir_op *op_InstOf;
FIRM_API ir_op *op_Jmp;
FIRM_API ir_op *op_Load;
FIRM_API ir_op *op_Minus;
FIRM_API ir_op *op_Mod;
FIRM_API ir_op *op_Mul;
FIRM_API ir_op *op_Mulh;
FIRM_API ir_op *op_Mux;
FIRM_API ir_op *op_NoMem;
FIRM_API ir_op *op_Not;
FIRM_API ir_op *op_Or;
FIRM_API ir_op *op_Phi;
FIRM_API ir_op *op_Pin;
FIRM_API ir_op *op_Proj;
FIRM_API ir_op *op_Quot;
FIRM_API ir_op *op_Raise;
FIRM_API ir_op *op_Return;
FIRM_API ir_op *op_Rotl;
FIRM_API ir_op *op_Sel;
FIRM_API ir_op *op_Shl;
FIRM_API ir_op *op_Shr;
FIRM_API ir_op *op_Shrs;
FIRM_API ir_op *op_Start;
FIRM_API ir_op *op_Store;
FIRM_API ir_op *op_Sub;
FIRM_API ir_op *op_SymConst;
FIRM_API ir_op *op_Sync;
FIRM_API ir_op *op_Tuple;
FIRM_API ir_op *op_Unknown;
FIRM_API ir_op *op_Unreachable;


FIRM_API ir_op *get_op_ASM(void);
FIRM_API ir_op *get_op_Abs(void);
FIRM_API ir_op *get_op_Add(void);
FIRM_API ir_op *get_op_Alloc(void);
FIRM_API ir_op *get_op_Anchor(void);
FIRM_API ir_op *get_op_And(void);
FIRM_API ir_op *get_op_Bad(void);
FIRM_API ir_op *get_op_Block(void);
FIRM_API ir_op *get_op_Borrow(void);
FIRM_API ir_op *get_op_Bound(void);
FIRM_API ir_op *get_op_Builtin(void);
FIRM_API ir_op *get_op_Call(void);
FIRM_API ir_op *get_op_Carry(void);
FIRM_API ir_op *get_op_Cast(void);
FIRM_API ir_op *get_op_Cmp(void);
FIRM_API ir_op *get_op_Cond(void);
FIRM_API ir_op *get_op_Confirm(void);
FIRM_API ir_op *get_op_Const(void);
FIRM_API ir_op *get_op_Conv(void);
FIRM_API ir_op *get_op_CopyB(void);
FIRM_API ir_op *get_op_Div(void);
FIRM_API ir_op *get_op_DivMod(void);
FIRM_API ir_op *get_op_Dummy(void);
FIRM_API ir_op *get_op_End(void);
FIRM_API ir_op *get_op_Eor(void);
FIRM_API ir_op *get_op_Free(void);
FIRM_API ir_op *get_op_IJmp(void);
FIRM_API ir_op *get_op_Id(void);
FIRM_API ir_op *get_op_InstOf(void);
FIRM_API ir_op *get_op_Jmp(void);
FIRM_API ir_op *get_op_Load(void);
FIRM_API ir_op *get_op_Minus(void);
FIRM_API ir_op *get_op_Mod(void);
FIRM_API ir_op *get_op_Mul(void);
FIRM_API ir_op *get_op_Mulh(void);
FIRM_API ir_op *get_op_Mux(void);
FIRM_API ir_op *get_op_NoMem(void);
FIRM_API ir_op *get_op_Not(void);
FIRM_API ir_op *get_op_Or(void);
FIRM_API ir_op *get_op_Phi(void);
FIRM_API ir_op *get_op_Pin(void);
FIRM_API ir_op *get_op_Proj(void);
FIRM_API ir_op *get_op_Quot(void);
FIRM_API ir_op *get_op_Raise(void);
FIRM_API ir_op *get_op_Return(void);
FIRM_API ir_op *get_op_Rotl(void);
FIRM_API ir_op *get_op_Sel(void);
FIRM_API ir_op *get_op_Shl(void);
FIRM_API ir_op *get_op_Shr(void);
FIRM_API ir_op *get_op_Shrs(void);
FIRM_API ir_op *get_op_Start(void);
FIRM_API ir_op *get_op_Store(void);
FIRM_API ir_op *get_op_Sub(void);
FIRM_API ir_op *get_op_SymConst(void);
FIRM_API ir_op *get_op_Sync(void);
FIRM_API ir_op *get_op_Tuple(void);
FIRM_API ir_op *get_op_Unknown(void);
FIRM_API ir_op *get_op_Unreachable(void);

#endif
