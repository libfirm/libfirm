
/* Warning: automatically generated code */
#ifndef FIRM_IR_NODEOPS_H
#define FIRM_IR_NODEOPS_H

#include "firm_types.h"

/**
 * @addtogroup ir_node
 * @{
 */


/**
 * Projection numbers for result of Alloc node (use for Proj nodes)
 */
typedef enum {
	pn_Alloc_M = pn_Generic_M, /**< memory result */
	pn_Alloc_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Alloc_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Alloc_res = pn_Generic_other, /**< pointer to newly allocated memory */
	pn_Alloc_max
} pn_Alloc;

/**
 * Projection numbers for result of Bound node (use for Proj nodes)
 */
typedef enum {
	pn_Bound_M = pn_Generic_M, /**< memory result */
	pn_Bound_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Bound_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Bound_res = pn_Generic_other, /**< the checked index */
	pn_Bound_max
} pn_Bound;

/**
 * Projection numbers for result of Builtin node (use for Proj nodes)
 */
typedef enum {
	pn_Builtin_M = pn_Generic_M, /**< memory result */
	pn_Builtin_1_result = pn_Generic_other, /**< first result */
	pn_Builtin_max
} pn_Builtin;

/**
 * Projection numbers for result of Call node (use for Proj nodes)
 */
typedef enum {
	pn_Call_M = pn_Generic_M, /**< memory result */
	pn_Call_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Call_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Call_T_result = pn_Generic_other, /**< tuple containing all results */
	pn_Call_P_value_res_base, /**< pointer to memory register containing copied results passed by value */
	pn_Call_max
} pn_Call;

/**
 * Projection numbers for result of Cmp node (use for Proj nodes)
 */
typedef enum {
	pn_Cmp_False = 0, /**< always false */
	pn_Cmp_Eq = 1, /**< equal */
	pn_Cmp_Lt = 2, /**< less */
	pn_Cmp_Le = pn_Cmp_Eq|pn_Cmp_Lt, /**< less or equal */
	pn_Cmp_Gt = 4, /**< greater */
	pn_Cmp_Ge = pn_Cmp_Eq|pn_Cmp_Gt, /**< greater or equal */
	pn_Cmp_Lg = pn_Cmp_Lt|pn_Cmp_Gt, /**< less or greater ('not equal' for integer numbers) */
	pn_Cmp_Leg = pn_Cmp_Lt|pn_Cmp_Eq|pn_Cmp_Gt, /**< less, equal or greater ('not unordered') */
	pn_Cmp_Uo = 8, /**< unordered */
	pn_Cmp_Ue = pn_Cmp_Uo|pn_Cmp_Eq, /**< unordered or equal */
	pn_Cmp_Ul = pn_Cmp_Uo|pn_Cmp_Lt, /**< unordered or less */
	pn_Cmp_Ule = pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Eq, /**< unordered, less or equal */
	pn_Cmp_Ug = pn_Cmp_Uo|pn_Cmp_Gt, /**< unordered or greater */
	pn_Cmp_Uge = pn_Cmp_Uo|pn_Cmp_Gt|pn_Cmp_Eq, /**< onordered, greater or equal */
	pn_Cmp_Ne = pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Gt, /**< unordered, less or greater ('not equal' for floatingpoint numbers) */
	pn_Cmp_True = 15, /**< always true */
	pn_Cmp_max
} pn_Cmp;

/**
 * Projection numbers for result of Cond node (use for Proj nodes)
 */
typedef enum {
	pn_Cond_false, /**< control flow if operand is "false" */
	pn_Cond_true, /**< control flow if operand is "true" */
	pn_Cond_max
} pn_Cond;

/**
 * Projection numbers for result of CopyB node (use for Proj nodes)
 */
typedef enum {
	pn_CopyB_M = pn_Generic_M, /**< memory result */
	pn_CopyB_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_CopyB_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_CopyB_max
} pn_CopyB;

/**
 * Projection numbers for result of Div node (use for Proj nodes)
 */
typedef enum {
	pn_Div_M = pn_Generic_M, /**< memory result */
	pn_Div_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Div_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Div_res = pn_Generic_other, /**< result of computation */
	pn_Div_max
} pn_Div;

/**
 * Projection numbers for result of DivMod node (use for Proj nodes)
 */
typedef enum {
	pn_DivMod_M = pn_Generic_M, /**< memory result */
	pn_DivMod_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_DivMod_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_DivMod_res_div = pn_Generic_other, /**< result of computation a/b */
	pn_DivMod_res_mod, /**< result of computation a%b */
	pn_DivMod_max
} pn_DivMod;

/**
 * Projection numbers for result of InstOf node (use for Proj nodes)
 */
typedef enum {
	pn_InstOf_M = pn_Generic_M, /**< memory result */
	pn_InstOf_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_InstOf_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_InstOf_res = pn_Generic_other, /**< checked object pointer */
	pn_InstOf_max
} pn_InstOf;

/**
 * Projection numbers for result of Load node (use for Proj nodes)
 */
typedef enum {
	pn_Load_M = pn_Generic_M, /**< memory result */
	pn_Load_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Load_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Load_res = pn_Generic_other, /**< result of load operation */
	pn_Load_max
} pn_Load;

/**
 * Projection numbers for result of Mod node (use for Proj nodes)
 */
typedef enum {
	pn_Mod_M = pn_Generic_M, /**< memory result */
	pn_Mod_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Mod_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Mod_res = pn_Generic_other, /**< result of computation */
	pn_Mod_max
} pn_Mod;

/**
 * Projection numbers for result of Quot node (use for Proj nodes)
 */
typedef enum {
	pn_Quot_M = pn_Generic_M, /**< memory result */
	pn_Quot_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Quot_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Quot_res = pn_Generic_other, /**< result of computation */
	pn_Quot_max
} pn_Quot;

/**
 * Projection numbers for result of Raise node (use for Proj nodes)
 */
typedef enum {
	pn_Raise_M = pn_Generic_M, /**< memory result */
	pn_Raise_X = pn_Generic_X_regular, /**< control flow to exception handler */
	pn_Raise_max
} pn_Raise;

/**
 * Projection numbers for result of Start node (use for Proj nodes)
 */
typedef enum {
	pn_Start_X_initial_exec, /**< control flow */
	pn_Start_M, /**< initial memory */
	pn_Start_P_frame_base, /**< frame base pointer */
	pn_Start_P_tls, /**< pointer to thread local storage segment */
	pn_Start_T_args, /**< function arguments */
	pn_Start_max
} pn_Start;

/**
 * Projection numbers for result of Store node (use for Proj nodes)
 */
typedef enum {
	pn_Store_M = pn_Generic_M, /**< memory result */
	pn_Store_X_regular = pn_Generic_X_regular, /**< control flow when no exception occurs */
	pn_Store_X_except = pn_Generic_X_except, /**< control flow when exception occured */
	pn_Store_max
} pn_Store;



/** Return true of the node is a ASM node. */
FIRM_API int is_ASM(const ir_node *node);
/** Return true of the node is a Abs node. */
FIRM_API int is_Abs(const ir_node *node);
/** Return true of the node is a Add node. */
FIRM_API int is_Add(const ir_node *node);
/** Return true of the node is a Alloc node. */
FIRM_API int is_Alloc(const ir_node *node);
/** Return true of the node is a Anchor node. */
FIRM_API int is_Anchor(const ir_node *node);
/** Return true of the node is a And node. */
FIRM_API int is_And(const ir_node *node);
/** Return true of the node is a Bad node. */
FIRM_API int is_Bad(const ir_node *node);
/** Return true of the node is a Block node. */
FIRM_API int is_Block(const ir_node *node);
/** Return true of the node is a Borrow node. */
FIRM_API int is_Borrow(const ir_node *node);
/** Return true of the node is a Bound node. */
FIRM_API int is_Bound(const ir_node *node);
/** Return true of the node is a Builtin node. */
FIRM_API int is_Builtin(const ir_node *node);
/** Return true of the node is a Call node. */
FIRM_API int is_Call(const ir_node *node);
/** Return true of the node is a Carry node. */
FIRM_API int is_Carry(const ir_node *node);
/** Return true of the node is a Cast node. */
FIRM_API int is_Cast(const ir_node *node);
/** Return true of the node is a Cmp node. */
FIRM_API int is_Cmp(const ir_node *node);
/** Return true of the node is a Cond node. */
FIRM_API int is_Cond(const ir_node *node);
/** Return true of the node is a Confirm node. */
FIRM_API int is_Confirm(const ir_node *node);
/** Return true of the node is a Const node. */
FIRM_API int is_Const(const ir_node *node);
/** Return true of the node is a Conv node. */
FIRM_API int is_Conv(const ir_node *node);
/** Return true of the node is a CopyB node. */
FIRM_API int is_CopyB(const ir_node *node);
/** Return true of the node is a Div node. */
FIRM_API int is_Div(const ir_node *node);
/** Return true of the node is a DivMod node. */
FIRM_API int is_DivMod(const ir_node *node);
/** Return true of the node is a Dummy node. */
FIRM_API int is_Dummy(const ir_node *node);
/** Return true of the node is a End node. */
FIRM_API int is_End(const ir_node *node);
/** Return true of the node is a Eor node. */
FIRM_API int is_Eor(const ir_node *node);
/** Return true of the node is a Free node. */
FIRM_API int is_Free(const ir_node *node);
/** Return true of the node is a IJmp node. */
FIRM_API int is_IJmp(const ir_node *node);
/** Return true of the node is a Id node. */
FIRM_API int is_Id(const ir_node *node);
/** Return true of the node is a InstOf node. */
FIRM_API int is_InstOf(const ir_node *node);
/** Return true of the node is a Jmp node. */
FIRM_API int is_Jmp(const ir_node *node);
/** Return true of the node is a Load node. */
FIRM_API int is_Load(const ir_node *node);
/** Return true of the node is a Minus node. */
FIRM_API int is_Minus(const ir_node *node);
/** Return true of the node is a Mod node. */
FIRM_API int is_Mod(const ir_node *node);
/** Return true of the node is a Mul node. */
FIRM_API int is_Mul(const ir_node *node);
/** Return true of the node is a Mulh node. */
FIRM_API int is_Mulh(const ir_node *node);
/** Return true of the node is a Mux node. */
FIRM_API int is_Mux(const ir_node *node);
/** Return true of the node is a NoMem node. */
FIRM_API int is_NoMem(const ir_node *node);
/** Return true of the node is a Not node. */
FIRM_API int is_Not(const ir_node *node);
/** Return true of the node is a Or node. */
FIRM_API int is_Or(const ir_node *node);
/** Return true of the node is a Phi node. */
FIRM_API int is_Phi(const ir_node *node);
/** Return true of the node is a Pin node. */
FIRM_API int is_Pin(const ir_node *node);
/** Return true of the node is a Proj node. */
FIRM_API int is_Proj(const ir_node *node);
/** Return true of the node is a Quot node. */
FIRM_API int is_Quot(const ir_node *node);
/** Return true of the node is a Raise node. */
FIRM_API int is_Raise(const ir_node *node);
/** Return true of the node is a Return node. */
FIRM_API int is_Return(const ir_node *node);
/** Return true of the node is a Rotl node. */
FIRM_API int is_Rotl(const ir_node *node);
/** Return true of the node is a Sel node. */
FIRM_API int is_Sel(const ir_node *node);
/** Return true of the node is a Shl node. */
FIRM_API int is_Shl(const ir_node *node);
/** Return true of the node is a Shr node. */
FIRM_API int is_Shr(const ir_node *node);
/** Return true of the node is a Shrs node. */
FIRM_API int is_Shrs(const ir_node *node);
/** Return true of the node is a Start node. */
FIRM_API int is_Start(const ir_node *node);
/** Return true of the node is a Store node. */
FIRM_API int is_Store(const ir_node *node);
/** Return true of the node is a Sub node. */
FIRM_API int is_Sub(const ir_node *node);
/** Return true of the node is a SymConst node. */
FIRM_API int is_SymConst(const ir_node *node);
/** Return true of the node is a Sync node. */
FIRM_API int is_Sync(const ir_node *node);
/** Return true of the node is a Tuple node. */
FIRM_API int is_Tuple(const ir_node *node);
/** Return true of the node is a Unknown node. */
FIRM_API int is_Unknown(const ir_node *node);
/** Return true of the node is a Unreachable node. */
FIRM_API int is_Unreachable(const ir_node *node);


FIRM_API ir_asm_constraint* get_ASM_input_constraints(const ir_node *node);
FIRM_API void set_ASM_input_constraints(ir_node *node, ir_asm_constraint* input_constraints);
FIRM_API ir_asm_constraint* get_ASM_output_constraints(const ir_node *node);
FIRM_API void set_ASM_output_constraints(ir_node *node, ir_asm_constraint* output_constraints);
FIRM_API ident** get_ASM_clobbers(const ir_node *node);
FIRM_API void set_ASM_clobbers(ir_node *node, ident** clobbers);
FIRM_API ident* get_ASM_text(const ir_node *node);
FIRM_API void set_ASM_text(ir_node *node, ident* text);

FIRM_API ir_node *get_Abs_op(const ir_node *node);
void set_Abs_op(ir_node *node, ir_node *op);

FIRM_API ir_node *get_Add_left(const ir_node *node);
void set_Add_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Add_right(const ir_node *node);
void set_Add_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Alloc_mem(const ir_node *node);
void set_Alloc_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Alloc_count(const ir_node *node);
void set_Alloc_count(ir_node *node, ir_node *count);
FIRM_API ir_type* get_Alloc_type(const ir_node *node);
FIRM_API void set_Alloc_type(ir_node *node, ir_type* type);
FIRM_API ir_where_alloc get_Alloc_where(const ir_node *node);
FIRM_API void set_Alloc_where(ir_node *node, ir_where_alloc where);


FIRM_API ir_node *get_And_left(const ir_node *node);
void set_And_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_And_right(const ir_node *node);
void set_And_right(ir_node *node, ir_node *right);



FIRM_API ir_node *get_Borrow_left(const ir_node *node);
void set_Borrow_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Borrow_right(const ir_node *node);
void set_Borrow_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Bound_mem(const ir_node *node);
void set_Bound_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Bound_index(const ir_node *node);
void set_Bound_index(ir_node *node, ir_node *index);
FIRM_API ir_node *get_Bound_lower(const ir_node *node);
void set_Bound_lower(ir_node *node, ir_node *lower);
FIRM_API ir_node *get_Bound_upper(const ir_node *node);
void set_Bound_upper(ir_node *node, ir_node *upper);

FIRM_API ir_node *get_Builtin_mem(const ir_node *node);
void set_Builtin_mem(ir_node *node, ir_node *mem);
FIRM_API ir_builtin_kind get_Builtin_kind(const ir_node *node);
FIRM_API void set_Builtin_kind(ir_node *node, ir_builtin_kind kind);
FIRM_API ir_type* get_Builtin_type(const ir_node *node);
FIRM_API void set_Builtin_type(ir_node *node, ir_type* type);

FIRM_API ir_node *get_Call_mem(const ir_node *node);
void set_Call_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Call_ptr(const ir_node *node);
void set_Call_ptr(ir_node *node, ir_node *ptr);
FIRM_API ir_type* get_Call_type(const ir_node *node);
FIRM_API void set_Call_type(ir_node *node, ir_type* type);
FIRM_API unsigned get_Call_tail_call(const ir_node *node);
FIRM_API void set_Call_tail_call(ir_node *node, unsigned tail_call);

FIRM_API ir_node *get_Carry_left(const ir_node *node);
void set_Carry_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Carry_right(const ir_node *node);
void set_Carry_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Cast_op(const ir_node *node);
void set_Cast_op(ir_node *node, ir_node *op);
FIRM_API ir_type* get_Cast_type(const ir_node *node);
FIRM_API void set_Cast_type(ir_node *node, ir_type* type);

FIRM_API ir_node *get_Cmp_left(const ir_node *node);
void set_Cmp_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Cmp_right(const ir_node *node);
void set_Cmp_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Cond_selector(const ir_node *node);
void set_Cond_selector(ir_node *node, ir_node *selector);
FIRM_API long get_Cond_default_proj(const ir_node *node);
FIRM_API void set_Cond_default_proj(ir_node *node, long default_proj);
FIRM_API cond_jmp_predicate get_Cond_jmp_pred(const ir_node *node);
FIRM_API void set_Cond_jmp_pred(ir_node *node, cond_jmp_predicate jmp_pred);

FIRM_API ir_node *get_Confirm_value(const ir_node *node);
void set_Confirm_value(ir_node *node, ir_node *value);
FIRM_API ir_node *get_Confirm_bound(const ir_node *node);
void set_Confirm_bound(ir_node *node, ir_node *bound);
FIRM_API pn_Cmp get_Confirm_cmp(const ir_node *node);
FIRM_API void set_Confirm_cmp(ir_node *node, pn_Cmp cmp);

FIRM_API tarval* get_Const_tarval(const ir_node *node);
FIRM_API void set_Const_tarval(ir_node *node, tarval* tarval);

FIRM_API ir_node *get_Conv_op(const ir_node *node);
void set_Conv_op(ir_node *node, ir_node *op);
FIRM_API int get_Conv_strict(const ir_node *node);
FIRM_API void set_Conv_strict(ir_node *node, int strict);

FIRM_API ir_node *get_CopyB_mem(const ir_node *node);
void set_CopyB_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_CopyB_dst(const ir_node *node);
void set_CopyB_dst(ir_node *node, ir_node *dst);
FIRM_API ir_node *get_CopyB_src(const ir_node *node);
void set_CopyB_src(ir_node *node, ir_node *src);
FIRM_API ir_type* get_CopyB_type(const ir_node *node);
FIRM_API void set_CopyB_type(ir_node *node, ir_type* type);

FIRM_API ir_node *get_Div_mem(const ir_node *node);
void set_Div_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Div_left(const ir_node *node);
void set_Div_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Div_right(const ir_node *node);
void set_Div_right(ir_node *node, ir_node *right);
FIRM_API ir_mode* get_Div_resmode(const ir_node *node);
FIRM_API void set_Div_resmode(ir_node *node, ir_mode* resmode);
FIRM_API int get_Div_no_remainder(const ir_node *node);
FIRM_API void set_Div_no_remainder(ir_node *node, int no_remainder);

FIRM_API ir_node *get_DivMod_mem(const ir_node *node);
void set_DivMod_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_DivMod_left(const ir_node *node);
void set_DivMod_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_DivMod_right(const ir_node *node);
void set_DivMod_right(ir_node *node, ir_node *right);
FIRM_API ir_mode* get_DivMod_resmode(const ir_node *node);
FIRM_API void set_DivMod_resmode(ir_node *node, ir_mode* resmode);



FIRM_API ir_node *get_Eor_left(const ir_node *node);
void set_Eor_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Eor_right(const ir_node *node);
void set_Eor_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Free_mem(const ir_node *node);
void set_Free_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Free_ptr(const ir_node *node);
void set_Free_ptr(ir_node *node, ir_node *ptr);
FIRM_API ir_node *get_Free_size(const ir_node *node);
void set_Free_size(ir_node *node, ir_node *size);
FIRM_API ir_type* get_Free_type(const ir_node *node);
FIRM_API void set_Free_type(ir_node *node, ir_type* type);
FIRM_API ir_where_alloc get_Free_where(const ir_node *node);
FIRM_API void set_Free_where(ir_node *node, ir_where_alloc where);

FIRM_API ir_node *get_IJmp_target(const ir_node *node);
void set_IJmp_target(ir_node *node, ir_node *target);

FIRM_API ir_node *get_Id_pred(const ir_node *node);
void set_Id_pred(ir_node *node, ir_node *pred);

FIRM_API ir_node *get_InstOf_store(const ir_node *node);
void set_InstOf_store(ir_node *node, ir_node *store);
FIRM_API ir_node *get_InstOf_obj(const ir_node *node);
void set_InstOf_obj(ir_node *node, ir_node *obj);
FIRM_API ir_type* get_InstOf_type(const ir_node *node);
FIRM_API void set_InstOf_type(ir_node *node, ir_type* type);


FIRM_API ir_node *get_Load_mem(const ir_node *node);
void set_Load_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Load_ptr(const ir_node *node);
void set_Load_ptr(ir_node *node, ir_node *ptr);
FIRM_API ir_mode* get_Load_mode(const ir_node *node);
FIRM_API void set_Load_mode(ir_node *node, ir_mode* mode);

FIRM_API ir_node *get_Minus_op(const ir_node *node);
void set_Minus_op(ir_node *node, ir_node *op);

FIRM_API ir_node *get_Mod_mem(const ir_node *node);
void set_Mod_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Mod_left(const ir_node *node);
void set_Mod_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Mod_right(const ir_node *node);
void set_Mod_right(ir_node *node, ir_node *right);
FIRM_API ir_mode* get_Mod_resmode(const ir_node *node);
FIRM_API void set_Mod_resmode(ir_node *node, ir_mode* resmode);

FIRM_API ir_node *get_Mul_left(const ir_node *node);
void set_Mul_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Mul_right(const ir_node *node);
void set_Mul_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Mulh_left(const ir_node *node);
void set_Mulh_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Mulh_right(const ir_node *node);
void set_Mulh_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Mux_sel(const ir_node *node);
void set_Mux_sel(ir_node *node, ir_node *sel);
FIRM_API ir_node *get_Mux_false(const ir_node *node);
void set_Mux_false(ir_node *node, ir_node *false_);
FIRM_API ir_node *get_Mux_true(const ir_node *node);
void set_Mux_true(ir_node *node, ir_node *true_);


FIRM_API ir_node *get_Not_op(const ir_node *node);
void set_Not_op(ir_node *node, ir_node *op);

FIRM_API ir_node *get_Or_left(const ir_node *node);
void set_Or_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Or_right(const ir_node *node);
void set_Or_right(ir_node *node, ir_node *right);


FIRM_API ir_node *get_Pin_op(const ir_node *node);
void set_Pin_op(ir_node *node, ir_node *op);

FIRM_API ir_node *get_Proj_pred(const ir_node *node);
void set_Proj_pred(ir_node *node, ir_node *pred);

FIRM_API ir_node *get_Quot_mem(const ir_node *node);
void set_Quot_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Quot_left(const ir_node *node);
void set_Quot_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Quot_right(const ir_node *node);
void set_Quot_right(ir_node *node, ir_node *right);
FIRM_API ir_mode* get_Quot_resmode(const ir_node *node);
FIRM_API void set_Quot_resmode(ir_node *node, ir_mode* resmode);

FIRM_API ir_node *get_Raise_mem(const ir_node *node);
void set_Raise_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Raise_exo_ptr(const ir_node *node);
void set_Raise_exo_ptr(ir_node *node, ir_node *exo_ptr);

FIRM_API ir_node *get_Return_mem(const ir_node *node);
void set_Return_mem(ir_node *node, ir_node *mem);

FIRM_API ir_node *get_Rotl_left(const ir_node *node);
void set_Rotl_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Rotl_right(const ir_node *node);
void set_Rotl_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Sel_mem(const ir_node *node);
void set_Sel_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Sel_ptr(const ir_node *node);
void set_Sel_ptr(ir_node *node, ir_node *ptr);
FIRM_API ir_entity* get_Sel_entity(const ir_node *node);
FIRM_API void set_Sel_entity(ir_node *node, ir_entity* entity);

FIRM_API ir_node *get_Shl_left(const ir_node *node);
void set_Shl_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Shl_right(const ir_node *node);
void set_Shl_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Shr_left(const ir_node *node);
void set_Shr_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Shr_right(const ir_node *node);
void set_Shr_right(ir_node *node, ir_node *right);

FIRM_API ir_node *get_Shrs_left(const ir_node *node);
void set_Shrs_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Shrs_right(const ir_node *node);
void set_Shrs_right(ir_node *node, ir_node *right);


FIRM_API ir_node *get_Store_mem(const ir_node *node);
void set_Store_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_Store_ptr(const ir_node *node);
void set_Store_ptr(ir_node *node, ir_node *ptr);
FIRM_API ir_node *get_Store_value(const ir_node *node);
void set_Store_value(ir_node *node, ir_node *value);

FIRM_API ir_node *get_Sub_left(const ir_node *node);
void set_Sub_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_Sub_right(const ir_node *node);
void set_Sub_right(ir_node *node, ir_node *right);





FIRM_API ir_node *get_Unreachable_mem(const ir_node *node);
void set_Unreachable_mem(ir_node *node, ir_node *mem);
/** @} */

#endif
