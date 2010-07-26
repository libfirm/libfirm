
/* Warning: automatically generated code */


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
