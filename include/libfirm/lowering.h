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
 * @brief   Lowering of high level constructs.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_LOWERING_H
#define FIRM_LOWERING_H

#include "firm_types.h"
/**
 * A type telling where to add hidden parameters.
 */
typedef enum add_hidden_params {
	ADD_HIDDEN_ALWAYS_IN_FRONT = 0,   /**< always add hidden parameters in front (default). */
	ADD_HIDDEN_ALWAYS_LAST     = 1,   /**< always add hidden parameters last, did not work for variadic functions. */
	ADD_HIDDEN_SMART           = 2    /**< add hidden parameters last for non-variadic and first for variadic functions. */
} add_hidden;

/**
 * Additional flags for the lowering.
 */
enum lowering_flags {
	LF_NONE              = 0, /**< no additional flags */
	LF_COMPOUND_PARAM    = 1, /**< lower calls with compound parameters */
	LF_COMPOUND_RETURN   = 2, /**< lower calls with compound returns */
	LF_RETURN_HIDDEN     = 4, /**< return the hidden address instead of void */
	LF_SMALL_CMP_IN_REGS = 8  /**< return small compound values in registers */
};

/** Maximum number of registers that can be used to return compound values. */
#define MAX_REGISTER_RET_VAL 2

/**
 * A struct containing all control parameters for
 * lower_compound_ret_calls().
 */
typedef struct {
	int        def_ptr_alignment;   /**< Default alignment for data pointer. */
	unsigned   flags;               /**< A bitmask of enum lowering_flags. */
	add_hidden hidden_params;       /**< Where to add hidden parameters. */

	/**
	 * A function returning a pointer type for a given type.
	 * If this pointer is NULL, a new pointer type is always created.
	 */
	ir_type *(*find_pointer_type)(ir_type *e_type, ir_mode *mode, int alignment);

	/**
	 * If the LF_SMALL_CMP_IN_REGS flag is set, this function will be called
	 * to decide, whether a compound value should be returned in registers.
	 * This function must return the number of used registers and fill in the modes
	 * of the registers to use. Up to MAX_REGISTER_RET_VAL registers can be used.
	 */
	int (*ret_compound_in_regs)(ir_type *compound_tp, ir_mode **modes);
} lower_params_t;

/**
 * Lower calls with compound parameter and return types.
 * This function does the following transformations:
 *
 * If LF_COMPOUND_PARAM is set:
 *
 * - Copy compound parameters to a new location on the callers
 *   stack and transmit the address of this new location
 *
 * If LF_COMPOUND_RETURN is set:
 *
 * - Adds a new (hidden) pointer parameter for
 *   any return compound type. The return type is replaced by void
 *   or if LOWERING_FLAGS_RETURN_HIDDEN is set by the address.
 *
 * - Use of the hidden parameters in the function code.
 *
 * - Change all calls to functions with compound return
 *   by providing space for the hidden parameter on the callers
 *   stack.
 *
 * - Replace a possible block copy after the function call.
 *
 * General:
 *
 * - Changes the types of methods and calls to the lowered ones
 *
 * - lower all method types of existing entities
 *
 * In pseudo-code, the following transformation is done:
 *
   @code
   struct x ret = func(a, b);
   @endcode
 *
 * is translated into
   @code
   struct x ret;
   func(&ret, a, b);
   @endcode
 *
 * If the function returns only one possible result, the copy-on-return
 * optimization is done, ie.
   @code
   struct x func(a) {
     struct x ret;
     ret.a = a;
     return ret;
   }
   @endcode
 *
 * is transformed into
 *
   @code
   void func(struct x *ret, a) {
     ret->a = a;
   }
   @endcode
 *
 * @param params  A structure containing the control parameter for this
 *                transformation.
 *
 * During the transformation, pointer types must be created or reused.
 * The caller can provide params->find_pointer_type for this task to
 * reduce the number of created pointer types.
 * If params->find_pointer_type is NULL, new pointer types
 * are always created automatically.
 */
void lower_calls_with_compounds(const lower_params_t *params);

/**
 * A callback type for creating an intrinsic entity for a given opcode.
 *
 * @param method   the method type of the emulation function entity
 * @param op       the emulated ir_op
 * @param imode    the input mode of the emulated opcode
 * @param omode    the output mode of the emulated opcode
 * @param context  the context parameter
 */
typedef ir_entity *(create_intrinsic_fkt)(ir_type *method, const ir_op *op,
                                          const ir_mode *imode, const ir_mode *omode,
                                          void *context);

/**
 * The lowering parameter description.
 */
typedef struct _lwrdw_param_t {
	int enable;                   /**< if true lowering is enabled */
	int little_endian;            /**< if true should be lowered for little endian, else big endian */
	ir_mode *high_signed;         /**< the double word signed mode to be lowered, typically Ls */
	ir_mode *high_unsigned;       /**< the double word unsigned mode to be lowered, typically Lu */
	ir_mode *low_signed;          /**< the word signed mode to be used, typically Is */
	ir_mode *low_unsigned;        /**< the word unsigned mode to be used, typically Iu */

	/** callback that creates the intrinsic entity */
	create_intrinsic_fkt *create_intrinsic;
	void *ctx;                    /**< context parameter for the creator function */
} lwrdw_param_t;

/**
 * Lower all double word operations.
 */
void lower_dw_ops(const lwrdw_param_t *param);

/**
 * Default implementation. Context is unused.
 */
ir_entity *def_create_intrinsic_fkt(ir_type *method, const ir_op *op,
                                    const ir_mode *imode, const ir_mode *omode,
                                    void *context);

/**
 * Replaces SymConsts by a real constant if possible.
 * Replace Sel nodes by address computation.  Also resolves array access.
 * Handle bit fields by added And/Or calculations.
 *
 * @Note: There is NO lowering ob objects oriented types. This is highly compiler
 *        and ABI specific and should be placed directly in the compiler.
 */
void lower_highlevel(void);

/**
 * Lowers mode_b operations to integer arithmetic. After the lowering the only
 * operations with mode_b are the Projs of Cmps; the only nodes with mode_b
 * inputs are Cond and Psi nodes.
 *
 * Example: Psi(a < 0, 1, 0) => a >> 31
 *
 * @param irg               the firm graph to lower
 * @param mode              the mode of the lowered operations
 * @param lower_direct_cmp  if set to 1, psi nodes with only have cmp/=0 as input
 */
void ir_lower_mode_b(ir_graph *irg, ir_mode *mode, int lower_direct_cmp);

/**
 * An intrinsic mapper function.
 *
 * @param node   the IR-node that will be mapped
 * @param ctx    a context
 *
 * @return  non-zero if the call was mapped
 */
typedef int (*i_mapper_func)(ir_node *node, void *ctx);

enum ikind {
	INTRINSIC_CALL  = 0,  /**< the record represents an intrinsic call */
	INTRINSIC_INSTR       /**< the record represents an intrinsic instruction */
};

/**
 * An intrinsic call record.
 */
typedef struct _i_call_record {
	enum ikind    kind;       /**< must be INTRINSIC_CALL */
	ir_entity     *i_ent;     /**< the entity representing an intrinsic call */
	i_mapper_func i_mapper;   /**< the mapper function to call */
	void          *ctx;       /**< mapper context */
	void          *link;      /**< used in the construction algorithm, must be NULL */
} i_call_record;

/**
 * An intrinsic instruction record.
 */
typedef struct _i_instr_record {
	enum ikind    kind;       /**< must be INTRINSIC_INSTR */
	ir_op         *op;        /**< the opcode that must be mapped. */
	i_mapper_func i_mapper;   /**< the mapper function to call */
	void          *ctx;       /**< mapper context */
	void          *link;      /**< used in the construction algorithm, must be NULL */
} i_instr_record;

/**
 * An intrinsic record.
 */
typedef union _i_record {
	i_call_record  i_call;
	i_instr_record i_instr;
} i_record;

/**
 * Go through all graphs and map calls to intrinsic functions and instructions.
 *
 * Every call or instruction is reported to its mapper function,
 * which is responsible for rebuilding the graph.
 *
 * current_ir_graph is always set.
 *
 * @param list             an array of intrinsic map records
 * @param length           the length of the array
 * @param part_block_used  set to true if part_block() must be using during lowering
 *
 * @return number of found intrinsics.
 */
unsigned lower_intrinsics(i_record *list, int length, int part_block_used);

/**
 * A mapper for the integer absolute value: inttype abs(inttype v).
 * Replaces the call by a Abs node.
 *
 * @return always 1
 */
int i_mapper_Abs(ir_node *call, void *ctx);

/**
 * A mapper for the alloca() function: pointer alloca(inttype size)
 * Replaces the call by a Alloca(stack_alloc) node.
 *
 * @return always 1
 */
int i_mapper_Alloca(ir_node *call, void *ctx);

/**
 * A runtime routine description.
 */
typedef struct _runtime_rt {
	ir_entity *ent;            /**< The entity representing the runtime routine. */
	ir_mode   *mode;           /**< The operation mode of the mapped instruction. */
	ir_mode   *res_mode;       /**< The result mode of the mapped instruction or NULL. */
	long      mem_proj_nr;     /**< if >= 0, create a memory ProjM() */
	long      regular_proj_nr; /**< if >= 0, create a regular ProjX() */
	long      exc_proj_nr;     /**< if >= 0, create a exception ProjX() */
	long      exc_mem_proj_nr; /**< if >= 0, create a exception memory ProjM() */
	long      res_proj_nr;     /**< if >= 0, first result projection number */
} runtime_rt;

/**
 * A mapper for mapping unsupported instructions to runtime calls.
 * Maps a op(arg_0, ..., arg_n) into a call to a runtime function
 * rt(arg_0, ..., arg_n).
 *
 * The mapping is only done, if the modes of all arguments matches the
 * modes of rt's argument.
 * Further, if op has a memory input, the generated Call uses it, else
 * it gets a NoMem.
 * The pinned state of the Call will be set to the pinned state of op.
 *
 * Note that i_mapper_RuntimeCall() must be used with a i_instr_record.
 *
 * @return 1 if an op was mapped, 0 else
 *
 * Some examples:
 *
 * - Maps signed Div nodes to calls to rt_Div():
   @code
  runtime_rt rt_Div = {
    ent("int rt_Div(int, int)"),
    mode_T,
    mode_Is,
    pn_Div_M,
    pn_Div_X_regular,
    pn_Div_X_except,
    pn_Div_M,
    pn_Div_res
  };
  i_instr_record map_Div = {
    INTRINSIC_INSTR,
    op_Div,
    i_mapper_RuntimeCall,
    &rt_Div,
    NULL
  };
  @endcode
 *
 * - Maps ConvD(F) to calls to rt_Float2Div():
  @code
  runtime_rt rt_Float2Double = {
    ent("double rt_Float2Div(float)"),
    get_type_mode("double"),
    NULL,
    -1,
    -1,
    -1,
    -1,
    -1
  };
  i_instr_record map_Float2Double = {
    INTRINSIC_INSTR,
    op_Conv,
    i_mapper_RuntimeCall,
    &rt_Float2Double,
    NULL
  };
  @endcode
 */
int i_mapper_RuntimeCall(ir_node *node, runtime_rt *rt);

#endif /* FIRM_LOWERING_H */
