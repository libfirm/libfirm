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

/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_intrinsics.h
 * Purpose:     lowering of Calls of intrinsic functions
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 */

/**
 * @file lower_intrinsics.h
 *
 * Lowering of Calls of intrinsic functions.
 *
 * @author Michael Beck
 */
#ifndef _LOWER_INTRINSICS_H_
#define _LOWER_INTRINSICS_H_

#include "firm_types.h"

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
 * An intrinsic record.
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
 * @param list    an array of intrinsic map records
 * @param length  the length of the array
 *
 * @return number of found intrinsics.
 */
unsigned lower_intrinsics(i_record *list, int length);

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
  long      mem_proj_nr;     /**< if >= 0, create a memory ProjM() */
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
 * - Maps Div nodes to calls to rt_Div():
   @code
  runtime_rt rt_Div = {
    ent("int rt_Div(int, int)"),
    mode_T,
    pn_Div_M,
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

#endif /* _LOWER_INTRINSICS_H_ */
