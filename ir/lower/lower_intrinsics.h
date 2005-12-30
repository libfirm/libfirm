/*
 * Project:     libFIRM
 * File name:   ir/lower/lower_intrinsics.h
 * Purpose:     lowering of Calls of intrinsic functions
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
 * @param call   the Call node
 * @param ctx    a context
 *
 * @return  non-zero if the call was mapped
 */
typedef int (*i_mapper_func)(ir_node *call, void *ctx);

/**
 * An intrinsic record.
 */
typedef struct _i_record {
  entity        *i_ent;     /**< the entity representing an intrinsic */
  i_mapper_func i_mapper;   /**< the mapper function to call */
  void          *ctx;       /**< mapper context */
} i_record;

/**
 * Go through all graphs and map calls to intrinsic functions.
 *
 * Every call is reported to its mapper function, which is responsible for
 * rebuilding the graph.
 *
 * current_ir_graph is always set.
 *
 * @param list    an array of intrinsic map records
 * @param length  the length of the array
 *
 * @return number of found intrinsic calls
 */
unsigned lower_intrinsic_calls(const i_record *list, int length);

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

#endif /* _LOWER_INTRINSICS_H_ */
