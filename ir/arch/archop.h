/*
 * Project:     libFIRM
 * File name:   ir/arch/archop.h
 * Purpose:     Architecture dependand IR operations
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _ARCHOP_H_
#define _ARCHOP_H_

/**
 * Mask defining which architecture depend
 * operations are supported.
 */
typedef enum _arch_ops_mask {
  ARCH_OPS_NONE   = 0,              /**< no additional Operations */
  ARCH_OPS_MINMAX = 1               /**< use the Min/Max Operation */
} arch_ops_mask;

typedef struct _arch_ops_info {
  arch_ops_mask  enabled_ops;         /**< a mask of enabled IR-opcodes */
  unsigned       minmax_handle_NaN:1; /**< if set, Min(a,a) == a, else unknown */
} arch_ops_info;

extern ir_op *op_Min, *op_Max;

ir_op *get_op_Min(void);
ir_op *get_op_Max(void);

/** construct a Min: Min(a,b) = a < b ? a : b */
ir_node *
new_rd_Min(dbg_info *db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode);

/** construct a Max: Max(a,b) = a > b ? a : b */
ir_node *
new_rd_Max(dbg_info *db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode);

ir_node *
new_r_Min(ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode);

ir_node *
new_r_Max(ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode);

ir_node *
new_Min(ir_node *op1, ir_node *op2, ir_mode *mode);

ir_node *
new_Max(ir_node *op1, ir_node *op2, ir_mode *mode);

/**
 * Create Min and Mux from Mux nodes
 */
ir_node *arch_transform_node_Mux(ir_node *mux);

/**
 * initialize the ops.
 */
void firm_archops_init(const arch_ops_info *info);

#endif /* _ARCHOP_H_ */
