/*
 * Project:     libFIRM
 * File name:   ir/ana/irouts.h
 * Purpose:     Compute and access out edges.
 * Author:      Goetz Lindenmaier
 * Modified by: Michael Beck
 * Created:     1.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2007 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irouts.h
 *
 * Implements Def-Use edges, also called outedges.
 *
 * @author Goetz Lindenmaier
 *
 * @todo eventually add reverse conrtol flow graph. (If needed.)
 */
#ifndef _IROUTS_H_
#define _IROUTS_H_

#include "firm_types.h"

/*------------------------------------------------------------------*/
/* Accessing the out datastructures.                                */
/* These routines only work properly if the ir_graph is in state    */
/* outs_consistent or outs_inconsistent.                            */
/*------------------------------------------------------------------*/

/** To iterate through the successors iterate from 0 to i < get_irn_outs(). No
   order of successors guaranteed.  Will return edges from block to floating
   nodes even if irgraph is in state "op_pin_state_floats". */
/* returns the number of successors of the node: */
int      get_irn_n_outs(ir_node *node);

/** Get predecessor n */
ir_node *get_irn_out(ir_node *node, int pos);

/** Set predecessor n */
void     set_irn_out(ir_node *node, int pos, ir_node *out);

/* Methods to iterate through the control flow graph. Iterate from 0 to
   i < get_Block_cfg_outs(block). No order of successors guaranteed. */

/** Return the number of control flow successors, ignore keep-alives. */
int      get_Block_n_cfg_outs(ir_node *node);

/** Return the number of control flow successors, honor keep-alives. */
int      get_Block_n_cfg_outs_ka(ir_node *node);

/** Access predecessor n, ignore keep-alives. */
ir_node *get_Block_cfg_out(ir_node *node, int pos);

/** Access predecessor n, honor keep-alives. */
ir_node *get_Block_cfg_out_ka(ir_node *node, int pos);

/** Walks over the graph starting at node.  Walks also if graph is in state
   "outs_inconsistent".  Assumes current_ir_graph is set properly. */
void irg_out_walk(ir_node *node,
                  irg_walk_func *pre, irg_walk_func *post,
                  void *env);

/** Walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.
   node must be either op_Block or mode_X.  */
void irg_out_block_walk(ir_node *node,
                        irg_walk_func *pre, irg_walk_func *post,
                        void *env);

/*------------------------------------------------------------------*/
/* Building and Removing the out datastructure                      */
/*------------------------------------------------------------------*/

/** Computes the out edges.  Sets a flag in irg to "outs_consistent".  If the
    graph is changed this flag must be set to "outs_inconsistent".  Computes
    out edges from block to floating nodes even if graph is in state
   "op_pin_state_floats".   Optimizes Tuple nodes. */
void compute_irg_outs(ir_graph *irg);
void compute_irp_outs(void);

void assure_irg_outs(ir_graph *irg);

/** Computes the out edges in interprocedural view */
void compute_ip_outs(void);
/** Frees the out datastructures.  Sets the flag in irg to "outs_none". */
void free_ip_outs(void);
void free_irg_outs(ir_graph *irg);
void free_irp_outs(void);

#endif /* _IROUTS_H_ */
