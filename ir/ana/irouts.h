/* Copyright (C) 2002 by Universitaet Karlsruhe
* All rights reserved.
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

/* $Id$ */

# ifndef _IROUTS_H_
# define _IROUTS_H_

# include "irgraph.h"
# include "irnode.h"

/*------------------------------------------------------------------*/
/* Accessing the out datastructures.                                */
/* These routines only work properly if the ir_graph is in state    */
/* outs_consistent or outs_inconsistent.                            */
/*------------------------------------------------------------------*/

/** To iterate through the successors iterate from 0 to i < get_irn_outs(). No
   order of successors guaranteed.  Will return edges from block to floating
   nodes even if irgraph is in state "floats". */
/* returns the number of successors of the node: */
int             get_irn_n_outs (ir_node *node);

/** Get predecessor n */
INLINE ir_node *get_irn_out  (ir_node *node, int pos);

/** Set predecessor n */
INLINE void     set_irn_out  (ir_node *node, int pos, ir_node *out);

/* Methods to iterate through the control flow graph. Iterate from 0 to
   i < get_Block_cfg_outs(block). No order of successors guaranteed. */
int             get_Block_n_cfg_outs (ir_node *node);

/** Access predecessor n. */
INLINE ir_node *get_Block_cfg_out  (ir_node *node, int pos);

#ifndef _IRG_WALK_FUNC_TYPEDEF_
#define _IRG_WALK_FUNC_TYPEDEF_
/** The type of the walk function */
typedef void irg_walk_func(ir_node *, void *);
#endif

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
/* Building and Removing the out datasturcture                      */
/*------------------------------------------------------------------*/

/** Computes the out edges.  Sets a flag in irg to "outs_consistent".  If the
   graph is changed this flag must be set to "outs_inconsistent".  Computes
   out edges from block to floating nodes even if graph is in state
   "floats".   Optimizes Tuple nodes. */
void compute_outs(ir_graph *irg);
/** Frees the out datastructures.  Sets the flag in irg to "no_outs". */
void free_outs(ir_graph *irg);

#endif /* _IROUTS_H_ */
