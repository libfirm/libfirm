/*
 * Project:     libFIRM
 * File name:   ir/ir/irgmod.h
 * Purpose:     Support for ir graph modification.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 *
 * @file irgmod.h
 *
 * ir graph modification.
 *
 * @author Martin Trapp, Christian Schaefer
 */


# ifndef _IRGMOD_H_
# define _IRGMOD_H_

# include "irnode.h"

/** Exchanges two nodes by conserving edges leaving old (i.e.,
   pointers pointing to old).  Turns the old node into an Id. */
void exchange (ir_node *old, ir_node *nw);

/** Turns a node into a "useless" Tuple.
 *
 *  Turns a node into a "useless" Tuple.  The Tuple node just forms a tuple
 *  from several inputs.  The predecessors of the tuple have to be
 *  set by hand.  The block predecessor automatically remains the same.
 *  This is useful if a node returning a tuple is removed, but the Projs
 *  extracting values from the tuple are not available.
 *
 *  @param node The node to be turned into a tuple.
 *  @param arity The number of values formed into a Tuple.
 */
void turn_into_tuple (ir_node *node, int arity);

/** Walks over the passed ir graph and collects all Phi nodes as a
  * list built with the link field in their corresponding block.
  * Further it collects all Proj nodes in a list of the node producing
  * the tuple. In case of nested tuples the Projs are collected in the
  * node producing the outermost Tuple.
  * All other link fields are cleared afterwards.
  */
void collect_phiprojs(ir_graph *irg);

/** Parts a block into two.  This is useful to insert other blocks within a
 *  given block.
 *
 * Adds a new block (new_block) in the control flow before the block
 * (old_block) of node.  Moves node and its predecessors from old_block to
 * new_block.  Moves all Projs that depend on moved nodes and are in old_block
 * to new_block. Moves all Phi nodes from old_block to new_block.  To achieve
 * this the routine assumes that all Phi nodes are in a list (using the link
 * field) in the link field of old_block.  Further it assumes that all Proj nodes
 * are accessible by the link field of the nodes producing the Tuple. This
 * can be established by collect_phiprojs().  part_block conserves this property.
 * Adds a Jmp node to new_block that jumps to old_block.
 * Assumes that node is contained in current_ir_graph.  Sets current_block in
 * this ir_graph to new_block.
 *
 * @param node   The node were to break the block
 */
void part_block(ir_node *node);

#endif /* ifndef _IRGMOD_H_ */
