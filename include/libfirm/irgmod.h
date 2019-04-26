/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Support for ir graph modification.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 */
#ifndef FIRM_IR_IRGMOD_H
#define FIRM_IR_IRGMOD_H

#include "firm_types.h"

#include "begin.h"

/**
 * Exchanges two nodes by conserving edges leaving old (i.e.,
 * pointers pointing to old).
 * The nodes op will be changed to op_Deleted and you must not do anything with
 * the node anymore except testing its op.
 */
FIRM_API void exchange(ir_node *old, ir_node *nw);

/** Turns a node into a "useless" Tuple.
 *
 *  Turns a node into a "useless" Tuple.  The Tuple node just forms a tuple
 *  from several inputs. All predecessors of the tuple are set to bad and
 *  should be replaced if necssary. The block predecessor remains the same.
 *  This is useful if a node returning a tuple is removed, but the Projs
 *  extracting values from the tuple are not available.
 *
 *  @param node The node to be turned into a tuple.
 *  @param arity The number of values formed into a Tuple.
 */
FIRM_API void turn_into_tuple(ir_node *node, int arity, ir_node *const in[]);

/** Walks over the passed IR graph and collects all Phi nodes as a
  * list in their corresponding block (using get_Block_phis() API).
  * Further it collects all Proj nodes in a list of the node producing
  * the tuple. In case of nested tuples the Projs are collected in the
  * node producing the outermost Tuple.
  * All other link fields are cleared afterwards.
  */
FIRM_API void collect_phiprojs_and_start_block_nodes(ir_graph *irg);

/** Introduce a new node with "start_block_placed" attribute. It is necesary
 * to call this function so the next part_block() works without running
 * collect_phiprojs_and_start_block_nodes() again. */
FIRM_API void collect_new_start_block_node(ir_node *node);

/** Introduce a new phi node. It is necessary to call this function so the next
 * part_block() works without running collect_phiprojs_and_start_block_nodes()
 * again. */
FIRM_API void collect_new_phi_node(ir_node *node);

/** Parts a block into two.  This is useful to insert other blocks within a
 *  given block.
 *
 * Adds a new block (new_block) in the control flow before the block
 * (old_block) of node. Moves node and its predecessors from old_block
 * to new_block. Moves all Projs that depend on moved nodes and are in
 * old_block to new_block. Moves all Phi nodes from old_block to
 * new_block. Adds a Jmp node to new_block that jumps to old_block.
 *
 * To achieve this the routine assumes that all Phi nodes are in the
 * Phi list (see get_Block_phis()) of old_block. Further it assumes
 * that all Proj nodes are accessible by the link field of the nodes
 * producing the Tuple. This can be established by
 * collect_phiprojs_and_start_block_nodes(). part_block() conserves
 * this property.
 *
 * @param node   The node were to break the block
 */
FIRM_API void part_block(ir_node *node);

/**
 * Same as part_block() but works with out-edges so you don't have to call
 * collect_phiprojs.
 * This variant also removes all predecessors of the old block and returns
 * it. You are responsible to add control flow predecessors to it.
 */
FIRM_API ir_node *part_block_edges(ir_node *node);

/**
 * Kill a node.  No other node may have this node as operand.
 */
FIRM_API void kill_node(ir_node *node);

/**
 * Creates a copy of the subgraph starting at node @p n.
 * This currently only works for subgraphs containing only arithmetic nodes
 * (= enough for everything that can be found on the const code irg).
 *
 * @param dbg       debug info for all newly created nodes
 * @param n         the node
 * @param to_block  block to copy to
 */
FIRM_API ir_node *duplicate_subgraph(dbg_info *dbg, ir_node *n,
                                     ir_node *to_block);

#include "end.h"

#endif
