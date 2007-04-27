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
 * File name:   ir/debug/dbginfo.h
 * Purpose:     Implements the Firm interface to debug information.
 * Author:      Goetz Lindenmaier
 * Modified by: Michael Beck
 * Created:     2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 */

/**
 * @file  dbginfo.h
 *
 *  This is the Firm interface to debugging support.
 *
 *  @author Goetz Lindenmaier, Michael Beck
 *
 *  Firm requires a debugging module fulfilling this interface, else no
 *  debugging information is passed to the backend.
 *  The interface requires a datatype representing the debugging
 *  information.  Firm supports administrating a reference to the debug
 *  information in every Firm node.  Further Firm optimizations call
 *  routines to propagate debug information from old nodes to new nodes
 *  if the optimization replaces the old ones by the new ones.
 *
 */

#ifndef _DBGINFO_H_
#define _DBGINFO_H_

#include "firm_types.h"
#include "ident.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @defgroup debug    The Firm interface to debugging support.
 *
 * @{
 */

/**
 * An abstract data type containing information for
 * debugging support.
 *
 * This opaque data type is not defined anywhere in the Firm library,
 * but pointers to this type can be stored in Firm nodes.
 */
typedef struct dbg_info dbg_info;

/**
 * Sets the debug information of a node.
 *
 * @param n   The node.
 * @param db  The debug info.
 */
void set_irn_dbg_info(ir_node *n, dbg_info *db);

/**
 * Returns the debug information of an node.
 *
 * @param n   The node.
 */
dbg_info *get_irn_dbg_info(const ir_node *n);

/**
 * Sets the debug information of an entity.
 *
 * @param ent The entity.
 * @param db  The debug info.
 */
void set_entity_dbg_info(ir_entity *ent, dbg_info *db);

/**
 * Returns the debug information of an entity.
 *
 * @param ent The entity.
 */
dbg_info *get_entity_dbg_info(const ir_entity *ent);

/**
 * Sets the debug information of a type.
 *
 * @param tp  The type.
 * @param db  The debug info.
 */
void set_type_dbg_info(ir_type *tp, dbg_info *db);

/**
 * Returns the debug information of a type.
 *
 * @param tp  The type.
 */
dbg_info *get_type_dbg_info(const ir_type *tp);

/**
 * An enumeration indicating the action performed by a transformation.
 */
typedef enum {
  dbg_error = 0,
  dbg_opt_ssa,           /**< Optimization of the SSA representation, e.g. removal of superfluent Phi nodes. */
  dbg_opt_auxnode,       /**< Removal of unnecessary auxiliary nodes. */
  dbg_const_eval,        /**< A Firm subgraph was evaluated to a single constant. */
  dbg_opt_cse,           /**< A Firm node was replaced due to common subexpression elimination. */
  dbg_straightening,     /**< A Firm subgraph was replaced by a single, existing block. */
  dbg_if_simplification, /**< The control flow of an if is changed as either the
                                    else, the then or both blocks are empty. */
  dbg_algebraic_simplification, /**< A Firm subgraph was replaced because of an algebraic
                                     simplification. */
  dbg_write_after_write,        /**< A Firm subgraph was replaced because of a write
                                     after write optimization. */
  dbg_write_after_read,         /**< A Firm subgraph was replaced because of a write
                                     after read optimization. */
  dbg_read_after_write,         /**< A Firm subgraph was replaced because of a read
                                     after write optimization. */
  dbg_read_after_read,          /**< A Firm subgraph was replaced because of a read
                                     after read optimization. */
  dbg_read_a_const,             /**< A Firm subgraph was replaced because of a read
                                     a constant optimization. */
  dbg_rem_poly_call,            /**< Remove polymorphic call. */
  dbg_dead_code,                /**< Removing unreachable code, I.e. blocks that are never executed. */
  dbg_opt_confirm,              /**< A Firm subgraph was replace because of a Confirmation. */
  dbg_backend,                  /**< A Firm subgraph was replaced because of a Backend transformation */
  dbg_max                       /**< Maximum value. */
} dbg_action;

/**
 * Converts a debug_action into a string.
 *
 * @param a  the debug action
 */
const char *dbg_action_2_str(dbg_action a);

/**
 * The type of the debug info merge function.
 *
 * @param new_node    the new ir node
 * @param old_node    the old ir node
 * @param action      the action that triggers the merge
 *
 * @see dbg_init()
 */
typedef void merge_pair_func(ir_node *new_node, ir_node *old_node, dbg_action action);

/**
 * The type of the debug info merge sets function.
 *
 * @param new_node_array    array of new nodes
 * @param new_num_entries   number of entries in new_node_array
 * @param old_node_array    array of old nodes
 * @param old_num_entries   number of entries in old_node_array
 * @param action            the action that triggers the merge
 *
 * @see dbg_init()
 */
typedef void merge_sets_func(ir_node **new_node_array, int new_num_entries, ir_node **old_node_array, int old_num_entries, dbg_action action);

/**
 * The type of the debug info to human readable string function.
 *
 * @param buf    pointer to a buffer that will hold the info
 * @param len    length of the buffer
 * @param dbg    the debug info
 *
 * @return  Number of written characters to the buffer.
 *
 * @see dbg_init()
 */
typedef unsigned snprint_dbg_func(char *buf, unsigned len, const dbg_info *dbg);

/**
 *  Initializes the debug support.
 *
 *  @param dbg_info_merge_pair   see function description
 *  @param dbg_info_merge_sets   see function description
 *  @param snprint_dbg           see function description
 *
 *  This function takes pointers to two functions that merge the
 *  debug information when a
 *  transformation of a Firm graph is performed.
 *  Firm transformations call one of these functions.
 *
 *   - dbg_info_merge_pair() is called in the following situation:
 *     The optimization replaced the old node by the new one.  The new node
 *     might be a recent allocated node not containing any debug information,
 *     or just another node from somewhere in the graph with the same
 *     semantics.
 *   - dbg_info_merge_sets() is called in the following situation:
 *     The optimization replaced a subgraph by another subgraph.  There is no
 *     obviously mapping between single nodes in both subgraphs.  The optimization
 *     simply passes two lists to the debug module, one containing the nodes in
 *     the old subgraph, the other containing the nodes in the new subgraph.
 *     The same node can be in both lists.
 *
 *   Further both functions pass an enumeration indicating the action
 *   performed by the transformation, e.g. the kind of optimization performed.
 *
 * The third argument snprint_dbg is called to convert a debug info into a human readable string.
 * This string is the dumped in the dumper functions.
 *
 * Note that if NULL is passed for dbg_info_merge_pair or dbg_info_merge_sets, the default
 * implementations default_dbg_info_merge_pair() and default_dbg_info_merge_sets() are used.
 * NULL passed for snprint_dbg means no output.
 */
void dbg_init(merge_pair_func *dbg_info_merge_pair, merge_sets_func *dbg_info_merge_sets, snprint_dbg_func *snprint_dbg);

/**
 * The default merge_pair_func implementation, simply copies the debug info
 * from the old Firm node to the new one if the new one does not have debug info yet.
 *
 * @param nw    The new Firm node.
 * @param old   The old Firm node.
 * @param info  The action that cause old node to be replaced by new one.
 */
void default_dbg_info_merge_pair(ir_node *nw, ir_node *old, dbg_action info);

/**
 * The default merge_sets_func implementation.  If n_old_nodes is equal 1, copies
 * the debug info from the old node to all new ones (if they do not have one), else does nothing.
 *
 * @param new_nodes     An array of new Firm nodes.
 * @param n_new_nodes   The length of the new_nodes array.
 * @param old_nodes     An array of old (replaced) Firm nodes.
 * @param n_old_nodes   The length of the old_nodes array.
 * @param info          The action that cause old node to be replaced by new one.
 */
void default_dbg_info_merge_sets(ir_node **new_nodes, int n_new_nodes,
                            ir_node **old_nodes, int n_old_nodes,
                            dbg_action info);

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* _DBGINFO_H_ */
