/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief     Implements the Firm interface to debug information.
 * @author    Goetz Lindenmaier, Michael Beck
 * @date      2001
 * @version   $Id$
 * @brief
 *  Firm requires a debugging module fulfilling this interface, else no
 *  debugging information is passed to the backend.
 *  The interface requires a datatype representing the debugging
 *  information.  Firm supports administrating a reference to the debug
 *  information in every Firm node.  Further Firm optimizations call
 *  routines to propagate debug information from old nodes to new nodes
 *  if the optimization replaces the old ones by the new ones.
 */
#ifndef FIRM_DEBUG_DBGINFO_H
#define FIRM_DEBUG_DBGINFO_H

#include <stdlib.h>
#include "firm_types.h"
#include "ident.h"
#include "begin.h"

/**
 * @defgroup debug    The Firm interface to debugging support.
 *
 * @{
 */

/**
 * @typedef dbg_info
 *
 * An abstract data type containing information for
 * debugging support.
 *
 * This opaque data type is not defined anywhere in the Firm library,
 * but pointers to this type can be stored in Firm nodes.
 */

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
	dbg_gvn_pre,                  /**< A Firm node was replace because of the GVN-PRE algorithm. */
	dbg_combo,                    /**< A Firm node was replace because of the combo algorithm. */
	dbg_jumpthreading,            /**< A Firm node was replace because of the jumpthreading algorithm. */
	dbg_backend,                  /**< A Firm subgraph was replaced because of a Backend transformation */
	dbg_max                       /**< Maximum value. */
} dbg_action;

/**
 * Converts a debug_action into a string.
 *
 * @param a  the debug action
 */
FIRM_API const char *dbg_action_2_str(dbg_action a);

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
 *  Initializes the debug support.
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
 */
FIRM_API void dbg_init(merge_pair_func *dbg_info_merge_pair,
                       merge_sets_func *dbg_info_merge_sets);

/** @} */

/**
 * The type of the debug info retriever function.
 *  When given a dbg_info returns the name (usually the filename) of the
 *  compilation unit defining it. @p line is set to the line number of the
 *  definition.
 */
typedef const char *(*retrieve_dbg_func)(const dbg_info *dbg, unsigned *line);

/**
 * Sets a debug info retriever.
 *
 * @param func   the debug retriever function.
 */
FIRM_API void ir_set_debug_retrieve(retrieve_dbg_func func);

/**
 * The type of the type debug info retrieve function.
 * Prints a human readable source representation of a type to an obstack.
 *  (Used for generating debug info like stabs or dwarf)
 */
typedef void (*retrieve_type_dbg_func)(char *buffer, size_t buffer_size,
                                       const type_dbg_info *tdbgi);

/**
 * Set global print_type_dbg_info function in firm
 */
FIRM_API void ir_set_type_debug_retrieve(retrieve_type_dbg_func func);

/**
 * Retrieve the debug info.
 */
FIRM_API const char *ir_retrieve_dbg_info(const dbg_info *dbg, unsigned *line);

/**
 * Retrieve type debug info
 */
FIRM_API void ir_retrieve_type_dbg_info(char *buffer, size_t buffer_size,
                                        const type_dbg_info *tdbgi);

#include "end.h"

#endif
