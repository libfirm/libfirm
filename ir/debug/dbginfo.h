/*
 * Project:     libFIRM
 * File name:   ir/debug/dbginfo.h
 * Purpose:     Implements the Firm interface to debug information.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file  dbginfo.h
*
*  This is the Firm interface to debugging support.
*
*  @author Goetz Lindenmaier
*
*  Firm requires a debugging module fulfilling this interface, else no
*  debugging information is passed to the backend.
*  The interface requires a datatype representing the debugging
*  information.  Firm supports administrating a reference to the debug
*  information in every firm node.  Further Firm optimizations call
*  routines to propagate debug information from old nodes to new nodes
*  if the optimization replaces the old ones by the new ones.
*
*/

# ifndef _DBGINFO_H_
# define _DBGINFO_H_

#include "ident.h"

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/* to resolve recursion between entity.h and type.h */
#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
typedef struct entity entity;
#endif

#ifndef _TYPE_TYPEDEF_
#define _TYPE_TYPEDEF_
typedef struct type type;
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
 * This datatype is not defined anywere in the firm library, but pointers
 * to this type can be stored in firm nodes.
 */
typedef struct dbg_info dbg_info;

/**
 * Sets the debug information of a node.
 */
INLINE void set_irn_dbg_info(ir_node *n, dbg_info* db);

/**
 * Returns the debug information of an node.
 */
INLINE dbg_info *get_irn_dbg_info(ir_node *n);

/**
 * Sets the debug information of an entity.
 */
INLINE void set_entity_dbg_info(entity *ent, dbg_info* db);

/**
 * Returns the debug information of an entity.
 */
INLINE dbg_info *get_entity_dbg_info(entity *ent);

/**
 * Sets the debug information of a type.
 */
INLINE void set_type_dbg_info(type *tp, dbg_info* db);

/**
 * Returns the debug information of a type.
 */
INLINE dbg_info *get_type_dbg_info(type *tp);

/**
 * An enumeration indicating the action performed by a transformation.
 */
typedef enum {
  dbg_error = 0,
  dbg_opt_ssa,           /**< Optimization of the SSA representation, e.g., removal of superfluent phi nodes. */
  dbg_opt_auxnode,       /**< Removal of unnecessary auxilliary nodes. */
  dbg_const_eval,        /**< A Firm subgraph was evaluated to a single constant. */
  dbg_straightening,     /**< A Firm subgraph was replaced by a single, existing block. */
  dbg_if_simplification, /**< The control flow of an if is changed as either the
			                        else, the then or both blocks are empty. */
  dbg_algebraic_simplification, /**< A Firm subgraph was replaced because of an algebraic
                                     simplification. */
  dbg_write_after_write,        /**< A Firm subgraph was replaced because of a write
                                     after write optimization. */
  dbg_write_after_read,         /**< A Firm subgraph was replaced because of a write
                                     after read optimization. */
  dbg_rem_poly_call,            /**< Remove polymorphic call. */
  dbg_max                       /**< Maximum value. */

} dbg_action;


/**
 * Converts enum values to strings.
 */
#ifdef __GNUC__
INLINE static const char* dbg_action_2_str(dbg_action) __attribute__ ((unused));
#endif

INLINE static const char* dbg_action_2_str(dbg_action a) {
  switch(a) {
  case dbg_error: return "dbg_error"; break;
  case dbg_opt_ssa: return "dbg_opt_ssa"; break;
  case dbg_opt_auxnode: return "dbg_opt_auxnode"; break;
  case dbg_const_eval: return "dbg_const_eval"; break;
  case dbg_straightening: return "dbg_straightening"; break;
  case dbg_if_simplification: return "dbg_if_simplification"; break;
  case dbg_algebraic_simplification:
    return "dbg_algebraic_simplification"; break;
  case dbg_write_after_write: return "dbg_write_after_write"; break;
  case dbg_write_after_read: return "dbg_write_after_read"; break;
  case dbg_rem_poly_call: return "dbg_rem_poly_call"; break;
  default:
    if (a <= dbg_max)
      return "string conversion not implemented";
    else
      assert(0);
    return NULL;
  }
}

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
 *  @param dbg_info_merge_pair   see function description
 *  @param dbg_info_merge_sets   see function description
 *
 *  This function takes Pointers to two functions that merge the
 *  debug information when a
 *  transformation of a firm graph is performed.
 *  Firm transformations call one of these functions.
 *
 *   - dbg_info_merge_pair() is called in the following situation:
 *     The optimization replaced the old node by the new one.  The new node
 *     might be a recent allocated node not containing any debug information,
 *     or just another node from somewhere in the graph with the same
 *     semantics.
 *   - dbg_info_merge_sets() is called in the following situation:
 *     The optimization replaced a subgraph by another subgraph.  There is no
 *     obviouse mapping between single nodes in both subgraphs.  The optimization
 *     simply passes two lists to the debug module, one containing the nodes in
 *     the old subgraph, the other containing the nodes in the new subgraph.
 *     The same node can be in both lists.
 *
 *   Further both functions pass an enumeration indicating the action
 *   performed by the transformation, e.g. the kind of optimization performed.
 */
void dbg_init(merge_pair_func *dbg_info_merge_pair, merge_sets_func *dbg_info_merge_sets);

/** @} */

#endif /* _DBGINFO_H_ */
