/*
**  Copyright (C) 2001 by Universitaet Karlsruhe
**  All rights reserved.
**
**  Authors: Goetz Lindenmaier
**
**  dbginfo: This is the Firm interface to debugging support.  Firm requires
**  a debugging module fulfilling this interface, else no debugging information
**  is passed to the backend.
**  The interface requires a datatype representing the debugging information.
**  Firm supports administrating a reference to the debug information
**  in every firm node.  Further Firm optimizations call routines to
**  propagate debug information from old nodes to new nodes if the optimization
**  replaces the old ones by the new ones.
**
*/

/* $Id$ */

# ifndef _DBGINFO_H_
# define _DBGINFO_H_

#include "ident.h"

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
/* to resolve recursion between entity.h and type.h */
typedef struct entity entity;
#endif

#ifndef _TYPE_TYPEDEF_
#define _TYPE_TYPEDEF_
typedef struct type type;
#endif

/****s* dbginfo/dbg_info
 *
 * NAME
 *   dbg_info - An abstract data type containing information for
 *   debugging support.
 * NOTE
 *   This datatype is not defined in the firm library, but pointers
 *   to this type can be stored in firm nodes.
 * SEE ALSO
 * SOURCE
 */
typedef struct dbg_info dbg_info;
/* Routines to access the field of an ir node containing the
   debugging information. */
INLINE void set_irn_dbg_info(ir_node *n, dbg_info* db);
INLINE dbg_info *get_irn_dbg_info(ir_node *n);
/* Routines to access the field of an entity containing the
   debugging information. */
INLINE void set_entity_dbg_info(entity *ent, dbg_info* db);
INLINE dbg_info *get_entity_dbg_info(entity *ent);
/* Routines to access the field of a type containing the
   debugging information. */
INLINE void set_type_dbg_info(type *tp, dbg_info* db);
INLINE dbg_info *get_type_dbg_info(type *tp);
/*****/

/****s* dbginfo/dbg_action
 *
 * NAME
 *   dbg_action - An enumeration indicating the action performed by a
 *   transformation.
 * NOTE
 * SOURCE
 */
typedef enum {
  dbg_error = 0,
  dbg_opt_ssa,        /* Optimization of the SSA representation, e.g., removal
			 of superfluent phi nodes. */
  dbg_opt_auxnode,    /* Removal of unnecessary auxilliary nodes. */
  dbg_const_eval,     /* A Firm subgraph was evaluated to a single constant. */
  dbg_straightening,  /* A Firm subgraph was replaced by a single, existing
			 block. */
  dbg_if_simplification, /* The control flow of an if is changed as either the
			    else, the then or both blocks are empty. */
  dbg_algebraic_simplification,
  dbg_write_after_write,
  dbg_write_after_read,
  dbg_max
} dbg_action;
/*****/


/****f* dbginfo/dbg_action_2_str
 *
 * NAME
 *   dbg_action_2_str - converts enum to string
 * SYNOPSIS
 * INPUTS
 * RESULT
 ***
 */
static const char* dbg_action_2_str(dbg_action a) {
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
  default:
    if (a <= dbg_max)
      return "string conversion not implemented";
    else
      assert(0);
  }
}


/****f* dbginfo/dbg_init
 *
 * NAME
 *   dbg_init - initializes the debug support.
 * SYNOPSIS
 *   void dbg_init( void (merge_pair)(ir_node *nw, ir_node *old, dbg_action info),
 *	            void (merge_sets)(ir_node **new_nodes, int n_new_nodes,
 *                                    ir_node **old_nodes, int n_old_nodes,
 *		      		      dbg_action info));
 * INPUTS
 *   Pointers to two functions that merge the debug information when a
 *   transformation of a firm graph is performed.
 *   Firm transformations call one of these functions.
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
 *   Further both functions pass an enumeration indicating the action
 *   performed by the transformation, e.g. the kind of optimization performed.
 * RESULT
 *   No result.
 ***
 */
void dbg_init( void (merge_pair)(ir_node *nw, ir_node *old, dbg_action info),
	       void (merge_sets)(ir_node **new_nodes, int n_new_nodes,
				 ir_node **old_nodes, int n_old_nodes,
				 dbg_action info)
	       );


#endif /* _DBGINFO_H_ */
