/*
**  Copyright (C) 2001 by Universitaet Karlsruhe
**  All rights reserved.
**
**  Authors: Goetz Lindenmaier
**
**  dbginfo: This is the Firm interface to debugging support.  Firm requires
**  a debugging module fulfilling this interface.
**  The interface requires a datatype representing the debugging information.
**  Firm supports administrating a reference to the debug information
**  in every firm node.  Further Firm optimizations call routines to
**  propagate debug information from old nodes to new nodes if the optimization
**  replaces the old ones by the new ones.
**
**  This file does not belong to the interface of the firm library.
*/

/* $Id$ */

# ifndef _DBGINFO_H_
# define _DBGINFO_H_

#include "irnode.h"
#include "ident.h"

#ifndef _IR_NODE_TYPEDEF_
#define _IR_NODE_TYPEDEF_
typedef struct ir_node ir_node;
#endif

/* A datastructure containing information for debugging.  */
typedef struct dbg_info dbg_info;
/* Every Firm node contains a reference to a dbg_info struct. This reference
   can be accessed by the debug support module by
   dbg_info *get_irn_dbg_info(irnode *n);
   void      set_irn_dbg_info(irnode *n, dbg_info *d);.
   The module may not touch or change anything else in the Firm data structure.
*/
inline void set_irn_dbg_info(ir_node *n, dbg_info* db);
inline dbg_info *get_irn_dbg_info(ir_node *n);

/** The following routines are called by firm optmizations.  The optimization
    passes an ident representing a string that describes the optimization
    performed.  **/
/* dbg_info_copy() is called in the following situation:
   The optimization replaced the old node by the new one.  The new node
   might be a recent allocated node not containing any debug information,
   or just another node from somewhere in the graph with the same
   semantics. */
void dbg_info_copy(ir_node *nw, ir_node *old, ident *info);

/* dbg_info_merge() is called in the following situation:
   The optimization replaced a subgraph by another subgraph.  There is no
   obviouse mapping between single nodes in both subgraphs.  The optimization
   simply passes two lists to the debug module, one containing the nodes in
   the old subgraph, the other containing the nodes in the new subgraph.  */
void dbg_info_merge(ir_node **new_nodes, ir_node **old_nodes, ident *info);




#endif /* _DBGINFO_H_ */
