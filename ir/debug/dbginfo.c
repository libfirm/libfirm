/*
*  Copyright (C) 2001 by Universitaet Karlsruhe
*  All rights reserved.
*
*  Authors: Goetz Lindenmaier
*
*  dbginfo: This is a empty implementation of the Firm interface to
*  debugging support.  It only guarantees that the Firm library compiles
*  and runs without any real debugging support.
*  The functions herein are declared weak so that they can be overriden
*  by a real implementation.
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dbginfo_t.h"
#include "irnode_t.h"
#include "type_t.h"
#include "entity_t.h"


INLINE void
dbg_info_merge_pair(ir_node *nw, ir_node *old, dbg_action info) {
  set_irn_dbg_info(nw, get_irn_dbg_info(old));
}

INLINE void
dbg_info_merge_sets(ir_node **new_nodes, int n_new_nodes,
		    ir_node **old_nodes, int n_old_nodes,
		    dbg_action info) {
}


void (*__dbg_info_merge_pair)(ir_node *nw, ir_node *old, dbg_action info)
     = &dbg_info_merge_pair;

void (*__dbg_info_merge_sets)(ir_node **new_nodes, int n_new_nodes,
			      ir_node **old_nodes, int n_old_nodes,
			      dbg_action info)
     = &dbg_info_merge_sets;


void dbg_init( merge_pair_func *mpf, merge_sets_func *msf )
{
  __dbg_info_merge_pair = mpf;
  __dbg_info_merge_sets = msf;
}


INLINE void
set_irn_dbg_info(ir_node *n, struct dbg_info* db) {
  n->dbi = db;
}

INLINE struct dbg_info *
get_irn_dbg_info(ir_node *n) {
  return n->dbi;
}


/* Routines to access the field of an entity containing the
   debugging information. */
INLINE void set_entity_dbg_info(entity *ent, dbg_info* db) {
  ent->dbi = db;
}
INLINE dbg_info *get_entity_dbg_info(entity *ent) {
  return ent->dbi;
}
/* Routines to access the field of a type containing the
   debugging information. */
INLINE void set_type_dbg_info(type *tp, dbg_info* db) {
  tp->dbi = db;
}
INLINE dbg_info *get_type_dbg_info(type *tp) {
  return tp->dbi;
}
