/*
**  Copyright (C) 2001 by Universitaet Karlsruhe
**  All rights reserved.
**
**  Authors: Goetz Lindenmaier
**
**  dbginfo: This is a empty implementation of the Firm interface to
**  debugging support.  It only guarantees that the Firm library compiles
**  and runs without any real debugging support.
**  The functions herein are declared weak so that they can be overriden
**  by a real implementation.
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dbginfo.h"
#include "irnode_t.h"

inline void
set_irn_dbg_info(ir_node *n, struct dbg_info* db) {
  n->dbi = db;
}

inline struct dbg_info *
get_irn_dbg_info(ir_node *n) {
  return n->dbi;
}

inline void
dbg_info_copy(ir_node *nw, ir_node *old, ident *info) {
  set_irn_dbg_info(new, get_irn_dbg_info(old));
}

inline void
dbg_info_merge(ir_node **new_nodes, ir_node **old_nodes, ident *info) {
}
