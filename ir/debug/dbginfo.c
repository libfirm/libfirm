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

inline void
dbg_info_copy(ir_node *new, ir_node *old, ident *info) {
}

inline void
dbg_info_merge(ir_node **new_nodes, ir_node **old_nodes, ident *info) {
}
