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

#ifndef __DBGINFO_T_H__
#define __DBGINFO_T_H__

#include "dbginfo.h"

extern void (*__dbg_info_merge_pair)(ir_node *nw, ir_node *old,
				     dbg_action info);

extern void (*__dbg_info_merge_sets)(ir_node **new_nodes, int n_new_nodes,
				     ir_node **old_nodes, int n_old_nodes,
				     dbg_action info);

#endif /* __DBGINFO_T_H__ */
