/*
 * Project:     libFIRM
 * File name:   ir/debug/dbginfo.h
 * Purpose:     Implements the Firm interface to debug information -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file dbginfo_t.h
*
* @author Goetz Lindenmaier
*
*  dbginfo: This is a empty implementation of the Firm interface to
*  debugging support.  It only guarantees that the Firm library compiles
*  and runs without any real debugging support.
*/


#ifndef __DBGINFO_T_H__
#define __DBGINFO_T_H__

#include "dbginfo.h"

/**
 * The current merge_pair_func(), access only from inside firm.
 */
extern void (*__dbg_info_merge_pair)(ir_node *nw, ir_node *old,
				     dbg_action info);

/**
 * The current merge_sets_func(), access only from inside firm.
 */
extern void (*__dbg_info_merge_sets)(ir_node **new_nodes, int n_new_nodes,
				     ir_node **old_nodes, int n_old_nodes,
				     dbg_action info);

#endif /* __DBGINFO_T_H__ */
