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
extern merge_pair_func *__dbg_info_merge_pair;

/**
 * The current merge_sets_func(), access only from inside firm.
 */
extern merge_sets_func *__dbg_info_merge_sets;

/**
 * The current snprint_dbg_func(), access only from inside firm.
 */
extern snprint_dbg_func *__dbg_info_snprint;

#endif /* __DBGINFO_T_H__ */
