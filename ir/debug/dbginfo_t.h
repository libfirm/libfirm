/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

/*
 * Project:     libFIRM
 * File name:   ir/debug/dbginfo.h
 * Purpose:     Implements the Firm interface to debug information -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
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
