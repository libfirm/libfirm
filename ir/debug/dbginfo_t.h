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

/**
 * @file
 * @brief      Implements the Firm interface to debug information -- private header.
 * @author     Goetz Lindenmaier
 * @date       2001
 * @version    $Id$
 * @summary
 *  dbginfo: This is a empty implementation of the Firm interface to
 *  debugging support.  It only guarantees that the Firm library compiles
 *  and runs without any real debugging support.
 */
#ifndef FIRM_DEBUG_DBGINFO_T_H
#define FIRM_DEBUG_DBGINFO_T_H

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

#endif
