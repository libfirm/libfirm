/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Statistics for Firm.
 * @author  Michael Beck
 */
#ifndef FIRM_STAT_FIRMSTAT_H
#define FIRM_STAT_FIRMSTAT_H

#include "firm_types.h"
#include "begin.h"

/**
 * Dump a snapshot of the statistic values.
 * Never called from libFirm should be called from user.
 *
 * @param fname  base name of the statistic output file
 * @param phase  a phase name. Prefix will be firmstat-\<phase\>-
 */
FIRM_API void stat_dump_snapshot(const char *fname, const char *phase);

/**
 * Creates an ir_prog pass for stat_dump_snapshot().
 *
 * @param name    the name of this pass or NULL
 * @param fname   base name of the statistic output file
 * @param phase   a phase name. Prefix will be firmstat-\<phase\>-
 *
 * @return  the newly created ir_prog pass
 */
FIRM_API ir_prog_pass_t *stat_dump_snapshot_pass(
	const char *name, const char *fname, const char *phase);

/**
 * initialize the statistics module.
 * Should be called directly after ir_init
 */
FIRM_API void firm_init_stat(void);

/**
 * terminates the statistics module, frees all memory
 */
FIRM_API void stat_term(void);

/**
 * returns 1 if statistic module is active, 0 otherwise
 */
FIRM_API int stat_is_active(void);

#include "end.h"

#endif
