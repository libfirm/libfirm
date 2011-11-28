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
 * @brief   Debug output support.
 * @author  Michael Beck
 * @date    11.9.2006
 */
#ifndef FIRM_BE_BE_DBGOUT_H
#define FIRM_BE_BE_DBGOUT_H

#include "beabi.h"

/* initialize and open debug handle */
void be_dbg_open(void);

/** close a debug handler. */
void be_dbg_close(void);

/** start a compilation unit */
void be_dbg_unit_begin(const char *filename);

/** end compilation unit */
void be_dbg_unit_end(void);

/** debug for a method begin */
void be_dbg_method_begin(const ir_entity *ent);

/** debug for a method end */
void be_dbg_method_end(void);

/** dump types */
void be_dbg_types(void);

/** dump a variable in the global type */
void be_dbg_variable(const ir_entity *ent);

void be_dbg_location(dbg_info *dbgi);

#endif
