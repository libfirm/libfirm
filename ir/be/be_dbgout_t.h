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
 * @version $Id: be_dbgout.h 17143 2008-01-02 20:56:33Z beck $
 */
#ifndef FIRM_BE_BE_DBGOUT_T_H
#define FIRM_BE_BE_DBGOUT_T_H

#include "obst.h"
#include "firm_types.h"
#include "be_types.h"

typedef struct dbg_handle dbg_handle;

/**
 * Debug operations.
 */
typedef struct debug_ops {
	/** close the stabs handler. */
	void (*close)(dbg_handle *handle);

	/** start a new source object (compilation unit) */
	void (*so)(dbg_handle *handle, const char *filename);

	/** Main Program */
	void (*main_program)(dbg_handle *handle);

	/** dumps the stabs for a method begin */
	void (*method_begin)(dbg_handle *handle, ir_entity *ent, const be_stack_layout_t *layout);

	/** dumps the stabs for a method end */
	void (*method_end)(dbg_handle *handle);

	/** dump types */
	void (*types)(dbg_handle *handle);

	/** dump a variable in the global type */
	void (*variable)(dbg_handle *h, ir_entity *ent);

	/** notify debug info about position change */
	void (*set_dbg_info)(dbg_handle *h, dbg_info *dbgi);
} debug_ops;

/** The base class of all debug implementations. */
struct dbg_handle {
	const debug_ops *ops;
};

typedef dbg_handle* (*be_create_dbgout_module_func)(void);
void be_register_dbgout_module(const char *name,
                               be_create_dbgout_module_func func);

#endif
