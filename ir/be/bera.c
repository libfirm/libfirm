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
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        22.11.2004
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pset.h"
#include "impl.h"

#include "irnode.h"
#include "irmode.h"
#include "irdom.h"
#include "iredges.h"

#include "bera.h"
#include "beutil.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bemodule.h"

be_ra_timer_t *global_ra_timer = NULL;

/** The list of register allocators */
static be_module_list_entry_t *register_allocators = NULL;
static be_ra_t *selected_allocator = NULL;

void be_register_allocator(const char *name, be_ra_t *allocator)
{
	if(selected_allocator == NULL)
		selected_allocator = allocator;
	be_add_module_to_list(&register_allocators, name, allocator);
}

void be_allocate_registers(be_irg_t *birg)
{
	assert(selected_allocator != NULL);
	if(selected_allocator != NULL) {
		selected_allocator->allocate(birg);
	}
}

void be_init_ra(void) {
#ifdef WITH_LIBCORE
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "regalloc", "register allocator",
	                       &register_allocators, (void**) &selected_allocator);
#endif
}
BE_REGISTER_MODULE_CONSTRUCTOR(init_be_ra);
