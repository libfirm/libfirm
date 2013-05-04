/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        22.11.2004
 */
#include <stdlib.h>

#include "irnode.h"
#include "irmode.h"
#include "irdom.h"
#include "iredges.h"
#include "irtools.h"

#include "bera.h"
#include "beutil.h"
#include "besched.h"
#include "belive_t.h"
#include "bemodule.h"

/** The list of register allocators */
static be_module_list_entry_t *register_allocators = NULL;
static be_ra_t *selected_allocator = NULL;

void be_register_allocator(const char *name, be_ra_t *allocator)
{
	if (selected_allocator == NULL)
		selected_allocator = allocator;
	be_add_module_to_list(&register_allocators, name, allocator);
}

void be_allocate_registers(ir_graph *irg)
{
	assert(selected_allocator != NULL);
	if (selected_allocator != NULL) {
		selected_allocator->allocate(irg);
	}
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ra)
void be_init_ra(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "regalloc", "register allocator",
	                       &register_allocators, (void**) &selected_allocator);
}
