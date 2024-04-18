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
#include "bera.h"

#include "bemodule.h"
#include "irtools.h"

/** The list of register allocators */
static be_module_list_entry_t *register_allocators;
static allocate_func           selected_allocator;

void be_register_allocator(const char *name, allocate_func allocator)
{
	if (selected_allocator == NULL)
		selected_allocator = allocator;
	be_add_module_to_list(&register_allocators, name, allocator);
}

void be_allocate_registers(ir_graph *irg, const regalloc_if_t *regif)
{
	selected_allocator(irg, regif);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ra)
void be_init_ra(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "regalloc", "register allocator",
	                       &register_allocators, (void**) &selected_allocator);
}
