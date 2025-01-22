/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Spill module selection; Preparation steps
 * @author      Matthias Braun
 * @date        29.09.2005
 */
#include "bespill.h"

#include "be.h"
#include "beirg.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irtools.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "raw_bitset.h"
#include "statev_t.h"

bool be_coalesce_spill_slots = true;
bool be_do_remats            = true;

static const lc_opt_table_entry_t be_spill_options[] = {
	LC_OPT_ENT_BOOL ("coalesce_slots", "coalesce the spill slots", &be_coalesce_spill_slots),
	LC_OPT_ENT_BOOL ("remat", "try to rematerialize values instead of reloading", &be_do_remats),
	LC_OPT_LAST
};

static be_module_list_entry_t *spillers;
static be_spill_func           selected_spiller;

void be_register_spiller(const char *name, be_spill_func spiller)
{
	if (selected_spiller == NULL)
		selected_spiller = spiller;
	be_add_module_to_list(&spillers, name, spiller);
}

void be_do_spill(ir_graph *irg, const arch_register_class_t *cls,
                 const regalloc_if_t *regif)
{
	selected_spiller(irg, cls, regif);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spilloptions)
void be_init_spilloptions(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
	be_add_module_list_opt(spill_grp, "algo", "spill algorithm", &spillers, (void**)&selected_spiller);
}
