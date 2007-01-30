/*
 * Author:      Daniel Grund, Sebastian Hack, Matthias Braun
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irtools.h"

#include "bespilloptions.h"
#include "bemodule.h"
#include "be.h"

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>

int be_coalesce_spill_slots = 1;
int be_do_remats = 1;

static const lc_opt_table_entry_t be_spill_options[] = {
	LC_OPT_ENT_BOOL ("coalesce_slots", "coalesce the spill slots", &be_coalesce_spill_slots),
	LC_OPT_ENT_BOOL ("remat", "try to rematerialize values instead of reloading", &be_do_remats),
	{ NULL }
};

static be_module_list_entry_t *spillers = NULL;
static be_spiller_t *selected_spiller = NULL;

void be_register_spiller(const char *name, be_spiller_t *spiller)
{
	if(selected_spiller == NULL)
		selected_spiller = spiller;
	be_add_module_to_list(&spillers, name, spiller);
}

void be_do_spill(be_irg_t *birg, const arch_register_class_t* cls)
{
	assert(selected_spiller != NULL);
	if(selected_spiller != NULL) {
		selected_spiller->spill(birg, cls);
	}
}

void be_init_spill(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
	be_add_module_list_opt(spill_grp, "spiller", "spill algorithm",
	                       &spillers, (void**) &selected_spiller);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spill);
