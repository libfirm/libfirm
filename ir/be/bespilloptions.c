/*
 * Author:      Daniel Grund, Sebastian Hack, Matthias Braun
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bespilloptions.h"

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>
#include <libcore/lc_timing.h>
#endif /* WITH_LIBCORE */

int be_coalesce_spill_slots = 1;
int be_do_remats = 1;

#ifdef WITH_LIBCORE
static const lc_opt_table_entry_t be_spill_options[] = {
	LC_OPT_ENT_BOOL ("coalesce_slots", "coalesce the spill slots", &be_coalesce_spill_slots),
	LC_OPT_ENT_BOOL ("remat", "try to rematerialize values instead of reloading", &be_do_remats),
	{ NULL }
};

void be_spill_register_options(lc_opt_entry_t *grp)
{
	static int     run_once = 0;
	lc_opt_entry_t *spill_grp;

	if (run_once)
		return;

	run_once       = 1;
	spill_grp = lc_opt_get_grp(grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
}
#endif /* WITH_LIBCORE */
