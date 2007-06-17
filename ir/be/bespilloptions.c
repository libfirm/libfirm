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
 * @brief       Option handling for spiller.
 * @author      Daniel Grund, Sebastian Hack, Matthias Braun
 * @date        29.09.2005
 * @version     $Id$
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
	LC_OPT_ENT_NULL
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

void be_init_spilloptions(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
	be_add_module_list_opt(spill_grp, "spiller", "spill algorithm",
	                       &spillers, (void**) &selected_spiller);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spilloptions);
