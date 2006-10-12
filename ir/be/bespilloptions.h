/*
 * Author:      Matthias Braun
 * Date:		12.10.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILL_OPTIONS_H_
#define BESPILL_OPTIONS_H_

extern int be_coalesce_spill_slots;
extern int be_do_remats;

#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
void be_spill_register_options(lc_opt_entry_t *grp);
#endif

#endif
