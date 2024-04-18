/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Flags to control optimizations.
 * @author  Michael Beck, Sebastian Hack
 */
#include "irflag_t.h"

#include "firm_common.h"
#include "irtools.h"
#include "lc_opts.h"
#include <stdio.h>

/* DISABLE - don't do this optimization
   ENABLE  - lets see, if there is a better graph */
#define ON   -1
#define OFF   0

optimization_state_t libFIRM_opt =
#define FLAG(name, value, def)   (irf_##name & def) |
#include "irflag_t.def"
#undef FLAG
  0;

void set_opt_optimize(int value);
int get_opt_optimize(void);

/* an external flag can be set and get from outside */
#define FLAG(name, value, def)           \
void set_opt_##name(int flag) {            \
  if (flag) libFIRM_opt |= irf_##name;     \
  else      libFIRM_opt &= ~irf_##name;    \
}                                          \
int (get_opt_##name)(void) {               \
  return (libFIRM_opt & irf_##name) != 0;  \
}

/* generate them */
#include "irflag_t.def"

#undef FLAG

void set_optimize(int value)
{
	set_opt_optimize(value);
}

int (get_optimize)(void)
{
	return get_opt_optimize();
}

void save_optimization_state(optimization_state_t *state)
{
	*state = libFIRM_opt;
}

void restore_optimization_state(const optimization_state_t *state)
{
	libFIRM_opt = *state;
}

void all_optimizations_off(void)
{
	libFIRM_opt = 0;
}

static const lc_opt_table_entry_t firm_flags[] = {
#define FLAG(name, val, def) LC_OPT_ENT_BIT(#name, #name, &libFIRM_opt, (1 << val)),
#include "irflag_t.def"
#undef FLAG
	LC_OPT_LAST
};

void firm_init_flags(void)
{
	lc_opt_entry_t *grp = lc_opt_get_grp(firm_opt_get_root(), "opt");
	lc_opt_add_table(grp, firm_flags);
}
