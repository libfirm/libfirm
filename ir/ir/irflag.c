/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Flags to control optimizations.
 * @author  Michael Beck, Sebastian Hack
 */
#include <stdio.h>

#include "lc_opts.h"

#include "firm_common.h"
#include "irtools.h"
#include "irflag_t.h"

/* DISABLE - don't do this optimization
   ENABLE  - lets see, if there is a better graph */
#define ON   -1
#define OFF   0

#define FLAG(name, value, def)     (irf_##name & def) |
#define E_FLAG(name, value, def)    FLAG(name, value, def)
#define I_FLAG(name, value, def)    FLAG(name, value, def)
#define R_FLAG(name, value)

optimization_state_t libFIRM_opt =
#include "irflag_t.def"
  0;

#undef FLAG
#undef E_FLAG
#undef I_FLAG
#undef R_FLAG

optimization_state_t libFIRM_running = 0;

optimization_state_t libFIRM_verb = 0;

void set_opt_optimize(int value);

/* an external flag can be set and get from outside */
#define E_FLAG(name, value, def)           \
void set_opt_##name(int flag) {            \
  if (flag) libFIRM_opt |= irf_##name;     \
  else      libFIRM_opt &= ~irf_##name;    \
}                                          \
int (get_opt_##name)(void) {               \
  return get_opt_##name##_();              \
}

/* an internal flag can only be set from outside */
#define I_FLAG(name, value, def)          \
void set_opt_##name(int flag) {           \
  if (flag) libFIRM_opt |= irf_##name;    \
  else      libFIRM_opt &= ~irf_##name;   \
}                                         \

#define R_FLAG(name, value)

/* generate them */
#include "irflag_t.def"

#undef I_FLAG
#undef E_FLAG
#undef R_FLAG

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
#define I_FLAG(name, val, def) LC_OPT_ENT_BIT(#name, #name, &libFIRM_opt, (1 << val)),
#define E_FLAG(name, val, def) LC_OPT_ENT_BIT(#name, #name, &libFIRM_opt, (1 << val)),
#define R_FLAG(name, val)
#include "irflag_t.def"
#undef I_FLAG
#undef E_FLAG
#undef R_FLAG
	LC_OPT_LAST
};

void firm_init_flags(void)
{
	lc_opt_entry_t *grp = lc_opt_get_grp(firm_opt_get_root(), "opt");
	lc_opt_add_table(grp, firm_flags);
}
