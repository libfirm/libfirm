/*
 * Project:     libFIRM
 * File name:   ir/ir/irflag.c
 * Purpose:     Flags to control optimizations.
 * Author:      Christian Schaefer, Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "firm_common.h"
#include "irflag_t.h"

/* DISABLE - don't do this optimization
   ENABLE  - lets see, if there is a better graph */
#define ON	(-1)
#define OFF  (0)

#define FLAG(name, value, def)	(irf_##name & def) |
#define E_FLAG(name, value, def)	FLAG(name, value, def)
#define I_FLAG(name, value, def)	FLAG(name, value, def)

optimization_state_t libFIRM_opt =
#include "irflag_t.def"
  0;

#undef FLAG
#undef E_FLAG
#undef I_FLAG


/* verbose is always off on default */
optimization_state_t libFIRM_verb = 0;

/** The Firm verbosity level */
int firm_verbosity_level;

/* an external flag can be set and get from outside */
#define E_FLAG(name, value, def)           \
void set_opt_##name(int flag) {            \
  if (flag) libFIRM_opt |= irf_##name;     \
  else      libFIRM_opt &= ~irf_##name;    \
}                                          \
void set_opt_##name##_verbose(int flag) {  \
  if (flag) libFIRM_verb |= irf_##name;    \
  else      libFIRM_verb &= ~irf_##name;   \
}                                          \
int (get_opt_##name)(void) {               \
  return _get_opt_##name();                \
}

/* an internal flag can only be set from outside */
#define I_FLAG(name, value, def)          \
void set_opt_##name(int flag) {           \
  if (flag) libFIRM_opt |= irf_##name;    \
  else      libFIRM_opt &= ~irf_##name;   \
}                                         \
void set_opt_##name##_verbose(int flag) { \
  if (flag) libFIRM_verb |= irf_##name;   \
  else      libFIRM_verb &= ~irf_##name;  \
}

/* generate them */
#include "irflag_t.def"

#undef I_FLAG
#undef E_FLAG

/* for compatibility reasons */
void set_optimize(int value) {
  if (value) libFIRM_opt |= irf_optimize;
  else       libFIRM_opt &= ~irf_optimize;
}

int (get_optimize)(void) {
  return get_opt_optimize();
}

void set_opt_control_flow(int value)
{
  set_opt_control_flow_straightening(value);
  set_opt_control_flow_weak_simplification(value);
  set_opt_control_flow_strong_simplification(value);
  set_opt_critical_edges(value);
}

void set_firm_verbosity (int value) {
  firm_verbosity_level = value;
}

int  (get_firm_verbosity) (void) {
  return _get_firm_verbosity();
}

/* Save the current optimization state. */
void save_optimization_state(optimization_state_t *state)
{
  *state = libFIRM_opt;
}

/* Restore the current optimization state. */
void restore_optimization_state(const optimization_state_t *state)
{
  libFIRM_opt = *state;
}

#ifdef _DEBUG
void firm_show_flags(void) {
#define E_FLAG(name, value, def) printf(#name " = %s\n", get_opt_##name() ? "ON" : "OFF");
#define I_FLAG(name, value, def) printf(#name " = %s\n", get_opt_##name() ? "ON" : "OFF");
#include "irflag_t.def"
}
#endif
