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
# include <config.h>
#endif

#include "firm_common.h"
#include "irflag_t.h"

/* DISABLE - don't do this optimization
   ENABLE  - lets see, if there is a better graph */
#define ENABLE(a)   a
#define DISABLE(a)  0

optimization_state_t libFIRM_opt =
  ENABLE(OPT_OPTIMIZED)                          |
  ENABLE(OPT_CSE)                                |
  DISABLE(OPT_GLOBAL_CSE)                        |
  ENABLE(OPT_STRENGTH_RED)                       |
  ENABLE(OPT_CONSTANT_FOLDING)                   |
  ENABLE(OPT_REDUNDANT_LOADSTORE)                |
  ENABLE(OPT_UNREACHABLE_CODE)                   |
  ENABLE(OPT_CONTROL_FLOW_STRAIGHTENING)         |
  ENABLE(OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION)   |
  ENABLE(OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION) |
  ENABLE(OPT_CRITICAL_EDGES)                     |
  ENABLE(OPT_DEAD_NODE_ELIMINATION)              |
  ENABLE(OPT_DEAD_METHOD_ELIMINATION)            |
  ENABLE(OPT_REASSOCIATION)                      |
  ENABLE(OPT_INLINE)                             |
  ENABLE(OPT_DYN_METH_DISPATCH)                  |
  ENABLE(OPT_NORMALIZE)                          |
  ENABLE(OPT_TAIL_RECURSION)                     |
  ENABLE(OPT_PRECISE_EXC_CONTEXT)                |
  DISABLE(OPT_FRAGILE_OPS)                       |
  0;

optimization_state_t libFIRM_verb =
  DISABLE(OPT_OPTIMIZED)                          |
  DISABLE(OPT_CSE)                                |
  DISABLE(OPT_GLOBAL_CSE)                         |
  DISABLE(OPT_STRENGTH_RED)                       |
  DISABLE(OPT_CONSTANT_FOLDING)                   |
  DISABLE(OPT_REDUNDANT_LOADSTORE)                |
  DISABLE(OPT_UNREACHABLE_CODE)                   |
  DISABLE(OPT_CONTROL_FLOW_STRAIGHTENING)         |
  DISABLE(OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION)   |
  DISABLE(OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION) |
  DISABLE(OPT_CRITICAL_EDGES)                     |
  DISABLE(OPT_DEAD_NODE_ELIMINATION)              |
  DISABLE(OPT_DEAD_METHOD_ELIMINATION)            |
  DISABLE(OPT_REASSOCIATION)                      |
  DISABLE(OPT_INLINE)                             |
  DISABLE(OPT_DYN_METH_DISPATCH)                  |
  DISABLE(OPT_NORMALIZE)                          |
  DISABLE(OPT_TAIL_RECURSION)                     |
  DISABLE(OPT_PRECISE_EXC_CONTEXT)                |
  DISABLE(OPT_FRAGILE_OPS)                       |
  0;

/** The Firm verbosity level */
int firm_verbosity_level;

/* set the flags with set_flagname, get the flag with get_flagname */
void set_opt_cse (int value)
{
  if (value)
    libFIRM_opt |= OPT_CSE;
  else
    libFIRM_opt &= ~OPT_CSE;
}

void set_opt_global_cse(int value)
{
  if (value)
    libFIRM_opt |= OPT_GLOBAL_CSE;
  else
    libFIRM_opt &= ~OPT_GLOBAL_CSE;
}

void set_opt_strength_red (int value)
{
  if (value)
    libFIRM_opt |= OPT_STRENGTH_RED;
  else
    libFIRM_opt &= ~OPT_STRENGTH_RED;
}

void set_opt_strength_red_verbose (int value)
{
  if (value)
    libFIRM_verb |= OPT_STRENGTH_RED;
  else
    libFIRM_verb &= ~OPT_STRENGTH_RED;
}

void
set_opt_constant_folding(int value)
{
  if (value)
    libFIRM_opt |= OPT_CONSTANT_FOLDING;
  else
    libFIRM_opt &= ~OPT_CONSTANT_FOLDING;
}

void
set_opt_redundant_LoadStore(int value) {
  if (value)
    libFIRM_opt |= OPT_REDUNDANT_LOADSTORE;
  else
    libFIRM_opt &= ~OPT_REDUNDANT_LOADSTORE;
}

void
set_opt_unreachable_code(int value)
{
  if (value)
    libFIRM_opt |= OPT_UNREACHABLE_CODE;
  else
    libFIRM_opt &= ~OPT_UNREACHABLE_CODE;
}

void set_opt_control_flow(int value)
{
  set_opt_control_flow_straightening(value);
  set_opt_control_flow_weak_simplification(value);
  set_opt_control_flow_strong_simplification(value);
  set_opt_critical_edges(value);
}

/* Performs Straightening */
void set_opt_control_flow_straightening(int value)
{
  if (value)
    libFIRM_opt |= OPT_CONTROL_FLOW_STRAIGHTENING;
  else
    libFIRM_opt &= ~OPT_CONTROL_FLOW_STRAIGHTENING;
}

/* Performs if simplifications in local optimizations. */
void set_opt_control_flow_weak_simplification(int value)
{
  if (value)
    libFIRM_opt |= OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION;
  else
    libFIRM_opt &= ~OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION;
}

/* Performs strong if and loop simplification (in optimize_cf). */
void set_opt_control_flow_strong_simplification(int value)
{
  if (value)
    libFIRM_opt |= OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION;
  else
    libFIRM_opt &= ~OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION;
}

void set_opt_critical_edges(int value)
{
  if (value)
    libFIRM_opt |= OPT_CRITICAL_EDGES;
  else
    libFIRM_opt &= ~OPT_CRITICAL_EDGES;
}

void set_opt_reassociation(int value)
{
  if (value)
    libFIRM_opt |= OPT_REASSOCIATION;
  else
    libFIRM_opt &= ~OPT_REASSOCIATION;
}

void set_opt_dead_node_elimination(int value)
{
  if (value)
    libFIRM_opt |= OPT_DEAD_NODE_ELIMINATION;
  else
    libFIRM_opt &= ~OPT_DEAD_NODE_ELIMINATION;
}

void set_opt_dead_method_elimination (int value) {
  if (value)
    libFIRM_opt |= OPT_DEAD_METHOD_ELIMINATION;
  else
    libFIRM_opt &= ~OPT_DEAD_METHOD_ELIMINATION;
}

void set_opt_dead_method_elimination_verbose (int value) {
  if (value)
    libFIRM_verb |= OPT_DEAD_METHOD_ELIMINATION;
  else
    libFIRM_verb &= ~OPT_DEAD_METHOD_ELIMINATION;
}

void set_optimize(int value)
{
  if (value)
    libFIRM_opt |= OPT_OPTIMIZED;
  else
    libFIRM_opt &= ~OPT_OPTIMIZED;
}

int get_optimize(void)
{
  return get_opt_optimize();
}


void set_firm_verbosity (int value) {
  firm_verbosity_level = value;
}

int  (get_firm_verbosity) (void) {
  return _get_firm_verbosity();
}



/* Enable/Disables inlining. */
void set_opt_inline(int value)
{
  if (value)
    libFIRM_opt |= OPT_INLINE;
  else
    libFIRM_opt &= ~OPT_INLINE;
}

/* Enable/Disable optimization of dynamic method dispatch */
void set_opt_dyn_meth_dispatch (int value)
{
  if (value)
    libFIRM_opt |= OPT_DYN_METH_DISPATCH;
  else
    libFIRM_opt &= ~OPT_DYN_METH_DISPATCH;
}

/* Enable/Disable normalizations of the firm representation. */
void set_opt_normalize(int value)
{
  if (value)
    libFIRM_opt |= OPT_NORMALIZE;
  else
    libFIRM_opt &= ~OPT_NORMALIZE;
}

/* Enable/Disable optimization of tail-recursion calls. */
void set_opt_tail_recursion(int value)
{
  if (value)
    libFIRM_opt |= OPT_TAIL_RECURSION;
  else
    libFIRM_opt &= ~OPT_TAIL_RECURSION;
}

/* Enable/Disable optimization of tail-recursion calls. */
void set_opt_tail_recursion_verbose(int value)
{
  if (value)
    libFIRM_verb |= OPT_TAIL_RECURSION;
  else
    libFIRM_verb &= ~OPT_TAIL_RECURSION;
}

/* Enable/Disable precise exception context. */
void set_opt_precise_exc_context(int value)
{
#if PRECISE_EXC_CONTEXT
  if (value)
    libFIRM_opt |= OPT_PRECISE_EXC_CONTEXT;
  else
    libFIRM_opt &= ~OPT_PRECISE_EXC_CONTEXT;
#endif
}

void set_opt_fragile_ops(int value)
{
  if (value)
    libFIRM_opt |= OPT_FRAGILE_OPS;
  else
    libFIRM_opt &= ~OPT_FRAGILE_OPS;
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
