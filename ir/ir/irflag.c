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

#include "irflag.h"
#include "firm_common.h"


/* 0 - don't do this optimization
   1 - lets see, if there is a better graph */
int optimized = 1;                  /* Turn off all optimizations. */

int opt_cse = 1;                    /* Hash the nodes. */
int opt_global_cse = 0;             /* Don't use block predecessor for comparison.
				       Default must be zero as code placement must
				       be run right after a local optimize walk with
				       opt_global_cse on. */
int opt_constant_folding = 1;       /* Evaluate operations. */
int opt_unreachable_code = 1;       /* Bad node propagation. */
int opt_control_flow_straightening = 1;           /*  */
int opt_control_flow_weak_simplification = 1;     /*  */
int opt_control_flow_strong_simplification = 1;   /*  */
int opt_critical_edges = 1;
int opt_dead_node_elimination = 1;  /* Reclaim memory. */
int opt_reassociation = 1;          /* Reassociate nodes. */
int opt_inline = 1;                 /* Do inlining transformation. */
int opt_dyn_meth_dispatch = 1;      /* Remove dynamic method dispatch. */

int opt_normalize = 1;              /* Transformations that normalize the firm representation
				       as removing Ids and Tuples, useless Phis, SymConst(id) ->
				       Const(entity) ... */

/* set the flags with set_flagname, get the flag with get_flagname */
INLINE void
set_opt_cse (int value)
{
  opt_cse = value;
}

INLINE int
get_opt_cse (void)
{
  return opt_cse;
}

void set_opt_global_cse (int value)
{
  opt_global_cse = value;
}

int  get_opt_global_cse (void)
{
  return opt_global_cse;
}

INLINE void
set_opt_constant_folding (int value)
{
  opt_constant_folding=value;
}

INLINE int
get_opt_constant_folding (void)
{
  return opt_constant_folding;
}

INLINE void
set_opt_unreachable_code(int value)
{
  opt_unreachable_code = value;
}

INLINE int
get_opt_unreachable_code(void)
{
  return opt_unreachable_code;
}

INLINE void set_opt_control_flow(int value) {
  set_opt_control_flow_straightening(value);
  set_opt_control_flow_weak_simplification(value);
  set_opt_control_flow_strong_simplification(value);
  set_opt_critical_edges(value);
}

/* Performs Straightening */
void set_opt_control_flow_straightening(int value) {
  opt_control_flow_straightening = value;
}
int  get_opt_control_flow_straightening(void) {
  return opt_control_flow_straightening;
}
/* Performs if simplifications in local optimizations. */
void set_opt_control_flow_weak_simplification(int value) {
  opt_control_flow_weak_simplification = value;
}
int  get_opt_control_flow_weak_simplification(void) {
  return opt_control_flow_weak_simplification;
}
/* Performs strong if and loop simplification (in optimize_cf). */
void set_opt_control_flow_strong_simplification(int value) {
  opt_control_flow_strong_simplification = value;
}
int  get_opt_control_flow_strong_simplification(void) {
  return opt_control_flow_strong_simplification;
}

void set_opt_critical_edges(int value) {
  opt_critical_edges = value;
}
int  get_opt_critical_edges(void) {
  return opt_critical_edges;
}


INLINE void
set_opt_reassociation(int value)
{
  opt_reassociation = value;
}

INLINE int
get_opt_reassociation(void)
{
  return opt_reassociation;
}

INLINE void
set_opt_dead_node_elimination (int value)
{
  opt_dead_node_elimination = value;
}

INLINE int
get_opt_dead_node_elimination (void)
{
  return opt_dead_node_elimination;
}

INLINE void
set_optimize (int value)
{
  optimized = value;
}

INLINE int
get_optimize (void)
{
  return optimized;
}


INLINE void set_opt_inline (int value) {
  opt_inline = value;
}

INLINE int  get_opt_inline (void) {
  return opt_inline;
}

/** Enable/Disable optimization of dynamic method dispatch
 *
 * This flag enables/disables the optimization of dynamic method dispatch.
 * If the flag is turned on Sel nodes can be replaced by Const nodes representing
 * the address of a function.
 */
void set_opt_dyn_meth_dispatch (int value) {
  opt_dyn_meth_dispatch = value;
}
int  get_opt_dyn_meth_dispatch (void) {
  return opt_dyn_meth_dispatch;
}



INLINE void set_opt_normalize (int value) {
  opt_normalize = value;
}

INLINE int  get_opt_normalize (void) {
  return opt_normalize;
}
