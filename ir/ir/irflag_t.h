/*
 * Project:     libFIRM
 * File name:   ir/ir/irgraph.c
 * Purpose:     Flags to control optimizations, inline implementation.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irflag_t.h
 *
 * Inline implementation of Optimization flags.
 *
 * @author Michael Beck
 */
#ifndef _IRFLAG_T_H_
#define _IRFLAG_T_H_

#include "irflag.h"

/**
 * current libFIRM optimizations
 */
typedef enum {
  /** Common subexpression eliminations: Hash the nodes. */
  OPT_CSE                                = 0x00000001,

  /** Don't use block predecessor for comparison.
   *  Default must be zero as code placement must
   *  be run right after a local optimize walk with
   *  opt_global_cse on. */
  OPT_GLOBAL_CSE                         = 0x00000002,

  /** Evaluate operations. */
  OPT_CONSTANT_FOLDING                   = 0x00000004,

  /** Bad node propagation. */
  OPT_UNREACHABLE_CODE                   = 0x00000008,

  /** */
  OPT_CONTROL_FLOW_STRAIGHTENING         = 0x00000010,

  /** */
  OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION   = 0x00000020,

  /** */
  OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION = 0x00000040,

  /** */
  OPT_CRITICAL_EDGES                     = 0x00000080,

  /** Reclaim memory. */
  OPT_DEAD_NODE_ELIMINATION              = 0x00000100,

  /** Reassociate nodes. */
  OPT_REASSOCIATION                      = 0x00000200,

  /** Do inlining transformation. */
  OPT_INLINE                             = 0x00000400,

  /** Remove dynamic method dispatch. */
  OPT_DYN_METH_DISPATCH                  = 0x00000800,

  /** Transformations that normalize the firm representation
   *  as removing Ids and Tuples, useless Phis, SymConst(id) -> Const(entity) ...
   */
  OPT_NORMALIZE                          = 0x00001000,

  /** Turn off all optimizations. */
  OPT_OPTIMIZED                          = 0x40000000
} libfirm_opts_t;

extern optimization_state_t libFIRM_opt;

/* take care of the INLINE/USE_GCC_INLINE mess */

# ifndef INLINE
# ifdef USE_GCC_INLINE
# define INLINE __extension__ ((__inline__))
# else /* defined USE_GCC_INLINE */
# define INLINE
# endif /* define USE_GCC_INLINE */
# endif /* defined INLINE */


/** Returns constant folding optimization setting. */
INLINE int get_opt_cse(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CSE;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns constant subexpression elimination setting. */
INLINE int get_opt_global_cse(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_GLOBAL_CSE;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns global constant subexpression elimination setting. */
INLINE int get_opt_constant_folding(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CONSTANT_FOLDING;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns unreachable code elimination setting. */
INLINE int get_opt_unreachable_code(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_UNREACHABLE_CODE;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns Straightening setting. */
INLINE int get_opt_control_flow_straightening(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CONTROL_FLOW_STRAIGHTENING;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns if simplifications in local optimizations setting. */
INLINE int get_opt_control_flow_weak_simplification(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CONTROL_FLOW_WEAK_SIMPLIFICATION;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns strong if and loop simplification setting */
INLINE int get_opt_control_flow_strong_simplification(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CONTROL_FLOW_STRONG_SIMPLIFICATION;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns whether critical edges are removed */
INLINE int get_opt_critical_edges(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_CRITICAL_EDGES;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns reassociation setting. */
INLINE int get_opt_reassociation(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_REASSOCIATION;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns dead node elimination setting. */
INLINE int get_opt_dead_node_elimination(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_DEAD_NODE_ELIMINATION;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns global optimization setting */
INLINE int get_opt_optimize(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_OPTIMIZED;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

/** Returns inlining setting. */
INLINE int get_opt_inline(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_INLINE;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

INLINE int get_opt_dyn_meth_dispatch(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_DYN_METH_DISPATCH;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

INLINE int get_opt_normalize(void)
# ifdef USE_GCC_INLINE
{
  return libFIRM_opt & OPT_NORMALIZE;
}
# else /* defined USE_GCC_INLINE */
;
# endif /* not defined USE_GCC_INLINE */

#endif /* _IRFLAG_T_H_ */
