/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** common_t.h: preprocessor flags
*/

/* $Id$ */

# ifndef _COMMON_T_H_
# define _COMMON_T_H_

#include "common.h"

/** Global flags.  Set these by autoconf?? **/

/* When set Phi node construction uses the values valid when the fragile
   operation is executed.  Else it uses the values valid at the end of the
   block with the fragile operation. */
#define PRECISE_EXC_CONTEXT 1
// #define PRECISE_EXC_CONTEXT 0

/* There are two implementations of the Phi node construction.  The first
   is faster, but does not work for blocks with more than 2 predecessors.
   The second works always but is slower and causes more unnecessary Phi
   nodes.
   Select the implementations by the following preprocessor flag: */
#define USE_FAST_PHI_CONSTRUCTION 0

/* Further there are two versions of the fast Phi node construction.
   If the following flag is set, new_r_Phi_in uses an explicit stack for
   allocating and deallocating Phi nodes.  Else it uses the obstack
   as a stack! */
#define USE_EXPLICIT_PHI_IN_STACK 1

/*
/* If this is defined debuging aids are created, e.g. a field in
 * ir_node uniquely numbering the nodes.
 * #define DEBUG_libfirm 1
 * This is now set by the configure script as an option.
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* If this and DEBUG_libfirm are defined irdump uses the nodeid numbers as
   labels for the vcg nodes.  This makes the vcg graph better readable.
   Sometimes it's useful to see the pointer values, though. */
#define NODEID_AS_LABEL 1

# endif /*_COMMON_T_H_ */
