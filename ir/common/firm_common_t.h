/*
 * Project:     libFIRM
 * File name:   ir/common/firm_common.c
 * Purpose:     Internal preprocessor directives.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file firm_common_t.h
 *
 * preprocessor flags
 *
 * @author Goetz Lindenmaier
 */

# ifndef _COMMON_T_H_
# define _COMMON_T_H_

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "firm_common.h"

/* --- Global flags.  --- */

/** When set Phi node construction uses the values valid when the fragile
 *  operation is executed.  Else it uses the values valid at the end of the
 *  block with the fragile operation. */
#define PRECISE_EXC_CONTEXT 1

/** There are two implementations of the Phi node construction.  The first
 *  is faster, but does not work for blocks with more than 2 predecessors.
 *  The second works always but is slower and causes more unnecessary Phi
 *  nodes.
 *  Select the implementations by the following preprocessor flag: */
#define USE_FAST_PHI_CONSTRUCTION 0

/** Further there are two versions of the fast Phi node construction.
 *  If the following flag is set, new_r_Phi_in uses an explicit stack for
 *  allocating and deallocating Phi nodes.  Else it uses the obstack
 *  as a stack! */
#define USE_EXPLICIT_PHI_IN_STACK 0

/** If this and DEBUG_libfirm are defined irdump uses the nodeid numbers as
 *  labels for the vcg nodes.  This makes the vcg graph better readable.
 *  Sometimes it's useful to see the pointer values, though. */
#define NODEID_AS_LABEL 1

# endif /*_COMMON_T_H_ */
