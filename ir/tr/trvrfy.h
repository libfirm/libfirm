

#ifndef TRVRFY_H
#define TRVRFY_H

/**
 * @file trvrfy.h
 *
 * Methods to verify the type representations.
 *
 * @author Goetz Lindenmaier
 *
 * Methods to verify the type representations.
 * Copyright 2003 University of Karlsruhe.
 * Created 29.1.2003.
 *
 * $Id$
 */

#include "firm.h"

/**
 * possible trvrfy() error codes
 */
enum  trvrfy_error_codes {
  no_error                 = 0,		/**< no error */
  error_ent_not_cont       = 1,		/**< overwritten entity not in superclass */
  error_null_mem           = 2,		/**< compound contains NULL member */
  error_const_on_wrong_irg = 3		/**< constant placed on wrong IRG */
};


/**
 * Walks the type information and performs a set of sanity checks.
 *
 * Currently, the following checks are executed:
 * - values of initialized entities must be allocated on the constant IRG
 * - class types: doesn't have NULL members
 * - class types: all overwrites are existant in the super type
 *
 * @return
 *    0 if graph is correct
 *    else error code.
 */
int tr_vrfy(void);

/**
 * If NDEBUG is defined performs nothing, else calles the tr_vrfy() function.
 */
#ifdef NDEBUG
#define TR_VRFY()	0
#else
#define TR_VRFY()	tr_vrfy()
#endif

#endif /* TRVRFY_H */
