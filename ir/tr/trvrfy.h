

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
 * possible error codes
 */
enum  trvrfy_error_codes {
  no_error = 0,
  error_ent_not_cont = 1,
  error_null_mem,
  error_const_on_wrong_obstack,
};


/**
 * Walks the type information and performs a set of sanity checks.
 *
 * @return
 *    0 if graph is correct
 *    else error code.
 */
int tr_vrfy(void);

#endif /* TRVRFY_H */
