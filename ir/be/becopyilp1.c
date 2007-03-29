/**
 * Author:      Daniel Grund
 * Date:		17.05.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-ID:      $Id$
 *
 * ILP formalization using:
 *   ????
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef WITH_ILP

#include "becopyilp_t.h"

#define DEBUG_LVL 1

typedef struct _my_env_t {
	int dummy;
} my_env_t;


int co_solve_ilp1(copy_opt_t *co, double time_limit) {
	return 1;
}

#else /* WITH_ILP */

static INLINE void only_that_you_can_compile_without_WITH_ILP_defined(void) {
}

#endif /* WITH_ILP */
