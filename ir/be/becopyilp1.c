/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

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
