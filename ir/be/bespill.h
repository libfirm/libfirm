/**
 * Author:      Daniel Grund
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef BESPILL_H_
#define BESPILL_H_

#include "set.h"
#include "pset.h"

#include "be_t.h"
#include "irnode.h"

typedef struct _spill_env_t {
	const be_main_session_env_t *session;
	set *spill_ctxs;
	pset *mem_phis;		/**< all phis which must be converted to memory phis */
} spill_env_t;

int be_set_cmp_spillctx(const void *a, const void *b, size_t n);

ir_node *be_spill_node(spill_env_t *senv, ir_node *to_spill);

void be_remove_spilled_phis(spill_env_t *senv);


#endif /*BESPILL_H_*/
