/**
 * Author:      Daniel Grund
 * Date:		15.12.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 */

#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"

#include "bearch.h"
#include "benode_t.h"
#include "besched_t.h"
#include "beconstrperm.h"

static void walker_insert_constr_perms(ir_node *irn, void *env) {
	be_chordal_env_t *cenv = env;
	const be_main_env_t *menv = cenv->main_env;
	const arch_env_t *aenv = menv->arch_env;
	arch_register_req_t req;
	int pos, max;

	/* check for a restriction of the result (-1) and the operands (0..n) */
	max = get_irn_arity(irn);
	for(pos=-1; pos<max; ++pos) {
		arch_get_register_req(aenv, &req, irn, pos);
		/* if a restriction is found, insert a perm before the irn */
		if (req.type == arch_register_req_type_limited) {
			insert_Perm_after(menv, cenv->cls, cenv->dom_front, sched_prev(irn));
			break;
		}
	}
}

void be_insert_constr_perms(be_chordal_env_t *cenv) {
	irg_walk_graph(cenv->irg, walker_insert_constr_perms, NULL, cenv);
}
