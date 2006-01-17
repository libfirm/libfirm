/**
 * Author:      Daniel Grund, Sebastian Hack
 * Date:		15.12.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"

#include "bearch.h"
#include "belive_t.h"
#include "benode_t.h"
#include "besched_t.h"
#include "beconstrperm.h"

static void check_constraints(const be_chordal_env_t *cenv, ir_node *base, ir_node *irn, ir_node **perm)
{
	const arch_env_t *aenv    = cenv->main_env->arch_env;
	const be_main_env_t *menv = cenv->main_env;
	arch_register_req_t req;
	int pos, n;

	assert(is_Proj(irn) || (base == irn));

	for(pos = -1, n = get_irn_arity(irn); pos < n; ++pos) {
		arch_get_register_req(aenv, &req, irn, pos);

		if(arch_irn_has_reg_class(aenv, irn, pos, cenv->cls) && arch_register_req_is(&req, limited)) {

		/*
		* If we inserted a perm,
		* we have to recompute liveness analysis since inserting
		* a Perm changes the liveness situation at the end
		* of the block.
		* (its needed by successive calls to insert_Perm_after)
		* Perhaps thinking about an online liveness analysis
		* would help.
			*/
			if(*perm == NULL) {
				*perm = insert_Perm_after(menv, cenv->cls, cenv->dom_front, sched_prev(base));
				be_liveness(cenv->irg);
			}

			/*
			* Turn an input constraint into an output constraint:
			* The Proj of the Perm which corresponds to the input
			* constraint will have the input constraint of the node
			* as an output constraint
			*/
			if(pos >= 0) {
				ir_node *op = get_irn_n(irn, pos);

				/*
				* The operand must be a proj now, since a perm cut
				* all live ranges.
				*/
				assert(is_Proj(op));
				be_set_Perm_out_req(*perm, get_Proj_proj(op), &req);
			}
		}
	}
}

static void walker_insert_constr_perms(ir_node *bl, void *env) {
	be_chordal_env_t *cenv    = env;
	ir_node *irn;

	for(irn = sched_first(bl); !sched_is_end(irn); irn = sched_next(irn)) {
		ir_node *perm = NULL;

		if(get_irn_mode(irn) == mode_T) {
			ir_node *proj;

			for(proj = sched_next(irn); is_Proj(proj); proj = sched_next(proj))
				check_constraints(cenv, irn, proj, &perm);

			irn = sched_prev(proj);
		}

		else
			check_constraints(cenv, irn, irn, &perm);
	}
}

void be_insert_constr_perms(be_chordal_env_t *cenv) {
	irg_block_walk_graph(cenv->irg, walker_insert_constr_perms, NULL, cenv);
}
