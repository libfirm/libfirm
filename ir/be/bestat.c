#include "irnode_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "dbginfo_t.h"
#include "firmstat.h"
#include "ident.h"

#include "bestat.h"
#include "belive_t.h"
#include "besched.h"

/**
 * Collect reg pressure statistics per block and per class.
 */
static void stat_reg_pressure_block(ir_node *block, void *env) {
	be_irg_t         *birg = env;
	const arch_env_t *aenv = birg->main_env->arch_env;
	int i, n = arch_isa_get_n_reg_class(aenv->isa);

	for (i = 0; i < n; i++) {
		const arch_register_class_t *cls = arch_isa_get_reg_class(aenv->isa, i);
		ir_node  *irn;
		pset     *live_nodes = pset_new_ptr(64);
		int       max_live;

		live_nodes = be_liveness_end_of_block(aenv, cls, block, live_nodes);
		max_live   = pset_count(live_nodes);

		sched_foreach_reverse(block, irn) {
			int cnt;

			live_nodes = be_liveness_transfer(aenv, cls, irn, live_nodes);
			cnt        = pset_count(live_nodes);

			max_live = cnt < max_live ? max_live : cnt;
		}

		hook_be_block_regpressure(block, birg->irg, max_live, new_id_from_str(cls->name));
	}
}

void be_do_stat_reg_pressure(be_irg_t *birg) {
	/* Collect register pressure information for each block */
	irg_block_walk_graph(birg->irg, stat_reg_pressure_block, NULL, birg);
}
