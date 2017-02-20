/*
 * This file is part of libFirm.
 * Copyright (C) 2013 University of Karlsruhe.
 */

/**
 * @brief   optimize nodes to const, where non-const bits are irrelevant
 * @author  Andreas Seltenreich, Andreas Zwinkau
 */
/* TODO might make sense to merge this optimization with fp-vrp */
#include "constbits.h"
#include "dca.h"
#include "debug.h"
#include "ircons.h"
#include "irdump_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "tv.h"
#include <stdbool.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct env_t {
	bool       changed;
	ir_nodemap dca;
} env_t;

static void occult_const_opt_walker(ir_node *node, void *data)
{
	/* Ignore already const nodes */
	if (is_irn_constlike(node))
	    return;

	/* Ignore mode_BB, mode_X, etc */
	if (!mode_is_data(get_irn_mode(node)))
		return;

	env_t   *env = (env_t*)data;
	bitinfo *vrp = get_bitinfo(node);
	if (vrp == NULL) {
		DB((dbg, LEVEL_4, "No VRP info: %+F\n", node));
		return;
	}

	ir_tarval *dc                      = ir_nodemap_get(ir_tarval, &env->dca, node);
	ir_tarval *not_const_bits          = tarval_eor(vrp->o, vrp->z);
	ir_tarval *relevant_not_const_bits = tarval_and(dc, not_const_bits);
	if (!tarval_is_null(relevant_not_const_bits)) {
		DB((dbg, LEVEL_4, "Not occult: %+F dc=%T, z=%T, o=%T\n", node, dc,
		    vrp->z, vrp->o));
		return;
	}

	ir_graph  *irg  = get_irn_irg(node);
	ir_tarval *tv   = tarval_and(vrp->z, dc);
	ir_node   *cnst = new_r_Const(irg, tv);
	DB((dbg, LEVEL_2, "Occult Const found: %+F -> %+F dc=%T, z=%T, o=%T\n",
	    node, cnst, dc, vrp->z, vrp->o));
	exchange(node, cnst);
	env->changed = true;
}

static void fill_nodemap(ir_node *node, void *data) {
	ir_nodemap *map = data;
	ir_nodemap_insert_fast(map, node, get_irn_link(node));
}

void occult_consts(ir_graph *irg)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.occults");

	constbits_analyze(irg);

	env_t env;
	memset(&env, 0, sizeof(env));

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	dca_analyze(irg);
	ir_nodemap_init(&env.dca, irg);
	irg_walk_graph(irg, fill_nodemap, 0, &env.dca);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	irg_walk_graph(irg, occult_const_opt_walker, 0, &env);

	ir_nodemap_destroy(&env.dca);

	constbits_clear(irg);
	confirm_irg_properties(irg,
	                       env.changed ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}
