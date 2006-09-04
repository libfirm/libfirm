/** vim: set sw=4 ts=4:
 * @file   bespillcost.c
 * @date   2006-06-28
 * @author Adam M. Szalkowski
 *
 * Spill cost estimation
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "list.h"
#include "pmap.h"

#include "irprintf.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "phiclass_t.h"
#include "iredges.h"
#include "execfreq.h"
#include "irvrfy.h"

#include <libcore/lc_bitset.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"
#include "bepressurestat.h"

#include "bechordal_t.h"

#define BIGM 100000.0

#define COST_LOAD      8
#define COST_STORE     50
#define COST_REMAT     1

typedef struct _spill_cost_t {
	const be_chordal_env_t       *chordal_env;
	double                        cost;
	DEBUG_ONLY(firm_dbg_module_t *dbg);
} spill_cost_t;

static double
execution_frequency(const be_chordal_env_t * chordal_env, const ir_node * irn)
{
#define FUDGE 0.001
#ifndef EXECFREQ_LOOPDEPH
	return get_block_execfreq(chordal_env->exec_freq, get_block(irn)) + FUDGE;
#else
	if(is_Block(irn))
		return exp(get_loop_depth(get_irn_loop(irn)) * log(10)) + FUDGE;
	else
		return exp(get_loop_depth(get_irn_loop(get_nodes_block(irn))) * log(10)) + FUDGE;
#endif
}


static int
get_cost(const be_chordal_env_t * chordal_env, const ir_node * irn)
{
	if(be_is_Spill(irn)) {
		return COST_STORE;
	} else if(be_is_Reload(irn)){
		return COST_LOAD;
	} else {
		return arch_get_op_estimated_cost(chordal_env->birg->main_env->arch_env, irn);
	}
}


static void
walker_cost_collector(ir_node * irn, void * data)
{
	spill_cost_t   *sc = data;
	double          freq, cost;

	if( (be_is_Reload(irn) && chordal_has_class(sc->chordal_env, irn))  ||
	    (be_is_Spill(irn) && chordal_has_class(sc->chordal_env, get_irn_n(irn,1)))) {

		freq = execution_frequency(sc->chordal_env, irn);
		cost = get_cost(sc->chordal_env, irn);

		DBG((sc->dbg, LEVEL_2, "%+F has cost %g with execfreq %g ->\t %g\n", irn, cost, freq, cost*freq));

		sc->cost += cost*freq;
	}
}

double
get_irg_spill_cost(const be_chordal_env_t * chordal_env)
{
	spill_cost_t   sc;
	char           problem_name[256];

	ir_snprintf(problem_name, sizeof(problem_name), "%F_%s", chordal_env->irg, chordal_env->cls->name);

	sc.cost = 0.0;
	sc.chordal_env = chordal_env;
	FIRM_DBG_REGISTER(sc.dbg, "firm.be.ra.spillcost");

	DBG((sc.dbg, LEVEL_2, "computing spill costs for %s\n", problem_name));
	irg_walk_graph(chordal_env->irg, walker_cost_collector, NULL, &sc);
	DBG((sc.dbg, LEVEL_1, "spill costs for %s: %g\n", problem_name, sc.cost));
	DBG((sc.dbg, LEVEL_2, "\n"));

	return sc.cost;
}
