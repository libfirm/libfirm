/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Copy node statistics.
 * @author      Daniel Grund
 * @date        19.04.2005
 */
#include <string.h>

#include "timing.h"
#include "irgwalk.h"
#include "irprog.h"
#include "iredges_t.h"
#include "irnodeset.h"

#include "bechordal_t.h"
#include "bediagnostic.h"
#include "benode.h"
#include "beutil.h"
#include "becopyopt_t.h"
#include "becopystat.h"
#include "bemodule.h"
#include "belive.h"

#define DEBUG_LVL SET_LEVEL_1
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

#define MAX_ARITY 20
#define MAX_CLS_SIZE 20
#define MAX_CLS_PHIS 20

/**
 * For an explanation of these values see the code of copystat_dump_pretty
 */
enum vals_t {
	/* FROM HERE: PROBLEM CHARACTERIZATION */

	I_ALL_NODES = 0,
	I_BLOCKS,

	/* phi nodes */
	I_PHI_CNT,          /* number of phi nodes */
	I_PHI_ARG_CNT,      /* number of arguments of phis */
	I_PHI_ARG_SELF,     /* number of arguments of phis being the phi itself */
	I_PHI_ARG_CONST,    /* number of arguments of phis being consts */
	I_PHI_ARG_PRED,     /* ... being defined in a cf-pred */
	I_PHI_ARG_GLOB,     /* ... being defined elsewhere */
	I_PHI_ARITY_S,
	I_PHI_ARITY_E    = I_PHI_ARITY_S+MAX_ARITY,

	/* copy nodes */
	I_CPY_CNT,          /* number of copynodes */

	/* phi classes */
	I_CLS_CNT,          /* number of phi classes */
	I_CLS_IF_FREE,      /* number of pc having no interference */
	I_CLS_IF_MAX,       /* number of possible interferences in all classes */
	I_CLS_IF_CNT,       /* number of actual interferences in all classes */
	I_CLS_SIZE_S,
	I_CLS_SIZE_E = I_CLS_SIZE_S+MAX_CLS_SIZE,
	I_CLS_PHIS_S,
	I_CLS_PHIS_E = I_CLS_PHIS_S+MAX_CLS_PHIS,

	/* FROM HERE: RESULT VLAUES */
	/* all of them are external set */

	/* ilp values */
	I_HEUR_TIME,        /* solving time in milli seconds */
	I_ILP_TIME,         /* solving time in milli seconds */
	I_ILP_VARS,
	I_ILP_CSTR,
	I_ILP_ITER,         /* number of simplex iterations */

	/* copy instructions */
	I_COPIES_MAX,       /* max possible costs of copies*/
	I_COPIES_INIT,      /* number of copies in initial allocation */
	I_COPIES_HEUR,      /* number of copies after heuristic */
	I_COPIES_5SEC,      /* number of copies after ilp with max n sec */
	I_COPIES_30SEC,     /* number of copies after ilp with max n sec */
	I_COPIES_OPT,       /* number of copies after ilp */
	I_COPIES_IF,        /* number of copies inevitable due to root-arg-interf */

	ASIZE
};

/**
 * Holds current values. Values are added till next copystat_reset
 */
static int curr_vals[ASIZE];

static ir_nodeset_t *all_phi_nodes;
static ir_nodeset_t *all_copy_nodes;

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copystat)
void be_init_copystat(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.copystat");

	all_phi_nodes  = ir_nodeset_new(64);
	all_copy_nodes = ir_nodeset_new(64);
	memset(curr_vals, 0, sizeof(curr_vals));
}

BE_REGISTER_MODULE_DESTRUCTOR(be_quit_copystat)
void be_quit_copystat(void)
{
	if (all_phi_nodes != NULL) {
		ir_nodeset_del(all_phi_nodes);
		all_phi_nodes = NULL;
	}
	if (all_copy_nodes != NULL) {
		ir_nodeset_del(all_copy_nodes);
		all_copy_nodes = NULL;
	}
}

void copystat_add_max_costs(int costs)
{
	curr_vals[I_COPIES_MAX] += costs;
}
void copystat_add_inevit_costs(int costs)
{
	curr_vals[I_COPIES_IF] += costs;
}
void copystat_add_init_costs(int costs)
{
	curr_vals[I_COPIES_INIT] += costs;
}
void copystat_add_heur_costs(int costs)
{
	curr_vals[I_COPIES_HEUR] += costs;
}
void copystat_add_opt_costs(int costs)
{
	curr_vals[I_COPIES_OPT] += costs;
}
void copystat_add_heur_time(int time)
{
	curr_vals[I_HEUR_TIME] += time;
}

void copystat_add_ilp_5_sec_costs(int costs)
{
	curr_vals[I_COPIES_5SEC] += costs;
}
void copystat_add_ilp_30_sec_costs(int costs)
{
	curr_vals[I_COPIES_30SEC] += costs;
}
void copystat_add_ilp_time(int time)
{
	curr_vals[I_ILP_TIME] += time;
}
void copystat_add_ilp_vars(int vars)
{
	curr_vals[I_ILP_VARS] += vars;
}
void copystat_add_ilp_csts(int csts)
{
	curr_vals[I_ILP_CSTR] += csts;
}
void copystat_add_ilp_iter(int iters)
{
	curr_vals[I_ILP_ITER] += iters;
}

/**
 * Opens a file named base.ext with the mode mode.
 */
static FILE *be_ffopen(const char *base, const char *ext, const char *mode)
{
	FILE *out;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s.%s", base, ext);
	buf[sizeof(buf) - 1] = '\0';
	if (! (out = fopen(buf, mode))) {
		be_errorf(NULL, "cannot open file '%s' in mode '%s'", buf, mode);
		return NULL;
	}
	return out;
}

void copystat_dump(ir_graph *irg)
{
	int i;
	char buf[1024];
	FILE *out;

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_name(), get_entity_name(get_irg_entity(irg)));
	buf[sizeof(buf) - 1] = '\0';
	out = be_ffopen(buf, "stat", "wt");

	fprintf(out, "%d\n", (int)ASIZE);
	for (i = 0; i < ASIZE; i++) {
		fprintf(out, "%i\n", curr_vals[i]);
	}

	fclose(out);
}

void copystat_dump_pretty(ir_graph *irg)
{
	int i;
	char buf[1024];
	FILE *out;

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_name(), get_entity_name(get_irg_entity(irg)));
	buf[sizeof(buf) - 1] = '\0';
	out = be_ffopen(buf, "pstat", "wt");

	fprintf(out, "Nodes     %4d\n", curr_vals[I_ALL_NODES]);
	fprintf(out, "Blocks    %4d\n", curr_vals[I_BLOCKS]);
	fprintf(out, "CopyIrn   %4d\n", curr_vals[I_CPY_CNT]);

	fprintf(out, "\nPhis      %4d\n", curr_vals[I_PHI_CNT]);
	fprintf(out, "... argument types\n");
	fprintf(out, " Total      %4d\n", curr_vals[I_PHI_ARG_CNT]);
	fprintf(out, " Self       %4d\n", curr_vals[I_PHI_ARG_SELF]);
	fprintf(out, " Constants  %4d\n", curr_vals[I_PHI_ARG_CONST]);
	fprintf(out, " CF-Pred    %4d\n", curr_vals[I_PHI_ARG_PRED]);
	fprintf(out, " Others     %4d\n", curr_vals[I_PHI_ARG_GLOB]);
	fprintf(out, "... arities\n");
	for (i = I_PHI_ARITY_S; i<=I_PHI_ARITY_E; i++)
		fprintf(out, " %2i %4d\n", i-I_PHI_ARITY_S, curr_vals[i]);

	fprintf(out, "\nPhi classes   %4d\n", curr_vals[I_CLS_CNT]);
	fprintf(out, " compl. free  %4d\n", curr_vals[I_CLS_IF_FREE]);
	fprintf(out, " inner intf.  %4d / %4d\n", curr_vals[I_CLS_IF_CNT], curr_vals[I_CLS_IF_MAX]);
	fprintf(out, "... sizes\n");
	for (i = I_CLS_SIZE_S; i<=I_CLS_SIZE_E; i++)
		fprintf(out, " %2i %4d\n", i-I_CLS_SIZE_S, curr_vals[i]);
	fprintf(out, "... contained phis\n");
	for (i = I_CLS_PHIS_S; i<=I_CLS_PHIS_E; i++)
		fprintf(out, " %2i %4d\n", i-I_CLS_PHIS_S, curr_vals[i]);

	fprintf(out, "\nILP stat\n");
	fprintf(out, " Time %8d\n", curr_vals[I_ILP_TIME]);
	fprintf(out, " Iter %8d\n", curr_vals[I_ILP_ITER]);

	fprintf(out, "\nCopy stat\n");
	fprintf(out, " Max  %4d\n", curr_vals[I_COPIES_MAX]);
	fprintf(out, " Init %4d\n", curr_vals[I_COPIES_INIT]);
	fprintf(out, " Heur %4d\n", curr_vals[I_COPIES_HEUR]);
	fprintf(out, " Opt  %4d\n", curr_vals[I_COPIES_OPT]);
	fprintf(out, " Intf %4d\n", curr_vals[I_COPIES_IF]);

	fclose(out);
}
