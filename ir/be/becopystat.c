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
 * @file
 * @brief       Copy node statistics.
 * @author      Daniel Grund
 * @date        19.04.2005
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <libcore/lc_timing.h>

#include "xmalloc.h"
#include "irgraph.h"
#include "irgwalk.h"
#include "irprog.h"
#include "iredges_t.h"
#include "phiclass.h"
#include "irnodeset.h"

#include "bechordal_t.h"
#include "beutil.h"
#include "becopyopt_t.h"
#include "becopystat.h"
#include "beirg_t.h"
#include "bemodule.h"
#include "beintlive_t.h"

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
	I_PHI_CNT,			/* number of phi nodes */
	I_PHI_ARG_CNT,		/* number of arguments of phis */
	I_PHI_ARG_SELF,		/* number of arguments of phis being the phi itself */
	I_PHI_ARG_CONST,	/* number of arguments of phis being consts */
	I_PHI_ARG_PRED,		/* ... being defined in a cf-pred */
	I_PHI_ARG_GLOB,		/* ... being defined elsewhere */
	I_PHI_ARITY_S,
	I_PHI_ARITY_E    = I_PHI_ARITY_S+MAX_ARITY,

	/* copy nodes */
	I_CPY_CNT,			/* number of copynodes */

	/* phi classes */
	I_CLS_CNT,			/* number of phi classes */
	I_CLS_IF_FREE,		/* number of pc having no interference */
	I_CLS_IF_MAX,		/* number of possible interferences in all classes */
	I_CLS_IF_CNT,		/* number of actual interferences in all classes */
	I_CLS_SIZE_S,
	I_CLS_SIZE_E = I_CLS_SIZE_S+MAX_CLS_SIZE,
	I_CLS_PHIS_S,
	I_CLS_PHIS_E = I_CLS_PHIS_S+MAX_CLS_PHIS,

	/* FROM HERE: RESULT VLAUES */
	/* all of them are external set */

	/* ilp values */
	I_HEUR_TIME,		/* solving time in milli seconds */
	I_ILP_TIME,			/* solving time in milli seconds */
	I_ILP_VARS,
	I_ILP_CSTR,
	I_ILP_ITER,			/* number of simplex iterations */

	/* copy instructions */
	I_COPIES_MAX,		/* max possible costs of copies*/
	I_COPIES_INIT,		/* number of copies in initial allocation */
	I_COPIES_HEUR,		/* number of copies after heuristic */
	I_COPIES_5SEC,		/* number of copies after ilp with max n sec */
	I_COPIES_30SEC,		/* number of copies after ilp with max n sec */
	I_COPIES_OPT,		/* number of copies after ilp */
	I_COPIES_IF,		/* number of copies inevitable due to root-arg-interf */

	ASIZE
};

/**
 * Holds current values. Values are added till next copystat_reset
 */
int curr_vals[ASIZE];

static ir_nodeset_t *all_phi_nodes;
static ir_nodeset_t *all_copy_nodes;
static ir_graph *last_irg;

void be_init_copystat(void) {
	FIRM_DBG_REGISTER(dbg, "firm.be.copystat");

	all_phi_nodes  = ir_nodeset_new(64);
	all_copy_nodes = ir_nodeset_new(64);
	memset(curr_vals, 0, sizeof(curr_vals));
}
BE_REGISTER_MODULE_CONSTRUCTOR(be_init_copystat);

void be_quit_copystat(void) {
	ir_nodeset_del(all_phi_nodes);
	ir_nodeset_del(all_copy_nodes);
}
BE_REGISTER_MODULE_DESTRUCTOR(be_quit_copystat);

void copystat_reset(void) {
	be_quit_copystat();
	be_init_copystat();
}

/**
 * Collect general data
 */
static void irg_stat_walker(ir_node *node, void *env) {
	arch_env_t *arch_env = env;
	curr_vals[I_ALL_NODES]++; /* count all nodes */

	if (is_Block(node)) /* count all blocks */
		curr_vals[I_BLOCKS]++;

	if (is_Reg_Phi(node)) /* collect phis */
		ir_nodeset_insert(all_phi_nodes, node);

	if (is_Perm_Proj(arch_env, node))
		ir_nodeset_insert(all_copy_nodes, node);

	/* TODO: Add 2-Addr-Code nodes */
}

static void copystat_collect_irg(ir_graph *irg, arch_env_t *arch_env) {
	irg_walk_graph(irg, irg_stat_walker, NULL, arch_env);
	last_irg = irg;
}

/**
 * @return 1 if the block at pos @p pos removed a critical edge
 * 		   0 else
 */
static INLINE int was_edge_critical(const ir_node *bl, int pos) {
	const ir_edge_t *edge;
	const ir_node *bl_at_pos, *bl_before;
	assert(is_Block(bl));

	/* Does bl have several predecessors ?*/
	if (get_irn_arity(bl) <= 1)
		return 0;

	/* Does the pred have exactly one predecessor */
	bl_at_pos = get_irn_n(bl, pos);
	if (get_irn_arity(bl_at_pos) != 1)
		return 0;

	/* Does the pred of the pred have several successors */
	bl_before = get_irn_n(bl_at_pos, 0);
	edge = get_block_succ_first(bl_before);
	return get_block_succ_next(bl_before, edge) ? 1 : 0;
}

/**
 * Collect phi node data
 */
static void stat_phi_node(be_chordal_env_t *chordal_env, ir_node *phi)
{
 	int arity, i;
 	ir_node *phi_bl;
	assert(is_Phi(phi));
	(void) chordal_env;

	/* count all phi phis */
	curr_vals[I_PHI_CNT]++;

	/* argument count */
	arity = get_irn_arity(phi);
	curr_vals[I_PHI_ARG_CNT] += arity;
	if (arity > MAX_ARITY)
		curr_vals[I_PHI_ARITY_E]++;
	else
		curr_vals[I_PHI_ARITY_S + arity]++;

	phi_bl = get_nodes_block(phi);
	/* type of argument {self, const, pred, glob} */
	for (i = 0; i < arity; i++) {
        ir_node *block_of_arg, *block_ith_pred;
		ir_node *arg = get_irn_n(phi, i);

		if (arg == phi) {
			curr_vals[I_PHI_ARG_SELF]++;
			continue;
		}

		if (iro_Const == get_irn_opcode(arg)) {
			curr_vals[I_PHI_ARG_CONST]++;
			continue;
		}

		/* get the pred block skipping blocks on critical edges */
		block_ith_pred = get_Block_cfgpred_block(phi_bl, i);
		if (was_edge_critical(phi_bl, i))
			block_ith_pred = get_Block_cfgpred_block(block_ith_pred, 0);

		block_of_arg = get_nodes_block(arg);
		if (block_of_arg == block_ith_pred) {
			curr_vals[I_PHI_ARG_PRED]++;
			continue;
		}

		curr_vals[I_PHI_ARG_GLOB]++;
	}
}

/**
 * Collect register-constrained node data
 */
static void stat_copy_node(be_chordal_env_t *chordal_env, ir_node *root) {
	curr_vals[I_CPY_CNT]++;
	curr_vals[I_COPIES_MAX]++;
	if (values_interfere(chordal_env->birg, root, get_Perm_src(root))) {
		curr_vals[I_COPIES_IF]++;
		assert(0 && "A Perm pair (in/out) should never interfere!");
	}
}

/**
 * Collect phi class data
 */
static void stat_phi_class(be_chordal_env_t *chordal_env, ir_node **pc) {
	int i, o, size, if_free, phis;

	/* phi class count */
	curr_vals[I_CLS_CNT]++;

	/* phi class size */
	size = ARR_LEN(pc);
	if (size > MAX_CLS_SIZE)
		curr_vals[I_CLS_SIZE_E]++;
	else
		curr_vals[I_CLS_SIZE_S + size]++;

	/* determine number of phis on this class */
	for (phis = i = 0; i < size; ++i)
		if (is_Phi(pc[i]))
			phis++;

	if (phis > MAX_CLS_PHIS)
		curr_vals[I_CLS_PHIS_E]++;
	else
		curr_vals[I_CLS_PHIS_S + phis]++;

	/* determine interference of phi class members */
	curr_vals[I_CLS_IF_MAX] += size * (size - 1) / 2;
	for (if_free = 1, i = 0; i < size - 1; ++i)
		for (o = i + 1; o < size; ++o)
			if (values_interfere(chordal_env->birg, pc[i], pc[o])) {
				if_free = 0;
				curr_vals[I_CLS_IF_CNT]++;
			}

	/* Does this phi class have an inner interference? */
	curr_vals[I_CLS_IF_FREE] += if_free;
}

static void copystat_collect_cls(be_chordal_env_t *cenv) {
	ir_graph              *irg  = cenv->irg;
	arch_env_t            *aenv = cenv->birg->main_env->arch_env;
	ir_node               *n, **pc;
	phi_classes_t         *pc_obj;
	pset                  *all_phi_classes;
	ir_nodeset_iterator_t iter;

	copystat_reset();
	copystat_collect_irg(irg, aenv);

	/* compute the Phi classes of the collected Phis */
	pc_obj          = phi_class_new_from_set(cenv->irg, all_phi_nodes, 0);
	all_phi_classes = get_all_phi_classes(pc_obj);

	foreach_ir_nodeset(all_phi_nodes, n, iter) {
		if (arch_get_irn_reg_class(aenv, n, -1) == cenv->cls)
			stat_phi_node(cenv, n);
	}

	foreach_ir_nodeset(all_copy_nodes, n, iter) {
		if (arch_get_irn_reg_class(aenv, n, -1) == cenv->cls)
			stat_copy_node(cenv, n);
	}

	foreach_pset(all_phi_classes, pc) {
		ir_node *member = pc[0];
		if (arch_get_irn_reg_class(aenv, member, -1) == cenv->cls)
			stat_phi_class(cenv, pc);
	}

	/* free the phi class object */
	phi_class_free(pc_obj);
}

void copystat_add_max_costs(int costs) {
	curr_vals[I_COPIES_MAX] += costs;
}
void copystat_add_inevit_costs(int costs) {
	curr_vals[I_COPIES_IF] += costs;
}
void copystat_add_init_costs(int costs) {
	curr_vals[I_COPIES_INIT] += costs;
}
void copystat_add_heur_costs(int costs) {
	curr_vals[I_COPIES_HEUR] += costs;
}
void copystat_add_ilp_5_sec_costs(int costs) {
	curr_vals[I_COPIES_5SEC] += costs;
}
void copystat_add_ilp_30_sec_costs(int costs) {
	curr_vals[I_COPIES_30SEC] += costs;
}
void copystat_add_opt_costs(int costs) {
	curr_vals[I_COPIES_OPT] += costs;
}
void copystat_add_heur_time(int time) {
	curr_vals[I_HEUR_TIME] += time;
}

#ifdef WITH_ILP

void copystat_add_ilp_time(int time) {
	curr_vals[I_ILP_TIME] += time;
}
void copystat_add_ilp_vars(int vars) {
	curr_vals[I_ILP_VARS] += vars;
}
void copystat_add_ilp_csts(int csts) {
	curr_vals[I_ILP_CSTR] += csts;
}
void copystat_add_ilp_iter(int iters) {
	curr_vals[I_ILP_ITER] += iters;
}

#endif /* WITH_ILP */

void copystat_dump(ir_graph *irg) {
	int i;
	char buf[1024];
	FILE *out;

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_prog_name(), get_entity_name(get_irg_entity(irg)));
	buf[sizeof(buf) - 1] = '\0';
	out = be_ffopen(buf, "stat", "wt");

	fprintf(out, "%d\n", ASIZE);
	for (i = 0; i < ASIZE; i++) {
#if 0
		if (i >= I_PHI_ARITY_S && i <= I_PHI_ARITY_E)
			fprintf(out, "%i %i\n", curr_vals[i], curr_vals[I_PHI_CNT]);
		else if (i >= I_CLS_SIZE_S && i <= I_CLS_SIZE_E)
			fprintf(out, "%i %i\n", curr_vals[i], curr_vals[I_CLS_CNT]);
		else
#endif
			fprintf(out, "%i\n", curr_vals[i]);
	}

	fclose(out);
}

void copystat_dump_pretty(ir_graph *irg) {
	int i;
	char buf[1024];
	FILE *out;

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_prog_name(), get_entity_name(get_irg_entity(irg)));
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

/**
 * Helpers for saving and restoring colors of nodes.
 * Used to get dependable and comparable benchmark results.
 */
typedef struct color_saver {
	arch_env_t *arch_env;
	be_chordal_env_t *chordal_env;
	pmap *saved_colors;
	int flag; /* 0 save, 1 load */
} color_save_t;

static void save_load(ir_node *irn, void *env) {
	color_save_t *saver = env;
	if (saver->chordal_env->cls == arch_get_irn_reg_class(saver->arch_env, irn, -1)) {
		if (saver->flag == 0) { /* save */
			const arch_register_t *reg = arch_get_irn_register(saver->arch_env, irn);
			pmap_insert(saver->saved_colors, irn, (void *) reg);
		} else { /*load */
			arch_register_t *reg = pmap_get(saver->saved_colors, irn);
			arch_set_irn_register(saver->arch_env, irn, reg);
		}
	}
}

static void save_colors(color_save_t *color_saver) {
	color_saver->flag = 0;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}

#ifdef WITH_ILP
static void load_colors(color_save_t *color_saver) {
	color_saver->flag = 1;
	irg_walk_graph(color_saver->chordal_env->irg, save_load, NULL, color_saver);
}
#endif

/**
 * Main compare routine
 */
void co_compare_solvers(be_chordal_env_t *chordal_env) {
	copy_opt_t    *co;
	lc_timer_t    *timer;
	color_save_t  saver;
	int costs_inevit, costs_init, costs_solved, lower_bound;

	copystat_collect_cls(chordal_env);

	co = new_copy_opt(chordal_env, co_get_costs_loop_depth);
	co_build_ou_structure(co);
	co_build_graph_structure(co);
	DBG((dbg, LEVEL_1, "----> CO: %s\n", co->name));

	/* save colors */
	saver.arch_env     = chordal_env->birg->main_env->arch_env;
	saver.chordal_env  = chordal_env;
	saver.saved_colors = pmap_create();
	save_colors(&saver);

	/* initial values */
	costs_inevit = co_get_inevit_copy_costs(co);
	lower_bound  = co_get_lower_bound(co);
	costs_init   = co_get_copy_costs(co);

	DBG((dbg, LEVEL_1, "Inevit Costs: %3d\n", costs_inevit));
	DBG((dbg, LEVEL_1, "Lower Bound: %3d\n", lower_bound));
	DBG((dbg, LEVEL_1, "Init costs: %3d\n", costs_init));

	copystat_add_inevit_costs(costs_inevit);
	copystat_add_init_costs(costs_init);
	copystat_add_max_costs(co_get_max_copy_costs(co));

	/* heuristic 1 (Daniel Grund) */
	timer = lc_timer_register("heur1", NULL);
	lc_timer_reset_and_start(timer);

	co_solve_heuristic(co);

	lc_timer_stop(timer);

	costs_solved = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "HEUR1 costs: %3d\n", costs_solved));
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
	copystat_add_heur_costs(costs_solved);
	assert(lower_bound <= costs_solved);

	/* heuristic 2 (Sebastian Hack) */
	timer = lc_timer_register("heur2", NULL);
	lc_timer_reset_and_start(timer);

	co_solve_heuristic_new(co);

	lc_timer_stop(timer);

	costs_solved = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "HEUR2 costs: %3d\n", costs_solved));
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
	copystat_add_heur_costs(costs_solved);
	assert(lower_bound <= costs_solved);

	/* Park & Moon register coalescing (Kimon Hoffmann) */
	timer = lc_timer_register("park", NULL);
	lc_timer_reset_and_start(timer);

	co_solve_park_moon(co);

	lc_timer_stop(timer);

	costs_solved = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "Park/Moon costs: %3d\n", costs_solved));
	copystat_add_heur_time(lc_timer_elapsed_msec(timer));
	copystat_add_heur_costs(costs_solved);
	assert(lower_bound <= costs_solved);


#ifdef WITH_ILP

	/* ILP 1 is not yet implemented, so it makes no sense to compare */
#if 0
	load_colors(&saver);

	co_solve_ilp1(co, 60.0);

	costs_solved = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "ILP1 costs: %3d\n", costs_solved));
	copystat_add_opt_costs(costs_solved); /* TODO: ADAPT */
	assert(lower_bound <= costs_solved);
#endif /* 0 */

	/* ILP 2 */
	load_colors(&saver);

	co_solve_ilp2(co);

	costs_solved = co_get_copy_costs(co);
	DBG((dbg, LEVEL_1, "ILP2 costs: %3d\n", costs_solved));
	copystat_add_opt_costs(costs_solved); /* TODO: ADAPT */
	assert(lower_bound <= costs_solved);

#endif /* WITH_ILP */

	/* free memory for statistic structures */
	pmap_destroy(saver.saved_colors);
	co_free_graph_structure(co);
	co_free_ou_structure(co);
	free_copy_opt(co);
}
