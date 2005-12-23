/**
 * Author:      Daniel Grund
 * Date:		19.04.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include "irgraph.h"
#include "irprog.h"
#include "iredges.h"
#include "phiclass_t.h"
#include "beutil.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "xmalloc.h"

#ifdef DO_STAT

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

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

static pset *all_phi_nodes;
static pset *all_phi_classes;
static pset *all_copy_nodes;
static ir_graph *last_irg;

void copystat_init(void) {
	dbg = firm_dbg_register("ir.be.copystat");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	all_phi_nodes = pset_new_ptr_default();
	all_phi_classes = pset_new_ptr_default();
	all_copy_nodes = pset_new_ptr_default();
}

void copystat_reset(void) {
	int i;
	for (i = 0; i < ASIZE; ++i)
		curr_vals[i] = 0;
	del_pset(all_phi_nodes);
	del_pset(all_phi_classes);
	del_pset(all_copy_nodes);
	all_phi_nodes = pset_new_ptr_default();
	all_phi_classes = pset_new_ptr_default();
	all_copy_nodes = pset_new_ptr_default();
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
 		pset_insert_ptr(all_phi_nodes, node);

 	if (is_Perm_Proj(arch_env, node))
 		pset_insert_ptr(all_copy_nodes, node);

	/* TODO: Add 2-Addr-Code nodes */
}

static void copystat_collect_irg(ir_graph *irg, arch_env_t *arch_env) {
	irg_walk_graph(irg, irg_stat_walker, NULL, arch_env);
	all_phi_classes = phi_class_compute_by_phis(all_phi_nodes);
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

	/* Does the pred of the pred have several sucsecessors */
	bl_before = get_irn_n(bl_at_pos, 0);
	edge = get_block_succ_first(bl_before);
	return get_block_succ_next(bl_before, edge) ? 1 : 0;
}

/**
 * Collect phi node data
 */
static void stat_phi_node(be_chordal_env_t *chordal_env, ir_node *phi) {
 	int arity, i;
 	ir_node *phi_bl;
	assert(is_Phi(phi));

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
	if (nodes_interfere(chordal_env, root, get_Copy_src(root))) {
		curr_vals[I_COPIES_IF]++;
		assert(0 && "A Perm pair (in/out) should never interfere!");
	}
}

/**
 * Collect phi class data
 */
static void stat_phi_class(be_chordal_env_t *chordal_env, pset *pc) {
	int i, o, size, if_free, phis;
	ir_node **members, *p;

	/* phi class count */
	curr_vals[I_CLS_CNT]++;

	/* phi class size */
	size = pset_count(pc);
	if (size > MAX_CLS_SIZE)
		curr_vals[I_CLS_SIZE_E]++;
	else
		curr_vals[I_CLS_SIZE_S + size]++;

	/* get an array of all members for double iterating */
	members = xmalloc(size * sizeof(*members));
	DBG((dbg, LEVEL_2, "Phi-class:\n"));
	for (i = 0, p = pset_first(pc); p; p = pset_next(pc)) {
		DBG((dbg, LEVEL_2, "  %+F\n", p));
		members[i++] = p;
	}
	assert(i == size);

	/* determine number of phis on this class */
	phis = 0;
	for (i = 0; i < size; ++i)
		if (is_Phi(members[i]))
			phis++;
	if (phis > MAX_CLS_PHIS)
		curr_vals[I_CLS_PHIS_E]++;
	else
		curr_vals[I_CLS_PHIS_S + phis]++;

	/* determine interference of phi class members */
	curr_vals[I_CLS_IF_MAX] += size*(size-1)/2;
	if_free = 1;
	for (i = 0; i < size-1; ++i)
		for (o = i+1; o < size; ++o)
			if (nodes_interfere(chordal_env, members[i], members[o])) {
				if_free = 0;
				curr_vals[I_CLS_IF_CNT]++;
			}

	/* Does this phi class have an inner interference? */
	curr_vals[I_CLS_IF_FREE] += if_free;

	xfree(members);
}

#define is_curr_reg_class(irn) \
  (arch_get_irn_reg_class(chordal_env->main_env->arch_env, irn, \
                          -1) == chordal_env->cls)

void copystat_collect_cls(be_chordal_env_t *chordal_env) {
	ir_node *n;
	pset *pc;
	ir_graph *irg = chordal_env->irg;

	if (last_irg != irg) {
		copystat_reset();
		copystat_collect_irg(irg, chordal_env->main_env->arch_env);
	}

	for (n = pset_first(all_phi_nodes); n; n = pset_next(all_phi_nodes))
		if (is_curr_reg_class(n))
			stat_phi_node(chordal_env, n);

	for (n = pset_first(all_copy_nodes); n; n = pset_next(all_copy_nodes))
		if (is_curr_reg_class(n))
			stat_copy_node(chordal_env, n);

	for (pc = pset_first(all_phi_classes); pc; pc = pset_next(all_phi_classes)) {
		ir_node *member = pset_first(pc);
		pset_break(pc);
		if (is_curr_reg_class(member))
			stat_phi_class(chordal_env, pc);
	}
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

void copystat_dump(ir_graph *irg) {
	int i;
	char buf[1024];
	FILE *out;

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_prog_name(), get_entity_name(get_irg_entity(irg)));
	out = ffopen(buf, "stat", "wt");

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
	out = ffopen(buf, "pstat", "wt");

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

#endif
