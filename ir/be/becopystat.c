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
#include "phiclass_t.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "xmalloc.h"

#ifdef DO_STAT

#define DEBUG_LVL 0 //SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

static pset *all_phi_nodes;
static pset *all_phi_classes;
static pset *all_copy_nodes;

void copystat_init(void) {
	dbg = firm_dbg_register("ir.be.copystat");
	firm_dbg_set_mask(dbg, DEBUG_LVL);

	all_phi_nodes = pset_new_ptr_default();
	all_phi_classes = pset_new_ptr_default();
	all_copy_nodes = pset_new_ptr_default();
	phi_class_init();
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
static void stat_walker(ir_node *node, void *env) {
	curr_vals[I_ALL_NODES]++; /* count all nodes */

 	if (is_Block(node)) /* count all blocks */
 		curr_vals[I_BLOCKS]++;

 	if (is_Phi(node)) /* collect phis */
 		pset_insert_ptr(all_phi_nodes, node);

 	if (is_Copy(node))
 		pset_insert_ptr(all_copy_nodes, node);
}

/**
 * Collect phi node data
 */
static void stat_phi_node(be_chordal_env_t *chordal_env, ir_node *phi) {
 	int arity, i;
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

	/* type of argument {self, const, pred, glob} */
	for (i = 0; i < arity; i++) {
        ir_node *block_of_arg, *block_ith_pred;
		ir_node *arg = get_irn_n(phi, i);

		if (phi != arg)	{
			curr_vals[I_COPIES_MAX]++; /* if arg!=phi this is a possible copy */
			if (nodes_interfere(chordal_env, phi, arg)) {
				DBG((dbg, LEVEL_1, "%e -- In Block %N: %n %N %n %N\n", get_irg_entity(chordal_env->irg), get_nodes_block(phi), phi, phi, arg, arg));
				curr_vals[I_COPIES_IF]++;
			}
		}

		if (arg == phi) {
			curr_vals[I_PHI_ARG_SELF]++;
			continue;
		}

		if (iro_Const == get_irn_opcode(arg)) {
			curr_vals[I_PHI_ARG_CONST]++;
			continue;
		}

		block_of_arg = get_nodes_block(arg);
		block_ith_pred = get_nodes_block(get_irn_n(get_nodes_block(phi), i));
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
	int i, o, size, if_free;
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
	for (i = 0, p = pset_first(pc); p; p = pset_next(pc))
		members[i++] = p;
	assert(i == size);

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

void copystat_collect_irg(ir_graph *irg) {
	irg_walk_graph(irg, stat_walker, NULL, NULL);
	curr_vals[I_BLOCKS] -= 2; /* substract 2 for start and end block */
	all_phi_classes = phi_class_compute_by_phis(all_phi_nodes);
}

#define is_curr_reg_class(irn) (arch_get_irn_reg_class(chordal_env->arch_env, irn, arch_pos_make_out(0)) == chordal_env->cls)

void copystat_collect_cls(be_chordal_env_t *chordal_env) {
	ir_node *n;
	pset *pc;

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

void copystat_dump(ir_graph *irg) {
	int i;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_prog_name(), get_entity_name(get_irg_entity(irg)));
	FILE *out = ffopen(buf, "stat", "wt");

	fprintf(out, "%s\n", get_irp_prog_name());
	for (i = 0; i < ASIZE; i++) {
		if (i >= I_PHI_ARITY_S && i <= I_PHI_ARITY_E)
			fprintf(out, "%i %i\n", curr_vals[i], curr_vals[I_PHI_CNT]);
		else if (i >= I_CLS_SIZE_S && i <= I_CLS_SIZE_E)
			fprintf(out, "%i %i\n", curr_vals[i], curr_vals[I_CLS_CNT]);
		else
			fprintf(out, "%i\n", curr_vals[i]);
	}

    fclose(out);
}

void copystat_dump_pretty(ir_graph *irg) {
	int i;
	char buf[1024];

	snprintf(buf, sizeof(buf), "%s__%s", get_irp_prog_name(), get_entity_name(get_irg_entity(irg)));
	FILE *out = ffopen(buf, "pstat", "wt");

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
