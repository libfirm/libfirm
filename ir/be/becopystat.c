/**
 * @author Daniel Grund
 * @date 19.04.2005
 */
#include <string.h>
#include "phiclass_t.h"
#include "irprog.h"
#include "becopyopt.h"
#include "becopystat.h"

#ifdef DO_STAT

static pset *all_phi_nodes;
static pset *all_phi_classes;
static pset *all_copy_nodes;

void stat_init(void) {
	all_phi_nodes = pset_new_ptr_default();
	all_phi_classes = pset_new_ptr_default();
	all_copy_nodes = pset_new_ptr_default();
	phi_class_init();
}

void stat_reset(void) {
	int i;
	for (i = 0; i < ASIZE; ++i)
		curr_vals[i] = 0;
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
static void stat_phi_node(ir_node *phi) {
 	int arity, i;

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

		if (arg == phi) {
			curr_vals[I_PHI_ARG_SELF]++;
			continue;
		}

		curr_vals[I_COPIES_MAX]++; /* if arg!=phi this is a possible copy */

		if (values_interfere(phi, arg))
			curr_vals[I_COPIES_IF]++;

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
//TODO stat_copy_node
static void stat_copy_node(ir_node *root) {
	curr_vals[I_CPY_CNT]++;
//	if (values_interfere(root, arg))
//		curr_vals[I_COPIES_IF]++;
}

/**
 * Collect phi class data
 */
static void stat_phi_class(pset *pc) {
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
	members = malloc(size * sizeof(*members));
	for (i = 0, p = pset_first(pc); p; p = pset_next(pc))
		members[i++] = p;
	assert(i == size);

	/* determine interference of phi class members */
	curr_vals[I_CLS_IF_MAX] += size*(size-1)/2;
	if_free = 1;
	for (i = 0; i < size-1; ++i)
		for (o = i+1; o < size; ++o)
			if (values_interfere(members[i], members[o])) {
				if_free = 0;
				curr_vals[I_CLS_IF_CNT]++;
			}

	/* Does this phi class have an inner interference? */
	curr_vals[I_CLS_IF_FREE] += if_free;

	free(members);
}

void stat_collect_irg(ir_graph *irg) {
	ir_node *n;
	pset *pc;

	irg_walk_graph(irg, stat_walker, NULL, NULL);
	curr_vals[I_BLOCKS] -= 2; /* substract 2 for start and end block */

	all_phi_classes = phi_class_compute_by_phis(all_phi_nodes);

	for (n = pset_first(all_phi_nodes); n; n = pset_next(all_phi_nodes))
		stat_phi_node(n);

	for (n = pset_first(all_copy_nodes); n; n = pset_next(all_copy_nodes))
		stat_copy_node(n);

	for (pc = pset_first(all_phi_classes); pc; pc = pset_next(all_phi_classes))
		stat_phi_class(pc);

}

void stat_dump(ir_graph *irg) {
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

//TODO stat_dump_pretty
void stat_dump_pretty(ir_graph *irg) {
	int i;
	FILE *out = ffopen(get_entity_name(get_irg_entity(irg)), "pretty", "wt");

	fprintf(out, "\nPhi argument types\n");
	fprintf(out, "Total     %4d\n", curr_vals[I_PHI_ARG_CNT]);
	fprintf(out, "Constants %4d\n", curr_vals[I_PHI_ARG_CONST]);
	fprintf(out, "CF-Pred   %4d\n", curr_vals[I_PHI_ARG_PRED]);
	fprintf(out, "Others    %4d\n", curr_vals[I_PHI_ARG_GLOB]);

	fprintf(out, "\nPhi class interference\n");
	fprintf(out, "Blocks         %4d\n", curr_vals[I_BLOCKS]);
	fprintf(out, "Phis           %4d\n", curr_vals[I_PHI_CNT]);

	fprintf(out, "\nPhi arity\n");
	for (i = I_PHI_ARITY_S; i<=I_PHI_ARITY_E; i++)
		fprintf(out, "%2i %4d\n", i-I_PHI_ARITY_S, curr_vals[i]);

	fprintf(out, "\nPhi class sizes\n");
	for (i = I_CLS_SIZE_S; i<=I_CLS_SIZE_E; i++)
		fprintf(out, "%2i %4d\n", i-I_CLS_SIZE_S, curr_vals[i]);

	fprintf(out, "\n\nTotal nodes:    %4d\n", curr_vals[I_ALL_NODES]);

	fclose(out);
}

#endif
