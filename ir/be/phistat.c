/**
 * @author Daniel Grund
 * @date 09.12.2004
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"

#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irop.h"
#include "irprog.h"
#include "irmode_t.h"

#include "bechordal.h"
#include "phistat.h"

#define MAX_ARITY 10
#define MAX_CLS_SIZE 10

enum vals_t {
	I_ARG         = 0,
	I_CONST,
	I_PRED,
	I_GLOB,

	I_SPACE1,
	I_BLOCKS,
	I_PHIS,
	I_PHICLS,
	I_PHICLSI,
	I_PAIRS,
	I_PAIRSI,
	I_VALUES,
	I_VALUESI,

	I_SPACE2,
	I_ARITY_S,
	I_ARITY_E    = I_ARITY_S+MAX_ARITY,

	I_SPACE3,
	I_CLS_SIZE_S,
	I_CLS_SIZE_E = I_CLS_SIZE_S+MAX_CLS_SIZE,
	I_ALL_NODES,
	ASIZE
};

/**
 * Holds current values. Values are added till next
 * phi_stat_reset
 */
static int curr_vals[ASIZE];


/**
 * Resets the array holding the data
 */
void phi_stat_reset(void) {
	int i;

	for (i = 0; i < ASIZE; ++i)
		curr_vals[i] = 0;
	curr_vals[I_SPACE1] = -1;
	curr_vals[I_SPACE2] = -1;
	curr_vals[I_SPACE3] = -1;
}


/**
 * Collect general data
 */
static void stat_walker(ir_node *node, void *env) {
	/* count all nodes */
	curr_vals[I_ALL_NODES]++;

	/* count all block nodes */
 	if (is_Block(node))
 		curr_vals[I_BLOCKS]++;
}


/**
 * Collect phi node data
 */
static void phi_node_stat(ir_node *node) {
 	int arity, i;

	/* count all phi nodes */
	curr_vals[I_PHIS]++;

	/* argument count */
	arity = get_irn_arity(node);
	curr_vals[I_ARG] += arity;
	if (arity > MAX_ARITY)
		curr_vals[I_ARITY_E]++;
	else
		curr_vals[I_ARITY_S + arity]++;

	/* type of argument {const, pred, glob} */
	for (i = 0; i < arity; i++) {
        ir_node *block_of_arg, *block_ith_pred;
		ir_node *arg = get_irn_n(node, i);

		if (iro_Const == get_irn_opcode(arg)) {
			curr_vals[I_CONST]++;
			continue;
		}

		block_of_arg = get_nodes_block(arg);
		block_ith_pred = get_nodes_block(get_irn_n(get_nodes_block(node), i));

		if (block_of_arg == block_ith_pred) {
			curr_vals[I_PRED]++;
			continue;
		}

		curr_vals[I_GLOB]++;
	}
}


/**
 * Collect phi class data
 */
static void phi_class_stat(pset *pc) {
	int i, o, size, doit, sth_interfered;
	ir_node **members, *p;

	/* phi class count */
	curr_vals[I_PHICLS]++;

	/* phi class size */
	size = pset_count(pc);
	if (size > MAX_CLS_SIZE)
		curr_vals[I_CLS_SIZE_E]++;
	else
		curr_vals[I_CLS_SIZE_S + size]++;

	/* get an array of all members for double iterating */
	members = (ir_node **) malloc(size * sizeof(ir_node*));
	for (i=0, p = (ir_node *)pset_first(pc); p; p = (ir_node *)pset_next(pc))
		members[i++] = p;
	assert(i == size);

	/* determine interference of phi class members */
	curr_vals[I_VALUES] += size;
	sth_interfered = 0;
	for (i = 0; i < size-1; ++i) {
		doit = 1;
		for (o = i+1; o < size; ++o) {
			curr_vals[I_PAIRS]++;
			if (phi_ops_interfere(members[i], members[o])) {
				sth_interfered = 1;
				curr_vals[I_PAIRSI]++;
				if (doit) {
					curr_vals[I_VALUESI]++;
					doit = 0;
				}
			}
		}
	}

	/* Does this phi class have an inner interference? */
	curr_vals[I_PHICLSI] += sth_interfered;

	free(members);
}


/**
 * Collect all stat data
 */
void phi_stat_collect(ir_graph *irg, pset *all_phi_nodes, pset *all_phi_classes) {
	ir_node *n;
	pset *pc;

	irg_walk_graph(irg, stat_walker, NULL, NULL);
	curr_vals[I_BLOCKS] -= 2;

	for (n = (ir_node *)pset_first(all_phi_nodes); n; n = (ir_node *)pset_next(all_phi_nodes))
		phi_node_stat(n);

	for (pc = (pset *)pset_first(all_phi_classes); pc; pc = (pset *)pset_next(all_phi_classes))
		phi_class_stat(pc);
}


/**
 * Dump statistic values in raw format
 */
static void dump_file(char *filename, int stat[ASIZE]) {
	FILE *file;
	int i;

	if (! (file = fopen(filename, "wt"))) {
		//TODO DBG((dbgmod, 0, "Cannot open file for writing\n"));
		return;
	}

	for (i = 0; i < ASIZE; i++) {
		if (i >= I_ARITY_S && i <= I_ARITY_E)
			fprintf(file, "%i %i\n", stat[i], stat[I_ALL_NODES]);
		else
			fprintf(file, "%i\n", stat[i]);
	}

    fclose(file);
}


/**
 * Updates a cumulative file with the current values.
 */
static void update_file(char *filename) {
    int i;
	FILE *all;
	int vals[ASIZE];

	/* read in */
	all = fopen(filename, "rt");

    if (all) {
		for (i = 0; i < ASIZE; i++) {
			if (i >= I_ARITY_S && i <= I_ARITY_E)
				fscanf(all, "%i %i\n", &vals[i], &vals[I_ALL_NODES]);
			else
				fscanf(all, "%i\n", &vals[i]);
		}
	    fclose(all);
	} else {
		for (i = 0; i < ASIZE; i++)
			vals[i] = 0;
	}

	/* add current values */
	for (i = 0; i < ASIZE; i++)
		vals[i] += curr_vals[i];

	/* write out */
	dump_file(filename, vals);
}


/**
 * Dumps the current contents of the values array to a file.
 * Updates a cumulative file.
 */
void phi_stat_dump(char *filename, char *cum_filename) {
	if (filename)
		dump_file(filename, curr_vals);
	if (cum_filename)
		update_file(cum_filename);
}


/**
 * Dumps the current contents of the values array
 * and annotations to a file.
 */
void phi_stat_dump_pretty(char *filename) {
	int i;
	FILE *out;

	if (! (out = fopen(filename, "wt"))) {
		//TODO DBG((dbgmod, 0, "Cannot open file for writing\n"));
		return;
	}

	fprintf(out, "\nPhi argument types\n");
	fprintf(out, "Total     %4d\n", curr_vals[I_ARG]);
	fprintf(out, "Constants %4d\n", curr_vals[I_CONST]);
	fprintf(out, "CF-Pred   %4d\n", curr_vals[I_PRED]);
	fprintf(out, "Others    %4d\n", curr_vals[I_GLOB]);

	fprintf(out, "\nPhi class interference\n");
	fprintf(out, "Blocks         %4d\n", curr_vals[I_BLOCKS]);
	fprintf(out, "Phis           %4d\n", curr_vals[I_PHIS]);
	fprintf(out, "Interf classes %4d of %4d\n", curr_vals[I_PHICLSI], curr_vals[I_PHICLS]);
	fprintf(out, "Interf pairs   %4d of %4d\n", curr_vals[I_PAIRSI], curr_vals[I_PAIRS]);
	fprintf(out, "Interf values  %4d of %4d\n", curr_vals[I_PAIRSI], curr_vals[I_VALUES]);

	fprintf(out, "\nPhi arity\n");
	for (i = I_ARITY_S; i<=I_ARITY_E; i++)
		fprintf(out, "%2i %4d\n", i-I_ARITY_S, curr_vals[i]);

	fprintf(out, "\nPhi class sizes\n");
	for (i = I_CLS_SIZE_S; i<=I_CLS_SIZE_E; i++)
		fprintf(out, "%2i %4d\n", i-I_CLS_SIZE_S, curr_vals[i]);

	fprintf(out, "\n\nTotal nodes:    %4d\n", curr_vals[I_ALL_NODES]);

	fclose(out);
}
