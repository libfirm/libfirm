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
#include "bephicongr_t.h"
#include "bechordal.h"


#define SUMMARY_FILE_NAME "all.phistat"

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

static int curr_vals[ASIZE];


/**
 * Dump statistic values and some annotations for current irp
 */
static void dump_file(void) {
	int i, next;
	FILE *out;
	char buf[200];

	next = sprintf(buf, get_irp_prog_name());
	sprintf(buf+next, ".phistat");
	out = fopen(buf, "wt");

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


/**
 * Updates the summary file with cumulated values
 */
static void update_all_file(void) {
    int i;
	FILE *all;
	int vals[ASIZE];

	/* read in */
	all = fopen(SUMMARY_FILE_NAME, "rt");
    if (all) {
		for (i = 0; i<ASIZE; i++) {
			if (i >= I_ARITY_S && i <= I_ARITY_E)
				fscanf(all, "%i %i\n", &vals[i], &vals[I_ALL_NODES]);
			else
				fscanf(all, "%i\n", &vals[i]);
		}
	    fclose(all);
	} else {
		for (i = 0; i<ASIZE; i++)
			vals[i] = 0;
	}

	/* write out */
	all = fopen(SUMMARY_FILE_NAME, "wt");
	for (i = 0; i<ASIZE; i++) {
		if (i >= I_ARITY_S && i <= I_ARITY_E)
			fprintf(all, "%i %i\n", vals[i]+curr_vals[i], vals[I_ALL_NODES]+curr_vals[I_ALL_NODES]);
		else
			fprintf(all, "%i\n", vals[i]+curr_vals[i]);
	}
    fclose(all);
}

/**
 * Collect the statistical data
 */
static void phi_stat_walker(ir_node *node, void *env) {
 	int arity, i;

	/* count all nodes */
	curr_vals[I_ALL_NODES]++;

	/* count all block nodes */
 	if (is_Block(node))
 		curr_vals[I_BLOCKS]++;

	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;

	/* count all phi nodes */
	curr_vals[I_PHIS]++;

	/* argument count */
	arity = get_irn_arity(node);
	curr_vals[I_ARG] += arity;
	if (arity > MAX_ARITY)
		curr_vals[I_ARITY_E]++;
	else
		curr_vals[I_ARITY_S + arity]++;

	/* type of argument */
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

static void phi_class_stat_walker(ir_node *node, void *env) {
	int i, o, size, doit, sth_interfered;
	ir_node **members, *p;
	pset *pc;

	pc = get_phi_class(node);
	if (pc) {

		/* phi class count */
		curr_vals[I_PHICLS]++;

		/* phi class size */
		size = pset_count(pc);
		if (size > MAX_CLS_SIZE)
			curr_vals[I_CLS_SIZE_E]++;
		else
			curr_vals[I_CLS_SIZE_S + size]++;

		/* count interfering pairs / values */
		members = (ir_node **) malloc(size * sizeof(ir_node*));

		for (i=0, p = (ir_node *)pset_first(pc); p; p = (ir_node *)pset_next(pc))
			members[i++] = p;
		assert(i == size);

		/* determine interference of phi args */
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

		/* has this phi class an interference? */
		curr_vals[I_PHICLSI] += sth_interfered;
		free(members);
	}
}


void do_phi_statistics(void) {
	int i, n;
	curr_vals[I_SPACE1] = -1;
	curr_vals[I_SPACE2] = -1;
	curr_vals[I_SPACE3] = -1;

	for (i = 0, n = get_irp_n_irgs(); i < n; i++) {
		ir_graph *irg = get_irp_irg(i);
		irg_walk_graph(irg, phi_stat_walker, NULL, NULL);
		irg_walk_graph(irg, phi_class_stat_walker, NULL, NULL);
		curr_vals[I_BLOCKS] -= 2;
	}

	dump_file();
	update_all_file();
}
