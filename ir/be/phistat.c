#include <stdio.h>
#include <string.h>

#include "config.h"

#include "irgraph.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irop.h"
#include "irprog.h"
#include "irmode_t.h"
#include "bephicongr_t.h"


#define SUMMARY_FILE_NAME "all.phi"

#define ARG_CNT_MAX 10
#define PHI_CLS_MAX 10

enum vals_t {
	I_PHI_CNT   = 0,
	I_BLK_CNT,
	I_SPACE3,
	I_ARG_CNT_S,
	I_ARG_CNT_E = I_ARG_CNT_S+ARG_CNT_MAX,
	I_SPACE1,
	I_PHI_CLS_S,
	I_PHI_CLS_E = I_PHI_CLS_S+PHI_CLS_MAX,
	I_SPACE2,
	I_INTERFP,          /* number of interfering pairs */
	I_INTERFV,          /* number of interfering values */
	I_PHICLSCNT,        /* number of phi classes */
	I_INTERFPHI,        /* number of phi classes which have interfering vals */
	I_CONST,
	I_PRED,
	I_GLOB,
	ASIZE
};

static int curr_vals[ASIZE];


static void phi_stat_walker(ir_node *node, void *env) {
 	int count, i, size;

	/* count all block nodes */
 	if (is_Block(node))
 		curr_vals[I_BLK_CNT]++;

	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;

	/* count all phi nodes */
	curr_vals[I_PHI_CNT]++;

	/* argument count */
	count = get_irn_arity(node);
	if (count > ARG_CNT_MAX)
		curr_vals[I_ARG_CNT_E]++;
	else
		curr_vals[I_ARG_CNT_S + count]++;

	/* type of argument */
	for (i = 0; i < count; i++) {
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


	size = get_phi_class_size(node);
	/* phi class count */
	if (size > 0)
		curr_vals[I_PHICLSCNT]++;

	/* phi class size */
	if (size > PHI_CLS_MAX)
		curr_vals[I_PHI_CLS_E]++;
	else
		curr_vals[I_PHI_CLS_S + size]++;

	/* count of interfering pairs / values */
	curr_vals[I_INTERFP] += get_irn_phi_info(node)->interf_pairs;
	curr_vals[I_INTERFV] += get_irn_phi_info(node)->interf_vals;
	if (get_irn_phi_info(node)->interf_vals > 0)
		curr_vals[I_INTERFPHI]++;
}


/**
 * Dump values and some annotations for current irp
 */
static void dump_files(void) {
	int i, sum1, sum2, sum3, next;
	FILE *out;
	char buf[200];

	sum1 = curr_vals[I_CONST] + curr_vals[I_PRED] + curr_vals[I_GLOB];
	sum2 = 0;
	for (i = I_ARG_CNT_S; i<=I_ARG_CNT_E; i++)
		sum2 += curr_vals[i];
	sum3 = 0;
	for (i = I_PHI_CLS_S; i<=I_PHI_CLS_E; i++)
		sum3 += curr_vals[i];

	next = sprintf(buf, get_irp_prog_name());
	sprintf(buf+next, ".phi.stat");
	out = fopen(buf, "wt");

	fprintf(out, "Phis %i\n", curr_vals[I_PHI_CNT]);
	fprintf(out, "Blks %i\n", curr_vals[I_BLK_CNT]);

	fprintf(out, "\nArgument counts\n");
	for (i = I_ARG_CNT_S; i<=I_ARG_CNT_E; i++)
		fprintf(out, "%2i %2.4f\n", i-I_ARG_CNT_S, (double) curr_vals[i] / sum2);

	fprintf(out, "\nPhi class sizes\n");
	for (i = I_PHI_CLS_S; i<=I_PHI_CLS_E; i++)
		fprintf(out, "%2i %2.4f\n", i-I_PHI_CLS_S, (double) curr_vals[i] / sum3);

	fprintf(out, "\n");
	fprintf(out, "Interf Pairs %d\n", curr_vals[I_INTERFP]);
	fprintf(out, "Interf Values %d\n", curr_vals[I_INTERFV]);
	fprintf(out, "PhiCls Count %d\n", curr_vals[I_PHICLSCNT]);
	fprintf(out, "Interf PhisCl %d\n", curr_vals[I_INTERFPHI]);
	fprintf(out, "Const %2.2f\n", (double) curr_vals[I_CONST] / sum1);
	fprintf(out, "Pred %2.2f\n", (double) curr_vals[I_PRED] / sum1);
	fprintf(out, "Glob %2.2f\n", (double) curr_vals[I_GLOB] / sum1);

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
		for (i = 0; i<ASIZE; i++)
			fscanf(all, "%i\n", &vals[i]);
	    fclose(all);
	} else {
		for (i = 0; i<ASIZE; i++)
			vals[i] = 0;
	}

	/* write out */
	all = fopen(SUMMARY_FILE_NAME, "wt");
	for (i = 0; i<ASIZE; i++)
		fprintf(all, "%i\n", vals[i]+curr_vals[i]);
    fclose(all);
}

void do_phi_statistics(void) {
	int i, n;
	curr_vals[I_SPACE1] = -1;
	curr_vals[I_SPACE2] = -1;
	curr_vals[I_SPACE3] = -1;

	for (i = 0, n = get_irp_n_irgs(); i < n; i++) {
		ir_graph *irg = get_irp_irg(i);
		irg_walk_graph(irg, phi_stat_walker, NULL, NULL);
		curr_vals[I_BLK_CNT] -= 2;
	}

	dump_files();
	update_all_file();
}
