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

/*
 * 0  to 10 arg-count of phi funct
 * 11 to 20 phi-congr-class size
 * 11 # of const args
 * 12 # of args defined in cf-pred block
 * 13 # of args defines elsewhere
 * 14 size of phi congruence class
 */

#define ARG_CNT_MAX 10
#define PHI_CLS_MAX 10

#define I_ARG_CNT_S 0
#define I_ARG_CNT_E (I_ARG_CNT_S+ARG_CNT_MAX)
#define I_PHI_CLS_S (I_ARG_CNT_E+1)
#define I_PHI_CLS_E (I_PHI_CLS_S+PHI_CLS_MAX)
#define I_CONST (I_PHI_CLS_E+1)
#define I_PRED (I_CONST+1)
#define I_GLOB (I_PRED+1)
#define ASIZE (I_GLOB+1)

static int curr_vals[ASIZE];

static void phi_stat_post_walker(ir_node *node, void *env) {
 	int size;
	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;

	size = get_phi_class_size(node);
	if (size > PHI_CLS_MAX)
		curr_vals[I_PHI_CLS_E]++;
	else
		curr_vals[I_PHI_CLS_S + size]++;
}

static void phi_stat_walker(ir_node *node, void *env) {
 	int count, i;

	if (!(is_Phi(node) && mode_is_datab(get_irn_mode(node)))) return;

	/* argument count */
	count = get_irn_arity(node);
	if (count > ARG_CNT_MAX)
		curr_vals[I_ARG_CNT_E]++;
	else
		curr_vals[I_ARG_CNT_S + count]++;

	/* type of argument */
	for (i = 0; i < count; i++) {
		ir_node *arg = get_irn_n(node, i);

		if (iro_Const == get_irn_opcode(arg)) {
			curr_vals[I_CONST]++;
			continue;
		}

		ir_node *block_of_arg = get_nodes_block(arg);
		ir_node *block_ith_pred = get_nodes_block(get_irn_n(get_nodes_block(node), i));

		if (block_of_arg == block_ith_pred) {
			curr_vals[I_PRED]++;
			continue;
		}

		curr_vals[I_GLOB]++;
	}

	/* phi congruence class */
	det_phi_congr_class(node);
}


static void dump_files(void) {
	int i, sum1, sum2, next;
	FILE *out;
	char buf[200];

	sum1 = curr_vals[I_CONST] + curr_vals[I_PRED] + curr_vals[I_GLOB];
	sum2 = 0;
	for (i = I_ARG_CNT_S; i<=I_ARG_CNT_E; i++)
		sum2 += curr_vals[i];

	next = sprintf(buf, get_irp_prog_name());
	sprintf(buf+next, ".phi.argcount");

	out = fopen(buf, "wt");
	for (i = I_ARG_CNT_S; i<=I_ARG_CNT_E; i++)
		fprintf(out, "%2i %2.2f\n", i, (double) curr_vals[i] / sum2);
	fclose(out);

	sprintf(buf+next, ".phi.defloc");
	out = fopen(buf, "wt");
	fopen(buf, "wt");
	fprintf(out, "Const %2.2f\n", (double) curr_vals[I_CONST] / sum1);
	fprintf(out, "Pred %2.2f\n", (double) curr_vals[I_PRED] / sum1);
	fprintf(out, "Glob %2.2f\n", (double) curr_vals[I_GLOB] / sum1);
	fclose(out);
}


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

	for (i = 0, n = get_irp_n_irgs(); i < n; i++) {
		ir_graph *irg = get_irp_irg(i);
		irg_walk_graph(irg, phi_stat_walker, NULL, NULL);
		irg_walk_graph(irg, phi_stat_post_walker, NULL, NULL);
	}

	dump_files();
	update_all_file();
}
