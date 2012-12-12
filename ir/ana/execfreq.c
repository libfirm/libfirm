/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Compute an estimate of basic block executions.
 * @author      Adam M. Szalkowski
 * @date        28.05.2006
 */
#include "config.h"

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "gaussseidel.h"

#include "set.h"
#include "hashptr.h"
#include "debug.h"
#include "statev_t.h"
#include "dfs_t.h"
#include "absgraph.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"
#include "irgwalk.h"
#include "iredges.h"
#include "irouts.h"
#include "irprintf.h"
#include "util.h"
#include "irhooks.h"
#include "irnodehashmap.h"

#include "execfreq_t.h"

#define EPSILON          1e-5
#define UNDEF(x)         (fabs(x) < EPSILON)
#define SEIDEL_TOLERANCE 1e-7

#define MAX_INT_FREQ 1000000

static hook_entry_t hook;

double get_block_execfreq(const ir_node *block)
{
	return block->attr.block.execfreq;
}

void set_block_execfreq(ir_node *block, double newfreq)
{
	block->attr.block.execfreq = newfreq;
}

static void exec_freq_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	(void)ctx;
	if (!is_Block(irn))
		return;
	double freq = get_block_execfreq(irn);
	if (freq != 0.0)
		fprintf(f, "execution frequency: %g\n", freq);
}

void init_execfreq(void)
{
	memset(&hook, 0, sizeof(hook));
	hook.hook._hook_node_info = exec_freq_node_info;
	register_hook(hook_node_info, &hook);
}

void exit_execfreq(void)
{
	unregister_hook(hook_node_info, &hook);
}


static double *solve_lgs(gs_matrix_t *mat, double *x, int size)
{
	double init = 1.0 / size;
	double dev;
	int i, iter = 0;

	/* better convergence. */
	for (i = 0; i < size; ++i)
		x[i] = init;

	stat_ev_dbl("execfreq_matrix_size", size);
	stat_ev_tim_push();
	do {
		++iter;
		dev = gs_matrix_gauss_seidel(mat, x, size);
	} while (fabs(dev) > SEIDEL_TOLERANCE);
	stat_ev_tim_pop("execfreq_seidel_time");
	stat_ev_dbl("execfreq_seidel_iter", iter);

	return x;
}

/*
 * Determine probability that predecessor pos takes this cf edge.
 */
static double get_cf_probability(const ir_node *bb, int pos, double loop_weight)
{
	double         sum = 0.0;
	double         cur = 1.0;
	double         inv_loop_weight = 1./loop_weight;
	const ir_node *pred = get_Block_cfgpred_block(bb, pos);
	const ir_loop *pred_loop;
	int            pred_depth;
	const ir_loop *loop;
	int            depth;
	int            d;

	if (is_Bad(pred))
		return 0;

	loop       = get_irn_loop(bb);
	depth      = get_loop_depth(loop);
	pred_loop  = get_irn_loop(pred);
	pred_depth = get_loop_depth(pred_loop);

	for (d = depth; d < pred_depth; ++d) {
		cur *= inv_loop_weight;
	}

	foreach_block_succ(pred, edge) {
		const ir_node *succ       = get_edge_src_irn(edge);
		const ir_loop *succ_loop  = get_irn_loop(succ);
		int            succ_depth = get_loop_depth(succ_loop);

		double         fac = 1.0;
		for (d = succ_depth; d < pred_depth; ++d) {
			fac *= inv_loop_weight;
		}
		sum += fac;
	}

	return cur/sum;
}

static double *freqs;
static double  min_non_zero;
static double  max_freq;

static void collect_freqs(ir_node *node, void *data)
{
	(void) data;
	double freq = get_block_execfreq(node);
	if (freq > max_freq)
		max_freq = freq;
	if (freq > 0.0 && freq < min_non_zero)
		min_non_zero = freq;
	ARR_APP1(double, freqs, freq);
}

void ir_calculate_execfreq_int_factors(ir_execfreq_int_factors *factors,
                                       ir_graph *irg)
{
	/* compute m and b of the transformation used to convert the doubles into
	 * scaled ints */
	freqs = NEW_ARR_F(double, 0);
	min_non_zero = HUGE_VAL;
	max_freq     = 0.0;
	irg_block_walk_graph(irg, collect_freqs, NULL, NULL);

	/*
	 * find the smallest difference of the execution frequencies
	 * we try to ressolve it with 1 integer.
	 */
	size_t n_freqs       = ARR_LEN(freqs);
	double smallest_diff = 1.0;
	for (size_t i = 0; i < n_freqs; ++i) {
		if (freqs[i] <= 0.0)
			continue;

		for (size_t j = i + 1; j < n_freqs; ++j) {
			double diff = fabs(freqs[i] - freqs[j]);

			if (!UNDEF(diff))
				smallest_diff = MIN(diff, smallest_diff);
		}
	}

	double l2 = min_non_zero;
	double h2 = max_freq;
	double l1 = 1.0;
	double h1 = MAX_INT_FREQ;

	/* according to that the slope of the translation function is
	 * 1.0 / smallest_diff */
	factors->m = 1.0 / smallest_diff;

	/* the abscissa is then given by */
	factors->b = l1 - factors->m * l2;

	/*
	 * if the slope is so high that the largest integer would be larger than
	 * MAX_INT_FREQ set the largest int freq to that upper limit and recompute
	 * the translation function
	 */
	if (factors->m * h2 + factors->b > MAX_INT_FREQ) {
		factors->m = (h1 - l1) / (h2 - l2);
		factors->b = l1 - factors->m * l2;
	}

	DEL_ARR_F(freqs);
}

int get_block_execfreq_int(const ir_execfreq_int_factors *factors,
                           const ir_node *block)
{
	double f   = get_block_execfreq(block);
	int    res = (int) (f > factors->min_non_zero ? factors->m * f + factors->b : 1.0);
	return res;
}

void ir_estimate_execfreq(ir_graph *irg)
{
	double loop_weight = 10.0;

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);

	/* compute a DFS.
	 * using a toposort on the CFG (without back edges) will propagate
	 * the values better for the gauss/seidel iteration.
	 * => they can "flow" from start to end.
	 */
	dfs_t *dfs = dfs_new(&absgraph_irg_cfg_succ, irg);

	int          size = dfs_get_n_nodes(dfs);
	gs_matrix_t *mat  = gs_new_matrix(size, size);

	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const end_block   = get_irg_end_block(irg);

	for (int idx = size - 1; idx >= 0; --idx) {
		const ir_node *bb = (ir_node*)dfs_get_post_num_node(dfs, size-idx-1);

		/* Sum of (execution frequency of predecessor * probability of cf edge) ... */
		for (int i = get_Block_n_cfgpreds(bb) - 1; i >= 0; --i) {
			const ir_node *pred           = get_Block_cfgpred_block(bb, i);
			int            pred_idx       = size - dfs_get_post_num(dfs, pred)-1;
			double         cf_probability = get_cf_probability(bb, i, loop_weight);
			gs_matrix_set(mat, idx, pred_idx, cf_probability);
		}
		/* ... equals my execution frequency */
		gs_matrix_set(mat, idx, idx, -1.0);

		/* Add an edge from end to start.
		 * The problem is then an eigenvalue problem:
		 * Solve A*x = 1*x => (A-I)x = 0
		 */
		if (bb == end_block) {
			int const s_idx = size - dfs_get_post_num(dfs, start_block) - 1;
			gs_matrix_set(mat, s_idx, idx, 1.0);
		}
	}

	/*
	 * Also add an edge for each kept block to start.
	 *
	 * This avoid strange results for e.g. an irg containing a exit()-call
	 * which block has no cfg successor.
	 */
	int            s_idx        = size - dfs_get_post_num(dfs, start_block)-1;
	const ir_node *end          = get_irg_end(irg);
	int            n_keepalives = get_End_n_keepalives(end);
	for (int idx = n_keepalives - 1; idx >= 0; --idx) {
		ir_node *keep = get_End_keepalive(end, idx);
		if (!is_Block(keep) || get_irn_n_edges_kind(keep, EDGE_KIND_BLOCK) > 0)
			continue;

		int k_idx = size-dfs_get_post_num(dfs, keep)-1;
		if (k_idx > 0)
			gs_matrix_set(mat, s_idx, k_idx, 1.0);
	}

	/* solve the system and delete the matrix */
	double *x = XMALLOCN(double, size);
	solve_lgs(mat, x, size);
	gs_delete_matrix(mat);

	/* compute the normalization factor.
	 * 1.0 / exec freq of start block.
	 * (note: start_idx is != 0 in strange cases involving endless loops,
	 *  probably a misfeature/bug)
	 */
	int    start_idx  = size - dfs_get_post_num(dfs, start_block) - 1;
	double start_freq = x[start_idx];
	double norm       = start_freq != 0.0 ? 1.0 / start_freq : 1.0;

	for (int idx = size - 1; idx >= 0; --idx) {
		ir_node *bb = (ir_node *) dfs_get_post_num_node(dfs, size - idx - 1);

		/* take abs because it sometimes can be -0 in case of endless loops */
		double freq = fabs(x[idx]) * norm;
		set_block_execfreq(bb, freq);
	}

	dfs_free(dfs);

	xfree(x);
}
