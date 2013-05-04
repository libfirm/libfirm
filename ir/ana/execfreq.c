/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Compute an estimate of basic block executions.
 * @author      Adam M. Szalkowski
 * @date        28.05.2006
 *
 * Estimate execution frequencies. We do this by creating a system of linear
 * equations with the following observations:
 *   - Each edge leaving a block (block successors, not block_cfgpreds) has
 *     a probabilty between 0 and 1.0 that it is taken.
 *   - The execution frequencies of a basic block is the sum of all execution
 *     frequencies of its predecessor blocks scaled by the probability factors
 *     of the edges to the predecessors.
 *   - All outgoing probabilities have a sum of 1.0.
 * We then assign equaly distributed probablilities for normal controlflow
 * splits, and higher probabilities for backedges.
 *
 * Special case: In case of endless loops or "noreturn" calls some blocks have
 * no path to the end node, which produces undesired results (0, infinite
 * execution frequencies). We aleviate that by adding artificial edges from kept
 * blocks with a path to end.
 */
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
#define KEEP_FAC         0.1

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
	/* better convergence. */
	double init = 1.0 / size;
	for (int i = 0; i < size; ++i)
		x[i] = init;

	stat_ev_dbl("execfreq_matrix_size", size);
	stat_ev_tim_push();
	int    iter = 0;
	double dev;
	do {
		++iter;
		dev = gs_matrix_gauss_seidel(mat, x, size);
	} while (fabs(dev) > SEIDEL_TOLERANCE);
	stat_ev_tim_pop("execfreq_seidel_time");
	stat_ev_dbl("execfreq_seidel_iter", iter);

	return x;
}

static bool has_path_to_end(const ir_node *block)
{
	return Block_block_visited(block);
}

static bool is_kept_block(const ir_node *block)
{
	return irn_visited(block);
}

static double get_sum_succ_factors(const ir_node *block, double inv_loop_weight)
{
	const ir_loop *loop  = get_irn_loop(block);
	const int      depth = get_loop_depth(loop);

	double sum = 0.0;
	foreach_block_succ(block, edge) {
		const ir_node *succ       = get_edge_src_irn(edge);
		const ir_loop *succ_loop  = get_irn_loop(succ);
		int            succ_depth = get_loop_depth(succ_loop);

		double fac = 1.0;
		for (int d = succ_depth; d < depth; ++d) {
			fac *= inv_loop_weight;
		}
		sum += fac;
	}

	/* we add an artifical edge from each kept block which has no path to the
	 * end node */
	if (is_kept_block(block) && !has_path_to_end(block))
		sum += KEEP_FAC;

	return sum;
}

/*
 * Determine probability that predecessor pos takes this cf edge.
 */
static double get_cf_probability(const ir_node *bb, int pos,
                                 double inv_loop_weight)
{
	const ir_node *pred = get_Block_cfgpred_block(bb, pos);
	if (is_Bad(pred))
		return 0;

	const ir_loop *loop       = get_irn_loop(bb);
	const int      depth      = get_loop_depth(loop);
	const ir_loop *pred_loop  = get_irn_loop(pred);
	const int      pred_depth = get_loop_depth(pred_loop);

	double cur = 1.0;
	for (int d = depth; d < pred_depth; ++d) {
		cur *= inv_loop_weight;
	}
	double sum = get_sum_succ_factors(pred, inv_loop_weight);

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

	factors->min_non_zero = min_non_zero;

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
	assert(factors->min_non_zero > 0.0);
	assert(factors->m != 0.0);
	double f   = get_block_execfreq(block);
	int    res = (int) (f > factors->min_non_zero ? factors->m * f + factors->b : 1.0);
	return res;
}

static void block_walk_no_keeps(ir_node *block)
{
	if (Block_block_visited(block))
		return;
	mark_Block_block_visited(block);

	for (int n_block_cfgspreds = get_Block_n_cfgpreds(block), i = 0;
	     i < n_block_cfgspreds; ++i) {
	    ir_node *cfgpred_block = get_Block_cfgpred_block(block, i);
	    block_walk_no_keeps(cfgpred_block);
	}
}

void ir_estimate_execfreq(ir_graph *irg)
{
	double loop_weight = 10.0;

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE);

	/* compute a DFS.
	 * using a toposort on the CFG (without back edges) will propagate
	 * the values better for the gauss/seidel iteration.
	 * => they can "flow" from start to end. */
	dfs_t *dfs = dfs_new(&absgraph_irg_cfg_succ, irg);

	int          size = dfs_get_n_nodes(dfs);
	gs_matrix_t *mat  = gs_new_matrix(size, size);

	ir_node *const start_block = get_irg_start_block(irg);
	ir_node *const end_block   = get_irg_end_block(irg);
	const int      start_idx   = size - dfs_get_post_num(dfs, start_block) - 1;

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED
	                     | IR_RESOURCE_IRN_VISITED);
	inc_irg_block_visited(irg);
	/* mark all blocks reachable from end_block as (block)visited
	 * (so we can detect places which like endless-loops/noreturn calls which
	 *  do not reach the End block) */
	block_walk_no_keeps(end_block);
	/* mark al kept blocks as (node)visited */
	const ir_node *end          = get_irg_end(irg);
	int const      n_keepalives = get_End_n_keepalives(end);
	for (int k = n_keepalives - 1; k >= 0; --k) {
		ir_node *keep = get_End_keepalive(end, k);
		if (is_Block(keep))
			mark_irn_visited(keep);
	}

	double const inv_loop_weight = 1.0 / loop_weight;
	for (int idx = size - 1; idx >= 0; --idx) {
		const ir_node *bb = (ir_node*)dfs_get_post_num_node(dfs, size-idx-1);

		/* Sum of (execution frequency of predecessor * probability of cf edge) ... */
		for (int i = get_Block_n_cfgpreds(bb) - 1; i >= 0; --i) {
			const ir_node *pred           = get_Block_cfgpred_block(bb, i);
			int            pred_idx       = size - dfs_get_post_num(dfs, pred)-1;
			double         cf_probability = get_cf_probability(bb, i, inv_loop_weight);
			gs_matrix_set(mat, idx, pred_idx, cf_probability);
		}
		/* ... equals my execution frequency */
		gs_matrix_set(mat, idx, idx, -1.0);

		if (bb == end_block) {
			/* Add an edge from end to start.
			 * The problem is then an eigenvalue problem:
			 * Solve A*x = 1*x => (A-I)x = 0
			 */
			gs_matrix_set(mat, start_idx, idx, 1.0);

			/* add artifical edges from "kept blocks without a path to end"
			 * to end */
			for (int k = n_keepalives - 1; k >= 0; --k) {
				ir_node *keep = get_End_keepalive(end, k);
				if (!is_Block(keep) || has_path_to_end(keep))
					continue;

				double sum      = get_sum_succ_factors(keep, inv_loop_weight);
				double fac      = KEEP_FAC/sum;
				int    keep_idx = size - dfs_get_post_num(dfs, keep)-1;
				//ir_printf("SEdge %+F -> %+F, fak: %f\n", keep, end_block, fac);
				//gs_matrix_set(mat, idx, keep_idx, fac);
				gs_matrix_set(mat, start_idx, keep_idx, fac);
			}
		}
	}

	/* solve the system and delete the matrix */
	double *x = XMALLOCN(double, size);
	//ir_fprintf(stderr, "%+F:\n", irg);
	//gs_matrix_dump(mat, 100, 100, stderr);
	solve_lgs(mat, x, size);
	gs_delete_matrix(mat);

	/* compute the normalization factor.
	 * 1.0 / exec freq of start block.
	 * (note: start_idx is != 0 in strange cases involving endless loops,
	 *  probably a misfeature/bug)
	 */
	double start_freq = x[start_idx];
	double norm       = start_freq != 0.0 ? 1.0 / start_freq : 1.0;
	for (int idx = size - 1; idx >= 0; --idx) {
		ir_node *bb = (ir_node *) dfs_get_post_num_node(dfs, size - idx - 1);

		/* take abs because it sometimes can be -0 in case of endless loops */
		double freq = fabs(x[idx]) * norm;
		set_block_execfreq(bb, freq);
	}

	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED | IR_RESOURCE_IRN_VISITED);

	dfs_free(dfs);

	free(x);
}
