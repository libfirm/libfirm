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
 * @brief       Compute an estimate of basic block executions.
 * @author      Adam M. Szalkowski
 * @date        28.05.2006
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "gaussseidel.h"

#include "firm_common_t.h"
#include "set.h"
#include "hashptr.h"
#include "debug.h"
#include "statev.h"
#include "dfs_t.h"
#include "absgraph.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"
#include "irgwalk.h"
#include "iredges.h"
#include "irprintf.h"
#include "irtools.h"
#include "irhooks.h"

#include "execfreq.h"

/* enable to also solve the equations with Gauss-Jordan */
#undef COMPARE_AGAINST_GAUSSJORDAN

#ifdef COMPARE_AGAINST_GAUSSJORDAN
#include "gaussjordan.h"
#endif


#define EPSILON		     1e-5
#define UNDEF(x)         (fabs(x) < EPSILON)
#define SEIDEL_TOLERANCE 1e-7

#define MAX_INT_FREQ 1000000

#define set_foreach(s,i) for((i)=set_first((s)); (i); (i)=set_next((s)))

typedef struct _freq_t {
	const ir_node    *irn;
	int               idx;
	double            freq;
} freq_t;

struct ir_exec_freq {
	set *set;
	hook_entry_t hook;
	double max;
	double min_non_zero;
	double m, b;
	unsigned infeasible : 1;
};

static int
cmp_freq(const void *a, const void *b, size_t size)
{
	const freq_t *p = a;
	const freq_t *q = b;
	(void) size;

	return !(p->irn == q->irn);
}

static freq_t *
set_find_freq(set * set, const ir_node * irn)
{
	freq_t     query;

	query.irn = irn;
	return set_find(set, &query, sizeof(query), HASH_PTR(irn));
}

static freq_t *
set_insert_freq(set * set, const ir_node * irn)
{
	freq_t query;

	query.irn = irn;
	query.freq = 0.0;
	query.idx  = -1;
	return set_insert(set, &query, sizeof(query), HASH_PTR(irn));
}

double
get_block_execfreq(const ir_exec_freq *ef, const ir_node * irn)
{
	if(!ef->infeasible) {
		set *freqs = ef->set;
		freq_t *freq;
		assert(is_Block(irn));
		freq = set_find_freq(freqs, irn);
		assert(freq);

		assert(freq->freq >= 0);
		return freq->freq;
	}

	return 1.0;
}

unsigned long
get_block_execfreq_ulong(const ir_exec_freq *ef, const ir_node *bb)
{
	double f       = get_block_execfreq(ef, bb);
	int res        = (int) (f > ef->min_non_zero ? ef->m * f + ef->b : 1.0);
	return res;
}

static double *
solve_lgs(gs_matrix_t *mat, double *x, int size)
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
	} while(fabs(dev) > SEIDEL_TOLERANCE);
	stat_ev_tim_pop("execfreq_seidel_time");
	stat_ev_dbl("execfreq_seidel_iter", iter);

#ifdef COMPARE_AGAINST_GAUSSJORDAN
	{
		double *nw = xmalloc(size * size * sizeof(*nw));
		double *nx = xmalloc(size * sizeof(*nx));

		memset(nx, 0, size * sizeof(*nx));
		gs_matrix_export(mat, nw, size);

		stat_ev_tim_push();
		firm_gaussjordansolve(nw, nx, size);
		stat_ev_tim_pop("execfreq_jordan_time");

		xfree(nw);
		xfree(nx);
	}
#endif

	return x;
}

static double
get_cf_probability(ir_node *bb, int pos, double loop_weight)
{
	double  sum = 0.0;
	double  cur = 0.0;
	const ir_node *pred = get_Block_cfgpred_block(bb, pos);
	const ir_loop *pred_loop = get_irn_loop(pred);
	int pred_depth = get_loop_depth(pred_loop);
	const ir_edge_t *edge;

	cur = get_loop_depth(get_irn_loop(bb)) < get_loop_depth(get_irn_loop(pred)) ? 1.0 : loop_weight;

	foreach_block_succ(pred, edge) {
		const ir_node *block = get_edge_src_irn(edge);
		const ir_loop *loop = get_irn_loop(block);
		int depth = get_loop_depth(loop);
		sum += depth < pred_depth ? 1.0 : loop_weight;
	}

	return cur/sum;
}

static void exec_freq_node_info(void *ctx, FILE *f, const ir_node *irn)
{
	if(is_Block(irn)) {
		ir_exec_freq *ef = ctx;
		fprintf(f, "execution frequency: %g/%lu\n", get_block_execfreq(ef, irn), get_block_execfreq_ulong(ef, irn));
	}
}

ir_exec_freq *create_execfreq(ir_graph *irg)
{
	ir_exec_freq *execfreq = xmalloc(sizeof(execfreq[0]));
	memset(execfreq, 0, sizeof(execfreq[0]));
	execfreq->set = new_set(cmp_freq, 32);

	memset(&execfreq->hook, 0, sizeof(execfreq->hook));
	execfreq->hook.context = execfreq;
	execfreq->hook.hook._hook_node_info = exec_freq_node_info;
	register_hook(hook_node_info, &execfreq->hook);
	(void) irg;

	return execfreq;
}

void set_execfreq(ir_exec_freq *execfreq, const ir_node *block, double freq)
{
	freq_t *f = set_insert_freq(execfreq->set, block);
	f->freq = freq;
}

static void collect_blocks(ir_node *bl, void *data)
{
	set *freqs = data;
	set_insert_freq(freqs, bl);
}

ir_exec_freq *
compute_execfreq(ir_graph * irg, double loop_weight)
{
	gs_matrix_t  *mat;
	int           size;
	int           idx;
	freq_t       *freq, *s, *e;
	ir_exec_freq *ef;
	set          *freqs;
	dfs_t        *dfs;
	double       *x;
	double        norm;

	/*
	 * compute a DFS.
	 * using a toposort on the CFG (without back edges) will propagate
	 * the values better for the gauss/seidel iteration.
	 * => they can "flow" from start to end.
	 */
	dfs = dfs_new(&absgraph_irg_cfg_succ, irg);
	ef = xmalloc(sizeof(ef[0]));
	memset(ef, 0, sizeof(ef[0]));
	ef->min_non_zero = HUGE_VAL; /* initialize with a reasonable large number. */
	freqs = ef->set = new_set(cmp_freq, dfs_get_n_nodes(dfs));

	/*
	 * Populate the exec freq set.
	 * The DFS cannot be used alone, since the CFG might not be connected
	 * due to unreachable code.
	 */
	irg_block_walk_graph(irg, collect_blocks, NULL, freqs);

	construct_cf_backedges(irg);
	/* TODO: edges are corrupt for EDGE_KIND_BLOCK after the local optimize
		 graph phase merges blocks in the x86 backend */
	edges_deactivate(irg);
	edges_activate(irg);
	/* edges_assure(irg); */

	size = dfs_get_n_nodes(dfs);
	mat  = gs_new_matrix(size, size);
	x    = xmalloc(size*sizeof(*x));

	for (idx = dfs_get_n_nodes(dfs) - 1; idx >= 0; --idx) {
		ir_node *bb = (ir_node *) dfs_get_post_num_node(dfs, size - idx - 1);
		freq_t *freq;
		int i;

		freq = set_insert_freq(freqs, bb);
		freq->idx = idx;

		gs_matrix_set(mat, idx, idx, -1.0);
		for(i = get_Block_n_cfgpreds(bb) - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred_block(bb, i);
			int pred_idx  = size - dfs_get_post_num(dfs, pred) - 1;

			gs_matrix_set(mat, idx, pred_idx, get_cf_probability(bb, i, loop_weight));
		}
	}

	/*
	 * Add a loop from end to start.
	 * The problem is then an eigenvalue problem:
	 * Solve A*x = 1*x => (A-I)x = 0
	 */
	s = set_find_freq(freqs, get_irg_start_block(irg));
	e = set_find_freq(freqs, get_irg_end_block(irg));
	if (e->idx >= 0)
		gs_matrix_set(mat, s->idx, e->idx, 1.0);

	/* solve the system and delete the matrix */
	solve_lgs(mat, x, size);
	gs_delete_matrix(mat);

	/*
	 * compute the normalization factor.
	 * 1.0 / exec freq of start block.
	 */
	norm = x[s->idx] != 0.0 ? 1.0 / x[s->idx] : 1.0;

	ef->max = 0.0;
	set_foreach(freqs, freq) {
		int idx = freq->idx;

		/* take abs because it sometimes can be -0 in case of endless loops */
		freq->freq = fabs(x[idx]) * norm;

		/* get the maximum exec freq */
		ef->max = MAX(ef->max, freq->freq);

		/* Get the minimum non-zero execution frequency. */
		if(freq->freq > 0.0)
			ef->min_non_zero = MIN(ef->min_non_zero, freq->freq);
	}

	/* compute m and b of the transformation used to convert the doubles into scaled ints */
	{
		double smallest_diff = 1.0;

		double l2 = ef->min_non_zero;
		double h2 = ef->max;
		double l1 = 1.0;
		double h1 = MAX_INT_FREQ;

		double *fs = malloc(set_count(freqs) * sizeof(fs[0]));
		int i, j, n = 0;

		set_foreach(freqs, freq)
			fs[n++] = freq->freq;

		/*
		 * find the smallest difference of the execution frequencies
		 * we try to ressolve it with 1 integer.
		 */
		for(i = 0; i < n; ++i) {
			if(fs[i] <= 0.0)
				continue;

			for(j = i + 1; j < n; ++j) {
				double diff = fabs(fs[i] - fs[j]);

				if(!UNDEF(diff))
					smallest_diff = MIN(diff, smallest_diff);
			}
		}

		/* according to that the slope of the translation function is 1.0 / smallest diff */
		ef->m = 1.0 / smallest_diff;

		/* the abscissa is then given by */
		ef->b = l1 - ef->m * l2;

		/*
		 * if the slope is so high that the largest integer would be larger than MAX_INT_FREQ
		 * set the largest int freq to that upper limit and recompute the translation function
		 */
		if(ef->m * h2 + ef->b > MAX_INT_FREQ) {
			ef->m = (h1 - l1) / (h2 - l2);
			ef->b = l1 - ef->m * l2;
		}

		free(fs);
	}

	memset(&ef->hook, 0, sizeof(ef->hook));
	ef->hook.context = ef;
	ef->hook.hook._hook_node_info = exec_freq_node_info;
	register_hook(hook_node_info, &ef->hook);

	xfree(x);

	return ef;
}

void
free_execfreq(ir_exec_freq *ef)
{
	del_set(ef->set);
	unregister_hook(hook_node_info, &ef->hook);
	free(ef);
}
