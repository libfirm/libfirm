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

#undef USE_GSL

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#ifdef USE_GSL
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_vector.h>
#else
#include "gaussjordan.h"
#endif

#include "firm_common_t.h"
#include "set.h"
#include "hashptr.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"
#include "irgwalk.h"
#include "iredges.h"
#include "irprintf.h"
#include "irhooks.h"

#include "execfreq.h"

#define set_foreach(s,i) for((i)=set_first((s)); (i); (i)=set_next((s)))

#define MAX_INT_FREQ 1000000

typedef struct _freq_t {
	const ir_node    *irn;
	double            freq;
} freq_t;


typedef struct _walkerdata_t {
  set    *set;
  size_t  idx;
} walkerdata_t;

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

	// printf("%20.6f %10d\n", f, res);
	return res;
}

#define EPSILON		0.0001
#define UNDEF(x)    !(x > EPSILON)

static void
block_walker(ir_node * bb, void * data)
{
  walkerdata_t  *wd = data;

  set_insert_freq(wd->set, bb);
  set_irn_link(bb, (void*)wd->idx++);
}

#ifdef USE_GSL
static gsl_vector *
solve_lgs(double * a_data, double * b_data, size_t size)
{
  gsl_matrix_view m
    = gsl_matrix_view_array (a_data, size, size);

  gsl_vector_view b
    = gsl_vector_view_array (b_data, size);

  gsl_vector *x = gsl_vector_alloc (size);

  int s;

  gsl_permutation * p = gsl_permutation_alloc (size);

  gsl_linalg_LU_decomp (&m.matrix, p, &s);

  gsl_linalg_LU_solve (&m.matrix, p, &b.vector, x);

  gsl_permutation_free (p);

  return x;
}
#else
static double *
solve_lgs(double * A, double * b, size_t size)
{
  if(firm_gaussjordansolve(A,b,size) == 0) {
    return b;
  } else {
    return NULL;
  }
}
#endif

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

	return execfreq;
}

void set_execfreq(ir_exec_freq *execfreq, const ir_node *block, double freq)
{
	freq_t *f = set_insert_freq(execfreq->set, block);
	f->freq = freq;
}

ir_exec_freq *
compute_execfreq(ir_graph * irg, double loop_weight)
{
	size_t        size;
	double       *matrix;
	double       *rhs;
	int           i;
	freq_t       *freq;
	walkerdata_t  wd;
	ir_exec_freq  *ef;
	set          *freqs;
#ifdef USE_GSL
	gsl_vector   *x;
#else
	double       *x;
#endif

	ef = xmalloc(sizeof(ef[0]));
	memset(ef, 0, sizeof(ef[0]));
	ef->min_non_zero = 1e50; /* initialize with a reasonable large number. */
	freqs = ef->set = new_set(cmp_freq, 32);

	construct_cf_backedges(irg);
	edges_assure(irg);

	wd.idx = 0;
	wd.set = freqs;

	irg_block_walk_graph(irg, block_walker, NULL, &wd);

	size = set_count(freqs);
	matrix = xmalloc(size*size*sizeof(*matrix));
	memset(matrix, 0, size*size*sizeof(*matrix));
	rhs = xmalloc(size*sizeof(*rhs));
	memset(rhs, 0, size*sizeof(*rhs));

	set_foreach(freqs, freq) {
		ir_node *bb = (ir_node *)freq->irn;
		size_t  idx = (int)get_irn_link(bb);

		matrix[idx * (size + 1)] = -1.0;

		if (bb == get_irg_start_block(irg)) {
			rhs[(int)get_irn_link(bb)] = -1.0;
			continue;
		}

		for(i = get_Block_n_cfgpreds(bb) - 1; i >= 0; --i) {
			ir_node *pred    = get_Block_cfgpred_block(bb, i);
			size_t  pred_idx = (int)get_irn_link(pred);

			//      matrix[pred_idx + idx*size] += 1.0/(double)get_Block_n_cfg_outs(pred);
			matrix[pred_idx + idx * size] += get_cf_probability(bb, i, loop_weight);
		}
	}

	x = solve_lgs(matrix, rhs, size);
	if(x == NULL) {
		ef->infeasible = 1;
		return ef;
	}

	ef->max = 0.0;

	set_foreach(freqs, freq) {
		const ir_node *bb = freq->irn;
		size_t        idx = PTR_TO_INT(get_irn_link(bb));

#ifdef USE_GSL
		freq->freq = UNDEF(gsl_vector_get(x, idx)) ? EPSILON : gsl_vector_get(x, idx);
#else
		freq->freq = UNDEF(x[idx]) ? EPSILON : x[idx];
#endif

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

		// printf("smallest_diff: %g, l1: %f, h1: %f, l2: %f, h2: %f, m: %f, b: %f\n", smallest_diff, l1, h1, l2, h2, ef->m, ef->b);
		free(fs);
	}

#ifdef USE_GSL
	gsl_vector_free(x);
#endif
	free(matrix);

	memset(&ef->hook, 0, sizeof(ef->hook));
	ef->hook.context = ef;
	ef->hook.hook._hook_node_info = exec_freq_node_info;
	register_hook(hook_node_info, &ef->hook);

	return ef;
}

void
free_execfreq(ir_exec_freq *ef)
{
	del_set(ef->set);
	unregister_hook(hook_node_info, &ef->hook);
	free(ef);
}
