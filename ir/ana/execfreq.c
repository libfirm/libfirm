/*
 * Project:     libFIRM
 * File name:   ir/ana/execfreq.c
 * Purpose:     Compute an estimate of basic block executions.
 * Author:      Adam M. Szalkowski
 * Modified by:
 * Created:     28.05.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

//#define USE_GSL

#include <stdio.h>
#include <string.h>

#ifdef USE_GSL
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_vector.h>
#else
#include "gaussjordan.h"
#endif

#include "execfreq.h"

#include "firm_common_t.h"
#include "set.h"
#include "hashptr.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irloop.h"
#include "irgwalk.h"
#include "irouts.h"
#include "irprintf.h"

#include "execfreq.h"

#define set_foreach(s,i) for((i)=set_first((s)); (i); (i)=set_next((s)))

typedef struct _walkerdata_t {
  set    *set;
  size_t  idx;
} walkerdata_t;

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
  freq_t     query;

  query.irn = irn;
  query.freq = 0.0;
  return set_insert(set, &query, sizeof(query), HASH_PTR(irn));
}

double
get_block_execfreq(set * freqs, const ir_node * irn)
{
  freq_t *freq;

  assert(is_Block(irn));

  freq = set_find_freq(freqs, irn);

  assert(freq);

  return freq->freq;
}

#define ZERO(x)   (((x) > 0) ? ((x) < 0.0001) : ((x) > -0.0001))

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
get_cf_probability(ir_node *bb, int pos)
{
#define LOOP_WEIGHT 9.0

  double  sum = 0.0;
  double  cur = 0.0;
  int     i, n;
  ir_node *pred = get_Block_cfgpred_block(bb, pos);

  cur = get_loop_depth(get_irn_loop(bb)) < get_loop_depth(get_irn_loop(pred)) ? 1.0 : LOOP_WEIGHT;

  for(i = 0, n = get_Block_n_cfg_outs(pred); i < n; ++i) {
    ir_node *succ = get_Block_cfg_out(pred, i);

    sum += get_loop_depth(get_irn_loop(succ)) < get_loop_depth(get_irn_loop(pred)) ? 1.0 : LOOP_WEIGHT;
  }

  return cur/sum;
}

set *
compute_execfreq(ir_graph * irg)
{
  set          *freqs = new_set(cmp_freq, 32);
  size_t        size;
  double       *matrix;
  double       *rhs;
  size_t        i = 0;
  freq_t       *freq;
  walkerdata_t  wd;
#ifdef USE_GSL
  gsl_vector   *x;
#else
  double       *x;
#endif

  construct_cf_backedges(irg);

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
      matrix[pred_idx + idx * size] += get_cf_probability(bb, i);
    }
  }

  x = solve_lgs(matrix, rhs, size);
  if(x == NULL) {
    del_set(freqs);
    return NULL;
  }

  set_foreach(freqs, freq) {
    const ir_node *bb = freq->irn;
    size_t        idx = PTR_TO_INT(get_irn_link(bb));

#ifdef USE_GSL
    freq->freq = ZERO(gsl_vector_get(x, idx)) ? 0.0 : gsl_vector_get(x, idx);
#else
    freq->freq = ZERO(x[idx]) ? 0.0 : x[idx];
#endif
//    ir_fprintf(stderr, "execfreq %+F: %f\n", bb, freq->freq);
  }

#ifdef USE_GSL
  gsl_vector_free(x);
#endif
  free(matrix);

  return freqs;
}

void
free_execfreq(set * freqs)
{
  if(freqs) del_set(freqs);
}

#undef ELEM
