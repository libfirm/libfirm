/*
 * Project:     libFIRM
 * File name:   ir/ana/execution_frequency.c
 * Purpose:     Compute an estimate of basic block executions.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     5.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "execution_frequency.h"

#include "firm_common_t.h"
#include "set.h"
#include "pdeq.h"

#include "irprog.h"
#include "irloop.h"

#include "interval_analysis.h"

/*------------------------------------------------------------------*/
/* A hashmap mapping the frequency to block and loop nodes.  Block
 * and loop nodes are regions.                                      */
/*------------------------------------------------------------------*/

typedef struct {
  void   *reg;
  double  freq;
} reg_exec_freq;

/* We use this set for all nodes in all irgraphs. */
static set *exec_freq_set = NULL;

static int exec_freq_cmp(const void *e1, const void *e2, size_t size) {
  reg_exec_freq *ef1 = (reg_exec_freq *)e1;
  reg_exec_freq *ef2 = (reg_exec_freq *)e2;
  return (ef1->reg != ef2->reg);
}

static INLINE unsigned int exec_freq_hash(void *e) {
  unsigned int v = (unsigned int) ((reg_exec_freq *)e)->reg;
  return v ^ (v>>8);
}

static INLINE void set_region_exec_freq(void *reg, double freq) {
  reg_exec_freq ef;
  ef.reg  = reg;
  ef.freq = freq;
  set_insert(exec_freq_set, &ef, sizeof(ef), exec_freq_hash(&ef));
}

INLINE double get_region_exec_freq(void *reg) {
  reg_exec_freq ef, *found;
  ef.reg  = reg;

  found = set_find(exec_freq_set, &ef, sizeof(ef), exec_freq_hash(&ef));

  /* Not found if information is invalid. */
  if (found)
    return found->freq;
  else
    return 0;
}

/* Returns the number of times the block is executed. */
double     get_Block_exec_freq(ir_node *b) {
  return get_region_exec_freq((void *)b);
}

double get_irn_exec_freq(ir_node *n) {
  if (!is_Block(n)) n = get_nodes_block(n);
  return get_Block_exec_freq(n);
}


/*------------------------------------------------------------------*/
/* The algorithm to compute the execution freqencies.
 *
 * Walk the control flow loop tree which we consider the interval
 * tree.  Compute the execution for the lowest loop, add inner loops
 * to worklist.  Consider the inner loops as simple nodes.  Check that
 * there is only one loop header in each loop.                      */
/*------------------------------------------------------------------*/

static double exception_prob = 0.001;

static INLINE int is_loop_head(ir_node *cond) {
  return false;
}

static INLINE double get_weighted_region_exec_freq(void *reg, int pos) {
  void *pred_reg = get_region_in(reg, pos);
  double res, full_freq = get_region_exec_freq(pred_reg);
  int n_outs     = get_region_n_outs    (pred_reg);
  int n_exc_outs = get_region_n_exc_outs(pred_reg);

  ir_node *cfop;
  if (is_ir_node(reg)) {
    cfop = skip_Proj(get_Block_cfgpred((ir_node *)reg, pos));
  } else {
    assert(is_ir_loop(reg));
    cfop = get_loop_cfop(reg, pos);
  }

  if (is_fragile_op(cfop)) {
    res = full_freq * exception_prob;
  } else {

    /* Equally distribute the weight after exceptions to the left over outs. */
    res = (full_freq *(1 - exception_prob * n_exc_outs)) / (n_outs - n_exc_outs);
  }

  return res;
}

static INLINE void compute_region_freqency(void *reg, double head_weight) {
  int i, n_ins = get_region_n_ins(reg);
  double my_freq = 0;

  //printf("head weight %lf: ", head_weight); DDMR(reg);

  for (i = 0; i < n_ins; ++i) {
    void *pred_reg = get_region_in(reg, i);
    if (pred_reg) {
      my_freq += get_weighted_region_exec_freq(reg, i);
    }
  }

  if (my_freq == 0.0) {
    /* All preds are from outer loop. We are a head or so. */
    my_freq = head_weight;
  }
  set_region_exec_freq(reg, my_freq);
}

static void check_proper_head(ir_loop *l, void *reg) {
  int i, n_ins = get_region_n_ins(reg);
  for (i = 0; i < n_ins; ++i) {
    assert(!get_region_in(reg, i));
  }
}

/* Compute the ex freq for current_ir_graph */
static void compute_frequency(int default_loop_weight) {
  ir_loop *outermost_l = get_irg_loop(current_ir_graph);
  pdeq *block_worklist = new_pdeq1(outermost_l);

  /* Outermost start is considered a loop head.  We will soon multiply
     by default_loop_weight. */
  set_region_exec_freq(outermost_l, 1.0/default_loop_weight);

  while (!pdeq_empty(block_worklist)) {
    ir_loop *l = (ir_loop *)pdeq_getl(block_worklist);
    int i, n_elems = get_loop_n_elements(l);

    /* The header is initialized with the freqency of the full loop times the iteration weight. */
    check_proper_head(l, get_loop_element(l, 0).son);

    for (i = 0; i < n_elems; ++i) {
      loop_element e = get_loop_element(l, i);
      if (is_ir_loop(e.son)) pdeq_putr(block_worklist, e.son);
      compute_region_freqency(e.son, default_loop_weight * get_region_exec_freq(l));
    }
  }
  del_pdeq(block_worklist);
}

/* Compute the execution frequency for all blocks in the given
 * graph.
 *
 * irg:                 The graph to be analyzed.
 * default_loop_weight: The number of executions of a loop.
 */
void compute_execution_frequency(ir_graph *irg, int default_loop_weight, double exception_probability) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;
  exception_prob = exception_probability;
  if (!exec_freq_set) exec_freq_set = new_set(exec_freq_cmp, 256);

  construct_intervals(current_ir_graph);
  compute_frequency(default_loop_weight);

  /*
    dump_loop_tree     (current_ir_graph, "-execfreq");
    dump_ir_block_graph(current_ir_graph, "-execfreq");
    dump_interval_graph(current_ir_graph, "-execfreq");
  */

  current_ir_graph = rem;
}


void compute_execution_frequencies(int default_loop_weight, double exception_probability) {
  int i, n_irgs = get_irp_n_irgs();
  free_intervals();
  for (i = 0; i < n_irgs; ++i) {
    compute_execution_frequency(get_irp_irg(i), default_loop_weight, exception_probability);
  }
}

/** free occupied memory, reset */
void free_execution_frequency(void) {
  free_intervals();
  del_set(exec_freq_set);
}
