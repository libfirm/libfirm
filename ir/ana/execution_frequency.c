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
#include "irnode_t.h"
#include "irloop.h"
#include "irgwalk.h"

#include "interval_analysis.h"

/*------------------------------------------------------------------*/
/* A hashmap mapping the frequency to block and loop nodes.  Block
 * and loop nodes are regions.                                      */
/*------------------------------------------------------------------*/

typedef struct {
  void   *reg;
  double  freq;
  int     prob;
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
  assert(exec_freq_set);

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
/* A algorithm that precomputes whether Conds lead to an exception.
 * Computes a field for all Projs from Conds that says the following:
 *   - The Proj projs from a normal dual Cond with probability 50:50
 *   - This Proj of the Cond leads to an exception, i.e., a raise node.
 *     It is taken with exception probability.
 *   - The Proj of the Cond avoids an exception.  It is taken with
 *     1 - exception probability.                                   */
/*------------------------------------------------------------------*/

#include "irouts.h"

typedef enum {
  Cond_prob_none,
  Cond_prob_normal,
  Cond_prob_avoid_exception,
  Cond_prob_exception_taken,
  Cond_prob_was_exception_taken,
} Cond_prob;

static int just_passed_a_Raise = 0;
static ir_node *Cond_list = NULL;

/* We do not use an extra set, as Projs are not yet in the existing one. */
void set_ProjX_probability(ir_node *n, Cond_prob prob) {
  reg_exec_freq ef;
  ef.reg  = n;
  ef.prob = prob;
  set_insert(exec_freq_set, &ef, sizeof(ef), exec_freq_hash(&ef));
}

Cond_prob get_ProjX_probability(ir_node *n) {
  reg_exec_freq ef, *found;
  ef.reg  = n;

  found = set_find(exec_freq_set, &ef, sizeof(ef), exec_freq_hash(&ef));

  if (found)
    return (Cond_prob)found->prob;
  else
    return Cond_prob_none;
}

/* A walker that only visits the nodes we want to see. */

static void
my_irg_walk_2_both(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void * env) {
  int i;
  set_irn_visited(node, current_ir_graph->visited);

  pre(node, env);

  if (node->op != op_Block) {
    ir_node *pred;
    if (node->op == op_Proj)
      pred = get_irn_n(node, 0);
    else
      pred = get_irn_n(node, -1);
    if (pred->visited < current_ir_graph->visited)
      my_irg_walk_2_both(pred, pre, post, env);
  }

  else {  /* a Block */
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node *pred = get_irn_n(node, i);
      if (pred->visited < current_ir_graph->visited)
        my_irg_walk_2_both(pred, pre, post, env);
    }
  }

  if (node->op == op_End) {
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node *pred = get_irn_n(node, i);
      if ((pred->op == op_Block) && (pred->visited < current_ir_graph->visited))
        my_irg_walk_2_both(pred, pre, post, env);
    }
  }

  post(node, env);
}
static void my_irg_walk_current_graph(irg_walk_func *pre, irg_walk_func *post, void *env) {
  inc_irg_visited(current_ir_graph);
  my_irg_walk_2_both(get_irg_end(current_ir_graph), pre, post, env);
}


static void walk_pre(ir_node *n, void *env) {

  if (get_irn_op(n) == op_Raise)
    just_passed_a_Raise = 1;

  if (   (get_irn_op(n) == op_Proj)
      && (get_irn_op(get_Proj_pred(n)) == op_Cond)
      && (just_passed_a_Raise)) {
    ir_node *other_proj;
    ir_node *c = get_Proj_pred(n);

    /* If we already visited the other Proj, and it also leads to a Raise,
       we are in the middle of something. Continue searching. */
    assert(get_irn_n_outs(c) == 2 && "encountered a switch cond");
    other_proj = get_irn_out(c, 0);
    if (other_proj == n) other_proj = get_irn_out(c, 1);
    if (get_ProjX_probability(other_proj) == Cond_prob_exception_taken) {
      set_ProjX_probability(other_proj, Cond_prob_was_exception_taken);
      /* Keep searching for the Proj, so keep just_passed_a_Raise. */
    } else {
      set_ProjX_probability(n, Cond_prob_exception_taken);
      just_passed_a_Raise = 0;
    }
  }

  if (get_irn_op(n) == op_Cond) {
    set_irn_link(n, Cond_list);
    Cond_list = n;
  }
}

static void walk_post(ir_node *n, void *env) {

  if (get_irn_op(n) == op_Raise)
    just_passed_a_Raise = 0;

  if (   (get_irn_op(n) == op_Proj)
      && (get_irn_op(get_Proj_pred(n)) == op_Cond)
      && ((get_ProjX_probability(n) == Cond_prob_exception_taken)    ||
	  (get_ProjX_probability(n) == Cond_prob_was_exception_taken)   )) {
    just_passed_a_Raise = 1;
  }
}

/** Precompute which Conds test for an exception.
 *
 *  Operates on current_ir_graph. */
void precompute_cond_evaluation(void) {
  ir_node *c;

  compute_outs(current_ir_graph);

  just_passed_a_Raise = 0;
  Cond_list = NULL;
  my_irg_walk_current_graph(walk_pre, walk_post, NULL);

  for (c = Cond_list; c; c = get_irn_link(c)) {
    ir_node *p0, *p1;

    assert(get_irn_n_outs(c) == 2 && "encountered a switch cond");
    p0 = get_irn_out(c, 0);
    p1 = get_irn_out(c, 1);

    /* both are exceptions */
    if ((get_ProjX_probability(p0) == Cond_prob_exception_taken) &&
        (get_ProjX_probability(p1) == Cond_prob_exception_taken)   ) {
      assert(0 && "I tried to avoid these!");
      /* It's a */
      set_ProjX_probability(p0, Cond_prob_normal);
      set_ProjX_probability(p1, Cond_prob_normal);
    }

    /* p0 is exception */
    else if (get_ProjX_probability(p0) == Cond_prob_exception_taken) {
      set_ProjX_probability(p1, Cond_prob_avoid_exception);
    }

    /* p1 is exception */
    else if (get_ProjX_probability(p1) == Cond_prob_exception_taken) {
      set_ProjX_probability(p0, Cond_prob_avoid_exception);
    }

    /* none is exception */
    else {
      set_ProjX_probability(p0, Cond_prob_normal);
      set_ProjX_probability(p1, Cond_prob_normal);
    }
  }
}

int is_fragile_Proj(ir_node *n) {
  return is_Proj(n) && (get_ProjX_probability(n) == Cond_prob_exception_taken);
}

/*------------------------------------------------------------------*/
/* The algorithm to compute the execution frequencies.
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

/** Weight a single region in edge.
 *
 *  Given all outs of the predecessor region, we can compute the weight of
 *  this single edge. */
static INLINE double get_weighted_region_exec_freq(void *reg, int pos) {
  void *pred_reg        = get_region_in(reg, pos);
  double res, full_freq = get_region_exec_freq (pred_reg);
  int n_outs            = get_region_n_outs    (pred_reg);
  int n_exc_outs        = get_region_n_exc_outs(pred_reg);

  ir_node *cfop;
  if (is_ir_node(reg)) {
    cfop = get_Block_cfgpred((ir_node *)reg, pos);
    if (is_Proj(cfop) && (get_irn_op(get_Proj_pred(cfop)) != op_Cond))
      cfop = skip_Proj(cfop);
  } else {
    assert(is_ir_loop(reg));
    cfop = get_loop_cfop(reg, pos);
  }

  if (is_fragile_op(cfop) || is_fragile_Proj(cfop)) {
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

    /* The header is initialized with the frequency of the full loop times the iteration weight. */
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

  precompute_cond_evaluation();
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
