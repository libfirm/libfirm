/*
 * Project:     libFIRM
 * File name:   ir/ana/irouts.c
 * Purpose:     Compute and access out edges.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     1.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */



 /* Copyright (C) 2002 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors:  Goetz Lindenmaier
*
* irouts.c --- Compute out edges for ir nodes (also called def-use
* edges).
*
*/

/* $Id$ */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* defined HAVE_CONFIG_H */

#include "irouts.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "irgwalk.h"
#include "string.h"

#ifdef DEBUG_libfirm
/* Note:  ir_node.out_valid and ir_graph.n_outs are only present when DEBUG_libfirm is defined */
/* Accesses to out_valid and n_outs are fenced out to avoid breakage
   when compiling with neither DEBUG_libfirm or NDEBUG defined */
#endif /* defined DEBUG_libfirm */

/**********************************************************************/
/** Accessing the out datastructures                                 **/
/**********************************************************************/

static void reset_outs (ir_node *node, void *unused)
{
  node->out = NULL;
#ifdef DEBUG_libfirm
  node->out_valid = 0;
#endif /* defined DEBUG_libfirm */
}

/* returns the number of successors of the node: */
INLINE int get_irn_n_outs    (ir_node *node) {
#ifdef DEBUG_libfirm
  /* assert (node->out_valid); */
#endif /* defined DEBUG_libfirm */
  return (int)(node->out[0]);
}

/* Access successor n */
INLINE ir_node *get_irn_out      (ir_node *node, int pos) {
  assert(node);
  assert(pos >= 0 && pos < get_irn_n_outs(node));
#ifdef DEBUG_libfirm
  /* assert (node->out_valid); */
#endif /* defined DEBUG_libfirm */
  return node->out[pos+1];
}

INLINE void set_irn_out      (ir_node *node, int pos, ir_node *out) {
  assert(node && out);
  assert(pos >= 0 && pos < get_irn_n_outs(node));
#ifdef DEBUG_libfirm
  node->out_valid = 1;          /* assume that this function is used correctly */
#endif /* defined DEBUG_libfirm */
  node->out[pos+1] = out;
}


INLINE int get_Block_n_cfg_outs (ir_node *bl) {
  int i, n_cfg_outs = 0;
  assert(bl && (get_irn_op(bl) == op_Block));
#ifdef DEBUG_libfirm
  assert (bl->out_valid);
#endif /* defined DEBUG_libfirm */
  for (i = 0; i < (int)bl->out[0]; i++)
    if ((get_irn_mode(bl->out[i+1]) == mode_X) &&
    (get_irn_op(bl->out[i+1]) != op_End)) n_cfg_outs++;
  return n_cfg_outs;
}


INLINE ir_node *get_Block_cfg_out  (ir_node *bl, int pos) {
  int i, out_pos = 0;
  assert(bl && (get_irn_op(bl) == op_Block));
#ifdef DEBUG_libfirm
  assert (bl->out_valid);
#endif /* defined DEBUG_libfirm */
  for (i = 0; i < (int)bl->out[0]; i++)
    if ((get_irn_mode(bl->out[i+1]) == mode_X)  &&
    (get_irn_op(bl->out[i+1]) != op_End)) {
      if (out_pos == pos) {
    ir_node *cfop = bl->out[i+1];
    return cfop->out[0+1];
      } else {
        out_pos++;
      }
    }
  return NULL;
}

void irg_out_walk_2(ir_node *node,  irg_walk_func *pre,
            irg_walk_func *post, void *env) {
  int i;
  ir_node *succ;

  assert(node);
  assert(get_irn_visited(node) < get_irg_visited(current_ir_graph));

  set_irn_visited(node, get_irg_visited(current_ir_graph));

  if (pre) pre(node, env);

  for (i = 0; i < get_irn_n_outs(node); i++) {
    succ = get_irn_out(node, i);
    if (get_irn_visited(succ) < get_irg_visited(current_ir_graph))
      irg_out_walk_2(succ, pre, post, env);
  }

  if (post) post(node, env);

  return;
}

void irg_out_walk(ir_node *node,
            irg_walk_func *pre, irg_walk_func *post,
            void *env) {
  assert(node);
  if (get_irg_outs_state(current_ir_graph) != outs_none) {
    inc_irg_visited (current_ir_graph);
    irg_out_walk_2(node, pre, post, env);
  }
  return;
}

void irg_out_block_walk2(ir_node *bl,
            irg_walk_func *pre, irg_walk_func *post,
            void *env) {
  int i;

  if(get_Block_block_visited(bl) < get_irg_block_visited(current_ir_graph)) {
    set_Block_block_visited(bl, get_irg_block_visited(current_ir_graph));

    if(pre)
      pre(bl, env);

    for(i = 0; i < get_Block_n_cfg_outs(bl); i++) {
      /* find the corresponding predecessor block. */
      ir_node *pred = get_Block_cfg_out(bl, i);
      /* recursion */
      irg_out_block_walk2(pred, pre, post, env);
    }

    if(post)
      post(bl, env);
  }
  return;
}

/* Walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_out_block_walk(ir_node *node,
            irg_walk_func *pre, irg_walk_func *post,
            void *env) {

  assert((get_irn_op(node) == op_Block) || (get_irn_mode(node) == mode_X));

  inc_irg_block_visited(current_ir_graph);

  if (get_irn_mode(node) == mode_X) node = node->out[1];

  irg_out_block_walk2(node, pre, post, env);

  return;

}

/**********************************************************************/
/** Building and Removing the out datasturcture                      **/
/**                                                                  **/
/** The outs of a graph are allocated in a single, large array.      **/
/** This allows to allocate and deallocate the memory for the outs   **/
/** on demand.  The large array is separated into many small ones    **/
/** for each node.  Only a single field to reference the out array   **/
/** is stored in each node and a field referencing the large out     **/
/** array in irgraph.  The 0 field of each out array contains the    **/
/** size of this array.  This saves memory in the irnodes themselves.**/
/** The construction does two passes over the graph.  The first pass **/
/** counts the overall number of outs and the outs of each node.  It **/
/** stores the outs of each node in the out reference of the node.   **/
/** Then the large array is allocated.  The second iteration chops   **/
/** the large array into smaller parts, sets the out edges and       **/
/** recounts the out edges.                                          **/
/** Removes Tuple nodes!                                             **/
/**********************************************************************/


/* Returns the amount of out edges for not yet visited successors. */
static int count_outs(ir_node *n) {
  int start, i, res, irn_arity;
  ir_node *succ;

  set_irn_visited(n, get_irg_visited(current_ir_graph));
  n->out = (ir_node **) 1;     /* Space for array size. */

  start = get_irn_op(n) == op_Block ? 0 : -1;
  irn_arity = get_irn_arity(n);
  res = irn_arity - start +1;  /* --1 or --0; 1 for array size. */
  for (i = start; i < irn_arity; i++) {
    /* Optimize Tuples.  They annoy if walking the cfg. */
    succ = skip_Tuple(get_irn_n(n, i));
    set_irn_n(n, i, succ);
    /* count outs for successors */
    if (get_irn_visited(succ) < get_irg_visited(current_ir_graph)) {
      res += count_outs(succ);
    }
    /* Count my outs */
    succ->out = (ir_node **)( (int)succ->out +1);
  }
  return res;
}

static ir_node **set_out_edges(ir_node *n, ir_node **free) {
  int n_outs, start, i, irn_arity;
  ir_node *succ;

  set_irn_visited(n, get_irg_visited(current_ir_graph));

  /* Allocate my array */
  n_outs = (int) n->out;
  n->out = free;
#ifdef DEBUG_libfirm
  n->out_valid = 1;
#endif /* defined DEBUG_libfirm */
  free = &free[n_outs];
  /* We count the successors again, the space will be sufficient.
     We use this counter to remember the position for the next back
     edge. */
  n->out[0] = (ir_node *)0;

  if (get_irn_op(n) == op_Block) start = 0; else start = -1;
  irn_arity = get_irn_arity(n);
  for (i = start; i < irn_arity; i++) {
    succ = get_irn_n(n, i);
    /* Recursion */
    if (get_irn_visited(succ) < get_irg_visited(current_ir_graph))
      free = set_out_edges(succ, free);
    /* Remember our back edge */
    succ->out[get_irn_n_outs(succ)+1] = n;
    succ->out[0] = (ir_node *) (get_irn_n_outs(succ) + 1);
  }
  return free;
}

static INLINE void fix_start_proj(ir_graph *irg) {
  ir_node *proj = NULL, *startbl;
  int i;
  if (get_Block_n_cfg_outs(get_irg_start_block(irg))) {
    startbl = get_irg_start_block(irg);
    for (i = 0; i < get_irn_n_outs(startbl); i++)
      if (get_irn_mode(get_irn_out(startbl, i)) == mode_X)
        proj = get_irn_out(startbl, i);
    if (get_irn_out(proj, 0) == startbl) {
      assert(get_irn_n_outs(proj) == 2);
      set_irn_out(proj, 0, get_irn_out(proj, 1));
      set_irn_out(proj, 1, startbl);
    }
  }
}

void compute_outs(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  int n_out_edges = 0;
  ir_node **end = NULL;         /* Only for debugging */

  current_ir_graph = irg;

  /* Update graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  if (current_ir_graph->outs_state != outs_none) free_outs(current_ir_graph);
  current_ir_graph->outs_state = outs_consistent;

  /* This first iteration counts the overall number of out edges and the
     number of out edges for each node. */
  inc_irg_visited(irg);
  n_out_edges = count_outs(get_irg_end(irg));

  /* allocate memory for all out edges. */
  irg->outs = (ir_node **) xmalloc (n_out_edges * sizeof(ir_node *));
#ifdef DEBUG_libfirm
  irg->n_outs = n_out_edges;
#endif /* defined DEBUG_libfirm */

  /* The second iteration splits the irg->outs array into smaller arrays
     for each node and writes the back edges into this array. */
  inc_irg_visited(irg);
  end = set_out_edges(get_irg_end(irg), irg->outs);

#ifdef DEBUG_libfirm
  /* Check how much memory we have used */
  assert (end == (irg->outs + n_out_edges));
#endif /* defined DEBUG_libfirm */

  /* We want that the out of ProjX from Start contains the next block at
     position 1, the Start block at position 2.  This is necessary for
     the out block walker. */
  fix_start_proj(irg);

  current_ir_graph = rem;
}




/*------------------------------------------------------------*
 *  This computes the outedges for in interprocedural graph.  *
 *  There is one quirk:                                       *
 *  The number of the outedges for each node is saved in      *
 *  the first member of the ir_node** array. Maybe we should  *
 *  change this to make it more portable...                   *
 *------------------------------------------------------------*/


/* ------------------------------------------
   Inits the number of outedges for each node
   before counting.
   ------------------------------------------ */

static void init_count(ir_node * node, void * env)
{
  node->out = (ir_node **) 1; /* 1 for the array size */
}


/* -----------------------------------------------
   Adjusts the out edge count for its predecessors
   and adds the current arity to the overall count,
   which is saved in "env"
   ------------------------------------------------ */

static void node_arity_count(ir_node * node, void * env)
{
  int *anz = (int *) env, arity, n_outs, i, start;
  ir_node *succ;

  arity = get_irn_arity(node);
  start = (is_Block(node)) ? 0 : -1;

  n_outs = 1 + arity + (-start);  // ((is_Block(node)) ? 0 : 1);   // Why + 1??
  *anz += n_outs;

  for(i = start; i < arity; i++) {
    succ = get_irn_n(node, i);
    succ->out = (ir_node **)((int)succ->out + 1);
  }
}


/* ----------------------------------------
   Inits all nodes for setting the outedges
   Returns the overall count of edges
   ---------------------------------------- */

int count_ip_outs(void) {

  int res = 0;

  cg_walk(init_count, node_arity_count, &res);

  return(res);
}

int dummy_count = 0, global_count; /* Only for debugging */

/* ---------------------------------------------
   For each node: Sets the pointer to array
   in which the outedges are written later.
   The current array start is transported in env
   --------------------------------------------- */

static void set_array_pointer(ir_node *node, void *env) {

  int n_outs;
  ir_node ***free = (ir_node ***) env;

  /* Allocate my array */
  n_outs = (int) node -> out;  /* We wrote the count here in count_ip_outs */
  dummy_count += n_outs;
  assert(dummy_count <= global_count && "More outedges than initially counted!");
  node -> out = *free;
  *free = &((*free)[n_outs]);
  /* We count the successors again, the space will be sufficient.
     We use this counter to remember the position for the next back
     edge. */
  node -> out[0] = (ir_node *) 0;
}


/* -------------------------------------------
   Adds an outedge from the predecessor to the
   current node.
   ------------------------------------------- */

static void set_out_pointer(ir_node * node, void * env) {
  int i, arity = get_irn_arity(node);
  ir_node *succ;
  int start = (!is_Block(node)) ? -1 : 0;

  for(i = start; i < arity; i++) {
    succ = get_irn_n(node, i);
    succ->out[get_irn_n_outs(succ)+1] = node;
    succ->out[0] = (ir_node *) (get_irn_n_outs(succ) + 1);
  }
}


/* -------------------------------
   Sets the outedges for all nodes.
   -------------------------------- */

void set_ip_outs(void)
{
  ir_node **outedge_array = get_irp_ip_outedges();
  cg_walk(set_array_pointer, set_out_pointer, (void *) &outedge_array);
}



/* --------------------------------------------------------
   Counts the outedges, allocates memory to save the
   outedges and fills this outedge array in interprocedural
   view!
   -------------------------------------------------------- */

void compute_ip_outs(void) {

  int n_out_edges;
  ir_node **out_edges;

  assert(get_irp_ip_view_state() == ip_view_valid &&
     "Cannot construct outs for invalid ip view.");

  if (irp->outs_state != outs_none) {
    free_ip_outs();
  }

  global_count = n_out_edges = count_ip_outs();
  out_edges = (ir_node **) malloc (n_out_edges * sizeof(ir_node *));
  set_irp_ip_outedges(out_edges);
  set_ip_outs();
}

void free_ip_outs(void)
{
  ir_node **out_edges = get_irp_ip_outedges();
  if (out_edges != NULL) {
    free(out_edges);
    set_irp_ip_outedges(NULL);
  }
  irp->outs_state = outs_none;
}


void free_outs(ir_graph *irg) {

  /*   current_ir_graph->outs_state = outs_none; */
  irg->outs_state = outs_none;

  if (irg->outs) {
#ifdef DEBUG_libfirm
    memset(irg->outs, 0, irg->n_outs);
#endif /* defined DEBUG_libfirm */
    free(irg->outs);
    irg->outs = NULL;
#ifdef DEBUG_libfirm
    irg->n_outs = 0;
#endif /* defined DEBUG_libfirm */
  }

#ifdef DEBUG_libfirm
  /* when debugging, *always* reset all nodes' outs!  irg->outs might
     have been lying to us */
  irg_walk_graph (irg, reset_outs, NULL, NULL);
#endif /* defined DEBUG_libfirm */
}
