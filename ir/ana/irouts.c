 /* Copyright (C) 2002 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors:  Goetz Lindenmaier
**
** irouts.c --- Compute out edges for ir nodes (also called def-use
** edges).
**
*/

/* $Id$ */

#include "irouts.h"
#include "irnode_t.h"
#include "irgraph_t.h"     /* To access irg->outs field (which is private to this module)
			      without public access routine */

/**********************************************************************/
/** Accessing the out datastructures                                 **/
/**********************************************************************/

/* returns the number of successors of the node: */
INLINE int get_irn_n_outs    (ir_node *node) {
  return (int)(node->out[0]);
}

/* Access successor n */
INLINE ir_node *get_irn_out      (ir_node *node, int pos) {
  assert(node);
  assert(pos >= 0 && pos < get_irn_n_outs(node));
  return node->out[pos+1];
}

INLINE void set_irn_out      (ir_node *node, int pos, ir_node *out) {
  assert(node && out);
  assert(pos >= 0 && pos < get_irn_n_outs(node));
  node->out[pos+1] = out;
}


INLINE int get_Block_n_cfg_outs (ir_node *bl) {
  int i, n_cfg_outs = 0;
  assert(bl && (get_irn_op(bl) == op_Block));
  for (i = 0; i < (int)bl->out[0]; i++)
    if ((get_irn_mode(bl->out[i+1]) == mode_X) &&
	(get_irn_op(bl->out[i+1]) != op_End)) n_cfg_outs++;
  return n_cfg_outs;
}


INLINE ir_node *get_Block_cfg_out  (ir_node *bl, int pos) {
  int i, out_pos = 0;
  assert(bl && (get_irn_op(bl) == op_Block));
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
		  void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		  void *env) {
  assert(node);
  if (get_irg_outs_state(current_ir_graph) != no_outs) {
    inc_irg_visited (current_ir_graph);
    irg_out_walk_2(node, pre, post, env);
  }
  return;
}

void irg_out_block_walk2(ir_node *bl,
			void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
			void *env) {
  int i;

  assert(get_irn_opcode(bl) == iro_Block);

  if(get_Block_block_visited(bl) < get_irg_block_visited(current_ir_graph)) {
    set_Block_block_visited(bl, get_irg_block_visited(current_ir_graph));

    if(pre)
      pre(bl, env);

    for(i = 0; i < get_Block_n_cfg_outs(bl); i++) {
      /* find the corresponding predecessor block. */
      ir_node *pred = get_Block_cfg_out(bl, i);
      assert(get_irn_opcode(pred) == iro_Block);
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
			void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
			void *env) {

  assert((get_irn_op(node) == op_Block) || (get_irn_mode(node) == mode_X));

  inc_irg_block_visited(current_ir_graph);

  if (get_irn_mode(node) == mode_X) node = node->out[1];
  assert(get_irn_opcode(node)  == iro_Block);

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
/**********************************************************************/


/* Returns the amount of out edges for not yet visited successors. */
int count_outs(ir_node *n) {
  int start, i, res;
  ir_node *succ;

  set_irn_visited(n, get_irg_visited(current_ir_graph));
  n->out = (ir_node **) 1;     /* Space for array size. */

  if ((get_irn_op(n) == op_Block)) start = 0; else start = -1;
  res = get_irn_arity(n) - start +1;  /* --1 or --0; 1 for array size. */
  for (i = start; i < get_irn_arity(n); i++) {
    /* Optimize Tuples.  They annoy if walking the cfg. */
    succ = skip_Tuple(get_irn_n(n, i));
    set_irn_n(n, i, succ);
    /* count outs for successors */
    if (get_irn_visited(succ) < get_irg_visited(current_ir_graph))
      res += count_outs(succ);
    /* Count my outs */
    succ->out = (ir_node **)( (int)succ->out +1);
  }
  return res;
}

ir_node **set_out_edges(ir_node *n, ir_node **free) {
  int n_outs, start, i;
  ir_node *succ;

  set_irn_visited(n, get_irg_visited(current_ir_graph));

  /* Allocate my array */
  n_outs = (int) n->out;
  n->out = free;
  free = &free[n_outs];
  /* We count the successors again, the space will be sufficient.
     We use this counter to remember the position for the next back
     edge. */
  n->out[0] = (ir_node *)0;

  if (get_irn_op(n) == op_Block) start = 0; else start = -1;
  for (i = start; i < get_irn_arity(n); i++) {
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

INLINE void fix_start_proj(ir_graph *irg) {
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

  current_ir_graph = irg;

  /* Update graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  current_ir_graph->outs_state = outs_consistent;

  /* This first iteration counts the overall number of out edges and the
     number of out edges for each node. */
  inc_irg_visited(irg);
  n_out_edges = count_outs(get_irg_end(irg));

  /* allocate memory for all out edges. */
  irg->outs = (ir_node **) malloc (n_out_edges * sizeof(ir_node *));

  /* The second iteration splits the irg->outs array into smaller arrays
     for each node and writes the back edges into this array. */
  inc_irg_visited(irg);
  set_out_edges(get_irg_end(irg), irg->outs);

  /* We want that the out of ProjX from Start contains the next block at
     position 1, the Start block at position 2.  This is necessary for
     the out block walker. */
  fix_start_proj(irg);

  current_ir_graph = rem;
}

void free_outs(ir_graph *irg) {

  /* Update graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  current_ir_graph->outs_state = no_outs;

  if (irg->outs) free(irg->outs);
  irg->outs = NULL;
}
