/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Christian Schaefer
**
** Optimizations for a whole ir graph, i.e., a procedure.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <assert.h>

# include "irgopt.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "iropt.h"
# include "irgwalk.h"
# include "ircons.h"
# include "misc.h"
# include "irgmod.h"

# include "pset.h"
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identity   (pset *value_table, ir_node *node);


/* To fill the hash table */
void
add_identity (pset *value_table, ir_node *n) {
  /* identify_remember (value_table, n);*/
}

/********************************************************************/
/* apply optimizations of iropt to all nodes.                       */
/********************************************************************/

void
optimize_in_place_wrapper (ir_node *n, void *env) {
  int i;
  ir_node *optimized;

  /* optimize all sons after recursion, i.e., the sons' sons are
     optimized already. */
  for (i = -1; i < get_irn_arity(n); i++) {
    optimized = optimize_in_place(get_irn_n(n, i));
    set_irn_n(n, i, optimized);
  }
}

void
local_optimize_graph (ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  /* Should we clean the value_table in irg for the cse? Better do so... */
  del_identities(irg->value_table);
  irg->value_table = new_identities();

  /* walk over the graph */
  irg_walk(irg->end, NULL, optimize_in_place_wrapper, NULL);

  current_ir_graph = rem;
}

/********************************************************************/
/* Routines for dead node elimination / copying garbage collection  */
/* of the obstack.                                                  */
/********************************************************************/

/* Remeber the new node in the old node by using a field all nodes have. */
inline void
set_new_node (ir_node *old, ir_node *new)
{
  old->link = new;
}

/* Get this new node, before the old node is forgotton.*/
inline ir_node *
get_new_node (ir_node * n)
{
  return n->link;
}


/* We use the block_visited flag to mark that we have computed the
   number of useful predecessors for this block.
   Further we encode the new arity in this flag in the old blocks.
   Remembering the arity is useful, as it saves a lot of pointer
   accesses.  This function is called for all Phi and Block nodes
   in a Block. */
inline int
compute_new_arity(ir_node *b) {
  int i, res;
  int irg_v, block_v;

  irg_v = get_irg_block_visited(current_ir_graph);
  block_v = get_Block_block_visited(b);
  if (block_v >= irg_v) {
    /* we computed the number of preds for this block and saved it in the
       block_v flag */
    return block_v - irg_v;
  } else {
    /* compute the number of good predecessors */
    res = get_irn_arity(b);
    for (i = 0; i < get_irn_arity(b); i++)
      if (get_irn_opcode(get_irn_n(b, i)) == iro_Bad) res--;
    /* save it in the flag. */
    set_Block_block_visited(b, irg_v + res);
    return res;
  }
}

/* Copies the node to the new obstack. The Ins of the new node point to
   the predecessors on the old obstack.  n->link points to the new node.
   For Phi and Block nodes the function allocates in-arrays with an arity
   only for useful predecessors.  The arity is determined by counting
   the non-bad predecessors of the block. */
inline void
copy_node (ir_node *n, void *env) {
  ir_node *nn, *block;
  int new_arity;

  if (get_irn_opcode(n) == iro_Block) {
    block = NULL;
    new_arity = compute_new_arity(n);
  } else {
    block = get_nodes_Block(n);
    if (get_irn_opcode(n) == iro_Phi) {
      new_arity = compute_new_arity(block);
    } else {
      new_arity = get_irn_arity(n);
    }
  }
  nn = new_ir_node(current_ir_graph,
		   block,
		   get_irn_op(n),
		   get_irn_mode(n),
		   new_arity,
		   get_irn_in(n));
  /* Copy the attributes.  These might point to additional data.  If this
     was allocated on the old obstack the pointers now are dangling.  This
     frees e.g. the memory of the graph_arr allocated in new_immBlock. */
  copy_attrs(n, nn);
  set_new_node(n, nn);
}

/* Copies new predecessors of old node to new node remembered in link.
   Spare the Bad predecessors of Phi and Block nodes. */
inline void
copy_preds (ir_node *n, void *env) {
  ir_node *nn, *block, *on;
  int i, j;

  nn = get_new_node(n);

  if (get_irn_opcode(n) == iro_Block) {
    /* Don't copy Bad nodes. */
    j = 0;
    for (i = 0; i < get_irn_arity(n); i++)
      if (get_irn_opcode(get_irn_n(n, i)) != iro_Bad) {
	set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
	j++;
      }
    /* repair the block visited flag from above misuse */
    set_Block_block_visited(nn, 0);
    /* Local optimization could not merge two subsequent blocks if
       in array contained Bads.  Now it's possible, but don't do it for
       the end block!  */
    /* GL: this is inefficient!!
    if (n != current_ir_graph->end_block)  on = optimize_in_place(nn);
    else on = nn;
    if (nn != on) exchange(nn, on);
    better: */
    if (n != current_ir_graph->end_block) {
      on = optimize_in_place(nn);
      if (nn != on) exchange(nn, on);
      nn = on;  /* For cse ... */
    }
  } else if (get_irn_opcode(n) == iro_Phi) {
    /* Don't copy node if corresponding predecessor in block is Bad.
       The Block itself should not be Bad. */
    block = get_nodes_Block(n);
    set_irn_n (nn, -1, get_new_node(block));
    j = 0;
    for (i = 0; i < get_irn_arity(n); i++)
      if (get_irn_opcode(get_irn_n(block, i)) != iro_Bad) {
	set_irn_n (nn, j, get_new_node(get_irn_n(n, i)));
	j++;
      }
    /* Compacting the Phi's ins might generate Phis with only one
       predecessor. */
    if (get_irn_arity(n) == 1)
      exchange(n, get_irn_n(n, 0));
  } else {
    for (i = -1; i < get_irn_arity(n); i++)
      set_irn_n (nn, i, get_new_node(get_irn_n(n, i)));
  }
  /* Now the new node is complete.  We can add it to the hash table for cse. */
  add_identity (current_ir_graph->value_table, nn);
}

/* Copies the graph reachable from current_ir_graph->end to the obstack
   in current_ir_graph.
   Then fixes the fields in current_ir_graph containing nodes of the
   graph.  */
void
copy_graph () {

  ir_node *old, *new;

  /* Not all nodes remembered in current_ir_graph might be reachable
     from the end node.  Assure their link is set to NULL, so that
     we can test whether new nodes have been computed. */
  set_irn_link(get_irg_frame  (current_ir_graph), NULL);
  set_irn_link(get_irg_globals(current_ir_graph), NULL);
  set_irn_link(get_irg_args   (current_ir_graph), NULL);

  /* we use the block walk flag for removing Bads from Blocks ins. */
  inc_irg_block_visited(current_ir_graph);

  /* copy the graph */
  irg_walk(get_irg_end(current_ir_graph), copy_node, copy_preds, NULL);

  /* fix the fields in current_ir_graph */
  set_irg_end        (current_ir_graph, get_new_node(get_irg_end(current_ir_graph)));
  set_irg_end_block  (current_ir_graph, get_new_node(get_irg_end_block(current_ir_graph)));
  if (get_irn_link(get_irg_frame(current_ir_graph)) == NULL) {
    copy_node (get_irg_frame(current_ir_graph), NULL);
    copy_preds(get_irg_frame(current_ir_graph), NULL);
  }
  if (get_irn_link(get_irg_globals(current_ir_graph)) == NULL) {
    copy_node (get_irg_globals(current_ir_graph), NULL);
    copy_preds(get_irg_globals(current_ir_graph), NULL);
  }
  if (get_irn_link(get_irg_args(current_ir_graph)) == NULL) {
    copy_node (get_irg_args(current_ir_graph), NULL);
    copy_preds(get_irg_args(current_ir_graph), NULL);
  }
  set_irg_start  (current_ir_graph, get_new_node(get_irg_start(current_ir_graph)));

  set_irg_start_block(current_ir_graph,
		      get_new_node(get_irg_start_block(current_ir_graph)));
  set_irg_frame  (current_ir_graph, get_new_node(get_irg_frame(current_ir_graph)));
  set_irg_globals(current_ir_graph, get_new_node(get_irg_globals(current_ir_graph)));
  set_irg_args   (current_ir_graph, get_new_node(get_irg_args(current_ir_graph)));
  if (get_irn_link(get_irg_bad(current_ir_graph)) == NULL) {
    copy_node(get_irg_bad(current_ir_graph), NULL);
    copy_preds(get_irg_bad(current_ir_graph), NULL);
  }
  set_irg_bad(current_ir_graph, get_new_node(get_irg_bad(current_ir_graph)));
}

/* Copies all reachable nodes to a new obstack.  Removes bad inputs
   from block nodes and the corresponding inputs from Phi nodes.
   Merges single exit blocks with single entry blocks and removes
   1-input Phis.
   Adds all new nodes to a new hash table for cse.  Does not
   perform cse, so the hash table might contain common subexpressions. */
/* Amroq call this emigrate() */
void
dead_node_elimination(ir_graph *irg) {
  ir_graph *rem;
  struct obstack *graveyard_obst = NULL;
  struct obstack *rebirth_obst   = NULL;

  /* Remember external state of current_ir_graph. */
  rem = current_ir_graph;
  current_ir_graph = irg;

  if (get_optimize() && get_opt_dead_node_elimination()) {

    /* A quiet place, where the old obstack can rest in peace,
       until it will be cremated. */
    graveyard_obst = irg->obst;

    /* A new obstack, where the reachable nodes will be copied to. */
    rebirth_obst = (struct obstack *) xmalloc (sizeof (struct obstack));
    current_ir_graph->obst = rebirth_obst;
    obstack_init (current_ir_graph->obst);

    /* We also need a new hash table for cse */
    del_identities (irg->value_table);
    irg->value_table = new_identities ();

    /* Copy the graph from the old to the new obstack */
    copy_graph();

    /* Free memory from old unoptimized obstack */
    obstack_free(graveyard_obst, 0);  /* First empty the obstack ... */
    xfree (graveyard_obst);           /* ... then free it.           */
  }

  current_ir_graph = rem;
}
