/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Boris Boesler
**
** traverse an ir graph
** - execute the pre function before recursion
** - execute the post function after recursion
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irnode.h"
# include "irgraph.h" /* visited flag */


void irg_walk_2(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env)
{
  int i;


  assert(node);
  /*      printf(" - "); DDMSG2(node);  */

  if (get_irn_visited(node) < get_irg_visited(current_ir_graph)) {

/*      printf(" -> "); DDMSG2(node);  */
    set_irn_visited(node, get_irg_visited(current_ir_graph));

    if (pre) {
      pre(node, env);
    }

    if (is_no_Block(node)) {
      irg_walk_2(get_nodes_Block(node), pre, post, env);
    }
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
/*        printf("   "); DDMSG2(node); */
/*        printf("   "); DDMSG2(get_irn_n(node, i)); */

      irg_walk_2(get_irn_n(node, i), pre, post, env);
    }

/*      printf(" <- "); DDMSG2(node);  */
    if (post)
      post(node, env);
  }
  return;
}


void irg_walk(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env)
{
  assert(node);
  inc_irg_visited (current_ir_graph);
  irg_walk_2(node, pre, post, env);
  return;
}

/***************************************************************************/

/* Walks back from n until it finds a real cf op. */
ir_node *get_cf_op(ir_node *n) {
  ir_node *pred;

  n = skip_nop(n);
  n = skip_Tuple(n);
  pred = skip_Proj(n);
  if (!(is_cfop(pred) || is_fragile_op(pred) ||
	(get_irn_op(pred) == op_Bad)))
    n = get_cf_op(n);

  return skip_Proj(n);
}

void irg_block_walk_2(ir_node *node, void (pre)(ir_node*, void*),
		      void (post)(ir_node*, void*), void *env)
{
  int i;

  assert(get_irn_opcode(node) == iro_Block);

  if(get_Block_block_visited(node) < get_irg_block_visited(current_ir_graph)) {
    set_Block_block_visited(node, get_irg_block_visited(current_ir_graph));

    if(pre)
      pre(node, env);

    for(i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {

      /* find the corresponding predecessor block. */
      ir_node *pred = get_cf_op(get_Block_cfgpred(node, i));
      /* GL: I'm not sure whether this assert must go through.  There
         could be Id chains?? Tuple/Proj .... */

      assert(is_cfop(pred) || is_fragile_op(pred) ||
	     (get_irn_op(pred) == op_Bad));
      pred = get_nodes_Block(pred);
      if(get_irn_opcode(pred) == iro_Block) {
	/* recursion */
	irg_block_walk_2(pred, pre, post, env);
      }
      else {
	assert(get_irn_opcode(pred) == iro_Bad);
      }
    }

    if(post)
      post(node, env);
  }
  return;
}


/* walks only over Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_block_walk(ir_node *node,
		    void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		    void *env)
{
  assert(node);
  inc_irg_block_visited(current_ir_graph);
  if (is_no_Block(node)) node = get_nodes_Block(node);
  assert(get_irn_opcode(node)  == iro_Block);
  irg_block_walk_2(node, pre, post, env);
  return;
}
