/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Author: Boris Boesler
**
** traverse an ir graph
** - execute the pre function before recursion
** - execute the post function after recursion
*/

# include "irnode.h"
# include "irgraph.h" /* ir_visited */


void irg_walk_2(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env)
{
  int i;

  assert(node);
  if(node->visit < ir_visited) {
    node->visit = ir_visited;
    if(pre) {
      pre(node, env);
    }

    if (is_no_Block(node))
      irg_walk_2(get_nodes_Block(node), pre, post, env);
    for(i = get_irn_arity(node) - 1; i >= 0; --i) {
      irg_walk_2(get_irn_n(node, i), pre, post, env);
    }

    if(post)
      post(node, env);
  }
  return;
}


void irg_walk(ir_node *node,
	      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
	      void *env)
{
  assert(node);
  ++ir_visited;
  irg_walk_2(node, pre, post, env);
  return;
}

/***************************************************************************/
void irg_block_walk_2(ir_node *node,
		      void (pre)(ir_node*, void*), void (post)(ir_node*, void*),
		      void *env)
{
  int i;

  assert(get_irn_opcode(node) == iro_Block);


  if(get_Block_block_visit(node) < block_visited) {
    set_Block_block_visit(node, block_visited);

    if(pre)
      pre(node, env);

    for(i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {

      /* find the corresponding predecessor block. */
      ir_node *pred = skip_Proj(get_Block_cfgpred(node, i));
      /* GL: I'm not sure whether this assert must go through.  There
         could be Id chains?? */
      assert(is_cfop(pred) || is_fragile_op(pred));
      pred = get_nodes_Block(pred);
      assert(get_irn_opcode(pred) == iro_Block);

      /* recursion */
      irg_block_walk_2(pred, pre, post, env);
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
  ++block_visited;
  if (is_no_Block(node)) node = get_nodes_Block(node);
  assert(get_irn_opcode(node)  == iro_Block);
  irg_block_walk_2(node, pre, post, env);
  return;
}
