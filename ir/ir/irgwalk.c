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
# include "irprog.h"
# include "irgwalk.h"

# include "eset.h"


/* walk over an interprocedural graph (callgraph). Visits only graphs in irg_set. */
static void irg_walk_cg(ir_node * node, int visited, eset * irg_set, irg_walk_func pre, irg_walk_func post, void * env) {
  int i;
  ir_graph * rem = current_ir_graph;
  ir_node * pred;

  assert(node);
  if (get_irn_visited(node) >= visited) return;

  set_irn_visited(node, visited);

  pred = skip_Proj(node);
  if (get_irn_op(pred) == op_CallBegin
      || get_irn_op(pred) == op_EndReg
      || get_irn_op(pred) == op_EndExcept) {
    current_ir_graph = get_irn_irg(pred);
  }

  if (pre) pre(node, env);

  if (is_no_Block(node)) irg_walk_cg(get_nodes_Block(node), visited, irg_set, pre, post, env);

  if (get_irn_op(node) == op_Block) { /* block */
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node * exec = get_irn_n(node, i);
      ir_node * pred = skip_Proj(exec);
      if ((get_irn_op(pred) != op_CallBegin
	   && get_irn_op(pred) != op_EndReg
	   && get_irn_op(pred) != op_EndExcept)
	  || eset_contains(irg_set, get_irn_irg(pred))) {
	irg_walk_cg(exec, visited, irg_set, pre, post, env);
      }
    }
  } else if (get_irn_op(node) == op_Filter) {
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      ir_node * pred = get_irn_n(node, i);
      if (get_irn_op(pred) == op_Unknown || get_irn_op(pred) == op_Bad) {
	irg_walk_cg(pred, visited, irg_set, pre, post, env);
      } else {
	ir_node * exec;
	exec = skip_Proj(get_Block_cfgpred(get_nodes_Block(node), i));
	assert(get_irn_op(exec) == op_CallBegin
	       || get_irn_op(exec) == op_EndReg
	       || get_irn_op(exec) == op_EndExcept);
	if (eset_contains(irg_set, get_irn_irg(exec))) {
	  current_ir_graph = get_irn_irg(exec);
	  irg_walk_cg(pred, visited, irg_set, pre, post, env);
	  current_ir_graph = rem;
	}
      }
    }
  } else {
    for (i = get_irn_arity(node) - 1; i >= 0; --i) {
      irg_walk_cg(get_irn_n(node, i), visited, irg_set, pre, post, env);
    }
  }

  if (post) post(node, env);

  current_ir_graph = rem;
}


/* Insert all ir_graphs in irg_set, that are (transitive) reachable. */
static void collect_irgs(ir_node * node, eset * irg_set) {
  if (get_irn_op(node) == op_Call) {
    int i;
    for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
      entity * ent = get_Call_callee(node, i);
      ir_graph * irg = ent ? get_entity_irg(ent) : NULL;
      if (irg && !eset_contains(irg_set, irg)) {
	eset_insert(irg_set, irg);
	irg_walk_graph(irg, (irg_walk_func) collect_irgs, NULL, irg_set);
      }
    }
  }
}


void irg_walk_2(ir_node *node, irg_walk_func pre, irg_walk_func post, void * env)
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
      /* printf("   "); DDMSG2(node);
	 printf("   "); DDMSG2(get_irn_n(node, i));  */

      irg_walk_2(get_irn_n(node, i), pre, post, env);
    }

/*      printf(" <- "); DDMSG2(node);  */
    if (post)
      post(node, env);
  }
  return;
}


void irg_walk(ir_node *node, irg_walk_func pre, irg_walk_func post, void *env)
{
  assert(node);
  if (interprocedural_view) {
    eset * irg_set = eset_create();
    int visited;
    ir_graph * irg;
    interprocedural_view = false;
    eset_insert(irg_set, current_ir_graph);
    irg_walk(node, (irg_walk_func) collect_irgs, NULL, irg_set);
    interprocedural_view = true;
    visited = get_max_irg_visited() + 1;
    irg_walk_cg(node, visited, irg_set, pre, post, env);
    for (irg = eset_first(irg_set); irg; irg = eset_next(irg_set)) {
      set_irg_visited(irg, visited);
    }
    eset_destroy(irg_set);
  } else {
    inc_irg_visited(current_ir_graph);
    irg_walk_2(node, pre, post, env);
  }
  return;
}


void irg_walk_graph(ir_graph *irg, irg_walk_func pre, irg_walk_func post, void *env) {
  ir_graph * rem = current_ir_graph;
  current_ir_graph = irg;
  irg_walk(get_irg_end(irg), pre, post, env);
  current_ir_graph = rem;
}


/***************************************************************************/

/* Executes irg_walk(end, pre, post, env) for all irgraphs in irprog.
   Sets current_ir_graph properly for each walk.  Conserves current
   current_ir_graph. */
void all_irg_walk(irg_walk_func pre, irg_walk_func post, void *env) {
  int i;
  ir_graph *irg, *rem;

  rem = current_ir_graph;

  for (i = 0; i < get_irp_n_irgs(); i++) {
    irg = get_irp_irg(i);
    current_ir_graph = irg;
    irg_walk(get_irg_end(irg), pre, post, env);
  }
  current_ir_graph = rem;
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

void irg_block_walk_2(ir_node *node, irg_walk_func pre, irg_walk_func post, void *env)
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

      assert(is_cfop(pred)
			 || is_fragile_op(pred)
			 || (get_irn_op(pred) == op_Bad));

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
void irg_block_walk(ir_node *node, irg_walk_func pre, irg_walk_func post, void *env)
{
  ir_node *block, *pred;
  int i;

  assert(node);
  assert(!interprocedural_view);   /* interprocedural_view not implemented, because it
				    * interleaves with irg_walk */
  inc_irg_block_visited(current_ir_graph);
  if (is_no_Block(node)) block = get_nodes_Block(node); else block = node;
  assert(get_irn_opcode(block)  == iro_Block);
  irg_block_walk_2(block, pre, post, env);
  /* keepalive: the endless loops ... */
  if (get_irn_op(node) == op_End)
    for (i = 0; i < get_irn_arity(node); i++) {
      pred = get_irn_n(node, i);
      if (get_irn_op(pred) == op_Block)
	irg_block_walk_2(pred, pre, post, env);
    }

  return;
}


void irg_block_walk_graph(ir_graph *irg, irg_walk_func pre, irg_walk_func post, void *env) {
  ir_graph * rem = current_ir_graph;
  current_ir_graph = irg;
  irg_block_walk(get_irg_end(irg), pre, post, env);
  current_ir_graph = rem;
}
