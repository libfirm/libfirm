/*
 * Project:     libFIRM
 * File name:   ir/ana/irdom.c
 * Purpose:     Construct and access dominator tree.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "irouts.h"

#include "xmalloc.h"
#include "irgwalk.h"
#include "irdom_t.h"
#include "irgraph_t.h"   /* To access state field. */
#include "irnode_t.h"
#include "ircons_t.h"

/*--------------------------------------------------------------------*/
/** Accessing the dominator data structures                          **/
/*--------------------------------------------------------------------*/

ir_node *get_Block_idom(ir_node *bl) {
  assert(get_irn_op(bl) == op_Block);
  if (get_Block_dom_depth(bl) == -1) {
    /* This block is not reachable from Start */
    return new_Bad();
  }
  return bl->attr.block.dom.idom;
}

void set_Block_idom(ir_node *bl, ir_node *n) {
  assert(get_irn_op(bl) == op_Block);
  bl->attr.block.dom.idom = n;
}

int get_Block_pre_num(ir_node *bl) {
  assert(get_irn_op(bl) == op_Block);
  return bl->attr.block.dom.pre_num;
}

void set_Block_pre_num(ir_node *bl, int num) {
  assert(get_irn_op(bl) == op_Block);
  bl->attr.block.dom.pre_num = num;
}

int get_Block_dom_depth(ir_node *bl) {
  assert(get_irn_op(bl) == op_Block);
  return bl->attr.block.dom.dom_depth;
}

void set_Block_dom_depth(ir_node *bl, int depth) {
  assert(get_irn_op(bl) == op_Block);
  bl->attr.block.dom.dom_depth = depth;
}



/*--------------------------------------------------------------------*/
/*  Building and Removing the dominator datastructure                 */
/*--------------------------------------------------------------------*/

static void count_and_init_blocks(ir_node *bl, void *env) {
  int *n_blocks = (int *) env;
  (*n_blocks) ++;

  set_Block_idom(bl, NULL);
  set_Block_pre_num(bl, -1);
  set_Block_dom_depth(bl, -1);
}

/* temporary type used while constructing the dominator tree. */
typedef struct tmp_dom_info {
  ir_node *block;               /* backlink */

  struct tmp_dom_info *semi;	/* semidominator */
  struct tmp_dom_info *parent;
  struct tmp_dom_info *label;	/* used for LINK and EVAL */
  struct tmp_dom_info *ancestor;/* used for LINK and EVAL */
  struct tmp_dom_info *dom;	/* After step 3, if the semidominator of w is
				   its immediate dominator, then w->dom is the
				   immediate dominator of w.  Otherwise w->dom
				   is a vertex v whose number is smaller than
				   w and whose immediate dominator is also w's
				   immediate dominator. After step 4, w->dom
				   is the immediate dominator of w.  */
  struct tmp_dom_info *bucket;	/* set of vertices with same semidominator */
} tmp_dom_info;

/* Struct to pass info through walker. */
typedef struct {
  tmp_dom_info *d;
  int used;

} dom_env;


/* Walks Blocks along the out datastructure.  If recursion started with
   Start block misses control dead blocks. */
static void init_tmp_dom_info(ir_node *bl, tmp_dom_info *parent,
			      tmp_dom_info *tdi_list, int* used) {
  tmp_dom_info *tdi;
  int i;

  assert(get_irn_op(bl) == op_Block);
  if (get_irg_block_visited(current_ir_graph) == get_Block_block_visited(bl))
    return;
  mark_Block_block_visited(bl);
  set_Block_pre_num(bl, *used);

  tdi = &tdi_list[*used];
  ++(*used);

  tdi->semi = tdi;
  tdi->label = tdi;
  tdi->ancestor = NULL;
  tdi->bucket = NULL;
  tdi->parent = parent;
  tdi->block = bl;

  /* Iterate */
  for(i = 0; i < get_Block_n_cfg_outs(bl); i++) {
    ir_node *pred = get_Block_cfg_out(bl, i);
    assert(get_irn_opcode(pred) == iro_Block);
    init_tmp_dom_info(pred, tdi, tdi_list, used);
  }
}

static void
dom_compress (tmp_dom_info *v)
{
  assert (v->ancestor);
  if (v->ancestor->ancestor) {
    dom_compress (v->ancestor);
    if (v->ancestor->label->semi < v->label->semi) {
      v->label = v->ancestor->label;
    }
    v->ancestor = v->ancestor->ancestor;
  }
}

/* if V is a root, return v, else return the vertex u, not being the
   root, with minimum u->semi on the path from v to its root. */
INLINE static tmp_dom_info*
dom_eval (tmp_dom_info *v)
{
  if (!v->ancestor) return v;
  dom_compress (v);
  return v->label;
}

/* make V W's ancestor */
INLINE static void
dom_link (tmp_dom_info *v, tmp_dom_info *w)
{
  w->ancestor = v;
}

/* Computes the dominator trees.  Sets a flag in irg to "dom_consistent".
   If the control flow of the graph is changed this flag must be set to
   "dom_inconsistent".  */
void compute_doms(ir_graph *irg) {
  ir_graph *rem = current_ir_graph;
  int n_blocks, used, i, j;
  tmp_dom_info *tdi_list;   /* Ein Golf? */

  current_ir_graph = irg;

  /* Update graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  current_ir_graph->dom_state = dom_consistent;

  /* Count the number of blocks in the graph. */
  n_blocks = 0;
  irg_block_walk(get_irg_end(current_ir_graph), count_and_init_blocks, NULL, &n_blocks);

  /* Memory for temporary information. */
  tdi_list = xcalloc(n_blocks, sizeof(tdi_list[0]));

  /* We need the out datastructure. */
  if (current_ir_graph->outs_state != outs_consistent)
    compute_outs(current_ir_graph);

  /* this with a standard walker as passing the parent to the sons isn't
     simple. */
  used = 0;
  inc_irg_block_visited(current_ir_graph);
  init_tmp_dom_info(get_irg_start_block(current_ir_graph), NULL, tdi_list, &used);
  /* If not all blocks are reachable from Start by out edges this assertion
     fails.
     assert(used == n_blocks && "Precondition for dom construction violated"); */
  n_blocks = used;


  for (i = n_blocks-1; i > 0; i--) {  /* Don't iterate the root, it's done. */
    int irn_arity;
    tmp_dom_info *w = &tdi_list[i];
    tmp_dom_info *v;

    /* Step 2 */
    irn_arity = get_irn_arity(w->block);
    for (j = 0;  j < irn_arity;  j++) {
      ir_node *cf_op = get_Block_cfgpred(w->block, j);
      ir_node *pred  = get_nodes_block(cf_op);
      tmp_dom_info *u;

      if ((is_Bad(cf_op)) || (is_Bad(pred)) ||
	  (get_Block_pre_num (pred) == -1))
	continue;	/* control-dead */

      u = dom_eval (&tdi_list[get_Block_pre_num(pred)]);
      if (u->semi < w->semi) w->semi = u->semi;
    }
    /* Add w to w->semi's bucket.  w is in exactly one bucket, so
       buckets can ben implemented as linked lists. */
    w->bucket = w->semi->bucket;
    w->semi->bucket = w;

    dom_link (w->parent, w);

    /* Step 3 */
    while (w->parent->bucket) {
      tmp_dom_info *u;
      v = w->parent->bucket;
      /* remove v from w->parent->bucket */
      w->parent->bucket = v->bucket;
      v->bucket = NULL;

      u = dom_eval (v);
      if (u->semi < v->semi)
	v->dom = u;
      else
	v->dom = w->parent;
    }
  }
  /* Step 4 */
  tdi_list[0].dom = NULL;
  set_Block_idom(tdi_list[0].block, NULL);
  set_Block_dom_depth(tdi_list[0].block, 1);
  for (i = 1;  i < n_blocks;  i++) {
    tmp_dom_info *w = &tdi_list[i];

    if (w->dom != w->semi) w->dom = w->dom->dom;
    set_Block_idom(w->block, w->dom->block);
    set_Block_dom_depth(w->block, get_Block_dom_depth(w->dom->block) + 1);
  }

  /* clean up */
  /*  free(tdi_list); @@@ does not work !!?? */
  current_ir_graph = rem;
}

void free_dom_and_peace(ir_graph *irg) {
  /* Update graph state */
  assert(get_irg_phase_state(current_ir_graph) != phase_building);
  current_ir_graph->dom_state = dom_none;

  /* With the implementation right now there is nothing to free,
     but better call it anyways... */
}
