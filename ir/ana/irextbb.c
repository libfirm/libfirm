/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Extended basis block support.
 * @author   Michael Beck
 * @date     5.2005
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "irextbb_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "irouts.h"
#include "xmalloc.h"
#include "irprintf.h"

typedef struct _env {
  struct obstack *obst;   /**< the obstack where allocations took place */
  ir_extblk *head;        /**< head of the list of all extended blocks */
  ir_node *start_block;   /**< the start block of the current graph */
} env_t;

int (is_ir_extbb)(const void *thing) {
  return _is_ir_extbb(thing);
}

/**
 * allocate a new extended block header.
 */
static void allocate_extblk(ir_node *block, env_t *env)
{
  ir_extblk *extblk = obstack_alloc(env->obst, sizeof(*extblk));

  extblk->kind    = k_ir_extblk;
  extblk->visited = 1;
  extblk->blks    = (ir_node **)env->head;
  extblk->link    = block;
  env->head       = extblk;

  set_Block_extbb(block, extblk);
  set_irn_link(block, NULL);
}

/**
 * Returns the number of block successors.
 * we are interested only in 1, 2 and >2.
 */
static int get_block_n_succs(ir_node *block) {
  if (edges_activated(current_ir_graph)) {
    const ir_edge_t *edge;

    edge = get_block_succ_first(block);
    if (! edge)
      return 0;
    edge = get_block_succ_next(block, edge);
    if (! edge)
      return 1;
    edge = get_block_succ_next(block, edge);
    return edge ? 3 : 2;
  }
  return get_Block_n_cfg_outs(block);
}

/**
 * Pre block-walker. Calculates the extended block info.
 */
static void pre_walk_calc_extbb(ir_node *block, void *ctx)
{
  int n = get_Block_n_cfgpreds(block);
  env_t *env = ctx;

  if (n <= 0 || n > 1 || block == env->start_block) {
    /*
     * block is a JOIN-node ie the control flow from
     * many other blocks joins here. block is a leader.
     * Note that we handle unreachable blocks (n <= 0) here too.
     */
    allocate_extblk(block, env);
  }
  else {    /* we have only one control flow predecessor */
    ir_node *add_to = get_Block_cfgpred_block(block, 0);

    /* blocks with only one BAD predecessors are leaders too */
    if (is_Bad(add_to)) {
      allocate_extblk(block, env);
    } else {
      /*
       * Only one control flow predecessor. This block belongs
       * to the same extended basic block as its predecessor.
       */
      ir_node *cf_op = skip_Proj(get_Block_cfgpred(block, 0));

      if (irn_not_visited(cf_op)) {
        ir_node *pred_bl = get_nodes_block(cf_op);
        if (get_block_n_succs(pred_bl) > 2) {
          /* More than two successors means we have a jump table.
           * we cannot include a jump target into the current extended
           * basic block, so create a new one here.
           */
          allocate_extblk(block, env);
        } else {
          /* either the previous block has only one successor or
           * this is the first successor after an if, include it.
           */
          set_Block_extbb(block, NULL);
        }
        mark_irn_visited(cf_op);
      } else {
        /* already marked, so begin a new extended block here */
        allocate_extblk(block, env);
      }
    }
  }
}

/** A special extended block used as sentinel */
static ir_extblk _sentinel = { k_ir_extblk, 0xFEA1DEAD };

/**
 * Post block-walker. Calculates the extended block info.
 * During construction, we use the (free) block input of all basic blocks
 * to point to there previous block.
 */
static void post_walk_calc_extbb(ir_node *block, void *ctx)
{
  ir_extblk *extbb = get_Block_extbb(block);
  env_t *env = ctx;
  ir_extblk *sentinel = &_sentinel;

  if (! extbb) {
    ir_node *curr, *prev, *list;

    /*
     * Search the leader. It can happen, that we fall into an endless
     * loop, because we enter an unreachable loop that is not yet detected.
     * We break the loop using a sentinel.
     */
    for (curr = block; !extbb; curr = prev) {
      prev = get_Block_cfgpred_block(curr, 0);
      extbb = get_Block_extbb(prev);
      set_Block_extbb(curr, sentinel);
    }

    if (extbb == sentinel) {
      /* We detect a dead loop. We fix this by allocating a
       * special Extended block
       */
      ir_printf("Dead loop detected starting with %+F::%+F\n", get_irg_entity(current_ir_graph), block);

      allocate_extblk(block, env);
      extbb = get_Block_extbb(block);
      set_Block_extbb(block, sentinel);
    }

    /* replace all sentinels by the extbb info */
    prev = block;
    list = NULL;
    while (1) {
      if (get_Block_extbb(prev) != sentinel)
        break;
      set_irn_link(prev, list);
      list = prev;
      prev = get_Block_cfgpred_block(prev, 0);
    }
    /* arg, the list is in wrong order, turn around and add to the extbb list */
    for (curr = list; curr; curr = prev) {
      prev = get_irn_link(curr);
      set_irn_link(curr, extbb->link);
      extbb->link = curr;
      set_Block_extbb(curr, extbb);
      ++extbb->visited;
    }
  }
}

/*
 * Compute the extended basic blocks for a graph
 */
void compute_extbb(ir_graph *irg) {
  env_t env;
  ir_extblk *extbb, *next;

  if (irg->extbb_obst)
    obstack_free(irg->extbb_obst, NULL);
  else {
    irg->extbb_obst = xmalloc(sizeof(*irg->extbb_obst));
  }
  obstack_init(irg->extbb_obst);

  env.obst        = irg->extbb_obst;
  env.head        = NULL;
  env.start_block = get_irg_start_block(irg);

  if (! edges_activated(irg)) {
    /* we don't have edges */
    assure_irg_outs(irg);
  }

  /* we must mark nodes, so increase the visited flag */
  inc_irg_visited(irg);
  irg_block_walk_graph(irg, pre_walk_calc_extbb, post_walk_calc_extbb, &env);

  /*
   * Ok, we have now the list of all extended blocks starting with env.head
   * every extended block "knowns" the number of blocks in visited and
   * the blocks are linked in link.
   * Now we can create arrays that hold the blocks, some kind of "out" edges
   * for the extended block
   */
  for (extbb = env.head; extbb; extbb = next) {
    int i, len = (int)extbb->visited;
    ir_node *block;

    next = (ir_extblk *)extbb->blks;

    extbb->blks = NEW_ARR_D(ir_node *, env.obst, len);

    for (block = extbb->link, i = 0; i < len; ++i) {
      ir_node *nblock = get_irn_link(block);

      /* ensure that the leader is the first one */
      extbb->blks[len - 1 - i] = block;
      set_irn_link(block, NULL);
      block = nblock;
    }

#ifndef NDEBUG
    /* check it */
    for (i = len - 1; i > 0; --i) {
      ir_node *blk = extbb->blks[i];

      if (get_Block_n_cfgpreds(blk) != 1) {
        assert(!"Block for more than one predecessors is no leader");
      } else if (get_Block_cfgpred_block(blk, 0) != extbb->blks[i - 1]) {
        assert(!"extbb block order wrong");
      }
    }
#endif

    extbb->link    = NULL;
    extbb->visited = 0;
  }

  irg->extblk_state = extblk_valid;
}

/* free all extended block info. */
void free_extbb(ir_graph *irg) {
  if (irg->extbb_obst) {
    obstack_free(irg->extbb_obst, NULL);
    xfree(irg->extbb_obst);
    irg->extbb_obst = NULL;
  }
  irg->extblk_state = extblk_none;
}

/* Return the extended block of a node. */
ir_extblk *get_nodes_extbb(ir_node *node) {
  ir_node *block = is_Block(node) ? node : get_irn_n(node, -1);
  return get_Block_extbb(block);
}

/* Gets the visited counter of an extended block. */
unsigned long (get_extbb_visited)(const ir_extblk *blk) {
  return _get_extbb_visited(blk);
}

/* Sets the visited counter of an extended block. */
void (set_extbb_visited)(ir_extblk *blk, unsigned long visited) {
  _set_extbb_visited(blk, visited);
}

/* Mark an extended block as visited in a graph. */
void (mark_extbb_visited)(ir_extblk *blk) {
  _mark_extbb_visited(blk);
}

/* Returns non-zero if an extended was visited. */
int (extbb_visited)(const ir_extblk *blk) {
  return _extbb_visited(blk);
}

/* Returns non-zero if an extended block was NOT visited. */
int (extbb_not_visited)(const ir_extblk *blk) {
  return _extbb_not_visited(blk);
}

/* Returns the link field of an extended block. */
void *(get_extbb_link)(const ir_extblk *blk) {
  return _get_extbb_link(blk);
}

/* Sets the link field of an extended block. */
void (set_extbb_link)(ir_extblk *blk, void *link) {
  _set_extbb_link(blk, link);
}

/* Return the number of basic blocks of an extended block */
int (get_extbb_n_blocks)(const ir_extblk *blk) {
  return _get_extbb_n_blocks(blk);
}

/* Return the i'th basic block of an extended block */
ir_node *(get_extbb_block)(ir_extblk *blk, int pos) {
  return _get_extbb_block(blk, pos);
}

/* Return the leader basis block of an extended block. */
ir_node *(get_extbb_leader)(ir_extblk *blk) {
  return _get_extbb_leader(blk);
}

/* Return the node number of an extended block. */
long get_extbb_node_nr(ir_extblk *blk) {
  return get_irn_node_nr(get_extbb_leader(blk));
}

static void irg_extblock_walk_2(ir_extblk *blk, extbb_walk_func *pre, extbb_walk_func *post, void *env)
{
  int i;
  ir_node *node;

  if (extbb_not_visited(blk)) {
    mark_extbb_visited(blk);

    if (pre) pre(blk, env);

    node = get_extbb_leader(blk);
    for (i = get_Block_n_cfgpreds(node) - 1; i >= 0; --i) {
      /* find the corresponding predecessor block. */
      ir_node *pred = get_Block_cfgpred_block(node, i);
      if (is_Block(pred)) {
        /* recursion */
        irg_extblock_walk_2(get_Block_extbb(pred), pre, post, env);
      }
      else {
        assert(is_Bad(pred));
      }
    }

    if (post) post(blk, env);
  }
}

/* walks only over extended Block nodes in the graph.  Has it's own visited
   flag, so that it can be interleaved with the other walker.         */
void irg_extblock_walk(ir_extblk *blk, extbb_walk_func *pre, extbb_walk_func *post, void *env)
{
  ir_node *pred, *start_bl = get_irg_start_block(current_ir_graph);
  ir_extblk *start_blk = get_Block_extbb(start_bl);
  int i;

  assert(blk);
  assert(!get_interprocedural_view());   /* interprocedural_view not implemented */
  inc_irg_block_visited(current_ir_graph);

  /* assure the start block is the first one */
  mark_extbb_visited(start_blk);
  if (post)
    post(start_blk, env);
  irg_extblock_walk_2(blk, pre, post, env);

  /* keepalive: the endless loops ... */
  if (blk == get_Block_extbb(get_irg_end_block(current_ir_graph))) {
    ir_node *node = get_irg_end(current_ir_graph);
    int arity = get_irn_arity(node);
    for (i = 0; i < arity; i++) {
      pred = get_irn_n(node, i);
      if (is_Block(pred))
        irg_extblock_walk_2(get_Block_extbb(pred), pre, post, env);
      else if (is_Phi(pred)) {
        /* Sometimes the blocks died, but are still reachable through Phis.
         * Make sure the algorithms that try to remove these reach them. */
        ir_node *block = get_nodes_block(pred);

        if (! is_Bad(block))
          irg_extblock_walk_2(get_Block_extbb(block), pre, post, env);
      }
    }
  }

  if (pre)
    pre(start_blk, env);
}

/* Walks only over reachable Extended Basic Block nodes in the graph. */
void irg_extblock_walk_graph(ir_graph *irg, extbb_walk_func *pre, extbb_walk_func *post, void *env)
{
  ir_node *endbl = get_irg_end_block(irg);
  ir_extblk *blk = get_Block_extbb(endbl);
  ir_graph *rem  = current_ir_graph;
  current_ir_graph = irg;
  irg_extblock_walk(blk, pre, post, env);
  current_ir_graph = rem;
}
