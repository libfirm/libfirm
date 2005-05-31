/*
 * Project:     libFIRM
 * File name:   ir/ana/irextbb.c
 * Purpose:     Extended basis block support.
 * Author:      Michael Beck
 * Modified by:
 * Created:     5.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irextbb.c
 *
 *  Computes extended basic blocks.
 *
 *  @author Michael Beck
 */
#include "irextbb_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "xmalloc.h"
#include "irprintf.h"

typedef struct _env {
  struct obstack *obst;   /**< the obstack where allocations took place */
  ir_extblk *head;        /**< head of the list of all extended blocks */
} env_t;

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
 * add a block to an extended block
 */
static void addto_extblk(ir_extblk *extblk, ir_node *block)
{
  /* link all blocks belonging to this extended block */
  set_irn_link(block, extblk->link);

  extblk->link = block;
  extblk->visited++;

  set_Block_extbb(block, extblk);
}


/**
 * Pre block-walker. Calculates the extended block info.
 * Currently, we use the (free) block input of all basis blocks
 * to point to the leader block.
 */
static void pre_walk_calc_extbb(ir_node *block, void *ctx)
{
  int n = get_Block_n_cfgpreds(block);
  env_t *env = ctx;

  if (n > 1 || block == get_irg_start_block(current_ir_graph)) {
    /*
     * block is a JOIN-node ie he control flow from
     * many other blocks joins here. block is a leader.
     */
    allocate_extblk(block, env);
  }
  else {
    ir_node *add_to = get_Block_cfgpred(block, 0);

    if (! is_Bad(add_to))
      add_to = get_nodes_block(add_to);

    /* blocks with only one BAD predecessors are leaders too */
    if (is_Bad(add_to)) {
      allocate_extblk(block, env);
    }
    else {
      /*
       * Only on control flow predecessor. This block belongs
       * to the same extended basic block as its predecessor.
       */
      set_Block_extbb(block, NULL);
    }
  }
}

/**
 * Post block-walker. Calculates the extended block info.
 * During construction, we use the (free) block input of all basic blocks
 * to point to there previous block.
 */
static void post_walk_calc_extbb(ir_node *block, void *env)
{
  ir_extblk *extbb = get_Block_extbb(block);

  if (! extbb) {
    ir_node *prev = block;

    /* search the leader */
    do {
      prev = get_nodes_block(get_Block_cfgpred(prev, 0));
      extbb = get_Block_extbb(prev);
    } while (! extbb);

    addto_extblk(extbb, block);
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

  env.obst = irg->extbb_obst;
  env.head = NULL;

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

      extbb->blks[i] = block;
      set_irn_link(block, NULL);
      block = nblock;
    }

    extbb->link    = NULL;
    extbb->visited = 0;
  }

  irg->extblk_state = ir_extblk_info_valid;
}

/* free all extended block info. */
void free_extbb(ir_graph *irg) {
  if (irg->extbb_obst) {
    obstack_free(irg->extbb_obst, NULL);
    xfree(irg->extbb_obst);
    irg->extbb_obst = NULL;
  }
  irg->extblk_state = ir_extblk_info_none;
}

/* Return the extended block of a node. */
ir_extblk *get_nodes_extbb(ir_node *node) {
  ir_node *block = is_Block(node) ? node : get_nodes_block(node);
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

/* Return the number of basis blocks of an extended block */
int (get_extbb_n_blocks)(const ir_extblk *blk) {
  return _get_extbb_n_blocks(blk);
}

/* Return the i'th basis block of an extended block */
ir_node *(get_extbb_block)(ir_extblk *blk, int pos) {
  return _get_extbb_block(blk, pos);
}
