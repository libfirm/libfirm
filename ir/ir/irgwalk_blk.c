/*
 * Project:     libFIRM
 * File name:   ir/ir/irgwalk_blk.c
 * Purpose:
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "irnode_t.h"
#include "irgraph_t.h" /* visited flag */
#include "irgwalk.h"
#include "pdeq.h"
#include "pset.h"
#include "firmstat.h"

/**
 * Metadata for block walker
 */
typedef struct _blk_collect_data_t {
  struct obstack obst;      /**< obstack to allocate objects on */
  pset           *blk_map;  /**< Hash map: Block -> List */
  pdeq           *blk_list; /**< the Block list */
} blk_collect_data_t;

/**
 * An entry for a block in the blk_map
 */
typedef struct _block_entry_t {
  ir_node *block;    /**< the block */
  pdeq    *list;     /**< the instruction list */
} block_entry_t;

/**
 * compare two block_entries
 */
static int addr_cmp(const void *elt, const void *key) {
  const block_entry_t *e1 = elt;
  const block_entry_t *e2 = key;

  return e1->block != e2->block;
}

/**
 * calculates a hash value for an block address
 * Addresses are typically aligned at 32bit, so we ignore the lowest bits
 */
static INLINE unsigned block_hash(const ir_node *node) {
  return (unsigned)node >> 3;
}

/**
 * Returns the associates block_entry_t for an block
 */
static block_entry_t *block_find_entry(ir_node *block, blk_collect_data_t *ctx)
{
  block_entry_t key;
  block_entry_t *elem;

  key.block = block;
  elem = pset_find(ctx->blk_map, &key, block_hash(block));
  if (elem)
    return elem;

  elem = obstack_alloc(&ctx->obst, sizeof(*elem));

  elem->block = block;
  elem->list  = new_pdeq();

  return pset_insert(ctx->blk_map, elem, block_hash(block));
}

/**
 * collect nodes
 */
static void collect_nodes(ir_node *node, void *env)
{
   blk_collect_data_t *ctx = env;
   ir_node            *block;
   block_entry_t      *entry;

   if (is_Block(node))  {
     /* it's a block, put it into the block list */
     pdeq_putr(ctx->blk_list, node);
     return;
   }

   block = get_nodes_block(node);
   entry = block_find_entry(block, ctx);

   if (get_irn_mode(node) == mode_X)
     pdeq_putl(entry->list, node);
   else
     pdeq_putr(entry->list, node);
}

/**
 * traverse the pre order only, from End to Start
 */
static void traverse_pre(blk_collect_data_t* blks, irg_walk_func *pre, void *env)
{
  int i, j;

  for (i = pdeq_len(blks->blk_list); i > 0; --i) {
    ir_node       *block = pdeq_getl(blks->blk_list);
    block_entry_t *entry = block_find_entry(block, blks);

    for (j = pdeq_len(entry->list); j > 0; --j) {
      ir_node *node = pdeq_getl(entry->list);
      pre(node, env);
    }
    pre(block, env);
  }
}

/**
 * traverse the post order only, from Start to End
 */
static void traverse_post(blk_collect_data_t* blks, irg_walk_func *post, void *env)
{
  int i, j;

  for (i = pdeq_len(blks->blk_list); i > 0; --i) {
    ir_node       *block = pdeq_getr(blks->blk_list);
    block_entry_t *entry = block_find_entry(block, blks);

    post(block, env);
    for (j = pdeq_len(entry->list); j > 0; --j) {
      ir_node *node = pdeq_getr(entry->list);
      post(node, env);
    }
  }
}

/**
 * traverse both
 */
static void traverse_both(blk_collect_data_t* blks, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  int i, j;

  /* pre walk, rotate all lists */
  for (i = pdeq_len(blks->blk_list); i > 0; --i) {
    ir_node       *block = pdeq_getl(blks->blk_list);
    block_entry_t *entry = block_find_entry(block, blks);

    pdeq_putr(blks->blk_list, block);

    for (j = pdeq_len(entry->list); j > 0; --j) {
      ir_node *node = pdeq_getl(entry->list);

      pdeq_putr(entry->list, node);
      pre(node, env);
    }
    pre(block, env);
  }

  /* second step */
  traverse_post(blks, post, env);
}


/**
 * Do the traversal
 */
static void traverse(blk_collect_data_t* blks, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  if      (!post) traverse_pre (blks, pre, env);
  else if (!pre)  traverse_post(blks, post, env);
  else            traverse_both(blks, pre, post, env);
}

/**
 * Intraprozedural graph walker over blocks.
 */
static void
do_irg_walk_blk(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  blk_collect_data_t blks;
  int old_view = get_interprocedural_view();

  /* switch off interprocedural view */
  set_interprocedural_view(false);

  obstack_init(&blks.obst);
  blks.blk_map  = new_pset(addr_cmp, 1);
  blks.blk_list = new_pdeq();

  if (node->visited < current_ir_graph->visited) {
    /* first step: traverse the graph and fill the lists */
    irg_walk(node, collect_nodes, NULL, &blks);

    /* second step: traverse the list */
    traverse(&blks, pre, post, env);
  }

  del_pdeq(blks.blk_list);
  del_pset(blks.blk_map);
  obstack_free(&blks.obst, NULL);

  set_interprocedural_view(old_view);
}

void irg_walk_blkwise(ir_node *node, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  inc_irg_visited(current_ir_graph);
  do_irg_walk_blk(node, pre, post, env);
}

void irg_walk_blkwise_graph(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post, void *env)
{
  ir_graph * rem = current_ir_graph;

  stat_irg_walk_blkwise(irg, (void *)pre, (void *)post);
  current_ir_graph = irg;
  irg_walk_blkwise(get_irg_end(irg), pre, post, env);
  current_ir_graph = rem;
}
