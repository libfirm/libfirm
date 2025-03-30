/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Blockwise walker implementation
 * @author  Michael Beck
 */
#include "array.h"
#include "hashptr.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irnode_t.h"
#include "pset.h"

/**
 * Metadata for block walker.
 */
typedef struct blk_collect_data_t {
	struct obstack obst;     /**< obstack to allocate objects on */
	pset          *blk_map;  /**< Hash map: Block -> List */
	ir_node      **blk_list; /**< the Block list */
} blk_collect_data_t;

/**
 * An entry for a block in the blk_map.
 */
typedef struct block_entry_t {
	ir_node  *block;      /**< the block */
	ir_node **phi_list;   /**< the list of Phi instruction */
	ir_node **df_list;    /**< the list of data flow instruction */
	ir_node **cf_list;    /**< the list of control flow instructions */
	ir_node **entry_list; /**< list of all block entries */
} block_entry_t;

/**
 * Compare two block_entries.
 */
static int addr_cmp(const void *elt, const void *key)
{
	const block_entry_t *e1 = (const block_entry_t*)elt;
	const block_entry_t *e2 = (const block_entry_t*)key;
	return e1->block != e2->block;
}

/**
 * Returns the associates block_entry_t for an block.
 */
static block_entry_t *block_find_entry(ir_node *block, blk_collect_data_t *ctx)
{
	block_entry_t key;
	key.block = block;
	block_entry_t *elem
		= (block_entry_t*)pset_find(ctx->blk_map, &key, hash_ptr(block));
	if (elem != NULL)
		return elem;

	elem = OALLOC(&ctx->obst, block_entry_t);
	elem->block      = block;
	elem->phi_list   = NEW_ARR_F(ir_node *, 0);
	elem->df_list    = NEW_ARR_F(ir_node *, 0);
	elem->cf_list    = NEW_ARR_F(ir_node *, 0);
	elem->entry_list = NEW_ARR_F(ir_node *, 0);
	return (block_entry_t*)pset_insert(ctx->blk_map, elem, hash_ptr(block));
}

/**
 * Traverse a block in pre order.
 */
static void traverse_block_pre(ir_node *block, block_entry_t *entry,
                               irg_walk_func *pre, void *env)
{
	for (size_t j = ARR_LEN(entry->cf_list); j-- > 0;) {
		ir_node *node = entry->cf_list[j];
		pre(node, env);
	}

	for (size_t j = ARR_LEN(entry->df_list); j-- > 0;) {
		ir_node *node = entry->df_list[j];
		pre(node, env);
	}

	for (size_t j = ARR_LEN(entry->phi_list); j-- > 0;) {
		ir_node *node = entry->phi_list[j];
		pre(node, env);
	}

	pre(block, env);
}

/**
 * Traverse a block in post order.
 */
static void traverse_block_post(ir_node *block, block_entry_t *entry,
                                irg_walk_func *post, void *env)
{
	post(block, env);

	for (size_t j = 0, n = ARR_LEN(entry->phi_list); j < n; ++j) {
		ir_node *node = entry->phi_list[j];
		post(node, env);
	}

	for (size_t j = 0, n = ARR_LEN(entry->df_list); j < n; ++j) {
		ir_node *node = entry->df_list[j];
		post(node, env);
	}

	for (size_t j = 0, n = ARR_LEN(entry->cf_list); j < n; ++j) {
		ir_node *node = entry->cf_list[j];
		post(node, env);
	}
}

static void free_block_entry(block_entry_t *entry)
{
	DEL_ARR_F(entry->entry_list);
	DEL_ARR_F(entry->phi_list);
	DEL_ARR_F(entry->df_list);
	DEL_ARR_F(entry->cf_list);
}

/**
 * Traverse the pre order only, from End to Start.
 */
static void traverse_pre(blk_collect_data_t *blks, irg_walk_func *pre, void *env)
{
	for (size_t i = ARR_LEN(blks->blk_list); i-- > 0;) {
		ir_node       *block = blks->blk_list[i];
		block_entry_t *entry = block_find_entry(block, blks);

		traverse_block_pre(block, entry, pre, env);
		free_block_entry(entry);
	}
}

/**
 * Traverse the post order only, from Start to End.
 */
static void traverse_post(blk_collect_data_t *blks, irg_walk_func *post,
                          void *env)
{
	for (size_t i = 0, n = ARR_LEN(blks->blk_list); i < n; ++i) {
		ir_node       *block = blks->blk_list[i];
		block_entry_t *entry = block_find_entry(block, blks);

		traverse_block_post(block, entry, post, env);
		free_block_entry(entry);
	}
}

/**
 * Traverse both.
 */
static void traverse_both(blk_collect_data_t *blks, irg_walk_func *pre,
                          irg_walk_func *post, void *env)
{
	for (size_t i = ARR_LEN(blks->blk_list); i-- > 0;) {
		ir_node       *block = blks->blk_list[i];
		block_entry_t *entry = block_find_entry(block, blks);

		traverse_block_pre(block, entry, pre, env);
	}

	/* second step */
	traverse_post(blks, post, env);
}

/**
 * Do the traversal.
 */
static void traverse_blocks(ir_graph *irg, blk_collect_data_t *blks,
                            irg_walk_func *pre, irg_walk_func *post, void *env)
{
	(void)irg;
	if (post == NULL)
		traverse_pre(blks, pre, env);
	else if (pre == NULL)
		traverse_post(blks, post, env);
	else
		traverse_both(blks, pre, post, env);
}

typedef struct dom_traversal_t {
	blk_collect_data_t *blks;
	irg_walk_func      *pre;
	irg_walk_func      *post;
	void               *env;
} dom_traversal_t;

/**
 * Dom block walker. Visit all nodes in pre oder.
 */
static void dom_block_visit_pre(ir_node *block, void *env)
{
	dom_traversal_t *ctx   = (dom_traversal_t*)env;
	block_entry_t   *entry = block_find_entry(block, ctx->blks);
	traverse_block_pre(block, entry, ctx->pre, ctx->env);
	free_block_entry(entry);
}

/**
 * Dom block walker. Visit all nodes in post oder.
 */
static void dom_block_visit_post(ir_node *block, void *env)
{
	dom_traversal_t *ctx   = (dom_traversal_t*)env;
	block_entry_t   *entry = block_find_entry(block, ctx->blks);
	traverse_block_post(block, entry, ctx->post, ctx->env);
	free_block_entry(entry);
}

/**
 * Dom block walker. Visit all nodes in pre oder, than in post order.
 */
static void dom_block_visit_both(ir_node *block, void *env)
{
	dom_traversal_t *ctx   = (dom_traversal_t*)env;
	block_entry_t   *entry = block_find_entry(block, ctx->blks);
	traverse_block_pre(block, entry, ctx->pre, ctx->env);
	traverse_block_post(block, entry, ctx->post, ctx->env);
	free_block_entry(entry);
}

/**
 * Do the traversal in the dominator tree in top-down order.
 */
static void traverse_dom_blocks_top_down(ir_graph *irg, blk_collect_data_t* blks,
                                         irg_walk_func *pre, irg_walk_func *post,
                                         void *env)
{
	dom_traversal_t ctx;
	ctx.blks = blks;
	ctx.pre  = pre;
	ctx.post = post;
	ctx.env  = env;

	if (pre != NULL && post != NULL)
		dom_tree_walk_irg(irg, dom_block_visit_both, NULL, &ctx);
	else if (pre != NULL)
		dom_tree_walk_irg(irg, dom_block_visit_pre, NULL, &ctx);
	else if (post != NULL)
		dom_tree_walk_irg(irg, dom_block_visit_post, NULL, &ctx);
}

/**
 * walks over the graph and collects all blocks and all block entries
 */
static void collect_walk(ir_node *node, blk_collect_data_t *env)
{
	mark_irn_visited(node);

	if (is_Block(node)) {
		/* predecessors of a block are control flow nodes */
		foreach_irn_in_r(node, i, pred) {
			if (irn_visited(pred))
				continue;

			ir_node *blk = get_nodes_block(pred);
			collect_walk(pred, env);

			/* control flow predecessors are always block inputs */
			block_entry_t *entry = block_find_entry(blk, env);
			ARR_APP1(ir_node *, entry->entry_list, pred);
		}

		/* it's a block, put it into the block list, except for the end block
		 * which we append in the main loop. This avoids it being placed
		 * elsewhere if the graph contains endless loops. */
		if (node != get_irg_end_block(get_irn_irg(node)))
			ARR_APP1(ir_node *, env->blk_list, node);
	} else {
		ir_node *block = get_nodes_block(node);
		if (!irn_visited(block))
			collect_walk(block, env);

		bool is_phi = is_Phi(node);
		foreach_irn_in_r(node, i, pred) {
			if (irn_visited(pred))
				continue;

			collect_walk(pred, env);

			/* BEWARE: predecessors of End nodes might be blocks */
			if (is_Block(pred))
				continue;

			ir_node *blk = get_nodes_block(pred);

			/* Note that Phi predecessors are always block entries
			 * because Phi edges are always "outside" a block */
			if (block != blk || is_phi) {
				block_entry_t *entry = block_find_entry(blk, env);
				ARR_APP1(ir_node *, entry->entry_list, pred);
			}
		}
	}
}

/**
 * walks over the nodes of a block
 * and collects them into the right list
 */
static void collect_blks_lists(ir_node *node, ir_node *block,
                               block_entry_t *entry, blk_collect_data_t *env)
{
	mark_irn_visited(node);

	/* Do not descent into Phi predecessors, these are always
	 * outside the current block because Phi edges are always
	 * "outside". */
	if (!is_Phi(node)) {
		foreach_irn_in_r(node, i, pred) {
			/* BEWARE: predecessors of End nodes might be blocks */
			if (is_Block(pred))
				continue;
			if (irn_visited(pred))
				continue;

			ir_node *blk = get_nodes_block(pred);
			if (block != blk)
				continue;
			collect_blks_lists(pred, block, entry, env);
		}
	} else {
		ARR_APP1(ir_node *, entry->phi_list, node);
		return;
	}

	if (get_irn_mode(node) == mode_X) {
		ARR_APP1(ir_node *, entry->cf_list, node);
	} else {
		ARR_APP1(ir_node *, entry->df_list, node);
	}
}

/**
 * walk over the graph and collect all lists
 */
static void collect_lists(ir_graph *const irg, blk_collect_data_t *const env)
{
	inc_irg_visited(irg);

	for (size_t i = ARR_LEN(env->blk_list); i-- > 0;) {
		ir_node       *block = env->blk_list[i];
		block_entry_t *entry = block_find_entry(block, env);
		for (size_t j = ARR_LEN(entry->entry_list); j-- > 0;) {
			ir_node *node = entry->entry_list[j];
			/* a entry might already be visited due to Phi loops */
			if (irn_visited(node))
				continue;
			collect_blks_lists(node, block, entry, env);
		}
	}
}

/**
 * Intra procedural graph walker over blocks.
 */
static void do_irg_walk_blk(ir_graph *irg, irg_walk_func *pre,
                            irg_walk_func *post, void *env,
                            void (*traverse)(ir_graph *irg,
                                             blk_collect_data_t* blks,
                                             irg_walk_func *pre,
                                             irg_walk_func *post, void *env))
{
	blk_collect_data_t blks;
	obstack_init(&blks.obst);
	blks.blk_map  = new_pset(addr_cmp, 1);
	blks.blk_list = NEW_ARR_F(ir_node*, 0);

	/* first step: traverse the graph and fill the lists */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	ir_node *end_node = get_irg_end(irg);
	collect_walk(end_node, &blks);

	/* add the end block */
	ir_node *end_blk = get_irg_end_block(irg);
	ARR_APP1(ir_node *, blks.blk_list, end_blk);

	/* and the end node */
	block_entry_t *entry = block_find_entry(end_blk, &blks);
	ARR_APP1(ir_node *, entry->entry_list, end_node);

	collect_lists(irg, &blks);

	/* second step: traverse the list */
	traverse(irg, &blks, pre, post, env);

	DEL_ARR_F(blks.blk_list);
	del_pset(blks.blk_map);
	obstack_free(&blks.obst, NULL);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

void irg_walk_blkwise_graph(ir_graph *irg, irg_walk_func *pre,
                            irg_walk_func *post, void *env)
{
	do_irg_walk_blk(irg, pre, post, env, traverse_blocks);
}

void irg_walk_blkwise_dom_top_down(ir_graph *irg, irg_walk_func *pre,
                                   irg_walk_func *post, void *env)
{
	do_irg_walk_blk(irg, pre, post, env, traverse_dom_blocks_top_down);
}
