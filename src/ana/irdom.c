/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Construct and access dominator / post dominator tree.
 * @author    Goetz Lindenmaier, Michael Beck, Rubino Geiss
 * @date      2.2002
 */
#include "irdom_t.h"

#include "array.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irouts_t.h"
#include "util.h"
#include "xmalloc.h"
#include <string.h>

static inline ir_dom_info *get_dom_info(ir_node *block)
{
	assert(is_Block(block));
	return &block->attr.block.dom;
}

static inline const ir_dom_info *get_dom_info_const(const ir_node *block)
{
	assert(is_Block(block));
	return &block->attr.block.dom;
}

static inline ir_dom_info *get_pdom_info(ir_node *block)
{
	assert(is_Block(block));
	return &block->attr.block.pdom;
}

static inline const ir_dom_info *get_pdom_info_const(const ir_node *block)
{
	assert(is_Block(block));
	return &block->attr.block.pdom;
}

ir_node *get_Block_idom(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));
	if (get_Block_dom_depth(block) == -1) {
		/* This block is not reachable from Start */
		ir_graph *irg = get_irn_irg(block);
		return new_r_Bad(irg, mode_BB);
	}
	return get_dom_info_const(block)->idom;
}

void set_Block_idom(ir_node *block, ir_node *n)
{
	/* Set the immediate dominator of block to n */
	ir_dom_info *bli = get_dom_info(block);
	bli->idom = n;

	/* If we don't set the root of the dominator tree
	 * Append block to the dominates queue of n. */
	if (n != NULL) {
		ir_dom_info *ni = get_dom_info(n);

		bli->next = ni->first;
		ni->first = block;
	}
}

ir_node *get_Block_ipostdom(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	assert(get_Block_postdom_depth(block) != -1);
	return get_pdom_info_const(block)->idom;
}

void set_Block_ipostdom(ir_node *block, ir_node *n)
{
	/* Set the immediate post dominator of block to n */
	ir_dom_info *bli = get_pdom_info(block);
	bli->idom = n;

	/* If we don't set the root of the post dominator tree
	 * Append block to the post dominates queue of n. */
	if (n != NULL) {
		ir_dom_info *ni = get_pdom_info(n);

		bli->next = ni->first;
		ni->first = block;
	}
}

int get_Block_dom_pre_num(const ir_node *block)
{
	return get_dom_info_const(block)->pre_num;
}

void set_Block_dom_pre_num(ir_node *block, int num)
{
	get_dom_info(block)->pre_num = num;
}

int get_Block_dom_depth(const ir_node *block)
{
	return get_dom_info_const(block)->dom_depth;
}

void set_Block_dom_depth(ir_node *block, int depth)
{
	get_dom_info(block)->dom_depth = depth;
}

int get_Block_postdom_pre_num(const ir_node *block)
{
	return get_pdom_info_const(block)->pre_num;
}

void set_Block_postdom_pre_num(ir_node *block, int num)
{
	get_pdom_info(block)->pre_num = num;
}

int get_Block_postdom_depth(const ir_node *block)
{
	return get_pdom_info_const(block)->dom_depth;
}

void set_Block_postdom_depth(ir_node *block, int depth)
{
	get_pdom_info(block)->dom_depth = depth;
}

unsigned get_Block_dom_tree_pre_num(const ir_node *block)
{
	return get_dom_info_const(block)->tree_pre_num;
}

unsigned get_Block_dom_max_subtree_pre_num(const ir_node *block)
{
	return get_dom_info_const(block)->max_subtree_pre_num;
}

unsigned get_Block_pdom_tree_pre_num(const ir_node *block)
{
	return get_pdom_info_const(block)->tree_pre_num;
}

unsigned get_Block_pdom_max_subtree_pre_num(const ir_node *block)
{
	return get_pdom_info_const(block)->max_subtree_pre_num;
}

int block_dominates(const ir_node *a, const ir_node *b)
{
	assert(irg_has_properties(get_irn_irg(a), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));
	const ir_dom_info *ai = get_dom_info_const(a);
	const ir_dom_info *bi = get_dom_info_const(b);
	return bi->tree_pre_num - ai->tree_pre_num
		<= ai->max_subtree_pre_num - ai->tree_pre_num;
}

ir_node *ir_deepest_common_dominator(ir_node *block0, ir_node *block1)
{
	assert(irg_has_properties(get_irn_irg(block0), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));

	/* Both blocks must be reachable. */
	assert(get_Block_dom_depth(block0) >= 0);
	assert(get_Block_dom_depth(block1) >= 0);

	/* Shortcut. */
	if (block0 == block1)
		return block0;

	/* block0 shall be the shallowest block. */
	if (get_Block_dom_depth(block1) > get_Block_dom_depth(block0)) {
		ir_node *tmp = block0;
		block0 = block1;
		block1 = tmp;
	}

	/* Walk idom chain upwards until we found a block that dominates the other
	 * block. */
	while (!block_dominates(block0, block1)) {
		block0 = get_Block_idom(block0);
	}
	return block0;
}

ir_node *get_Block_dominated_first(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));
	return get_dom_info_const(block)->first;
}

ir_node *get_Block_dominated_next(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));
	return get_dom_info_const(block)->next;
}

int block_postdominates(const ir_node *a, const ir_node *b)
{
	assert(irg_has_properties(get_irn_irg(a), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	const ir_dom_info *ai = get_pdom_info_const(a);
	const ir_dom_info *bi = get_pdom_info_const(b);
	return bi->tree_pre_num - ai->tree_pre_num
		<= ai->max_subtree_pre_num - ai->tree_pre_num;
}

int block_strictly_postdominates(const ir_node *a, const ir_node *b)
{
	assert(irg_has_properties(get_irn_irg(a), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	return (a != b) && block_postdominates(a, b);
}

ir_node *get_Block_postdominated_first(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	return get_pdom_info_const(block)->first;
}

ir_node *get_Block_postdominated_next(const ir_node *block)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	return get_pdom_info_const(block)->next;
}

void dom_tree_walk(ir_node *block, irg_walk_func *pre, irg_walk_func *post,
                   void *env)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));

	if (pre != NULL)
		pre(block, env);

	dominates_for_each(block, p) {
		dom_tree_walk(p, pre, post, env);
	}

	if (post != NULL)
		post(block, env);
}

void postdom_tree_walk(ir_node *block, irg_walk_func *pre,
                       irg_walk_func *post, void *env)
{
	assert(irg_has_properties(get_irn_irg(block), IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));

	if (pre != NULL)
		pre(block, env);

	postdominates_for_each(block, p) {
		postdom_tree_walk(p, pre, post, env);
	}

	if (post != NULL)
		post(block, env);
}

void dom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre, irg_walk_func *post,
                       void *env)
{
	/* The root of the dominator tree should be the Start block. */
	ir_node *root = get_irg_start_block(irg);

	assert(irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE));
	assert(get_dom_info(root)->idom == NULL);
	dom_tree_walk(root, pre, post, env);
}

void postdom_tree_walk_irg(ir_graph *irg, irg_walk_func *pre,
                           irg_walk_func *post, void *env)
{
	/* The root of the post dominator tree should be the End block. */
	ir_node *root = get_irg_end_block(irg);

	assert(irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE));
	assert(get_pdom_info(root)->idom == NULL);
	postdom_tree_walk(root, pre, post, env);
}


static void assign_tree_dom_pre_order(ir_node *block, void *data)
{
	unsigned    *num = (unsigned*)data;
	ir_dom_info *bi  = get_dom_info(block);

	bi->tree_pre_num = (*num)++;
}

static void assign_tree_dom_pre_order_max(ir_node *block, void *data)
{
	(void)data;
	ir_dom_info *bi           = get_dom_info(block);
	unsigned     max          = 0;
	bool         has_children = false;

	for (ir_node *p = bi->first; p; p = get_dom_info(p)->next) {
		unsigned max_p = get_dom_info(p)->max_subtree_pre_num;
		max          = MAX(max, max_p);
		has_children = true;
	}

	bi->max_subtree_pre_num = has_children ? max : bi->tree_pre_num;
	assert(bi->max_subtree_pre_num >= bi->tree_pre_num);
}

static void assign_tree_postdom_pre_order(ir_node *block, void *data)
{
	unsigned    *num = (unsigned*)data;
	ir_dom_info *bi  = get_pdom_info(block);

	bi->tree_pre_num = (*num)++;
}

static void assign_tree_postdom_pre_order_max(ir_node *block, void *data)
{
	(void)data;
	ir_dom_info *bi           = get_pdom_info(block);
	unsigned     max          = 0;
	bool         has_children = false;

	for (ir_node *p = bi->first; p; p = get_pdom_info(p)->next) {
		unsigned max_p = get_pdom_info(p)->max_subtree_pre_num;
		max          = MAX(max, max_p);
		has_children = true;
	}

	bi->max_subtree_pre_num = has_children ? max : bi->tree_pre_num;
	assert(bi->max_subtree_pre_num >= bi->tree_pre_num);
}

/**
 * count the number of blocks and clears the post dominance info
 */
static void count_and_init_blocks_pdom(ir_node *block, void *env)
{
	unsigned *n_blocks = (unsigned*)env;
	(*n_blocks)++;

	memset(get_pdom_info(block), 0, sizeof(ir_dom_info));
	set_Block_ipostdom(block, NULL);
	set_Block_postdom_pre_num(block, -1);
	set_Block_postdom_depth(block, -1);
}

/** temporary type used while constructing the dominator / post dominator tree. */
typedef struct tmp_dom_info {
	ir_node *block;               /**< backlink */

	struct tmp_dom_info *semi;    /**< semidominator */
	struct tmp_dom_info *parent;
	struct tmp_dom_info *label;   /**< used for LINK and EVAL */
	struct tmp_dom_info *ancestor;/**< used for LINK and EVAL */
	struct tmp_dom_info *dom;     /**< After step 3, if the semidominator of w
	                                   is its immediate dominator, then w->dom
	                                   is the immediate dominator of w.
	                                   Otherwise w->dom is a vertex v whose
	                                   number is smaller than w and whose
	                                   immediate dominator is also w's
	                                   immediate dominator. After step 4,
	                                   w->dom is the immediate dominator of w.*/
	struct tmp_dom_info *bucket;  /**< set of vertices with same semidominator */
	int unreachable : 1; /**< node is not reachable by control flow edges */
} tmp_dom_info;

/**
 * Walks Blocks along the out data structure.  If recursion started with
 * Start block misses control dead blocks.
 */
static void init_tmp_dom_info(ir_node *block, tmp_dom_info *parent,
                              tmp_dom_info *tdi_list, int *used, int n_blocks)
{
	if (Block_block_visited(block)) {
		return;
	}
	mark_Block_block_visited(block);
	set_Block_dom_pre_num(block, *used);

	assert(*used < n_blocks);
	tmp_dom_info *tdi = &tdi_list[*used];
	++(*used);

	tdi->block       = block;
	tdi->semi        = tdi;
	tdi->parent      = parent;
	tdi->label       = tdi;
	tdi->ancestor    = NULL;
	tdi->dom         = NULL;
	tdi->bucket      = NULL;
	tdi->unreachable = 0;

	/* Iterate */
	for (unsigned i = get_Block_n_cfg_outs_ka(block); i-- != 0;) {
		ir_node *pred = get_Block_cfg_out_ka(block, i);
		/* can happen for half-optimized dead code */
		if (!is_Block(pred))
			continue;

		init_tmp_dom_info(pred, tdi, tdi_list, used, n_blocks);
	}
}

/**
 * Walks Blocks along the control flow.  If recursion started with
 * End block misses blocks in endless loops.
 */
static void init_tmp_pdom_info(ir_node *block, tmp_dom_info *parent,
                               tmp_dom_info *tdi_list, int* used, int n_blocks,
                               int unreachable)
{
	if (Block_block_visited(block))
		return;
	mark_Block_block_visited(block);
	set_Block_postdom_pre_num(block, *used);

	assert(*used < n_blocks);
	tmp_dom_info *tdi = &tdi_list[*used];
	++(*used);

	tdi->block       = block;
	tdi->semi        = tdi;
	tdi->parent      = parent;
	tdi->label       = tdi;
	tdi->ancestor    = NULL;
	tdi->dom         = NULL;
	tdi->bucket      = NULL;
	tdi->unreachable = unreachable;

	/* Iterate */
	for (int i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		ir_node *pred = get_Block_cfgpred_block(block, i);
		if (pred == NULL)
			continue;
		init_tmp_pdom_info(pred, tdi, tdi_list, used, n_blocks, unreachable);
	}

	/* All remaining block keep-alives are edges to endless loops.
	 * Mark the following unvisited blocks as unreachable.
	 * Later, we will treat the keep-alive edges as normal control flow. */
	const ir_graph *irg = get_irn_irg(block);
	if (block == get_irg_end_block(irg)) {
		foreach_irn_in_r(get_irg_end(irg), i, pred) {
			if (is_Block(pred))
				init_tmp_pdom_info(pred, tdi, tdi_list, used, n_blocks, 1);
		}
	}
}

static void dom_compress(tmp_dom_info *v)
{
	assert(v->ancestor);
	if (v->ancestor->ancestor) {
		dom_compress(v->ancestor);
		if (v->ancestor->label->semi < v->label->semi) {
			v->label = v->ancestor->label;
		}
		v->ancestor = v->ancestor->ancestor;
	}
}

/**
 * if V is a root, return v, else return the vertex u, not being the
 * root, with minimum u->semi on the path from v to its root.
 */
inline static tmp_dom_info *dom_eval(tmp_dom_info *v)
{
	if (!v->ancestor)
		return v;
	dom_compress(v);
	return v->label;
}

/** make V W's ancestor */
inline static void dom_link(tmp_dom_info *v, tmp_dom_info *w)
{
	w->ancestor = v;
}

/**
 * Walker: count the number of blocks and clears the dominance info
 */
static void count_and_init_blocks_dom(ir_node *block, void *env)
{
	unsigned *n_blocks = (unsigned*)env;
	(*n_blocks)++;

	memset(get_dom_info(block), 0, sizeof(ir_dom_info));
	set_Block_idom(block, NULL);
	set_Block_dom_pre_num(block, -1);
	set_Block_dom_depth(block, -1);
}

void compute_doms(ir_graph *irg)
{
	assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));

	/* We need the out data structure. */
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS
	                         | IR_GRAPH_PROPERTY_NO_TUPLES);

	/* Count the number of blocks in the graph. */
	int n_blocks = 0;
	irg_block_walk_graph(irg, count_and_init_blocks_dom, NULL, &n_blocks);

	/* Memory for temporary information. */
	tmp_dom_info *tdi_list = XMALLOCN(tmp_dom_info, n_blocks);

	/* this with a standard walker as passing the parent to the sons isn't
	   simple. */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	int used = 0;
	init_tmp_dom_info(get_irg_start_block(irg), NULL, tdi_list, &used, n_blocks);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	/* If not all blocks are reachable from Start by out edges this assertion
	   fails. */
	assert(used <= n_blocks);
	n_blocks = used;

	for (int i = n_blocks; i-- > 1; ) {  /* Don't iterate the root, it's done. */
		tmp_dom_info  *w     = &tdi_list[i];
		const ir_node *block = w->block;

		/* Step 2 */
		for (int j = 0, arity = get_irn_arity(block); j < arity; j++) {
			const ir_node *pred       = get_Block_cfgpred(block, j);
			const ir_node *pred_block = get_nodes_block(pred);

			if (is_Bad(pred) || get_Block_dom_pre_num(pred_block) == -1)
				continue;    /* unreachable */

			const tmp_dom_info *u = dom_eval(&tdi_list[get_Block_dom_pre_num(pred_block)]);
			if (u->semi < w->semi)
				w->semi = u->semi;
		}

		/* handle keep-alives if we are at the end block */
		if (block == get_irg_end_block(irg)) {
			foreach_irn_in(get_irg_end(irg), j, pred) {
				if (!is_Block(pred) || get_Block_dom_pre_num(pred) == -1)
					continue;   /* unreachable */

				const tmp_dom_info *u = dom_eval(&tdi_list[get_Block_dom_pre_num(pred)]);
				if (u->semi < w->semi)
					w->semi = u->semi;
			}
		}

		/* Add w to w->semi's bucket.  w is in exactly one bucket, so
		   buckets can been implemented as linked lists. */
		w->bucket = w->semi->bucket;
		w->semi->bucket = w;

		dom_link(w->parent, w);

		/* Step 3 */
		while (w->parent->bucket) {
			tmp_dom_info *v = w->parent->bucket;
			/* remove v from w->parent->bucket */
			w->parent->bucket = v->bucket;
			v->bucket         = NULL;

			tmp_dom_info *u = dom_eval(v);
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
	for (int i = 1; i < n_blocks; i++) {
		tmp_dom_info *w = &tdi_list[i];
		if (w->dom == NULL)
			continue; /* control dead */

		if (w->dom != w->semi)
			w->dom = w->dom->dom;
		set_Block_idom(w->block, w->dom->block);

		/* blocks dominated by dead one's are still dead */
		int depth = get_Block_dom_depth(w->dom->block);
		if (depth > 0)
			++depth;
		set_Block_dom_depth(w->block, depth);
	}

	/* clean up */
	free(tdi_list);

	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	/* Do a walk over the tree and assign the tree pre orders. */
	unsigned tree_pre_order = 0;
	dom_tree_walk(get_irg_start_block(irg), assign_tree_dom_pre_order,
	              assign_tree_dom_pre_order_max, &tree_pre_order);
}

static void update_pdom_semi(tmp_dom_info *tdi_list, tmp_dom_info *w,
                             ir_node *succ_block)
{
	assert(is_Block(succ_block));
	const int           pre_num = get_Block_postdom_pre_num(succ_block);
	assert(pre_num != -1);
	const tmp_dom_info *u       = dom_eval(&tdi_list[pre_num]);
	if (u->semi < w->semi) {
		w->semi = u->semi;
	}
}

void compute_postdoms(ir_graph *irg)
{
	/* Update graph state */
	assert(!irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION));

	/* We need the out data structure. */
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS | IR_GRAPH_PROPERTY_NO_TUPLES);

	/* Count the number of blocks in the graph. */
	int n_blocks = 0;
	irg_block_walk_graph(irg, count_and_init_blocks_pdom, NULL, &n_blocks);

	/* memory for temporary information. */
	tmp_dom_info *tdi_list = XMALLOCN(tmp_dom_info, n_blocks);

	/* this with a standard walker as passing the parent to the sons isn't
	   simple. */
	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	int used = 0;
	ir_node *end_block = get_irg_end_block(irg);
	init_tmp_pdom_info(end_block, NULL, tdi_list, &used, n_blocks, 0);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	assert(used <= n_blocks);
	n_blocks = used;

	for (int i = n_blocks; i-- > 1; ) {  /* Don't iterate the root, it's done. */
		tmp_dom_info *w = &tdi_list[i];

		/* Step 2 */
		ir_node  *block       = w->block;
		bool      unreachable = w->unreachable;
		foreach_irn_out(block, j, succ) {
			if (get_irn_mode(succ) != mode_X || is_Bad(succ))
				continue;
			if (is_End(succ)) {
				if (unreachable && end_block != block)
					/* Handle keep-alive edges to unreachable
					 * blocks as normal control flow. */
					update_pdom_semi(tdi_list, w, end_block);
				continue;
			}
			foreach_irn_out(succ, k, succ_block) {
				update_pdom_semi(tdi_list, w, succ_block);
			}
		}

		/* Add w to w->semi's bucket.  w is in exactly one bucket, so
		   buckets can be implemented as linked lists. */
		w->bucket = w->semi->bucket;
		w->semi->bucket = w;

		dom_link(w->parent, w);

		/* Step 3 */
		while (w->parent->bucket) {
			tmp_dom_info *v = w->parent->bucket;
			/* remove v from w->parent->bucket */
			w->parent->bucket = v->bucket;
			v->bucket         = NULL;

			tmp_dom_info *u = dom_eval(v);
			if (u->semi < v->semi)
				v->dom = u;
			else
				v->dom = w->parent;
		}
	}
	/* Step 4 */
	tdi_list[0].dom = NULL;
	set_Block_ipostdom(tdi_list[0].block, NULL);
	set_Block_postdom_depth(tdi_list[0].block, 1);
	for (int i = 1; i < n_blocks; i++) {
		tmp_dom_info *w = &tdi_list[i];

		if (w->dom != w->semi)
			w->dom = w->dom->dom;
		set_Block_ipostdom(w->block, w->dom->block);
		set_Block_postdom_depth(w->block, get_Block_postdom_depth(w->dom->block) + 1);
	}

	/* clean up */
	free(tdi_list);

	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE);

	/* Do a walk over the tree and assign the tree pre orders. */
	unsigned tree_pre_order = 0;
	postdom_tree_walk(get_irg_end_block(irg), assign_tree_postdom_pre_order,
	                  assign_tree_postdom_pre_order_max, &tree_pre_order);
}
