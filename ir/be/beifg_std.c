/**
 * @file   beifg_std.c
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "list.h"

#include "irnode_t.h"
#include "irnodeset.h"
#include "irgraph_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "belive_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"

#define MAX(x, y) ((x) > (y) ? (x) : (y))

typedef struct _ifg_std_t ifg_std_t;

struct _ifg_std_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
};

static void ifg_std_free(void *self)
{
	free(self);
}

static int ifg_std_connected(const void *self, const ir_node *a, const ir_node *b)
{
	const ifg_std_t *ifg = self;
	be_lv_t *lv = ifg->env->birg->lv;
	return values_interfere(lv, a, b);
}

typedef struct _nodes_iter_t {
	const be_chordal_env_t *env;
	struct obstack obst;
	int n;
	int curr;
	ir_node **nodes;
} nodes_iter_t;

static void nodes_walker(ir_node *bl, void *data)
{
	nodes_iter_t *it = data;
	struct list_head *head = get_block_border_head(it->env, bl);

	border_t *b;

	foreach_border_head(head, b) {
		if(b->is_def && b->is_real) {
			obstack_ptr_grow(&it->obst, b->irn);
			it->n++;
		}
	}
}

static void find_nodes(const void *self, void *iter) {
	const ifg_std_t *ifg = self;
	nodes_iter_t *it = iter;

	obstack_init(&it->obst);
	it->n     = 0;
	it->curr  = 0;
	it->env   = ifg->env;

	irg_block_walk_graph(ifg->env->irg, nodes_walker, NULL, iter);
	obstack_ptr_grow(&it->obst, NULL);
	it->nodes = obstack_finish(&it->obst);
}

static INLINE void node_break(nodes_iter_t *it, int force)
{
	if((it->curr >= it->n || force) && it->nodes) {
		obstack_free(&it->obst, NULL);
		it->nodes = NULL;
	}
}

static ir_node *get_next_node(void *iter)
{
	nodes_iter_t *it = iter;
	ir_node *res     = NULL;

	if(it->curr < it->n)
		res = it->nodes[it->curr++];

	node_break(it, 0);

	return res;
}

static ir_node *ifg_std_nodes_begin(const void *self, void *iter)
{
	find_nodes(self, iter);
	return get_next_node(iter);
}

static ir_node *ifg_std_nodes_next(const void *self, void *iter)
{
	return get_next_node(iter);
}

static void ifg_std_nodes_break(const void *self, void *iter)
{
	node_break(iter, 1);
}

typedef struct _adj_iter_t {
	const be_chordal_env_t *env;
	const ir_node        *irn;
	int                   valid;
	ir_nodeset_t          neighbours;
	ir_nodeset_iterator_t iter;
} adj_iter_t;

static void find_neighbour_walker(ir_node *block, void *data)
{
	adj_iter_t *it          = data;
	struct list_head *head  = get_block_border_head(it->env, block);

	border_t *b;
	int has_started = 0;

	if(!be_is_live_in(it->env->birg->lv, block, it->irn) && block != get_nodes_block(it->irn))
		return;

	foreach_border_head(head, b) {
		ir_node *irn = b->irn;

		if(irn == it->irn) {
			if(b->is_def)
				has_started = 1;
			else
				break; /* if we reached the end of the node's lifetime we can safely break */
		}
		else if(b->is_def) {
			/* if any other node than the one in question starts living, add it to the set */
			ir_nodeset_insert(&it->neighbours, irn);
		}
		else if(!has_started) {
			/* we only delete, if the live range in question has not yet started */
			ir_nodeset_remove(&it->neighbours, irn);
		}

	}
}

static void find_neighbours(const ifg_std_t *ifg, adj_iter_t *it, const ir_node *irn)
{
	it->env         = ifg->env;
	it->irn         = irn;
	it->valid       = 1;
	ir_nodeset_init(&it->neighbours);

	dom_tree_walk(get_nodes_block(irn), find_neighbour_walker, NULL, it);

	ir_nodeset_iterator_init(&it->iter, &it->neighbours);
}

static INLINE void neighbours_break(adj_iter_t *it, int force)
{
	assert(it->valid == 1);
	ir_nodeset_destroy(&it->neighbours);
	it->valid = 0;
}

static ir_node *get_next_neighbour(adj_iter_t *it) {
	ir_node *res = ir_nodeset_iterator_next(&it->iter);

	if (res == NULL) {
		ir_nodeset_destroy(&it->neighbours);
	}
	return res;
}

static ir_node *ifg_std_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	adj_iter_t *it = iter;
	find_neighbours(self, iter, irn);
	return ir_nodeset_iterator_next(&it->iter);
}

static ir_node *ifg_std_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_std_neighbours_break(const void *self, void *iter)
{
	neighbours_break(iter, 1);
}

typedef struct _cliques_iter_t {
	struct obstack ob;
	const be_chordal_env_t *cenv;
	ir_node **buf;
	ir_node **blocks;
	int n_blocks, blk;
	struct list_head *bor;
	pset *living;
} cliques_iter_t;

static INLINE void free_clique_iter(cliques_iter_t *it) {
	it->n_blocks = -1;
	obstack_free(&it->ob, NULL);
	del_pset(it->living);
}

static void get_blocks_dom_order(ir_node *blk, void *env) {
	cliques_iter_t *it = env;
	obstack_ptr_grow(&it->ob, blk);
}

#define pset_foreach(pset, irn)  for(irn=pset_first(pset); irn; irn=pset_next(pset))


/**
 * NOTE: Be careful when changing this function!
 *       First understand the control flow of consecutive calls.
 */
static INLINE int get_next_clique(cliques_iter_t *it) {

	/* continue in the block we left the last time */
	for (; it->blk < it->n_blocks; it->blk++) {
		int output_on_shrink = 0;
		struct list_head *head = get_block_border_head(it->cenv, it->blocks[it->blk]);

		/* on entry to a new block set the first border ... */
		if (!it->bor)
			it->bor = head->prev;

		/* ... otherwise continue with the border we left the last time */
		for (; it->bor != head; it->bor = it->bor->prev) {
			border_t *b = list_entry(it->bor, border_t, list);

			/* if its a definition irn starts living */
			if (b->is_def) {
				pset_insert_ptr(it->living, b->irn);
				if (b->is_real)
					output_on_shrink = 1;
			} else

			/* if its the last usage the irn dies */
			{
				/* before shrinking the set, return the current maximal clique */
				if (output_on_shrink) {
					int count = 0;
					ir_node *irn;

					/* fill the output buffer */
					pset_foreach(it->living, irn)
						it->buf[count++] = irn;

					assert(count > 0 && "We have a 'last usage', so there must be sth. in it->living");

					return count;
				}

				pset_remove_ptr(it->living, b->irn);
			}
		}

		it->bor = NULL;
		assert(0 == pset_count(it->living) && "Something has survived! (At the end of the block it->living must be empty)");
	}

	if (it->n_blocks != -1)
		free_clique_iter(it);

	return -1;
}

static int ifg_std_cliques_begin(const void *self, void *iter, ir_node **buf)
{
	const ifg_std_t *ifg = self;
	cliques_iter_t *it = iter;
	ir_node *start_bl = get_irg_start_block(ifg->env->irg);

	obstack_init(&it->ob);
	dom_tree_walk(start_bl, get_blocks_dom_order, NULL, it);

	it->cenv     = ifg->env;
	it->buf      = buf;
	it->n_blocks = obstack_object_size(&it->ob) / sizeof(void *);
	it->blocks   = obstack_finish(&it->ob);
	it->blk      = 0;
	it->bor      = NULL;
	it->living   = pset_new_ptr(2 * arch_register_class_n_regs(it->cenv->cls));

	return get_next_clique(it);
}

static int ifg_std_cliques_next(const void *self, void *iter)
{
	return get_next_clique(iter);
}

static void ifg_std_cliques_break(const void *self, void *iter)
{
	free_clique_iter(iter);
}


static int ifg_std_degree(const void *self, const ir_node *irn)
{
	adj_iter_t it;
	int degree;
	find_neighbours(self, &it, irn);
	degree = ir_nodeset_size(&it.neighbours);
	neighbours_break(&it, 1);
	return degree;
}

static const be_ifg_impl_t ifg_std_impl = {
	sizeof(nodes_iter_t),
	sizeof(adj_iter_t),
	sizeof(cliques_iter_t),

	ifg_std_free,
	ifg_std_connected,
	ifg_std_neighbours_begin,
	ifg_std_neighbours_next,
	ifg_std_neighbours_break,
	ifg_std_nodes_begin,
	ifg_std_nodes_next,
	ifg_std_nodes_break,
	ifg_std_cliques_begin,
	ifg_std_cliques_next,
	ifg_std_cliques_break,
	ifg_std_degree
};

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env)
{
	ifg_std_t *ifg = xmalloc(sizeof(*ifg));

	ifg->impl = &ifg_std_impl;
	ifg->env  = env;

	return (be_ifg_t *) ifg;
}
