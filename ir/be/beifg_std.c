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
	return values_interfere(a, b);
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
	const ir_node *irn;
	int reached_end;
	pset *neighbours;
} adj_iter_t;

static void find_neighbour_walker(ir_node *block, void *data)
{
	adj_iter_t *it          = data;
	struct list_head *head  = get_block_border_head(it->env, block);

	border_t *b;
	int has_started = 0;

	if(!is_live_in(block, it->irn) && block != get_nodes_block(it->irn))
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
			pset_insert_ptr(it->neighbours, irn);
		}
		else if(!has_started) {
			/* we only delete, if the live range in question has not yet started */
			pset_remove_ptr(it->neighbours, irn);
		}

	}
}

static void find_neighbours(const ifg_std_t *ifg, adj_iter_t *it, const ir_node *irn)
{
	it->env         = ifg->env;
	it->irn         = irn;
	it->neighbours  = pset_new_ptr(16);
	it->reached_end = 0;

	dom_tree_walk(get_nodes_block(irn), find_neighbour_walker, NULL, it);
}

static INLINE void neighbours_break(adj_iter_t *it, int force)
{
	if((it->reached_end || force) && it->neighbours) {
		del_pset(it->neighbours);
		it->neighbours = NULL;
	}
}

static ir_node *get_next_neighbour(adj_iter_t *it) {
	ir_node *res = pset_next(it->neighbours);

	it->reached_end = res == NULL;
	neighbours_break(it, 0);

	return res;
}

static ir_node *ifg_std_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	adj_iter_t *it = iter;
	find_neighbours(self, iter, irn);
	return pset_first(it->neighbours);
}

static ir_node *ifg_std_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_std_neighbours_break(const void *self, void *iter)
{
	neighbours_break(iter, 1);
}

static int ifg_std_degree(const void *self, const ir_node *irn)
{
	adj_iter_t it;
	int degree;
	find_neighbours(self, &it, irn);
	degree = pset_count(it.neighbours);
	neighbours_break(&it, 1);
	return degree;
}

static const be_ifg_impl_t ifg_std_impl = {
	sizeof(nodes_iter_t),
	sizeof(adj_iter_t),

	ifg_std_free,
	ifg_std_connected,
	ifg_std_neighbours_begin,
	ifg_std_neighbours_next,
	ifg_std_neighbours_break,
	ifg_std_nodes_begin,
	ifg_std_nodes_next,
	ifg_std_nodes_break,
	ifg_std_degree
};

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env)
{
	ifg_std_t *ifg = malloc(sizeof(*ifg));

	ifg->impl = &ifg_std_impl;
	ifg->env  = env;

	return (be_ifg_t *) ifg;
}
