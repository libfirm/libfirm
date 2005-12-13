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
	it->n    = 0;
	it->curr = 0;

	irg_block_walk_graph(ifg->env->irg, nodes_walker, NULL, iter);
	it->nodes = obstack_finish(&it->obst);
}

static ir_node *get_next_node(void *iter)
{
	nodes_iter_t *it = iter;
	ir_node *res     = NULL;

	if(it->curr < it->n)
		res = it->nodes[it->curr++];

	if(it->curr >= it-> n && it->nodes) {
		obstack_free(&it->obst, NULL);
		it->nodes = NULL;
	}

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

typedef struct _adj_iter_t {
	const be_chordal_env_t *env;
	const ir_node *irn;
	struct obstack obst;
	int degree;
	unsigned visited_nr;
	unsigned build_list : 1;

	int curr;
	ir_node **neighbours;
} adj_iter_t;

static void find_neighbour_walker(ir_node *block, void *data)
{
	adj_iter_t *it          = data;
	unsigned visited        = it->visited_nr;
	struct list_head *head  = get_block_border_head(it->env, block);

	border_t *b;
	int has_started = 0;

	foreach_border_head(head, b) {
		ir_node *irn = b->irn;

		if(irn == it->irn)
			has_started = b->is_def;

		/*
		 * If the def/use of the node is inside the live range
		 * of the node in question, they interfere.
		 * To avoid that a node is added twice, we record the
		 * visit in the visited number provided by core firm.
		 */
		else if(has_started && get_irn_visited(irn) < visited) {
			if(it->build_list)
				obstack_ptr_grow(&it->obst, irn);

			set_irn_visited(irn, visited);
			it->degree++;
		}
	}
}

static void find_neighbours(const ifg_std_t *ifg, adj_iter_t *it, const ir_node *irn, int build_list)
{
	if(build_list)
		obstack_init(&it->obst);

	it->env        = ifg->env;
	it->irn        = irn;
	it->degree     = 0;
	it->visited_nr = get_irg_visited(ifg->env->irg) + 1;
	it->build_list = build_list;
	it->neighbours = NULL;
	it->curr       = 0;

	set_irg_visited(ifg->env->irg, it->visited_nr);
	dom_tree_walk(get_nodes_block(irn), find_neighbour_walker, NULL, it);

	if(build_list)
		it->neighbours = obstack_finish(&it->obst);
}

static ir_node *get_next_neighbour(adj_iter_t *it) {
	ir_node *res = NULL;

	if(it->curr < it->degree)
		res = it->neighbours[it->curr++];

	if(it->curr >= it->degree && it->neighbours) {
		obstack_free(&it->obst, NULL);
		it->neighbours = NULL;
	}

	return res;
}

static ir_node *ifg_std_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	find_neighbours(self, iter, irn, 1);
	return get_next_neighbour(iter);
}

static ir_node *ifg_std_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static int ifg_std_degree(const void *self, const ir_node *irn)
{
	adj_iter_t it;
	find_neighbours(self, &it, irn, 0);
	return it.degree;
}

static const be_ifg_impl_t ifg_std_impl = {
	MAX(sizeof(adj_iter_t), sizeof(nodes_iter_t)),
	ifg_std_free,
	ifg_std_connected,
	ifg_std_neighbours_begin,
	ifg_std_neighbours_next,
	ifg_std_nodes_begin,
	ifg_std_nodes_next,
	ifg_std_degree
};

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env)
{
	ifg_std_t *ifg = malloc(sizeof(*ifg));

	ifg->impl = &ifg_std_impl;
	ifg->env  = env;

	return (be_ifg_t *) ifg;
}
