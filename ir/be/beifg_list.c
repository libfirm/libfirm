/**
 * @file   beifg_list.c
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

#include "benodesets.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"


typedef struct _adj_head_t adj_head_t;

typedef struct _ifg_list_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
	struct obstack obst;
	adj_head_t **adj_heads;
} ifg_list_t;

typedef struct _adj_element_t adj_element_t;

struct _adj_element_t {
	adj_element_t *next_adj_element;
	ir_node *neighbour;
};

struct _adj_head_t {
	ir_node *irn; /* the node you search neighbours for */
	adj_element_t *first_adj_element;
	int degree;
};

typedef struct _nodes_iter_t {
	const ifg_list_t *ifg;
	unsigned int curr_node_idx;
} nodes_iter_t;

typedef struct _adj_iter_t {
	const ifg_list_t *ifg;
	adj_element_t *curr_adj_element;
} adj_iter_t;

/* PRIVATE FUNCTIONS */

static void create_node (ifg_list_t *ifg, ir_node *irn) /* add node to the array of all nodes in this ifg implementation, if the node isn't already in the ifg */
{
	adj_head_t *adj_head = NULL;

	adj_head = ifg->adj_heads[irn->node_idx];
	if (!adj_head)
	{
		adj_head = obstack_alloc(&ifg->obst, sizeof(*adj_head));
		adj_head->irn = irn;
		adj_head->first_adj_element = NULL;
		adj_head->degree = 0;
		ifg->adj_heads[irn->node_idx] = adj_head;
	}
}

static adj_element_t *create_adj_element(ifg_list_t *ifg, ir_node *irn)
{
	adj_element_t *element = NULL;

	element = obstack_alloc(&ifg->obst, sizeof(*element));
	element->next_adj_element = NULL;
	element->neighbour = irn;

	return element;
}

static void add_edge(ifg_list_t *ifg, ir_node *node_a, ir_node *node_b) /* write the information about the edge between a and b */
{
	adj_head_t *adj_head = NULL;
	adj_element_t *curr_element = NULL;
	adj_element_t *new_element = NULL;

	adj_head = ifg->adj_heads[node_a->node_idx]; /* find the neighbours list of a */

	assert (adj_head && "There is no entry for node a");
	curr_element = adj_head->first_adj_element;

	if (curr_element)
	{
		while (curr_element->neighbour != node_b && curr_element->next_adj_element)
		{
			curr_element = curr_element->next_adj_element;
		}

		if (curr_element->neighbour != node_b && curr_element->next_adj_element == NULL)
		{
			adj_head->degree++;
			new_element = create_adj_element(ifg, node_b); /* if b isn't in list, add b */
			curr_element->next_adj_element = new_element;
		}
	}
	else
	{
		adj_head->degree++;
		new_element = create_adj_element(ifg, node_b); /* a has no neighbours, add b as the first one*/
		adj_head->first_adj_element = new_element;
	}

	/* do the same vice versa */
	adj_head = ifg->adj_heads[node_b->node_idx];

	assert (adj_head && "There is no entry for node a");
	curr_element = adj_head->first_adj_element;

	if (curr_element)
	{
		while (curr_element->neighbour != node_a && curr_element->next_adj_element)
		{
			curr_element = curr_element->next_adj_element;
		}

		if (curr_element->neighbour != node_a && curr_element->next_adj_element == NULL)
		{
			adj_head->degree++;
			new_element = create_adj_element(ifg, node_a);
			curr_element->next_adj_element = new_element;
		}
	}
	else
	{
		adj_head->degree++;
		new_element = create_adj_element(ifg, node_a);
		adj_head->first_adj_element = new_element;
	}
}

static void find_neighbour_walker(ir_node *bl, void *data) /* find all adjacent nodes in the irg */
{
	ifg_list_t *ifg = data;
	struct list_head *head  = get_block_border_head(ifg->env, bl);

	nodeset *live = new_nodeset(ifg->env->cls->n_regs);
	ir_node *live_irn = NULL;
	border_t *b = NULL;

	assert (is_Block(bl) && "There is no block to work on");

	foreach_border_head(head, b) /* follow the borders of each block */
	{
		if (b->is_def)
		{
			create_node(ifg, b->irn); /* add the node to the array of all nodes of this ifg implementation */
			nodeset_insert(live, b->irn);
			if (b->is_real) /* this is a new node */
			{
				foreach_nodeset(live, live_irn)
				{
					if (b->irn != live_irn) /* add a as a neighbour to b and vice versa */
						add_edge(ifg, b->irn, live_irn);
				}
			}
		}
		else /* b->irn is now dead */
		{
			if (nodeset_find(live, b->irn))
				nodeset_remove(live, b->irn);
		}
	}

	if (live)
		del_nodeset(live);
}

static ir_node *get_first_node(const ifg_list_t *ifg, nodes_iter_t *it)
{
	ir_node *res = NULL;
	adj_head_t *adj_head= NULL;
	int curr_idx = -1;

	it->ifg = ifg;
	it->curr_node_idx = 0;

	while (adj_head == NULL)
	{
		curr_idx++;
		adj_head = ifg->adj_heads[curr_idx];
	}

	if (adj_head == NULL) /* there are no nodes in this ifg */
		return NULL;
	else
	{
		res = adj_head->irn;
		it->curr_node_idx = curr_idx;
	}

	return res;
}

static ir_node *get_next_node(nodes_iter_t *it)
{
	const ifg_list_t *ifg = it->ifg;
	ir_node *res = NULL;
	adj_head_t *adj_head= NULL;
	unsigned int curr_idx = it->curr_node_idx;

	while (adj_head == NULL && curr_idx < it->ifg->env->irg->last_node_idx - 1)
	{
		curr_idx++;
		adj_head = ifg->adj_heads[curr_idx];
	}

	if (adj_head == NULL) /* there are no more nodes in this ifg */
		return NULL;
	else
	{
		res = adj_head->irn;
		it->curr_node_idx = curr_idx;
	}

	return res;
}

static ir_node *get_first_neighbour(const ifg_list_t *ifg, adj_iter_t *it, const ir_node *curr_irn)
{
	ir_node *res = NULL;
	adj_head_t *adj_head = NULL;

	adj_head = ifg->adj_heads[curr_irn->node_idx];
	assert (adj_head && "There is no entry for this node");

	it->curr_adj_element = NULL;
	it->ifg = ifg;

	if (adj_head->first_adj_element) /* return first neighbour */
	{
		res = adj_head->first_adj_element->neighbour;
		it->curr_adj_element = adj_head->first_adj_element;
	}
	else /* node has no neighbours */
		return NULL;

	return res;
}

static ir_node *get_next_neighbour(adj_iter_t *it)
{
	ir_node *res = NULL;
	adj_element_t *element = it->curr_adj_element;

	if (element->next_adj_element) /* return next neighbour */
	{
		res = element->next_adj_element->neighbour;
		it->curr_adj_element = element->next_adj_element;
	}
	else /* was last neighbour */
		return NULL;

	return res;
}

/* PUBLIC FUNCTIONS */

static void ifg_list_free(void *self)
{
	ifg_list_t *ifg = self;
	obstack_free(&ifg->obst, NULL);
	free(ifg->adj_heads);
	free(ifg);
}

static int ifg_list_connected(const void *self, const ir_node *a, const ir_node *b)
{
	const ifg_list_t *ifg = self;
	int res = -1;
	adj_head_t *adj_head = NULL;
	adj_element_t *curr_element = NULL;

	/* first try: find b in the neigbours of a */
	adj_head = ifg->adj_heads[a->node_idx];

	assert(adj_head && "There is no entry for the node a");
	curr_element = adj_head->first_adj_element;

	if(curr_element)
	{
		while (curr_element->neighbour != b && curr_element->next_adj_element)
		{
			curr_element = curr_element->next_adj_element;
		}
		if (curr_element->neighbour == b)
			return 1;
		else
			res = 0;
	}
	else /* node a has no neighbours */
		res = 0;

	/* second try, this should not be necessary... only to check the solution */
	adj_head = ifg->adj_heads[b->node_idx];

	assert(adj_head && "There is no entry for the node b");
	curr_element = adj_head->first_adj_element;

	if(curr_element)
	{
		while (curr_element->neighbour != a && curr_element->next_adj_element)
		{
			curr_element = curr_element->next_adj_element;
		}
		if (curr_element->neighbour == a)
		{
			assert ("Found the neighbour only in the second try.");
			return 1;
		}
		else
			res = 0;
	}
	else /* node b has no neighbours */
		res = 0;

	return res;
}

static ir_node *ifg_list_nodes_begin(const void *self, void *iter)
{
	nodes_iter_t *it = iter;
	return get_first_node(self, it);
}

static ir_node *ifg_list_nodes_next(const void *self, void *iter)
{
	return get_next_node(iter);
}

static void ifg_list_nodes_break(const void *self, void *iter)
{
	nodes_iter_t *it = iter;
	it->curr_node_idx = 0;
	it->ifg = NULL;
}

static ir_node *ifg_list_neighbours_begin(const void *self, void *iter,const ir_node *irn)
{
	adj_iter_t *it = iter;
	return get_first_neighbour(self, it, irn);
}

static ir_node *ifg_list_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_list_neighbours_break(const void *self, void *iter)
{
	adj_iter_t *it= iter;
	it->curr_adj_element = NULL;
	it->ifg = NULL;
}

static int ifg_list_degree(const void *self, const ir_node *irn)
{
	const ifg_list_t *ifg = self;
	adj_head_t *adj_head = NULL;

	adj_head = ifg->adj_heads[irn->node_idx];

	assert (adj_head && "There is no entry for this node");

	return adj_head->degree;
}

static const be_ifg_impl_t ifg_list_impl = {
	sizeof(nodes_iter_t),
	sizeof(adj_iter_t),
	0,
	ifg_list_free,
	ifg_list_connected,
	ifg_list_neighbours_begin,
	ifg_list_neighbours_next,
	ifg_list_neighbours_break,
	ifg_list_nodes_begin,
	ifg_list_nodes_next,
	ifg_list_nodes_break,
	NULL,
	NULL,
	NULL,
	ifg_list_degree
};

be_ifg_t *be_ifg_list_new(const be_chordal_env_t *env)
{
	ifg_list_t *ifg = xmalloc(sizeof(*ifg));
	adj_head_t **adj_heads_array = xmalloc(env->irg->last_node_idx * sizeof(adj_heads_array[0]));

	ifg->impl = &ifg_list_impl;
	ifg->env  = env;

	memset(adj_heads_array, 0, env->irg->last_node_idx * sizeof(adj_heads_array[0]));
	ifg->adj_heads = adj_heads_array;

	obstack_init(&ifg->obst);
	dom_tree_walk_irg(env->irg, find_neighbour_walker, NULL, ifg);
	obstack_finish(&ifg->obst);

	return (be_ifg_t *) ifg;
}
