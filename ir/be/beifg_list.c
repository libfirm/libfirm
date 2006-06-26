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

#include "belive_t.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "benodesets.h"

#include "be_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"

#define MAX(x, y) ((x) > (y) ? (x) : (y))

typedef struct _adj_head_t adj_head_t;

typedef struct _ifg_list_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
	struct obstack obst;
	adj_head_t **adj_heads;
} ifg_list_t;

typedef struct _list_element_t {
	struct list_head list;
	ir_node *irn;
} list_element_t;

struct _adj_head_t {
	struct list_head list;
	struct list_head *next_adj_head;
	ir_node *irn;
	int degree;
};

typedef struct _adj_iter_t {
	ifg_list_t *ifg;
	ir_node *irn;
	adj_head_t *curr_adj_head;
	list_element_t *curr_list_element;
	int curr_node_idx;
} adj_iter_t;

/* PRIVATE FUNCTIONS */

static adj_head_t *get_or_set_adj_head(ifg_list_t *ifg, ir_node *irn)
{
	adj_head_t *adj_head = NULL;

	adj_head = ifg->adj_heads[irn->node_idx];
	if(!adj_head){
		adj_head = obstack_alloc(&ifg->obst, sizeof(*adj_head));
		adj_head->irn = irn;
		adj_head->degree = 0;
		INIT_LIST_HEAD(&adj_head->list);
		ifg->adj_heads[irn->node_idx] = adj_head;
	}

	return adj_head;
}

static list_element_t *get_new_list_element(ifg_list_t *ifg, ir_node *irn)
{
	list_element_t *element;

	element = obstack_alloc(&ifg->obst, sizeof(*element));
	element->irn = irn;
	INIT_LIST_HEAD(&element->list);

	return element;
}

static void add_edge(ifg_list_t *ifg, ir_node *a, ir_node *b) /* add node B as a neighbour to A and vice versa */
{
	adj_head_t *adj_head;
	list_element_t *new_element;
	list_element_t *element;
	int is_element = 0;

	adj_head = get_or_set_adj_head(ifg, a);
	list_for_each_entry(list_element_t, element, &adj_head->list, list){
		if(element->irn == b){
			is_element = 1;
			break;
		}
	}
	if (!is_element){ /* if B is not yet a neighbour of A add the node to A's neighbours */
		new_element = get_new_list_element(ifg, b);
		adj_head->degree++;
		list_add(&new_element->list, &adj_head->list) ;
	}

	adj_head = get_or_set_adj_head(ifg, b);
	is_element = 0;
	list_for_each_entry(list_element_t, element, &adj_head->list, list){
		if(element->irn == a){
			is_element = 1;
		}
	}
	if (!is_element){ /* if A is not yet a neighbour of B add the node to B's neighbours */
		new_element = get_new_list_element(ifg, a);
		adj_head->degree++;
		list_add(&new_element->list, &adj_head->list);
	}
}

static void find_nodes(const ifg_list_t *ifg, adj_iter_t *it)
{
	adj_head_t *adj_head = NULL;
	int i = -1;

	while(adj_head == NULL) /* find first adj_head */
	{
		i++;
		adj_head = ifg->adj_heads[i];
	}

	it->ifg = ifg;
	it->ifg->adj_heads = ifg->adj_heads;
	it->curr_adj_head = adj_head;
	it->curr_node_idx = i;

	return;
}

static ir_node *get_next_node(adj_iter_t *it)
{
	adj_head_t *adj_head;
	ir_node *irn;
	unsigned int node_idx = it->curr_node_idx;

	if (it->curr_adj_head == NULL)
		return NULL;
	else
	{
		adj_head = it->curr_adj_head;
		irn = adj_head->irn; /* return the last found node */

		it->irn = irn;

		adj_head = NULL;
		while (adj_head == NULL && node_idx < it->ifg->env->irg->last_node_idx - 1)
		{
			node_idx++;
			adj_head = it->ifg->adj_heads[node_idx];
		}

		it->curr_node_idx = node_idx;
		it->curr_adj_head = adj_head;

		if (!adj_head) /* was last node */
		{
			it->curr_adj_head = NULL;
			it->curr_node_idx = -1;
		}
	}
  return irn;
}

static void find_neighbour_walker(ir_node *bl, void *data) /* find all adjacent nodes in one given block */
{
	ifg_list_t *ifg = data;
	struct list_head *head  = get_block_border_head(ifg->env, bl);
	border_t *b;
	int delete_nodeset = 0;
	nodeset *live = new_nodeset(ifg->env->cls->n_regs);
	ir_node *live_irn;
	adj_head_t *adj_head;

	assert(is_Block(bl) && "There is no block to work on.");

	foreach_border_head(head, b) /* follow the borders of the block */
	{
		//if(get_irn_node_nr(b->irn) == 2049)
		//	printf("Hallo");
		if (b->is_def) {
			adj_head = get_or_set_adj_head(ifg, b->irn);
			nodeset_insert(live, b->irn);
//			if (b->is_real) /* b is a new node */ {
				foreach_nodeset(live, live_irn) {
					if (b->irn != live_irn) /* add b as a neighbour to each previous found adjacent node */
						add_edge(ifg, b->irn, live_irn);
//				}
			}
		}

		else {
			if (nodeset_find(live,b->irn)) /* b is used, deleting... */
				nodeset_remove(live, b->irn);
		}
	}
	if (delete_nodeset)
		del_nodeset(live);
}


static void find_first_neighbour(const ifg_list_t *ifg, adj_iter_t *it, const ir_node *irn)
{
	list_element_t *element;
	adj_head_t *adj_head = ifg->adj_heads[irn->node_idx];

	assert(adj_head && "There is no entry for this node.");

	if (adj_head->list.next == &adj_head->list)
	{
		/* element has no neighbours */
		it->irn = NULL;
		it->curr_adj_head = adj_head;
		it->curr_list_element = NULL;
		return;
	}

	element = list_entry(adj_head->list.next, list_element_t, list); /* get the first neighbour of the given node irn */

	it->curr_list_element = element;
	it->curr_adj_head = adj_head;
	it->irn = element->irn;

	return;
}

static ir_node *get_next_neighbour(adj_iter_t *it)
{
	ir_node *res;
	list_element_t *element;
	adj_head_t *adj_head = it->curr_adj_head;

	if(it->irn != NULL) /* return the previous found neighbour */
		res = it->irn;
	else
	{
		if(it->curr_list_element != NULL)
		{
			res = it->curr_list_element->irn; /* was last element */
			it ->curr_list_element = NULL;
			return res;
		}
		else
			return NULL;
	}

	element = list_entry(it->curr_list_element->list.next, list_element_t, list); /* find the next neighbour */
	if (element->list.next == &it->curr_list_element->list) /* single element (only one neighbour) */
	{
		it->irn = NULL;
		it->curr_list_element = NULL;
		return res;
	}

	if(element->list.next == &adj_head->list) /* reaching end of list */
		it->irn = NULL;
	else
		it->irn = element->irn;

	it->curr_list_element = element;

	return res;
}


/* PUBLIC FUNCTIONS */
static void ifg_list_free(void *self)
{
	ifg_list_t *ifg = self;
	obstack_free(&ifg->obst, NULL);
	free(ifg->adj_heads);
	free(self);
}

static int ifg_list_connected(const void *self, const ir_node *a, const ir_node *b)
{
	const ifg_list_t *ifg = self;
	ir_node *node_a = (void *) a;
	ir_node *node_b = (void *) b;
	adj_head_t *adj_head;
	list_element_t *element;
	int is_element = 0;

	adj_head = ifg->adj_heads[node_a->node_idx];
	assert(adj_head && "There is no entry for the first node.");

	//if (adj_head == NULL) /* node is not in this ifg */
	//	return 1;

	list_for_each_entry(list_element_t, element, &adj_head->list, list)
		if(element->irn == b)
			is_element = 1;

	adj_head = ifg->adj_heads[node_b->node_idx];
	assert(adj_head && "There is no entry for the second node");

	//if (adj_head == NULL) /* node is not in this ifg */
	//	return 1;

	list_for_each_entry(list_element_t, element, &adj_head->list, list)
		if(element->irn == a)
			is_element = 1;

	return is_element;
}

static ir_node *ifg_list_neighbours_begin(const void *self, adj_iter_t *it, const ir_node *irn)
{
	find_first_neighbour(self, it, irn);
	return get_next_neighbour(it);
}

static ir_node *ifg_list_neighbours_next(const void *self, adj_iter_t *it)
{
	return get_next_neighbour(it);
}

static void ifg_list_neighbours_break(const void *self, adj_iter_t *it)
{
}

static ir_node *ifg_list_nodes_begin(const void *self, adj_iter_t *it)
{
	find_nodes(self, it);
	return get_next_node(it);
}

static ir_node *ifg_list_nodes_next(const void *self, adj_iter_t *it)
{
	return get_next_node(it);
}

static void ifg_list_nodes_break(const void *self, adj_iter_t *it)
{
}

static int ifg_list_degree(const void *self, const ir_node *irn)
{
	const ifg_list_t *ifg = self;
	adj_head_t *adj_head;

	adj_head = ifg->adj_heads[irn->node_idx];
	assert(adj_head && "There is no entry for this node.");

	return adj_head->degree;
}

static const be_ifg_impl_t ifg_list_impl = {
	sizeof(adj_iter_t),
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
	ifg_list_t *ifg 				= xmalloc(sizeof(*ifg));
	adj_head_t **adj_heads_array	= xmalloc(env->irg->last_node_idx * sizeof(adj_heads_array[0]));
	ifg->impl     					= &ifg_list_impl;
	ifg->env						= env;

	memset(adj_heads_array, 0, env->irg->last_node_idx * sizeof(adj_heads_array[0]));
	ifg->adj_heads	 				= adj_heads_array;

	obstack_init(&ifg->obst);
	dom_tree_walk_irg(env->irg, find_neighbour_walker, NULL, ifg);

	obstack_finish(&ifg->obst);

	return (be_ifg_t *) ifg;
}
