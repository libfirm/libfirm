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
 * @brief       Clique calculation for chordal ifg.
 * @author      Sebastian Hack
 * @date        18.11.2005
 * @version     $Id$
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
#include "irbitset.h"

#include "bearch_t.h"
#include "be_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"
#include "benodesets.h"

typedef struct _cli_head_t {
	struct list_head list;
	struct _cli_head_t *next_cli_head;
	ir_node *min;
	ir_node *max;
} cli_head_t;

typedef struct _ifg_clique_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
	cli_head_t *cli_root;
	struct obstack obst;
	cli_head_t *curr_cli_head;
} ifg_clique_t;

typedef struct _cli_element_t {
	struct list_head list;
	ir_node *irn;
} cli_element_t;

typedef struct _cli_iter_t {
	const ifg_clique_t *ifg;
	cli_head_t *curr_cli_head;
	cli_element_t *curr_cli_element;
	const ir_node *curr_irn;
	bitset_t *visited_neighbours;
	bitset_t *visited_nodes;
} cli_iter_t;

/* PRIVATE FUNCTIONS */
static cli_head_t *get_new_cli_head(ifg_clique_t *ifg)
{
	cli_head_t *cli_head;
	cli_head_t *new_cli_head;

	if (ifg->cli_root == NULL)
	{
		new_cli_head = obstack_alloc(&ifg->obst, sizeof(*new_cli_head));
		INIT_LIST_HEAD(&new_cli_head->list);
		ifg->cli_root = new_cli_head;
	}
	else
	{
		cli_head = ifg->cli_root;
		while(!(cli_head->next_cli_head == NULL))
		{
			cli_head = cli_head->next_cli_head;
		}
		new_cli_head = obstack_alloc(&ifg->obst, sizeof(*new_cli_head));
		INIT_LIST_HEAD(&new_cli_head->list);
		cli_head->next_cli_head = new_cli_head;
	}

	new_cli_head->min = NULL;
	new_cli_head->max = NULL;
	new_cli_head->next_cli_head = NULL;
	ifg->curr_cli_head = new_cli_head;

	return new_cli_head;
}

static cli_element_t *get_new_cli_element(ifg_clique_t *ifg)
{
	cli_element_t *cli_element;

	cli_element = obstack_alloc(&ifg->obst, sizeof(*cli_element));
	INIT_LIST_HEAD(&cli_element->list);

	return cli_element;
}

static void write_clique(nodeset *live_set, ifg_clique_t *ifg)
{
	ir_node *live_irn;
	ir_node *test_node;
	int test_int = 0;
	ir_node *max_node = NULL;
	ir_node *min_node = NULL;
	cli_element_t *new_element = NULL;
	cli_element_t *element = NULL;
	cli_head_t *cli_head = get_new_cli_head(ifg);
	int is_element = 0;

	foreach_nodeset(live_set, live_irn)
	{
		/* test if node is max or min dominator*/
		test_node = live_irn;
		if (max_node == NULL)
		{
			max_node = test_node;
			min_node = test_node;
		}
		else
		{
			test_int = value_dominates(test_node, max_node);
			if (test_int == 1)
			{
				max_node = test_node;
			}
			test_int = value_dominates(min_node, test_node);
			if (test_int == 1)
			{
				min_node = test_node;
			}
		}

		list_for_each_entry(cli_element_t, element, &cli_head->list, list){
			if(element->irn == live_irn){
				is_element = 1;
				break;
			}
		}

		if (!is_element){
			new_element = get_new_cli_element(ifg);
			new_element->irn = live_irn;
			list_add(&new_element->list, &cli_head->list) ;
		}
	}

	cli_head->min = min_node;
	cli_head->max = max_node;
}

static cli_head_t *get_next_cli_head(const ir_node *irn, cli_iter_t *it) /* ...containing the node *irn */
{
	cli_head_t *head;
	cli_element_t *element;

	int is_dominated_by_max;
	//int dominates_min;

	if (it->curr_cli_head == NULL || it->curr_cli_head->next_cli_head == NULL) /* way back of recursion or this is the last clique */
	{
		it->curr_cli_head = NULL;
		return NULL;
	}

	head = it->curr_cli_head->next_cli_head;

	is_dominated_by_max = value_dominates(head->max, irn);
	//dominates_min = value_dominates(irn, head->min);

	if ((is_dominated_by_max) || (irn == head->max)) /* node could be in clique */
	{
		/* check if node is in clique */
		list_for_each_entry(cli_element_t, element, &head->list, list)
		{
			if (&element->list != &head->list)
			{
				if (element->irn == irn)
				{ /* node is in clique */
					it->curr_cli_head    = head;
					it->curr_cli_element = (void *) head; /* needed because the next element is searched with list.next of it->curr_cli_element */
					break;
				}
			}
		}

		if (it->curr_cli_head != head) /*node was not in clique */
		{
			it->curr_cli_head = head;
			head = get_next_cli_head(irn, it);
		}
	}
	else
	{
		it->curr_cli_head = head;
		head = get_next_cli_head(irn, it);
	}
	return head;
}

static cli_element_t *get_next_element(const ir_node *irn, cli_iter_t *it) /* ... of the current clique, returns NULL if there were no more elements ..*/
{
	cli_element_t *element = it->curr_cli_element;
	cli_head_t *head = it->curr_cli_head;

	if (!head || it->curr_cli_element == NULL) /* way back of recursion or there are no more heads */
	{
		it->curr_cli_element = NULL;
		return NULL;
	}
	else
	{
		element = list_entry(element->list.next, cli_element_t, list);

		if (&element->list == &head->list) /* Clique has no more elements */
		{
			head = get_next_cli_head(irn, it);
			element = get_next_element(irn, it);
		}

		if (element && element->irn == irn) /* the node you are searching neighbors for */
		{
			it->curr_cli_element = element;
			element = get_next_element(irn, it);
		}

		it->curr_cli_element = element;

		return element;
	}
}

static void find_nodes(const ifg_clique_t *ifg, cli_iter_t *it)
{
	cli_element_t *element;
	cli_head_t *cli_head = ifg->cli_root;

	bitset_t *bitset_visnodes = bitset_malloc(get_irg_last_idx(ifg->env->irg));

	it->visited_nodes = bitset_visnodes;
	it->curr_cli_head = cli_head;

	assert(cli_head && "There is no root entry to work on!");

	if (cli_head->list.next != &cli_head->list) /* if cli_head contains an element */
	{
		element = list_entry(cli_head->list.next, cli_element_t, list);
		it->curr_cli_element = element;
	}

	return;
}

static ir_node *get_next_node(cli_iter_t *it)
{
	cli_head_t *cli_head = NULL;
	cli_element_t *element = it->curr_cli_element;

	ir_node *irn;

	if (element == NULL)
		return NULL;

	if (!(&it->curr_cli_head->list == element->list.next))
	{
		irn = element->irn;
		element = list_entry(element->list.next, cli_element_t, list);
		it->curr_cli_element = element;
	}
	else /* reached end of clique */
	{
		cli_head = it->curr_cli_head;
		if (!(cli_head->next_cli_head == NULL))
		{
			irn = element->irn;
			cli_head = cli_head->next_cli_head;
			it->curr_cli_head = cli_head;
			element = list_entry(cli_head->list.next, cli_element_t, list);
			it->curr_cli_element = element;
		}
		else
		{
			it->curr_cli_head = NULL;
			it->curr_cli_element = NULL;
			irn = element->irn;
		}
	}
	if (!(irn == NULL))
	{
		if (bitset_is_set(it->visited_nodes, get_irn_idx(irn)))
		{
			irn = get_next_node(it);
		}
		if (!(irn == NULL))
		{
			bitset_set(it->visited_nodes, get_irn_idx(irn));
		}
	}

	return irn;
}

static void find_neighbour_walker(ir_node *bl, void *data)
{
	ifg_clique_t     *ifg    = data;
	struct list_head *head   = get_block_border_head(ifg->env, bl);
	int              was_def = 0;
	nodeset          *live   = new_nodeset(ifg->env->cls->n_regs);
	border_t         *b;

	assert(is_Block(bl) && "There is no block to work on.");

	foreach_border_head(head, b) /* follow the borders of the block */
	{
		ir_node *irn = b->irn;

		if (b->is_def) /* b is a new node */
		{
			nodeset_insert(live, irn);
			if(b->is_real)
			{
				was_def = 1;
			}
		}
		else
		{
			if (was_def == 1) /* if there is a USE after a DEF... */
			{
				write_clique(live, ifg); /* ...add the clique. */
				was_def = 0;
			}
			nodeset_remove(live, irn);
		}
	}
	del_nodeset(live);
}

static void find_first_neighbour(const ifg_clique_t *ifg, cli_iter_t *it, const ir_node *irn)
{
	cli_head_t *cli_head = ifg->cli_root;
	cli_element_t *element;
	bitset_t *bitset_visneighbours = bitset_malloc(get_irg_last_idx(ifg->env->irg));

	int is_dominated_by_max = 0;
	int dominates_min = 0;
	int is_in_clique = 0;

	it->curr_cli_head = cli_head;
	it->ifg = ifg;
	it->visited_neighbours = bitset_visneighbours;

	assert(cli_head && "There is no root entry for a cli_head.");

	is_dominated_by_max = value_dominates(cli_head->max, irn);
	dominates_min = value_dominates(irn, cli_head->min);

	if ((is_dominated_by_max) || (irn == cli_head->max))  /* node could be in clique */
	{
		/* check if node is in clique */
		list_for_each_entry(cli_element_t, element, &cli_head->list, list)
		{
			if (element->irn == irn) /* node is in clique */
			{
				it->curr_cli_element = (void *) cli_head; /* needed because the next element is searched with list.next of it->curr_cli_element */
				is_in_clique = 1;
				element = get_next_element(irn, it);
				break;
			}
		}
	}
	if(!is_in_clique)
	{
		cli_head = get_next_cli_head(irn, it);
		element = get_next_element(irn, it);
	}

	it->curr_cli_element = element;
	it->curr_irn = irn;

	return;
}

static ir_node *get_next_neighbour(cli_iter_t *it)
{
	ir_node *res = NULL;
	const ir_node *irn = it->curr_irn;

	if (it->curr_cli_element != NULL)
		res = it->curr_cli_element->irn;
	else
		return NULL;

	it->curr_cli_element = get_next_element(irn, it);

	if (res)
	{
		if (bitset_contains_irn(it->visited_neighbours, res))
		{
			res = get_next_neighbour(it);
		}
		else
		{
			bitset_set(it->visited_neighbours, get_irn_idx(res));
		}
	}

	return res;
}


/* PUBLIC FUNCTIONS */

static void ifg_clique_free(void *self)
{
	ifg_clique_t *ifg = self;
	obstack_free(&ifg->obst, NULL);

	free(self);
}

static int ifg_clique_connected(const void *self, const ir_node *a, const ir_node *b)
{
	const ifg_clique_t *ifg = self;
	cli_iter_t it;
	int connected = -1;
	ir_node *irn = NULL;

	find_first_neighbour(ifg, &it, a);
	connected = 0;
	irn = get_next_neighbour(&it);
	while (irn != NULL)
	{
		if (irn == b)
		{
			connected = 1;
			break;
		}
		irn = get_next_neighbour(&it);
	}

	return connected;
}

static ir_node *ifg_clique_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	find_first_neighbour(self, iter, irn);
	return get_next_neighbour(iter);
}

static ir_node *ifg_clique_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_clique_neighbours_break(const void *self, void *iter)
{
	cli_iter_t *it = iter;

	bitset_free(it->visited_neighbours);

	return;
}

static ir_node *ifg_clique_nodes_begin(const void *self, void *iter)
{
	find_nodes(self, iter);
	return get_next_node(iter);
}

static ir_node *ifg_clique_nodes_next(const void *self, void *iter)
{
	return get_next_node(iter);
}

static void ifg_clique_nodes_break(const void *self, void *iter)
{
	cli_iter_t *it = iter;

	bitset_free(it->visited_nodes);

	return;
}

static int ifg_clique_degree(const void *self, const ir_node *irn)
{
	int degree = -1;
	cli_iter_t it;

	find_first_neighbour(self, &it, irn);
	degree = 0;
	irn = get_next_neighbour(&it);
	while (irn != NULL)
	{
		degree++;
		irn = get_next_neighbour(&it);
	}

	return degree;
}

static const be_ifg_impl_t ifg_clique_impl = {
	sizeof(cli_iter_t),
	sizeof(cli_iter_t),
	0,
	ifg_clique_free,
	ifg_clique_connected,
	ifg_clique_neighbours_begin,
	ifg_clique_neighbours_next,
	ifg_clique_neighbours_break,
	ifg_clique_nodes_begin,
	ifg_clique_nodes_next,
	ifg_clique_nodes_break,
	NULL,
	NULL,
	NULL,
	ifg_clique_degree
};

be_ifg_t *be_ifg_clique_new(const be_chordal_env_t *env)
{
	ifg_clique_t *ifg	= xmalloc(sizeof(*ifg));

	ifg->impl     		= &ifg_clique_impl;
	ifg->env			= env;

	ifg->cli_root		= NULL;
	obstack_init(&ifg->obst);

	dom_tree_walk_irg(env->irg, find_neighbour_walker, NULL, ifg);

	obstack_finish(&ifg->obst);
	return (be_ifg_t *) ifg;
}
