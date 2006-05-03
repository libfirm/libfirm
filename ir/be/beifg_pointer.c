/**
 * @file   beifg_pointer.c
 * @date   20.03.2006
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#if 0
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "belive_t.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"

#include "be_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"

typedef struct _cli_head_t {
  struct list_head list;
	struct _cli_head_t *next_cli_head;
	ir_node *min;
	ir_node *max;
} cli_head_t;

typedef struct _ifg_pointer_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
	cli_head_t *cli_root;
	struct obstack obst;
} ifg_pointer_t;

typedef struct _cli_element_t {
	ir_node *irn;
	struct list_head list;
} cli_element_t;

typedef struct _cli_iter_t {
	ifg_pointer_t *ifg;
	cli_head_t *curr_cli_head;
	cli_element_t *curr_cli_element;
	unsigned long curr_irg_visited;
	const ir_node *curr_irn;
} cli_iter_t;

/* PRIVATE FUNCTIONS */
static cli_head_t *get_new_cli_head(void *data)
{
	cli_iter_t *it = data;
	cli_head_t *cli_head;
	cli_head_t *new_cli_head;

	if (it->ifg->cli_root == NULL)
	{
	new_cli_head = obstack_alloc(&it->ifg->obst, sizeof(*new_cli_head));
	INIT_LIST_HEAD(&new_cli_head->list);

	new_cli_head->next_cli_head = NULL;
	it->ifg->cli_root = new_cli_head;
	it->curr_cli_head = new_cli_head;
	}
	else
	{
		cli_head = it->ifg->cli_root;
		while(!(cli_head->next_cli_head == NULL))
		{
			cli_head = cli_head->next_cli_head;
		}
		new_cli_head = obstack_alloc(&it->ifg->obst, sizeof(*new_cli_head));
		INIT_LIST_HEAD(&new_cli_head->list);
		cli_head->next_cli_head = new_cli_head;
		it->curr_cli_head = new_cli_head;
	}

	cli_head->min = NULL;
	cli_head->max = NULL;

	return new_cli_head;
}

static cli_head_t *get_first_cli_head(void *data)
{
	cli_iter_t *it = data;

	return it->ifg->cli_root;
}

static void write_clique(pset *live_set, void *data)
{
	ir_node *live_irn;
	ir_node *test_node;
	int test_int = 0;
	ir_node *max_node;
	ir_node *min_node;
	cli_iter_t *it = data;
	pset *live = live_set;
	cli_element_t *element;
	int is_element = 0;

	cli_head_t *cli_head = get_new_cli_head(it);

	for(live_irn = pset_first(live); live_irn; live_irn = pset_next(live))
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
			element->irn = live_irn;
			list_add(&element->list, &cli_head->list) ;
		}
	}
	cli_head->min = min_node;
	cli_head->max = max_node;
}

static void find_nodes(const void *self, void *iter)
{
	const ifg_clique_t *ifg	 = self;
	cli_iter_t *it = iter;
	cli_element_t *element = NULL;
	cli_head_t *cli_head = get_first_cli_head(it);

	assert((cli_head == NULL) && "There is no node to work on!");

	it->curr_cli_head = cli_head;
	it->curr_cli_element = element;

	element = list_entry(cli_head->list.next, cli_element_t, list);

	inc_irg_visited(get_irn_irg(element->irn));
	it->curr_irg_visited = get_irg_visited(get_irn_irg(element->irn));

	return;
}

static ir_node *get_next_node(void *iter)
{
	cli_iter_t *it = iter;
	cli_head_t *cli_head = NULL;
	cli_element_t *element = it->curr_cli_element;
	unsigned long irn_visited = get_irn_visited(element->irn);

	ir_node *irn;

	if (!(element == it->curr_cli_element))
	{
		irn = element->irn;
		element = list_entry(cli_head->list.next, cli_element_t, list);
		it->curr_cli_element = element;
	}
	else
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
			return NULL;
		}
	}

	if (irn_visited == it->curr_irg_visited)
	{
		irn = get_next_node(it);
	}

	set_irn_visited(irn, it->curr_irg_visited);
	return irn;
}

static void find_neighbour_walker(ir_node *bl, void *data)
{
	cli_iter_t *it          = data;
	struct list_head *head  = get_block_border_head(it->ifg->env, bl);
	border_t *b;
	int was_def = 0;

	pset *live = put_live_in(bl, pset_new_ptr_default());

	assert(is_Block(bl) && "There is no block to work on.");

	list_for_each_entry_reverse(border_t, b, head, list)
	{
		ir_node *irn = b->irn;

		if (!(pset_find_ptr(live, irn)) && (b->is_def))
		{
			pset_insert_ptr(live, irn);
			was_def = 1;
		}

		else if (!(b->is_def) && (was_def == 1))
		{
			if (was_def == 1)
			{
				write_clique(live, it);
				was_def = 0;
			}
			pset_remove_ptr(live, irn);
		}
	}
	del_pset(live);
}

static ifg_clique_t *build_neighbours(const be_chordal_env_t *env)
{
	ifg_clique_t *ifg = NULL;
	cli_iter_t *it = NULL;

	it->ifg       			= ifg;
	it->curr_cli_head 		= NULL;
	it->curr_cli_element 	= NULL;
	it->curr_irg_visited 	= get_irg_visited(env->irg);

	obstack_init(&it->ifg->obst);
	dom_tree_walk_irg(env->irg, find_neighbour_walker, NULL, it);

	return ifg;
}

static void find_first_neighbour(const void *self, void *iter, const ir_node *irn)
{
	const ifg_clique_t *ifg = self;
	cli_iter_t *it = iter;

	cli_head_t *cli_head = ifg->cli_root;
	cli_element_t *element;

	int is_dominated_by_max = 0;
	int dominates_min = 0;

	assert(!cli_head && "There is no root entry for a cli_head.");

	while(!(cli_head->next_cli_head == NULL))
	{
		is_dominated_by_max = value_dominates(cli_head->max, irn);
		dominates_min = value_dominates(irn, cli_head->min);

		if ((is_dominated_by_max && dominates_min) /* node is in clique */
				|| (irn == cli_head->max)
				|| (irn == cli_head->min))
		{
			element = list_entry(cli_head->list.next, cli_element_t, list);

			it->curr_cli_element = element;
			it->curr_cli_head = cli_head;
			break;
		}
		else
		{
			cli_head = cli_head->next_cli_head;
		}
	}
	it->curr_irn = irn;

	inc_irg_visited(get_irn_irg(irn));
	it->curr_irg_visited = get_irg_visited(get_irn_irg(irn));
	return;
}

static ir_node *get_next_neighbour(cli_iter_t *it)
{
	ir_node *res = NULL;
	cli_element_t *element = NULL;
	cli_head_t *cli_head = NULL;
	int is_dominated_by_max;
	int dominates_min;
	const ir_node *irn = it->curr_irn;
	unsigned long irn_visited = 0;

	res = it->curr_cli_element->irn;

	/* get next element */

	element = list_entry(it->curr_cli_element->list.next, cli_element_t, list);
	irn_visited = get_irn_visited(element->irn);

	if ((cli_head_t *)element == it->curr_cli_head) /* end of clique, search next one */
	{
		cli_head = cli_head->next_cli_head;
		while(!(cli_head->next_cli_head == NULL))
		{
			is_dominated_by_max = value_dominates(cli_head->max, irn);
			dominates_min = value_dominates(irn, cli_head->min);

			if ((is_dominated_by_max && dominates_min) /* node is in clique */
					|| (irn == cli_head->max)
					|| (irn == cli_head->min))
			{
				element = list_entry(cli_head->list.next, cli_element_t, list);

				it->curr_cli_element = element;
				it->curr_cli_head = cli_head;
				break;
			}
			else
			{
				cli_head = cli_head->next_cli_head;
			}
		}
	}
	else /* get next element of this clique */
	{
		it->curr_cli_element = element;
	}

	if (irn_visited == it->curr_irg_visited)
	{
		irn = get_next_neighbour(it);
	}

	set_irn_visited(res, it->curr_irg_visited);

	return res;
}


/* PUBLIC FUNCTIONS */

static void ifg_pointer_free(void *self)
{
	ifg_clique_t *ifg = self;

	free(self);
}

static int ifg_pointer_connected(const void *self, const ir_node *a, const ir_node *b)
{
	cli_iter_t *it = NULL;
	int connected = -1;
	ir_node *irn = NULL;

	find_first_neighbour(self, it, a);
	connected = 0;
	irn = get_next_neighbour(it);
	while (irn != NULL)
	{
		if (irn == b)
		{
			connected = 1;
		}
		irn = get_next_neighbour(it);
	}

	return connected;
}

static ir_node *ifg_pointer_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	find_first_neighbour(self, iter, irn);
	return get_next_neighbour(iter);
}

static ir_node *ifg_pointer_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_pointer_neighbours_break(const void *self, void *iter)
{
	return;
}

static ir_node *ifg_pointer_nodes_begin(const void *self, void *iter)
{
	find_nodes(self, iter);
	return get_next_node(iter);
}

static ir_node *ifg_pointer_nodes_next(const void *self, void *iter)
{
	return get_next_node(iter);
}

static void ifg_pointer_nodes_break(const void *self, void *iter)
{
	return;
}

static int ifg_pointer_degree(const void *self, const ir_node *irn)
{
	int degree = -1;
	cli_iter_t *it = NULL;

	find_first_neighbour(self, it, irn);
	degree = 0;
	irn = get_next_neighbour(it);
	while (irn != NULL)
	{
		degree++;
		irn = get_next_neighbour(it);
	}

	return degree;
}

static const be_ifg_impl_t ifg_pointer_impl = {
	sizeof(cli_iter_t),
	0,
	0,
	ifg_pointer_free,
	ifg_pointer_connected,
	ifg_pointer_neighbours_begin,
	ifg_pointer_neighbours_next,
	ifg_pointer_neighbours_break,
	ifg_pointer_nodes_begin,
	ifg_pointer_nodes_next,
	ifg_pointer_nodes_break,
	NULL,
	NULL,
	NULL,
	ifg_pointer_degree
};

be_ifg_t *be_ifg_pointer_new(const be_chordal_env_t *env)
{
	ifg_pointer_t *ifg	= malloc(sizeof(*ifg));
	ifg->impl     		= &ifg_pointer_impl;

	ifg->cli_root		= NULL;
	ifg->env		 	= env;

	ifg = build_neighbours(env);

	return (be_ifg_t *) ifg;
}

#endif
