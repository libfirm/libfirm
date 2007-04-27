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
 * @file   beifg_pointer.c
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
#include "irphase_t.h"

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irbitset.h"

#include "be_t.h"
#include "bera.h"
#include "beifg_t.h"
#include "bechordal_t.h"

typedef struct _ptr_element_t ptr_element_t;

typedef union element_content {
	ir_node *irn;
	ptr_element_t *element;
} element_content;

struct _ptr_element_t {
	int kind; /* kind = 8888 ..> both are ir_nodes, = 3101 ..> first is another element, second an ir_node */
	element_content content_first; /* could be a ptr_element or ir_node */
	element_content content_second; /* could be a ptr_element or ir_node */
};

typedef struct _ptr_head_t {
	struct list_head list;
	ptr_element_t *element;
} ptr_head_t;

typedef struct _ifg_pointer_t {
	const be_ifg_impl_t *impl;
	const be_chordal_env_t *env;
	ir_phase ph;
	struct obstack obst;
	ptr_head_t *curr_ptr_head;
	ptr_element_t *curr_element;
	pmap *node_map;
} ifg_pointer_t;

typedef struct _ptr_iter_t {
	const ifg_pointer_t *ifg;
	const ir_node *irn;
	ptr_head_t *curr_ptr_head;
	ptr_head_t *first_head;
	ptr_element_t *curr_element_t;
	ir_node *curr_irn;
	int get_first;
	int sub_call;
	bitset_t *visited_neighbours;
} ptr_iter_t;

/* PRIVATE FUNCTIONS */

static void *ptr_irn_data_init(ir_phase *ph, ir_node *irn, void *data)
{
	ptr_head_t *head = phase_alloc(ph, sizeof(*head));
	INIT_LIST_HEAD(&head->list);
	return head;
}

static ptr_element_t *ptr_get_new_element(ifg_pointer_t *ifg)
{
	ptr_element_t *new_element = obstack_alloc(&ifg->obst, sizeof(ptr_element_t));
	memset(new_element, 0, sizeof(*new_element));
	return new_element;
}

static ptr_head_t *ptr_get_new_head(ifg_pointer_t *ifg)
{
	ptr_head_t *new_element = obstack_alloc(&ifg->obst, sizeof(*new_element));
	INIT_LIST_HEAD(&new_element->list);
	return new_element;
}

static void write_pointers(bitset_t *live, ifg_pointer_t *ifg)
{
	ir_node *live_irn;
	bitset_pos_t elm;

	bitset_foreach_irn(ifg->env->irg, live, elm, live_irn)
	{
		ptr_head_t *head = phase_get_or_set_irn_data(&ifg->ph, live_irn);
		ptr_head_t *element = ptr_get_new_head(ifg);
		ir_node *irn = NULL;

#if 0
		// Matze: huh, what is this?!? node numbers aren't in any way deterministic AFAIK
		if (live_irn->node_nr == 1883 || live_irn->node_nr == 1858)
			irn = NULL;
#endif

		element->element = ifg->curr_element; /* write current highest sub-clique for each node */
		list_add(&element->list, &head->list);

		/* the following code is to find all nodes, it should be replaced by a "keywalker" of irphase */
		irn = pmap_get(ifg->node_map, live_irn);
		if (!irn)
			pmap_insert(ifg->node_map, live_irn, live_irn);
	}
}

static ptr_element_t *get_last_sub_clique(ifg_pointer_t *ifg, bitset_t *live, bitset_t *my_live, ir_node *irn)
{
	ptr_element_t *element = ifg->curr_element;
	ptr_element_t *res = NULL;

	/* search the last sub-clique before the sub-clique that contains the node irn */
	if (element && element->kind == 8888
		&& (element->content_first.irn == irn
		|| element->content_second.irn == irn)) /* contains the node we search and there is no other sub-clique before */
	{
		if (bitset_is_set(live, get_irn_idx(element->content_first.irn)) && element->content_first.irn != irn) /* irn is still alive and not the node we search a sub-clique for */
		{
			bitset_set(my_live, get_irn_idx(element->content_first.irn));
		}

		if (bitset_is_set(live, get_irn_idx(element->content_second.irn))&& element->content_second.irn != irn) /* irn is still alive and not the node we search a sub-clique for */
		{
			bitset_set(my_live, get_irn_idx(element->content_second.irn));
		}
		res = NULL;
	}
	else
	{
		if (element && element->kind == 8889)
		{ /* this was a "single-node-clique" */
			res = NULL;
		}

		if (element && (element->kind == 3101)
			&& (element->content_second.irn == irn)) /* sub-clique contains node, return previous sub-clique */
		{
			res = element->content_first.element;
		}
		else
		{
			if (element && element->kind == 3101) /* look at previous sub-cliques if the contain the node you are searching for*/
			{
				if (bitset_is_set(live, get_irn_idx(element->content_second.irn))) /* irn is still alive */
				{
					bitset_set(my_live, get_irn_idx(element->content_second.irn));
				}
				ifg->curr_element = element->content_first.element;
				res = get_last_sub_clique(ifg, live, my_live, irn);
			}
			else
			{
				res = NULL;
			}
		}
	}
	return res;
}

static void find_neighbour_walker(ir_node *bl, void *data)
{
	ifg_pointer_t *ifg      = data;
	struct list_head *head  = get_block_border_head(ifg->env, bl);
	border_t *b;
	bitset_t *live = bitset_malloc(get_irg_last_idx(ifg->env->irg));
	bitset_t *my_live;
	bitset_pos_t my_elm;
	ir_node *my_irn;
	element_content last_irn;
	element_content last_element;
	int was_def = 0;
	ir_node *first = NULL;
	int was_first = 0;

	last_irn.irn = NULL;
	last_element.element = NULL;

	assert(is_Block(bl) && "There is no block to work on.");

	foreach_border_head(head, b) /* follow the borders of the block */
	{
		ir_node *irn = b->irn;
		ptr_element_t *element = NULL;

#if 0
		// ?!?
		if (irn->node_nr == 1883 || irn->node_nr == 1858)
			i=1;
#endif

		if (b->is_def) /* b is a new node */
		{
			bitset_set(live, get_irn_idx(irn));
			if (last_element.element)
			{
				element = ptr_get_new_element(ifg);
				element->content_first.element = last_element.element;
				element->content_second.irn = b->irn;
				element->kind = 3101; /* first is an element, second an ir_node */

				last_element.element = element;
				ifg->curr_element = element;
			}
			else
			{
				if (last_irn.irn)	/* create new sub-clique */
				{
					element = ptr_get_new_element(ifg);
					element->content_first.irn = last_irn.irn;
					element->content_second.irn = b->irn;
					element->kind = 8888; /* both are ir_nodes */

#if 0
					// ?!?
					if (irn->node_nr == 1883 || irn->node_nr == 1858 || irn->node_nr == 1936)
						i=1;
#endif


					last_element.element = element;
					ifg->curr_element = element;
					last_irn.irn = NULL;
				}
				else
				{
					last_irn.irn = b->irn;	/* needed to create first sub-clique */
					last_element.element = NULL;
				}
			}

			was_def = 1;
		}
		else
		{
			if (was_def == 1) /* if there is a USE after a DEF... */
			{
				if (!last_element.element)
				{ /* there was only one element in the clique */
					element = ptr_get_new_element(ifg);
					element->kind = 8889; /* first is a node, second is NULL, because this is a "single-node-clique" */
					element->content_first.irn = last_irn.irn;
					last_irn.irn = NULL;
					element = NULL;
					ifg->curr_element = NULL;
				}


				write_pointers(live, ifg); /* ...write a pointer to the highest sub-clique for each living node. */
				was_def = 0;
			}

			my_live = bitset_malloc(get_irg_last_idx(ifg->env->irg));
			last_element.element = get_last_sub_clique(ifg, live, my_live, irn);

			/* check and add still living nodes */
			//bitset_remv_irn(my_live, irn);
			if (bitset_popcnt(my_live) > 1)
			{
				if (last_element.element)
				{
					bitset_foreach_irn(ifg->env->irg, my_live, my_elm, my_irn)
					{
						ptr_element_t *my_element = ptr_get_new_element(ifg);
						my_element->content_first.element = last_element.element;
						my_element->content_second.irn = my_irn;
						my_element->kind = 3101; /* first is an element, second an ir_node */

						last_element.element = my_element;
						ifg->curr_element = my_element;
					}
				}
				else
				{
					bitset_foreach_irn(ifg->env->irg, my_live, my_elm, my_irn)
					{
						ptr_element_t *my_element = NULL;
						if (!first && !was_first)
						{
							first = my_irn;
							was_first = 1;
						}
						else
						{
							if (first && was_first)
							{
								my_element = ptr_get_new_element(ifg);
								my_element->content_first.irn = first;
								my_element->content_second.irn = my_irn;
								my_element->kind = 8888; /* both are ir_nodes */
								last_element.element = my_element;
								ifg->curr_element = my_element;

#if 0
								// ?!?
								if (my_irn->node_nr == 1883 || my_irn->node_nr == 1858 || my_irn->node_nr == 1936)
									i=1;
#endif


								first = NULL;
							}
							else
							{
								my_element = ptr_get_new_element(ifg);
								my_element->content_first.element = last_element.element;
								my_element->content_second.irn = my_irn;
								my_element->kind = 3101; /* first is an element, second an ir_node */
								last_element.element = my_element;
								ifg->curr_element = my_element;
							}
						}
					}
					was_first = 0;
				}
			}
			else
			{
				if (bitset_popcnt(my_live) == 1) /* there is only one node left */
				{
					if (last_element.element)
					{
						bitset_foreach_irn(ifg->env->irg, my_live, my_elm, my_irn)
						{
							ptr_element_t *my_element = ptr_get_new_element(ifg);
							my_element->content_first.element = last_element.element;
							my_element->content_second.irn = my_irn;
							my_element->kind = 3101; /* first is an element, second an ir_node */

							last_element.element = my_element;
							ifg->curr_element = my_element;
						}
					}
					else
					{
						bitset_foreach_irn(ifg->env->irg, my_live, my_elm, my_irn);
						{
							ptr_element_t *my_element = ptr_get_new_element(ifg);
							my_element->content_first.irn = my_irn;
							my_element->content_second.irn = NULL;
							my_element->kind = 8889;
							last_element.element =  my_element;
							ifg->curr_element = my_element;
						}
					}
				}
			}
			bitset_free(my_live);
			bitset_remv_irn(live, irn);
		}
	}
	bitset_free(live);
}

static ir_node *get_first_irn(const ifg_pointer_t *ifg, ptr_iter_t *it)
{
	/* this should be replaced using a keywalker of the irphase &ifg.ph */
	ir_node *irn;
	pmap_entry *entry;

	it->ifg = ifg;
	entry = pmap_first(ifg->node_map);

	if (!entry)
		return NULL;

	irn =  entry->value;
	it->curr_irn = irn;

	return irn;
}

static ir_node *get_next_irn(ptr_iter_t *it)
{
	/* this should be replaced using a keywalker of the irphase &ifg.ph */
	ir_node *irn;
	pmap_entry *entry;

	irn = it->curr_irn;
	entry = pmap_next(it->ifg->node_map);

	if (!entry)
		return NULL;

	it->curr_irn = entry->value;

	return entry->value;
}

static ir_node *get_next_neighbour(ptr_iter_t *it)
{
	ir_node *res;
	ptr_head_t *head;
	ptr_element_t *element;

	element = it->curr_element_t;

	if (element == NULL)
	{
#if 0
		// ?!?
		if (it->irn->node_nr == 1883 || it->irn->node_nr == 1858)
			i=1;
#endif

		if (it->curr_ptr_head->list.next != &it->first_head->list)
		{
			head = list_entry(it->curr_ptr_head->list.next, ptr_head_t, list);
			it->curr_ptr_head = head;
			element = head->element;
		}
		else
			return NULL; /* there are no more neighbours */
	}

	if (element && element->kind == 8889) /* node has no neighbours */
	{
		res = element->content_first.irn;
		it->curr_element_t = NULL;
	}
	else
	{
		if (element && element->kind == 8888) /* node has only one more neighbour */
		{
			if (it->get_first)
			{
				if (element->content_first.irn != it->irn)
				{
					res = element->content_first.irn;
					it->get_first = 0;
					it->curr_element_t = NULL;
				}
				else
				{
					it->get_first = 0;
					it->curr_element_t = NULL;
					it->sub_call++;
					res = get_next_neighbour(it);
					it->sub_call--;
				}
			}
			else
			{
				if (element->content_second.irn != it->irn)
				{
					res = element->content_second.irn;
					it->get_first = 1;
					it->curr_element_t = element;
				}
				else
				{
					it->get_first = 1;
					it->curr_element_t = element;
					it->sub_call++;
					res = get_next_neighbour(it);
					it->sub_call--;
				}
			}
		}
		else
		{
			if (element && element->kind == 3101)
			{
				it->curr_element_t = element->content_first.element;
				res = element->content_second.irn;
			}
			else
			{ /* element is only an ir_node */// TODO
				it->curr_element_t = NULL;
				return NULL;
			}
		}
	}

	if (res && !it->sub_call)
	{
		if (bitset_contains_irn(it->visited_neighbours, res) || res == it->irn)
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

static ir_node *get_first_neighbour(const ifg_pointer_t *ifg, ptr_iter_t *it, const ir_node *irn)
{
	ir_node *res;
	ptr_head_t *head;
	ptr_element_t *element;
	bitset_t *bitsetvisited_neighbours = bitset_malloc(get_irg_last_idx(ifg->env->irg));

	it->ifg = ifg;
	it->irn = irn;
	it->get_first = 0;
	it->sub_call = 0;

	it->visited_neighbours = bitsetvisited_neighbours;

	head = phase_get_irn_data(&ifg->ph, irn);
	if (!head)
		return NULL;
	else
	{
		it->first_head = head;
		head = list_entry(it->first_head->list.next, ptr_head_t, list); /* because first element is NULL */
		it->curr_ptr_head = head;
		element = head->element;
	}

	if (element && element->kind == 8889) /* node has no neighbours */
	{
		res = element->content_first.irn;
		it->curr_element_t = NULL;
	}
	else
	{
		if (element && element->kind == 8888) /* node has only one neighbour */
		{
			if (it->get_first)
			{
				if (element->content_first.irn != it->irn)
				{
					res = element->content_first.irn;
					it->get_first = 0;
					it->curr_element_t = NULL;
				}
				else
				{
					it->get_first = 0;
					it->curr_element_t = NULL;
					it->sub_call++;
					res = get_next_neighbour(it);
					it->sub_call--;
				}
			}
			else
			{
				if (element->content_second.irn != it->irn)
				{
					res = element->content_second.irn;
					it->curr_element_t = element;
					it->get_first = 1;
				}
				else
				{
					it->get_first = 1;
					it->curr_element_t = element;
					it->sub_call++;
					res = get_next_neighbour(it);
					it->sub_call--;
				}
			}
		}
		else
			if (element && element->kind == 3101)
			{
				it->curr_element_t = element->content_first.element;
				res = element->content_second.irn;
			}
			else
			{ /* element is only an ir_node */
				it->curr_element_t = NULL;
				return NULL;
			}
	}

	if (res && !it->sub_call)
	{
		if (bitset_contains_irn(it->visited_neighbours, res) || res == it->irn)
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

static void ifg_pointer_free(void *self)
{
	ifg_pointer_t *ifg = self;
	obstack_free(&ifg->obst, NULL);
	phase_free(&ifg->ph);

	free(self);
}

static int ifg_pointer_connected(const void *self, const ir_node *a, const ir_node *b)
{
	const ifg_pointer_t *ifg = self;
	int connected = -1;
	ptr_iter_t it;
	ir_node *irn = NULL;

	irn = get_first_neighbour(ifg, &it, a);
	connected = 0;
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

static ir_node *ifg_pointer_neighbours_begin(const void *self, void *iter, const ir_node *irn)
{
	return get_first_neighbour(self, iter, irn);
}

static ir_node *ifg_pointer_neighbours_next(const void *self, void *iter)
{
	return get_next_neighbour(iter);
}

static void ifg_pointer_neighbours_break(const void *self, void *iter)
{
	ptr_iter_t *it = iter;

	bitset_free(it->visited_neighbours);

	return;
}

static ir_node *ifg_pointer_nodes_begin(const void *self, void *iter)
{
	return get_first_irn(self, iter);
}

static ir_node *ifg_pointer_nodes_next(const void *self, void *iter)
{
	return get_next_irn(iter);
}

static void ifg_pointer_nodes_break(const void *self, void *iter)
{
	return;
}

static int ifg_pointer_degree(const void *self, const ir_node *irn)
{
	int degree = -1;
	ptr_iter_t it;

	irn = get_first_neighbour(self, &it, irn);
	degree = 0;
	while (irn != NULL)
	{
		degree++;
		irn = get_next_neighbour(&it);
	}

	return degree;
}

static const be_ifg_impl_t ifg_pointer_impl = {
	sizeof(ptr_iter_t),
	sizeof(ptr_iter_t),
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
	ifg_pointer_t *ifg	= xmalloc(sizeof(*ifg));
	ifg->impl     		= &ifg_pointer_impl;
	ifg->env			= env;

	ifg->node_map 		= pmap_create(); /* to find all nodes, should be replaced by a "keywalker" of irphase */

	phase_init(&ifg->ph, "ptr_map", env->irg, PHASE_DEFAULT_GROWTH, ptr_irn_data_init, NULL);
	obstack_init(&ifg->obst);

	dom_tree_walk_irg(env->irg, find_neighbour_walker, NULL, ifg);

	obstack_finish(&ifg->obst);
	return (be_ifg_t *) ifg;
}
