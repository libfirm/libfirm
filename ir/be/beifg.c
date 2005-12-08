/**
 * @file   beifg.c
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "irnode_t.h"
#include "irprintf.h"
#include "beifg_t.h"

size_t (be_ifg_iter_size)(const void *self)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->iter_size;
}

void (be_ifg_free)(void *self)
{
	be_ifg_t *ifg = self;
	ifg->impl->free(self);
}

int (be_ifg_connected)(const void *self, const ir_node *a, const ir_node *b)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->connected(self, a, b);
}

ir_node *(be_ifg_neighbours_begin)(const void *self, void *iter, const ir_node *irn)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->neighbours_begin(self, iter, irn);
}

ir_node *(be_ifg_neighbours_next)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->neighbours_next(self, iter);
}

ir_node *(be_ifg_nodes_begin)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->nodes_begin(self, iter);
}

ir_node *(be_ifg_nodes_next)(const void *self, void *iter)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->nodes_next(self, iter);
}

int (be_ifg_degree)(const void *self, const ir_node *irn)
{
	const be_ifg_t *ifg = self;
	return ifg->impl->degree(self, irn);
}


int be_ifg_is_simplicial(const be_ifg_t *ifg, const ir_node *irn)
{
	int degree = be_ifg_degree(ifg, irn);
	void *iter = be_ifg_iter_alloca(ifg);

	ir_node **neighbours = malloc(degree * sizeof(neighbours[0]));

	ir_node *curr;
	int i, j;

	be_ifg_foreach_neighbour(ifg, iter, irn, curr)
		neighbours[i++] = curr;

	for(i = 0; i < degree; ++i) {
		for(j = 0; j < i; ++j)
			if(!be_ifg_connected(ifg, neighbours[i], neighbours[j])) {
				free(neighbours);
				return 0;
			}
	}


	free(neighbours);
	return 1;
}

void be_fg_check(const be_ifg_t *ifg)
{
	void *iter1 = be_ifg_iter_alloca(ifg);
	void *iter2 = be_ifg_iter_alloca(ifg);

	ir_node *n, *m;

	/* Check, if all neighbours are indeed connected to the node. */
	be_ifg_foreach_node(ifg, iter1, n) {
		be_ifg_foreach_neighbour(ifg, iter2, n, m)
			if(!be_ifg_connected(ifg, n, m))
				ir_fprintf(stderr, "%+F is a neighbour of %+F but they are not connected!\n", n, m);
	}
}
