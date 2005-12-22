/**
 * @file   beifg.h
 * @date   18.11.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEIFG_H
#define _BEIFG_H

#include "irnode.h"

typedef struct _be_ifg_impl_t   be_ifg_impl_t;
typedef struct _be_ifg_t        be_ifg_t;

#define be_ifg_nodes_iter_alloca(self)          (alloca(be_ifg_nodes_iter_size(self)))
#define be_ifg_neighbours_iter_alloca(self)     (alloca(be_ifg_neighbours_iter_size(self)))

size_t   (be_ifg_nodes_iter_size)(const void *self);
size_t   (be_ifg_neighbours_iter_size)(const void *self);
void     (be_ifg_free)(void *self);
int      (be_ifg_connected)(const void *self, const ir_node *a, const ir_node *b);
ir_node *(be_ifg_neighbours_begin)(const void *self, void *iter, const ir_node *irn);
ir_node *(be_ifg_neighbours_next)(const void *self, void *iter);
void     (be_ifg_neighbours_break)(const void *self, void *iter);
ir_node *(be_ifg_nodes_begin)(const void *self, void *iter);
ir_node *(be_ifg_nodes_next)(const void *self, void *iter);
void     (be_ifg_nodes_break)(const void *self, void *iter);
int      (be_ifg_degree)(const void *self, const ir_node *irn);

#define be_ifg_foreach_neighbour(self, iter, irn, pos) \
	for(pos = be_ifg_neighbours_begin(self, iter, irn); (pos); pos = be_ifg_neighbours_next(self, iter))

#define be_ifg_foreach_node(self, iter, pos) \
	for(pos = be_ifg_nodes_begin(self, iter); (pos); pos = be_ifg_nodes_next(self, iter))

#endif /* _BEIFG_H */
