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

#include "becopyopt.h"

#include "firm_types.h"

typedef struct _be_ifg_impl_t   be_ifg_impl_t;
typedef struct _be_ifg_t        be_ifg_t;

#define be_ifg_nodes_iter_alloca(self)          (alloca(be_ifg_nodes_iter_size(self)))
#define be_ifg_neighbours_iter_alloca(self)     (alloca(be_ifg_neighbours_iter_size(self)))
#define be_ifg_cliques_iter_alloca(self)        (alloca(be_ifg_cliques_iter_size(self)))

size_t   (be_ifg_nodes_iter_size)(const be_ifg_t *self);
size_t   (be_ifg_neighbours_iter_size)(const be_ifg_t *self);
size_t   (be_ifg_cliques_iter_size)(const be_ifg_t *self);
void     (be_ifg_free)(be_ifg_t *self);
int      (be_ifg_connected)(const be_ifg_t *self, const ir_node *a, const ir_node *b);
ir_node *(be_ifg_neighbours_begin)(const be_ifg_t *self, void *iter, const ir_node *irn);
ir_node *(be_ifg_neighbours_next)(const be_ifg_t *self, void *iter);
void     (be_ifg_neighbours_break)(const be_ifg_t *self, void *iter);
ir_node *(be_ifg_nodes_begin)(const be_ifg_t *self, void *iter);
ir_node *(be_ifg_nodes_next)(const be_ifg_t *self, void *iter);
void     (be_ifg_nodes_break)(const be_ifg_t *self, void *iter);
int      (be_ifg_cliques_begin)(const be_ifg_t *self, void *iter, ir_node **buf);
int      (be_ifg_cliques_next)(const be_ifg_t *self, void *iter);
void     (be_ifg_cliques_break)(const be_ifg_t *self, void *iter);
int      (be_ifg_degree)(const be_ifg_t *self, const ir_node *irn);

#define be_ifg_foreach_neighbour(self, iter, irn, pos) \
	for(pos = be_ifg_neighbours_begin(self, iter, irn); (pos); pos = be_ifg_neighbours_next(self, iter))

#define be_ifg_foreach_node(self, iter, pos) \
	for(pos = be_ifg_nodes_begin(self, iter); (pos); pos = be_ifg_nodes_next(self, iter))

#define be_ifg_foreach_clique(self, iter, buf, count) \
	for(*(count) = be_ifg_cliques_begin(self, iter, buf); \
        *(count) != -1 ; \
        *(count) = be_ifg_cliques_next(self, iter))

typedef struct {
	int n_nodes;
	int n_edges;
	int n_comps;
} be_ifg_stat_t;

void be_ifg_stat(be_irg_t *birg, be_ifg_t *ifg, be_ifg_stat_t *stat);

be_ifg_t *be_create_ifg(const be_chordal_env_t *env);

/*
     ____                        _
    |  _ \ _   _ _ __ ___  _ __ (_)_ __   __ _
    | | | | | | | '_ ` _ \| '_ \| | '_ \ / _` |
    | |_| | |_| | | | | | | |_) | | | | | (_| |
    |____/ \__,_|_| |_| |_| .__/|_|_| |_|\__, |
                          |_|            |___/
*/

typedef struct _be_ifg_dump_dot_cb_t {
	int  (*is_dump_node)(void *self, ir_node *irn);
	void (*graph_attr)(FILE *f, void *self);
	void (*node_attr)(FILE *f, void *self, ir_node *irn);
	void (*edge_attr)(FILE *f, void *self, ir_node *from, ir_node *to);
	void (*at_begin)(FILE *file, void *self);
	void (*at_end)(FILE *file, void *self);
} be_ifg_dump_dot_cb_t;

void be_ifg_dump_dot(be_ifg_t *ifg, ir_graph *irg, FILE *file, const be_ifg_dump_dot_cb_t *cb, void *self);
void be_ifg_check_sorted(const be_ifg_t *ifg);
void be_ifg_check_sorted_to_file(const be_ifg_t *ifg, FILE *f);
void be_ifg_check_performance(be_chordal_env_t *chordal_env);


#endif /* _BEIFG_H */
