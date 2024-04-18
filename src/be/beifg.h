/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for interference graphs.
 * @author      Sebastian Hack
 * @date        18.11.2005
 */
#ifndef FIRM_BE_BEIFG_H
#define FIRM_BE_BEIFG_H

#include "be_types.h"
#include "bechordal.h"
#include "irnodeset.h"
#include "obstack.h"
#include "pset.h"

struct be_ifg_t {
	const be_chordal_env_t *env;
};

typedef struct nodes_iter_t {
	const be_chordal_env_t *env;
	struct obstack         obst;
	int                    n;
	int                    curr;
	ir_node                **nodes;
} nodes_iter_t;

typedef struct neighbours_iter_t {
	const be_chordal_env_t *env;
	const ir_node        *irn;
	int                   valid;
	ir_nodeset_t          neighbours;
	ir_nodeset_iterator_t iter;
} neighbours_iter_t;

typedef struct cliques_iter_t {
	struct obstack ob;
	const be_chordal_env_t *cenv;
	ir_node **buf;
	ir_node **blocks;
	int n_blocks, blk;
	struct list_head *bor;
	pset *living;
} cliques_iter_t;

void     be_ifg_free(be_ifg_t *ifg);
ir_node *be_ifg_neighbours_begin(const be_ifg_t *ifg, neighbours_iter_t *iter,
                                 const ir_node *irn);
ir_node *be_ifg_neighbours_next(neighbours_iter_t *iter);
void     be_ifg_neighbours_break(neighbours_iter_t *iter);
nodes_iter_t be_ifg_nodes_begin(be_ifg_t const *ifg);
ir_node *be_ifg_nodes_next(nodes_iter_t *iter);
int      be_ifg_cliques_begin(const be_ifg_t *ifg, cliques_iter_t *iter,
                              ir_node **buf);
int      be_ifg_cliques_next(cliques_iter_t *iter);
void     be_ifg_cliques_break(cliques_iter_t *iter);
int      be_ifg_degree(const be_ifg_t *ifg, const ir_node *irn);

#define be_ifg_foreach_neighbour(ifg, iter, irn, pos) \
	for (ir_node *pos = be_ifg_neighbours_begin(ifg, iter, irn); pos; pos = be_ifg_neighbours_next(iter))

#define be_ifg_foreach_node(ifg, pos) \
	for (bool pos##__once = true; pos##__once;) \
		for (nodes_iter_t pos##__iter = be_ifg_nodes_begin(ifg); pos##__once; pos##__once = false) \
			for (ir_node *pos; (pos = be_ifg_nodes_next(&pos##__iter));)

#define be_ifg_foreach_clique(ifg, iter, buf, count) \
	for (*(count) = be_ifg_cliques_begin(ifg, iter, buf); \
	     *(count) != -1 ; \
	     *(count) = be_ifg_cliques_next(iter))

typedef struct {
	int n_nodes;
	int n_edges;
	int n_comps;
} be_ifg_stat_t;

void be_ifg_stat(ir_graph *irg, be_ifg_t *ifg, be_ifg_stat_t *stat);

be_ifg_t *be_create_ifg(const be_chordal_env_t *env);

#endif
