/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Interface for interference graphs.
 * @author      Sebastian Hack
 * @date        18.11.2005
 */
#ifndef FIRM_BE_BEIFG_H
#define FIRM_BE_BEIFG_H

#include "be_types.h"
#include "bechordal.h"
#include "irnodeset.h"
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
int      be_ifg_connected(const be_ifg_t *ifg, const ir_node *a,
                          const ir_node *b);
ir_node *be_ifg_neighbours_begin(const be_ifg_t *ifg, neighbours_iter_t *iter,
                                 const ir_node *irn);
ir_node *be_ifg_neighbours_next(neighbours_iter_t *iter);
void     be_ifg_neighbours_break(neighbours_iter_t *iter);
ir_node *be_ifg_nodes_begin(const be_ifg_t *ifg, nodes_iter_t *iter);
ir_node *be_ifg_nodes_next(nodes_iter_t *iter);
void     be_ifg_nodes_break(nodes_iter_t *iter);
int      be_ifg_cliques_begin(const be_ifg_t *ifg, cliques_iter_t *iter,
                              ir_node **buf);
int      be_ifg_cliques_next(cliques_iter_t *iter);
void     be_ifg_cliques_break(cliques_iter_t *iter);
int      be_ifg_degree(const be_ifg_t *ifg, const ir_node *irn);

#define be_ifg_foreach_neighbour(ifg, iter, irn, pos) \
	for (ir_node *pos = be_ifg_neighbours_begin(ifg, iter, irn); pos; pos = be_ifg_neighbours_next(iter))

#define be_ifg_foreach_node(ifg, iter, pos) \
	for(pos = be_ifg_nodes_begin(ifg, iter); pos != NULL; pos = be_ifg_nodes_next(iter))

#define be_ifg_foreach_clique(ifg, iter, buf, count) \
	for(*(count) = be_ifg_cliques_begin(ifg, iter, buf); \
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
