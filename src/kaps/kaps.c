/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Partitioned Boolean Quadratic Problem (PBQP) solver.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "kaps.h"

#include "adt/array.h"
#include "adt/xmalloc.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

pbqp_node_t *get_node(pbqp_t *pbqp, unsigned index)
{
	return pbqp->nodes[index];
}

pbqp_edge_t *get_edge(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index)
{
	if (tgt_index < src_index) {
		unsigned tmp = src_index;
		src_index    = tgt_index;
		tgt_index    = tmp;
	}

	pbqp_node_t *src_node = get_node(pbqp, src_index);
	pbqp_node_t *tgt_node = get_node(pbqp, tgt_index);
	assert(tgt_node);

	size_t len = ARR_LEN(src_node->edges);

	for (size_t i = 0; i < len; ++i) {
		pbqp_edge_t *cur_edge = src_node->edges[i];

		if (cur_edge->tgt == tgt_node) {
			return cur_edge;
		}
	}

	return NULL;
}

pbqp_t *alloc_pbqp(unsigned number_nodes)
{
	pbqp_t *pbqp = XMALLOC(pbqp_t);

	obstack_init(&pbqp->obstack);

#ifdef NDEBUG
	pbqp->solution     = 0;
#else
	pbqp->solution     = INF_COSTS;
#endif
	pbqp->num_nodes    = number_nodes;
#if KAPS_DUMP
	pbqp->dump_file    = NULL;
#endif
	pbqp->nodes        = OALLOCNZ(&pbqp->obstack, pbqp_node_t*, number_nodes);
#if KAPS_STATISTIC
	pbqp->num_bf       = 0;
	pbqp->num_edges    = 0;
	pbqp->num_r0       = 0;
	pbqp->num_r1       = 0;
	pbqp->num_r2       = 0;
	pbqp->num_rm       = 0;
	pbqp->num_rn       = 0;
#endif

	return pbqp;
}

void free_pbqp(pbqp_t *pbqp)
{
	obstack_free(&pbqp->obstack, NULL);
	free(pbqp);
}

void add_node_costs(pbqp_t *pbqp, unsigned node_index, vector_t *costs)
{
	pbqp_node_t *node = get_node(pbqp, node_index);

	if (node == NULL) {
		node = alloc_node(pbqp, node_index, costs);
		pbqp->nodes[node_index] = node;
	} else {
		vector_add(node->costs, costs);
	}
}

void add_edge_costs(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index,
                    pbqp_matrix_t *costs)
{
	/* Add self loops to the node's costs. */
	if (src_index == tgt_index) {
		assert(costs->rows == costs->cols);

		unsigned  length   = costs->rows;
		vector_t *diagonal = vector_alloc(pbqp, length);

		for (unsigned i = length; i-- != 0;) {
			num value = costs->entries[i * length + i];

			vector_set(diagonal, i, value);
		}

		add_node_costs(pbqp, src_index, diagonal);

		return;
	}

	pbqp_edge_t *edge = get_edge(pbqp, src_index, tgt_index);

	if (tgt_index < src_index) {
		pbqp_matrix_transpose(pbqp, costs);
		add_edge_costs(pbqp, tgt_index, src_index, costs);
		return;
	}

	if (edge == NULL) {
		alloc_edge(pbqp, src_index, tgt_index, costs);
	} else {
		pbqp_matrix_add(edge->costs, costs);
	}
}

num get_node_solution(pbqp_t *pbqp, unsigned node_index)
{
	pbqp_node_t *node = get_node(pbqp, node_index);

	return node->solution;
}

num get_solution(pbqp_t *pbqp)
{
	return pbqp->solution;
}

#if KAPS_DUMP
void set_dumpfile(pbqp_t *pbqp, FILE *f)
{
	pbqp->dump_file = f;
}
#endif
