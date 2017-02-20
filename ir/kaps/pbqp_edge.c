/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP edges.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "pbqp_edge.h"

#include "adt/array.h"
#include "kaps.h"
#include "matrix.h"
#include "optimal.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "pbqp_t.h"
#include <assert.h>

pbqp_edge_t *alloc_edge(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index,
                        pbqp_matrix_t *costs)
{
	int          transpose = 0;
	pbqp_edge_t *edge      = OALLOC(&pbqp->obstack, pbqp_edge_t);

	if (tgt_index < src_index) {
		unsigned tmp = src_index;
		src_index = tgt_index;
		tgt_index = tmp;

		transpose = 1;
	}

	pbqp_node_t *src_node = get_node(pbqp, src_index);
	pbqp_node_t *tgt_node = get_node(pbqp, tgt_index);

	if (transpose) {
		edge->costs = pbqp_matrix_copy_and_transpose(pbqp, costs);
	} else {
		edge->costs = pbqp_matrix_copy(pbqp, costs);
	}

	/*
	 * Connect edge with incident nodes. Since the edge is allocated, we know
	 * that it don't appear in the edge lists of the nodes.
	 */
	ARR_APP1(pbqp_edge_t *, src_node->edges, edge);
	edge->src = src_node;
	ARR_APP1(pbqp_edge_t *, tgt_node->edges, edge);
	edge->tgt = tgt_node;
	edge->bucket_index = UINT_MAX;

	return edge;
}

void delete_edge(pbqp_edge_t *edge)
{
	pbqp_node_t *src_node = edge->src;
	pbqp_node_t *tgt_node = edge->tgt;

	disconnect_edge(src_node, edge);
	disconnect_edge(tgt_node, edge);

	edge->src = NULL;
	edge->tgt = NULL;

	reorder_node_after_edge_deletion(src_node);
	reorder_node_after_edge_deletion(tgt_node);
}

unsigned is_deleted(pbqp_edge_t *edge)
{

	return edge->src == NULL && edge->tgt == NULL;
}

pbqp_edge_t *pbqp_edge_deep_copy(pbqp_t *pbqp, pbqp_edge_t *edge,
                                 pbqp_node_t *src_node, pbqp_node_t *tgt_node)
{
	assert(src_node);
	assert(tgt_node);

	pbqp_edge_t *copy = OALLOC(&pbqp->obstack, pbqp_edge_t);

	copy->costs = pbqp_matrix_copy(pbqp, edge->costs);

	/* Connect edge with incident nodes. */
	copy->src = src_node;
	copy->tgt = tgt_node;
	copy->bucket_index = UINT_MAX;

	return copy;
}
