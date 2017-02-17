/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   PBQP nodes.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#include "pbqp_node.h"

#include "adt/array.h"
#include "bucket.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node_t.h"
#include "vector.h"
#include <assert.h>
#include <stdbool.h>

pbqp_node_t *alloc_node(pbqp_t *pbqp, unsigned node_index, vector_t *costs)
{
	pbqp_node_t *node = OALLOC(&pbqp->obstack, pbqp_node_t);

	node->edges = NEW_ARR_F(pbqp_edge_t *, 0);
	node->costs = vector_copy(pbqp, costs);
	node->bucket_index = UINT_MAX;
	node->solution = UINT_MAX;
	node->index = node_index;

	return node;
}

int is_connected(pbqp_node_t *node, pbqp_edge_t *edge)
{
	assert(node);

	if (edge->src != node && edge->tgt != node)
		return 0;

	pbqp_edge_t **edges    = node->edges;
	size_t        edge_len = ARR_LEN(edges);

	for (size_t edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *edge_candidate = edges[edge_index];

		if (edge_candidate == edge) {
			return 1;
		}
	}

	return 0;
}

void disconnect_edge(pbqp_node_t *node, pbqp_edge_t *edge)
{
	pbqp_edge_t **edges    = node->edges;
	size_t        edge_len = ARR_LEN(edges);

	for (size_t edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *edge_candidate = edges[edge_index];

		if (edge_candidate == edge) {
			edges[edge_index] = edges[edge_len - 1];
			ARR_SHRINKLEN(edges, (int)edge_len - 1);
			break;
		}
	}
}

unsigned pbqp_node_get_degree(pbqp_node_t *node)
{
	return ARR_LEN(node->edges);
}

pbqp_node_t *pbqp_node_deep_copy(pbqp_t *pbqp, pbqp_node_bucket_t new_bucket,
                                 pbqp_node_t *node)
{
	unsigned     edge_length = pbqp_node_get_degree(node);
	pbqp_node_t *copy        = OALLOC(&pbqp->obstack, pbqp_node_t);

	copy->edges = NEW_ARR_F(pbqp_edge_t *, 0);

	for (unsigned edge_index = 0; edge_index < edge_length; ++edge_index) {
		pbqp_edge_t *edge_copy = NULL;
		pbqp_edge_t *edge      = node->edges[edge_index];
		bool         is_src    = edge->src == node;

		if (is_src) {
			unsigned other_index = edge->tgt->bucket_index;
			bool     is_copied   = other_index < node->bucket_index;

			if (is_copied) {
				pbqp_node_t *other_copy = new_bucket[other_index];
				unsigned     degree     = pbqp_node_get_degree(other_copy);
				unsigned     index;

				for (index = 0; index < degree; ++index) {
					if (other_copy->edges[index]->src == node) {
						edge_copy      = other_copy->edges[index];
						edge_copy->src = copy;
						break;
					}
				}
			} else {
				edge_copy = pbqp_edge_deep_copy(pbqp, edge, copy, edge->tgt);
			}
		} else {
			unsigned other_index = edge->src->bucket_index;
			bool     is_copied   = other_index < node->bucket_index;

			if (is_copied) {
				pbqp_node_t *other_copy = new_bucket[other_index];
				unsigned     degree     = pbqp_node_get_degree(other_copy);

				for (unsigned index = 0; index < degree; ++index) {
					if (other_copy->edges[index]->tgt == node) {
						edge_copy      = other_copy->edges[index];
						edge_copy->tgt = copy;
						break;
					}
				}
			} else {
				edge_copy = pbqp_edge_deep_copy(pbqp, edge, edge->src, copy);
			}
		}

		ARR_APP1(pbqp_edge_t *, copy->edges, edge_copy);
	}

	copy->costs        = vector_copy(pbqp, node->costs);
	copy->bucket_index = node->bucket_index;
	copy->solution     = node->solution;
	copy->index        = node->index;

	return copy;
}
