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
 * @brief   PBQP nodes.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 * @version $Id$
 */
#include "config.h"

#include "adt/array.h"

#include "assert.h"

#include "bucket.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

pbqp_node_t *alloc_node(pbqp_t *pbqp, unsigned node_index, vector_t *costs)
{
	pbqp_node_t *node = OALLOC(&pbqp->obstack, pbqp_node_t);
	assert(node);

	node->edges = NEW_ARR_F(pbqp_edge_t *, 0);
	node->costs = vector_copy(pbqp, costs);
	node->bucket_index = UINT_MAX;
	node->solution = UINT_MAX;
	node->index = node_index;

	return node;
}

int is_connected(pbqp_node_t *node, pbqp_edge_t *edge)
{
	pbqp_edge_t **edges;
	unsigned      edge_index;
	unsigned      edge_len;

	assert(node);
	assert(edge);

	if (edge->src != node && edge->tgt != node) return 0;

	edges = node->edges;
	edge_len = ARR_LEN(edges);

	for (edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge_t *edge_candidate = edges[edge_index];
		if (edge_candidate == edge) {
			return 1;
		}
	}

	return 0;
}

void disconnect_edge(pbqp_node_t *node, pbqp_edge_t *edge)
{
	pbqp_edge_t **edges;
	unsigned      edge_index;
	unsigned      edge_len;

	edges = node->edges;
	edge_len = ARR_LEN(edges);

	for (edge_index = 0; edge_index < edge_len; ++edge_index) {
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
	assert(node);
	return ARR_LEN(node->edges);
}

pbqp_node_t *pbqp_node_deep_copy(pbqp_t *pbqp, pbqp_node_bucket_t new_bucket,
                                 pbqp_node_t *node)
{
	unsigned     edge_index;
	unsigned     edge_length = pbqp_node_get_degree(node);
	pbqp_node_t *copy        = OALLOC(&pbqp->obstack, pbqp_node_t);

	assert(copy);

	copy->edges        = NEW_ARR_F(pbqp_edge_t *, 0);
	for (edge_index = 0; edge_index < edge_length; ++edge_index) {
		pbqp_edge_t *edge_copy = NULL;
		pbqp_edge_t *edge      = node->edges[edge_index];
		int          is_src    = edge->src == node;

		if (is_src) {
			unsigned other_index = edge->tgt->bucket_index;
			unsigned is_copied   = other_index < node->bucket_index;

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
			unsigned is_copied   = other_index < node->bucket_index;

			if (is_copied) {
				pbqp_node_t *other_copy = new_bucket[other_index];
				unsigned     degree     = pbqp_node_get_degree(other_copy);
				unsigned     index;

				for (index = 0; index < degree; ++index) {
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
	copy->index   = node->index;

	return copy;
}
