/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Buckets for nodes and edges.
 * @date    30.11.2008
 * @author  Sebastian Buchwald
 */
#include "bucket.h"

#include "adt/array.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"

int edge_bucket_contains(pbqp_edge_bucket_t bucket, pbqp_edge_t *edge)
{
	return edge->bucket_index < edge_bucket_get_length(bucket)
	       && bucket[edge->bucket_index] == edge;
}

void edge_bucket_free(pbqp_edge_bucket_t *bucket)
{
	DEL_ARR_F(*bucket);
	*bucket = NULL;
}

unsigned edge_bucket_get_length(pbqp_edge_bucket_t bucket)
{
	return ARR_LEN(bucket);
}

void edge_bucket_init(pbqp_edge_bucket_t *bucket)
{
	*bucket = NEW_ARR_F(pbqp_edge_t *, 0);
}

void edge_bucket_insert(pbqp_edge_bucket_t *bucket, pbqp_edge_t *edge)
{
	edge->bucket_index = edge_bucket_get_length(*bucket);
	ARR_APP1(pbqp_edge_t *, *bucket, edge);
}

pbqp_edge_t *edge_bucket_pop(pbqp_edge_bucket_t *bucket)
{
	unsigned bucket_len = edge_bucket_get_length(*bucket);

	assert(bucket_len > 0);

	pbqp_edge_t *edge = (*bucket)[bucket_len - 1];

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	edge->bucket_index = UINT_MAX;

	return edge;
}

void node_bucket_shrink(pbqp_node_bucket_t *bucket, unsigned len)
{
	ARR_SHRINKLEN(*bucket, (int)len);
}

int node_bucket_contains(pbqp_node_bucket_t bucket, pbqp_node_t *node)
{
	return node->bucket_index < node_bucket_get_length(bucket)
	       && bucket[node->bucket_index] == node;
}

void node_bucket_copy(pbqp_node_bucket_t *dst, pbqp_node_bucket_t src)
{
	unsigned src_length = node_bucket_get_length(src);

	for (unsigned src_index = 0; src_index < src_length; ++src_index) {
		node_bucket_insert(dst, src[src_index]);
	}
}

void node_bucket_update(pbqp_t *pbqp, pbqp_node_bucket_t bucket)
{
	unsigned length = node_bucket_get_length(bucket);

	for (unsigned index = 0; index < length; ++index) {
		pbqp->nodes[bucket[index]->index] = bucket[index];
	}
}

void node_bucket_free(pbqp_node_bucket_t *bucket)
{
	DEL_ARR_F(*bucket);
	*bucket = NULL;
}

unsigned node_bucket_get_length(pbqp_node_bucket_t bucket)
{
	return ARR_LEN(bucket);
}

void node_bucket_init(pbqp_node_bucket_t *bucket)
{
	*bucket = NEW_ARR_F(pbqp_node_t*, 0);
}

void node_bucket_insert(pbqp_node_bucket_t *bucket, pbqp_node_t *node)
{
	node->bucket_index = node_bucket_get_length(*bucket);
	ARR_APP1(pbqp_node_t *, *bucket, node);
}

pbqp_node_t *node_bucket_pop(pbqp_node_bucket_t *bucket)
{
	unsigned bucket_len = node_bucket_get_length(*bucket);

	assert(bucket_len > 0);

	pbqp_node_t *node = (*bucket)[bucket_len - 1];

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	node->bucket_index = UINT_MAX;

	return node;
}

void node_bucket_remove(pbqp_node_bucket_t *bucket, pbqp_node_t *node)
{
	assert(node_bucket_contains(*bucket, node));

	unsigned     bucket_len = node_bucket_get_length(*bucket);
	unsigned     node_index = node->bucket_index;
	pbqp_node_t *other      = (*bucket)[bucket_len - 1];

	other->bucket_index   = node_index;
	(*bucket)[node_index] = other;

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	node->bucket_index = UINT_MAX;
}

void node_bucket_deep_copy(pbqp_t *pbqp, pbqp_node_bucket_t *dst, pbqp_node_bucket_t src)
{
	unsigned bucket_length = node_bucket_get_length(src);

	for (unsigned bucket_index = 0; bucket_index < bucket_length; ++bucket_index) {
		node_bucket_insert(dst, pbqp_node_deep_copy(pbqp, *dst, src[bucket_index]));
	}
}
