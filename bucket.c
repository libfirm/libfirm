#include "adt/array.h"

#include "bucket.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"

int edge_bucket_contains(pbqp_edge_bucket bucket, pbqp_edge *edge)
{
	assert(edge);

	return edge->bucket_index < edge_bucket_get_length(bucket)
			&& bucket[edge->bucket_index] == edge;
}

void edge_bucket_free(pbqp_edge_bucket *bucket)
{
	DEL_ARR_F(*bucket);
	*bucket = NULL;
}

unsigned edge_bucket_get_length(pbqp_edge_bucket bucket)
{
	return ARR_LEN(bucket);
}

void edge_bucket_init(pbqp_edge_bucket *bucket)
{
	*bucket = NEW_ARR_F(pbqp_edge *, 0);
}

void edge_bucket_insert(pbqp_edge_bucket *bucket, pbqp_edge *edge)
{
	edge->bucket_index = edge_bucket_get_length(*bucket);
	ARR_APP1(pbqp_edge *, *bucket, edge);
}

pbqp_edge *edge_bucket_pop(pbqp_edge_bucket *bucket)
{
	unsigned   bucket_len = edge_bucket_get_length(*bucket);
	pbqp_edge *edge;

	assert(bucket_len > 0);

	edge = (*bucket)[bucket_len - 1];

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	edge->bucket_index = UINT_MAX;

	return edge;
}

void node_bucket_shrink(pbqp_node_bucket *bucket, unsigned len)
{
	ARR_SHRINKLEN(*bucket, len);
}

int node_bucket_contains(pbqp_node_bucket bucket, pbqp_node *node)
{
	assert(node);

	return node->bucket_index < node_bucket_get_length(bucket)
			&& bucket[node->bucket_index] == node;
}

void node_bucket_copy(pbqp *pbqp, pbqp_node_bucket *dst, pbqp_node_bucket src)
{
	unsigned src_index;
	unsigned src_length = node_bucket_get_length(src);

	for (src_index = 0; src_index < src_length; ++src_index) {
		node_bucket_insert(dst, src[src_index]);
		pbqp->nodes[src[src_index]->index] = src[src_index];
	}
}

void node_bucket_free(pbqp_node_bucket *bucket)
{
	DEL_ARR_F(*bucket);
	*bucket = NULL;
}

unsigned node_bucket_get_length(pbqp_node_bucket bucket)
{
	return ARR_LEN(bucket);
}

void node_bucket_init(pbqp_node_bucket *bucket)
{
	*bucket = NEW_ARR_F(pbqp_node *, 0);
}

void node_bucket_insert(pbqp_node_bucket *bucket, pbqp_node *node)
{
	node->bucket_index = node_bucket_get_length(*bucket);
	ARR_APP1(pbqp_node *, *bucket, node);
}

pbqp_node *node_bucket_pop(pbqp_node_bucket *bucket)
{
	unsigned   bucket_len = node_bucket_get_length(*bucket);
	pbqp_node *node;

	assert(bucket_len > 0);

	node = (*bucket)[bucket_len - 1];
	assert(node);

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	node->bucket_index = UINT_MAX;

	return node;
}

void node_bucket_remove(pbqp_node_bucket *bucket, pbqp_node *node)
{
	unsigned   bucket_len = node_bucket_get_length(*bucket);
	unsigned   node_index;
	pbqp_node *other;

	assert(node);
	assert(node_bucket_contains(*bucket, node));
	assert(bucket_len > 0);

	node_index            = node->bucket_index;
	other                 = (*bucket)[bucket_len - 1];
	other->bucket_index   = node_index;
	(*bucket)[node_index] = other;

	ARR_SHRINKLEN(*bucket, (int)bucket_len - 1);
	node->bucket_index = UINT_MAX;
}

void node_bucket_deep_copy(pbqp *pbqp, pbqp_node_bucket *dst, pbqp_node_bucket src)
{
	unsigned          bucket_index;
	unsigned          bucket_length;

	bucket_length = node_bucket_get_length(src);

	for (bucket_index = 0; bucket_index < bucket_length; ++bucket_index) {
		node_bucket_insert(dst, pbqp_node_deep_copy(pbqp, *dst, src[bucket_index]));
	}
}
