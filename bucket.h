#ifndef KAPS_BUCKET_H
#define KAPS_BUCKET_H

#include "bucket_t.h"

int edge_bucket_contains(pbqp_edge_bucket bucket, pbqp_edge *edge);
void edge_bucket_free(pbqp_edge_bucket *bucket);
unsigned edge_bucket_get_length(pbqp_edge_bucket bucket);
void edge_bucket_init(pbqp_edge_bucket *bucket);
void edge_bucket_insert(pbqp_edge_bucket *bucket, pbqp_edge *edge);
pbqp_edge *edge_bucket_pop(pbqp_edge_bucket *bucket);

int node_bucket_contains(pbqp_node_bucket bucket, pbqp_node *node);
void node_bucket_copy(pbqp_node_bucket *dst, pbqp_node_bucket src);
void node_bucket_deep_copy(pbqp *pbqp, pbqp_node_bucket *dst, pbqp_node_bucket src);
void node_bucket_free(pbqp_node_bucket *bucket);
unsigned node_bucket_get_length(pbqp_node_bucket bucket);
void node_bucket_init(pbqp_node_bucket *bucket);
void node_bucket_insert(pbqp_node_bucket *bucket, pbqp_node *node);
pbqp_node *node_bucket_pop(pbqp_node_bucket *bucket);
void node_bucket_remove(pbqp_node_bucket *bucket, pbqp_node *node);
void node_bucket_shrink(pbqp_node_bucket *bucket, unsigned len);
void node_bucket_update(pbqp *pbqp, pbqp_node_bucket bucket);

#endif /* KAPS_BUCKET_H */
