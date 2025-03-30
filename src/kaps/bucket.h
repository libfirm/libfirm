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
#ifndef KAPS_BUCKET_H
#define KAPS_BUCKET_H

#include "bucket_t.h"

int edge_bucket_contains(pbqp_edge_bucket_t bucket, pbqp_edge_t *edge);
void edge_bucket_free(pbqp_edge_bucket_t *bucket);
unsigned edge_bucket_get_length(pbqp_edge_bucket_t bucket);
void edge_bucket_init(pbqp_edge_bucket_t *bucket);
void edge_bucket_insert(pbqp_edge_bucket_t *bucket, pbqp_edge_t *edge);
pbqp_edge_t *edge_bucket_pop(pbqp_edge_bucket_t *bucket);

int node_bucket_contains(pbqp_node_bucket_t bucket, pbqp_node_t *node);
void node_bucket_copy(pbqp_node_bucket_t *dst, pbqp_node_bucket_t src);
void node_bucket_deep_copy(pbqp_t *pbqp, pbqp_node_bucket_t *dst, pbqp_node_bucket_t src);
void node_bucket_free(pbqp_node_bucket_t *bucket);
unsigned node_bucket_get_length(pbqp_node_bucket_t bucket);
void node_bucket_init(pbqp_node_bucket_t *bucket);
void node_bucket_insert(pbqp_node_bucket_t *bucket, pbqp_node_t *node);
pbqp_node_t *node_bucket_pop(pbqp_node_bucket_t *bucket);
void node_bucket_remove(pbqp_node_bucket_t *bucket, pbqp_node_t *node);
void node_bucket_shrink(pbqp_node_bucket_t *bucket, unsigned len);
void node_bucket_update(pbqp_t *pbqp, pbqp_node_bucket_t bucket);

#endif
