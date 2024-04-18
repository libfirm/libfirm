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
#ifndef KAPS_PBQP_NODE_H
#define KAPS_PBQP_NODE_H

#include "bucket_t.h"
#include "pbqp_t.h"

pbqp_node_t *alloc_node(pbqp_t *pbqp, unsigned node_index, vector_t *costs);

void disconnect_edge(pbqp_node_t *node, pbqp_edge_t *edge);

int is_connected(pbqp_node_t *node, pbqp_edge_t *edge);

unsigned pbqp_node_get_degree(pbqp_node_t *node);

pbqp_node_t *pbqp_node_deep_copy(pbqp_t *pbqp, pbqp_node_bucket_t new_bucket,
                                 pbqp_node_t *node);

#endif
