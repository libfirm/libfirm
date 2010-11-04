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
 * @brief   Buckets for nodes and edges.
 * @date    30.11.2008
 * @author  Sebastian Buchwald
 * @version $Id$
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

#endif /* KAPS_BUCKET_H */
