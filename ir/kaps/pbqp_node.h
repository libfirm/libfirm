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

#endif /* KAPS_PBQP_NODE_H */
