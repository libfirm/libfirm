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
 * @brief   PBQP edges.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_PBQP_EDGE_H
#define KAPS_PBQP_EDGE_H

#include "pbqp_t.h"

pbqp_edge_t *alloc_edge(pbqp_t *pbqp, int src_index, int tgt_index,
                        pbqp_matrix_t *costs);

pbqp_edge_t *pbqp_edge_deep_copy(pbqp_t *pbqp, pbqp_edge_t *edge,
                                 pbqp_node_t *src_node, pbqp_node_t *tgt_node);

void delete_edge(pbqp_edge_t *edge);
unsigned is_deleted(pbqp_edge_t *edge);

#endif /* KAPS_PBQP_EDGE_H */
