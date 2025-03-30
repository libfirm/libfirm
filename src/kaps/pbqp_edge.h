/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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

pbqp_edge_t *alloc_edge(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index,
                        pbqp_matrix_t *costs);

pbqp_edge_t *pbqp_edge_deep_copy(pbqp_t *pbqp, pbqp_edge_t *edge,
                                 pbqp_node_t *src_node, pbqp_node_t *tgt_node);

void delete_edge(pbqp_edge_t *edge);
unsigned is_deleted(pbqp_edge_t *edge);

#endif
