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
 * @brief   Heuristic PBQP solver.
 * @date    28.12.2009
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_OPTIMAL_H
#define KAPS_OPTIMAL_H

#include "pbqp_t.h"

extern pbqp_edge_t **edge_bucket;
extern pbqp_node_t **node_buckets[4];
extern pbqp_node_t **reduced_bucket;
extern pbqp_node_t  *merged_node;

void apply_edge(pbqp_t *pbqp);

void apply_RI(pbqp_t *pbqp);
void apply_RII(pbqp_t *pbqp);
void apply_RM(pbqp_t *pbqp, pbqp_node_t *node);

void back_propagate(pbqp_t *pbqp);
num determine_solution(pbqp_t *pbqp);
void fill_node_buckets(pbqp_t *pbqp);
void free_buckets(void);
unsigned get_local_minimal_alternative(pbqp_t *pbqp, pbqp_node_t *node);
pbqp_node_t *get_node_with_max_degree(void);
void initial_simplify_edges(pbqp_t *pbqp);
void select_alternative(pbqp_node_t *node, unsigned selected_index);
void simplify_edge(pbqp_t *pbqp, pbqp_edge_t *edge);
void reorder_node_after_edge_deletion(pbqp_node_t *node);
void reorder_node_after_edge_insertion(pbqp_node_t *node);

int node_is_reduced(pbqp_node_t *node);

#endif /* KAPS_OPTIMAL_H */
