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
 * @version $Id$
 */
#ifndef KAPS_OPTIMAL_H
#define KAPS_OPTIMAL_H

#include "pbqp_t.h"

extern pbqp_edge **edge_bucket;
extern pbqp_node **node_buckets[4];
extern pbqp_node **reduced_bucket;
extern pbqp_node  *merged_node;

void apply_edge(pbqp *pbqp);

void apply_RI(pbqp *pbqp);
void apply_RII(pbqp *pbqp);
void apply_RM(pbqp *pbqp, pbqp_node *node);

void back_propagate(pbqp *pbqp);
num determine_solution(pbqp *pbqp);
void fill_node_buckets(pbqp *pbqp);
void free_buckets(void);
unsigned get_local_minimal_alternative(pbqp *pbqp, pbqp_node *node);
pbqp_node *get_node_with_max_degree(void);
void initial_simplify_edges(pbqp *pbqp);
void select_alternative(pbqp_node *node, unsigned selected_index);
void simplify_edge(pbqp *pbqp, pbqp_edge *edge);
void reorder_node_after_edge_deletion(pbqp_node *node);

int node_is_reduced(pbqp_node *node);

#endif /* KAPS_OPTIMAL_H */
