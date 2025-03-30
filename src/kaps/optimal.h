/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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

#endif
