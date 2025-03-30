/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Partitioned Boolean Quadratic Problem (PBQP) solver.
 * @date    02.10.2008
 * @author  Sebastian Buchwald
 */
#ifndef KAPS_KAPS_H
#define KAPS_KAPS_H

#include "pbqp_t.h"

/**
 * Create an empty PBQP instance with the given number of nodes.
 */
pbqp_t *alloc_pbqp(unsigned number_nodes);

/**
 * Free the given PBQP.
 */
void free_pbqp(pbqp_t *pbqp);

/**
 * Add costs vector to given node.
 */
void add_node_costs(pbqp_t *pbqp, unsigned node_index, vector_t *costs);

/**
 * Add costs matrix between given nodes.
 */
void add_edge_costs(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index,
                    pbqp_matrix_t *costs);

pbqp_edge_t *get_edge(pbqp_t *pbqp, unsigned src_index, unsigned tgt_index);
pbqp_node_t *get_node(pbqp_t *pbqp, unsigned index);

num get_node_solution(pbqp_t *pbqp, unsigned node_index);
num get_solution(pbqp_t *pbqp);

void set_dumpfile(pbqp_t *pbqp, FILE *f);

#endif
