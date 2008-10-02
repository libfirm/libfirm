#ifndef KAPS_KAPS_H
#define KAPS_KAPS_H

#include "pbqp_t.h"

/**
 * Create an empty PBQP instance with the given number of nodes.
 */
pbqp* alloc_pbqp(int number_nodes);

/**
 * Free the given PBQP.
 */
void free_pbqp(pbqp *pbqp);

/**
 * Add costs vector to given node.
 */
void add_node_costs(pbqp *pbqp, int node_index, vector *costs);

/**
 * Add costs matrix between given nodes.
 */
void add_edge_costs(pbqp *pbqp, int src_index, int tgt_index, matrix *costs);

#endif /* KAPS_KAPS_H */
