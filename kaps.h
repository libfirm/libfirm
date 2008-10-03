#ifndef KAPS_KAPS_H
#define KAPS_KAPS_H

#include "pbqp_t.h"

/**
 * Create an empty PBQP instance with the given number of nodes.
 */
pbqp* alloc_pbqp(unsigned number_nodes);

/**
 * Free the given PBQP.
 */
void free_pbqp(pbqp *pbqp);

/**
 * Add costs vector to given node.
 */
void add_node_costs(pbqp *pbqp, unsigned node_index, vector *costs);

/**
 * Add costs matrix between given nodes.
 */
void add_edge_costs(pbqp *pbqp, unsigned src_index, unsigned tgt_index, pbqp_matrix *costs);

pbqp_edge *get_edge(pbqp *pbqp, unsigned src_index, unsigned tgt_index);
pbqp_node *get_node(pbqp *pbqp, unsigned index);

num get_solution(pbqp *pbqp);

void set_dumpfile(pbqp *pbqp, FILE *f);

#endif /* KAPS_KAPS_H */
