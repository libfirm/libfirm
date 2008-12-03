#ifndef KAPS_PBQP_EDGE_H
#define KAPS_PBQP_EDGE_H

#include "pbqp_t.h"

pbqp_edge *alloc_edge(pbqp *pbqp, int src_index, int tgt_index, pbqp_matrix *costs);

pbqp_edge *pbqp_edge_deep_copy(pbqp *pbqp, pbqp_edge *edge);

void delete_edge(pbqp_edge *edge);

#endif /* KAPS_PBQP_EDGE_H */
