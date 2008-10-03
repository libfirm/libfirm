#ifndef KAPS_PBQP_EDGE_H
#define KAPS_PBQP_EDGE_H

#include "pbqp_t.h"

pbqp_edge *alloc_edge(pbqp *pbqp, int src_index, int tgt_index, matrix *costs);

#endif /* KAPS_PBQP_EDGE_H */
