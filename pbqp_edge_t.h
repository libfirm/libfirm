#ifndef KAPS_PBQP_EDGE_T_H
#define KAPS_PBQP_EDGE_T_H

#include "pbqp_t.h"

struct pbqp_edge {
	pbqp_node   *src;                  /* Source index. */
	pbqp_node   *tgt;                  /* Target index. */
	pbqp_matrix *costs;                /* Cost matrix. */
	unsigned     bucket_index;         /* Index of edge bucket. */
};

#endif /* KAPS_PBQP_EDGE_T_H */
