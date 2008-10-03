#ifndef KAPS_PBQP_EDGE_T_H
#define KAPS_PBQP_EDGE_T_H

#include "pbqp_t.h"

struct pbqp_edge {
	unsigned src;                      /* Source index. */
	unsigned tgt;                      /* Target index. */
	pbqp_matrix  *costs;                    /* Cost matrix. */
};

#endif /* KAPS_PBQP_EDGE_T_H */
