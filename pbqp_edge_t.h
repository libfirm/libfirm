#ifndef KAPS_PBQP_EDGE_T_H
#define KAPS_PBQP_EDGE_T_H

#include "pbqp_t.h"

struct pbqp_edge {
	pbqp_node *src;                    /* Source node. */
	pbqp_node *tgt;                    /* Target node. */
	matrix *costs;                     /* Cost matrix. */
};

#endif /* KAPS_PBQP_EDGE_T_H */
