#ifndef KAPS_PBQP_NODE_T_H
#define KAPS_PBQP_NODE_T_H

#include "pbqp_t.h"

struct pbqp_node {
	pbqp_edge **edges;
	vector     *costs;
	unsigned    bucket_index;
	unsigned    solution;
};

#endif /* KAPS_PBQP_NODE_T_H */
