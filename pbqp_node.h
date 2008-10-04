#ifndef KAPS_PBQP_NODE_H
#define KAPS_PBQP_NODE_H

#include "pbqp_t.h"

pbqp_node *alloc_node(pbqp *pbqp, unsigned node_index, vector *costs);

void disconnect_edge(pbqp_node *node, pbqp_edge *edge);

#endif /* KAPS_PBQP_NODE_H */
