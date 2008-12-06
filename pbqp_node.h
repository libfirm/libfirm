#ifndef KAPS_PBQP_NODE_H
#define KAPS_PBQP_NODE_H

#include "bucket_t.h"
#include "pbqp_t.h"

pbqp_node *alloc_node(pbqp *pbqp, unsigned node_index, vector *costs);

void disconnect_edge(pbqp_node *node, pbqp_edge *edge);

int is_connected(pbqp_node *node, pbqp_edge *edge);

unsigned pbqp_node_get_degree(pbqp_node *node);

pbqp_node *pbqp_node_deep_copy(pbqp *pbqp, pbqp_node_bucket new_bucket,
		pbqp_node *node);

#endif /* KAPS_PBQP_NODE_H */
