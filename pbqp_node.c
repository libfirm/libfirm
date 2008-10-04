#include "adt/array.h"

#include "assert.h"

#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

pbqp_node *alloc_node(pbqp *pbqp, vector *costs)
{
	pbqp_node *node = obstack_alloc(&pbqp->obstack, sizeof(*node));
	assert(node);

	node->edges = NEW_ARR_F(pbqp_edge *, 0);
	node->costs = vector_copy(pbqp, costs);

	return node;
}

void disconnect_edge(pbqp_node *node, pbqp_edge *edge)
{
	pbqp_edge **edges;
	unsigned    edge_index;
	unsigned    edge_len;

	edges = node->edges;
	edge_len = ARR_LEN(edges);

	for (edge_index = 0; edge_index < edge_len; ++edge_index) {
		pbqp_edge *edge_candidate = edges[edge_index];
		if (edge_candidate == edge) {
			edges[edge_index] = edges[edge_len - 1];
			ARR_SHRINKLEN(edges, (int)edge_len - 1);
			break;
		}
	}
}
