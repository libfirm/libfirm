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
