#include "kaps.h"

static pbqp_node *get_node(pbqp *pbqp, int index)
{
	return pbqp->nodes[index];
}

pbqp *alloc_pbqp(int number_nodes)
{
	pbqp* pbqp = xmalloc(sizeof(*pbqp));

	obstack_init(&pbqp->obstack);

	pbqp->solution = 0;
	pbqp->num_nodes = number_nodes;
	pbqp->nodes = obstack_alloc(&pbqp->obstack, number_nodes
			* sizeof(*pbqp->nodes));
}

void free_pbqp(pbqp *pbqp)
{
	obstack_free(&pbqp->obstack, NULL);
	xfree(pbqp);
}

void add_node_costs(pbqp *pbqp, int node_index, vector *costs)
{
	pbqp_node *node = get_node(pbqp, node_index);

	if (node == NULL) {
		node = alloc_node(pbqp, costs);
	} else {
		vector_add(node->costs, costs);
	}
}
