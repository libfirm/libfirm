#include "adt/array.h"

#include "kaps.h"
#include "matrix.h"
#include "pbqp_edge.h"
#include "pbqp_edge_t.h"
#include "pbqp_node.h"
#include "pbqp_node_t.h"
#include "vector.h"

pbqp_node *get_node(pbqp *pbqp, unsigned index)
{
	return pbqp->nodes[index];
}

pbqp_edge *get_edge(pbqp *pbqp, unsigned src_index, unsigned tgt_index)
{
	int i;
	int len;

	if (tgt_index < src_index) {
		return get_edge(pbqp, tgt_index, src_index);
	}

	pbqp_node *src_node = get_node(pbqp, src_index);
	assert(src_node);
	assert(get_node(pbqp, tgt_index));

	len = ARR_LEN(src_node->edges);

	for (i = 0; i < len; ++i) {
		pbqp_edge *cur_edge = src_node->edges[i];
		if (cur_edge->tgt->index == tgt_index) {
			return cur_edge;
		}
	}

	return NULL;
}

pbqp *alloc_pbqp(unsigned number_nodes)
{
	pbqp* pbqp = xmalloc(sizeof(*pbqp));

	obstack_init(&pbqp->obstack);

	pbqp->solution = 0;
	pbqp->num_nodes = number_nodes;
	pbqp->nodes = obstack_alloc(&pbqp->obstack, number_nodes
			* sizeof(*pbqp->nodes));

	return pbqp;
}

void free_pbqp(pbqp *pbqp)
{
	obstack_free(&pbqp->obstack, NULL);
	xfree(pbqp);
}

void add_node_costs(pbqp *pbqp, unsigned node_index, vector *costs)
{
	pbqp_node *node = get_node(pbqp, node_index);

	if (node == NULL) {
		node = alloc_node(pbqp, node_index, costs);
		pbqp->nodes[node_index] = node;
	} else {
		vector_add(node->costs, costs);
	}
}

void add_edge_costs(pbqp *pbqp, unsigned src_index, unsigned tgt_index,
		pbqp_matrix *costs)
{
	pbqp_edge *edge = get_edge(pbqp, src_index, tgt_index);

	if (edge == NULL) {
		edge = alloc_edge(pbqp, src_index, tgt_index, costs);
	} else {
		pbqp_matrix_add(edge->costs, costs);
	}
}

num get_solution(pbqp *pbqp)
{
	return pbqp->solution;
}

void set_dumpfile(pbqp *pbqp, FILE *f)
{
	assert(pbqp);
	pbqp->dump_file = f;
}
