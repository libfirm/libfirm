/**
 * Common use interference graph.
 * Originally written by Sebastian Hack. Refactored into a seperate
 * source file and header by Kimon Hoffmann.
 * @author Sebastian Hack
 * @date 27.06.2005
 */

#include <malloc.h>
#include "debug.h"

#include "beifg_t.h"

#define IF_EDGE_HASH(e) ((e)->sourceNode)
#define IF_NODE_HASH(n) ((n)->nodeNumber)
#define IF_NODE_NEIGHBOUR_SLOTS 8

static int if_edge_cmp(const void* p1, const void* p2, size_t size) {
	const be_if_edge_t* e1 = p1;
	const be_if_edge_t* e2 = p2;

	return !((e1->sourceNode == e2->sourceNode) && (e1->targetNode == e2->targetNode));
}

static int if_node_cmp(const void* p1, const void* p2, size_t size) {
	const be_if_node_t* n1 = p1;
	const be_if_node_t* n2 = p2;

	return (n1->nodeNumber != n2->nodeNumber);
}

static INLINE be_if_edge_t *edge_init(be_if_edge_t* edge, int source, int target) {
	/* Bring the smaller entry to src. */
	if (source > target) {
		edge->sourceNode = target;
		edge->targetNode = source;
	} else {
		edge->sourceNode = source;
		edge->targetNode = target;
	}
	return edge;
}

be_if_graph_t* be_ifg_new(int slots) {
	be_if_graph_t* graph = malloc(sizeof(*graph));
	graph->edges = new_set(if_edge_cmp, slots);
	graph->nodes = new_set(if_node_cmp, slots);
	return graph;
}

void be_ifg_free(be_if_graph_t* graph) {
	be_if_node_t* ifn;
	for (ifn = set_first(graph->nodes); ifn; ifn = set_next(graph->nodes)) {
		free(ifn->neighbourNodes);
		ifn->neighbourNodes = 0;
	}
	free(graph->nodes);
	graph->nodes = 0;
	free(graph->edges);
	graph->edges = 0;
	free(graph);
}

void be_ifg_add_interference(be_if_graph_t* graph, int source, int target) {
	/* insert edge */
	be_if_edge_t edge;
	edge_init(&edge, source, target);
	set_insert(graph->edges, &edge, sizeof(edge), IF_EDGE_HASH(&edge));

	/* insert nodes */
	be_if_node_t node;
	node.nodeNumber = source;
	node.neighbourNodes = pset_new_ptr(IF_NODE_NEIGHBOUR_SLOTS);
	be_if_node_t* sourceNode = set_insert(graph->nodes, &node, sizeof(node), IF_NODE_HASH(&node));
	node.nodeNumber = target;
	node.neighbourNodes = pset_new_ptr(IF_NODE_NEIGHBOUR_SLOTS);
	be_if_node_t* targetNode = set_insert(graph->nodes, &node, sizeof(node), IF_NODE_HASH(&node));

	/* insert neighbors into nodes */
	pset_insert_ptr(sourceNode->neighbourNodes, targetNode);
	pset_insert_ptr(targetNode->neighbourNodes, sourceNode);
}

static INLINE int are_connected(const be_if_graph_t* graph, int source, int target) {
	be_if_edge_t edge;
	edge_init(&edge, source, target);
	return set_find(graph->edges, &edge, sizeof(edge), IF_EDGE_HASH(&edge)) != NULL;
}

int be_ifg_has_edge(const be_if_graph_t* graph, const be_if_node_t* n1, const be_if_node_t* n2) {
	return are_connected(graph, n1->nodeNumber, n2->nodeNumber);
}
