/**
 * Common use interference graph.
 * Originally written by Sebastian Hack. Refactored into a seperate
 * source file and header by Kimon Hoffmann.
 * @author Sebastian Hack
 * @date 27.06.2005
 */
#ifndef _BEIFG_H_
#define _BEIFG_H_

typedef struct _be_if_graph_t be_if_graph_t;
typedef struct _be_if_node_t be_if_node_t;
typedef struct _be_if_edge_t be_if_edge_t;

/**
 * Creates a new interference graph with the specified number of
 * initial slots.
 * @param slots the initial number of slots used for hashing.
 * 		Must be greater than 0.
 * @return the created, empty interference graph.
 */
be_if_graph_t* be_ifg_new(int slots);

/**
 * Frees the passed interference graph an all contained nodes.
 * @param graph the interference graph that shall be freed. Please note
 * 		that after a call to this function all pointers to the graph,
 * 		its edges or its nodes are invalid.
 */
void be_ifg_free(be_if_graph_t* graph);

/**
 * Adds a new undirected interference edge between the two nodes
 * specified by their node numbers. If the corresponding nodes do
 * not yet exist in the iunterference grpah they are created pririor
 * to creation of the edge.
 * @param graph the graph to add the interference edge to.
 * @param source the key of the source node.
 * @param target the key of the target node.
 */
void be_ifg_add_interference(be_if_graph_t* graph, int source, int target);

/**
 * Returns the set of all edges within the specified
 * graph.
 * @param graph the graph the return the set of edges of.
 * @return A set of all edges within the specified graph as
 * 		be_if_edge_t instances.
 */
set* be_get_ifg_edges(const be_if_graph_t* graph);

/**
 * Returns the set of all nodes within the specified
 * graph.
 * @param graph the graph the return the set of nodes of.
 * @return A set of all nodes within the specified graph as
 * 		be_if_node_t instances.
 */
set* be_get_ifg_nodes(const be_if_graph_t* graph);

/**
 * Returns whether the specified graph alreadz contains an undirected
 * edge between the two passed nodes.
 * @param graph the graph to which containment both nodes @p n1 and
 * 		@p n2 belong.
 * @param n1 the one end of the edge to query the graph for.
 * @param n2 the other end of the edge.
 * @return TRUE if the graph contains an edge between @p n1 and @p n2,
 * 		otherwise FALSE.
 */
int be_ifg_has_edge(const be_if_graph_t* graph, const be_if_node_t* n1, const be_if_node_t* n2);

#define be_ifn_get_degree(ifnode) \
	pset_count(ifnode->neighb)

#define be_ifn_foreach_neighbour(ifnode, curr) \
	for (curr = pset_first(ifnode->neighbourNodes); curr; curr = pset_next(ifnode->neighbourNodes))

#define be_ifg_foreach_node(ifgraph, curr) \
	for (curr = set_first(ifgraph->nodes); curr; curr = set_next(ifgraph->nodes))

#define be_ifg_foreach_edge(ifgraph, curr) \
	for (curr = set_first(ifgraph->edges); curr; curr = set_next(ifgraph->edges))


#endif /*_BEIFG_H_*/
