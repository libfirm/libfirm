/**
 * Common use interference graph.
 * Originally written by Sebastian Hack. Refactored into a seperate
 * source file and header by Kimon Hoffmann.
 * @author Sebastian Hack
 * @date 27.06.2005
 */
#ifndef _BEIFG_T_H_
#define _BEIFG_T_H_

#include "pset.h"
#include "set.h"

#include "beifg.h"

/**
 * Structure that represents a single interference graph.
 */
struct _be_if_graph_t {
	/**
	 * Set of nodes in this graph as be_if_node_t structs.
	 */
	set* nodes;
	/**
	 * Set of edges in this graph as be_if_edge_t structs.
	 */
	set* edges;
};

/**
 * Node type contained in i0nterference graphs.
 */
struct _be_if_node_t {
	int nodeNumber;
	pset* neighbourNodes;
};

/**
 * Edge type contained in interference graphs.
 */
struct _be_if_edge_t {
	int sourceNode;
	int targetNode;
};

#endif /*_BEIFG_T_H_*/
