
/**
 * Public header for the automatically updating outs.
 * @author Sebastian Hack
 * @date 3.2.2005
 */

#ifndef _FIRM_EDGES_H
#define _FIRM_EDGES_H

#include "irnode.h"

/**
 * Get the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @return The first out edge that points to this node.
 */
const struct _ir_edge_t *(get_irn_out_edge_first)(const ir_node *irn);

/**
 * Get the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @return The next out edge in @p irn 's out list after @p last.
 */
const struct _ir_edge_t *(get_irn_out_edge_next)(const ir_node *irn,
		const struct _ir_edge_t *last);

/**
 * A convenience iteration macro over all out edges of a node.
 * @param irn The node.
 * @param edge An @c ir_edge_t pointer which shall be set to the current
 * edge.
 */
#define foreach_out_edge(irn,edge) \
	for(edge = get_irn_out_edge_first(irn); edge; edge = get_irn_out_edge_next(irn, edge))

/*
 * Get the source node of an edge.
 * @param edge The edge.
 * @return The source node of that edge.
 */
ir_node *(get_edge_src_irn)(const struct _ir_edge_t *edge);

/**
 * Get the position of an edge.
 * @param edge.
 * @return The position in the in array of that edges source.
 */
extern int (get_edge_src_pos)(const struct _ir_edge_t *edge);

extern int (edges_activated)(const ir_graph *irg);

extern void (edges_activate)(ir_graph *irg);

extern void (edges_deactivate)(ir_graph *irg);


#endif /* _FIRM_EDGES_H */
