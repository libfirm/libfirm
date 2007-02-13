/*
 * Project:     libFIRM
 * File name:   ir/ir/iredges.h
 * Purpose:     Public header for the automatically updating outs.
 * Author:      Sebastian Hack
 * Created:     3.2.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * Public header for the automatically updating outs.
 * @author Sebastian Hack
 * @date 3.2.2005
 */

#ifndef _FIRM_IR_EDGES_H
#define _FIRM_IR_EDGES_H

#include "firm_types.h"
#include "iredgekinds.h"

#ifndef _IR_EDGE_TYPEDEF_
#define _IR_EDGE_TYPEDEF_
typedef struct _ir_edge_t ir_edge_t;
#endif

#if 0
#ifndef _IR_EDGE_KIND_TYPEDEF_
#define _IR_EDGE_KIND_TYPEDEF_
typedef enum _ir_edge_kind_t ir_edge_kind_t;
#endif /* _IR_EDGE_KIND_TYPEDEF_ */
#endif

/**
 * Get the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @param kind The kind of the edge.
 * @return The first out edge that points to this node.
 */
const ir_edge_t *get_irn_out_edge_first_kind(const ir_node *irn, ir_edge_kind_t kind);

/**
 * Get the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @return The next out edge in @p irn 's out list after @p last.
 */
const ir_edge_t *get_irn_out_edge_next(const ir_node *irn, const ir_edge_t *last);

/**
 * A convenience iteration macro over all out edges of a node.
 * @param irn  The node.
 * @param kind The edge's kind.
 * @param edge An ir_edge_t pointer which shall be set to the current
 * edge.
 */
#define foreach_out_edge_kind(irn, edge, kind) \
	for(edge = get_irn_out_edge_first_kind(irn, kind); edge; edge = get_irn_out_edge_next(irn, edge))

/**
 * A convenience iteration macro over all out edges of a node, which is safe
 * against alteration of the current edge.
 *
 * @param irn  The node.
 * @param edge An ir_edge_t pointer which shall be set to the current edge.
 * @param ne   The next edge, enables alteration safe edge processing.
 */
#define foreach_out_edge_kind_safe(irn, edge, ne, kind) \
	for((edge) = (get_irn_out_edge_first_kind(irn, kind)), (ne) = ((edge) ? (get_irn_out_edge_next(irn, edge)) : NULL); \
		edge; (edge) = (ne), (ne) = ((edge) ? (get_irn_out_edge_next(irn, edge)) : NULL))

/**
 * Convenience macro for normal out edges.
 */
#define foreach_out_edge(irn, edge)            foreach_out_edge_kind(irn, edge, EDGE_KIND_NORMAL)

/**
 * Convenience macro for normal out edges.
 */
#define foreach_out_edge_safe(irn, edge, tmp)  foreach_out_edge_kind_safe(irn, edge, tmp, EDGE_KIND_NORMAL)

/**
 * A convenience iteration macro for all control flow edges.
 */
#define foreach_block_succ(bl, edge)           foreach_out_edge_kind(bl, edge, EDGE_KIND_BLOCK)

/*
 * Get the source node of an edge.
 * @param edge The edge.
 * @return The source node of that edge.
 */
ir_node *get_edge_src_irn(const ir_edge_t *edge);

/**
 * Get the number of edges pointing to a node.
 * @param irn The node.
 * @return The number of edges pointing to this node.
 */
int get_irn_n_edges(const ir_node *irn);

/**
 * Get the position of an edge.
 * @param edge The edge.
 * @return The position in the in array of that edges source.
 */
extern int get_edge_src_pos(const ir_edge_t *edge);

/**
 * Get the edge object of an outgoing edge at a node.
 * @param   irg The graph, the node is in.
 * @param   irn The node at which the edge originates.
 * @param   pos The position of the edge.
 * @return      The corresponding edge object or NULL,
 *              if no such edge exists.
 */
const ir_edge_t *get_irn_edge_kind(ir_graph *irg, const ir_node *irn, int pos, ir_edge_kind_t kind);

/**
 * Get the number of registered out edges for a specific kind.
 * @param irn The node.
 * @param kind The kind.
 */
extern int get_irn_n_edges_kind(const ir_node *irn, ir_edge_kind_t kind);


/**
 * Check, if the out edges are activated.
 * @param irg The graph.
 * @param kind The edge kind.
 * @return 1, if the edges are present for the given irg, 0 if not.
 */
extern int edges_activated_kind(const ir_graph *irg, ir_edge_kind_t kind);

/**
 * Activate the edges for an irg.
 * @param irg The graph to activate the edges for.
 * @param kind The edge kind.
 */
extern void edges_activate_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Deactivate the edges for an irg.
 * @param irg The graph.
 * @param kind The edge kind.
 */
extern void edges_deactivate_kind(ir_graph *irg, ir_edge_kind_t kind);

extern void edges_reroute_kind(ir_node *old, ir_node *nw, ir_edge_kind_t kind, ir_graph *irg);

/**
 * Verifies the out edges of graph @p irg.
 * @return 1 if a problem was found, 0 otherwise
 */
int edges_verify(ir_graph *irg);

/************************************************************************/
/* Begin Old Interface                                                  */
/************************************************************************/

const ir_edge_t *get_irn_edge(ir_graph *irg, const ir_node *src, int pos);

#define edges_reroute(old, nw, irg)                     edges_reroute_kind(old, nw, EDGE_KIND_NORMAL, irg)
#define edges_activated(irg)                            (edges_activated_kind(irg, EDGE_KIND_NORMAL) && edges_activated_kind(irg, EDGE_KIND_BLOCK))

#ifndef get_irn_n_edges
#define get_irn_n_edges(irn)                            get_irn_n_edges_kind(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_irn_out_edge_first
#define get_irn_out_edge_first(irn)                     get_irn_out_edge_first_kind(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_block_succ_first
#define get_block_succ_first(irn)                       get_irn_out_edge_first_kind(irn, EDGE_KIND_BLOCK)
#endif

#ifndef get_block_succ_next
#define get_block_succ_next(irn, last)                  get_irn_out_edge_next(irn, last)
#endif

/**
* Activate all the edges for an irg.
* @param irg The graph to activate the edges for.
*/
extern void edges_activate(ir_graph *irg);

/**
* Deactivate all the edges for an irg.
* @param irg The graph.
*/
extern void edges_deactivate(ir_graph *irg);

extern int edges_assure(ir_graph *irg);

extern void edges_node_deleted(ir_node *irn, ir_graph *irg);

/**
* Notify normal and block edges.
*/
extern void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_graph *irg);

void edges_reset_private_data(ir_graph *irg, int offset, size_t size);

/************************************************************************/
/* End Old Interface                                                    */
/************************************************************************/


#endif /* _FIRM_IR_EDGES_H */
