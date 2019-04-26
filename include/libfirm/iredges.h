/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Public header for the automatically updating outs.
 * @author  Sebastian Hack
 * @date    3.2.2005
 */
#ifndef FIRM_IR_IREDGES_H
#define FIRM_IR_IREDGES_H

#include "firm_types.h"
#include "iredgekinds.h"

#include "begin.h"

/**
 * @ingroup irana
 * @defgroup iredges Dynamic Reverse Edges
 * @{
 */

/**
 * Returns the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @param kind The kind of the edge.
 * @return The first out edge that points to this node.
 */
FIRM_API const ir_edge_t *get_irn_out_edge_first_kind(const ir_node *irn,
                                                      ir_edge_kind_t kind);

/**
 * Returns the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @return The first out edge that points to this node.
 */
FIRM_API const ir_edge_t *get_irn_out_edge_first(const ir_node *irn);

/**
 * Returns the first edge pointing to a successor block.
 *
 * You can navigate the list with the usual get_irn_out_edge_next().
 * @param block  the Block
 * @return first block successor edge
 */
FIRM_API const ir_edge_t *get_block_succ_first(const ir_node *block);

/**
 * Returns the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @param kind the kind of edge that are iterated
 * @return The next out edge in @p irn 's out list after @p last.
 */
FIRM_API const ir_edge_t *get_irn_out_edge_next(const ir_node *irn,
                                                const ir_edge_t *last,
                                                ir_edge_kind_t kind);

/**
 * A convenience iteration macro over all out edges of a node.
 * @param irn  The node.
 * @param kind The edge's kind.
 * @param edge An ir_edge_t pointer which shall be set to the current
 * edge.
 */
#define foreach_out_edge_kind(irn, edge, kind) \
	for (ir_edge_t const *edge = get_irn_out_edge_first_kind(irn, kind); edge; edge = get_irn_out_edge_next(irn, edge, kind))

/**
 * A convenience iteration macro over all out edges of a node, which is safe
 * against alteration of the current edge.
 *
 * @param irn  The node.
 * @param edge An ir_edge_t pointer which shall be set to the current edge.
 * @param kind The kind of the edge.
 */
#define foreach_out_edge_kind_safe(irn, edge, kind) \
	for (ir_edge_t const *edge = get_irn_out_edge_first_kind((irn), (kind)), *edge##__next; edge; edge = edge##__next) \
		if (edge##__next = get_irn_out_edge_next((irn), edge, (kind)), 0) {} else

/**
 * Convenience macro for normal out edges.
 */
#define foreach_out_edge(irn, edge)       foreach_out_edge_kind(irn, edge, EDGE_KIND_NORMAL)

/**
 * Convenience macro for normal out edges.
 */
#define foreach_out_edge_safe(irn, edge)  foreach_out_edge_kind_safe(irn, edge, EDGE_KIND_NORMAL)

/**
 * A convenience iteration macro for all control flow edges.
 */
#define foreach_block_succ(bl, edge)      foreach_out_edge_kind(bl, edge, EDGE_KIND_BLOCK)

/**
 * A convenience iteration macro for all control flow edges.
 */
#define foreach_block_succ_safe(bl, edge) foreach_out_edge_kind_safe(bl, edge, EDGE_KIND_BLOCK)

/**
 * Returns the source node of an edge.
 * @param edge The edge.
 * @return The source node of that edge.
 */
FIRM_API ir_node *get_edge_src_irn(const ir_edge_t *edge);

/**
 * Returns the position of an edge.
 * @param edge The edge.
 * @return The position in the in array of that edges source.
 */
FIRM_API int get_edge_src_pos(const ir_edge_t *edge);

/**
 * Returns the number of registered out edges for a specific kind.
 * @param irn The node.
 * @param kind The kind.
 */
FIRM_API int get_irn_n_edges_kind(const ir_node *irn, ir_edge_kind_t kind);

/**
 * Returns the number of registered out edges with EDGE_KIND_NORMAL
 * @param irn The node.
 */
FIRM_API int get_irn_n_edges(const ir_node *irn);

/**
 * Checks if the out edges are activated.
 *
 * @param irg   The graph.
 * @param kind  The edge kind.
 *
 * @return 1, if the edges are present for the given irg, 0 if not.
 */
FIRM_API int edges_activated_kind(const ir_graph *irg, ir_edge_kind_t kind);

/**
 * Checks if out edges with EDG_KIND_NORMAL and EDGE_KIND_BLOCK are activated.
 * @param irg   The graph.
 * @return 1, if the edges are present for the given irg, 0 if not.
 */
FIRM_API int edges_activated(const ir_graph *irg);

/**
 * Activates the edges for an irg.
 *
 * @param irg   The graph to activate the edges for.
 * @param kind  The edge kind.
 */
FIRM_API void edges_activate_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Deactivates the edges for an irg.
 *
 * @param irg   The graph.
 * @param kind  The edge kind.
 */
FIRM_API void edges_deactivate_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Reroutes edges of a specified kind from an old node to a new one.
 *
 * @param old   the old node
 * @param nw    the new node
 * @param kind  the edge kind
 */
FIRM_API void edges_reroute_kind(ir_node *old, ir_node *nw, ir_edge_kind_t kind);

/**
 * Reroutes edges of EDGE_KIND_NORMAL from an old node to a new one.
 *
 * @param old   the old node
 * @param nw    the new node
 */
FIRM_API void edges_reroute(ir_node *old, ir_node *nw);

/**
 * reroutes (normal) edges from an old node to a new node, except for the
 * @p exception node which keeps its input even if it is @p old.
 */
FIRM_API void edges_reroute_except(ir_node *old, ir_node *nw,
                                   ir_node *exception);

/**
 * Verifies the out edges of graph @p irg.
 * @return 0 if a problem was found
 */
FIRM_API int edges_verify(ir_graph *irg);

/**
 * Verifies a certrain kind of out edges of graph @p irg.
 * @returns 0 if a problem was found
 */
FIRM_API int edges_verify_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Sets edge verification flag.
 */
FIRM_API void edges_init_dbg(int do_dbg);

/**
 * Activates data and block edges for an irg.
 * If the irg phase is phase_backend, Dependence edges are
 * additionally activated.
 *
 * @param irg  The graph to activate the edges for.
 */
FIRM_API void edges_activate(ir_graph *irg);

/**
 * Deactivates data and block edges for an irg.
 * If the irg phase is phase_backend, Dependence edges are
 * additionally deactivated.
 * @param irg  The graph.
 */
FIRM_API void edges_deactivate(ir_graph *irg);

/**
 * Ensures that edges are activated.
 *
 * @param irg  the IR graph
 */
FIRM_API void assure_edges(ir_graph *irg);

/**
 * Ensures that edges of a given kind are activated.
 *
 * @param irg   the IR graph
 * @param kind  the edge kind
 */
FIRM_API void assure_edges_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Walks only over Block nodes in the graph. Uses the block visited
 * flag, so that it can be interleaved with another walker.
 *
 * @param block  the start block
 * @param pre    the pre visit function
 * @param post   the post visit function
 * @param env    the environment for the walker
 */
FIRM_API void irg_block_edges_walk(ir_node *block, irg_walk_func *pre,
                                   irg_walk_func *post, void *env);

/** Graph walker following #EDGE_KIND_NORMAL edges. */
FIRM_API void irg_walk_edges(ir_node *start, irg_walk_func *pre,
                             irg_walk_func *post, void *env);

/** @} */

#include "end.h"

#endif
