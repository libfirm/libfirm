
/**
 * everlasting outs.
 * @author Sebastian Hack
 * @date 15.1.2005
 */

#ifndef _FIRM_EDGES_T_H
#define _FIRM_EDGES_T_H

#include "config.h"
#include "debug.h"

#include "set.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iredges.h"

#define DBG_EDGES 				"edges"

/**
 * An edge.
 */
typedef struct _ir_edge_t {
#ifdef DEBUG_libfirm
	long src_nr;						/**< The node number of the source node. */
#endif
	 ir_node *src;		/**< The source node of the edge. */
	int pos;								/**< The position of the edge at @p src. */
	struct list_head list;  /**< The list head to queue all out edges at a node. */
	unsigned invalid : 1;		/**< edges that are removed are marked invalid. */
	unsigned present : 1;		/**< Used by the verifier. Don't rely on its content. */
} ir_edge_t;

/** Accessor for private irn info. */
#define _get_irn_edge_info(irn) ((irn_edge_info_t *) &(irn)->edge_info)

/** Accessor for private irg info. */
#define _get_irg_edge_info(irg) ((irg_edge_info_t *) &(irg)->edge_info)

/**
 * Convenience macro to get the outs_head from a irn_edge_info_t
 * struct.
 */
#define _get_irn_outs_head(irn) (&_get_irn_edge_info(irn)->outs_head)

/**
 * Get the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @return The first out edge that points to this node.
 */
static INLINE const ir_edge_t *_get_irn_out_edge_first(const ir_node *irn)
{
	struct list_head *head = _get_irn_outs_head(irn);
	return list_empty(head) ? NULL : list_entry(head->next, ir_edge_t, list);
}

/**
 * Get the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @return The next out edge in @p irn 's out list after @p last.
 */
static INLINE const ir_edge_t *_get_irn_out_edge_next(const ir_node *irn, const ir_edge_t *last)
{
	struct list_head *next = last->list.next;
	return next == _get_irn_outs_head(irn) ? NULL : list_entry(next, ir_edge_t, list);
}

/**
 * A convenience iteration macro over all out edges of a node.
 * @param irn The node.
 * @param edge An @c ir_edge_t pointer which shall be set to the current
 * edge.
 */
#define foreach_out_edge(irn,edge) \
	for(edge = get_irn_out_edge_first(irn); edge; edge = get_irn_out_edge_next(irn, edge))

/**
 * Get the source node of an edge.
 * @param edge The edge.
 * @return The source node of that edge.
 */
static INLINE  ir_node *_get_edge_src_irn(const ir_edge_t *edge)
{
	return edge ? edge->src : NULL;
}

/**
 * Get the position of an edge.
 * @param edge.
 * @return The position in the in array of that edges source.
 */
static INLINE int _get_edge_src_pos(const ir_edge_t *edge)
{
	return edge ? edge->pos : -1;
}

static INLINE int _edges_activated(const ir_graph *irg)
{
	return _get_irg_edge_info(irg)->activated;
}

void edges_reroute(ir_node *old, ir_node *nw, ir_graph *irg);

void edges_init_graph(ir_graph *irg);

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_graph *irg);

void edges_node_deleted(ir_node *old, ir_graph *irg);

void edges_invalidate(ir_node *irn, ir_graph *irg);

/**
 * Initialize the out edges.
 * This must be called before firm is initialized.
 */
extern void init_edges(void);

#define get_irn_out_edge_first(irn)					_get_irn_out_edge_first(irn)
#define get_irn_out_edge_next(irn,last)			_get_irn_out_edge_next(irn, last)
#define get_edge_src_irn(edge)							_get_edge_src_irn(edge)
#define get_edge_src_pos(edge)							_get_edge_src_pos(edge)
#define edges_activated(irg)								_edges_activated(irg)

#endif /* _FIRM_EDGES_T_H */
