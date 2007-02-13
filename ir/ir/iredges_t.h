/*
 * Project:     libFIRM
 * File name:   ir/ir/iredges_t.h
 * Purpose:     Everlasting outs -- private header.
 * Author:      Sebastian Hack
 * Modified by: Andreas Schoesser
 * Created:     15.01.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * everlasting outs.
 * @author Sebastian Hack
 * @date 15.1.2005
 */

#ifndef _FIRM_EDGES_T_H
#define _FIRM_EDGES_T_H

#include "firm_config.h"
#include "debug.h"

#include "set.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iredgekinds.h"
#include "iredges.h"

#define DBG_EDGES  "firm.ir.edges"

/**
 * An edge.
 */
struct _ir_edge_t {
  ir_node *src;           /**< The source node of the edge. */
  int pos;                /**< The position of the edge at @p src. */
  unsigned invalid : 1;   /**< edges that are removed are marked invalid. */
  unsigned present : 1;   /**< Used by the verifier. Don't rely on its content. */
  unsigned kind    : 4;   /**< The kind of the edge. */
  struct list_head list;  /**< The list head to queue all out edges at a node. */
#ifdef DEBUG_libfirm
  long src_nr;            /**< The node number of the source node. */
#endif
};


/** Accessor for private irn info. */
#define _get_irn_edge_info(irn, kind) (&(((irn)->edge_info)[kind]))

/** Accessor for private irg info. */
#define _get_irg_edge_info(irg, kind) (&(((irg)->edge_info)[kind]))

/**
* Convenience macro to get the outs_head from a irn_edge_info_t
* struct.
*/
#define _get_irn_outs_head(irn, kind) (&_get_irn_edge_info(irn, kind)->outs_head)

/**
* Get the first edge pointing to some node.
* @note There is no order on out edges. First in this context only
* means, that you get some starting point into the list of edges.
* @param irn The node.
* @return The first out edge that points to this node.
*/
static INLINE const ir_edge_t *_get_irn_out_edge_first_kind(const ir_node *irn, ir_edge_kind_t kind)
{
	const struct list_head *head = _get_irn_outs_head(irn, kind);
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
	return next == _get_irn_outs_head(irn, last->kind) ? NULL : list_entry(next, ir_edge_t, list);
}

/**
* Get the number of edges pointing to a node.
* @param irn The node.
* @return The number of edges pointing to this node.
*/
static INLINE int _get_irn_n_edges_kind(const ir_node *irn, int kind)
{
	/* Perhaps out_count was buggy. This code does it more safely. */
#if 1
	int res = 0;
	const struct list_head *pos, *head = _get_irn_outs_head(irn, kind);
	list_for_each(pos, head)
		res++;
	return res;
#else
	return _get_irn_edge_info(irn, kind)->out_count;
#endif
}

static INLINE int _edges_activated_kind(const ir_graph *irg, ir_edge_kind_t kind)
{
	return _get_irg_edge_info(irg, kind)->activated;
}

/**
* Assure, that the edges information is present for a certain graph.
* @param irg The graph.
*/
static INLINE void _edges_assure_kind(ir_graph *irg, int kind)
{
	if(!_edges_activated_kind(irg, kind))
		edges_activate_kind(irg, kind);
}

void edges_init_graph_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
* Notify of a edge change.
* The edge from (src, pos) -> old_tgt is redirected to tgt
*/
void edges_notify_edge_kind(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt, ir_edge_kind_t kind, ir_graph *irg);

/**
* A node is deleted.
*/
void edges_node_deleted(ir_node *old, ir_graph *irg);

void edges_invalidate_kind(ir_node *irn, ir_edge_kind_t kind, ir_graph *irg);

/**
* Register additional memory in an edge.
* This must be called before Firm is initialized.
* @param  n Number of bytes you need.
* @return A number you have to keep and to pass
*         edges_get_private_data()
*         to get a pointer to your data.
*/
int edges_register_private_data(size_t n);

/**
* Get a pointer to the private data you registered.
* @param  edge The edge.
* @param  ofs  The number, you obtained with
*              edges_register_private_data().
* @return A pointer to the private data.
*/
static INLINE void *_get_edge_private_data(const ir_edge_t *edge, int ofs)
{
	return (void *) ((char *) edge + sizeof(edge[0]) + ofs);
}

static INLINE ir_node *_get_edge_src_irn(const ir_edge_t *edge)
{
	return edge->src;
}

static INLINE int _get_edge_src_pos(const ir_edge_t *edge)
{
	return edge->pos;
}

/**
* Initialize the out edges.
* This must be called before firm is initialized.
*/
extern void init_edges(void);

/**
 * Set dbg information for edges.
 */
void edges_init_dbg(int do_dbg);

void edges_invalidate_all(ir_node *irn, ir_graph *irg);

#define get_irn_n_edges_kind(irn, kind)   _get_irn_n_edges_kind(irn, kind)
#define get_edge_src_irn(edge)            _get_edge_src_irn(edge)
#define get_edge_src_pos(edge)            _get_edge_src_pos(edge)
#define get_edge_private_data(edge, ofs)  _get_edge_private_data(edge,ofs)
#define get_irn_out_edge_next(irn, last)  _get_irn_out_edge_next(irn, last)

#ifndef get_irn_n_edges
#define get_irn_n_edges(irn)              _get_irn_n_edges_kind(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_irn_out_edge_first
#define get_irn_out_edge_first(irn)       _get_irn_out_edge_first_kind(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_block_succ_first
#define get_block_succ_first(irn)         _get_irn_out_edge_first_kind(irn, EDGE_KIND_BLOCK)
#endif

#ifndef get_block_succ_next
#define get_block_succ_next(irn, last)    _get_irn_out_edge_next(irn, last)
#endif



#endif /* _FIRM_EDGES_T_H */
