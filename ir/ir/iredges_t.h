/*
 * Project:     libFIRM
 * File name:   ir/ir/iredges_t.h
 * Purpose:     Everlasting outs -- private header.
 * Author:      Sebastian Hack
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

#include "iredges.h"

#if FIRM_EDGES_INPLACE

#define DBG_EDGES  "edges"

/**
 * An edge.
 */
struct _ir_edge_t {
#ifdef DEBUG_libfirm
  long src_nr;            /**< The node number of the source node. */
#endif
  ir_node *src;           /**< The source node of the edge. */
  int pos;                /**< The position of the edge at @p src. */
  unsigned invalid : 1;   /**< edges that are removed are marked invalid. */
  unsigned present : 1;   /**< Used by the verifier. Don't rely on its content. */
  struct list_head list;  /**< The list head to queue all out edges at a node. */
};

/**
 * A block edge inherits from a normal edge.
 * They represent edges leading from a block to a control flow node
 * and are used to quickly find all control flow successors of
 * a block.
 */
struct _ir_block_edge_t {
  struct _ir_edge_t edge;      /**< The inherited data. */
  struct list_head succ_list;  /**< List element listing all
                                 control flow edges to the
                                 successors of a block. */
};

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
 * Convenience macro to get the succ_head from a block_attr
 * struct.
 */
#define _get_block_succ_head(bl) (&((bl)->attr.block.succ_head))

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
 * Get the first successor edge of a block.
 * A successor edge is an edge originated from another block, pointing
 * to a mode_X node in the given block and is thus a control flow
 * successor edge.
 * @param irn The block.
 * @return The first successor edge of the block.
 */
static INLINE const ir_edge_t *_get_block_succ_first(const ir_node *irn)
{
  const struct list_head *head;

  assert(is_Block(irn) && "Node must be a block here");
  head = _get_block_succ_head(irn);
  return (ir_edge_t *) (list_empty(head) ? NULL :
      list_entry(head->next, ir_block_edge_t, succ_list));
}

/**
 * Get the next block successor edge.
 * @see See _get_block_succ_first() for details.
 * @param irn The block.
 * @param last The last edge.
 * @return The next edge, or NULL if there is no further.
 */
static INLINE const ir_edge_t *_get_block_succ_next(const ir_node *irn, const ir_edge_t *last)
{
  const ir_block_edge_t *block_edge;
  struct list_head *next;

  assert(is_Block(irn) && "Node must be a block here");
  block_edge = (const ir_block_edge_t *) last;
  next = block_edge->succ_list.next;
  return (ir_edge_t *) (next == _get_block_succ_head(irn) ? NULL :
      list_entry(next, ir_block_edge_t, succ_list));
}

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

/**
 * Get the number of edges pointing to a node.
 * @param irn The node.
 * @return The number of edges pointing to this node.
 */
static INLINE int _get_irn_n_edges(const ir_node *irn)
{
  return _get_irn_edge_info(irn)->out_count;
}

static INLINE int _edges_activated(const ir_graph *irg)
{
  return _get_irg_edge_info(irg)->activated;
}

/**
 * Assure, that the edges information is present for a certain graph.
 * @param irg The graph.
 */
static INLINE void _edges_assure(ir_graph *irg)
{
	if(!_edges_activated(irg))
		edges_activate(irg);
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

#define get_irn_out_edge_first(irn)      _get_irn_out_edge_first(irn)
#define get_irn_out_edge_next(irn,last)  _get_irn_out_edge_next(irn, last)
#define get_block_succ_first(irn)        _get_block_succ_first(irn)
#define get_block_succ_next(irn,last)    _get_block_succ_next(irn, last)
#define get_edge_src_irn(edge)           _get_edge_src_irn(edge)
#define get_edge_src_pos(edge)           _get_edge_src_pos(edge)
#define edges_activated(irg)             _edges_activated(irg)
#define edges_assure(irg)                _edges_assure(irg)

#else
/* new edges are disabled */

#define init_edges()
#define edges_reroute(old, nw, irg)
#define edges_init_graph(irg);
#define edges_notify_edge(src, pos, tgt, old_tgt, irg)
#define edges_node_deleted(old, irg)
#define edges_invalidate(irn, irg)
#define get_irn_out_edge_first(irn)       NULL
#define get_irn_out_edge_next(irn,last)   NULL
#define get_edge_src_irn(edge)            NULL
#define get_edge_src_pos(edge)            -1
#define edges_activated(irg)              0
#define edges_assure(irg)

#endif /* FIRM_EDGES_INPLACE */

#endif /* _FIRM_EDGES_T_H */
