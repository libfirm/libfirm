/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Everlasting outs -- private header.
 * @author  Sebastian Hack, Andreas Schoesser
 * @date    15.01.2005
 */
#ifndef FIRM_IR_EDGES_T_H
#define FIRM_IR_EDGES_T_H

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
struct ir_edge_t {
	ir_node  *src;          /**< The source node of the edge. */
	int      pos;           /**< The position of the edge at @p src. */
	unsigned invalid : 1;   /**< edges that are removed are marked invalid. */
	unsigned present : 1;   /**< Used by the verifier. Don't rely on its content. */
	unsigned kind    : 4;   /**< The kind of the edge. */
	struct list_head list;  /**< The list head to queue all out edges at a node. */
};

/** Accessor for private irn info. */
static inline irn_edge_info_t *get_irn_edge_info(ir_node *node,
                                                 ir_edge_kind_t kind)
{
	return &node->edge_info[kind];
}

static inline const irn_edge_info_t *get_irn_edge_info_const(
		const ir_node *node, ir_edge_kind_t kind)
{
	return &node->edge_info[kind];
}

/** Accessor for private irg info. */
static inline irg_edge_info_t *get_irg_edge_info(ir_graph *irg,
                                                 ir_edge_kind_t kind)
{
	return &irg->edge_info[kind];
}

/** Accessor for private irg info. */
static inline const irg_edge_info_t *get_irg_edge_info_const(
		const ir_graph *irg, ir_edge_kind_t kind)
{
	return &irg->edge_info[kind];
}

/**
 * Get the first edge pointing to some node.
 * @note There is no order on out edges. First in this context only
 * means, that you get some starting point into the list of edges.
 * @param irn The node.
 * @return The first out edge that points to this node.
 */
static inline const ir_edge_t *get_irn_out_edge_first_kind_(const ir_node *irn, ir_edge_kind_t kind)
{
	const struct list_head *head;
	assert(edges_activated_kind(get_irn_irg(irn), kind));
	head = &get_irn_edge_info_const(irn, kind)->outs_head;
	return list_empty(head) ? NULL : list_entry(head->next, ir_edge_t, list);
}

/**
 * Get the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @return The next out edge in @p irn 's out list after @p last.
 */
static inline const ir_edge_t *get_irn_out_edge_next_(const ir_node *irn, const ir_edge_t *last)
{
	struct list_head *next = last->list.next;
	const struct list_head *head
		= &get_irn_edge_info_const(irn, (ir_edge_kind_t)last->kind)->outs_head;
	return next == head ? NULL : list_entry(next, ir_edge_t, list);
}

/**
 * Get the number of edges pointing to a node.
 * @param irn The node.
 * @return The number of edges pointing to this node.
 */
static inline int get_irn_n_edges_kind_(const ir_node *irn, ir_edge_kind_t kind)
{
	return get_irn_edge_info_const(irn, kind)->out_count;
}

static inline int edges_activated_kind_(const ir_graph *irg, ir_edge_kind_t kind)
{
	return get_irg_edge_info_const(irg, kind)->activated;
}

/**
 * Assure, that the edges information is present for a certain graph.
 * @param irg The graph.
 */
static inline void edges_assure_kind_(ir_graph *irg, ir_edge_kind_t kind)
{
	if(!edges_activated_kind_(irg, kind))
		edges_activate_kind(irg, kind);
}

void edges_init_graph_kind(ir_graph *irg, ir_edge_kind_t kind);

void edges_node_deleted(ir_node *irn);

/**
 * A node might be revivaled by CSE.
 */
void edges_node_revival(ir_node *node);

void edges_invalidate_kind(ir_node *irn, ir_edge_kind_t kind);

/**
 * Register additional memory in an edge.
 * This must be called before Firm is initialized.
 * @param  n Number of bytes you need.
 * @return A number you have to keep and to pass
 *         edges_get_private_data()
 *         to get a pointer to your data.
 */
size_t edges_register_private_data(size_t n);

/**
 * Get a pointer to the private data you registered.
 * @param  edge The edge.
 * @param  ofs  The number, you obtained with
 *              edges_register_private_data().
 * @return A pointer to the private data.
 */
static inline void *get_edge_private_data_(const ir_edge_t *edge, int ofs)
{
	return (void *) ((char *) edge + sizeof(edge[0]) + ofs);
}

static inline ir_node *get_edge_src_irn_(const ir_edge_t *edge)
{
	return edge->src;
}

static inline int get_edge_src_pos_(const ir_edge_t *edge)
{
	return edge->pos;
}

/**
 * Returns the edge object of an outgoing edge at a node.
 * @param  irn  The node at which the edge originates.
 * @param  pos  The position of the edge.
 * @param  kind The kind of the edge.
 * @return      The corresponding edge object or NULL,
 *              if no such edge exists.
 */
FIRM_API const ir_edge_t *get_irn_edge_kind(const ir_node *irn,
                                            int pos, ir_edge_kind_t kind);

/**
 * Initialize the out edges.
 * This must be called before firm is initialized.
 */
extern void init_edges(void);

void edges_invalidate_all(ir_node *irn);

/**
 * Helper function to dump the edge set of a graph,
 * unused in normal code.
 */
void edges_dump_kind(ir_graph *irg, ir_edge_kind_t kind);

/**
 * Notify normal and block edges.
 */
void edges_notify_edge(ir_node *src, int pos, ir_node *tgt,
                       ir_node *old_tgt, ir_graph *irg);

#define get_irn_n_edges_kind(irn, kind)   get_irn_n_edges_kind_(irn, kind)
#define get_edge_src_irn(edge)            get_edge_src_irn_(edge)
#define get_edge_src_pos(edge)            get_edge_src_pos_(edge)
#define get_edge_private_data(edge, ofs)  get_edge_private_data_(edge,ofs)
#define get_irn_out_edge_next(irn, last)  get_irn_out_edge_next_(irn, last)

#ifndef get_irn_n_edges
#define get_irn_n_edges(irn)              get_irn_n_edges_kind_(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_irn_out_edge_first
#define get_irn_out_edge_first(irn)       get_irn_out_edge_first_kind_(irn, EDGE_KIND_NORMAL)
#endif

#ifndef get_block_succ_first
#define get_block_succ_first(irn)         get_irn_out_edge_first_kind_(irn, EDGE_KIND_BLOCK)
#endif

#ifndef get_block_succ_next
#define get_block_succ_next(irn, last)    get_irn_out_edge_next_(irn, last)
#endif

#endif
