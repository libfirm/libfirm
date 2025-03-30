/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Everlasting outs -- private header.
 * @author  Sebastian Hack, Andreas Schoesser
 * @date    15.01.2005
 */
#ifndef FIRM_IR_EDGES_T_H
#define FIRM_IR_EDGES_T_H

#include <stdbool.h>

#include "set.h"
#include "list.h"

#include "irnode_t.h"
#include "irgraph_t.h"

#include "iredgekinds.h"
#include "iredges.h"

#define get_irn_n_edges_kind(irn, kind)   get_irn_n_edges_kind_(irn, kind)
#define get_edge_src_irn(edge)            get_edge_src_irn_(edge)
#define get_edge_src_pos(edge)            get_edge_src_pos_(edge)
#define get_irn_out_edge_next(irn, last, kind)  get_irn_out_edge_next_(irn, last, kind)
#define get_irn_n_edges(irn)              get_irn_n_edges_kind_(irn, EDGE_KIND_NORMAL)
#define get_irn_out_edge_first(irn)       get_irn_out_edge_first_kind_(irn, EDGE_KIND_NORMAL)
#define get_block_succ_first(irn)         get_irn_out_edge_first_kind_(irn, EDGE_KIND_BLOCK)
#define get_block_succ_next(irn, last)    get_irn_out_edge_next_(irn, last, EDGE_KIND_BLOCK)

/**
 * An edge.
 */
struct ir_edge_t {
	ir_node *src;         /**< The source node of the edge. */
	int      pos;         /**< The position of the edge at @p src. */
#ifdef DEBUG_libfirm
	bool     present : 1; /**< Used by the verifier. */
#endif
	struct list_head list;  /**< The list head to queue all out edges at a node. */
};

/** Accessor for private irn info. */
static inline irn_edge_info_t *get_irn_edge_info(ir_node *node,
                                                 ir_edge_kind_t kind)
{
	assert(edges_activated_kind(get_irn_irg(node), kind));
	return &node->edge_info[kind];
}

static inline const irn_edge_info_t *get_irn_edge_info_const(
		const ir_node *node, ir_edge_kind_t kind)
{
	assert(edges_activated_kind(get_irn_irg(node), kind));
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
	struct list_head const *const head = &get_irn_edge_info_const(irn, kind)->outs_head;
	return list_empty(head) ? NULL : list_entry(head->next, ir_edge_t, list);
}

/**
 * Get the next edge in the out list of some node.
 * @param irn The node.
 * @param last The last out edge you have seen.
 * @return The next out edge in @p irn 's out list after @p last.
 */
static inline const ir_edge_t *get_irn_out_edge_next_(const ir_node *irn, const ir_edge_t *last, ir_edge_kind_t kind)
{
	struct list_head *next = last->list.next;
	const struct list_head *head
		= &get_irn_edge_info_const(irn, kind)->outs_head;
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

static inline int edges_activated_(const ir_graph *irg)
{
	return edges_activated_kind(irg, EDGE_KIND_NORMAL)
	    && edges_activated_kind(irg, EDGE_KIND_BLOCK);
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
 * A node might be revived by CSE.
 */
void edges_node_revival(ir_node *node);

void edges_invalidate_kind(ir_node *irn, ir_edge_kind_t kind);

static inline ir_node *get_edge_src_irn_(const ir_edge_t *edge)
{
	return edge->src;
}

static inline int get_edge_src_pos_(const ir_edge_t *edge)
{
	return edge->pos;
}

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

void edges_notify_edge(ir_node *src, int pos, ir_node *tgt, ir_node *old_tgt,
                       ir_graph *irg);

#endif
