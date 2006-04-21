/*
 * Project:     libFIRM
 * File name:   ir/ana/height.c
 * Purpose:     Compute heights of nodes inside basic blocks
 * Author:      Sebastian Hack
 * Modified by:
 * Created:     19.04.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <stdlib.h>
#include <stdio.h>

#include "list.h"

#include "irdump.h"
#include "irgwalk.h"
#include "irtools.h"
#include "irphase_t.h"
#include "iredges_t.h"

typedef struct _heights_t heights_t;

struct _heights_t {
	phase_t ph;
	unsigned visited;
	void *dump_handle;
	struct list_head sink_head;
};

typedef struct {
	unsigned height;
	unsigned visited;
	unsigned is_sink : 1;
	struct list_head sink_list;
} irn_height_t;

static void irn_height_init(const phase_t *ph, const ir_node *irn, void *data)
{
	irn_height_t *h = data;
	memset(h, 0, sizeof(h[0]));
	INIT_LIST_HEAD(&h->sink_list);
}

static void height_dump_cb(void *data, FILE *f, const ir_node *irn)
{
	heights_t *heights = data;
	irn_height_t *h    = phase_get_irn_data(&heights->ph, irn);

	if(h)
		fprintf(f, "height: %u\n", h->height);
}

/**
 * Check, if we can reach a target node from a given node inside one basic block.
 * @param h    The heights object.
 * @param curr The current node from which we tried to reach the other one.
 * @param tgt  The node we try to reach.
 * @return     1, one of tgt can be reached from curr, 0 else.
 */
static int search(heights_t *h, const ir_node *curr, const ir_node *tgt)
{
	irn_height_t *h_curr;
	irn_height_t *h_tgt;
	int i, n;

	/* if the current node is the one we were looking for, we're done. */
	if(curr == tgt)
		return 1;

	/* If we are in another block we won't find our target. */
	if(get_nodes_block(curr) != get_nodes_block(tgt))
		return 0;

	/* Check, if we have already been here. Coming more often won't help :-) */
	h_curr = phase_get_irn_data(&h->ph, curr);
	if(h_curr->visited >= h->visited)
		return 0;

	/* If we are too deep into the DAG we won't find the target either. */
	h_tgt = phase_get_irn_data(&h->ph, tgt);
	if(h_curr->height > h_tgt->height)
		return 0;

	/* Mark this place as visited. */
	h_curr->visited = h->visited;

	/* Start a search from this node. */
	for(i = 0, n = get_irn_arity(curr); i < n; ++i) {
		ir_node *op = get_irn_n(curr, i);
		if(search(h, op, tgt))
			return 1;
	}

	return 0;
}

/**
 * Check, if one node can be reached from another one, according to data dependence.
 */
int heights_reachable_in_block(heights_t *h, const ir_node *n, const ir_node *m)
{
	int res          = 0;
	irn_height_t *hn = phase_get_irn_data(&h->ph, n);
	irn_height_t *hm = phase_get_irn_data(&h->ph, m);

	assert(get_nodes_block(n) == get_nodes_block(m));
	assert(hn != NULL && hm != NULL);

	if(hn->height <= hm->height) {
		h->visited++;
		res = search(h, n, m);
	}

	return res;
}

/**
 * Compute the height of a node in a block.
 * @param h   The heights object.
 * @param irn The node.
 * @param bl  The block.
 */
static unsigned compute_height(heights_t *h, const ir_node *irn, const ir_node *bl)
{
	irn_height_t *ih = phase_get_or_set_irn_data(&h->ph, irn);
	int is_sink;

	const ir_edge_t *edge;

	/* bail out if we already visited that node. */
	if(ih->visited >= h->visited)
		return ih->height;

	ih->visited = h->visited;
	ih->height  = 0;

	is_sink = 1;

	foreach_out_edge(irn, edge) {
		ir_node *dep = get_edge_src_irn(edge);

		if(!is_Block(dep) && get_nodes_block(dep) == bl) {
			unsigned dep_height = compute_height(h, dep, bl);
			ih->height          = MAX(ih->height, dep_height);
			is_sink             = 0;
		}

		ih->height++;
	}

	ih->is_sink = is_sink;
	if(is_sink)
		list_add(&ih->sink_list, &h->sink_head);

	return ih->height;
}

static void compute_heights_in_block(ir_node *bl, void *data)
{
	heights_t *h = data;
	const ir_edge_t *edge;

	h->visited++;

	foreach_out_edge(bl, edge) {
		ir_node *dep = get_edge_src_irn(edge);
		compute_height(h, dep, bl);
	}
}

unsigned get_irn_height(heights_t *heights, const ir_node *irn)
{
	irn_height_t *h = phase_get_irn_data(&heights->ph, irn);
	assert(h && "No height information for node");
	return h->height;
}

void heights_recompute(heights_t *h)
{
	edges_assure(phase_get_irg(&h->ph));
	phase_reinit_irn_data(&h->ph);
	h->visited = 0;
	INIT_LIST_HEAD(&h->sink_head);
	irg_block_walk_graph(phase_get_irg(&h->ph), compute_heights_in_block, NULL, h);
}

heights_t *heights_new(ir_graph *irg)
{
	heights_t *res = xmalloc(sizeof(res[0]));
	phase_init(&res->ph, "heights", irg, sizeof(irn_height_t), PHASE_DEFAULT_GROWTH, irn_height_init);
	res->dump_handle = dump_add_node_info_callback(height_dump_cb, res);
	heights_recompute(res);

	return res;
}

void heights_free(heights_t *h)
{
	phase_free(&h->ph);
	dump_remv_node_info_callback(h->dump_handle);
	xfree(h);
}
