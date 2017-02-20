/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Compute heights of nodes inside basic blocks
 * @author   Sebastian Hack
 * @date     19.04.2006
 */
#include "heights.h"

#include "irdump.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnodemap.h"
#include "list.h"
#include "util.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct ir_heights_t {
	ir_nodemap      data;
	unsigned        visited;
	hook_entry_t   *dump_handle;
	struct obstack  obst;
};

typedef struct {
	unsigned height;
	unsigned visited;
} irn_height_t;

static irn_height_t *maybe_get_height_data(const ir_heights_t *heights,
                                           const ir_node *node)
{
	irn_height_t *height = ir_nodemap_get(irn_height_t, &heights->data, node);
	return height;
}

static irn_height_t *get_height_data(ir_heights_t *heights, const ir_node *node)
{
	irn_height_t *height = ir_nodemap_get(irn_height_t, &heights->data, node);
	if (height == NULL) {
		height = OALLOCZ(&heights->obst, irn_height_t);
		ir_nodemap_insert(&heights->data, node, height);
	}
	return height;
}

static void height_dump_cb(void *data, FILE *f, const ir_node *irn)
{
	const ir_heights_t *heights = (const ir_heights_t*) data;
	const irn_height_t *h       = maybe_get_height_data(heights, irn);
	if (h != NULL)
		fprintf(f, "height: %u\n", h->height);
}

/**
 * Check, if we can reach a target node from a given node inside one basic block.
 * @param h    The heights object.
 * @param curr The current node from which we tried to reach the other one.
 * @param tgt  The node we try to reach.
 * @return     1, one of tgt can be reached from curr, 0 else.
 */
static bool search(ir_heights_t *h, const ir_node *curr, const ir_node *tgt)
{
	/* if the current node is the one we were looking for, we're done. */
	if (curr == tgt)
		return true;

	/* If we are in another block or at a phi we won't find our target. */
	if (get_nodes_block(curr) != get_nodes_block(tgt))
		return false;
	if (is_Phi(curr))
		return false;

	/* Check, if we have already been here. Coming more often won't help :-) */
	irn_height_t *h_curr = maybe_get_height_data(h, curr);
	if (h_curr->visited >= h->visited)
		return false;

	/* If we are too deep into the DAG we won't find the target either. */
	irn_height_t *h_tgt = maybe_get_height_data(h, tgt);
	if (h_curr->height > h_tgt->height)
		return false;

	/* Mark this place as visited. */
	h_curr->visited = h->visited;

	/* Start a search from this node. */
	foreach_irn_in(curr, i, op) {
		if (search(h, op, tgt))
			return true;
	}

	return false;
}

int heights_reachable_in_block(ir_heights_t *h, const ir_node *n,
                               const ir_node *m)
{
	int           res = 0;
	irn_height_t *hn  = maybe_get_height_data(h, n);
	irn_height_t *hm  = maybe_get_height_data(h, m);

	assert(get_nodes_block(n) == get_nodes_block(m));
	assert(hn != NULL && hm != NULL);

	if (hn->height <= hm->height) {
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
static unsigned compute_height(ir_heights_t *h, ir_node *irn, const ir_node *bl)
{
	irn_height_t *ih = get_height_data(h, irn);

	/* bail out if we already visited that node. */
	if (ih->visited >= h->visited)
		return ih->height;

	ih->visited = h->visited;
	ih->height  = 0;

	foreach_out_edge(irn, edge) {
		ir_node *dep = get_edge_src_irn(edge);

		if (!is_Block(dep) && !is_Phi(dep) && get_nodes_block(dep) == bl) {
			unsigned dep_height = compute_height(h, dep, bl);
			ih->height          = MAX(ih->height, dep_height+1);
		}
	}

	return ih->height;
}

static unsigned compute_heights_in_block(ir_node *bl, ir_heights_t *h)
{
	h->visited++;

	int max_height = -1;
	foreach_out_edge(bl, edge) {
		ir_node *dep = get_edge_src_irn(edge);
		int     curh = compute_height(h, dep, bl);

		max_height = MAX(curh, max_height);
	}

	return max_height;
}

static void compute_heights_in_block_walker(ir_node *block, void *data)
{
	ir_heights_t *h = (ir_heights_t*) data;
	compute_heights_in_block(block, h);
}

unsigned get_irn_height(const ir_heights_t *heights, const ir_node *irn)
{
	const irn_height_t *height = maybe_get_height_data(heights, irn);
	assert(height != NULL);
	return height->height;
}

unsigned heights_recompute_block(ir_heights_t *h, ir_node *block)
{
	ir_graph *irg = get_irn_irg(block);
	assure_edges(irg);

	/* reset data for all nodes in the block */
	foreach_out_edge(block, edge) {
		ir_node      *irn = get_edge_src_irn(edge);
		irn_height_t *ih  = get_height_data(h, irn);
		memset(ih, 0, sizeof(*ih));
	}

	h->visited = 0;
	return compute_heights_in_block(block, h);
}

ir_heights_t *heights_new(ir_graph *irg)
{
	ir_heights_t *res = XMALLOCZ(ir_heights_t);
	ir_nodemap_init(&res->data, irg);
	obstack_init(&res->obst);
	res->dump_handle = dump_add_node_info_callback(height_dump_cb, res);

	assure_edges(irg);
	irg_block_walk_graph(irg, compute_heights_in_block_walker, NULL, res);

	return res;
}

void heights_free(ir_heights_t *h)
{
	dump_remove_node_info_callback(h->dump_handle);
	obstack_free(&h->obst, NULL);
	ir_nodemap_destroy(&h->data);
	free(h);
}
