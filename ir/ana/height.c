/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief    Compute heights of nodes inside basic blocks
 * @author   Sebastian Hack
 * @date     19.04.2006
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

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
	ir_phase ph;
	unsigned visited;
	void *dump_handle;
};

typedef struct {
	unsigned height;
	unsigned visited;
} irn_height_t;

static void *irn_height_init(ir_phase *ph, ir_node *irn, void *data)
{
	irn_height_t *h = data ? data : phase_alloc(ph, sizeof(h[0]));
	memset(h, 0, sizeof(h[0]));
	return h;
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
	for(i = 0, n = get_irn_ins_or_deps(curr); i < n; ++i) {
		ir_node *op = get_irn_in_or_dep(curr, i);
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
static unsigned compute_height(heights_t *h, ir_node *irn, const ir_node *bl)
{
	irn_height_t *ih = phase_get_or_set_irn_data(&h->ph, irn);

	const ir_edge_t *edge;

	/* bail out if we already visited that node. */
	if(ih->visited >= h->visited)
		return ih->height;

	ih->visited = h->visited;
	ih->height  = 0;

	foreach_out_edge(irn, edge) {
		ir_node *dep = get_edge_src_irn(edge);

		if(!is_Block(dep) && get_nodes_block(dep) == bl) {
			unsigned dep_height = compute_height(h, dep, bl);
			ih->height          = MAX(ih->height, dep_height);
		}

		ih->height++;
	}

	foreach_out_edge_kind(irn, edge, EDGE_KIND_DEP) {
		ir_node *dep = get_edge_src_irn(edge);

		if(!is_Block(dep) && get_nodes_block(dep) == bl) {
			unsigned dep_height = compute_height(h, dep, bl);
			ih->height          = MAX(ih->height, dep_height);
		}

		ih->height++;
	}

	return ih->height;
}

static unsigned compute_heights_in_block(ir_node *bl, heights_t *h)
{
	int             max_height = -1;
	const ir_edge_t *edge;

	h->visited++;

	foreach_out_edge(bl, edge) {
		ir_node *dep = get_edge_src_irn(edge);
		int     curh = compute_height(h, dep, bl);

		max_height = MAX(curh, max_height);
	}

	foreach_out_edge_kind(bl, edge, EDGE_KIND_DEP) {
		ir_node *dep = get_edge_src_irn(edge);
		int     curh = compute_height(h, dep, bl);

		max_height = MAX(curh, max_height);
	}

	return max_height;
}

static void compute_heights_in_block_walker(ir_node *block, void *data)
{
	heights_t *h = data;
	compute_heights_in_block(block, h);
}

unsigned get_irn_height(heights_t *heights, const ir_node *irn)
{
	irn_height_t *h = phase_get_irn_data(&heights->ph, irn);
	assert(h && "No height information for node");
	return h->height;
}

unsigned heights_recompute_block(heights_t *h, ir_node *block)
{
	const ir_edge_t *edge;

	edges_assure(phase_get_irg(&h->ph));

	/* reset phase data for all nodes in the block */
	foreach_out_edge(block, edge) {
		ir_node      *irn = get_edge_src_irn(edge);
		irn_height_t *ih  = phase_get_irn_data(&h->ph, irn);

		irn_height_init(&h->ph, irn, ih);
	}

	h->visited = 0;
	return compute_heights_in_block(block, h);
}

void heights_recompute(heights_t *h)
{
	edges_assure(phase_get_irg(&h->ph));
	phase_reinit_irn_data(&h->ph);
	h->visited = 0;
	irg_block_walk_graph(phase_get_irg(&h->ph), compute_heights_in_block_walker, NULL, h);
}

heights_t *heights_new(ir_graph *irg)
{
	heights_t *res = xmalloc(sizeof(res[0]));
	phase_init(&res->ph, "heights", irg, PHASE_DEFAULT_GROWTH, irn_height_init, NULL);
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
