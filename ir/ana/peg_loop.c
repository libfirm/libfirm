/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Compute loop information for PEG graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "peg_loop_t.h"
#include "irphase_t.h"
#include "plist.h"
#include "obstack.h"
#include "irtools.h"

typedef struct pl_node {
	plist_t *links;
	int      depth;
} pl_node;

struct pl_info {
	struct obstack  obst;
	ir_phase       *phase;
	plist_t        *etas;
};

int pl_get_depth(pl_info *pli, ir_node *irn)
{
	pl_node *pln = phase_get_irn_data(pli->phase, irn);
	if (!pln) return -1;
	return pln->depth;
}

ir_node *pl_get_link(pl_info *pli, ir_node *irn, pl_iter *it)
{
	plist_element_t *plist_it;
	pl_node *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");

	/* Get the first link element. */
	plist_it = plist_first(pln->links);
	if (!plist_it) return NULL;
	if (it != NULL) *it = plist_it;

	return (ir_node*)plist_it->data;
}

ir_node *pl_get_eta(pl_info *pli, pl_iter *it)
{
	/* Get the first eta element. */
	plist_element_t *plist_it = plist_first(pli->etas);
	if (!plist_it) return NULL;
	if (it != NULL) *it = plist_it;

	return (ir_node*)plist_it->data;
}

ir_node *pl_iter_next(pl_iter *it)
{
	ir_node *cur;
	plist_element_t *plist_it = *it;
	if (!plist_it) return NULL;

	cur = plist_it->data;
	*it = plist_it->next;
	return cur;
}

static void *pl_init_node(ir_phase *phase, const ir_node *irn)
{
	pl_info *info = phase_get_private(phase);
	pl_node *pln  = OALLOCZ(&info->obst, pl_node);

	(void)irn;
	pln->depth = -1;
	return pln;
}

static void pl_compute_depth(pl_info *pli, ir_node *irn, plist_t *todo)
{
	int i;
	pl_node *pln;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	if (is_Theta(irn)) {
		/* The next dep may recurse back to one of the nodes we have already
		 * visited, but not processed. Prevent that, by queueing the dep for
		 * later processing, so that post-order processing can finish first.
		 * The depth of the theta itself is known after all. */
		pl_compute_depth(pli, get_Theta_init(irn), todo);
		plist_insert_back(todo, get_Theta_next(irn));

		/* Use the known theta depth. */
		pln = phase_get_or_set_irn_data(pli->phase, irn);
		pln->depth = get_Theta_depth(irn);
	} else {
		/* Recurse first and calculate post-order. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			pl_compute_depth(pli, ir_dep, todo);
		}

		/* Use this when no deps are present (can't be in a loop). */
		pln = phase_get_or_set_irn_data(pli->phase, irn);
		pln->depth = 0;

		/* Calculate minimal and maximal depth of the deps. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			pl_node *pl_dep = phase_get_irn_data(pli->phase, ir_dep);
			assert(pl_dep);

			/* Just take the nodes depth on the first try. */
			pln->depth = i ? MAX(pln->depth, pl_dep->depth) : pl_dep->depth;
		}

		/* Leave nodes on eta. */
		if (is_Eta(irn)) {
			pln->depth--;
			assert(pln->depth >= 0);

			/* Store the node in the eta list. */
			plist_insert_back(pli->etas, irn);
		}
	}
}

static void pl_compute_thetas(pl_info *pli, ir_node *irn,
                              ir_node *ir_eta, pl_node *pl_eta)
{
	int i;
	pl_node *pln;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	pln = phase_get_irn_data(pli->phase, irn);
	assert(pln);

	/* Return when leaving the subgraph guarded by the eta node. */
	if ((irn != ir_eta) && (pln->depth <= pl_eta->depth)) return;

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		pl_compute_thetas(pli, ir_dep, ir_eta, pl_eta);
	}

	/* Store theta nodes nested inside the eta, but not deeper. */
	if (is_Theta(irn) && (get_Theta_depth(irn) == (pl_eta->depth + 1))) {
		plist_insert_back(pl_eta->links, irn);

		/* And link the eta on the theta, too. */
		if (!pln->links) pln->links = plist_obstack_new(&pli->obst);
		plist_insert_back(pln->links, ir_eta);
	}
}

pl_info *pl_init(ir_graph *irg)
{
	plist_element_t *it;

	plist_t *todo;
	ir_node *end  = get_irg_end_block(irg);
	ir_node *ret  = get_Block_cfgpred(end, 0);
	pl_info *info = XMALLOC(pl_info);
	assert(is_Return(ret) && "Invalid PEG graph.");

	obstack_init(&info->obst);
	info->phase = new_phase(irg, pl_init_node);
	info->etas  = plist_obstack_new(&info->obst);
	phase_set_private(info->phase, info);

	/* Do the depth analysis by processing acyclic fragments of the graph.
	 * On every theta node, analysis stops and the fragment on the thetas
	 * next dependency is added to the queue for later processing. */

	todo = plist_obstack_new(&info->obst);
	plist_insert_back(todo, ret);
	inc_irg_visited(irg); /* Only reset once. */

	while (plist_count(todo) > 0) {
		ir_node *next = plist_first(todo)->data;
		plist_erase(todo, plist_first(todo));
		pl_compute_depth(info, next, todo);
	}

	/* Collect thetas for each eta node. Can we improve this? */
	foreach_plist(info->etas, it) {
		ir_node *ir_eta = it->data;
		pl_node *pl_eta = phase_get_irn_data(info->phase, ir_eta);
		assert(pl_eta);

		inc_irg_visited(irg);
		pl_eta->links = plist_obstack_new(&info->obst);
		pl_compute_thetas(info, it->data, ir_eta, pl_eta);
	}

	return info;
}

void pl_free(pl_info *pli)
{
	phase_free(pli->phase);
	obstack_free(&pli->obst, NULL);
	xfree(pli);
}

ir_graph *pl_get_irg(pl_info *info)
{
	return phase_get_irg(info->phase);
}

static void pl_dump_irn(pl_info *pli, ir_node *irn, FILE* f)
{
	int i;
	pl_node *pln;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		pl_dump_irn(pli, ir_dep, f);
	}

	pln = phase_get_irn_data(pli->phase, irn);
	fprintf(f, "depth(%3li) = %i\n", get_irn_node_nr(irn), pln->depth);

	if (pln->links) {
		fprintf(f, "links(%3li) = {", get_irn_node_nr(irn));

		plist_element_t *it;
		foreach_plist(pln->links, it) {
			if (it != plist_first(pln->links)) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(it->data));
		}

		fprintf(f, "}\n");
	}
}

void pl_dump(pl_info *pli, FILE* f)
{
	ir_graph *irg = phase_get_irg(pli->phase);
	ir_node  *end = get_irg_end_block(irg);
	ir_node  *ret = get_Block_cfgpred(end, 0);
	assert(ret);

	/* Walk the tree and dump every node. */
	inc_irg_visited(irg);
	pl_dump_irn(pli, ret, f);
}
