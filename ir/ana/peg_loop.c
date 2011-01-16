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

typedef struct obstack obstack;

typedef enum pl_type {
	plt_any,
	plt_eta
} pl_type;

typedef struct pl_node {
	pl_type  type;
	ir_node *irn;
	int      depth;
	plist_t *etas;
} pl_node;

typedef struct pl_node_eta {
	pl_node  base;
	plist_t *border;
	plist_t *thetas;
} pl_node_eta;

struct pl_info {
	obstack   obst;
	ir_phase *phase;
	plist_t  *etas;
	plist_t  *thetas;
};

void pl_copy_info(pl_info *pli, ir_node *src, ir_node *dst)
{
	pl_node *src_pln = phase_get_irn_data(pli->phase, src);
	pl_node *dst_pln = phase_get_or_set_irn_data(pli->phase, dst);
	assert(src_pln->type == dst_pln->type);

	dst_pln->etas  = src_pln->etas;
	dst_pln->irn   = dst;
	dst_pln->depth = src_pln->depth;

	if (src_pln->type == plt_eta) {
		pl_node_eta *src_pln_eta = (pl_node_eta*)src_pln;
		pl_node_eta *dst_pln_eta = (pl_node_eta*)dst_pln;

		dst_pln_eta->border = src_pln_eta->border;
		dst_pln_eta->thetas = src_pln_eta->thetas;
	}
}

int pl_get_depth(pl_info *pli, ir_node *irn)
{
	pl_node *pln = phase_get_irn_data(pli->phase, irn);
	if (!pln) return -1;
	return pln->depth;
}

void pl_set_depth(pl_info *pli, ir_node *irn, int depth)
{
	pl_node *pln = phase_get_or_set_irn_data(pli->phase, irn);
	pln->depth = depth;
}

static void *pl_init_node(ir_phase *phase, const ir_node *irn)
{
	pl_info *pli = phase_get_private(phase);
	(void)irn;

	if (is_Eta(irn) || is_EtaA(irn)) {
		pl_node_eta *pln = OALLOC(&pli->obst, pl_node_eta);
		pln->base.type  = plt_eta;
		pln->thetas     = plist_obstack_new(&pli->obst);
		pln->border     = plist_obstack_new(&pli->obst);
		pln->base.etas  = plist_obstack_new(&pli->obst);
		pln->base.depth = -1;
		return pln;
	} else {
		pl_node *pln = OALLOC(&pli->obst, pl_node);
		pln->type  = plt_any;
		pln->etas  = plist_obstack_new(&pli->obst);
		pln->depth = -1;
		return pln;
	}

	assert(0);
	return NULL;
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
		pln->irn   = irn;
		pln->depth = get_Theta_depth(irn);
		plist_insert_back(pli->thetas, pln);
	} else {
		/* Recurse first and calculate post-order. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			pl_compute_depth(pli, ir_dep, todo);
		}

		/* Use this when no deps are present (can't be in a loop). */
		pln = phase_get_or_set_irn_data(pli->phase, irn);
		pln->irn   = irn;
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
			plist_insert_back(pli->etas, pln);
		}
	}
}

static void pl_compute_links(pl_info *pli, pl_node *pln, pl_node_eta *pl_eta)
{
	int i;

	if (irn_visited(pln->irn)) return;
	mark_irn_visited(pln->irn);

	/* Return when leaving the subgraph guarded by the eta node. */
	if ((pln != (pl_node*)pl_eta) && (pln->depth <= pl_eta->base.depth)) {
		/* Put the invariant node in the etas border. */
		plist_insert_back(pl_eta->border, pln);
		return;
	}

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(pln->irn); i++) {
		ir_node *ir_dep = get_irn_n(pln->irn, i);
		pl_node *pl_dep = phase_get_irn_data(pli->phase, ir_dep);
		assert(pl_dep);

		pl_compute_links(pli, pl_dep, pl_eta);
	}

	/* Assign the eta node to all irns in the eta (but not deeper). */
	if (pln->depth == (pl_eta->base.depth + 1)) {
		plist_insert_back(pln->etas, pl_eta);

		/* Store theta links on the eta node. */
		if (is_Theta(pln->irn)) {
			plist_insert_back(pl_eta->thetas, pln);
		}
	}
}

pl_info *pl_init(ir_graph *irg)
{
	plist_element_t *it;

	plist_t *todo;
	ir_node *end = get_irg_end_block(irg);
	ir_node *ret = get_Block_cfgpred(end, 0);
	pl_info *pli = XMALLOC(pl_info);
	assert(is_Return(ret) && "Invalid PEG graph.");

	obstack_init(&pli->obst);
	pli->phase  = new_phase(irg, pl_init_node);
	pli->etas   = plist_obstack_new(&pli->obst);
	pli->thetas = plist_obstack_new(&pli->obst);
	phase_set_private(pli->phase, pli);

	/* Do the depth analysis by processing acyclic fragments of the graph.
	 * On every theta node, analysis stops and the fragment on the thetas
	 * next dependency is added to the queue for later processing. */

	todo = plist_obstack_new(&pli->obst);
	plist_insert_back(todo, ret);
	inc_irg_visited(irg); /* Only reset once. */

	while (plist_count(todo) > 0) {
		ir_node *next = plist_first(todo)->data;
		plist_erase(todo, plist_first(todo));
		pl_compute_depth(pli, next, todo);
	}

	/* Collect thetas for each eta node. Can we improve this? */
	foreach_plist(pli->etas, it) {
		inc_irg_visited(irg);
		pl_compute_links(pli, it->data, it->data);
	}

	return pli;
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
	plist_element_t *it;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		pl_dump_irn(pli, ir_dep, f);
	}

	pln = phase_get_irn_data(pli->phase, irn);
	fprintf(f, "depth (%3li) = %i\n", get_irn_node_nr(irn), pln->depth);

	if (pln->etas) {
		/* Output the list of associated eta nodes. */
		fprintf(f, "etas  (%3li) = {", get_irn_node_nr(irn));
		foreach_plist(pln->etas, it) {
			if (it != plist_first(pln->etas)) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(((pl_node*)it->data)->irn));
		}
		fprintf(f, "}\n");

		/* For eta nodes also output the thetas and the border. */
		if (pln->type == plt_eta) {
			pl_node_eta *pln_eta = (pl_node_eta*)pln;

			fprintf(f, "thetas(%3li) = {", get_irn_node_nr(irn));
			foreach_plist(pln_eta->thetas, it) {
				if (it != plist_first(pln_eta->thetas)) fprintf(f, ", ");
				fprintf(f, "%li", get_irn_node_nr(((pl_node*)it->data)->irn));
			}
			fprintf(f, "}\n");

			fprintf(f, "border(%3li) = {", get_irn_node_nr(irn));
			foreach_plist(pln_eta->border, it) {
				if (it != plist_first(pln_eta->border)) fprintf(f, ", ");
				fprintf(f, "%li", get_irn_node_nr(((pl_node*)it->data)->irn));
			}
			fprintf(f, "}\n");
		}
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

/******************************************************************************
 * Public interfaces and iterators.                                           *
 ******************************************************************************/

/* IRG etas */
int pl_get_irg_eta_count(pl_info *pli)
{
	return plist_count(pli->etas);
}

void pl_irg_etas_iter_init(pl_info *pli, pl_irg_etas_iter *it)
{
	*it = plist_first(pli->etas);
}

ir_node *pl_irg_etas_iter_next(pl_irg_etas_iter *it)
{
	if (!*it) return NULL;
	pl_node *pln = (*it)->data; *it = (*it)->next;
	return pln->irn;
}

/* IRG thetas */
int pl_get_irg_theta_count(pl_info *pli)
{
	return plist_count(pli->thetas);
}

void pl_irg_thetas_iter_init(pl_info *pli, pl_irg_thetas_iter *it)
{
	*it = plist_first(pli->thetas);
}

ir_node *pl_irg_thetas_iter_next(pl_irg_thetas_iter *it)
{
	if (!*it) return NULL;
	pl_node *pln = (*it)->data; *it = (*it)->next;
	return pln->irn;
}

/* IRN etas */
int pl_get_eta_count(pl_info *pli, ir_node *irn)
{
	pl_node *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	return plist_count(pln->etas);
}

void pl_etas_iter_init(pl_info *pli, ir_node *irn, pl_etas_iter *it)
{
	pl_node *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	*it = plist_first(pln->etas);
}

ir_node *pl_etas_iter_next(pl_thetas_iter *it)
{
	if (!*it) return NULL;
	pl_node *pln = (*it)->data; *it = (*it)->next;
	return pln->irn;
}

/* IRN thetas */
int pl_get_theta_count(pl_info *pli, ir_node *irn)
{
	pl_node_eta *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	assert((pln->base.type == plt_eta) && "Not an eta node.");
	return plist_count(pln->thetas);
}

void pl_thetas_iter_init(pl_info *pli, ir_node *irn, pl_thetas_iter *it)
{
	pl_node_eta *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	assert((pln->base.type == plt_eta) && "Not an eta node.");
	*it = plist_first(pln->thetas);
}

ir_node *pl_thetas_iter_next(pl_thetas_iter *it)
{
	if (!*it) return NULL;
	pl_node *pln = (*it)->data; *it = (*it)->next;
	return pln->irn;
}

/* IRN border */
int pl_get_border_count(pl_info *pli, ir_node *irn)
{
	pl_node_eta *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	assert((pln->base.type == plt_eta) && "Not an eta node.");
	return plist_count(pln->border);
}

void pl_border_iter_init(pl_info *pli, ir_node *irn, pl_border_iter *it)
{
	pl_node_eta *pln = phase_get_irn_data(pli->phase, irn);
	assert(pln && "No loop information for the given node.");
	assert((pln->base.type == plt_eta) && "Not an eta node.");
	*it = plist_first(pln->border);
}

ir_node *pl_border_iter_next(pl_border_iter *it)
{
	if (!*it) return NULL;
	pl_node *pln = (*it)->data; *it = (*it)->next;
	return pln->irn;
}
