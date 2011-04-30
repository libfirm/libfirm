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

#include "vf_loop.h"
#include "irphase_t.h"
#include "plist.h"
#include "obstack.h"
#include "irtools.h"

typedef struct obstack obstack;

typedef enum vl_type {
	vl_type_any,
	vl_type_eta
} vl_type;

typedef struct vl_node {
	vl_type  type;
	ir_node *irn;
	int      depth;
	plist_t *etas;
} vl_node;

typedef struct vl_node_eta {
	vl_node  base;
	plist_t *border;
	plist_t *thetas;
} vl_node_eta;

struct vl_info {
	obstack   obst;
	ir_phase *phase;
	plist_t  *etas;
	plist_t  *thetas;
};

int vl_node_get_depth(vl_info *vli, ir_node *irn)
{
	vl_node *vln = phase_get_irn_data(vli->phase, irn);
	if (!vln) return -1;
	return vln->depth;
}

static void *vl_init_node(ir_phase *phase, const ir_node *irn)
{
	vl_info *vli = phase_get_private(phase);

	if (is_Eta(irn)) {
		vl_node_eta *vln = OALLOC(&vli->obst, vl_node_eta);
		vln->base.type  = vl_type_eta;
		vln->thetas     = plist_obstack_new(&vli->obst);
		vln->border     = plist_obstack_new(&vli->obst);
		vln->base.etas  = plist_obstack_new(&vli->obst);
		vln->base.depth = -1;
		return vln;
	} else {
		vl_node *vln = OALLOC(&vli->obst, vl_node);
		vln->type  = vl_type_any;
		vln->etas  = plist_obstack_new(&vli->obst);
		vln->depth = -1;
		return vln;
	}

	assert(0);
	return NULL;
}

static void vl_compute_depth(vl_info *vli, ir_node *irn, plist_t *todo)
{
	int i;
	vl_node *vln;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	if (is_Theta(irn)) {
		/* The next dep may recurse back to one of the nodes we have already
		 * visited, but not processed. Prevent that, by queueing the dep for
		 * later processing, so that post-order processing can finish first.
		 * The depth of the theta itself is known after all. */
		vl_compute_depth(vli, get_Theta_init(irn), todo);
		plist_insert_back(todo, get_Theta_next(irn));

		/* Use the known theta depth. */
		vln = phase_get_or_set_irn_data(vli->phase, irn);
		vln->irn   = irn;
		vln->depth = get_Theta_depth(irn);
		plist_insert_back(vli->thetas, vln);
	} else {
		/* Recurse first and calculate post-order. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			vl_compute_depth(vli, ir_dep, todo);
		}

		/* Use this when no deps are present (can't be in a loop). */
		vln = phase_get_or_set_irn_data(vli->phase, irn);
		vln->irn   = irn;
		vln->depth = 0;

		/* Calculate minimal and maximal depth of the deps. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			vl_node *vl_dep = phase_get_irn_data(vli->phase, ir_dep);
			assert(vl_dep);

			/* Just take the nodes depth on the first try. */
			vln->depth = i ? MAX(vln->depth, vl_dep->depth) : vl_dep->depth;
		}

		/* Leave nodes on eta. */
		if (is_Eta(irn)) {
			vln->depth--;
			assert(vln->depth >= 0);
			plist_insert_back(vli->etas, vln);
		}
	}
}

static void vl_compute_links(vl_info *vli, vl_node *vln, vl_node_eta *vl_eta)
{
	int i;

	if (irn_visited(vln->irn)) return;
	mark_irn_visited(vln->irn);

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(vln->irn); i++) {
		ir_node *ir_dep = get_irn_n(vln->irn, i);
		vl_node *vl_dep = phase_get_irn_data(vli->phase, ir_dep);
		assert(vl_dep);

		/* Check if we leave the loop area here. */
		if ((vl_dep != (vl_node*)vl_eta) &&
		    (vl_dep->depth <= vl_eta->base.depth)) {

			/* If so, store the edge in vl_eta and don't recurse. */
			vl_edge *ple = OALLOC(&vli->obst, vl_edge);
			ple->src = vln->irn;
			ple->dst = vl_dep->irn;
			plist_insert_back(vl_eta->border, ple);

		} else {
			vl_compute_links(vli, vl_dep, vl_eta);
		}
	}

	/* Assign the eta node to all irns in the eta (but not deeper). */
	if (vln->depth == (vl_eta->base.depth + 1)) {
		plist_insert_back(vln->etas, vl_eta);

		/* Store theta links on the eta node. */
		if (is_Theta(vln->irn)) {
			plist_insert_back(vl_eta->thetas, vln);
		}
	}
}

vl_info *vl_init(ir_graph *irg)
{
	plist_element_t *it;

	plist_t *todo;
	ir_node *end = get_irg_end_block(irg);
	ir_node *ret = get_Block_cfgpred(end, 0);
	vl_info *vli = XMALLOC(vl_info);
	assert(is_Return(ret) && "Invalid PEG graph.");

	obstack_init(&vli->obst);
	vli->phase  = new_phase(irg, vl_init_node);
	vli->etas   = plist_obstack_new(&vli->obst);
	vli->thetas = plist_obstack_new(&vli->obst);
	phase_set_private(vli->phase, vli);

	/* Do the depth analysis by processing acyclic fragments of the graph.
	 * On every theta node, analysis stops and the fragment on the thetas
	 * next dependency is added to the queue for later processing. */

	todo = plist_obstack_new(&vli->obst);
	plist_insert_back(todo, ret);
	inc_irg_visited(irg); /* Only reset once. */

	while (plist_count(todo) > 0) {
		ir_node *next = plist_first(todo)->data;
		plist_erase(todo, plist_first(todo));
		vl_compute_depth(vli, next, todo);
	}

	/* Collect thetas for each eta node. Can we improve this? */
	foreach_plist(vli->etas, it) {
		inc_irg_visited(irg);
		vl_compute_links(vli, it->data, it->data);
	}

	return vli;
}

void vl_free(vl_info *vli)
{
	phase_free(vli->phase);
	obstack_free(&vli->obst, NULL);
	xfree(vli);
}

ir_graph *vl_get_irg(vl_info *info)
{
	return phase_get_irg(info->phase);
}

static void vl_dump_irn(vl_info *vli, ir_node *irn, FILE* f)
{
	int i;
	vl_node *vln;
	plist_element_t *it;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Recurse deeper. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		vl_dump_irn(vli, ir_dep, f);
	}

	vln = phase_get_irn_data(vli->phase, irn);
	fprintf(f, "depth (%3li) = %i\n", get_irn_node_nr(irn), vln->depth);

	if (vln->etas) {
		/* Output the list of associated eta nodes. */
		fprintf(f, "etas  (%3li) = {", get_irn_node_nr(irn));
		foreach_plist(vln->etas, it) {
			if (it != plist_first(vln->etas)) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(((vl_node*)it->data)->irn));
		}
		fprintf(f, "}\n");

		/* For eta nodes also output the thetas and the border. */
		if (vln->type == vl_type_eta) {
			vl_node_eta *vln_eta = (vl_node_eta*)vln;

			fprintf(f, "thetas(%3li) = {", get_irn_node_nr(irn));
			foreach_plist(vln_eta->thetas, it) {
				if (it != plist_first(vln_eta->thetas)) fprintf(f, ", ");
				fprintf(f, "%li", get_irn_node_nr(((vl_node*)it->data)->irn));
			}
			fprintf(f, "}\n");

			fprintf(f, "border(%3li) = {", get_irn_node_nr(irn));
			foreach_plist(vln_eta->border, it) {
				if (it != plist_first(vln_eta->border)) fprintf(f, ", ");
				fprintf(f, "%li", get_irn_node_nr(((vl_node*)it->data)->irn));
			}
			fprintf(f, "}\n");
		}
	}
}

void vl_dump(vl_info *vli, FILE* f)
{
	ir_graph *irg = phase_get_irg(vli->phase);
	ir_node  *end = get_irg_end_block(irg);
	ir_node  *ret = get_Block_cfgpred(end, 0);
	assert(ret);

	/* Walk the tree and dump every node. */
	inc_irg_visited(irg);
	vl_dump_irn(vli, ret, f);
}

/******************************************************************************
 * Public interfaces and iterators.                                           *
 ******************************************************************************/

/* Graph etas */
int vl_get_eta_count(vl_info *vli)
{
	return plist_count(vli->etas);
}

void vl_eta_it_init(vl_info *vli, vl_eta_it *it)
{
	*it = plist_first(vli->etas);
}

ir_node *vl_eta_it_next(vl_eta_it *it)
{
	if (!*it) return NULL;
	vl_node *vln = (*it)->data; *it = (*it)->next;
	return vln->irn;
}

/* Graph thetas */
int vl_get_theta_count(vl_info *vli)
{
	return plist_count(vli->thetas);
}

void vl_theta_it_init(vl_info *vli, vl_theta_it *it)
{
	*it = plist_first(vli->thetas);
}

ir_node *vl_theta_it_next(vl_theta_it *it)
{
	if (!*it) return NULL;
	vl_node *vln = (*it)->data; *it = (*it)->next;
	return vln->irn;
}

/* Etas */
int vl_node_get_eta_count(vl_info *vli, ir_node *irn)
{
	vl_node *vln = phase_get_irn_data(vli->phase, irn);
	assert(vln && "No loop information for the given node.");
	return plist_count(vln->etas);
}

void vl_node_eta_it_init(vl_info *vli, vl_node_eta_it *it, ir_node *irn)
{
	vl_node *vln = phase_get_irn_data(vli->phase, irn);
	assert(vln && "No loop information for the given node.");
	*it = plist_first(vln->etas);
}

ir_node *vl_node_eta_it_next(vl_eta_theta_it *it)
{
	if (!*it) return NULL;
	vl_node *vln = (*it)->data; *it = (*it)->next;
	return vln->irn;
}

/* Thetas */
int vl_eta_get_theta_count(vl_info *vli, ir_node *eta)
{
	vl_node_eta *vln = phase_get_irn_data(vli->phase, eta);
	assert(vln && "No loop information for the given node.");
	assert((vln->base.type == vl_type_eta) && "Not an eta node.");
	return plist_count(vln->thetas);
}

void vl_eta_theta_it_init(vl_info *vli, vl_eta_theta_it *it, ir_node *eta)
{
	vl_node_eta *vln = phase_get_irn_data(vli->phase, eta);
	assert(vln && "No loop information for the given node.");
	assert((vln->base.type == vl_type_eta) && "Not an eta node.");
	*it = plist_first(vln->thetas);
}

ir_node *vl_eta_theta_it_next(vl_eta_theta_it *it)
{
	if (!*it) return NULL;
	vl_node *vln = (*it)->data; *it = (*it)->next;
	return vln->irn;
}

/* Invariants */
int vl_eta_get_invar_count(vl_info *vli, ir_node *eta)
{
	vl_node_eta *vln = phase_get_irn_data(vli->phase, eta);
	assert(vln && "No loop information for the given node.");
	assert((vln->base.type == vl_type_eta) && "Not an eta node.");
	return plist_count(vln->border);
}

void vl_eta_invar_it_init(vl_info *vli, vl_eta_invar_it *it, ir_node *eta)
{
	vl_node_eta *vln = phase_get_irn_data(vli->phase, eta);
	assert(vln && "No loop information for the given node.");
	assert((vln->base.type == vl_type_eta) && "Not an eta node.");
	*it = plist_first(vln->border);
}

int vl_eta_invar_it_next(vl_eta_invar_it *it, vl_edge *edge)
{
	if (!*it) {
		edge->src = NULL;
		edge->dst = NULL;
		return 0;
	}

	*edge = *((vl_edge*)(*it)->data);
	*it = plist_element_get_next(*it);
	return 1;
}

void vl_eta_theta_change(vl_info *vli, ir_node *eta, ir_node *old, ir_node *new)
{
	plist_element_t *it;

	vl_node_eta *vln = phase_get_irn_data(vli->phase, eta);
	assert(vln && "No loop information for the given node.");
	assert((vln->base.type == vl_type_eta) && "Not an eta node.");

	vl_node *vl_old = phase_get_irn_data(vli->phase, old);

	it = plist_find_value(vln->thetas, vl_old);
	if (it) {
		vl_node *vl_new = phase_get_or_set_irn_data(vli->phase, new);
		vl_new->depth = vl_old->depth;
		vl_new->etas  = vl_old->etas;
		vl_new->irn   = new;
		vl_new->type  = vl_old->type;

		plist_insert_after(vln->thetas, it, vl_new);
		plist_erase(vln->thetas, it);
	}
}
