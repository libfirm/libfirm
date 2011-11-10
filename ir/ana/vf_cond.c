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
 * @brief   Gating condition analysis for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include <assert.h>
#include "firm_types.h"
#include "hashptr.h"
#include "plist.h"
#include "obst.h"
#include "pmap_new.h"
#include "irnodemap.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "vf_cond.h"

#define VF_DEBUG_CALCULATIONS 0
//#define VF_DEBUG_CONDITIONS   1

typedef struct obstack obstack;

/* Convenience macro to iterate two pmaps in succession. */
#define foreach_pmap_2(lhs, rhs, entry, i, it) \
	for ((i) = 0; (i) < 2; (i)++) \
		foreach_pmap_new(((i == 0) ? (lhs) : (rhs)), (entry), (it))

typedef struct vc_node {
	ir_node    *irn;

	/* Contains conds for dominated >children< (no other dominated nodes) and
	 * undominated other nodes that are reachable. */
	pmap_new_t  conds;

	/* Contains conds for >siblings< we can reach and nodes we can reach from
	 * there, that the sibling doesn't dominate itself. */
	pmap_new_t *cross_conds;

} vc_node;

/* Gating Condition info. */
struct vc_info {
	obstack     obst;
	vd_info    *vdi;
	vf_info    *vfi;
	vc_node    *root;
	ir_nodemap *nodemap;
	ir_node    *block;
};

static int vc_skip_node(vc_info *vci, ir_node *irn)
{
	return vci->block && (get_nodes_block(irn) != vci->block);
}

/******************************************************************************
 * Build gating conditions for nodes.                                         *
 ******************************************************************************/

/* Compute a condition for a single edge. */
static vf_cond vc_edge_cond(vc_info *vci, ir_node *src, ir_node *dst)
{
	/* On gamma edges return an appropriate literal. */
	if (is_Gamma(src)) {

		char value = -1;
		if      (get_Gamma_true(src)  == dst) value = 1;
		else if (get_Gamma_false(src) == dst) value = 0;

		if (value >= 0)
		{
			return vf_cond_new(vci->vfi, get_Gamma_cond(src), value);
		}
	}

	return vf_cond_get_true();
}

/* Store the condition in the given map for the given target. If needed,
 * OR-combine the condition with the existing condition. */
static void vc_store_cond(vc_info *vci, pmap_new_t *map, ir_node *dst,
                          vf_cond cond)
{
	/* Allocate space for the new condition (we have to provide a reliable
	 * address to store it in the map) and try a lookup in the given map. */
	vf_cond *new_cond = OALLOC(&vci->obst, vf_cond);
	vf_cond *map_cond = pmap_new_get(map, dst);

	/* Either combine the existing and new condition or just use the new. */
	if (map_cond)
	{
		*new_cond = vf_cond_or(vci->vfi, *map_cond, cond);

#if VF_DEBUG_CALCULATIONS
		vf_cond_dump(*map_cond, stdout);
		printf(" OR ");
		vf_cond_dump(cond, stdout);
		printf(" = ");
#endif
	}
	else
	{
		*new_cond = cond;
	}

#if VF_DEBUG_CALCULATIONS
	vf_cond_dump(*new_cond, stdout);
	printf("\n");
#endif

	assert(!vc_skip_node(vci, dst));
	pmap_new_insert(map, dst, new_cond);
}

static vf_cond vc_load_cond(pmap_new_t *map, ir_node *dst)
{
	vf_cond *map_cond = pmap_new_get(map, dst);
	if (!map_cond) return vf_cond_get_false();
	return *map_cond;
}

static void vc_compute_cross_conds(vc_info *vci, vc_node *vc_parent,
                                   vc_node *vc_src)
{
	pmap_new_iterator_t it_path, it_cont;
	pmap_new_entry_t    en_path, en_cont;
	ir_node *ir_parent = vc_parent->irn;

	/* Skip finished children (normal and cross conds known). */
	if (vc_src->cross_conds) return;
	if (vc_skip_node(vci, vc_src->irn)) return;

	vc_src->cross_conds = XMALLOC(pmap_new_t);
	pmap_new_init(vc_src->cross_conds);

	/* Scan conventional conds in the child subgraph. */
	foreach_pmap_new(&vc_src->conds, en_path, it_path) {
		vf_cond *vf_path = en_path.value;
		ir_node *ir_dst  = en_path.key;

		/**
		 * Try to find two gating paths like this:
		 *
		 *    cont     path
		 *  /------\ /------\
		 *  |      | |      |
		 *  v      | v      |
		 * app     dst     src
		 *           |      |
		 *           |      |x
		 *           \      /
		 *            parent
		 *
		 * The conditions for the combined (non-gating) path path.x is a
		 * cross condition. When adding the edge cont later, the combined
		 * path cont.path.x will become a gating path.
		 */

		/* Check if the parent of ir_dst in the dom tree is ir_parent. */
		if (vd_node_get_parent(vci->vdi, ir_dst) == ir_parent) {
			int i;

			/* If it is, first calculate all the remaining conds in ir_dst. */
			vc_node *vc_dst = ir_nodemap_get_fast(vci->nodemap, ir_dst);
			vc_compute_cross_conds(vci, vc_parent, vc_dst);
			assert(vc_dst->cross_conds); /* Normal and cross conds known. */

			/* Concatenate the path gating conds with the continuations. */
			foreach_pmap_2(&vc_dst->conds, vc_dst->cross_conds, en_cont, i, it_cont) {
				vf_cond  vf_full;
				vf_cond *vf_cont = en_cont.value;
				ir_node *ir_app  = en_cont.key;

				/* Skip continuations where ir_dst dominates ir_app. There is
				 * no need to add further information to those conds. */
				if (vd_node_dominates(vci->vdi, ir_dst, ir_app)) continue;

#if VF_DEBUG_CALCULATIONS
				printf("XC %ld -> %ld -> %ld: ",
					get_irn_node_nr(vc_src->irn),
					get_irn_node_nr(ir_dst),
					get_irn_node_nr(ir_app)
				);
#endif

				/* AND-combine the conditions to get a new condition for ir_src ->
				 * ir_app and OR-combine it with the existing ones on store. */
				vf_full = vf_cond_and(vci->vfi, *vf_path, *vf_cont);
				vc_store_cond(vci, vc_src->cross_conds, ir_app, vf_full);
			}
		}
	}
}

static vc_node *vc_init_node(vc_info *vci, const ir_node *irn)
{
	vc_node *vcn = OALLOC(&vci->obst, vc_node);

	/* Initialize the condition maps. It's not obstacked, but we only allocate
	 * them once, so allocation speed isn't that much of an issue. */

	pmap_new_init(&vcn->conds);
	vcn->cross_conds = NULL;
	vcn->irn         = (ir_node*)irn;

	ir_nodemap_insert(vci->nodemap, irn, vcn);

	return vcn;
}

static void vc_compute_conds(vc_info *vci, ir_node *irn)
{
	vd_node_child_it it_path;

	int i, j;

	/**
	 * Define variables for the nodes no process. Paths are layed out like:
	 * src ------> dst ------> app     (source / destination / appendix)
	 *      path        cont           (path / continuation)
	 *            full                 (full path)
	 */

	vf_cond  vf_path, vf_full, *vf_cont;
	vc_node *vc_src;
	vc_node *vc_dst;
	ir_node *ir_src = irn;
	ir_node *ir_dst, *ir_app;

	if (vc_skip_node(vci, irn)) return;
	vc_src = ir_nodemap_get(vci->nodemap, irn);
	if (vc_src == NULL) {
		vc_src = vc_init_node(vci, irn);
	}

	assert(!is_Eta(irn));

	/* Recurse to the children. */
	foreach_vd_node_child(vci->vdi, ir_src, ir_dst, it_path) {
		vc_compute_conds(vci, ir_dst);
	}

	/* Compute crossing conds for the children. */
	foreach_vd_node_child(vci->vdi, ir_src, ir_dst, it_path) {
		if (vc_skip_node(vci, ir_dst)) continue;

		vc_dst = ir_nodemap_get(vci->nodemap, ir_dst);
		if (vc_dst == NULL) {
			vc_dst = vc_init_node(vci, ir_dst);
		}
		vc_compute_cross_conds(vci, vc_src, vc_dst);
	}

	/* Determine and store gating conds of length 1 from ir_src. */
	for (i = 0; i < get_irn_arity(ir_src); i++) {
		ir_dst = get_irn_n(ir_src, i);

		if (vc_skip_node(vci, ir_dst)) continue;
		vf_path = vc_edge_cond(vci, ir_src, ir_dst);

#if VF_DEBUG_CALCULATIONS
		printf("EC %ld -- %ld: ",
			get_irn_node_nr(ir_src),
			get_irn_node_nr(ir_dst)
		);
#endif

		vc_store_cond(vci, &vc_src->conds, ir_dst, vf_path);
	}

	/* Combine gating conditions to children with unfinished conditions that
	 * are stored there. Those are conditions for nodes not yet dominated. */
	foreach_vd_node_child(vci->vdi, ir_src, ir_dst, it_path) {
		pmap_new_iterator_t  it_cont;
		pmap_new_entry_t     en_cont;

		if (vc_skip_node(vci, ir_dst)) continue;

		/* Get the condition for the paths src -> dst. This may be false if no
		 * condition has been determined yet. Consider:
		 *
		 *     c
		 *    / \
		 *   a   b        a  c  b
		 *    \ /          \ | /
		 *     d             d
		 *
		 *    Dep           Dom
		 *
		 * c is a children of d in the dominance tree. However the only paths
		 * from d that we have unveiled up to here are d -> a and d -> b. So
		 * false is the correct answer for the condition of d -> c. */

		vf_path = vc_load_cond(&vc_src->conds, ir_dst);

		/* Now AND-combine the condition with (cross) conditions found below
		 * ir_dst, to obtain conditions other target nodes. Do not process the
		 * paths that lead to nodes dominated by ir_dst. We don't need to add
		 * further information (aka the ir_dst condition) to them. */
		vc_dst = ir_nodemap_get(vci->nodemap, ir_dst);
		if (vc_dst == NULL) {
			vc_dst = vc_init_node(vci, ir_dst);
		}
		assert(vc_dst);

		foreach_pmap_2(&vc_dst->conds, vc_dst->cross_conds, en_cont, j, it_cont) {
			/* Get the "appendix" node and condition. */
			ir_app  = en_cont.key;
			vf_cont = en_cont.value;

			if (vd_node_dominates(vci->vdi, ir_dst, ir_app)) continue;

			/* AND-combine the conditions to get a new condition for ir_src ->
			 * ir_app and OR-combine it with the existing ones on store. */
			vf_full = vf_cond_and(vci->vfi, vf_path, *vf_cont);

#if VF_DEBUG_CALCULATIONS
			printf("NC %ld -> %ld -> %ld: ",
				get_irn_node_nr(ir_src),
				get_irn_node_nr(ir_dst),
				get_irn_node_nr(ir_app)
			);
			printf("\n   ");
			vf_cond_dump(vf_path, stdout);
			printf(" AND ");
			vf_cond_dump(*vf_cont, stdout);
			printf(" = ");
			vf_cond_dump(vf_full, stdout);
			printf("\n   ");
#endif

			vc_store_cond(vci, &vc_src->conds, ir_app, vf_full);
		}
	}

	/* Free the child cross conds again. */
	foreach_vd_node_child(vci->vdi, ir_src, ir_dst, it_path) {
		if (vc_skip_node(vci, ir_dst)) continue;

		vc_dst = ir_nodemap_get(vci->nodemap, ir_dst);
		if (vc_dst == NULL) {
			vc_dst = vc_init_node(vci, ir_dst);
		}

		pmap_new_destroy(vc_dst->cross_conds);
		xfree(vc_dst->cross_conds);
		vc_dst->cross_conds = NULL;
	}
}

int vc_node_get_rel_cond(vc_info *vci, ir_node *irn, vc_rel_cond *rel_cond)
{
	vc_node *vcn;
	vf_cond *cond;

	/* Special case for the root (return) node. */
	if (irn == vci->root->irn) {
		rel_cond->idom = NULL;
		rel_cond->cond = vf_cond_get_true();
		return 1;
	}

	assert(!vc_skip_node(vci, irn));

	rel_cond->idom = vd_node_get_parent(vci->vdi, irn);
	if (!rel_cond->idom) return 0;

	vcn = ir_nodemap_get(vci->nodemap, rel_cond->idom);
	if (!vcn) return 0;

	/* Try to look up the nodes condition in the parent. */
	cond = pmap_new_get(&vcn->conds, irn);
	if (!cond) return 0;

	rel_cond->cond = *cond;
	return 1;
}

vc_info *vc_init_root(ir_node *root, int keep_block)
{
	vc_info  *vci = XMALLOC(vc_info);
	ir_graph *irg = get_irn_irg(root);

	obstack_init(&vci->obst);
	vci->vdi   = vd_init_root(root, keep_block);
	vci->vfi   = vf_init();
	vci->block = keep_block ? get_nodes_block(root) : NULL;

#if VF_DEBUG_CONDITIONS || VF_DEBUG_CALCULATIONS
	printf("+------------------------------------------------+\n");
	printf("| Conditions                                     |\n");
	printf("+------------------------------------------------+\n");
#endif

	ir_nodemap_init(vci->nodemap, irg);

	/* Walk through the tree to compute directions. */
	vc_compute_conds(vci, root);
	vci->root = ir_nodemap_get(vci->nodemap, root);
	assert(vci->root);

#if VF_DEBUG_CONDITIONS
	vc_dump(vci, stdout);
#endif

	return vci;
}

void vc_free(vc_info *vci)
{
	size_t pn;

	/* Free all the maps in the nodes. */
	for (pn = 0; pn < ARR_LEN(vci->nodemap->data); ++pn) {
		vc_node *vcn = vci->nodemap->data[pn];
		assert(vcn);

		pmap_new_destroy(&vcn->conds);
		assert(!vcn->cross_conds);
	}

	vf_free(vci->vfi);
	vd_free(vci->vdi);

	ir_nodemap_destroy(vci->nodemap);
	obstack_free(&vci->obst, NULL);
	xfree(vci);
}

static void vc_dump_node(vc_info *vci, ir_node *irn, FILE *f, int indent)
{
	vd_node_child_it it;

	ir_node *ir_child;
	vc_node *vcn = ir_nodemap_get(vci->nodemap, irn);
	if (!vcn) return;

	foreach_vd_node_child(vci->vdi, vcn->irn, ir_child, it)
	{
		int      i;
		vf_cond *cond = pmap_new_get(&vcn->conds, ir_child);
		if (!cond) continue;

		for (i = 0; i < indent; i++) fprintf(f, "  ");

		fprintf(f, "%s %li",
			get_op_name(get_irn_op(ir_child)),
			get_irn_node_nr(ir_child)
		);

		fprintf(f, ": ");
		vf_cond_dump(*cond, f);
		fprintf(f, "\n");

		vc_dump_node(vci, ir_child, f, indent + 1);
	}
}

void vc_dump(vc_info *vci, FILE *f)
{
	fprintf(f, "%s %li\n",
		get_op_name(get_irn_op(vci->root->irn)),
		get_irn_node_nr(vci->root->irn)
	);

	vc_dump_node(vci, vci->root->irn, f, 1);
}

ir_node *vc_get_root(vc_info *vci)
{
	return vci->root->irn;
}

vd_info *vc_get_vd_info(vc_info *vci)
{
	return vci->vdi;
}

vf_info *vc_get_vf_info(vc_info *vci)
{
	return vci->vfi;
}
