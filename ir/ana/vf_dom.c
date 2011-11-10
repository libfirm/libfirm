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
 * @brief   Compute the dominance tree for VFirm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "vf_dom.h"
#include "irnodemap.h"
#include "plist.h"
#include "iredges.h"
#include "obstack.h"

//#define VD_DEBUG_TREE 1

typedef struct obstack obstack;

typedef struct vd_node {
	ir_node        *irn;
	char            defined;
	int             index; /* = min_index */
	int             max_index;
	plist_t        *children;
	struct vd_node *parent;
} vd_node;

/* VFirm dominance tree. */
struct vd_info {
	obstack     obst;
	vd_node    *root;
	ir_nodemap *nodemap;
	ir_node    *block;
};

static void vd_set_parent(vd_node *parent, vd_node *child)
{
	/* Detach from the old parent if needed. */
	if (child->parent) {
		plist_element_t *it;
		it = plist_find_value(child->parent->children, child);
		assert(it);

		plist_erase(child->parent->children, it);
	}

	/* Add to the new parent. */
	child->parent = parent;
	plist_insert_back(parent->children, child);
}

static int vd_skip_node(vd_info *vdi, ir_node *irn)
{
	return vdi->block && (get_nodes_block(irn) != vdi->block);
}

static vd_node *vd_init_node(vd_info *info, const ir_node *irn)
{
	vd_node *vdn  = OALLOCZ(&info->obst, vd_node);

	vdn->children = plist_obstack_new(&info->obst);

	ir_nodemap_insert(info->nodemap, irn, vdn);

	return vdn;
}

/* Calculate post-order indices. */
static int vd_compute_indices_post(vd_info *vdi, ir_node *irn, int counter)
{
	int i;
	vd_node *vdn;

	if (irn_visited(irn) || vd_skip_node(vdi, irn)) return counter;
	mark_irn_visited(irn);

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *dep = get_irn_n(irn, i);
		counter = vd_compute_indices_post(vdi, dep, counter);
	}

	/* Postorder indices. */
	vdn = ir_nodemap_get(vdi->nodemap, irn);
	if (vdn == NULL) {
		vdn = vd_init_node(vdi, irn);
	}
	vdn->irn   = irn;
	vdn->index = counter++;
	return counter;
}

/* Calculate indices for queries. */
static int vd_compute_indices_dom(vd_node *vdn, int counter)
{
	plist_element_t *it;
	vdn->index = counter;

	foreach_plist(vdn->children, it) {
		counter = vd_compute_indices_dom(it->data, counter + 1);
	}

	vdn->max_index = counter;
	return counter;
}

static vd_node *vd_compute_intersect(vd_node *lhs, vd_node *rhs)
{
	while (lhs != rhs) {
		while (lhs->index < rhs->index) lhs = lhs->parent;
		while (rhs->index < lhs->index) rhs = rhs->parent;
	}
	return lhs;
}

/* Paper: A simple, fast dominance algorithm. Keith et al. */
static int vd_compute(vd_info *vdi, ir_node *irn)
{
	int i, changed;

	if (irn_visited(irn) || vd_skip_node(vdi, irn)) return 0;
	mark_irn_visited(irn);
	changed = 0;

	if (irn != vdi->root->irn) {
		const ir_edge_t *edge;
		vd_node *vd_idom = NULL;
		vd_node *vdn     = ir_nodemap_get(vdi->nodemap, irn);
		assert(vdn);

		/* Find new_idom. */
		foreach_out_edge(irn, edge) {
			ir_node *ir_src = get_edge_src_irn(edge);
			vd_node *vd_src;
			if (vd_skip_node(vdi, ir_src)) continue;

			vd_src = ir_nodemap_get(vdi->nodemap, ir_src);
			if (!vd_src) continue;

			if (vd_src->defined) {
				vd_idom = vd_src;
				break;
			}
		}

		assert(vd_idom);

		/* For all others. */
		foreach_out_edge(irn, edge) {
			ir_node *ir_src = get_edge_src_irn(edge);
			vd_node *vd_src;
			if (vd_skip_node(vdi, ir_src)) continue;

			vd_src = ir_nodemap_get(vdi->nodemap, ir_src);
			if (!vd_src) continue;

			if ((vd_src != vd_idom) && vd_src->defined) {
				vd_idom = vd_compute_intersect(vd_src, vd_idom);
			}
		}

		/* Link new_idom to the node. */
		if (vdn->parent != vd_idom) {
			vd_set_parent(vd_idom, vdn);
			vdn->defined = 1;
			changed = 1;
		}
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *dep = get_irn_n(irn, i);
		changed |= vd_compute(vdi, dep);
	}

	return changed;
}

vd_info *vd_init(ir_graph *irg)
{
	ir_node *end = get_irg_end_block(irg);
	ir_node *ret = get_Block_cfgpred(end, 0);
	assert(is_Return(ret) && "Invalid VFirm graph.");
	return vd_init_root(ret, 0);
}

vd_info *vd_init_root(ir_node *root, int keep_block)
{
	/* Get the return node from the PEG, alloc the tree. */
	int       edges;
	int       changed;
	vd_info  *vdi = XMALLOC(vd_info);
	ir_graph *irg = get_irn_irg(root);

	/* Prepare data structures for computation. */
	obstack_init(&vdi->obst);
	ir_nodemap_init(vdi->nodemap, irg);

	edges = edges_assure(irg);

	/* Setup the root node. */
	vdi->root = ir_nodemap_get(vdi->nodemap, root);
	if (vdi->root == NULL) {
		vdi->root = vd_init_node(vdi, root);
	}
	vdi->root->irn     = root;
	vdi->root->defined = 1;
	vdi->block         = keep_block ? get_nodes_block(root) : NULL;

	/* Index nodes in post-order for the algorithm. */
	inc_irg_visited(irg);
	vd_compute_indices_post(vdi, root, 0);

	/* Compute the dominance tree. */
	do {
		inc_irg_visited(irg);
		changed = vd_compute(vdi, root);
	} while(changed);

	/* Index nodes for fast queries. */
	vd_compute_indices_dom(vdi->root, 0);

	if (!edges) edges_deactivate(irg);

#if VD_DEBUG_TREE
	printf("+------------------------------------------------+\n");
	printf("| Dominance Tree                                 |\n");
	printf("+------------------------------------------------+\n");
	vd_dump(vdi, stdout);
#endif

	return vdi;
}

void vd_free(vd_info *vdi)
{
	ir_nodemap_destroy(vdi->nodemap);
	obstack_free(&vdi->obst, NULL);
	xfree(vdi);
}

int vd_node_dominates(vd_info *vdi, ir_node *lhs, ir_node *rhs)
{
	/* Check for (non-strict) dominance. */
	vd_node *lhs_node = ir_nodemap_get(vdi->nodemap, lhs);
	vd_node *rhs_node = ir_nodemap_get(vdi->nodemap, rhs);

	return (rhs_node->index >= lhs_node->index) &&
	       (rhs_node->index <= lhs_node->max_index);
}

ir_node *vd_node_get_parent(vd_info *vdi, ir_node *irn)
{
	vd_node *vdn = ir_nodemap_get(vdi->nodemap, irn);
	assert(vdn && "No dominance information for the given node.");
	return vdn->parent ? vdn->parent->irn : NULL;
}

int vd_node_get_child_count(vd_info *vdi, ir_node *irn)
{
	vd_node *vdn = ir_nodemap_get(vdi->nodemap, irn);
	assert(vdn && "No dominance information for the given node.");
	return plist_count(vdn->children);
}

void vd_node_child_it_init(vd_info *vdi, vd_node_child_it *it, ir_node *irn)
{
	vd_node *vdn = ir_nodemap_get(vdi->nodemap, irn);
	assert(vdn && "No dominance information for the given node.");
	*it = plist_first(vdn->children);
}

ir_node *vd_node_child_it_next(vd_node_child_it *it)
{
	if (!*it) return NULL;

	vd_node *vdn = (*it)->data;
	*it = (*it)->next;
	return vdn->irn;
}

ir_node *vd_get_root(vd_info *vdi)
{
	return vdi->root->irn;
}

static void vd_dump_node(vd_node *vdn, FILE *f, int indent)
{
	plist_element_t *it;
	int i;

	for (i = 0; i < indent; i++) fprintf(f, "  ");
	fprintf(f, "%s %li\n",
		get_op_name(get_irn_op(vdn->irn)),
		get_irn_node_nr(vdn->irn)
	);

	foreach_plist(vdn->children, it) {
		vd_dump_node(it->data, f, indent + 1);
	}
}

void vd_dump(vd_info *vdi, FILE *f)
{
	vd_dump_node(vdi->root, f, 0);
}
