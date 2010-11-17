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
 * @brief   Compute the dominance tree for PEG graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "peg_dom_t.h"
#include "irphase_t.h"
#include "plist.h"
#include "iredges.h"

typedef struct pd_node {
	ir_node        *irn;
	char            defined;
	int             index;
	int             min_index;
	int             max_index;
	plist_t        *children;
	struct pd_node *parent;
} pd_node;

/* PEG Dominator tree. */
struct pd_tree {
	struct obstack  obst;
	pd_node        *root;
	ir_phase       *phase;
};

static void pd_set_parent(pd_node *parent, pd_node *child)
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

/* Calculate post-order indices. */
static int pd_compute_indices_post(pd_tree *tree, ir_node *irn, int counter)
{
	int i;
	pd_node *pdn;

	if (irn_visited(irn)) return counter;
	mark_irn_visited(irn);

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *dep = get_irn_n(irn, i);
		counter = pd_compute_indices_post(tree, dep, counter);
	}

	/* Postorder indices. */
	pdn = phase_get_or_set_irn_data(tree->phase, irn);
	pdn->irn   = irn;
	pdn->index = counter++;
	return counter;
}

/* Calculate indices for queries. */
static int pd_compute_indices_dom(pd_node *pdn, int counter)
{
	plist_element_t *it;
	pdn->index = counter;
	pdn->min_index = counter;

	foreach_plist(pdn->children, it) {
		counter++;
		counter = pd_compute_indices_dom(it->data, counter);
	}

	pdn->max_index = counter;
	return counter;
}

static pd_node *pd_compute_intersect(pd_node *lhs, pd_node *rhs)
{
	while (lhs != rhs) {
		while (lhs->index < rhs->index) lhs = lhs->parent;
		while (rhs->index < lhs->index) rhs = rhs->parent;
	}
	return lhs;
}

/* Paper: A simple, fast dominance algorithm. Keith et al. */
static int pd_compute(pd_tree *tree, ir_node *irn)
{
	int i, changed;

	if (irn_visited(irn)) return 0;
	mark_irn_visited(irn);
	changed = 0;

	if (irn != tree->root->irn) {
		const ir_edge_t *edge;
		pd_node *pd_idom = NULL;
		pd_node *pdn = phase_get_irn_data(tree->phase, irn);
		assert(pdn);

		/* Find new_idom. */
		foreach_out_edge(irn, edge) {
			ir_node *ir_src = get_edge_src_irn(edge);
			pd_node *pd_src = phase_get_irn_data(tree->phase, ir_src);
			if (!pd_src) continue;

			if (pd_src->defined) {
				pd_idom = pd_src;
				break;
			}
		}

		assert(pd_idom);

		/* For all others. */
		foreach_out_edge(irn, edge) {
			ir_node *ir_src = get_edge_src_irn(edge);
			pd_node *pd_src = phase_get_irn_data(tree->phase, ir_src);
			if (!pd_src) continue;

			if ((pd_src != pd_idom) && pd_src->defined) {
				pd_idom = pd_compute_intersect(pd_src, pd_idom);
			}
		}

		/* Link new_idom to the node. */
		if (pdn->parent != pd_idom) {
			pd_set_parent(pd_idom, pdn);
			pdn->defined = 1;
			changed = 1;
		}
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *dep = get_irn_n(irn, i);
		changed |= pd_compute(tree, dep);
	}

	return changed;
}

static void *pd_init_node(ir_phase *phase, const ir_node *irn)
{
	pd_tree *tree = phase_get_private(phase);
	pd_node *pdn  = OALLOCZ(&tree->obst, pd_node);

	(void)irn;
	pdn->children = plist_obstack_new(&tree->obst);
	return pdn;
}

pd_tree *pd_init(ir_graph *irg)
{
	/* Get the return node from the PEG, alloc the tree. */
	int       edges;
	int       changed = 1;
	ir_node  *end     = get_irg_end_block(irg);
	ir_node  *ret     = get_Block_cfgpred(end, 0);
	pd_tree  *tree    = XMALLOC(pd_tree);
	assert(is_Return(ret) && "Invalid PEG graph.");

	/* Prepare data structures for computation. */
	obstack_init(&tree->obst);
	tree->phase = new_phase(irg, pd_init_node);
	phase_set_private(tree->phase, tree);

	edges = edges_assure(irg);

	/* Setup the root node. */
	tree->root = phase_get_or_set_irn_data(tree->phase, ret);
	tree->root->irn     = ret;
	tree->root->defined = 1;

	/* Index nodes in post-order for the algorithm. */
	inc_irg_visited(irg);
	pd_compute_indices_post(tree, ret, 0);

	/* Compute the dominance tree. */
	while (changed) {
		inc_irg_visited(irg);
		changed = pd_compute(tree, ret);
	}

	/* Index nodes for fast queries. */
	pd_compute_indices_dom(tree->root, 0);

	if (!edges) edges_deactivate(irg);
	return tree;
}

void pd_free(pd_tree *tree)
{
	phase_free(tree->phase);
	obstack_free(&tree->obst, NULL);
	xfree(tree);
}

int pd_dominates(pd_tree *tree, ir_node *lhs, ir_node *rhs)
{
	/* Check for (non-strict) dominance. */
	pd_node *lhs_node = phase_get_irn_data(tree->phase, lhs);
	pd_node *rhs_node = phase_get_irn_data(tree->phase, rhs);

	return (rhs_node->index >= lhs_node->min_index) &&
	       (rhs_node->index <= lhs_node->max_index);
}

ir_node *pd_get_parent(pd_tree *tree, ir_node *irn)
{
	pd_node *pdn = phase_get_irn_data(tree->phase, irn);
	assert(pdn && "No dominance information for the given node.");
	return pdn->parent->irn;
}

ir_node *pd_get_child(pd_tree *tree, ir_node *irn, pd_iter *it)
{
	plist_element_t *plist_it;
	pd_node *pdn = phase_get_irn_data(tree->phase, irn);
	assert(pdn && "No dominance information for the given node.");

	/* Get the first child element. */
	plist_it = plist_first(pdn->children);
	if (!plist_it) return NULL;

	if (it != NULL) {
		*it = plist_first(pdn->children);
	}

	return ((pd_node*)plist_it->data)->irn;
}

ir_node *pd_iter_next(pd_iter *it)
{
	pd_node *cur;
	plist_element_t *plist_it = *it;
	if (!plist_it) return NULL;

	cur = plist_it->data;
	*it = plist_it->next;
	return cur->irn;
}

ir_node *pd_get_root(pd_tree *tree)
{
	return tree->root->irn;
}

static void pd_dump_node(pd_node *pdn, FILE *f, int indent)
{
	plist_element_t *it;
	int i;

	for (i = 0; i < indent; i++) fprintf(f, "  ");
	fprintf(f, "%s %li (%i - %i)\n",
		get_op_name(get_irn_op(pdn->irn)),
		get_irn_node_nr(pdn->irn),
		pdn->min_index, pdn->max_index
	);

	foreach_plist(pdn->children, it) {
		pd_dump_node(it->data, f, indent + 1);
	}
}

void pd_dump(pd_tree *tree, FILE *f)
{
	pd_dump_node(tree->root, f, 0);
}
