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
 * @brief   Program expression graphs to convert firm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "iroptimize.h"
#include "irgraph_t.h"
#include "irphase_t.h"
#include "irdump.h"
#include "irgwalk.h"
#include "array.h"
#include "irtools.h"
#include "irop_t.h"
#include "pset_new.h"
#include "plist.h"
#include "iredges.h"

#define LOG_DIRS_COMBINE   1
#define LOG_DOMINATOR_TREE 1
#define LOG_GATING_DIRS    1

/******************************************************************************
 * Dominator trees.                                                           *
 ******************************************************************************/

/**
 * Alas dominator analysis in firm is limited to CFG blocks. We need dominator
 * information for individual nodes with the return node being the root. These
 * functions provide that information. Use pd_init to calculate the dom tree
 * and pd_free to delete it. pd_get_node can then be used, to get the dominator
 * tree node for the given ir node and pd_dump can dump the tree to a file.
 * This is a simple implementation and not particularly smart or fast.
 */

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
typedef struct pd_tree {
	struct obstack  obst;
	pd_node        *root;
	ir_phase       *phase;
} pd_tree;

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

static void pd_dump(pd_tree *tree, FILE *f);

static pd_tree *pd_init(ir_graph *irg)
{
	// Get the return node from the PEG, alloc the tree.
	int       changed = 1;
	ir_node  *end     = get_irg_end_block(irg);
	ir_node  *ret     = get_Block_cfgpred(end, 0);
	pd_tree  *tree    = XMALLOC(pd_tree);
	assert(is_Return(ret)); // The PEG has to be well-formed.

	// Prepare data structures for computation.
	obstack_init(&tree->obst);
	tree->phase = new_phase(irg, pd_init_node);
	phase_set_private(tree->phase, tree);
	edges_activate(irg);

	// Setup the root node.
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

#ifdef LOG_DOMINATOR_TREE
	pd_dump(tree, stdout);
#endif

	edges_deactivate(irg);
	return tree;
}

static void pd_free(pd_tree *tree)
{
	phase_free(tree->phase);
	obstack_free(&tree->obst, NULL);
	xfree(tree);
}

static int pd_dominates(pd_tree *tree, ir_node *lhs, ir_node *rhs)
{
	/* Check for (non-strict) dominance. */
	pd_node *lhs_node = phase_get_irn_data(tree->phase, lhs);
	pd_node *rhs_node = phase_get_irn_data(tree->phase, rhs);

	return (rhs_node->index >= lhs_node->min_index) &&
	       (rhs_node->index <= lhs_node->max_index);
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

static void pd_dump(pd_tree *tree, FILE *f)
{
	fprintf(f, "------------------\n");
	fprintf(f, "PEG dominator tree\n");
	fprintf(f, "------------------\n");
	pd_dump_node(tree->root, f, 0);
}

/******************************************************************************
 * Gating conditions.                                                         *
 ******************************************************************************/

/**
 * Gating conditions are just plain conditions without a relationship to nodes.
 * They are considered immutable, so any modification yields a new condition.
 * However due to their immutability, partial conditions can often be reused.
 */

typedef enum gc_type {
	gct_demand, /* lambda */
	gct_ignore, /* emptyset */
	gct_branch, /* gamma(g, a, b) */
	gct_union   /* a u b */
} gc_type;

typedef struct gc_cond {
	gc_type type;
} gc_cond;

/* A pointer to a struct is guaranteed to also point to the first member. */
typedef struct gc_branch {
	gc_cond  base;
	ir_node *gamma;
	gc_cond *lhs;
	gc_cond *rhs;
} gc_branch;

/* Unions form a left-recursive linked list. a u b u c = (a u b) u c. */
typedef struct gc_union {
	gc_cond  base;
	gc_cond *lhs;
	gc_cond *rhs;
} gc_union;

/**
 * Directions represent a bunch of paths that go from a source node to a target
 * and have an associated gating condition to describe them.
 */

typedef struct gc_dirs {
	ir_node *source;
	ir_node *target;
	gc_cond *cond;
} gc_dirs;

/**
 * A map is used to store multiple directions to different targets. This may
 * become a real map structure at some time, to perform better.
 */

typedef struct gc_map {
	plist_t *dirs;
} gc_map;

/**
 * The node stores all directions originating at the associated irn. The cross
 * directions are used during computation. These are paths that cross into the
 * subtrees of a sibling in the dominator tree.
 */

typedef struct gc_node {
	ir_node *irn;
	gc_map  *map;
	gc_map  *cross_map;
} gc_node;

/* Gating Condition info. */
typedef struct gc_info {
	struct obstack  obst;
	pd_tree  *tree;
	gc_node  *root;
	ir_phase *phase;
} gc_info;

/* Iterate a union (going through the child unions). */
static gc_cond *gc_union_next(gc_cond **it)
{
	gc_cond *cur = *it;
	if (!cur) return NULL;

	if (cur->type == gct_union) {
		*it = ((gc_union*)cur)->lhs;
		cur = ((gc_union*)cur)->rhs;
		return cur;
	}

	*it = NULL;
	return cur;
}

#define foreach_gc_union(gcu, it, cur) \
	for ((it) = (gcu); (cur); (cur) = gc_union_next(&(it)))

/* Compare conditions. Not perfect, but should do its job. */
static int gc_cond_equals(gc_cond *lhs, gc_cond *rhs)
{
	if (lhs->type != rhs->type) return 0;

	switch (lhs->type) {
	case gct_demand: return 1; /* Types match, see above. */
	case gct_ignore: return 1;

	case gct_branch: {
		gc_branch *lhs_branch = (gc_branch*)lhs;
		gc_branch *rhs_branch = (gc_branch*)rhs;

		/* Compare the gamma node and both conditions. */
		if (!(lhs_branch->gamma == rhs_branch->gamma)) return 0;
		if (!gc_cond_equals(lhs_branch->lhs, rhs_branch->lhs)) return 0;
		if (!gc_cond_equals(lhs_branch->rhs, rhs_branch->rhs)) return 0;
		return 1;
	}
	case gct_union: {
		/* Tricky because the union is not ordered. */
		gc_cond *lhs_it, *rhs_it, *lhs_cur, *rhs_cur;
		int lhs_count = 0, rhs_count = 0;

		/* Compare element counts first. */
		foreach_gc_union(lhs, lhs_it, lhs_cur) lhs_count++;
		foreach_gc_union(rhs, rhs_it, rhs_cur) rhs_count++;
		if (lhs_count != rhs_count) return 0;

		/* Hopefully this won't happen too often and with small unions. */
		foreach_gc_union(lhs, lhs_it, lhs_cur) {
			int found = 0;

			foreach_gc_union(rhs, rhs_it, rhs_cur) {
				found = gc_cond_equals(lhs_cur, rhs_cur);
				if (found) break;
			}

			if (!found) return 0;
		}

		return 1;
	}}

	assert(0);
	return 0;
}

/******************************************************************************
 * Construct/simplify gating conditions.                                      *
 ******************************************************************************/

/**
 * Constructor methods. These construct new gating conditions. Note that the
 * simplification of conditions happens on-the-fly. So you might get some other
 * node back, than initially requested.
 */

static gc_cond *gc_new_demand(void)
{
	static gc_cond cond = { gct_demand };
	return &cond;
}

static gc_cond *gc_new_ignore(void)
{
	static gc_cond cond = { gct_ignore };
	return &cond;
}

static gc_cond *gc_new_branch(gc_info *info, gc_cond *lhs,
                              gc_cond *rhs, ir_node *gamma)
{
	/* G(g, a, a) = a */
	if (gc_cond_equals(lhs, rhs)) return lhs;

	gc_branch *gcb = OALLOC(&info->obst, gc_branch);
	gcb->base.type = gct_branch;
	gcb->lhs   = lhs;
	gcb->rhs   = rhs;
	gcb->gamma = gamma;
	return (gc_cond*)gcb;
}

/* This is a complex beast. */
static gc_cond *gc_new_union(gc_info *info, gc_cond *lhs, gc_cond *rhs)
{
	gc_cond *cur, *new_cur;

	/* Do not allow a rhs union in a union (left associative). */
	assert(rhs->type != gct_union);

	/* Swap A or 0 to the left. */
	if ((rhs->type == gct_demand) || (rhs->type == gct_ignore)) {
		gc_cond *tmp = lhs; lhs = rhs; rhs = tmp;
	}

	/* Try to optimize the full union away. */
	switch (lhs->type) {
	case gct_demand: return lhs; /* A u c = A */
	case gct_ignore: return rhs; /* 0 u c = c */
	default: break;
	}

	/* ((a u cur) u rhs) OR (cur u rhs) */
	cur = lhs; new_cur = NULL;
	if (lhs->type == gct_union) {
		cur = ((gc_union*)lhs)->rhs;
	}

	/* Try optimizations on all union members. Set new_cur if possible. */

	/* G(g, a, b) u G(g, c, d) = G(g, a u c, b u d) */
	if ((cur->type == gct_branch) && (rhs->type == gct_branch)) {
		gc_branch *cur_branch = (gc_branch*)cur;
		gc_branch *rhs_branch = (gc_branch*)rhs;

		if (cur_branch->gamma == rhs_branch->gamma) {
			/* Optimization can be applied. */
			new_cur = gc_new_branch(info,
				gc_new_union(info, cur_branch->lhs, rhs_branch->lhs),
				gc_new_union(info, cur_branch->rhs, rhs_branch->rhs),
				cur_branch->gamma
			);
		}
	}

	/* These optimizations may be applicable now. */
	if (new_cur) {
		switch (new_cur->type) {
		case gct_demand: return new_cur; /* a u A = A */
		case gct_ignore: /* a u 0 = a */
			if (lhs->type == gct_union) return ((gc_union*)lhs)->lhs;
			return new_cur;

		default: break;
		}
	}

	if (lhs->type != gct_union) {
		/* No tuple on the left. Return a single value or new tuple. */
		if (new_cur) {
			return new_cur;
		} else {
			gc_union *gcu = OALLOC(&info->obst, gc_union);
			gcu->base.type = gct_union;
			gcu->lhs = lhs;
			gcu->rhs = rhs;
			return (gc_cond*)gcu;
		}
	} else {
		/* Tuple on the left. Replace the rhs or recurse. */
		gc_union *lhs_gcu = (gc_union*)lhs;
		gc_union *gcu = OALLOC(&info->obst, gc_union);
		gcu->base.type = gct_union;

		if (new_cur) {
			gcu->lhs = lhs_gcu->lhs;
			gcu->rhs = new_cur;
			return (gc_cond*)gcu;
		} else {
			gcu->lhs = gc_new_union(info, lhs_gcu->lhs, rhs);
			gcu->rhs = lhs_gcu->rhs;
			return (gc_cond*)gcu;
		}
	}
}

static gc_cond *gc_new_concat(gc_info *info, gc_cond *lhs, gc_cond *rhs)
{
	/* Swap A or 0 to the left. */
	if ((rhs->type == gct_demand) || (rhs->type == gct_ignore)) {
		gc_cond *tmp = lhs; lhs = rhs; rhs = tmp;
	}

	switch (lhs->type) {
	case gct_demand: return rhs; /* A.c = c */
	case gct_ignore: return lhs; /* 0.c = 0 */
	case gct_branch: {
		/* G(g, a, b).c = G(g, a.c, b.c) */
		gc_branch *lhs_branch = (gc_branch*)lhs;

		return gc_new_branch(info,
			gc_new_concat(info, lhs_branch->lhs, rhs),
			gc_new_concat(info, lhs_branch->rhs, rhs),
			lhs_branch->gamma
		);
	}
	case gct_union: {
		/* (a u b).c = a.c u b.c */
		gc_union *lhs_union = (gc_union*)lhs;

		return gc_new_union(info,
			gc_new_concat(info, lhs_union->lhs, rhs),
			gc_new_concat(info, lhs_union->lhs, rhs)
		);
	}}

	assert(0);
	return NULL;
}

/******************************************************************************
 * Construct/combine directions and maps                                      *
 ******************************************************************************/

/**
 * Combine paths into directions. This is mostly wrapping around cond and some
 * debug logging to control gating condition simplification.
 */

/* Compute directions for a single edge. */
static gc_dirs *gc_new_edge_dirs(gc_info *info, ir_node *source,
                                 ir_node *target)
{
	gc_dirs *dirs = OALLOC(&info->obst, gc_dirs);
	dirs->target = target;
	dirs->source = source;

	if (is_Gamma(source)) {
		/* Determine the edge. */
		int edge = 2;
		if (get_Gamma_false(source) == target) edge = 0;
		if (get_Gamma_true(source)  == target) edge = 1;

		if (edge < 2) {
			dirs->cond = gc_new_branch(info,
				(edge == 0) ? gc_new_demand() : gc_new_ignore(),
				(edge == 0) ? gc_new_ignore() : gc_new_demand(),
				source
			);
		} else {
			/* Always demand condition edges. */
			dirs->cond = gc_new_demand();
		}
	} else {
		/* Always demand normal edges. */
		dirs->cond = gc_new_demand();
	}

	return dirs;
}

static void gc_dump_cond(gc_cond *cond, FILE *f);

static gc_dirs *gc_dirs_concat(gc_info *info, gc_dirs *lhs, gc_dirs *rhs)
{
	gc_dirs *dirs = OALLOC(&info->obst, gc_dirs);
	assert(lhs->target == rhs->source);

#ifdef LOG_DIRS_COMBINE
	printf("%3li -> %3li -> %3li: ",
		get_irn_node_nr(lhs->source),
		get_irn_node_nr(lhs->target),
		get_irn_node_nr(rhs->target)
	);
	printf("concat(");
	gc_dump_cond(lhs->cond, stdout); printf(", ");
	gc_dump_cond(rhs->cond, stdout); printf(") = ");
#endif

	dirs->cond   = gc_new_concat(info, lhs->cond, rhs->cond);
	dirs->source = lhs->source;
	dirs->target = rhs->target;

#ifdef LOG_DIRS_COMBINE
	gc_dump_cond(dirs->cond, stdout); printf("\n");
#endif

	return dirs;
}

static gc_dirs *gc_dirs_union(gc_info *info, gc_dirs *lhs, gc_dirs *rhs)
{
	gc_dirs *dirs = OALLOC(&info->obst, gc_dirs);
	assert((lhs->target == rhs->target) && (lhs->source == rhs->source));

#ifdef LOG_DIRS_COMBINE
	printf("%3li -> %3li:        ",
		get_irn_node_nr(lhs->source),
		get_irn_node_nr(lhs->target)
	);
	printf("union(");
	gc_dump_cond(lhs->cond, stdout); printf(", ");
	gc_dump_cond(rhs->cond, stdout); printf(") = ");
#endif

	dirs->cond   = gc_new_union(info, lhs->cond, rhs->cond);
	dirs->source = lhs->source;
	dirs->target = rhs->target;

#ifdef LOG_DIRS_COMBINE
	gc_dump_cond(dirs->cond, stdout); printf("\n");
#endif

	return dirs;
}

static void gc_map_merge(gc_info *info, gc_map *map, gc_dirs *dirs)
{
	plist_element_t *it;
	foreach_plist(map->dirs, it) {
		gc_dirs *it_dirs = it->data;
		assert(it_dirs->source == dirs->source);

		/* Combine dirs with the same target. */
		if (it_dirs->target == dirs->target) {
			/* it_dirs is only used by us, so we can be destructive. */
			it->data = gc_dirs_union(info, it_dirs, dirs);
			return;
		}
	}

	/* Just insert the path if merging doesn't work. */
	plist_insert_back(map->dirs, dirs);
}

/******************************************************************************
 * Populate maps with directions.                                             *
 ******************************************************************************/

static void gc_compute_cross_dirs(gc_info *info, pd_node *pdn,
                                  gc_node *gcn, pd_node *pd_lhs)
{
	plist_element_t *it_lhs, *it_mid, *it_rhs;

	gc_node *gc_lhs = phase_get_irn_data(info->phase, pd_lhs->irn);
	assert(gc_lhs);

	/* Skip children with known cross dirs. */
	if (gc_lhs->cross_map) return;

	gc_lhs->cross_map = OALLOC(&info->obst, gc_map);
	gc_lhs->cross_map->dirs = plist_obstack_new(&info->obst);

	/* Scan conventional dirs in the child subgraph. */
	foreach_plist(gc_lhs->map->dirs, it_lhs) {
		gc_dirs *lhs_dirs = it_lhs->data;

		/* Search for dirs that cross into another childs subgraph. */
		foreach_plist(pdn->children, it_mid) {
			pd_node *pd_rhs = it_mid->data;
			gc_node *gc_rhs;

			/* Skip boring dirs. */
			if (lhs_dirs->target != pd_rhs->irn) continue;

			/* Compute their cross dirs first. */
			gc_compute_cross_dirs(info, pdn, gcn, pd_rhs);

			gc_rhs = phase_get_irn_data(info->phase, pd_rhs->irn);
			assert(gc_rhs->cross_map);

			/* Form our own cross dirs by concatenation.
			 * First combine our cross paths with theirs. */
			foreach_plist(gc_rhs->cross_map->dirs, it_rhs) {
				gc_dirs *rhs_path = it_rhs->data;
				gc_dirs *cross_dirs;

				cross_dirs = gc_dirs_concat(info, lhs_dirs, rhs_path);
				gc_map_merge(info, gc_lhs->cross_map, cross_dirs);
			}

			/* Then combine our cross paths with their normal paths. */
			foreach_plist(gc_rhs->map->dirs, it_rhs) {
				gc_dirs *rhs_dirs = it_rhs->data;
				gc_dirs *cross_dirs;

				cross_dirs = gc_dirs_concat(info, lhs_dirs, rhs_dirs);
				gc_map_merge(info, gc_lhs->cross_map, cross_dirs);
			}
		}
	}
}

static void gc_compute_dirs(gc_info *info, pd_node *pdn)
{
	int i;
	plist_element_t *it;
	gc_node *gcn;

	/* Recurse first. */
	foreach_plist(pdn->children, it) {
		gc_compute_dirs(info, it->data);
	}

	gcn = phase_get_or_set_irn_data(info->phase, pdn->irn);
	gcn->irn = pdn->irn;

	/* Compute crossing dirs for the dominator children. */
	foreach_plist(pdn->children, it) {
		gc_compute_cross_dirs(info, pdn, gcn, it->data);
	}

	/* Now create dirs edge by edge. */
	for (i = 0; i < get_irn_arity(pdn->irn); i++) {
		ir_node *ir_dep = get_irn_n(pdn->irn, i);

		/* Add the trivial gating path of length 1. */
		gc_dirs *edge = gc_new_edge_dirs(info, gcn->irn, ir_dep);
		gc_map_merge(info, gcn->map, edge);

		/* For dominated target nodes add the combined dirs. */
		if (pd_dominates(info->tree, pdn->irn, ir_dep)) {
			gc_node *gc_dep = phase_get_irn_data(info->phase, ir_dep);
			assert(gc_dep);

			/* First all usual gating paths in gc_dep. */
			foreach_plist(gc_dep->map->dirs, it) {
				gc_dirs *dirs = gc_dirs_concat(info, edge, it->data);
				gc_map_merge(info, gcn->map, dirs);
			}

			/* Then all the crossing paths. */
			foreach_plist(gc_dep->cross_map->dirs, it) {
				gc_dirs *dirs = gc_dirs_concat(info, edge, it->data);
				gc_map_merge(info, gcn->map, dirs);
			}
		}
	}
}

static void *gc_init_node(ir_phase *phase, const ir_node *irn)
{
	gc_info *info = phase_get_private(phase);
	gc_node *gcn  = OALLOCZ(&info->obst, gc_node);

	/* Initialize the nodes directions map. */
	gcn->map = OALLOC(&info->obst, gc_map);
	gcn->map->dirs = plist_obstack_new(&info->obst);

	(void)irn;
	return gcn;
}

static void gc_dump(gc_info *info, FILE *f);

static gc_info *gc_init(ir_graph *irg)
{
	gc_info *info = XMALLOC(gc_info);
	obstack_init(&info->obst);
	info->tree = pd_init(irg);

	info->phase = new_phase(irg, gc_init_node);
	phase_set_private(info->phase, info);

#ifdef LOG_DIRS_COMBINE
	printf("--------------------\n");
	printf("Computing directions\n");
	printf("--------------------\n");
#endif

	/* Walk through the tree to compute directions. */
	gc_compute_dirs(info, info->tree->root);
	info->root = phase_get_irn_data(info->phase, info->tree->root->irn);

#ifdef LOG_GATING_DIRS
	gc_dump(info, stdout);
#endif

	return info;
}

static void gc_free(gc_info *info)
{
	pd_free(info->tree);
	phase_free(info->phase);
	obstack_free(&info->obst, NULL);
	xfree(info);
}

static void gc_dump_cond(gc_cond *cond, FILE *f)
{
	switch (cond->type) {
	case gct_demand: fprintf(f, "A"); break;
	case gct_ignore: fprintf(f, "0"); break;
	case gct_branch: {
		gc_branch *branch = (gc_branch*)cond;

		fprintf(f, "G(%li,", get_irn_node_nr(branch->gamma));
		gc_dump_cond(branch->rhs, f);
		fprintf(f, ",");
		gc_dump_cond(branch->lhs, f);
		fprintf(f, ")");
		break;
	}
	case gct_union: {
		gc_union *gcu = (gc_union*)cond;
		gc_dump_cond(gcu->lhs, f);
		fprintf(f, " u ");
		gc_dump_cond(gcu->rhs, f);
		break;
	}}
}

static void gc_dump_path(gc_dirs *path, FILE *f)
{
	fprintf(f, "gc_%3li(%3li) = ",
		get_irn_node_nr(path->source),
		get_irn_node_nr(path->target)
	);

	gc_dump_cond(path->cond, f);
}

static void gc_dump(gc_info *info, FILE *f)
{
	plist_element_t *it;

	fprintf(f, "-----------------\n");
	fprintf(f, "Gating conditions\n");
	fprintf(f, "-----------------\n");

	foreach_plist(info->root->map->dirs, it) {
		gc_dump_path(it->data, f);
		fprintf(f, "\n");
	}
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

void peg_to_cfg(ir_graph *irg)
{
	gc_info *info = gc_init(irg);
	gc_free(info);
}
