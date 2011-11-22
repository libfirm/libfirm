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
 * @brief   Arranging VFirm graphs for Firm construction.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include "vf_dstr_arrange.h"
#include "irnode_t.h"
#include "irnodemap.h"
#include "xmalloc.h"
#include "obstack.h"
#include "vf_formula.h"
#include "ircons.h"
#include <stdio.h>
#include <assert.h>

#define VA_SMART_RE_USE    0
//#define VA_DEBUG_ARRANGE   1
//#define VA_DEBUG_CONSTRUCT 1

#define foreach_plist_lazy(list, it) \
	if (list) foreach_plist(list, it)

#define foreach_pmap_new_lazy(map, entry, it) \
	if (map) foreach_pmap_new(map, entry, it)

typedef struct obstack obstack;
typedef struct va_node va_node;

struct va_region {
	int         index;
	va_region  *base;
	ir_node    *gamma;    /* Used to get the >real< branch cond later. */
	vf_cond     cond;     /* This is the full condition. */
	vf_cond     rel_cond; /* The condition relative to the idom. */
	va_region  *pred;
	va_region  *link;
	va_region  *parent;

	/* Putting children next to each other reflects the dependencies more
	 * accurately, than placing them in a row like it is actually done. */
	pmap_new_t *children;

	/* Used for loop fusion and to speed up finding loops. Lazy initialized. */
	pmap_new_t *loops;
};

typedef struct va_edge {
	ir_node *irn;
	int      index;
} va_edge;

typedef struct va_hint {
	va_region *region;
	plist_t   *edges;
	va_node   *copy;
} va_hint;

struct va_node {
	ir_node   *irn;
	int        marker;
	va_region *region;
	va_region *branch_region;
	plist_t   *hints;
	va_hint   *last_hint;
};

struct va_info {
	obstack     obst;
	ir_nodemap  nodemap;
	vc_info    *vci;
	va_region  *root;
	int         counter;
	int         copy_count;
	ir_node    *block;
};

static int va_skip_node(va_info *vai, ir_node *irn)
{
	return vai->block && (get_nodes_block(irn) != vai->block);
}

static void va_plist_lazy_insert(va_info *vai, plist_t **list, void *value)
{
	if (!*list) *list = plist_obstack_new(&vai->obst);
	plist_insert_back(*list, value);
}

static void *va_pmap_new_lazy_get(pmap_new_t *map, void *key)
{
	if (!map) return NULL;
	return pmap_new_get(map, key);
}

static void va_pmap_new_lazy_insert(va_info *vai, pmap_new_t **map, void *key,
                                     void *value)
{
	if (!*map) {
		*map = OALLOC(&vai->obst, pmap_new_t);
		pmap_new_init(*map);
	}
	pmap_new_insert(*map, key, value);
}

/* Create a new child region. */
static va_region *va_region_new_child(va_info *vai, va_region *parent,
                                      vf_cond rel_cond)
{
	va_region *region = OALLOC(&vai->obst, va_region);

	/* True is allowed for roots only. Ensure literals anywhere else. */
	assert(
		(!parent && vf_cond_is_true(rel_cond)) ||
		 vf_cond_is_literal(rel_cond)
	);

	region->parent   = parent;
	region->index    = vai->counter;
	region->base     = region;
	region->link     = NULL;
	region->rel_cond = rel_cond;
	region->pred     = NULL;
	region->gamma    = NULL;
	region->loops    = NULL;
	region->children = NULL;
	vai->counter++;

	/* Create the full condition. */
	if (parent) {
		region->cond = vf_cond_and(
			vc_get_vf_info(vai->vci), parent->cond, rel_cond
		);
	} else {
		region->cond = rel_cond;
	}

	return region;
}

/* Create a new root region. */
static va_region *va_region_new_root(va_info *vai)
{
	return va_region_new_child(vai, NULL, vf_cond_get_true());
}

int va_region_has_loops(va_region *region)
{
	return (region->loops != NULL);
}

static void va_region_add_loop(va_info *vai, va_region *region, ir_node *irn)
{
	ir_node *eta, *cond, *old_irn;

	/* The loop should be valid and not fused already. */
	assert(is_Loop(irn));
	assert(get_Loop_next(irn) == irn);

	/* Obtain the loops condition first. */
	eta = get_Loop_eta(irn);
	assert(is_Eta(eta));

	cond = get_Eta_cond(eta);

	/* Lazily initialize the map. */
	if (!region->loops) {
		region->loops = OALLOC(&vai->obst, pmap_new_t);
		pmap_new_init(region->loops);
	}

	/* Try to look up an existing loop with the same cond. */
	old_irn = pmap_new_get(region->loops, cond);

	if (old_irn) {
		/* If there is one, add us to its list. */
		set_Loop_next(irn, get_Loop_next(old_irn));
		set_Loop_next(old_irn, irn);

#if VA_DEBUG_CONSTRUCT
		printf(
			"Fusing loops %ld and %ld\n",
			get_irn_node_nr(old_irn), get_irn_node_nr(irn)
		);
#endif
	} else {
		/* Otherwise put it in the map. */
		pmap_new_insert(region->loops, cond, irn);
	}
}

#if VA_DEBUG_CONSTRUCT
static void va_region_dump(va_region *region, FILE *f)
{
	printf("%i", region->index);
	if (region->base != region) {
		printf(":%i", region->base->index);
	}
	printf(" (");
	vf_cond_dump(region->cond, f);
	printf(")");
}
#endif

typedef enum va_node_cond {
	va_node_cond_implied,
	va_node_cond_not_implied,
	va_node_cond_not_available
} va_node_cond;

/* Tests if the given condition implies the nodes condition. Starts at the top,
 * where conditions change to find a contradiction as soon as possible. */
static va_node_cond va_cond_implies_node(va_info *vai, vf_cond cond,
                                         ir_node *irn)
{
	vc_rel_cond rel_cond;

	/* Get the relative condition of irn and the parent node. */
	if (!vc_node_get_rel_cond(vai->vci, irn, &rel_cond)) {
		return va_node_cond_not_available;
	}

	/* We have to imply all relative conditions. */
	if (!vf_cond_implies(cond, rel_cond.cond)) {
		return va_node_cond_not_implied;
	}

	/* Recurse if there is a dominator. */
	if (rel_cond.idom) {
		return va_cond_implies_node(vai, cond, rel_cond.idom);
	}

	return va_node_cond_implied;
}

/* Scan the upper regions for the first region that implies the given nodes
 * condition (ie. that could host the node). This is used to determine if the
 * node still belongs to the given region. If not, a copy of the result region
 * can be used to continue placing. Returns NULL if the node should not be
 * placed at all (start block nodes). */
static va_region *va_region_upscan(va_info *vai, va_region *region,
                                   ir_node *irn)
{
	/* Start at the bottom, where contradictions are likely and tests fast. */
	if (region->parent) {
		/* If we found a suitable region below, stop and return it. */
		va_region *result = va_region_upscan(vai, region->parent, irn);
		if (result) return result;
	}

	/* If the region is suitable for the node, return it. */
	if (va_cond_implies_node(vai, region->cond, irn) == va_node_cond_implied) {
		return region;
	}

	/* No suitable region found. */
	return NULL;
}

static va_node *va_init_node(va_info *vai, const ir_node *irn)
{
	va_node *van = OALLOC(&vai->obst, va_node);

	van->irn           = (ir_node*)irn;
	van->hints         = NULL;
	van->region        = NULL;
	van->branch_region = NULL;
	van->marker        = 0;
	van->last_hint     = NULL;

	ir_nodemap_insert(&vai->nodemap, irn, van);

	return van;
}

/* Set the node markers. When finished, they are set to the number of edges
 * that lead to the currently considered node. This is used when iterating
 * the graph later, to ensure that all users have been considered. */
static void va_set_markers_walk(va_info *vai, ir_node *irn)
{
	int      i;
	va_node *van = ir_nodemap_get(&vai->nodemap, irn);
	if (van == NULL) {
		van = va_init_node(vai, irn);
	}
	van->marker++;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		if (va_skip_node(vai, ir_dep)) continue;

		va_set_markers_walk(vai, ir_dep);
	}
}

static void va_set_markers(va_info *vai, ir_node *irn)
{
	inc_irg_visited(va_get_irg(vai));
	va_set_markers_walk(vai, irn);
}

static void va_region_link(va_region *lhs, va_region *rhs)
{
	assert(!lhs->link && !rhs->link);
	lhs->link = rhs;
	rhs->link = lhs;
}

static va_region *va_region_get_child(va_info *vai, va_region *parent,
                                      ir_node *irn, char value)
{
	/* Try to find a region for the given cond node. */
	va_region *child_region = va_pmap_new_lazy_get(parent->children, irn);
	va_region *new_region;

	if (child_region) {
		/* Depending on true/false return that region or the link. */
		if (vf_cond_get_value(child_region->rel_cond) == value) {
			return child_region;
		}

		if (child_region->link) return child_region->link;

		/* If the region was not found, create it and add it as link. */
		new_region = va_region_new_child(vai, parent,
			vf_cond_new(vc_get_vf_info(vai->vci), irn, value)
		);

		va_region_link(child_region, new_region);
		return new_region;
	}

	/* Create the first region for cond. */
	new_region = va_region_new_child(vai, parent,
		vf_cond_new(vc_get_vf_info(vai->vci), irn, value)
	);

	va_pmap_new_lazy_insert(vai, &parent->children, irn, new_region);
	return new_region;
}

static va_region *va_region_ensure_pred(va_info *vai, va_region *region)
{
	if (!region->pred) {
		region->pred = va_region_new_child(
			vai, region->parent, region->rel_cond
		);
		region->pred->base = region->base;

#if VA_DEBUG_CONSTRUCT
		printf("  Added predecessor ");
		va_region_dump(region->pred, stdout);
		printf(" to ");
		va_region_dump(region, stdout);
		printf(".\n");
#endif
	}

	return region->pred;
}

typedef enum va_node_state {
	va_node_state_ignored,  /* Node without condition. Should be ignored. */
	va_node_state_analyzed, /* Node has been analyzed. All regions known. */
	va_node_state_partial   /* Node has been partially analyzed. */
} va_node_state;

#if VA_SMART_RE_USE
static void va_place_phis(va_info *vai, va_region *old_region,
                          va_region *new_region, ir_node *irn, plist_t *edges)
{
	plist_element_t *it;

	if (old_region->base == new_region->base) return;

	ir_node *block   = get_nodes_block(irn);
	ir_mode *mode    = get_irn_mode(irn);
	ir_node *result  = irn;
	ir_node *unknown = new_r_Unknown(get_irn_irg(result), mode);
	va_node *van;

	/* Go up to a region we can reach from old_region. */
	while (old_region->base != new_region->base) {
		ir_node   *cond  = va_region_get_cond(new_region);
		int        value = vf_cond_get_value(new_region->cond);
		va_region *branch_region = new_region;

		old_region = old_region->parent;
		new_region = new_region->parent;

		result = new_r_Gamma(
			block, cond,
			value ? unknown : result,
			value ? result : unknown,
			mode
		);

		van = nodemap_get_or_set_irn_data(&vai->nodemap, result);
		van->region = new_region;
		van->branch_region = branch_region;
	}

	/* Redirect current edges. */
	foreach_plist(edges, it) {
		va_edge *edge = plist_element_get_value(it);
		set_irn_n(edge->irn, edge->index, result);
	}
}
#endif

static va_node_state va_node_visit(va_info *vai, va_node *van,
                                   va_region *req_region, va_edge edge)
{
	plist_element_t *it;
	va_node_state    state;
	va_region       *new_region;
	va_edge         *new_edge   = NULL;
	va_hint         *found_hint = NULL;

	/* Ignore nodes in the start block. */
	if (va_skip_node(vai, van->irn)) {
		return va_node_state_ignored;
	}

	if (van->marker <= 0) return va_node_state_analyzed;
	van->marker--;

	/* The state after analyzing the node. */
	if (van->marker <= 0) state = va_node_state_analyzed;
	else state = va_node_state_partial;

	/* Shortcut for accesses from the same region (nothing to add). */
	if (van->last_hint && (req_region == van->last_hint->region)) {
		found_hint = van->last_hint;
	} else {
		/* Determine the parent of the request region that is suitable and add
		 * it to the list of regions for the node, if not already present. */
		new_region = va_region_upscan(vai, req_region, van->irn);
		assert(new_region); /* Something is wrong if we get no cond here. */

		/* If we did go up, we have to use the predecessor region now. */
		if (new_region != req_region) {
			new_region = va_region_ensure_pred(vai, new_region);
		}

		/* Try to rule out, if we have to add a new region hint. */
		foreach_plist_lazy(van->hints, it) {
			va_hint *hint = plist_element_get_value(it);

			/**
			 * Always place the node in the newest region with the given cond.
			 * Allow use edges to leave the regions nesting level. This avoids
			 * duplication but may require additional phi nodes. Sample:
			 *
			 * if (a) { c += b; }
			 * c++;
			 * if (a) { c += b; }
			 *
			 * The node b is only evaluated if a is true. However we are unable
			 * to combine the two branches here. Smart re-use can only evaluate
			 * b once, but a phi node is required after the first branch to
			 * select a value for the addition.
			 *
			 * If smart re-use is disabled, a use edge is not allowed to leave
			 * and enter a region. So an edge from the second addition to the
			 * first and only b is not allowed. This should avoid additional
			 * phi nodes.
			 */

#if VA_SMART_RE_USE
			if (vf_cond_equals(hint->region->cond, new_region->cond)) {
#else
			/* The base index will stay the same for all regions in one branch. */
			if (hint->region->base == new_region->base) {
#endif
				/* Use the newer region for the hint. */

				/* FIXME: shouldn't this be placed after the loop, so that it
				 * also runs when searching is cut short? */

				if (new_region->index > hint->region->index) {
#if VA_SMART_RE_USE
					va_place_phis(vai, hint->region, new_region, van->irn, hint->edges);
#endif
					hint->region = new_region;
#if VA_SMART_RE_USE
				} else {
					va_place_phis(vai, new_region, hint->region, van->irn, hint->edges);
#endif
				}

				found_hint = hint;
				break;
			}
		}

		/* Create a new hint? */
		if (!found_hint) {
			found_hint = OALLOC(&vai->obst, va_hint);
			found_hint->region = new_region;
			found_hint->edges  = NULL;
			found_hint->copy   = NULL;

			/* Nodes clones will never need this list. */
			va_plist_lazy_insert(vai, &van->hints, found_hint);
		}
	}

	assert(found_hint);

	/* Add the current edge to the region hint (if given), if there are more
	 * than one region hints. This means we have to duplicate the node. */
#if VA_SMART_RE_USE
	if (edge.irn) {
#else
	if (edge.irn && (plist_count(van->hints) > 1)) {
#endif
		new_edge = OALLOC(&vai->obst, va_edge);
		new_edge->irn   = edge.irn;
		new_edge->index = edge.index;

		/* Only needed for nodes that are cloned. */
		va_plist_lazy_insert(vai, &found_hint->edges, new_edge);
	}

	van->last_hint = found_hint;
	return state;
}

/* Just to shorten construction. */
static va_edge va_edge_make(ir_node *irn, int index)
{
	va_edge edge;
	edge.irn   = irn;
	edge.index = index;
	return edge;
}

/* Increase markers on all dependency nodes. This is needed to repair markers
 * after cloning a node and prevents placement of the dependencies, before the
 * cloned edges have been processed. */
static void va_node_inc_dep_markers(va_info *vai, ir_node *irn)
{
	int i;
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		va_node *va_dep;

		if (va_skip_node(vai, ir_dep)) continue;
		va_dep = ir_nodemap_get(&vai->nodemap, ir_dep);
		if (va_dep == NULL) {
			va_dep = va_init_node(vai, ir_dep);
		}
		va_dep->marker++;
	}
}

static void va_region_hint(va_info *vai, va_region *region, va_edge edge);

/* Use the single hint of the given node, to place it. This will continue
 * searching for hints from the placed node. */
static void va_region_place_single(va_info *vai, va_node *van,
                                   va_region *region)
{
	ir_node *irn = van->irn;

	/* Assign the node to the region. */
	van->region    = region;
	van->last_hint = NULL;

	if (is_Loop(irn)) {
		va_region_add_loop(vai, region, irn);
		/* Loops force a region change, so that the loop blocks can later be
		 * inserted between the blocks of both regions. */
		region = va_region_ensure_pred(vai, region);
	}

	/* If the node is a gamma node, the request region may change. */
	if (is_Gamma(irn)) {
		ir_node *ir_cond = get_Gamma_cond(irn);

		/* Create sub-regions for the gamma. */
		va_region *rg_true  = va_region_get_child(vai, region, ir_cond, 1);
		va_region *rg_false = va_region_get_child(vai, region, ir_cond, 0);
		va_region *pred;

#if VA_DEBUG_CONSTRUCT
		printf("  Adds regions ");
		va_region_dump(rg_true, stdout);
		printf(" and ");
		va_region_dump(rg_false, stdout);
		printf(".\n");
#endif

		pred = va_region_ensure_pred(vai, region);

		/* Storing a reference to at least one gamma for the branch makes it
		 * possible to access the correct condition later, even if it has been
		 * cloned. Note that cloning may appear after we added the region hint,
		 * so we can't decide this yet. */
		rg_true->gamma     = irn;
		rg_false->gamma    = irn;
		van->branch_region = rg_true; /* Also store the reverse reference. */

		/* Pull the dependencies to the new regions. Move the condition to
		 * the predecessor region, to ensure its there before the branch.
		 * Can we avoid the hard-coded edge indices here? */
		va_region_hint(vai, pred,     va_edge_make(irn, 0));
		va_region_hint(vai, rg_true,  va_edge_make(irn, 2));
		va_region_hint(vai, rg_false, va_edge_make(irn, 1));
	} else {
		int i;
		int arity = get_irn_arity(irn);

		/* For usual nodes just pull deps to the current region. */
		for (i = 0; i < arity; i++) {
			va_region_hint(vai, region, va_edge_make(irn, i));
		}
	}
}

/* Resolve multiple hints for the given node and place it in one or more
 * regions. This will continue searching for hints from the placed node. */
static void va_region_place_multiple(va_info *vai, va_node *van)
{
	plist_element_t *it, *it_edge;

	/* All users have been discovered and we have multiple hints on where
	 * the node should be placed. Furthermore we have a list of use edges
	 * for the additional region hints that indicate that placement. */

	foreach_plist(van->hints, it) {
		ir_node *ir_copy;
		va_node *va_copy;
		va_hint *hint = plist_element_get_value(it);

		/* For the first hint use irn. For others create a copy. */
		if (it == plist_first(van->hints)) {
			va_copy = van;
		} else {
			ir_copy = vl_exact_copy(van->irn);
			va_copy = ir_nodemap_get(&vai->nodemap, ir_copy);
			if (va_copy == NULL) {
				va_copy = va_init_node(vai, ir_copy);
			}
			va_node_inc_dep_markers(vai, ir_copy);
			vai->copy_count++;

#if VA_DEBUG_CONSTRUCT
			printf("  Made copy %ld for region ", get_irn_node_nr(ir_copy));
			va_region_dump(hint->region, stdout);
			printf(".\n");
#endif

			/* Redirect all the edges that produced this hint to the copy. */
			foreach_plist(hint->edges, it_edge) {
				va_edge *edge = plist_element_get_value(it_edge);
				set_irn_n(edge->irn, edge->index, ir_copy);
			}
		}

		/* Just store the copy for now. */
		hint->copy = va_copy;
	}

	/* Now that all copies are created, we can try to place them. This has to
	 * be done after copying, because there are new edges and the markers have
	 * to be updated first AND for all nodes. */

	foreach_plist(van->hints, it) {
		va_hint *hint = plist_element_get_value(it);
		va_region_place_single(vai, hint->copy, hint->region);
	}
}

/* Add the given region as hint to the node at the end of the given edge. This
 * will assign a region to the node if enough hints have been found. */
static void va_region_hint(va_info *vai, va_region *region, va_edge edge)
{
#if VA_DEBUG_CONSTRUCT
	plist_element_t *it, *it_edge;
#endif
	va_node_state state;
	va_node *van;

	/* Get the edges target edge and information. */
	ir_node *irn = edge.irn ? get_irn_n(edge.irn, edge.index) :
	                          vc_get_root(vai->vci);
	if (va_skip_node(vai, irn)) return;

	/* Visit the node and add the region for this request. */
	van   = ir_nodemap_get(&vai->nodemap, irn);
	if (van == NULL) {
		van = va_init_node(vai, irn);
	}
	state = va_node_visit(vai, van, region, edge);
	if (state != va_node_state_analyzed) return;

	assert((plist_count(van->hints) > 0) && "Node placed twice?");

#if VA_DEBUG_CONSTRUCT
	if (plist_count(van->hints) > 1) {
		printf("Node %ld hints:\n", get_irn_node_nr(irn));
	} else {
		printf("Node %ld hint: ", get_irn_node_nr(irn));
	}

	foreach_plist(van->hints, it) {
		va_hint *hint = plist_element_get_value(it);

		if (plist_count(van->hints) > 1) printf("  ");
		va_region_dump(hint->region, stdout);

		if (hint->edges) {
			printf(" for edge(s) ");
			foreach_plist(hint->edges, it_edge) {
				va_edge *edge = plist_element_get_value(it_edge);
				if (it_edge != plist_first(hint->edges)) printf(", ");
				printf("%ld:%i", get_irn_node_nr(edge->irn), edge->index);
			}
		}

		printf("\n");
	}
#endif

	/* Now place the node and pull its dependencies. If there are multiple
	 * region hints, things become complicated, so this was split up here. */
	if (plist_count(van->hints) == 1) {
		va_hint *hint;

		/* Get the only region hint. */
		assert(plist_count(van->hints) == 1);
		hint = plist_element_get_value(plist_first(van->hints));

		va_region_place_single(vai, van, hint->region);
	} else {
		va_region_place_multiple(vai, van);
	}

	/* Cleanup placement hints. */
	plist_free(van->hints);
	van->hints     = NULL;
	van->last_hint = NULL;
}

va_info *va_init_root(ir_node *root, int keep_block)
{
	va_info  *vai = XMALLOC(va_info);
	ir_graph *irg = get_irn_irg(root);

	obstack_init(&vai->obst);
	vai->vci = vc_init_root(root, keep_block);

	ir_nodemap_init(&vai->nodemap, irg);

	vai->counter    = 0;
	vai->copy_count = 0;
	vai->root       = va_region_new_root(vai);
	vai->block      = keep_block ? get_nodes_block(root) : NULL;

#if VA_DEBUG_CONSTRUCT || VA_DEBUG_ARRANGE
	printf("+------------------------------------------------+\n");
	printf("| Arrange                                        |\n");
	printf("+------------------------------------------------+\n");
#endif

	va_set_markers(vai, root);
	va_region_hint(vai, vai->root, va_edge_make(NULL, -1));

#if VA_DEBUG_ARRANGE
	va_dump(vai, stdout);
#endif

#if VA_DEBUG_CONSTRUCT || VA_DEBUG_ARRANGE
	printf("%i node(s) copied.\n", vai->copy_count);
#endif

	return vai;
}

static void va_free_region(va_region *region)
{
	pmap_new_entry_t    entry;
	pmap_new_iterator_t it;

	foreach_pmap_new_lazy(region->children, entry, it) {
		va_free_region(entry.value);
	}

	if (region->loops)    pmap_new_destroy(region->loops);
	if (region->children) pmap_new_destroy(region->children);
}

void va_free(va_info *vai)
{
	va_free_region(vai->root);
	vc_free(vai->vci);

	ir_nodemap_destroy(&vai->nodemap);
	obstack_free(&vai->obst, NULL);
	xfree(vai);
}

/* Collect all nodes in the given region. This makes dumps horribly slow, but
 * they are only for debugging anyway. */
static void va_dump_collect(va_info *vai, ir_node *irn, va_region *region,
                            plist_t *list)
{
	va_node *van;
	int      i;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	van = ir_nodemap_get(&vai->nodemap, irn);
	if (van && (van->region == region)) {
		plist_insert_back(list, irn);
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		va_dump_collect(vai, ir_dep, region, list);
	}
}

static void va_dump_indent(int indent, FILE *f)
{
	int i;
	for (i = 0; i < indent; i++) fprintf(f, "  ");
}

static void va_dump_child(va_info *vai, va_region *region, int indent, FILE *f);

static void va_dump_lane(va_info *vai, va_region *region, int indent, FILE *f)
{
	pmap_new_entry_t entry;
	pmap_new_iterator_t it;
	plist_t *nodes = plist_new();

	/* Dump predecessor regions first. */
	if (region->pred) {
		va_dump_lane(vai, region->pred, indent, f);
	}

	/* Dump the child regions. */
	foreach_pmap_new_lazy(region->children, entry, it) {
		va_dump_child(vai, entry.value, indent, f);
	}

	/* Collect nodes in the region. */
	inc_irg_visited(va_get_irg(vai));
	va_dump_collect(vai, va_get_root(vai), region, nodes);

	/* Dump nodes. */
	va_dump_indent(indent, f);

	if (plist_count(nodes) > 0) {
		plist_element_t *it_node;
		foreach_plist(nodes, it_node) {
			ir_node *irn = plist_element_get_value(it_node);
			if (it_node != plist_first(nodes)) fprintf(f, ", ");
			fprintf(f, "%ld", get_irn_node_nr(irn));
		}
	} else {
		fprintf(f, "// empty");
	}

	fprintf(f, "\n");
	plist_free(nodes);
}

static void va_dump_child(va_info *vai, va_region *region, int indent, FILE *f)
{
	int value = vf_cond_get_value(region->rel_cond);

	va_region *rg_true  = value ? region : region->link;
	va_region *rg_false = value ? region->link : region;
	ir_node   *cond     = get_Gamma_cond(region->gamma);

	/* Dump the if-statement. */
	va_dump_indent(indent, f);
	fprintf(f, "if (");
	if (!value) fprintf(f, "!");
	fprintf(f, "%ld) {", get_irn_node_nr(cond));

	if (cond != vf_cond_get_irn(region->rel_cond)) {
		fprintf(f, " // =");
		vf_cond_dump(region->rel_cond, f);
	}
	fprintf(f, "\n");

	if (rg_true) {
		va_dump_lane(vai, rg_true, indent + 1, f);

		if (rg_false) {
			va_dump_indent(indent, f);
			fprintf(f, "} else {\n");
		}
	}

	if (rg_false) {
		va_dump_lane(vai, rg_false, indent + 1, f);
	}

	va_dump_indent(indent, f);
	fprintf(f, "}\n");
}

void va_dump(va_info *vai, FILE *f)
{
	fprintf(f, "graph {\n");
	va_dump_lane(vai, vai->root, 1, f);
	fprintf(f, "}\n");
}

va_region *va_node_get_region(va_info *vai, ir_node *irn)
{
	va_node *van = ir_nodemap_get(&vai->nodemap, irn);
	return van ? van->region : NULL;
}

va_region *va_region_get_parent(va_region *region)
{
	return region->parent;
}

ir_node *va_region_get_cond(va_region *region)
{
	if (!region->parent) return NULL;

	/* Do not pull this out of rel_cond. That wouldn't handle cloning. */
	assert(region->base && region->base->gamma);
	return get_Gamma_cond(region->base->gamma);
}

int va_region_get_value(va_region *region)
{
	if (!region->parent) return 0;
	return vf_cond_get_value(region->rel_cond);
}

va_region *va_region_get_link(va_region *region)
{
	return region->link;
}

va_region *va_region_get_pred(va_region *region)
{
	return region->pred;
}

void va_region_child_it_init(va_region_child_it *it, va_region *region)
{
	it->has_map = (region->children != NULL);
	if (it->has_map) {
		pmap_new_iterator_init(&it->it, region->children);
	}
}

va_region *va_region_child_it_next(va_region_child_it *it)
{
	if (it->has_map) {
		pmap_new_entry_t entry = pmap_new_iterator_next(&it->it);
		if (pmap_new_is_null(entry)) return NULL;
		return entry.value;
	}

	return NULL;
}

int va_region_get_child_count(va_region *region)
{
	if (!region->children) return 0;
	return pmap_new_size(region->children);
}

ir_node *va_get_root(va_info *vai)
{
	return vc_get_root(vai->vci);
}

va_region *va_get_root_region(va_info *vai)
{
	return vai->root;
}

vc_info *va_get_vc_info(va_info *vai)
{
	return vai->vci;
}

ir_graph *va_get_irg(va_info *vai)
{
	return get_irn_irg(vai->block);
}

va_region *va_gamma_get_branch_region(va_info *vai, ir_node *gamma)
{
	va_node *van = ir_nodemap_get(&vai->nodemap, gamma);
	if (!van) return NULL;
	return van->branch_region;
}
