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
#include "irphase_t.h"
#include "plist.h"
#include "peg_dom_t.h"
#include "peg_loop_t.h"
#include "obstack.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irdump.h"
#include "irgmod.h"

#define LOG_DIRS_COMBINE   1
#define LOG_DOMINATOR_TREE 1
#define LOG_LOOP_ANALYSIS  1
#define LOG_GATING_CONDS   1

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
	gct_union,  /* a u b */
	gct_repeat
} gc_type;

typedef struct gc_cond {
	gc_type type;
} gc_cond;

/* A pointer to a struct is guaranteed to also point to the first member. */
typedef struct gc_branch {
	gc_cond  base;
	ir_node *irn;
	gc_cond *lhs;
	gc_cond *rhs;
} gc_branch;

typedef struct gc_repeat {
	gc_cond  base;
	ir_node *irn;
	gc_cond *cond;
} gc_repeat;

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
	pd_tree  *pdt;
	pl_info  *pli;
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

		/* Compare the irn and both conditions. */
		if (!(lhs_branch->irn == rhs_branch->irn)) return 0;
		if (!gc_cond_equals(lhs_branch->lhs, rhs_branch->lhs)) return 0;
		if (!gc_cond_equals(lhs_branch->rhs, rhs_branch->rhs)) return 0;
		return 1;
	}
	case gct_repeat: {
		gc_repeat *lhs_repeat = (gc_repeat*)lhs;
		gc_repeat *rhs_repeat = (gc_repeat*)rhs;

		/* Compare the irn and both conditions. */
		if (!(lhs_repeat->irn == rhs_repeat->irn)) return 0;
		if (!gc_cond_equals(lhs_repeat->cond, rhs_repeat->cond)) return 0;
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

static gc_cond *gc_new_branch(gc_info *gci, gc_cond *lhs,
                              gc_cond *rhs, ir_node *irn)
{
	/* G(g, a, a) = a */
	if (gc_cond_equals(lhs, rhs)) return lhs;

	gc_branch *gcb = OALLOC(&gci->obst, gc_branch);
	gcb->base.type = gct_branch;
	gcb->lhs = lhs;
	gcb->rhs = rhs;
	gcb->irn = irn;
	return (gc_cond*)gcb;
}

static gc_cond *gc_new_repeat(gc_info *gci, gc_cond *cond, ir_node *irn)
{
	gc_repeat *gcr = OALLOC(&gci->obst, gc_repeat);
	gcr->base.type = gct_repeat;
	gcr->cond = cond;
	gcr->irn  = irn;
	return (gc_cond*)gcr;
}

/* This is a complex beast. */
static gc_cond *gc_new_union(gc_info *gci, gc_cond *lhs, gc_cond *rhs)
{
	gc_cond *cur, *new_cur;

	/* Do not allow a rhs union in a union (left associative).
	 * Instead add all entries in the union separately. */
	if (rhs->type == gct_union) {
		gc_cond *it, *rhs_entry;

		cur = lhs;
		foreach_gc_union(rhs, it, rhs_entry) {
			cur = gc_new_union(gci, cur, rhs_entry);
		}

		return cur;
	}

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

	/* G(n, a, b) u G(n, c, d) = G(n, a u c, b u d) */
	if ((cur->type == gct_branch) && (rhs->type == gct_branch)) {
		gc_branch *cur_branch = (gc_branch*)cur;
		gc_branch *rhs_branch = (gc_branch*)rhs;

		if (cur_branch->irn == rhs_branch->irn) {
			/* Optimization can be applied. */
			new_cur = gc_new_branch(gci,
				gc_new_union(gci, cur_branch->lhs, rhs_branch->lhs),
				gc_new_union(gci, cur_branch->rhs, rhs_branch->rhs),
				cur_branch->irn
			);
		}
	} else if ((cur->type == gct_repeat) && (rhs->type == gct_repeat)) {
		gc_repeat *cur_repeat = (gc_repeat*)cur;
		gc_repeat *rhs_repeat = (gc_repeat*)rhs;

		if (cur_repeat->irn == rhs_repeat->irn) {
			new_cur = gc_new_repeat(gci,
				gc_new_union(gci, cur_repeat->cond, rhs_repeat->cond),
				cur_repeat->irn
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
			gc_union *gcu = OALLOC(&gci->obst, gc_union);
			gcu->base.type = gct_union;
			gcu->lhs = lhs;
			gcu->rhs = rhs;
			return (gc_cond*)gcu;
		}
	} else {
		/* Tuple on the left. Replace the rhs or recurse. */
		gc_union *lhs_gcu = (gc_union*)lhs;
		gc_union *gcu = OALLOC(&gci->obst, gc_union);
		gcu->base.type = gct_union;

		if (new_cur) {
			gcu->lhs = lhs_gcu->lhs;
			gcu->rhs = new_cur;
			return (gc_cond*)gcu;
		} else {
			gcu->lhs = gc_new_union(gci, lhs_gcu->lhs, rhs);
			gcu->rhs = lhs_gcu->rhs;
			return (gc_cond*)gcu;
		}
	}
}

static gc_cond *gc_new_concat(gc_info *gci, gc_cond *lhs, gc_cond *rhs)
{
	/* Swap A or 0 to the left. */
	if ((rhs->type == gct_demand) || (rhs->type == gct_ignore)) {
		gc_cond *tmp = lhs; lhs = rhs; rhs = tmp;
	}

	switch (lhs->type) {
	case gct_demand: return rhs; /* A.c = c */
	case gct_ignore: return lhs; /* 0.c = 0 */
	case gct_branch: {
		/* G(n, a, b).c = G(n, a.c, b.c) */
		gc_branch *lhs_branch = (gc_branch*)lhs;
		return gc_new_branch(gci,
			gc_new_concat(gci, lhs_branch->lhs, rhs),
			gc_new_concat(gci, lhs_branch->rhs, rhs),
			lhs_branch->irn
		);
	}
	case gct_repeat: {
		/* R(n, a).b = R(n, a.b) */
		gc_repeat *lhs_repeat = (gc_repeat*)lhs;
		return gc_new_repeat(gci,
			gc_new_concat(gci, lhs_repeat->cond, rhs),
			lhs_repeat->irn
		);
	}
	case gct_union: {
		/* (a u b).c = a.c u b.c */
		gc_union *lhs_union = (gc_union*)lhs;
		return gc_new_union(gci,
			gc_new_concat(gci, lhs_union->lhs, rhs),
			gc_new_concat(gci, lhs_union->lhs, rhs)
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
static gc_dirs *gc_new_edge_dirs(gc_info *gci, ir_node *source, ir_node *target)
{
	gc_dirs *dirs = OALLOC(&gci->obst, gc_dirs);
	dirs->target = target;
	dirs->source = source;

	if (is_Gamma(source)) {
		/* Determine the edge. */
		int edge = 2;
		if      (get_Gamma_true(source)  == target) edge = 0;
		else if (get_Gamma_false(source) == target) edge = 1;

		if (edge < 2) {
			dirs->cond = gc_new_branch(gci,
				(edge == 0) ? gc_new_demand() : gc_new_ignore(),
				(edge == 0) ? gc_new_ignore() : gc_new_demand(),
				source
			);
		} else {
			/* Always demand condition edges. */
			dirs->cond = gc_new_demand();
		}
	} else if (is_EtaA(source)) {
		int source_depth = pl_get_depth(gci->pli, source);
		int target_depth = pl_get_depth(gci->pli, target);

		int edge = 2;
		if      (get_EtaA_result(source) == target) edge = 0;
		else if (get_EtaA_repeat(source) == target) edge = 1;

		if (edge < 2) {
			/* Evaluate the result value or next values, depending on the
			 * loop condition. This can be represented by a branch cond. */
			dirs->cond = gc_new_branch(gci,
				(edge == 0) ? gc_new_demand() : gc_new_ignore(),
				(edge == 0) ? gc_new_ignore() : gc_new_demand(),
				source
			);
		} else {
			/* Thetas and conditions are always evaluated. */
			dirs->cond = gc_new_demand();
		}

		/* Only create a repeat edge if the target is loop-nested. The header
		 * and repeat tuples are forced to be nested. The force tuple isn't. */
		if (target_depth > source_depth) {
			dirs->cond = gc_new_repeat(gci, dirs->cond, source);
		}
	} else {
		/* Always demand normal edges. */
		dirs->cond = gc_new_demand();
	}

	return dirs;
}

static void gc_dump_cond(gc_cond *cond, FILE *f);

static gc_dirs *gc_dirs_concat(gc_info *gci, gc_dirs *lhs, gc_dirs *rhs)
{
	gc_dirs *dirs = OALLOC(&gci->obst, gc_dirs);
	assert(lhs->target == rhs->source);

#ifdef LOG_DIRS_COMBINE
	printf("%3li -> %3li -> %3li: ",
		get_irn_node_nr(lhs->source),
		get_irn_node_nr(lhs->target),
		get_irn_node_nr(rhs->target)
	);
	printf("concat("); gc_dump_cond(lhs->cond, stdout);
	printf(",");       gc_dump_cond(rhs->cond, stdout);
#endif

	dirs->cond   = gc_new_concat(gci, lhs->cond, rhs->cond);
	dirs->source = lhs->source;
	dirs->target = rhs->target;

#ifdef LOG_DIRS_COMBINE
	printf(") = "); gc_dump_cond(dirs->cond, stdout); printf("\n");
#endif

	return dirs;
}

static gc_dirs *gc_dirs_union(gc_info *gci, gc_dirs *lhs, gc_dirs *rhs)
{
	gc_dirs *dirs = OALLOC(&gci->obst, gc_dirs);
	assert((lhs->target == rhs->target) && (lhs->source == rhs->source));

#ifdef LOG_DIRS_COMBINE
	printf("%3li -> %3li:        ",
		get_irn_node_nr(lhs->source),
		get_irn_node_nr(lhs->target)
	);
	printf("union(");
	gc_dump_cond(lhs->cond, stdout); printf(",");
	gc_dump_cond(rhs->cond, stdout); printf(") = ");
#endif

	dirs->cond   = gc_new_union(gci, lhs->cond, rhs->cond);
	dirs->source = lhs->source;
	dirs->target = rhs->target;

#ifdef LOG_DIRS_COMBINE
	gc_dump_cond(dirs->cond, stdout); printf("\n");
#endif

	return dirs;
}

static void gc_map_merge(gc_info *gci, gc_map *map, gc_dirs *dirs)
{
	plist_element_t *it;
	foreach_plist(map->dirs, it) {
		gc_dirs *it_dirs = it->data;
		assert(it_dirs->source == dirs->source);

		/* Combine dirs with the same target. */
		if (it_dirs->target == dirs->target) {
			/* it_dirs is only used by us, so we can be destructive. */
			it->data = gc_dirs_union(gci, it_dirs, dirs);
			return;
		}
	}

	/* Just insert the path if merging doesn't work. */
	plist_insert_back(map->dirs, dirs);
}

/******************************************************************************
 * Populate maps with directions.                                             *
 ******************************************************************************/

static void gc_compute_cross_dirs(gc_info *gci, gc_node *gcn, gc_node *gc_lhs)
{
	plist_element_t *it_lhs, *it_rhs;
	pd_iter it_mid;

	/* Skip children with known cross dirs. */
	if (gc_lhs->cross_map) return;

	gc_lhs->cross_map = OALLOC(&gci->obst, gc_map);
	gc_lhs->cross_map->dirs = plist_obstack_new(&gci->obst);

	/* Scan conventional dirs in the child subgraph. */
	foreach_plist(gc_lhs->map->dirs, it_lhs) {
		ir_node *ir_rhs;
		gc_dirs *lhs_dirs = it_lhs->data;

		/* Search for dirs that cross into another childs subgraph. */
		foreach_pd_child(gci->pdt, gcn->irn, it_mid, ir_rhs) {
			gc_node *gc_rhs;

			/* Skip boring dirs. */
			if (lhs_dirs->target != ir_rhs) continue;

			gc_rhs = phase_get_irn_data(gci->phase, ir_rhs);
			assert(gc_rhs);

			/* Compute their cross dirs first. */
			gc_compute_cross_dirs(gci, gcn, gc_rhs);
			assert(gc_rhs->cross_map);

			/* Form our own cross dirs by concatenation.
			 * First combine our cross paths with theirs. */
			foreach_plist(gc_rhs->cross_map->dirs, it_rhs) {
				gc_dirs *rhs_path = it_rhs->data;
				gc_dirs *cross_dirs;

				cross_dirs = gc_dirs_concat(gci, lhs_dirs, rhs_path);
				gc_map_merge(gci, gc_lhs->cross_map, cross_dirs);
			}

			/* Then combine our cross paths with their normal paths. */
			foreach_plist(gc_rhs->map->dirs, it_rhs) {
				gc_dirs *rhs_dirs = it_rhs->data;
				gc_dirs *cross_dirs;

				cross_dirs = gc_dirs_concat(gci, lhs_dirs, rhs_dirs);
				gc_map_merge(gci, gc_lhs->cross_map, cross_dirs);
			}
		}
	}
}

static void gc_compute_dirs(gc_info *gci, gc_node *gcn)
{
	int i;

	plist_element_t *it;
	pd_iter  it_child;
	ir_node *ir_child;

	/* Recurse first. */
	foreach_pd_child(gci->pdt, gcn->irn, it_child, ir_child) {
		gc_node *gc_child = phase_get_or_set_irn_data(gci->phase, ir_child);
		gc_child->irn = ir_child;
		gc_compute_dirs(gci, gc_child);
	}

	/* Compute crossing dirs for the dominator children. */
	foreach_pd_child(gci->pdt, gcn->irn, it_child, ir_child) {
		gc_node *gc_child = phase_get_irn_data(gci->phase, ir_child);
		gc_compute_cross_dirs(gci, gcn, gc_child);
	}

	/* Now create dirs edge by edge. */
	for (i = 0; i < get_irn_arity(gcn->irn); i++) {
		ir_node *ir_dep = get_irn_n(gcn->irn, i);
		gc_dirs *edge   = gc_new_edge_dirs(gci, gcn->irn, ir_dep);

		/* Add the trivial gating path of length 1. */
		gc_map_merge(gci, gcn->map, edge);

		/* For dominated target nodes add the combined dirs. */
		if (pd_dominates(gci->pdt, gcn->irn, ir_dep)) {
			gc_node *gc_dep = phase_get_irn_data(gci->phase, ir_dep);
			assert(gc_dep);

			/* First all usual gating paths in gc_dep. */
			foreach_plist(gc_dep->map->dirs, it) {
				gc_dirs *dirs = gc_dirs_concat(gci, edge, it->data);
				gc_map_merge(gci, gcn->map, dirs);
			}

			/* Then all the crossing paths. */
			foreach_plist(gc_dep->cross_map->dirs, it) {
				gc_dirs *dirs = gc_dirs_concat(gci, edge, it->data);
				gc_map_merge(gci, gcn->map, dirs);
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

static void gc_dump(gc_info *gci, FILE *f);

static gc_info *gc_init(ir_graph *irg, pd_tree *pdt, pl_info *pli)
{
	gc_info *info = XMALLOC(gc_info);
	ir_node *ret;
	assert((pd_get_irg(pdt) == irg) && (pl_get_irg(pli) == irg));

	obstack_init(&info->obst);

	info->pdt = pdt;
	info->pli = pli;
	ret = pd_get_root(info->pdt);
	assert(is_Return(ret));

	info->phase = new_phase(irg, gc_init_node);
	phase_set_private(info->phase, info);

#ifdef LOG_DIRS_COMBINE
	printf("--------------------\n");
	printf("Computing directions\n");
	printf("--------------------\n");
#endif

	/* Walk through the tree to compute directions. */
	info->root = phase_get_or_set_irn_data(info->phase, ret);
	info->root->irn = ret;
	gc_compute_dirs(info, info->root);

	return info;
}

static void gc_free(gc_info *gci)
{
	phase_free(gci->phase);
	obstack_free(&gci->obst, NULL);
	xfree(gci);
}

static void gc_dump_cond(gc_cond *cond, FILE *f)
{
	switch (cond->type) {
	case gct_demand: fprintf(f, "A"); break;
	case gct_ignore: fprintf(f, "0"); break;
	case gct_branch: {
		gc_branch *branch = (gc_branch*)cond;

		fprintf(f, "G(%li,", get_irn_node_nr(branch->irn));
		gc_dump_cond(branch->lhs, f);
		fprintf(f, ",");
		gc_dump_cond(branch->rhs, f);
		fprintf(f, ")");
		break;
	}
	case gct_repeat: {
		gc_repeat *repeat = (gc_repeat*)cond;

		fprintf(f, "R(%li,", get_irn_node_nr(repeat->irn));
		gc_dump_cond(repeat->cond, f);
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

static void gc_dump(gc_info *gci, FILE *f)
{
	plist_element_t *it;
	foreach_plist(gci->root->map->dirs, it) {
		gc_dump_path(it->data, f);
		fprintf(f, "\n");
	}
}

/******************************************************************************
 * Acyclic PEG creation.                                                      *
 ******************************************************************************/

static void peg_to_acpeg(ir_graph *irg, pl_info *pli)
{
	struct obstack obst;
	pl_iter  it, it_theta, it_border;
	ir_node *eta, *theta, *irn;

	obstack_init(&obst);
	assert(pl_get_irg(pli) == irg);

	/* Process all etas in the graph. */
	foreach_pl_irg_eta(pli, it, eta) {
		/* Create the tuples for thetas and next values. */
		ir_node  *header, *repeat, *force, *eta_a;
		ir_node  *block        = get_nodes_block(eta);
		int       theta_count  = pl_get_theta_count(pli, eta);
		int       border_count = pl_get_border_count(pli, eta);
		int       depth        = pl_get_depth(pli, eta);
		ir_node **header_ins   = OALLOCN(&obst, ir_node*, theta_count);
		ir_node **repeat_ins   = OALLOCN(&obst, ir_node*, theta_count);
		ir_node **force_ins    = OALLOCN(&obst, ir_node*, border_count);
		ir_node  *value        = get_Eta_value(eta);
		ir_node  *cond         = get_Eta_cond(eta);
		ir_mode  *mode         = get_irn_mode(eta);
		int       index;

		/* Collect the nodes. */
		index = 0;
		foreach_pl_theta(pli, eta, it_theta, theta) {
			assert(pl_get_depth(pli, theta) == (depth + 1));
			header_ins[index] = theta;
			repeat_ins[index] = get_Theta_next(theta);
			index++;
		}

		index = 0;
		foreach_pl_border(pli, eta, it_border, irn) {
			assert(pl_get_depth(pli, irn) <= depth);
			force_ins[index] = irn;
			index++;
		}

		header = new_r_Tuple(block, theta_count,  header_ins);
		repeat = new_r_Tuple(block, theta_count,  repeat_ins);
		force  = new_r_Tuple(block, border_count, force_ins);

		/* Create an "acyclic" eta node. */
		eta_a = new_r_EtaA(block, header, repeat, value, cond, force, mode);

		pl_set_depth(pli, header, depth + 1);
		pl_set_depth(pli, repeat, depth + 1);
		pl_set_depth(pli, force,  depth);
		pl_copy_info(pli, eta, eta_a);
		exchange(eta, eta_a);

		obstack_free(&obst, force_ins);
		obstack_free(&obst, repeat_ins);
		obstack_free(&obst, header_ins);
	}

	/* Replace thetas by "acyclic" thetas. */
	foreach_pl_irg_theta(pli, it_theta, theta) {
		ir_node *block   = get_nodes_block(theta);
		ir_mode *mode    = get_irn_mode(theta);
		ir_node *init    = get_Theta_init(theta);
		ir_node *theta_a = new_r_ThetaA(block, init, mode);

		pl_copy_info(pli, theta, theta_a);
		exchange(theta, theta_a);
	}

	obstack_free(&obst, NULL);
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

void peg_to_cfg(ir_graph *irg)
{
	/* TODO: speculative execution of loop invariant memory? */

	pd_tree *pdt;
	pl_info *pli;
	gc_info *gci;

	pli = pl_init(irg);

#ifdef LOG_LOOP_ANALYSIS
	printf("------------------\n");
	printf("Loop analysis info\n");
	printf("------------------\n");
	pl_dump(pli, stdout);
#endif

	/* Transform the graph in an acyclic PEG. */
	peg_to_acpeg(irg, pli);

	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "acpeg");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

	/* Calculate the dominator tree on the AcPEG. */
	pdt = pd_init(irg);

#ifdef LOG_DOMINATOR_TREE
	printf("------------------\n");
	printf("PEG dominator tree\n");
	printf("------------------\n");
	pd_dump(pdt, stdout);
#endif

	gci = gc_init(irg, pdt, pli);

#ifdef LOG_GATING_CONDS
	printf("-----------------\n");
	printf("Gating conditions\n");
	printf("-----------------\n");
	gc_dump(gci, stdout);
#endif

	gc_free(gci);
	pl_free(pli);
	pd_free(pdt);
}
