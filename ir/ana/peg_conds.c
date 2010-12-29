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

#include "peg_conds_t.h"
#include "irphase_t.h"
#include "plist.h"
#include "peg_dom_t.h"
#include "peg_loop_t.h"
#include "obstack.h"
#include "ircons.h"
#include "pmap.h"
#include "irgmod.h"

typedef struct obstack obstack;

//#define LOG_DIRS_COMBINE 1

/* Convenience macro to iterate two pmaps in succession. */
#define foreach_pmap_2(lhs, rhs, i, entry) \
	for ((i) = 0; (i) < 2; (i)++) \
		foreach_pmap(((i == 0) ? (lhs) : (rhs)), (entry))

/******************************************************************************
 * Gating conditions.                                                         *
 ******************************************************************************/

/**
 * Gating conditions are just plain conditions without a relationship to nodes.
 * They are considered immutable, so any modification yields a new condition.
 * However due to their immutability, partial conditions can often be reused.
 */

struct gc_cond {
	gc_type type;
};

/* A pointer to a struct is guaranteed to also point to the first member. */
typedef struct gc_branch {
	gc_cond  base;
	ir_node *irn;
	gc_cond *lhs; /* True */
	gc_cond *rhs; /* False */
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
 * The node stores gating conditions for paths originating at the associated
 * irn. The cross gating conditions are used during computation. These are for
 * paths that cross into the subtrees of a sibling in the dominator tree.
 */

typedef struct gc_node {
	ir_node *irn;
	pmap    *conds;       /* Indexed by the target node. */
	pmap    *cross_conds; /* Indexed by the target node. */
	char     has_cross_conds;
} gc_node;

/* Gating Condition info. */
struct gc_info {
	obstack   obst;
	pd_tree  *pdt;
	pl_info  *pli;
	gc_node  *root;
	ir_phase *phase;
};

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
		gc_iter lhs_it, rhs_it;
		gc_cond *lhs_cur, *rhs_cur;
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

gc_cond *gc_get_demand(void)
{
	return gc_new_demand();
}

gc_cond *gc_get_ignore(void)
{
	return gc_new_ignore();
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
		gc_iter it;
		gc_cond *rhs_entry;

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

/**
 * The same as above, but with some debug output, to verify that operations
 * work as expected.
 */

static void gc_dump_cond(gc_cond *cond, FILE *f);

static gc_cond *gc_new_concat_log(gc_info *gci, gc_cond *lhs, gc_cond *rhs)
{
	gc_cond *result;

#ifdef LOG_DIRS_COMBINE
	printf("concat(");
	gc_dump_cond(lhs, stdout); printf(",");
	gc_dump_cond(rhs, stdout); printf(") = ");
#endif

	result = gc_new_concat(gci, lhs, rhs);

#ifdef LOG_DIRS_COMBINE
	gc_dump_cond(result, stdout); printf("\n");
#endif

	return result;
}

static gc_cond *gc_new_union_log(gc_info *gci, gc_cond *lhs, gc_cond *rhs)
{
	gc_cond *result;

#ifdef LOG_DIRS_COMBINE
	printf("               union (");
	gc_dump_cond(lhs, stdout); printf(",");
	gc_dump_cond(rhs, stdout); printf(") = ");
#endif

	result = gc_new_union(gci, lhs, rhs);

#ifdef LOG_DIRS_COMBINE
	gc_dump_cond(result, stdout); printf("\n");
#endif

	return result;
}

static void gc_cmap_add(gc_info *gci, pmap *map, gc_cond *cond, ir_node *dst)
{
	/* Combine with existing conds by union. */
	gc_cond *old_cond = pmap_get(map, dst);
	if (old_cond) cond = gc_new_union_log(gci, old_cond, cond);
	pmap_insert(map, dst, cond);
}

/******************************************************************************
 * Populate maps with conditions.                                             *
 ******************************************************************************/

/* Compute a condition for a single edge. */
static gc_cond *gc_new_edge_cond(gc_info *gci, ir_node *src, ir_node *dst)
{
	if (is_Gamma(src)) {
		/* Determine the edge. */
		int edge = 2;
		if      (get_Gamma_true(src)  == dst) edge = 0;
		else if (get_Gamma_false(src) == dst) edge = 1;

		if (edge < 2) {
			return gc_new_branch(gci,
				(edge == 0) ? gc_new_demand() : gc_new_ignore(),
				(edge == 0) ? gc_new_ignore() : gc_new_demand(),
				src
			);
		}
	} else if (is_EtaA(src)) {
		gc_cond *cond;
		int source_depth = pl_get_depth(gci->pli, src);
		int target_depth = pl_get_depth(gci->pli, dst);

		int edge = 2;
		if      (get_EtaA_result(src) == dst) edge = 0;
		else if (get_EtaA_repeat(src) == dst) edge = 1;

		if (edge < 2) {
			/* Evaluate the result value or next values, depending on the
			 * loop condition. This can be represented by a branch cond. */
			cond = gc_new_branch(gci,
				(edge == 0) ? gc_new_demand() : gc_new_ignore(),
				(edge == 0) ? gc_new_ignore() : gc_new_demand(),
				src
			);
		} else {
			/* Thetas and conditions are always evaluated. */
			cond = gc_new_demand();
		}

		/* Only create a repeat edge if the target is loop-nested. The header
		 * and repeat tuples are forced to be nested. The force tuple isn't. */
		if (target_depth > source_depth) {
			cond = gc_new_repeat(gci, cond, src);
		}

		return cond;
	}

	/* Default to demand. */
	return gc_new_demand();
}

static void gc_compute_cross_conds(gc_info *gci, gc_node *gcn, gc_node *lhs)
{
	pmap_entry *lhs_entry, *rhs_entry;

	/* Skip children with known cross dirs. */
	if (lhs->has_cross_conds) return;
	lhs->has_cross_conds = 1;

	/* Scan conventional conds in the child subgraph. */
	foreach_pmap(lhs->conds, lhs_entry) {
#ifdef LOG_DIRS_COMBINE
		ir_node *lhs_src  = lhs->irn;
#endif
		gc_cond *lhs_cond = lhs_entry->value;
		ir_node *lhs_dst  = (ir_node*)lhs_entry->key;

		/**
		 * Try to find two gating paths like this:
		 *
		 * rhs_dst
		 *  ^       y
		 *  |    /------\
		 *  |x   |      |
		 *  |    v      |
		 * lhs_dst   lhs_src
		 * rhs_src      |
		 *       |      |z
		 *       \      /
		 *       gcn->irn
		 *
		 * The conditions for the combined (non-gating) path y.x is a cross
		 * condition. When adding the edge z later, the whole path z.y.x will
		 * become a gating path.
		 */

		/* Check if the parent of lhs_dst in the dom tree is gcn->irn. */
		if (pd_get_parent(gci->pdt, lhs_dst) == gcn->irn) {
			int i;

			/* If it is, first calculate all cross conds in lhs_dst. */
			gc_node *rhs = phase_get_irn_data(gci->phase, lhs_dst);
			assert(rhs);

			gc_compute_cross_conds(gci, gcn, rhs);
			assert(rhs->has_cross_conds);

#ifdef LOG_DIRS_COMBINE
			if ((pmap_count(rhs->conds) + pmap_count(rhs->cross_conds)) > 0) {
				printf("Cross conds via %li -> %li\n",
					get_irn_node_nr(lhs_src), get_irn_node_nr(lhs_dst)
				);
			}
#endif

			/* Concatenate our gating conds with the rhs (cross) conds. */
			foreach_pmap_2(rhs->conds, rhs->cross_conds, i, rhs_entry) {
				gc_cond *rhs_cond = rhs_entry->value;
				ir_node *rhs_dst  = (ir_node*)rhs_entry->key;

#ifdef LOG_DIRS_COMBINE
				printf("   %3li -> %3li: ",
					get_irn_node_nr(lhs_src), get_irn_node_nr(rhs_dst)
				);
#endif

				gc_cond *cond = gc_new_concat_log(gci, lhs_cond, rhs_cond);
				gc_cmap_add(gci, lhs->cross_conds, cond, rhs_dst);
			}

#ifdef LOG_DIRS_COMBINE
			if ((pmap_count(rhs->conds) + pmap_count(rhs->cross_conds)) > 0) {
				printf("\n");
			}
#endif
		}
	}
}

static void gc_compute_dirs(gc_info *gci, gc_node *lhs)
{
	int      i, j;
	pd_iter  it_child;
	ir_node *ir_child;

	/* Recurse first. */
	foreach_pd_child(gci->pdt, lhs->irn, it_child, ir_child) {
		gc_node *gc_child = phase_get_or_set_irn_data(gci->phase, ir_child);
		gc_child->irn = ir_child;
		gc_compute_dirs(gci, gc_child);
	}

	/* Compute crossing dirs for the dominator children. */
	foreach_pd_child(gci->pdt, lhs->irn, it_child, ir_child) {
		gc_node *gc_child = phase_get_irn_data(gci->phase, ir_child);
		gc_compute_cross_conds(gci, lhs, gc_child);
	}

	/* Now create gating conds edge by edge. */
	for (i = 0; i < get_irn_arity(lhs->irn); i++) {
		ir_node *lhs_src  = lhs->irn;
		ir_node *lhs_dst  = get_irn_n(lhs_src, i);
		gc_cond *lhs_cond = gc_new_edge_cond(gci, lhs_src, lhs_dst);

#ifdef LOG_DIRS_COMBINE
		printf("Normal conds via (%li, %li)\n   %3li -> %3li: ",
			get_irn_node_nr(lhs_src), get_irn_node_nr(lhs_dst),
			get_irn_node_nr(lhs_src), get_irn_node_nr(lhs_dst)
		);

		gc_dump_cond(lhs_cond, stdout); printf("\n");
#endif

		/* Add the trivial gating cond for the lhs path of length 1. */
		gc_cmap_add(gci, lhs->conds, lhs_cond, lhs_dst);

		/* For dominated target nodes add the edge to their gating conds. */
		if (pd_dominates(gci->pdt, lhs_src, lhs_dst)) {
			/**
			 * Add the combined paths:
			 *
			 * gcn->irn ----> lhs_src ----> lhs_dst
			 *                              rhs_src ----> rhs_dst
			 */

			pmap_entry *rhs_entry;
			gc_node    *rhs = phase_get_irn_data(gci->phase, lhs_dst);
			assert(rhs);

			/* Concatenate the edge cond with the rhs (cross) conds. */
			foreach_pmap_2(rhs->conds, rhs->cross_conds, j, rhs_entry) {
				ir_node *rhs_dst  = (ir_node*)rhs_entry->key;
				gc_cond *rhs_cond = rhs_entry->value;

#ifdef LOG_DIRS_COMBINE
				printf("   %3li -> %3li: ",
					get_irn_node_nr(lhs_src), get_irn_node_nr(rhs_dst)
				);
#endif
				gc_cond *cond = gc_new_concat_log(gci, lhs_cond, rhs_cond);
				gc_cmap_add(gci, lhs->conds, cond, rhs_dst);
			}
		}

#ifdef LOG_DIRS_COMBINE
		printf("\n");
#endif
	}
}

static void *gc_init_node(ir_phase *phase, const ir_node *irn)
{
	gc_info *gci = phase_get_private(phase);
	gc_node *gcn = OALLOC(&gci->obst, gc_node);

	/* Initialize the condition maps. It's not obstacked, but we only allocate
	 * them once, so allocation speed isn't that much of an issue. */
	gcn->conds           = pmap_create_ex(10);
	gcn->cross_conds     = pmap_create_ex(5);
	gcn->has_cross_conds = 0;

	(void)irn;
	return gcn;
}

gc_info *gc_init(ir_graph *irg, pd_tree *pdt, pl_info *pli)
{
	gc_info *gci = XMALLOC(gc_info);
	ir_node *ret;
	assert((pd_get_irg(pdt) == irg) && (pl_get_irg(pli) == irg));

	obstack_init(&gci->obst);

	gci->pdt = pdt;
	gci->pli = pli;
	ret = pd_get_root(gci->pdt);
	assert(is_Return(ret));

	gci->phase = new_phase(irg, gc_init_node);
	phase_set_private(gci->phase, gci);

#ifdef LOG_DIRS_COMBINE
	printf("--------------------\n");
	printf("Computing conditions\n");
	printf("--------------------\n");
#endif

	/* Walk through the tree to compute directions. */
	gci->root = phase_get_or_set_irn_data(gci->phase, ret);
	gci->root->irn = ret;
	gc_compute_dirs(gci, gci->root);

	return gci;
}

void gc_free(gc_info *gci)
{
	ir_node *irn;

	/* Free all the maps in the nodes. */
	foreach_phase_irn(gci->phase, irn) {
		gc_node *gcn = phase_get_irn_data(gci->phase, irn);
		assert(gcn);

		pmap_destroy(gcn->conds);
		pmap_destroy(gcn->cross_conds);
	}

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

static void gc_dump_path(ir_node *src, ir_node *dst, gc_cond *cond, FILE *f)
{
	fprintf(f, "gc_%3li(%3li) = ",
		get_irn_node_nr(src),
		get_irn_node_nr(dst)
	);

	gc_dump_cond(cond, f);
}

void gc_dump(gc_info *gci, FILE *f)
{
	pmap_entry *entry;
	foreach_pmap(gci->root->conds, entry) {
		ir_node *dst  = (ir_node*)entry->key;
		gc_cond *cond = entry->value;

		gc_dump_path(gci->root->irn, dst, cond, f);
		fprintf(f, "\n");
	}
}

/******************************************************************************
 * Public query interface.                                                    *
 ******************************************************************************/

gc_cond *gc_get_cond_for(gc_info *gci, ir_node *src, ir_node *dst)
{
	pmap_entry *entry;
	gc_node    *gcn = phase_get_irn_data(gci->phase, src);
	assert(gcn && "No information for the given node.");

	entry = pmap_find(gcn->conds, dst);
	if (!entry) return NULL;
	return entry->value;
}

int gc_get_cond_count(gc_info *gci, ir_node *irn)
{
	gc_node *gcn = phase_get_irn_data(gci->phase, irn);
	assert(gcn && "No information for the given node.");
	return pmap_count(gcn->conds);
}

gc_entry gc_get_cond(gc_info *gci, ir_node *irn, gc_iter *it)
{
	gc_node    *gcn = phase_get_irn_data(gci->phase, irn);
	pmap_entry *entry;
	gc_entry    res;
	assert(gcn && "No information for the given node.");

	/* Unfortunately, pmap iteration is a bit strange, with the need to break
	 * iterations. This will have to do for now. */
	*it   = gcn;
	entry = pmap_first(gcn->conds);
	res.cond = entry->value;
	res.dst  = (ir_node*)entry->key;
	return res;
}

gc_entry gc_cond_iter_next(gc_iter *it)
{
	gc_entry    res   = { NULL, NULL };
	gc_node    *gcn   = *it;
	pmap_entry *entry = pmap_next(gcn->conds);
	if (!entry) return res;

	res.cond = entry->value;
	res.dst  = (ir_node*)entry->key;
	return res;
}

gc_cond *gc_get_union_cond(gc_cond *cond, gc_iter *it)
{
	gc_union *gcu = (gc_union*)cond;
	assert(cond->type == gct_union);

	*it = gcu->lhs;
	return gcu->rhs;
}

gc_cond *gc_union_iter_next(gc_iter *it)
{
	gc_cond  *cond = *it;
	gc_union *gcu;
	if (!cond) return NULL;

	/* Cancel if it is no union. */
	if (cond->type != gct_union) {
		*it = NULL;
		return cond;
	}

	/* Return the rhs, make lhs the new it. */
	gcu = (gc_union*)cond;
	*it = gcu->lhs;
	return gcu->rhs;
}

/* A bit tedious, but it keeps the implementation details hidden. */

gc_type gc_get_cond_type(gc_cond *cond)
{
	return cond->type;
}

ir_node *gc_get_branch_irn(gc_cond *cond)
{
	assert(cond->type == gct_branch);
	return ((gc_branch*)cond)->irn;
}

gc_cond *gc_get_branch_lhs(gc_cond *cond)
{
	assert(cond->type == gct_branch);
	return ((gc_branch*)cond)->lhs;
}

gc_cond *gc_get_branch_rhs(gc_cond *cond)
{
	assert(cond->type == gct_branch);
	return ((gc_branch*)cond)->rhs;
}

ir_node *gc_get_repeat_irn(gc_cond *cond)
{
	assert(cond->type == gct_repeat);
	return ((gc_repeat*)cond)->irn;
}

gc_cond *gc_get_repeat_cond(gc_cond *cond)
{
	assert(cond->type == gct_repeat);
	return ((gc_repeat*)cond)->cond;
}

gc_cond *gc_get_union_lhs(gc_cond *cond)
{
	assert(cond->type == gct_union);
	return ((gc_union*)cond)->lhs;
}

gc_cond *gc_get_union_rhs(gc_cond *cond)
{
	assert(cond->type == gct_union);
	return ((gc_union*)cond)->rhs;
}

/******************************************************************************
 * Acyclic PEG creation.                                                      *
 ******************************************************************************/

void peg_to_acpeg(ir_graph *irg, pl_info *pli)
{
	obstack  obst;
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
