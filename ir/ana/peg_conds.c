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
#include <assert.h>
#include <stdio.h>
#include "firm_types.h"
#include "hashptr.h"
#include "plist.h"
#include "obst.h"
#include "pmap_new.h"
#include "irphase_t.h"
#include "ircons.h"
#include "irgmod.h"

//#define LOG_COMBINE 1

typedef struct obstack obstack;

/* Convenience macro to iterate two pmaps in succession. */
#define foreach_pmap_2(lhs, rhs, entry, i, it) \
	for ((i) = 0; (i) < 2; (i)++) \
		foreach_pmap_new(((i == 0) ? (lhs) : (rhs)), (entry), (it))

/******************************************************************************
 * Gating conditions.                                                         *
 ******************************************************************************/

struct gc_cond {
	/* Set if one cond in the chain is while. */
	char     loop : 1;
	gc_type  type;
	ir_node *irn;
	const struct gc_cond *next;
};

/******************************************************************************
 * Gating conditions concat map.                                              *
 ******************************************************************************/

typedef struct gc_cmap          gc_cmap;
typedef struct gc_cmap_iterator gc_cmap_iterator;

/* Operation and associated result. */
typedef struct gc_cmap_entry {
	const gc_cond *lhs;
	const gc_cond *rhs;
	const gc_cond *res;
} gc_cmap_entry;

#define INVL ((void*)-1)

gc_cmap_entry _gc_cmap_null = { NULL, NULL, NULL };
gc_cmap_entry _gc_cmap_del  = { INVL, INVL, INVL };

#define DO_REHASH
#define SCALAR_RETURN
#define HashSet                   gc_cmap
#define HashSetIterator           gc_cmap_iterator
#define ValueType                 gc_cmap_entry
#define NullValue                _gc_cmap_null
#define DeletedValue             _gc_cmap_del
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof(HashSetEntry))
#define KeysEqual(map,key1,key2)  ((key1.lhs == key2.lhs) && (key1.rhs == key2.rhs))
#define Hash(map,key1)            HASH_COMBINE(hash_ptr(key1.lhs), hash_ptr(key1.rhs))
#define EntryIsEmpty(e)           (EntryGetValue(e).lhs == NULL)
#define EntryIsDeleted(e)         (EntryGetValue(e).lhs == INVL)

#define hashset_init            _gc_cmap_init
#define hashset_init_size       _gc_cmap_init_size
#define hashset_destroy         _gc_cmap_destroy
#define hashset_insert          _gc_cmap_insert
#define hashset_remove          _gc_cmap_remove
#define hashset_find            _gc_cmap_find
#define hashset_size            _gc_cmap_size
#define hashset_iterator_init   _gc_cmap_iterator_init
#define hashset_iterator_next   _gc_cmap_iterator_next
#define hashset_remove_iterator _gc_cmap_remove_iterator

#include "hashset.h"

/* Prototypes for map operations. */
size_t        _gc_cmap_size      (const gc_cmap *map);
gc_cmap_entry _gc_cmap_find      (const gc_cmap *map, const gc_cmap_entry entry);
void          _gc_cmap_init      (gc_cmap *map);
void          _gc_cmap_destroy   (gc_cmap *map);
gc_cmap_entry _gc_cmap_insert    (gc_cmap *map, gc_cmap_entry entry);
void          _gc_cmap_remove    (gc_cmap *map, const gc_cmap_entry entry);
void          _gc_cmap_init_size (gc_cmap *map, size_t expected_elements);

gc_cmap_entry _gc_cmap_iterator_next   (gc_cmap_iterator *it);
void          _gc_cmap_iterator_init   (gc_cmap_iterator *it, const gc_cmap *map);
void          _gc_cmap_remove_iterator (gc_cmap *map, const gc_cmap_iterator *it);

#include "hashset.c"

/******************************************************************************
 * Gating condition creation.                                                 *
 ******************************************************************************/

struct gc_union {
	plist_t *conds;
	char     done;
};

typedef struct gc_node {
	ir_node     *irn;

	/* Contains conds for dominated >children< (no other dominated nodes) and
	 * undominated other nodes that are reachable. */
	pmap_new_t   conds;

	/* Contains conds for >siblings< we can reach and nodes we can reach from
	 * there, that the sibling doesn't dominate itself. */
	pmap_new_t  *cross_conds;

	/* The root conds for construction. Every new cond will be looked up in
	 * the gc_node of its irn, so that the same objects are used for the same
	 * conditions. */
	gc_cond    **roots;
} gc_node;

/* Gating Condition info. */
struct gc_info {
	obstack   obst;
	pd_tree  *pdt;
	pl_info  *pli;
	gc_node  *root;
	ir_phase *phase;
	gc_cmap   cmap;
};

static const gc_cond *gc_get_once(void)
{
	static gc_cond once = { 0, gct_once, NULL, NULL };
	return &once;
}

static const gc_cond *gc_get_cond(gc_info *info, gc_type type, ir_node *irn)
{
	gc_node *gcn;
	gc_cond *cond;

	if (type == gct_once) return gc_get_once();
	assert(irn);

	gcn = phase_get_or_set_irn_data(info->phase, irn);

	/* Use gcn->roots as lazily initialized lookup table. */
	if (!gcn->roots) {
		gcn->roots = OALLOCNZ(&info->obst, gc_cond*, gct_last_cached + 1);
	}

	assert(type <= gct_last_cached);
	cond = gcn->roots[type];

	if (!cond) {
		/* Create a new cond if none is present. */
		cond = OALLOC(&info->obst, gc_cond);
		cond->loop = (type == gct_while_true);
		cond->type = type;
		cond->irn  = irn;
		cond->next = gc_get_once();
		gcn->roots[type] = cond;
	}

	return cond;
}

static const gc_cond *gc_concat(gc_info *info, const gc_cond *lhs,
                                               const gc_cond *rhs)
{
	gc_cmap_entry entry = { lhs, rhs, NULL };
	assert(lhs && rhs);

	/* Add piecewise, to rebuild the lhs chain with rhs as tail. */
	if (lhs->next) {
		rhs = gc_concat(info, lhs->next, rhs);
	}

	/* Try to lookup the result first. */
	entry = _gc_cmap_find(&info->cmap, entry);

	/* Calculate it if needed. */
	if (entry.lhs == NULL) {
		/* Handle once. */
		if (lhs->type == gct_once) {
			entry.res = rhs;
		} else if (rhs->type == gct_once) {
			entry.res = lhs;
		} else {
			/* Prepend a new node. */
			gc_cond *res = OALLOC(&info->obst, gc_cond);
			res->loop = lhs->loop || rhs->loop;
			res->type = lhs->type;
			res->irn  = lhs->irn;
			res->next = rhs;
			entry.res = res;
		}

		entry.lhs = lhs;
		entry.rhs = rhs;

		assert(entry.res);
		_gc_cmap_insert(&info->cmap, entry);
	}

	assert(entry.res);
	return entry.res;
}

static void gc_dump_cond(const gc_cond *cond, FILE *f)
{
	assert(cond);

	if (cond->irn) {
		fprintf(f, "%li", get_irn_node_nr(cond->irn));
	}

	switch (cond->type) {
	case gct_once:       fprintf(f, "Once");    break;
	case gct_while_true: fprintf(f, "@");       break;
	case gct_if_true:    fprintf(f, "+");       break;
	case gct_if_false:   fprintf(f, "-");       break;
	case gct_invalid:    fprintf(f, "INVALID"); break;
	}

	if (cond->next && (cond->next->type != gct_once)) {
		fprintf(f, " ");
		gc_dump_cond(cond->next, f);
	}
}

/******************************************************************************
 * Gating condition unification.                                              *
 ******************************************************************************/

static gc_union *gc_new_union(gc_info *info)
{
	gc_union *uni = OALLOC(&info->obst, gc_union);
	uni->conds = plist_obstack_new(&info->obst);
	uni->done  = 0;
	return uni;
}

static void gc_dump_union(const gc_union *uni, FILE *f)
{
	if (plist_count(uni->conds) == 0) {
		fprintf(f, "Never");
	} else {
		plist_element_t *it;
		foreach_plist(uni->conds, it) {
			if (it != plist_first(uni->conds)) fprintf(f, " | ");
			gc_dump_cond(it->data, f);
		}
	}
}

static const gc_cond *gc_try_unify(gc_info *info, const gc_cond *lhs,
                                   const gc_cond *rhs)
{
	assert(lhs && rhs);
	if (lhs == rhs) return lhs;

	/* Handle the once cond. */
	if (lhs->type == gct_once) return rhs->loop ? INVL : gc_get_once();
	if (rhs->type == gct_once) return lhs->loop ? INVL : gc_get_once();

	/* Two identical conds that only differ in one position can be collapsed
	 * if that condition is if_true and if_false. For example:
	 *
	 * A+ B@ C- D+ E- | A+ B@ C+ D+ E- = A+ B@ D+ E-
	 *
	 * Note that shorter paths have less restrictions and are collapsed, too.
	 *
	 * A+ B- | A+ = A+
	 *
	 * But loops don't apply to path shortening:
	 *
	 * A+ B@ | A+
	 *
	 * Can't be collapsed, since if A+ is satisfied, the rhs causes evaluation
	 * once, while the lhs causes evaluation 0 to n times. */

	/* Same node is the first obvious check. */
	if (lhs->irn == rhs->irn) {

		/* Is this a common prefix? */
		if (lhs->type == rhs->type) {
			/* Recurse. If we get a new cond, prepend us. */
			const gc_cond *res = gc_try_unify(info, lhs->next, rhs->next);
			if (res == INVL) return INVL;

			/* Use res as new suffix (shortened to the same suffix). */
			assert(lhs->irn);
			return gc_concat(info,
				gc_get_cond(info, lhs->type, lhs->irn), res
			);
		} else {
			/* Check if the types allow folding. */
			if (((lhs->type != gct_if_true) && (lhs->type != gct_if_false)) ||
			    ((rhs->type != gct_if_true) && (rhs->type != gct_if_false))) {
				return INVL;
			}

			/* The suffixes have to match. */
			if (lhs->next != rhs->next) {
				return INVL;
			}

			/* If they do, just return the suffix. */
			return lhs->next;
		}
	}

	/* Can't unify both conds. */
	return INVL;
}

static void gc_unify(gc_info *info, gc_union *uni, const gc_cond *lhs)
{
	plist_element_t *it, *it_next;
	assert(lhs);

	/* Insert the element into the list. */
	it = plist_first(uni->conds);

	while (it) {
		const gc_cond *rhs = it->data;
		const gc_cond *res = gc_try_unify(info, lhs, rhs);
		assert(res);

		it_next = it->next;

		if (res != INVL) {
			/* Nothing changed? Don't recurse. */
			if (res == rhs) return;

			/* Delete the old entry, recursively insert the new. */
			plist_erase(uni->conds, it);
			gc_unify(info, uni, res);
			return; /* We are done here. */
		}

		it = it_next;
	}

	plist_insert_back(uni->conds, (void*)lhs);
}

/******************************************************************************
 * Build gating conditions for nodes.                                         *
 ******************************************************************************/

/* Compute a condition for a single edge. */
static const gc_cond *gc_new_edge_cond(gc_info *gci, ir_node *src, ir_node *dst)
{
	if (is_Gamma(src)) {
		/* Determine the edge. */
		gc_type type = gct_invalid;
		if      (get_Gamma_true(src)  == dst) type = gct_if_true;
		else if (get_Gamma_false(src) == dst) type = gct_if_false;

		if (type != gct_invalid) {
			ir_node *cond = get_Gamma_cond(src);
			return gc_get_cond(gci, type, cond);
		}

		return gc_get_once();

	} else if (is_EtaA(src)) {
		ir_node       *cond = get_EtaA_cond(src);
		const gc_cond *res  = gc_get_once();

		int source_depth = pl_get_depth(gci->pli, src);
		int target_depth = pl_get_depth(gci->pli, dst);

		/* Determine the edge. */
		gc_type type = gct_invalid;
		if      (get_EtaA_result(src) == dst) type = gct_if_true;
		else if (get_EtaA_repeat(src) == dst) type = gct_if_false;

		if (type != gct_invalid) {
			res = gc_get_cond(gci, type, cond);
		}

		/* Only create a while edge if the target is loop-nested. The header
		 * and repeat tuples are forced to be nested. The force tuple isn't. */
		if (target_depth > source_depth) {
			res = gc_get_cond(gci, gct_while_true, cond);
		}

		return res;
	}

	return gc_get_once();
}

static gc_union *gc_get_union_from(gc_info *gci, pmap_new_t *map, ir_node *dst)
{
	/* Get the union for the target node. */
	gc_union *uni = pmap_new_get(map, dst);

	if (!uni) {
		uni = gc_new_union(gci);
		pmap_new_insert(map, dst, uni);
	}

	return uni;
}

static void gc_concat_union(gc_info *info, gc_union *lhs,
                            gc_union *rhs, gc_union *res)
{
	/* Consider all combined paths. */
	plist_element_t *i, *j;

	foreach_plist(lhs->conds, i) {
		foreach_plist(rhs->conds, j) {
			gc_unify(info, res, gc_concat(info, i->data, j->data));
		}
	}
}

static void gc_compute_cross_conds(gc_info *gci, gc_node *gc_parent,
                                   gc_node *gc_src)
{
	pmap_new_iterator_t it_path, it_cont;
	pmap_new_entry_t    en_path, en_cont;
	ir_node *ir_parent = gc_parent->irn;

	/* Skip finished children (normal and cross conds known). */
	if (gc_src->cross_conds) return;

	gc_src->cross_conds = XMALLOC(pmap_new_t);
	pmap_new_init(gc_src->cross_conds);

	/* Scan conventional conds in the child subgraph. */
	foreach_pmap_new(&gc_src->conds, en_path, it_path) {
		gc_union *un_path = en_path.value;
		ir_node  *ir_dst  = en_path.key;

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
		if (pd_get_parent(gci->pdt, ir_dst) == ir_parent) {
			int i;

			/* If it is, first calculate all the remaining conds in ir_dst. */
			gc_node *gc_dst = phase_get_irn_data(gci->phase, ir_dst);
			gc_compute_cross_conds(gci, gc_parent, gc_dst);
			assert(gc_dst->cross_conds); /* Normal and cross conds known. */

			/* Concatenate the path gating conds with the continuations. */
			foreach_pmap_2(&gc_dst->conds, gc_dst->cross_conds, en_cont, i, it_cont) {
				gc_union *un_full;
				gc_union *un_cont = en_cont.value;
				ir_node  *ir_app  = en_cont.key;

				/* Skip continuations where ir_dst dominates ir_app. There is
				 * no need to add further information to those conds. */
				if (pd_dominates(gci->pdt, ir_dst, ir_app)) continue;

				un_full = gc_get_union_from(gci, gc_src->cross_conds, ir_app);
				gc_concat_union(gci, un_path, un_cont, un_full);
			}
		}
	}
}

static void gc_compute_conds(gc_info *gci, ir_node *irn)
{
	pd_children_iter it_path;

	int i, j;

	/**
	 * Define variables for the nodes no process. Paths are layed out like:
	 * src ------> dst ------> app     (source / destination / appendix)
	 *      path        cont           (path / continuation)
	 *            full                 (full path)
	 */

	const gc_cond *cd_path;
	gc_node  *gc_src = phase_get_or_set_irn_data(gci->phase, irn);
	gc_node  *gc_dst;
	ir_node  *ir_src = irn;
	ir_node  *ir_dst, *ir_app;
	gc_union *un_path, *un_cont, *un_full;

	/* Recurse to the children. */
	foreach_pd_children(gci->pdt, ir_src, ir_dst, it_path) {
		gc_compute_conds(gci, ir_dst);
	}

	/* Compute crossing conds for the children. */
	foreach_pd_children(gci->pdt, ir_src, ir_dst, it_path) {
		gc_dst = phase_get_or_set_irn_data(gci->phase, ir_dst);
		gc_compute_cross_conds(gci, gc_src, gc_dst);
	}

#ifdef LOG_COMBINE
	int output = 0;
#endif

	/* Add gating conds of length 1. */
	for (i = 0; i < get_irn_arity(ir_src); i++) {
		int dp_src, dp_dst;

		/* Get information about the edge. */
		ir_dst  = get_irn_n(ir_src, i);
		cd_path = gc_new_edge_cond(gci, ir_src, ir_dst);
		un_path = gc_get_union_from(gci, &gc_src->conds, ir_dst);
		dp_src  = pl_get_depth(gci->pli, ir_src);
		dp_dst  = pl_get_depth(gci->pli, ir_dst);

		/* Only consider edges to nodes with equal or greater depth. */
		if (dp_src <= dp_dst) {
			/* Add the gating cond to the paths union. */
			gc_unify(gci, un_path, cd_path);

#ifdef LOG_COMBINE
			if (plist_count(un_path->conds) > 0) {
				printf("%4li -- %4li:         ",
					get_irn_node_nr(ir_src),
					get_irn_node_nr(ir_dst)
				);

				output = 1;
				gc_dump_union(un_path, stdout);
				printf("\n");
			}
#endif
		}
	}

	/* Combine gating conds on the children. */
	foreach_pd_children(gci->pdt, ir_src, ir_dst, it_path) {
		pmap_new_iterator_t it_cont;
		pmap_new_entry_t    en_cont;

		/* Get the union for the path. */
		gc_dst  = phase_get_irn_data(gci->phase, ir_dst);
		un_path = gc_get_union_from(gci, &gc_src->conds, ir_dst);
		assert(gc_dst);

		/* Concatenate the path with a continuation path. */
		foreach_pmap_2(&gc_dst->conds, gc_dst->cross_conds, en_cont, j, it_cont) {
			ir_app  = en_cont.key;
			un_cont = en_cont.value;

			/* Skip continuations, that dst dominates. There is no need
			 * to add further information to those conds. */
			if (pd_dominates(gci->pdt, ir_dst, ir_app)) continue;

			un_full = gc_get_union_from(gci, &gc_src->conds, ir_app);
			gc_concat_union(gci, un_path, un_cont, un_full);

#ifdef LOG_COMBINE
			if (plist_count(un_full->conds) > 0) {
				printf("%4li -> %4li %s> %4li: ",
					get_irn_node_nr(ir_src),
					get_irn_node_nr(ir_dst),
					(j == 0) ? "-" : "#",
					get_irn_node_nr(ir_app)
				);

				output = 1;
				gc_dump_union(un_full, stdout);
				printf("\n");
			}
#endif
		}

		/* Mark the union as done. This way we don't need pl_info later. */
		un_path->done = 1;
	}

#ifdef LOG_COMBINE
	if (output) printf("\n");
#endif

	/* Free the child cross conds again. */
	foreach_pd_children(gci->pdt, ir_src, ir_dst, it_path) {
		gc_dst = phase_get_irn_data(gci->phase, ir_dst);

		pmap_new_destroy(gc_dst->cross_conds);
		xfree(gc_dst->cross_conds);
		gc_dst->cross_conds = NULL;
	}
}

static void *gc_init_node(ir_phase *phase, const ir_node *irn)
{
	gc_info *gci = phase_get_private(phase);
	gc_node *gcn = OALLOC(&gci->obst, gc_node);

	/* Initialize the condition maps. It's not obstacked, but we only allocate
	 * them once, so allocation speed isn't that much of an issue. */

	pmap_new_init(&gcn->conds);
	gcn->roots       = NULL;
	gcn->cross_conds = NULL;
	gcn->irn         = (ir_node*)irn;

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

	_gc_cmap_init(&gci->cmap);

	/* Walk through the tree to compute directions. */
	gc_compute_conds(gci, ret);
	gci->root = phase_get_irn_data(gci->phase, ret);
	assert(gci->root);

	gci->pli = NULL;
	gci->pdt = NULL;
	return gci;
}

void gc_free(gc_info *gci)
{
	ir_node *irn;

	/* Free all the maps in the nodes. */
	foreach_phase_irn(gci->phase, irn) {
		gc_node *gcn = phase_get_irn_data(gci->phase, irn);
		assert(gcn);

		pmap_new_destroy(&gcn->conds);
		assert(!gcn->cross_conds);
	}

	_gc_cmap_destroy(&gci->cmap);

	phase_free(gci->phase);
	obstack_free(&gci->obst, NULL);
	xfree(gci);
}

static void gc_dump_node(gc_info *gci, gc_node *gcn, FILE *f, int indent)
{
	pmap_new_entry_t    entry;
	pmap_new_iterator_t it;

	foreach_pmap_new(&gcn->conds, entry, it) {
		int       i;
		gc_union *uni   = entry.value;
		ir_node  *ir_dst = entry.key;
		gc_node  *gc_dst;
		if (!uni->done) continue;

		for (i = 0; i < indent; i++) fprintf(f, "  ");

		fprintf(f, "%s %li",
			get_op_name(get_irn_op(ir_dst)),
			get_irn_node_nr(ir_dst)
		);

		fprintf(f, ": ");
		gc_dump_union(uni, f);
		fprintf(f, "\n");

		gc_dst = phase_get_irn_data(gci->phase, ir_dst);
		if (gc_dst) gc_dump_node(gci, gc_dst, f, indent + 1);
	}
}

void gc_dump(gc_info *gci, FILE *f)
{
	fprintf(f, "%s %li\n",
		get_op_name(get_irn_op(gci->root->irn)),
		get_irn_node_nr(gci->root->irn)
	);

	gc_dump_node(gci, gci->root, f, 1);
}

/******************************************************************************
 * Acyclic PEG creation.                                                      *
 ******************************************************************************/

void peg_to_acpeg(ir_graph *irg, pl_info *pli)
{
	obstack obst;

	pl_irg_thetas_iter it;
	pl_thetas_iter     it_theta;
	pl_border_iter     it_border;

	ir_node *eta, *theta, *irn;

	obstack_init(&obst);
	assert(pl_get_irg(pli) == irg);

	/* Process all etas in the graph. */
	foreach_pl_irg_etas(pli, eta, it) {
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
		foreach_pl_thetas(pli, eta, theta, it_theta) {
			assert(pl_get_depth(pli, theta) == (depth + 1));
			header_ins[index] = theta;
			repeat_ins[index] = get_Theta_next(theta);
			index++;
		}

		index = 0;
		foreach_pl_border(pli, eta, irn, it_border) {
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
	foreach_pl_irg_thetas(pli, theta, it_theta) {
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

void gc_union_map_iter_init(gc_info *gci, ir_node *irn, gc_union_map_iter *it)
{
	gc_node *gcn = phase_get_irn_data(gci->phase, irn);
	assert(gcn && "No information for the given node.");

	pmap_new_iterator_init(it, &gcn->conds);
}

gc_entry gc_union_map_iter_next(gc_union_map_iter *it)
{
	gc_entry res = { NULL, NULL };

	while (1)
	{
		/* Advance the iterator. */
		pmap_new_entry_t entry = pmap_new_iterator_next(it);
		gc_union *uni;

		/* If we reached the end, cancel. */
		if (pmap_new_is_null(entry)) break;
		uni = entry.value;

		/* Only return finished unions. */
		if (uni->done) {
			res.uni = uni;
			res.dst = entry.key;
			return res;
		}
	}

	return res;
}

int gc_entry_is_null(gc_entry entry)
{
	return entry.dst == NULL;
}

void gc_union_iter_init(gc_union *uni, gc_union_iter *it)
{
	*it = plist_first(uni->conds);
}

gc_cond *gc_union_iter_next(gc_union_iter *it)
{
	if (!*it) return NULL;

	gc_cond *cond = (*it)->data;
	*it = (*it)->next;
	return cond;
}

gc_type gc_get_cond_type(gc_cond *cond)
{
	return cond->type;
}

ir_node *gc_get_cond_irn(gc_cond *cond)
{
	return cond->irn;
}

gc_cond *gc_get_cond_next(gc_cond *cond)
{
	/* Const is just a protection for the code in this source file. Outside
	 * of it, gc_cond is an opaque type, so no protection is needed. */
	return (gc_cond*)cond->next;
}

int gc_union_is_empty(gc_union *uni)
{
	return plist_count(uni->conds) == 0;
}
