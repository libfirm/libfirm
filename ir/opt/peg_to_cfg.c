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
 * @brief   Convert program expression graphs to firm graphs.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "config.h"

#include <string.h>
#include "iroptimize.h"
#include "irphase_t.h"
#include "iredges.h"
#include "irtools.h"
#include "plist.h"
#include "peg_dom_t.h"
#include "peg_loop_t.h"
#include "peg_conds_t.h"
#include "obstack.h"
#include "irgwalk.h"
#include "ircons.h"
#include "irdump.h"
#include "irgmod.h"
#include "pmap_new.h"
#include "pset_new.h"
#include "irflag.h"
#include "irnode_t.h"

//#define LOG_DOMINATOR_TREE 1
////#define LOG_LOOP_ANALYSIS  1
#define LOG_GATING_CONDS   1
#define LOG_CFG_REGIONS    1

typedef struct obstack obstack;

/******************************************************************************
 * CFG template creation.                                                     *
 ******************************************************************************/

typedef enum ct_type {
	ctt_root,
	ctt_branch,
	ctt_loop
} ct_type;

typedef struct ct_node     ct_node;
typedef struct ct_region   ct_region;
typedef struct ct_template ct_template;

/**
 * Templates give a structure to the tree. Each template contains several
 * regions, which in turn contain other templates. The container template
 * is used at the leaves, to store nodes.
 */
struct ct_template {
	ct_type       type;
	ir_node      *irn;
	ct_region    *region;
	ct_region    *children[2];
	pset_new_t    deps;
	plist_t      *links;
	ir_node      *block;
	ct_template  *next;
	unsigned int  visited;
};

struct ct_region {
	char         index;
	ct_template *template;
	plist_t     *children;
	pmap_new_t   branches, loops;
	ir_node     *block;
	ct_region   *link;
	int          depth;
};

struct ct_node {
	plist_t *regions;
	ir_node *irn;
	ct_node *copy;
};

typedef struct ct_info {
	obstack       obst;
	gc_info      *gci;
	pd_tree      *pdt;
	ct_template  *root;
	ir_phase     *phase;
	unsigned int  counter;
} ct_info;

static ct_region *ct_new_region(ct_info *cti, ct_template *tmpl, char index)
{
	ct_region *reg = OALLOC(&cti->obst, ct_region);
	reg->index     = index;
	reg->template  = tmpl;
	reg->children  = plist_obstack_new(&cti->obst);
	reg->block     = NULL;
	reg->link      = NULL;
	reg->depth     = -1;

	if (tmpl) {
		reg->depth = tmpl->region ? (tmpl->region->depth + 1) : 0;
	}

	pmap_new_init(&reg->branches);
	pmap_new_init(&reg->loops);
	return reg;
}

/* Freeing the data again is a bit complex because of the maps. */
static void ct_free_template(ct_template *tmpl);

static void ct_free_region(ct_region *region)
{
	plist_element_t *it;

	/* Free templates in the region first. */
	foreach_plist(region->children, it) {
		ct_free_template(it->data);
	}

	pmap_new_destroy(&region->branches);
	pmap_new_destroy(&region->loops);
}

static void ct_free_template(ct_template *tmpl)
{
	/* Free all the regions in the template. */
	if (tmpl->children[0]) ct_free_region(tmpl->children[0]);
	if (tmpl->children[1]) ct_free_region(tmpl->children[1]);
	pset_new_destroy(&tmpl->deps);
}

/* Just for convenience. */
static ct_template *ct_new_template(ct_info *cti, ct_type type, ir_node *irn,
                                    int size, ct_region *region)
{
	ct_template *tmpl = OALLOC(&cti->obst, ct_template);
	tmpl->type        = type;
	tmpl->irn         = irn;
	tmpl->region      = region;
	tmpl->children[0] = (size > 0) ? ct_new_region(cti, tmpl, 0) : NULL;
	tmpl->children[1] = (size > 1) ? ct_new_region(cti, tmpl, 1) : NULL;
	tmpl->links       = plist_obstack_new(&cti->obst);
	tmpl->next        = NULL;
	tmpl->block       = NULL;
	tmpl->visited     = 0;
	pset_new_init(&tmpl->deps);
	return tmpl;
}

static void ct_transfer_regions(ct_info *cti, ir_node *lhs, ir_node *rhs)
{
	plist_element_t *it;
	ct_node *ct_lhs = phase_get_irn_data(cti->phase, lhs);
	ct_node *ct_rhs = phase_get_or_set_irn_data(cti->phase, rhs);

	plist_clear(ct_rhs->regions);
	foreach_plist(ct_lhs->regions, it) {
		plist_insert_back(ct_rhs->regions, it->data);
	}
}

static void *ct_init_node(ir_phase *phase, const ir_node *irn)
{
	ct_info *cti = phase_get_private(phase);
	ct_node *ctn = OALLOC(&cti->obst, ct_node);

	ctn->regions = plist_obstack_new(&cti->obst);
	ctn->copy    = NULL;
	ctn->irn     = (ir_node*)irn;

	return ctn;
}

static ct_template *ct_ensure_template(ct_info *cti, ct_type type,
                                       ir_node *irn, ct_region *region)
{
	pmap_new_t  *map = NULL;
	ct_template *tmpl;

	/* Select the map to use. */
	switch (type) {
	case ctt_branch: map = &region->branches; break;
	case ctt_loop:   map = &region->loops;    break;
	default:         assert(0);               break;
	}

	/* Try to get an existing template from the region. */
	tmpl = pmap_new_get(map, irn);

	if (!tmpl) {
		tmpl = ct_new_template(cti, type, irn, 2, region);
		plist_insert_back(region->children, tmpl);
		pmap_new_insert(map, irn, tmpl);
	}

	return tmpl;
}

static void ct_pull_nodes(ct_info *cti, ir_node *irn, ct_region *region);

static void ct_place_node(ct_info *cti, ir_node *irn, gc_cond *gc,
                          ct_region *region)
{
	switch (gc_get_cond_type(gc)) {
	case gct_once: {
		/* No condition. Place in reg and be done. */
		ct_node *ctn = phase_get_or_set_irn_data(cti->phase, irn);
		plist_insert_back(ctn->regions, region);

		/* Pull in dominated nodes. */
		ct_pull_nodes(cti, irn, region);
		break;
	}
	case gct_if_true:
	case gct_if_false: {
		/* Get the condition node. */
		ir_node     *cond    = gc_get_cond_irn(gc);
		ct_template *tmpl    = region->template;
		int          is_true = (gc_get_cond_type(gc) == gct_if_true);

		/* Is this the repeat/leave branch of a loop? */
		if (tmpl && (tmpl->type == ctt_loop) && (tmpl->irn == cond)) {
			/* Place in the region above the loop or the repeat region. */
			ct_place_node(
				cti, irn, gc_get_cond_next(gc),
				is_true ? tmpl->region : tmpl->children[1]
			);
		} else {
			ct_template *tmpl = ct_ensure_template(
				cti, ctt_branch, cond, region
			);

			/* Recurse to the deeper regions. */
			ct_place_node(
				cti, irn, gc_get_cond_next(gc),
				tmpl->children[is_true ? 0 : 1]
			);
		}
		break;
	}
	case gct_while_true: {
		/* Create/get the loop for the node. */
		ir_node     *cond = gc_get_cond_irn(gc);
		ct_template *tmpl = ct_ensure_template(cti, ctt_loop, cond, region);

		/* Gamma may switch to the repeat region use header for now. */
		ct_place_node(cti, irn,	gc_get_cond_next(gc), tmpl->children[0]);
		break;
	}
	case gct_invalid: break;
	}
}

static void ct_pull_nodes(ct_info *cti, ir_node *irn, ct_region *region)
{
	gc_union_map_iter it_union_map;
	gc_union_iter     it_union;
	gc_entry          entry;

	/* Pull dominated nodes of irn to reg. */
	foreach_gc_union_map(cti->gci, irn, entry, it_union_map) {
		gc_cond *cond;

		if (gc_union_is_empty(entry.uni)) {
			ct_place_node(cti, entry.dst, NULL, region);
		} else {
			foreach_gc_union(entry.uni, cond, it_union) {
				/* Place the node in the region. */
				ct_place_node(cti, entry.dst, cond, region);
			}
		}
	}
}

static ct_info *ct_init(ir_graph *irg, gc_info *gci, pd_tree *pdt)
{
	ct_info *cti = XMALLOC(ct_info);
	obstack_init(&cti->obst);

	cti->phase = new_phase(irg, ct_init_node);
	phase_set_private(cti->phase, cti);

	cti->root    = ct_new_template(cti, ctt_root, pd_get_root(pdt), 1, NULL);
	cti->gci     = gci;
	cti->pdt     = pdt;
	cti->counter = 0;

	ct_pull_nodes(cti, cti->root->irn, cti->root->children[0]);

	return cti;
}

static void ct_free(ct_info *cti)
{
	phase_free(cti->phase);
	ct_free_template((ct_template*)cti->root);
	obstack_free(&cti->obst, NULL);
	xfree(cti);
}

static void ct_dump_indent(FILE *f, int indent)
{
	int i;
	for (i = 0; i < indent; i++) fprintf(f, "  ");
}

static void ct_dump_template(ct_info *cti, ct_template *tmpl, FILE *f,
                             int indent);

/* Collect all nodes in the given region. Horribly inefficient, but its for
 * dumping only, so its not important. */
static void ct_dump_collect(ct_info *cti, ct_region *region, ir_node *irn,
                            pset_new_t *set)
{
	int      i;
	ct_node *ctn;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	ctn = phase_get_irn_data(cti->phase, irn);

	if (ctn && plist_has_value(ctn->regions, region)) {
		pset_new_insert(set, irn);
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ct_dump_collect(cti, region, get_irn_n(irn, i), set);
	}
}

static void ct_dump_region(ct_info *cti, ct_region *region,
                           const char *name, FILE *f, int indent)
{
	pset_new_iterator_t it;

	pset_new_t   set;
	ir_node     *irn;
	int          first = 1;

	ct_dump_indent(f, indent);
	fprintf(f, "region %s {", name);
	fprintf(f, "\n");

	/* Find the nodes in the region. */
	pset_new_init(&set);
	inc_irg_visited(phase_get_irg(cti->phase));
	ct_dump_collect(cti, region, cti->root->irn, &set);

	ct_dump_indent(f, indent + 1); fprintf(f, "nodes {");
	foreach_pset_new(&set, ir_node*, irn, it) {
		if (!first) fprintf(f, ", ");
		fprintf(f, "%li", get_irn_node_nr(irn));
		first = 0;
	}
	fprintf(f, "}\n");

	pset_new_destroy(&set);

	if (plist_count(region->children)) {
		plist_element_t *it;
		fprintf(f, "\n");

		foreach_plist(region->children, it) {
			ct_dump_template(cti, it->data, f, indent + 1);
		}
	}

	ct_dump_indent(f, indent); fprintf(f, "}\n");
}

static void ct_dump_template(ct_info *cti, ct_template *tmpl, FILE *f,
                             int indent)
{
	ct_dump_indent(f, indent);
	fprintf(f, "template ");

	switch(tmpl->type) {
	case ctt_root: {
		fprintf(f, "root {\n");
		ct_dump_region(cti, tmpl->children[0], "graph",  f, indent + 1);
		break;
	}
	case ctt_branch: {
		fprintf(f, "branch {\n");
		ct_dump_region(cti, tmpl->children[0],  "true",  f, indent + 1);
		fprintf(f, "\n");
		ct_dump_region(cti, tmpl->children[1], "false", f, indent + 1);
		break;
	}
	case ctt_loop: {
		fprintf(f, "loop {\n");
		ct_dump_region(cti, tmpl->children[0], "header", f, indent + 1);
		fprintf(f, "\n");
		ct_dump_region(cti, tmpl->children[1], "repeat", f, indent + 1);
		break;
	}}

	ct_dump_indent(f, indent);
	fprintf(f, "}\n");
}

static void ct_dump(ct_info *cti, FILE *f)
{
	if (!cti->root->region) ct_dump_template(cti, cti->root, f, 0);
	else ct_dump_region(cti, cti->root->region, "root", f, 0);
}

static void ct_add_dep(ct_info *cti, ir_node *irn, ir_node *dep)
{
	int       n   = get_irn_arity(irn);
	ir_node **ins = OALLOCN(&cti->obst, ir_node*, n + 1);

	memcpy(ins, get_irn_in(irn) + 1, n * sizeof(ir_node*));
	ins[n] = dep;
	set_irn_in(irn, n + 1, ins);

	obstack_free(&cti->obst, ins);
}

static void ct_add_phidep(ct_info *cti, ir_node *irn, int idx, ir_node *dep)
{
	ir_node *old_dep = get_irn_n(irn, idx);

	if (is_Phi(old_dep)) {
		/* Append to the existing phi node. */
		ct_add_dep(cti, old_dep, dep);
	} else if (is_Unknown(old_dep)) {
		/* Overwrite unknown deps. */
		set_irn_n(irn, idx, dep);
	} else {
		/* Make a new phi node for the dep. */
		ir_node *block  = get_nodes_block(irn);
		ir_mode *mode   = get_irn_mode(dep);
		ir_node *ins[2] = { old_dep, dep };
		ir_node *phi    = new_r_Phi(block, 2, ins, mode);

		set_irn_n(irn, idx, phi);
		ct_transfer_regions(cti, irn, phi);
	}
}

static int ct_are_mutual_exclusive(ct_region *lhs, ct_region *rhs)
{
	/* Two regions are mutually exclusive, if running one of them implies
	 * that the other is not run. This is the case for the two regions of
	 * a branch and the nested regions. */

	/* Scan up until both regions have the same depth. */
	assert(lhs && rhs);
	while (lhs->depth > rhs->depth) lhs = lhs->template->region;
	while (rhs->depth > lhs->depth) rhs = rhs->template->region;

	assert(lhs && rhs && (lhs->depth == rhs->depth));
	while (lhs != rhs) {
		/* Same template and different region? */
		if (lhs->template == rhs->template) {
			/* If it is a branch, they are exclusive. */
			if (lhs->template->type == ctt_branch) {
				return 1;
			}
		}

		/* Go further upwards. */
		lhs = lhs->template->region;
		rhs = rhs->template->region;
		assert((!lhs && !rhs) || ((lhs && rhs) && (lhs->depth == rhs->depth)));
	}

	/* Same region and not in different branches. */
	return 0;
}

static void ct_duplicate_walk(ct_info *cti, ir_node *irn)
{
	int        i;
	ct_node   *ctn;
	ct_region *reg;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Visit deps and duplicate them them first. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		ct_duplicate_walk(cti, ir_dep);
	}

	/* Get region information about the node. */
	ctn = phase_get_or_set_irn_data(cti->phase, irn);

	/* Nodes without region info are considered to be in the root. */
	if (!plist_count(ctn->regions)) {
		plist_insert_back(ctn->regions, cti->root->children[0]);
	}

	/* Proj nodes need some special handling as they have to be put in their
	 * deps block (and will therefore be duplicated with it). */
	if (is_Proj(irn)) {
		ir_node *ir_dep = get_Proj_pred(irn);
		ct_node *ct_dep = phase_get_irn_data(cti->phase, ir_dep);
		assert(ct_dep);

		/* Force the same regions for the proj as for the dep. */
		plist_clear(ctn->regions);
		while (ct_dep) {
			ct_region *rg_dep = plist_first(ct_dep->regions)->data;
			plist_insert_back(ctn->regions, rg_dep);
			ct_dep = ct_dep->copy;
		}
	}

	assert(plist_count(ctn->regions) > 0);

	/* Found an unduplicated node? */
	if (plist_count(ctn->regions) > 1) {
		plist_element_t *it;
		ct_node *last = ctn;

		/* Create copies for every region. */
		it = plist_first(ctn->regions)->next;
		while (it) {
			plist_element_t *tmp;
			ct_node *ct_copy;
			ir_node *ir_copy;

			/* Make a copy, assign a region and link it. */
			ir_copy = exact_copy(irn);
			ct_copy = phase_get_or_set_irn_data(cti->phase, ir_copy);
			plist_insert_back(ct_copy->regions, it->data);

			last->copy = ct_copy;
			last = ct_copy;

			/* Erase the region from the original node. */
			tmp = it;
			it  = it->next;
			plist_erase(ctn->regions, tmp);
		}
	}

	assert(plist_count(ctn->regions) == 1);
	reg = plist_first(ctn->regions)->data;

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		ct_node *ct_dep = phase_get_irn_data(cti->phase, ir_dep);

		/* Fix the deps on all of the copies. */
		if (is_Proj(irn)) {
			/* Projs have the same regions and thus an 1:1 relationship. */
			ct_node *ct_src = ctn, *ct_dst = ct_dep;
			while (ct_src) {
				assert(ct_dst);
				set_Proj_pred(ct_src->irn, ct_dst->irn);
				ct_dst = ct_dst->copy;
				ct_src = ct_src->copy;
			}
		} else {
			ct_node *ct_src = ctn;
			while (ct_src) {
				/* Resolve multiple deps. */
				if (ct_dep && ct_dep->copy) {
					ir_graph *irg  = phase_get_irg(cti->phase);
					ir_mode  *mode = get_irn_mode(ir_dep);

					/* All copies of ir_dep that we can access should be added
					 * as dep with a phi node. We have to fix the node later.
					 * We can access all nodes that aren't kept in conflicting
					 * regions (like the other branch of a branch region). */

					/* Make the dep unknown at first. */
					set_irn_n(ct_src->irn, i, new_r_Unknown(irg, mode));

					/* Iterate the copies of dep. */
					while (ct_dep) {
						ct_region *rg_dep = plist_first(ct_dep->regions)->data;
						assert(plist_count(ct_dep->regions) == 1);

						/* Add all non-mutual-exclusive copies as dep. */
						if (!ct_are_mutual_exclusive(reg, rg_dep)) {
							ct_add_phidep(cti, ct_src->irn, i, ct_dep->irn);
						}

						ct_dep = ct_dep->copy;
					}
				}

				ct_src = ct_src->copy;
			}
		}
	}
}

static void ct_duplicate(ct_info *cti)
{
	inc_irg_visited(phase_get_irg(cti->phase));
	ct_duplicate_walk(cti, cti->root->irn);
}

static void ct_acpeg_to_ppeg_walk(ir_node *irn, void *ctx)
{
	ct_info *cti = ctx;

	if (is_Gamma(irn)) {
		/* Create a phi with the same preds. The cond is no longer needed. */
		ir_node *block = get_nodes_block(irn);
		ir_mode *mode  = get_irn_mode(irn);
		ir_node *phi   = new_r_Phi(block, 2, get_irn_in(irn) + 2, mode);
		ct_transfer_regions(cti, irn, phi);
		exchange(irn, phi);
	} else if (is_EtaA(irn)) {
		int      i;
		ir_node *block  = get_nodes_block(irn);
		ir_node *header = get_EtaA_header(irn);
		ir_node *repeat = get_EtaA_repeat(irn);
		ir_node *proxy;

		/* Exchange the theta proxies by phi nodes. */
		assert(get_irn_arity(header) == get_irn_arity(repeat));
		for (i = 0; i < get_irn_arity(header); i++) {
			proxy = get_irn_n(header, i);

			/* Proxies may be used by multiple loops. */
			if (is_Proxy(proxy)) {
				ir_mode *mode   = get_irn_mode(proxy);
				ir_node *ins[2] = { get_Proxy_value(proxy), get_irn_n(repeat, i) };
				ir_node *phi    = new_r_Phi(block, 2, ins, mode);
				ct_transfer_regions(cti, proxy, phi);
				exchange(proxy, phi);
			}
		}

		/* And the eta by a proxy of its result (the proxy prevents loop
		 * fusion, when a loop accesses the result of another loop with the
		 * some condition). */
		proxy = new_r_Proxy(block, get_EtaA_result(irn), get_irn_mode(irn));
		ct_transfer_regions(cti, irn, proxy);
		exchange(irn, proxy);
	}
}

static void ct_acpeg_to_ppeg(ct_info *cti)
{
	/* Phi in the PPEG (Phi PEG) means: select the predecessor that the control
	 * flow allows to be selected. It doesn't care about the indices or about
	 * the real blocks. The actual program structure is no longer contained in
	 * the graph, but in the region/template data structures.
	 * Further processing is needed, once the block structure is in place, in
	 * order to repair the phi nodes that are created here and by duplication.
	 * This is only possible because of the proxy nodes. */

	irg_walk_graph(phase_get_irg(cti->phase), ct_acpeg_to_ppeg_walk, NULL, cti);
}

static int ct_get_template_size(ct_template *tmpl)
{
	return tmpl->children[1] ? 2 : (tmpl->children[0] ? 1 : 0);
}

static int ct_region_includes(ct_region *lhs, ct_region *rhs)
{
	int i;
	plist_element_t *it;

	/* TODO: make this effective using indices. */
	if (lhs == rhs) return 1;

	foreach_plist(lhs->children, it) {
		ct_template *tmpl = it->data;

		for (i = 0; i < 2; i++) {
			if (!tmpl->children[i]) break;
			if (ct_region_includes(tmpl->children[i], rhs)) return 1;
		}
	}

	return 0;
}

static int ct_template_includes_region(ct_template *lhs, ct_region *rhs)
{
	int i;
	for (i = 0; i < 2; i++) {
		if (!lhs->children[i]) break;
		if (ct_region_includes(lhs->children[i], rhs)) return 1;
	}
	return 0;
}

static int ct_template_includes(ct_template *lhs, ct_template *rhs)
{
	return (lhs == rhs) || ct_template_includes_region(lhs, rhs->region);
}

static int ct_can_reach_walk(ct_info *cti, ct_template *lhs, ct_template *rhs)
{
	pset_new_iterator_t it;
	ct_template *dep;

	if (lhs->visited >= cti->counter) return 0;
	lhs->visited = cti->counter;

	if (lhs == rhs) return 1;

	/* Recurse along dependencies and not to the children. */
	foreach_pset_new(&lhs->deps, ct_template*, dep, it) {
		if (ct_can_reach_walk(cti, dep, rhs)) return 1;
	}

	return 0;
}

static int ct_can_reach(ct_info *cti, ct_template *lhs, ct_template *rhs)
{
	cti->counter++;
	return ct_can_reach_walk(cti, lhs, rhs);
}

static ct_template *ct_get_link(ct_template *tmpl)
{
	assert(plist_count(tmpl->links) == 1);
	return plist_first(tmpl->links)->data;
}

/* Retrieves the most specific "instance" above "link" that can be found below
 * "scope" and avoids cycles with "pred". This is the starting point for
 * construction or the required template, if it corresponds to "link". Note
 * that "pred" can be NULL, to turn of cycle detection. */
static ct_region *ct_find_region(ct_info *cti, ct_region *scope,
                                 ct_region *link, ct_template *pred)
{
	plist_element_t *it, *it_list;
	ct_template *next = link->template;
	plist_t     *list;

	/* If there is no "instance", we won't find anything. */
	if (!plist_count(link->template->links)) return NULL;

	list = plist_obstack_new(&cti->obst);
	assert(!pred || (scope == pred->region));

	/* Get the template below "scope" that contains "link". */
	while (next->region != scope->link) {
		next = next->region->template;
	}

	/* We have to find a template for "link" below an "instance" of "next".
	 * Scan the children of "scope" for such an "instance", that won't cycle
	 * back to "pred". Then search for templates below. */
	foreach_plist(scope->children, it) { /* Should be sorted old to new. */
		ct_template *child = it->data;

		if (ct_get_link(child) != next) continue;
		if (pred && ct_can_reach(cti, child, pred)) continue;

		/* Okay we can use this template. */
		plist_insert_back(list, child);
	}

	/* No suitable template found? */
	if (!plist_count(list)) {
		plist_free(list);
		obstack_free(&cti->obst, list);
		return NULL;
	}

	/* Now try to find the most specific template we can re-use. This has to
	 * terminate because "list" is not emty and eventually we will and up with
	 * a template from there. */
	while (1) {
		/* Scan the "instances" of "link->template". */
		foreach_plist(link->template->links, it) {
			/* Check if the template is below a valid template from "list". */
			foreach_plist(list, it_list) {
				/* If it is, we are done. */
				if (ct_template_includes(it_list->data, it->data)) {
					plist_free(list);
					obstack_free(&cti->obst, list);
					return ((ct_template*)it->data)->children[(int)link->index];
				}
			}
		}

		/* Iterate "link" from most to least specific region. */
		link = link->template->region;
	}

	assert(0);
	return NULL;
}

/* Build a region below "root" corresponding to "link". If "pred" is set, the
 * child template of root is added as dependency to "pred". Returns a list of
 * created templates in. */
static ct_region *ct_build_region(ct_info *cti, ct_region *root,
                                  ct_region *link, ct_template *pred,
                                  plist_t *todo)
{
	int          size, i;
	ct_template *tmpl;
	ct_region   *reg = root;

	/* Go up till we reach the root region and build the reduced template
	 * first, using it as the parent for the next one. */
	if (link->template->region != reg->link) {
		reg  = ct_build_region(cti, root, link->template->region, pred, todo);
		pred = NULL; /* No deps below the root level. */
	}

	/* Build the new template. */
	size = ct_get_template_size(link->template);
	tmpl = ct_new_template(
		cti, link->template->type, link->template->irn, size, reg
	);

	plist_insert_back(reg->children, tmpl);
	plist_insert_back(todo, tmpl);

	/* Add as dependency to pred if we are directly below the root and pred is
	 * set. This needs to be done if no template was re-used. */
	if (pred) pset_new_insert(&pred->deps, tmpl);

	/* Link the new template with link and vice-versa. */
	plist_insert_back(link->template->links, tmpl);
	plist_insert_back(tmpl->links, link->template);

	for (i = 0; i < size; i++) {
		tmpl->children[i]->link = link->template->children[i];
	}

	return tmpl->children[(int)link->index];
}

/* Gets a template for "link" in "scope", by trying to re-use the most specific
 * template that can be found and constructing the remaining templates on top
 * of that. The template is linked to "pred" (which shall reside in "scope"),
 * while cycles are avoided. If "pred" is NULL, no cycle detection or linking
 * takes place. */
static ct_region *ct_get_region(ct_info *cti, ct_region *scope,
                                ct_region *link, ct_template *pred,
                                plist_t *todo)
{
	/* Find the most specific template we can use first. If we get a template
	 * linked to "link", we are done already and can return directly. */
	ct_region *root = ct_find_region(cti, scope, link, pred);

	/* If not, we have to construct additional templates above root. Note
	 * that if we have found a template, we don't need to link it, thus we
	 * pass NULL as "pred". Otherwise let build link it to "pred". */
	if (root) {
		if (root->link == link) return root;
		return ct_build_region(cti, root, link, NULL, todo);
	}

	return ct_build_region(cti, scope, link, pred, todo);
}

static ct_region *ct_determine_region(ct_info *cti, ct_region *link,
                                      ct_template *pred, plist_t *todo,
                                      ir_node ***block)
{
	/* Find the template in the common region, that is the region that contains
	 * both, "link" and "pred". Note that construction of templates for "link"
	 * takes place inside of "pred->region". */
	ct_region *scope, *reg;

	while (!ct_region_includes(pred->region->link, link)) {
		pred = pred->region->template;
	}

	scope = pred->region;

	/* If "link" is a parent of "pred", use the block of "pred_tmpl", because
	 * the nodes have to be put in front of "pred", to satisly deps. */
	if (scope->link == link) {
		*block = &pred->block;
	}

	/* Loop regions may access each other. */
	if (pred->type == ctt_loop) {
	    if (ct_template_includes_region(ct_get_link(pred), link)) {
			/* Disable linking and cycle detection, change the scope to the
			 * proper region, to construct the templates there. */
			int index;
			index  = ct_region_includes(pred->children[0]->link, link) ? 0 : 1;
			scope  = pred->children[index];
			pred   = NULL;
			*block = &scope->block;
	    }
	}

	/* Scope may already be what we are looking for. */
	if (scope->link == link) {
		reg = scope;
	} else {
		/* Create a new region otherwise. */
		reg = ct_get_region(cti, scope, link, pred, todo);
		*block = &reg->block;
	}

	return reg;
}

static void ct_arrange_walk(ct_info *cti, ir_node *irn, ct_region *pred,
                            plist_t *todo, ir_node **block)
{
	int        i;
	ct_region *reg;
	ir_graph  *irg  = phase_get_irg(cti->phase);
	ct_region *next = pred;
	ct_node   *ctn;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Determine the new region. */
	ctn = phase_get_irn_data(cti->phase, irn);
	assert(ctn && plist_first(ctn->regions));
	reg = plist_first(ctn->regions)->data;

	if (reg != pred->link) {
		/* Determine the next block to use for storing nodes. */
		next = ct_determine_region(cti, reg, pred->template, todo, &block);
	}

	/* Create the block if needed. */
	if (!*block) {
		ir_graph *irg = phase_get_irg(cti->phase);
		ir_node  *ins[0];
		*block = new_r_Block(irg, 0, ins);
	}

	if (get_nodes_block(irn) != get_irg_start_block(irg)) {
		set_nodes_block(irn, *block);
	}

	plist_clear(ctn->regions);
	plist_insert_back(ctn->regions, next);

	/* Process added templates. */
	while (plist_count(todo)) {
		ct_template *tmpl = plist_first(todo)->data;
		plist_erase(todo, plist_first(todo));

		if ((tmpl->type == ctt_branch) || (tmpl->type == ctt_loop)) {
			/* Ensure that the condition nodes are visited. Simulate visiting
			 * from the first child region. For the branch it shouldn't matter,
			 * since the region will change, causing new region determination.
			 * For the loop this is the header, where the cond should actually
			 * be placed, so it should re-use the passed region. */
			ct_region *child = tmpl->children[0];
			ct_arrange_walk(cti, tmpl->irn, child, todo, &child->block);
		}
	}

	/* Recurse. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		ct_arrange_walk(cti, ir_dep, next, todo, block);
	}
}

static void ct_arrange(ct_info *cti)
{
	/* Create a new root region and template. Note that differently from the
	 * "old" graph, we may now have multiple "root" templates, so we need a
	 * "root" region to store them in. We only have to remember the base. */
	ir_graph    *irg = phase_get_irg(cti->phase);
	ir_node     *ret = cti->root->irn;
	ct_template *root;
	ct_region   *base;
	plist_t     *todo;

	cti->root->region = ct_new_region(cti, NULL, -1);
	plist_insert_back(cti->root->region->children, cti->root);

	root = ct_new_template(
		cti, ctt_root, cti->root->irn, 1, ct_new_region(cti, NULL, -1)
	);
	plist_insert_back(root->region->children, root);

	base = root->children[0];
	plist_insert_back(root->links, cti->root);
	plist_insert_back(cti->root->links, root);
	base->link         = cti->root->children[0];
	root->region->link = cti->root->region;

	/* Do the hard work. */
	todo = plist_new();
	inc_irg_visited(irg);
	ct_arrange_walk(cti, ret, base, todo, &base->block);
	plist_free(todo);

	/* Replace the old root. */
	cti->root = root;
}

static void ct_order_templates(ct_info *cti, ct_template *tmpl, plist_t *list)
{
	pset_new_iterator_t it;
	ct_template *dep;

	if (tmpl->visited >= cti->counter) return;
	tmpl->visited = cti->counter;

	/* Just fill the list post-order to obtain topological sorted order. */
	foreach_pset_new(&tmpl->deps, ct_template*, dep, it) {
		ct_order_templates(cti, dep, list);
	}

	plist_insert_front(list, tmpl);
}

static ir_node *ct_construct_region(ct_info *cti, ct_region *reg,
                                    ir_node *next);

static ir_node *ct_construct_template(ct_info *cti, ct_template *tmpl,
                                      ir_node *next)
{
	ir_graph *irg    = phase_get_irg(cti->phase);
	ir_node  *header = NULL;

	switch (tmpl->type) {
	case ctt_root: {
		/* Pretty simple, just recurse. */
		header = ct_construct_region(cti, tmpl->children[0], next);
		break;
	}
	case ctt_branch: {
		ir_node *ins[0];
		ir_node *btrue, *bfalse, *cond, *ptrue, *pfalse;

		/* Obtain both subgraphs, link them to next. */
		btrue  = ct_construct_region(cti, tmpl->children[0], next);
		bfalse = ct_construct_region(cti, tmpl->children[1], next);

		/* Now merge control flow it the header. */
		header = new_r_Block(irg, 0, ins);
		cond   = new_r_Cond(header, tmpl->irn);
		ptrue  = new_r_Proj(cond, mode_X, pn_Cond_true);
		pfalse = new_r_Proj(cond, mode_X, pn_Cond_false);

		/* Connect the subgraphs to the header. */
		ct_add_dep(cti, btrue,  ptrue);
		ct_add_dep(cti, bfalse, pfalse);
		break;
	}
	case ctt_loop: {
		ir_node *ins[0];
		ir_node *split, *bheader, *brepeat, *cond, *ptrue, *pfalse;

		/* Construct a split region behind the header and before the body. */
		split   = new_r_Block(irg, 0, ins);
		bheader = ct_construct_region(cti, tmpl->children[0], split);
		brepeat = ct_construct_region(cti, tmpl->children[1], bheader);

		/* Create the conditional branch. */
		cond   = new_r_Cond(split, tmpl->irn);
		ptrue  = new_r_Proj(cond, mode_X, pn_Cond_true);
		pfalse = new_r_Proj(cond, mode_X, pn_Cond_false);

		ct_add_dep(cti, next, ptrue);
		ct_add_dep(cti, brepeat, pfalse);
		header = bheader;
		break;
	}}

	/* If present, prepend the templates block. */
	if (tmpl->block) {
		ir_node *jmp = new_r_Jmp(tmpl->block);
		ct_add_dep(cti, header, jmp);
		header = tmpl->block;
	}

	assert(header);
	return header;
}

static ir_node *ct_construct_region(ct_info *cti, ct_region *reg, ir_node *next)
{
	plist_element_t *it;
	plist_t *list;

	/* The templates it the region for an acyclic graph with their deps.
	 * Do a topologic sort on them to get an evaluation order. Note that
	 * the order is reversed, because construction is done bottom-up. */
	list = plist_obstack_new(&cti->obst);

	cti->counter++;
	foreach_plist(reg->children, it) {
		ct_order_templates(cti, it->data, list);
	}

	/* "list" now contains the "toposorted" templates of the region. Chain
	 * the blocks of the subgraphs together. */
	foreach_plist(list, it) {
		next = ct_construct_template(cti, it->data, next);
	}

	plist_free(list);
	obstack_free(&cti->obst, list);

	/* Prepend the region block. */
	if (reg->block) {
		ir_node *jmp = new_r_Jmp(reg->block);
		ct_add_dep(cti, next, jmp);
		next = reg->block;
	}

	return next;
}

static void ct_construct_cfg(ct_info *cti)
{
	ir_graph *irg    = phase_get_irg(cti->phase);
	ir_node  *exec   = get_irg_initial_exec(irg);
	ir_node  *next   = get_irg_end_block(irg);
	ir_node  *ins[1] = { exec };

	next = ct_construct_region(cti, cti->root->region, next);
	set_irn_in(next, 1, ins);
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

void peg_to_cfg(ir_graph *irg)
{
	pd_tree *pdt;
	pl_info *pli;
	gc_info *gci;
	ct_info *cti;
	ir_node *ins[0];

	optimization_state_t opt;
	save_optimization_state(&opt);
	all_optimizations_off();

	/* Remove all keepalives. We shouldn't need them. */
	set_End_keepalives(get_irg_end(irg), 0, ins);

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
	pl_free(pli);

#ifdef LOG_GATING_CONDS
	printf("-----------------\n");
	printf("Gating conditions\n");
	printf("-----------------\n");
	gc_dump(gci, stdout);
#endif

	cti = ct_init(irg, gci, pdt);
	pd_free(pdt);

#ifdef LOG_CFG_REGIONS
	printf("-----------\n");
	printf("CFG Regions\n");
	printf("-----------\n");
	ct_dump(cti, stdout);
#endif

	//ct_acpeg_to_ppeg(cti);

	/*ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "ppeg");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);*/

	ct_duplicate(cti);

	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "dup");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

#ifdef LOG_CFG_REGIONS
	printf("-----------------------------\n");
	printf("CFG Regions after duplication\n");
	printf("-----------------------------\n");
	ct_dump(cti, stdout);
#endif

	ct_acpeg_to_ppeg(cti);
	dump_ir_graph(irg, "ppeg");

	ct_arrange(cti);

#ifdef LOG_CFG_REGIONS
	printf("---------------------------\n");
	printf("CFG Regions after arranging\n");
	printf("---------------------------\n");
	ct_dump(cti, stdout);
#endif

	ct_construct_cfg(cti);

//	inc_irg_visited(irg);
//	ct_arrange_graph(cti, cti->ret, cti->root);
//
//#ifdef LOG_CFG_REGIONS
//	printf("---------------------------\n");
//	printf("CFG Regions after arranging\n");
//	printf("---------------------------\n");
//	ct_dump(cti, stdout);
//#endif
//
//	ir_node *exec  = get_irg_initial_exec(irg);
//	ir_node *block = new_r_Block(irg, 1, &exec);
//
//	block = ct_build_template(irg, cti->root, block);
//	set_irn_in(get_irg_end_block(irg), 1, &cti->ret);

	dump_ir_graph(irg, "newcfg");

	ct_free(cti);
	gc_free(gci);

	restore_optimization_state(&opt);

	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);
}
