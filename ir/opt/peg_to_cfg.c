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
#include "pmap.h"
#include "pset_new.h"
#include "irflag.h"

#define LOG_DOMINATOR_TREE 1
#define LOG_LOOP_ANALYSIS  1
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

typedef struct ct_region ct_region;

/**
 * Templates give a structure to the tree. Each template contains several
 * regions, which in turn contain other templates. The container template
 * is used at the leaves, to store nodes.
 */
typedef struct ct_template {
	int        depth;
	ct_type    type;
	ct_region *parent;
	ir_node   *block;
} ct_template;

/* TODO: do we really need the node list? */
struct ct_region {
	ct_template *parent;
	pmap        *templates;
	plist_t     *collector;
	int          index; /* = min_index */
	int          max_index;
};

typedef struct ct_branch {
	ct_template  base;
	ir_node     *cond;
	ct_region   *rg_true;
	ct_region   *rg_false;
	plist_t     *gammas;
} ct_branch;

typedef struct ct_root {
	ct_template  base;
	ct_region   *graph;
} ct_root;

typedef struct ct_loop {
	ct_template  base;
	ir_node     *cond;
	ct_region   *header;
	ct_region   *repeat;
	plist_t     *etas;
} ct_loop;

typedef struct ct_node {
	plist_t *regions;
} ct_node;

typedef struct ct_info {
	obstack      obst;
	ir_node     *ret;
	ct_template *root;
	ir_phase    *phase;
	pset_new_t   shared; /* Shared nodes. */
} ct_info;

static ct_region *ct_new_region(ct_info *cti, ct_template *parent)
{
	ct_region *ctr = OALLOC(&cti->obst, ct_region);
	ctr->templates = pmap_create_ex(5);
	ctr->parent    = parent;
	ctr->collector = plist_obstack_new(&cti->obst);
	ctr->index     = 0;
	ctr->max_index = 0;
	return ctr;
}

/* Freeing the data again is a bit complex because of the maps. */
static void ct_free_template(ct_template *tmpl);

static void ct_free_region(ct_region *ctr)
{
	pmap_entry *entry;

	/* Free templates in the region first. */
	foreach_pmap(ctr->templates, entry) {
		ct_free_template(entry->value);
	}

	pmap_destroy(ctr->templates);
}

static void ct_free_template(ct_template *tmpl)
{
	/* Free all the regions in the template. */
	switch (tmpl->type) {
	case ctt_root: {
		ct_root *ctr = (ct_root*)tmpl;
		ct_free_region(ctr->graph);
		break;
	}
	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;
		ct_free_region(ctb->rg_true);
		ct_free_region(ctb->rg_false);
		break;
	}
	case ctt_loop: {
		ct_loop *ctl = (ct_loop*)tmpl;
		ct_free_region(ctl->header);
		ct_free_region(ctl->repeat);
		break;
	}}
}

/* Just for convenience. */
static ct_template *ct_new_root(ct_info *cti)
{
	ct_root *ctr = OALLOC(&cti->obst, ct_root);
	ctr->base.type   = ctt_root;
	ctr->base.parent = NULL;
	ctr->base.block  = NULL;
	ctr->base.depth  = 0;
	ctr->graph       = ct_new_region(cti, (ct_template*)ctr);
	return (ct_template*)ctr;
}

static ct_template *ct_new_branch(ct_info *cti, ir_node *cond, ct_region *par)
{
	ct_branch *ctb = OALLOC(&cti->obst, ct_branch);
	ctb->base.type   = ctt_branch;
	ctb->base.parent = par;
	ctb->base.block  = NULL;
	ctb->base.depth  = par->parent ? par->parent->depth + 1 : 0;
	ctb->cond        = cond;
	ctb->rg_true     = ct_new_region(cti, (ct_template*)ctb);
	ctb->rg_false    = ct_new_region(cti, (ct_template*)ctb);
	ctb->gammas      = plist_obstack_new(&cti->obst);
	return (ct_template*)ctb;
}

static ct_template *ct_new_loop(ct_info *cti, ir_node *cond, ct_region *par)
{
	ct_loop *ctl = OALLOC(&cti->obst, ct_loop);
	ctl->base.type   = ctt_loop;
	ctl->base.parent = par;
	ctl->base.block  = NULL;
	ctl->base.depth  = par->parent ? par->parent->depth + 1 : 0;
	ctl->cond        = cond;
	ctl->header      = ct_new_region(cti, (ct_template*)ctl);
	ctl->repeat      = ct_new_region(cti, (ct_template*)ctl);
	ctl->etas        = plist_obstack_new(&cti->obst);
	return (ct_template*)ctl;
}

/* Tests if rhs is below lhs. */
static int ct_region_below(ct_region *lhs, ct_region *rhs)
{
	return (rhs->index >= lhs->index) &&
	       (rhs->index <= lhs->max_index);
}

static int ct_insert_node(ct_info *cti, ir_node *irn, gc_cond *cond,
                          ct_region *reg, int counter)
{
	reg->index = counter;

	switch (gc_get_cond_type(cond)) {
	/* Do nothing on ignore. */
	case gct_ignore: break;

	/* Add the node to the current region and vice-versa. */
	case gct_demand: {
		ct_node *ctn = phase_get_or_set_irn_data(cti->phase, irn);
		plist_insert_back(ctn->regions, reg);

		/* Remember shared nodes. */
		if (plist_count(ctn->regions) > 1) {
			pset_new_insert(&cti->shared, irn);
		}
		break;
	}
	/* Branch on union to consider both conds. */
	case gct_union: {
		counter = ct_insert_node(
			cti, irn, gc_get_union_lhs(cond), reg, counter + 1
		);
		counter = ct_insert_node(
			cti, irn, gc_get_union_rhs(cond), reg, counter + 1
		);
		break;
	}
	/* On repeat, try to fetch an existing template in the region. */
	case gct_repeat: {
		ir_node *eta = gc_get_repeat_irn(cond);
		ct_loop *ctl = pmap_get(reg->templates, eta);

		/* Create a new template if necessary. */
		if (!ctl) {
			ir_node *cond = get_EtaA_cond(eta);
			ctl = (ct_loop*)ct_new_loop(cti, cond, reg);

			pmap_insert(reg->templates, eta, ctl);
			plist_insert_back(ctl->etas, eta);
		}
		assert(ctl->base.type == ctt_loop);

		counter = ct_insert_node(
			cti, irn, gc_get_repeat_cond(cond), ctl->header, counter + 1
		);
		break;
	}
	/* On branch, try to fetch an existing template in the region. */
	case gct_branch: {
		ir_node *node = gc_get_branch_irn(cond);

		/* Is this a loop-tail branch? */
		if (is_EtaA(node)) {
			ct_loop *ctl = (ct_loop*)reg->parent;
			assert(ctl->base.type == ctt_loop);
			assert(ctl->base.parent);

			/* Insert the only-once-executed part in the parent region. */
			counter = ct_insert_node(
				cti, irn, gc_get_branch_lhs(cond), ctl->base.parent, counter + 1
			);
			counter = ct_insert_node(
				cti, irn, gc_get_branch_rhs(cond), ctl->repeat, counter + 1
			);
		} else {
			ct_branch *ctb  = pmap_get(reg->templates, node);
			assert(is_Gamma(node));

			/* Create a new template if necessary. */
			if (!ctb) {
				ir_node *cond = get_Gamma_cond(node);
				ctb = (ct_branch*)ct_new_branch(cti, cond, reg);

				pmap_insert(reg->templates, node, ctb);
				plist_insert_back(ctb->gammas, node);
			}
			assert(ctb->base.type == ctt_branch);

			counter = ct_insert_node(
				cti, irn, gc_get_branch_lhs(cond), ctb->rg_true, counter + 1
			);
			counter = ct_insert_node(
				cti, irn, gc_get_branch_rhs(cond), ctb->rg_false, counter + 1
			);
		}
		break;
	}}

	reg->max_index = MAX(reg->max_index, counter);
	return counter;
}

static ct_template *ct_build_regions(ct_info *cti, gc_info *gci, ir_node *irn)
{
	ct_template *root  = ct_new_root(cti);
	ct_region   *reg   = ((ct_root*)root)->graph;
	ir_graph    *irg   = phase_get_irg(cti->phase);
	ir_node     *start = get_irg_start_block(irg);
	ir_node     *end   = get_irg_end_block(irg);
	gc_entry     entry;
	gc_iter      it;

	/* Insert all the nodes into the root region. */
	foreach_gc_conds(gci, irn, it, entry) {
		ir_node *block = get_nodes_block(entry.dst);

		/* Skip nodes in start and end block. */
		if ((block != start) && (block != end)) {
			ct_insert_node(cti, entry.dst, entry.cond, reg, 0);
		}
	}

	/* Insert the return node manually. It has no condition. */
	ct_insert_node(cti, irn, gc_get_demand(), reg, 0);

	return root;
}

static void ct_duplicate_node(obstack *obst, ct_info *cti, ir_node *irn)
{
	const ir_edge_t *edge, *tmp;
	int i;

	plist_element_t *it;
	ir_node   **copies;
	ct_region **regions;

	ct_node  *ctn   = phase_get_irn_data(cti->phase, irn);
	int       count = plist_count(ctn->regions);
	if (count <= 1) return;

	/* First create n copies. */
	copies  = OALLOCN(obst, ir_node*,   count);
	regions = OALLOCN(obst, ct_region*, count);

	/* The known node will be associated with the first region. */
	it = plist_first(ctn->regions);
	copies[0]  = irn;
	regions[0] = it->data;
	it = it->next;

	for (i = 1; i < count; i++) {
		plist_element_t *tmp;
		ct_node *ct_copy;
		assert(it);

		/* Create a copy for any further regions. */
		copies[i]  = exact_copy(irn);
		regions[i] = it->data;

		/* Store the copies region in its data. */
		ct_copy = phase_get_or_set_irn_data(cti->phase, copies[i]);
		plist_insert_front(ct_copy->regions, regions[i]);

		/* Remove the region from the region list. */
		tmp = it; it = it->next;
		plist_erase(ctn->regions, tmp);
	}

	assert(plist_count(ctn->regions) == 1);

	/* Duplicate users where needed. */
	foreach_out_edge(irn, edge) {
		ct_duplicate_node(obst, cti, get_edge_src_irn(edge));
	}

	foreach_out_edge_safe(irn, edge, tmp) {
		/**
		 * Depending on where the user is, select a copy to use.
		 *
		 * 1. In the parent region (eta or gamma nodes).
		 * 2. At the nodes region or in its region subtree.
		 */
		ir_node   *ir_user = get_edge_src_irn(edge);
		ct_node   *ct_user = phase_get_irn_data(cti->phase, ir_user);
		ct_region *rg_user = plist_first(ct_user->regions)->data;
		ir_node   *ir_copy = NULL;
		assert(ct_user && (plist_count(ct_user->regions) == 1));

		/* Test the regions that we have copies for. */
		for (i = 0; i < count; i++) {
			ct_region *rg_copy = regions[i];

			/* Is the user_reg in the copy_reg context? */
			if (ct_region_below(rg_copy, rg_user) ||
			    (rg_user == rg_copy->parent->parent)) {

				assert(!ir_copy);
				ir_copy = copies[i];
				// break;
			}
		}

		assert(ir_copy);

		/* Now we know which copy the user uses. And we can redirect the
		 * edge to that copy and continue with the next. */

		i = get_edge_src_pos(edge);
		set_irn_n(ir_user, i, ir_copy);
	}

	obstack_free(obst, regions);
	obstack_free(obst, copies);
}

static void ct_duplicate(ct_info *cti)
{
	pset_new_iterator_t it;
	ir_node *irn;
	obstack  obst;

	obstack_init(&obst);

	foreach_pset_new(&cti->shared, irn, it) {
		ct_duplicate_node(&obst, cti, irn);
	}

	/* Clear the duplicate node list. */
	pset_new_destroy(&cti->shared);
	pset_new_init(&cti->shared);

	obstack_free(&obst, NULL);
}

static void *ct_init_node(ir_phase *phase, const ir_node *irn)
{
	ct_info *cti = phase_get_private(phase);
	ct_node *ctn = OALLOC(&cti->obst, ct_node);

	ctn->regions = plist_obstack_new(&cti->obst);

	(void)irn;
	return ctn;
}

static ct_info *ct_init(ir_graph *irg, gc_info *gci, pd_tree *pdt)
{
	ct_info *cti = XMALLOC(ct_info);
	obstack_init(&cti->obst);

	cti->phase = new_phase(irg, ct_init_node);
	phase_set_private(cti->phase, cti);

	pset_new_init(&cti->shared);
	cti->root = ct_build_regions(cti, gci, pd_get_root(pdt));
	cti->ret  = pd_get_root(pdt);

	return cti;
}

static void ct_free(ct_info *cti)
{
	pset_new_destroy(&cti->shared);
	phase_free(cti->phase);
	ct_free_template(cti->root);
	obstack_free(&cti->obst, NULL);
	xfree(cti);
}

/* Ensure that the template is marked in the parent regions collector. It
 * ensures, that all nodes currently collected in the parent region are
 * evaluated before the template is. */
static void ct_ensure_marker(ct_template *tmpl)
{
	if (!tmpl->parent) return;

	if ((plist_count(tmpl->parent->collector) == 0) ||
	    (plist_last(tmpl->parent->collector)->data != tmpl)) {

		plist_insert_back(tmpl->parent->collector, NULL);
		plist_insert_back(tmpl->parent->collector, tmpl);
	}
}

static void ct_arrange_graph(ct_info *cti, ir_node *irn)
{
	int          i;
	ct_node     *ctn;
	ct_region   *current = NULL;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	ctn = phase_get_or_set_irn_data(cti->phase, irn);

	/* Get the nodes region and template (duplication required!). */
	assert(plist_count(ctn->regions) <= 1);

	/* Some nodes may not be in any region (start and end block). */
	if (plist_count(ctn->regions) == 1) {
		current = plist_first(ctn->regions)->data;
		assert(current);
	}

	if (is_Gamma(irn)) {
		/* Gamma node handling. */
		ct_template *tmpl;

		assert(current);
		tmpl = pmap_find(current->templates, irn)->value;

		/* Set a marker after the condition, to put the condition before the
		 * branch template, instead of putting it behind. */
		ct_arrange_graph(cti, get_Gamma_cond(irn));
		ct_ensure_marker(tmpl);

		ct_arrange_graph(cti, get_Gamma_true(irn));
		ct_arrange_graph(cti, get_Gamma_false(irn));
	} else {
		/* Recurse to all deps. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *dep = get_irn_n(irn, i);
			ct_arrange_graph(cti, dep);
		}
	}

	if (current) {
		/* Collect the node in the current region. */
		plist_insert_back(current->collector, irn);

		/* Set the template marker to ensure execution of all collected nodes
		 * in the parent region before this template. Post-order ensures that
		 * all the deps are placed there before the last marker is set. */
		ct_ensure_marker(current->parent);
	}
}

static ir_node *ct_build_template(ir_graph *irg, ct_template *tmpl,
                                  ir_node *start);

static ir_node *ct_build_region(ir_graph *irg, ct_region *reg, ir_node *start)
{
	plist_element_t *it;
	ir_node *block = start;

	foreach_plist(reg->collector, it) {
		/* Move nodes to the current block. */
		if (it->data) {
			set_nodes_block(it->data, block);
		} else {
			/* For templates, embed a new CFG. */
			ct_template *tmpl = it->next->data;
			it = it->next;

			/* The template build function will return a new block to use. */
			block = ct_build_template(irg, tmpl, block);
		}
	}

	return block;
}

static ir_node *ct_build_template(ir_graph *irg, ct_template *tmpl,
                                  ir_node *start)
{
	switch (tmpl->type) {
	case ctt_root: {
		ct_root *ctr = (ct_root*)tmpl;
		return ct_build_region(irg, ctr->graph, start);
	}
	case ctt_branch: {
		plist_element_t *it;
		ct_branch *ctb = (ct_branch*)tmpl;

		/* Add the cond node and projs to the given start block. */
		ir_node *cond     = new_r_Cond(start, ctb->cond);
		ir_node *ir_true  = new_r_Proj(cond, mode_X, pn_Cond_true);
		ir_node *ir_false = new_r_Proj(cond, mode_X, pn_Cond_false);

		/* Make two blocks for the branches. */
		ir_node *bl_true  = new_r_Block(irg, 1, &ir_true);
		ir_node *bl_false = new_r_Block(irg, 1, &ir_false);

		/* Build the regions CFGs and get the end blocks. */
		ir_node *bl_true_end  = ct_build_region(irg, ctb->rg_true,  bl_true);
		ir_node *bl_false_end = ct_build_region(irg, ctb->rg_false, bl_false);

		/* Join control flow in a new return block. */
		ir_node *jumps[2] = { new_r_Jmp(bl_true_end), new_r_Jmp(bl_false_end) };
		ir_node *bl_join  = new_r_Block(irg, 2, jumps);

		/* Replace associated gamma nodes by phis. */
		foreach_plist(ctb->gammas, it) {
			ir_node *gamma  = it->data;
			ir_node *ins[2] = { get_Gamma_true(gamma), get_Gamma_false(gamma) };
			ir_node *phi    = new_r_Phi(bl_join, 2, ins, get_irn_mode(gamma));
			exchange(gamma, phi);
		}

		return bl_join;
	}}

	assert(0);
	return NULL;
}

static void ct_dump_indent(FILE *f, int indent)
{
	int i;
	for (i = 0; i < indent; i++) fprintf(f, "  ");
}

static void ct_dump_template(ct_info *cti, ct_template *tmpl, FILE *f,
                             int indent);

static void ct_dump_template_name(ct_template *tmpl, FILE *f)
{
	plist_element_t *it;
	int first = 1;

	switch(tmpl->type) {
	case ctt_root: {
		fprintf(f, "root");
		break;
	}
	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;

		fprintf(f, "branch(");
		foreach_plist(ctb->gammas, it) {
			if (!first) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(it->data));
			first = 0;
		}
		fprintf(f, ")");
		break;
	}
	case ctt_loop: {
		ct_loop *ctl = (ct_loop*)tmpl;

		fprintf(f, "loop(");
		foreach_plist(ctl->etas, it) {
			if (!first) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(it->data));
			first = 0;
		}
		fprintf(f, ")");
		break;
	}}
}

/* Collect all nodes in the given region. Horribly inefficient, but its for
 * dumping only, so its not important. */
static void ct_dump_collect(ct_info *cti, ct_region *reg, ir_node *irn,
                            pset_new_t *set)
{
	int      i;
	ct_node *ctn;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	ctn = phase_get_irn_data(cti->phase, irn);

	if (ctn && plist_has_value(ctn->regions, reg)) {
		pset_new_insert(set, irn);
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ct_dump_collect(cti, reg, get_irn_n(irn, i), set);
	}
}

static void ct_dump_region(ct_info *cti, ct_region *reg, const char *name,
                           FILE *f, int indent)
{
	pset_new_iterator_t it;

	pset_new_t   set;
	pmap_entry  *entry;
	ct_template *tmpl;
	ir_node     *irn;
	int          first = 1;

	ct_dump_indent(f, indent);
	fprintf(f, "region %s {", name);
	fprintf(f, "\n");

	/* Try to find the root template. */
	tmpl = reg->parent;
	while (tmpl->type != ctt_root) {
		tmpl = tmpl->parent->parent;
	}

	/* Find the nodes in the region. */
	pset_new_init(&set);
	inc_irg_visited(phase_get_irg(cti->phase));
	ct_dump_collect(cti, reg, cti->ret, &set);

	ct_dump_indent(f, indent + 1); fprintf(f, "nodes {");
	foreach_pset_new(&set, irn, it) {
		if (!first) fprintf(f, ", ");
		fprintf(f, "%li", get_irn_node_nr(irn));
		first = 0;
	}
	fprintf(f, "}\n");

	pset_new_destroy(&set);

	/* Output the ordered nodes (if any). */
	if (plist_count(reg->collector) > 0) {
		plist_element_t *it2;

		first = 1;
		ct_dump_indent(f, indent + 1); fprintf(f, "order {");
		foreach_plist(reg->collector, it2) {
			if (!first) fprintf(f, ", ");

			if (it2->data) {
				fprintf(f, "%li", get_irn_node_nr(it2->data));
			} else {
				it2 = it2->next;
				ct_dump_template_name(it2->data, f);
			}

			first = 0;
		}
		fprintf(f, "}\n");
	}

	if (pmap_count(reg->templates) > 0) {
		fprintf(f, "\n");
		foreach_pmap(reg->templates, entry) {
			ct_dump_template(cti, entry->value, f, indent + 1);
		}
	}

	ct_dump_indent(f, indent); fprintf(f, "}\n");
}

static void ct_dump_template(ct_info *cti, ct_template *tmpl, FILE *f,
                             int indent)
{
	ct_dump_indent(f, indent);
	fprintf(f, "template ");
	ct_dump_template_name(tmpl, f);

	fprintf(f, " {\n");
	switch(tmpl->type) {
	case ctt_root: {
		ct_root *ctr = (ct_root*)tmpl;
		ct_dump_region(cti, ctr->graph, "graph",  f, indent + 1);
		break;
	}
	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;
		ct_dump_region(cti, ctb->rg_true,  "true",  f, indent + 1);
		fprintf(f, "\n");
		ct_dump_region(cti, ctb->rg_false, "false", f, indent + 1);
		break;
	}
	case ctt_loop: {
		ct_loop *ctl = (ct_loop*)tmpl;
		ct_dump_region(cti, ctl->header, "header", f, indent + 1);
		fprintf(f, "\n");
		ct_dump_region(cti, ctl->repeat, "repeat", f, indent + 1);
		break;
	}}

	ct_dump_indent(f, indent); fprintf(f, "}\n");
}

static void ct_dump(ct_info *cti, FILE *f)
{
	pset_new_iterator_t it;
	char     first = 1;
	ir_node *irn;

	ct_dump_template(cti, cti->root, f, 0);

	if (pset_new_size(&cti->shared) > 0) {
		fprintf(f, "duplicate nodes {\n");
		ct_dump_indent(f, 1);

		foreach_pset_new(&cti->shared, irn, it) {
			if (!first) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(irn));
			first = 0;
		}

		fprintf(f, "\n}\n");
	}
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

/**
 * 1. Unnütze Join-Blocks etc. weglassen.
 * 2. Gamma-Knoten durch Phi ersetzen (sollte dann direkt gehen).
 * 3. Ausführbarer Code?
 */

void peg_to_cfg(ir_graph *irg)
{
	pd_tree *pdt;
	pl_info *pli;
	gc_info *gci;
	ct_info *cti;
	int edge_state;

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

#ifdef LOG_CFG_REGIONS
	printf("-----------\n");
	printf("CFG Regions\n");
	printf("-----------\n");
	ct_dump(cti, stdout);
#endif

	edge_state = edges_assure(irg);
	ct_duplicate(cti);
	if (!edge_state) edges_deactivate(irg);

	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "dup");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

#ifdef LOG_CFG_REGIONS
	printf("-----------------------------\n");
	printf("CFG Regions after duplication\n");
	printf("-----------------------------\n");
	ct_dump(cti, stdout);
#endif

	int opt = get_optimize();
	set_optimize(0);

	inc_irg_visited(irg);
	ct_arrange_graph(cti, cti->ret);

#ifdef LOG_CFG_REGIONS
	printf("---------------------------\n");
	printf("CFG Regions after arranging\n");
	printf("---------------------------\n");
	ct_dump(cti, stdout);
#endif

	ir_node *exec  = get_irg_initial_exec(irg);
	ir_node *block = new_r_Block(irg, 1, &exec);

	block = ct_build_template(irg, cti->root, block);
	set_irn_in(get_irg_end_block(irg), 1, &cti->ret);

	set_optimize(opt);

	ct_free(cti);
	gc_free(gci);
	pd_free(pdt);

	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);

	dump_ir_graph(irg, "newcfg");
}
