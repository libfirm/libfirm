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

#define LOG_DOMINATOR_TREE 1
#define LOG_LOOP_ANALYSIS  1
#define LOG_GATING_CONDS   1
#define LOG_CFG_REGIONS    1

/******************************************************************************
 * CFG template creation.                                                     *
 ******************************************************************************/

typedef enum ct_type {
	ctt_block,
	ctt_branch,
	ctt_loop
} ct_type;

/**
 * Templates give a structure to the tree. Each template contains several
 * regions, which in turn contain other templates. The container template
 * is used at the leaves, to store nodes.
 */
typedef struct ct_template {
	ct_type type;
} ct_template;

typedef struct ct_region {
	ct_template *parent;
	plist_t     *children; /* Allows specifying an order. */
	pmap        *lookup;   /* Used for fast construction. */
} ct_region;

typedef struct ct_block {
	ct_template  base;
	plist_t     *nodes;
} ct_block;

typedef struct ct_branch {
	ct_template  base;
	ir_node     *cond;
	ct_region   *rtrue;
	ct_region   *rfalse;
} ct_branch;

typedef struct ct_loop {
	ct_template  base;
	ir_node     *cond;
	ct_region   *header;
	ct_region   *repeat;
	ct_region   *leave;
} ct_loop;

typedef struct ct_info {
	struct obstack obst;
	ct_region *root;
} ct_info;

static ct_region *ct_new_region(ct_info *cti, ct_template *parent)
{
	ct_region *ctr = OALLOC(&cti->obst, ct_region);
	ctr->children = plist_obstack_new(&cti->obst);
	ctr->lookup   = pmap_create_ex(5);
	ctr->parent   = parent;
	return ctr;
}

/* Freeing the data again is a bit complex because of the maps. */
static void ct_free_template(ct_template *tmpl);

static void ct_free_region(ct_region *ctr)
{
	plist_element_t *it;

	/* Free templates in the region first. */
	foreach_plist(ctr->children, it) {
		ct_free_template(it->data);
	}

	pmap_destroy(ctr->lookup);
}

static void ct_free_template(ct_template *tmpl)
{
	/* Free all the regions in the template. */
	switch (tmpl->type) {
	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;
		ct_free_region(ctb->rtrue);
		ct_free_region(ctb->rfalse);
		break;
	}
	case ctt_loop: {
		ct_loop *ctr = (ct_loop*)tmpl;
		ct_free_region(ctr->header);
		ct_free_region(ctr->repeat);
		ct_free_region(ctr->leave);
		break;
	}
	/* Nothing to free for block templates. */
	case ctt_block: break;
	}
}

/* Just for convenience. */
static ct_template *ct_new_branch(ct_info *cti, ir_node *cond)
{
	ct_branch *ctb = OALLOC(&cti->obst, ct_branch);
	ctb->base.type = ctt_branch;
	ctb->cond   = cond;
	ctb->rtrue  = ct_new_region(cti, (ct_template*)ctb);
	ctb->rfalse = ct_new_region(cti, (ct_template*)ctb);
	return (ct_template*)ctb;
}

static ct_template *ct_new_loop(ct_info *cti, ir_node *cond)
{
	ct_loop *ctl = OALLOC(&cti->obst, ct_loop);
	ctl->base.type = ctt_loop;
	ctl->cond   = cond;
	ctl->header = ct_new_region(cti, (ct_template*)ctl);
	ctl->repeat = ct_new_region(cti, (ct_template*)ctl);
	ctl->leave  = ct_new_region(cti, (ct_template*)ctl);
	return (ct_template*)ctl;
}

static ct_template *ct_new_block(ct_info *cti)
{
	ct_block *ctb = OALLOC(&cti->obst, ct_block);
	ctb->base.type = ctt_block;
	ctb->nodes     = plist_obstack_new(&cti->obst);
	return (ct_template*)ctb;
}

static void ct_insert_node(ct_info *cti, ir_node *irn,
                           gc_cond *cond, ct_region *reg)
{
	switch (gc_get_cond_type(cond)) {
	/* Do nothing on ignore. */
	case gct_ignore: break;

	/* Add the node to the current region. */
	case gct_demand: {
		ct_block *block = NULL;

		/* Use a block as first child, to store nodes for now. */
		if (plist_count(reg->children) > 0) {
			ct_template *tmpl = plist_first(reg->children)->data;
			if (tmpl->type == ctt_block) {
				block = (ct_block*)tmpl;
			}
		}

		if (!block) {
			block = (ct_block*)ct_new_block(cti);
			plist_insert_front(reg->children, block);
		}

		plist_insert_back(block->nodes, irn);
		break;
	}
	/* Branch on union to consider both conds. */
	case gct_union: {
		ct_insert_node(cti, irn, gc_get_union_lhs(cond), reg);
		ct_insert_node(cti, irn, gc_get_union_rhs(cond), reg);
		break;
	}
	/* On repeat, try to fetch an existing template in the region. */
	case gct_repeat: {
		ir_node *eta = gc_get_repeat_irn(cond);
		ct_loop *ctl = pmap_get(reg->lookup, eta);

		/* Create a new template if necessary. */
		if (!ctl) {
			ctl = (ct_loop*)ct_new_loop(cti, eta);
			plist_insert_back(reg->children, ctl);
			pmap_insert(reg->lookup, eta, ctl);
		}
		assert(ctl->base.type == ctt_loop);

		ct_insert_node(cti, irn, gc_get_repeat_cond(cond), ctl->header);
		break;
	}
	/* On branch, try to fetch an existing template in the region. */
	case gct_branch: {
		ir_node *node = gc_get_branch_irn(cond);

		/* Is this a loop-tail branch? */
		if (is_EtaA(node)) {
			ct_loop *ctl = (ct_loop*)reg->parent;
			assert(ctl->base.type == ctt_loop);

			ct_insert_node(cti, irn, gc_get_branch_lhs(cond), ctl->leave);
			ct_insert_node(cti, irn, gc_get_branch_rhs(cond), ctl->repeat);
		} else {
			ct_branch *ctb = pmap_get(reg->lookup, node);

			/* Create a new template if necessary. */
			if (!ctb) {
				ctb = (ct_branch*)ct_new_branch(cti, node);
				plist_insert_back(reg->children, ctb);
				pmap_insert(reg->lookup, node, ctb);
			}
			assert(ctb->base.type == ctt_branch);

			ct_insert_node(cti, irn, gc_get_branch_lhs(cond), ctb->rtrue);
			ct_insert_node(cti, irn, gc_get_branch_rhs(cond), ctb->rfalse);
		}
		break;
	}}
}

static ct_region *ct_build_regions(ct_info *cti, gc_info *gci, ir_node *irn)
{
	ct_region *root = ct_new_region(cti, NULL);
	gc_entry   entry;
	gc_iter    it;

	/* Insert all the nodes into the root region. */
	foreach_gc_conds(gci, irn, it, entry) {
		ct_insert_node(cti, entry.dst, entry.cond, root);
	}

	return root;
}

static void ct_dump_indent(FILE *f, int indent)
{
	int i;
	for (i = 0; i < indent; i++) fprintf(f, "  ");
}

static void ct_dump_template(ct_template *tmpl, FILE *f, int indent);

static void ct_dump_region(ct_region *reg, const char *name, FILE *f, int indent)
{
	plist_element_t *it;
	ct_dump_indent(f, indent); fprintf(f, "[%s] {", name);

	if (plist_count(reg->children) > 0) {
		fprintf(f, "\n");

		foreach_plist(reg->children, it) {
			ct_dump_template(it->data, f, indent + 1);
		}

		ct_dump_indent(f, indent);
	}

	fprintf(f, "}\n");
}

static void ct_dump_template(ct_template *tmpl, FILE *f, int indent)
{
	ct_dump_indent(f, indent);

	switch(tmpl->type) {
	case ctt_block: {
		plist_element_t *it;
		ct_block *ctb = (ct_block*)tmpl;

		fprintf(f, "block {");
		foreach_plist(ctb->nodes, it) {
			if (it != plist_first(ctb->nodes)) fprintf(f, ", ");
			fprintf(f, "%li", get_irn_node_nr(it->data));
		}
		fprintf(f, "}\n");
		break;
	}
	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;
		fprintf(f, "branch (%li) {\n", get_irn_node_nr(ctb->cond));
		ct_dump_region(ctb->rtrue,  "true",  f, indent + 1);
		ct_dump_region(ctb->rfalse, "false", f, indent + 1);
		ct_dump_indent(f, indent); fprintf(f, "}\n");
		break;
	}
	case ctt_loop: {
		ct_loop *ctl = (ct_loop*)tmpl;
		fprintf(f, "loop (%li) {\n", get_irn_node_nr(ctl->cond));
		ct_dump_region(ctl->header, "header", f, indent + 1);
		ct_dump_region(ctl->repeat, "repeat", f, indent + 1);
		ct_dump_region(ctl->leave,  "leave",  f, indent + 1);
		ct_dump_indent(f, indent); fprintf(f, "}\n");
		break;
	}}
}

static void ct_dump(ct_info *cti, FILE *f)
{
	ct_dump_region(cti->root, "root", f, 0);
}

static ct_info *ct_init(gc_info *gci, pd_tree *pdt)
{
	ct_info *cti = XMALLOC(ct_info);
	obstack_init(&cti->obst);

	cti->root = ct_build_regions(cti, gci, pd_get_root(pdt));
	return cti;
}

static void ct_free(ct_info *cti)
{
	ct_free_region(cti->root);
	obstack_free(&cti->obst, NULL);
	xfree(cti);
}

/******************************************************************************
 * Public interface.                                                          *
 ******************************************************************************/

/**
 * 1. Phase draus machen
 * 2. Reverse-Kanten für Regionen (evtl. nur das?)
 * 3. Kanten zwischen Regionen aufbauen
 *   3.1. Setze Blockmarker (ähnlich visited? +1, +2, +3, ...)
 *   3.2. Iteriere Subgraphen
 * 4. Topologische Sortierung
 *   4.1 Gleichzeitiger Aufbau von Containerblöcken
 */

void peg_to_cfg(ir_graph *irg)
{
	pd_tree *pdt;
	pl_info *pli;
	gc_info *gci;
	ct_info *cti;

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

	cti = ct_init(gci, pdt);

#ifdef LOG_CFG_REGIONS
	printf("-----------\n");
	printf("CFG Regions\n");
	printf("-----------\n");
	ct_dump(cti, stdout);
#endif

	ct_free(cti);
	gc_free(gci);
	pd_free(pdt);
}
