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
#include "pmap_new.h"
#include "pset_new.h"
#include "irflag.h"

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

typedef struct ct_region ct_region;

/**
 * Templates give a structure to the tree. Each template contains several
 * regions, which in turn contain other templates. The container template
 * is used at the leaves, to store nodes.
 */
typedef struct ct_template {
	ct_type    type;
	ct_region *parent;
} ct_template;

struct ct_region {
	ct_template *parent;
	plist_t     *templates;
	pmap_new_t   branches, loops;
};

typedef struct ct_branch {
	ct_template  base;
	ir_node     *cond;
	ct_region   *rg_true;
	ct_region   *rg_false;
} ct_branch;

typedef struct ct_root {
	ct_template  base;
	ir_node     *irn;
	ct_region   *reg;
} ct_root;

typedef struct ct_loop {
	ct_template  base;
	ir_node     *cond;
	ct_region   *header;
	ct_region   *repeat;
} ct_loop;

typedef struct ct_node {
	plist_t *regions;
	ir_node *irn;
	struct ct_node *copy;
} ct_node;

typedef struct ct_info {
	obstack   obst;
	gc_info  *gci;
	pd_tree  *pdt;
	ct_root  *root;
	ir_phase *phase;
} ct_info;

static ct_region *ct_new_region(ct_info *cti, ct_template *parent)
{
	ct_region *reg = OALLOC(&cti->obst, ct_region);
	reg->parent    = parent;
	reg->templates = plist_obstack_new(&cti->obst);

	pmap_new_init(&reg->branches);
	pmap_new_init(&reg->loops);
	return reg;
}

/* Freeing the data again is a bit complex because of the maps. */
static void ct_free_template(ct_template *tmpl);

static void ct_free_region(ct_region *reg)
{
	plist_element_t *it;

	/* Free templates in the region first. */
	foreach_plist(reg->templates, it) {
		ct_free_template(it->data);
	}

	pmap_new_destroy(&reg->branches);
	pmap_new_destroy(&reg->loops);
}

static void ct_free_template(ct_template *tmpl)
{
	/* Free all the regions in the template. */
	switch (tmpl->type) {
	case ctt_root: {
		ct_root *ctr = (ct_root*)tmpl;
		ct_free_region(ctr->reg);
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
static ct_root *ct_new_root(ct_info *cti, ir_node *irn)
{
	ct_root *ctr = OALLOC(&cti->obst, ct_root);
	assert(is_Return(irn));
	ctr->base.type   = ctt_root;
	ctr->base.parent = NULL;
	ctr->reg         = ct_new_region(cti, (ct_template*)ctr);
	ctr->irn         = irn;
	return ctr;
}

static ct_branch *ct_new_branch(ct_info *cti, ir_node *cond, ct_region *par)
{
	ct_branch *ctb = OALLOC(&cti->obst, ct_branch);
	ctb->base.type   = ctt_branch;
	ctb->base.parent = par;
	ctb->cond        = cond;
	ctb->rg_true     = ct_new_region(cti, (ct_template*)ctb);
	ctb->rg_false    = ct_new_region(cti, (ct_template*)ctb);
	return ctb;
}

static ct_loop *ct_new_loop(ct_info *cti, ir_node *cond, ct_region *par)
{
	ct_loop *ctl = OALLOC(&cti->obst, ct_loop);
	ctl->base.type   = ctt_loop;
	ctl->base.parent = par;
	ctl->cond        = cond;
	ctl->header      = ct_new_region(cti, (ct_template*)ctl);
	ctl->repeat      = ct_new_region(cti, (ct_template*)ctl);
	return ctl;
}

static void ct_dump_template_name(ct_template *tmpl, FILE *f);

static void *ct_init_node(ir_phase *phase, const ir_node *irn)
{
	ct_info *cti = phase_get_private(phase);
	ct_node *ctn = OALLOC(&cti->obst, ct_node);

	ctn->regions = plist_obstack_new(&cti->obst);
	ctn->copy    = NULL;
	ctn->irn     = (ir_node*)irn;

	return ctn;
}

static ct_branch *ct_ensure_branch(ct_info *cti, ct_region *reg, ir_node *cond)
{
	ct_branch *res = pmap_new_get(&reg->branches, cond);
	if (!res) {
		res = ct_new_branch(cti, cond, reg);
		plist_insert_back(reg->templates, res);
		pmap_new_insert(&reg->branches, cond, res);
	}
	return res;
}

static ct_loop *ct_ensure_loop(ct_info *cti, ct_region *reg, ir_node *cond)
{
	ct_loop *res = pmap_new_get(&reg->loops, cond);
	if (!res) {
		res = ct_new_loop(cti, cond, reg);
		plist_insert_back(reg->templates, res);
		pmap_new_insert(&reg->loops, cond, res);
	}
	return res;
}

static void ct_pull_nodes(ct_info *cti, ir_node *irn, ct_region *reg);

static void ct_place_node(ct_info *cti, ir_node *irn, gc_cond *gc, ct_region *reg)
{
	switch (gc_get_cond_type(gc)) {
	case gct_once: {
		/* No condition. Place in reg and be done. */
		ct_node *ctn = phase_get_or_set_irn_data(cti->phase, irn);
		plist_insert_back(ctn->regions, reg);

		/* Pull in dominated nodes. */
		ct_pull_nodes(cti, irn, reg);

		break;
	}
	case gct_if_true:
	case gct_if_false: {
		/* Get the condition node. */
		ir_node     *cond    = gc_get_cond_irn(gc);
		ct_template *tmpl    = reg->parent;
		int          is_true = (gc_get_cond_type(gc) == gct_if_true);

		/* Is this the repeat/leave branch of a loop? */
		if (tmpl && (tmpl->type == ctt_loop) &&
		            (((ct_loop*)tmpl)->cond == cond)) {

			/* Place in the region above the loop or the repeat region. */
			ct_place_node(
				cti, irn, gc_get_cond_next(gc),
				is_true ? tmpl->parent : ((ct_loop*)tmpl)->repeat
			);

		} else {
			ct_branch *branch = ct_ensure_branch(cti, reg, cond);

			/* Recurse to the deeper regions. */
			ct_place_node(
				cti, irn, gc_get_cond_next(gc),
				is_true ? branch->rg_true : branch->rg_false
			);
		}
		break;
	}
	case gct_while_true: {
		/* Create/get the loop for the node. */
		ir_node *cond = gc_get_cond_irn(gc);
		ct_loop *tmpl = ct_ensure_loop(cti, reg, cond);

		/* Gamma may switch to the repeat region. */
		ct_place_node(cti, irn,	gc_get_cond_next(gc), tmpl->header);
		break;
	}
	case gct_invalid: break;
	}
}

static void ct_pull_nodes(ct_info *cti, ir_node *irn, ct_region *reg)
{
	gc_union_map_iter it_union_map;
	gc_union_iter     it_union;
	gc_entry          entry;

	/* Pull dominated nodes of irn to reg. */
	foreach_gc_union_map(cti->gci, irn, entry, it_union_map) {
		gc_cond *cond;

		if (gc_union_is_empty(entry.uni)) {
			ct_place_node(cti, entry.dst, NULL, reg);
		} else {
			foreach_gc_union(entry.uni, cond, it_union) {
				/* Place the node in the region. */
				ct_place_node(cti, entry.dst, cond, reg);
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

	cti->root = ct_new_root(cti, pd_get_root(pdt));
	cti->gci  = gci;
	cti->pdt  = pdt;

	ct_pull_nodes(cti, cti->root->irn, cti->root->reg);

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

static void ct_dump_template_name(ct_template *tmpl, FILE *f)
{
	plist_element_t *it;
	int first = 1;

	switch(tmpl->type) {
	case ctt_root: fprintf(f, "root");  break;

	case ctt_branch: {
		ct_branch *ctb = (ct_branch*)tmpl;
		fprintf(f, "branch");
		break;
	}
	case ctt_loop: {
		ct_loop *ctl = (ct_loop*)tmpl;
		fprintf(f, "loop");
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
	ct_dump_collect(cti, reg, cti->root->irn, &set);

	ct_dump_indent(f, indent + 1); fprintf(f, "nodes {");
	foreach_pset_new(&set, ir_node*, irn, it) {
		if (!first) fprintf(f, ", ");
		fprintf(f, "%li", get_irn_node_nr(irn));
		first = 0;
	}
	fprintf(f, "}\n");

	pset_new_destroy(&set);

	if (plist_count(reg->templates) > 0) {
		plist_element_t *it;
		fprintf(f, "\n");

		foreach_plist(reg->templates, it) {
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
	ct_dump_template_name(tmpl, f);

	fprintf(f, " {\n");
	switch(tmpl->type) {
	case ctt_root: {
		ct_root *ctr = (ct_root*)tmpl;
		ct_dump_region(cti, ctr->reg, "graph",  f, indent + 1);
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
	ct_dump_template(cti, (ct_template*)cti->root, f, 0);
}

static ir_node *ct_find_copy(ct_node *ctn, ct_region *reg)
{
	ct_node *cur = ctn;
	while (cur) {
		assert(plist_count(cur->regions) == 1); /* Must be duplicated. */
		if (plist_first(cur->regions)->data == reg) return cur->irn;
		cur = cur->copy;
	}
	return NULL;
}

static ir_node *ct_select_copy(ct_node *ctn, ir_node *ir_prev,
                               ct_region *rg_prev)
{
	ir_node *sel;

	/* For gamma and eta nodes, try to search the lower regions. */
	if (is_Gamma(ir_prev)) {
		ir_node   *cond   = get_Gamma_cond(ir_prev);
		ct_branch *branch = pmap_new_get(&rg_prev->branches, cond);

		/* Try to be specific. An error is better than weird behaviour. */
		if (branch && (ctn->irn == get_Gamma_true(ir_prev))) {
			/* Note: ASSIGNMENT IS INTENDED here! (and below) */
			if ((sel = ct_find_copy(ctn, branch->rg_true))) return sel;
		}

		if (branch && (ctn->irn == get_Gamma_false(ir_prev))) {
			if ((sel = ct_find_copy(ctn, branch->rg_false))) return sel;
		}
	} else if (is_EtaA(ir_prev)) {
		ir_node *cond = get_EtaA_cond(ir_prev);
		ct_loop *loop = pmap_new_get(&rg_prev->loops, cond);

		/* The condition of a loop resides in its header. The result may
		 * be there too, if it is a theta. */
		if (loop && (
		       (ctn->irn == get_EtaA_cond(ir_prev))   ||
		       (ctn->irn == get_EtaA_header(ir_prev)) ||
		       (ctn->irn == get_EtaA_result(ir_prev))
		)) {
			if ((sel = ct_find_copy(ctn, loop->header))) return sel;
		}

		/* This is still an AcPEG. Therefore there may be edges that lead
		 * directly into the repeat part of the loop. */
		if (loop && (ctn->irn == get_EtaA_repeat(ir_prev))) {
			if ((sel = ct_find_copy(ctn, loop->repeat))) return sel;
		}
	}

	/* If this is a repeat region of a loop, allow access to the header.
	 * We don't have to check the other direction, the graph is still an
	 * AcPEG, so no header can access the repeat region. */
	if ((rg_prev->parent->type == ctt_loop)) {
		ct_loop *loop = (ct_loop*)rg_prev->parent;
		if (rg_prev == loop->repeat) {
			if ((sel = ct_find_copy(ctn, loop->header))) return sel;
		}
	}

	/* If any of those specific lookups fail, try the current region and
	 * go upwards from there, scanning the parent regions. */

	while (rg_prev) {
		if ((sel = ct_find_copy(ctn, rg_prev))) return sel;
		rg_prev = rg_prev->parent->parent;
	}

	return NULL;
}

static ir_node *ct_duplicate_walk(ct_info *cti, ir_node *irn,
                                  ir_node *ir_prev, ct_region *rg_prev)
{
	int        i;
	ir_node   *sel = irn;
	ct_node   *ctn;
	ct_region *reg;

	/* Get region information about the node. */
	ctn = phase_get_irn_data(cti->phase, irn);

	/* Some nodes may have no region information (start/end blocks). We can
	 * assume that they don't need duplication and can just ignore them. */
	if (ctn) {
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

		/* Try to select a copy of ctn->irn for use by the previous node, if
		 * there are copies of the node. If not, just leave everything be. */
		if (ir_prev && ctn->copy) {
			sel = ct_select_copy(ctn, ir_prev, rg_prev);

			if (!sel)
			{
				dump_irnode_to_file(stdout, ir_prev);
				dump_irnode_to_file(stdout, ctn->irn);
			}

			assert(sel && "Couldn't select a copy for ir_prev.");
		}
	} else {
		/* Treat nodes without region information as being in the root. */
		reg = cti->root->reg;
	}

	if (!irn_visited(irn)) {
		mark_irn_visited(irn);

		/* Visit deps and replace them. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);
			ir_node *ir_sel = ct_duplicate_walk(cti, ir_dep, irn, reg);
			if (ir_sel != ir_dep) set_irn_n(irn, i, ir_dep);
		}
	}

	return sel;
}

static void ct_duplicate(ct_info *cti)
{
	inc_irg_visited(phase_get_irg(cti->phase));
	ct_duplicate_walk(cti, cti->root->irn, NULL, NULL);
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

	ct_duplicate(cti);


//	edge_state = edges_assure(irg);
//	ct_duplicate(cti);
//	if (!edge_state) edges_deactivate(irg);

	ir_add_dump_flags(ir_dump_flag_hide_control_flow);
	dump_ir_graph(irg, "dup");
	ir_remove_dump_flags(ir_dump_flag_hide_control_flow);

#ifdef LOG_CFG_REGIONS
	printf("-----------------------------\n");
	printf("CFG Regions after duplication\n");
	printf("-----------------------------\n");
	ct_dump(cti, stdout);
#endif

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
//
//	dump_ir_graph(irg, "newcfg");

	ct_free(cti);
	gc_free(gci);

	restore_optimization_state(&opt);

	set_irg_outs_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_loopinfo_inconsistent(irg);
}
