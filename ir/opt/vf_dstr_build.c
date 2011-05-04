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
 * @brief   Building Firm graphs from VFirm with arrange information.
 * @author  Olaf Liebe
 * @version $Id: $
 */

#include "firm_types.h"
#include "ircons.h"
#include "irgraph.h"
#include "irtools.h"
#include "irnode_t.h"
#include "irgmod.h"
#include "irdump.h"
#include "irphase_t.h"
#include "vf_depth.h"
#include "vf_dstr_build.h"
#include "vf_dstr_arrange.h"
#include "xmalloc.h"
#include "obstack.h"
#include "pset_new.h"
#include "pmap_new.h"
#include "iredges.h"
#include "irgwalk.h"
#include <assert.h>
#include <string.h>

#define VB_DEBUG_BUILD 1

typedef struct obstack obstack;

typedef struct vb_info {
	vl_info    *vli;
	obstack     obst;      /* For long-time allocations. */
	obstack     temp_obst; /* For temporary allocations inside functions. */
	pmap_new_t  blocks;
	pmap_new_t  merges;
	ir_node    *old_block; /* The block that hosts the VFirm graph. */
	plist_t    *todo;      /* Remaining loop subgraphs to process. */
	ir_phase   *phase;
} vb_info;

typedef struct vb_node {
	char     on_todo;    /* Is the node on the todo list? */
	ir_node *loop_copy;  /* The copy that belongs to the last seen loop. */
	ir_node *loop;       /* The loop that was last seen. */
	ir_node *link;       /* Links loops with etas and weaks with targets. */
} vb_node;

/**
 * Loop nodes:
 * - When adding them, mark all fused nodes with on_todo, to prevent adding
 *   them, too.
 * - Process loops by depth. Put a NULL after loops from one phase to detect
 *   when the next phase begins.
 * - Duplicate loop nodes between phases.
 */

static int vb_skip_node(vb_info *vbi, ir_node *irn)
{
	/* Only arrange nodes from the "old" block. */
	return get_nodes_block(irn) != vbi->old_block;
}

static ir_node *vb_build_lane(vb_info *vbi, va_info *vai, va_region *region,
                              ir_node *pred);

static ir_node *vb_build_child(vb_info *vbi, va_info *vai, va_region *child,
                               ir_node *pred)
{
	ir_node  *ins[2];
	ir_graph *irg   = va_get_irg(vai);
	ir_node  *cond  = va_region_get_cond(child);
	int       value = va_region_get_value(child);

	/* Construct the branch blocks etc. */
	ir_node   *branch   = new_r_Cond(pred, cond);
	ir_node   *ir_true  = new_r_Proj(branch, mode_X, pn_Cond_true);
	ir_node   *ir_false = new_r_Proj(branch, mode_X, pn_Cond_false);
	ir_node   *bl_true  = new_r_Block(irg, 1, &ir_true);
	ir_node   *bl_false = new_r_Block(irg, 1, &ir_false);
	va_region *rg_true  = value ? child : va_region_get_link(child);
	va_region *rg_false = value ? va_region_get_link(child) : child;
	ir_node   *bl_merge;

	/* Construct the linked regions (where available). */
	if (rg_true) {
		bl_true = vb_build_lane(vbi, vai, rg_true,  bl_true);
	}
	if (rg_false) {
		bl_false = vb_build_lane(vbi, vai, rg_false, bl_false);
	}

	/* Merge control flow again. The first jump MUST BE from true here. */
	ins[0] = new_r_Jmp(bl_true);
	ins[1] = new_r_Jmp(bl_false);
	bl_merge = new_r_Block(irg, 2, ins);

	/* Store the merge block for both regions. */
	pmap_new_insert(&vbi->merges, rg_true,  bl_merge);
	pmap_new_insert(&vbi->merges, rg_false, bl_merge);

	return bl_merge;
}

static ir_node *vb_build_lane(vb_info *vbi, va_info *vai, va_region *region,
                              ir_node *pred)
{
	va_region_child_it it;

	int        new_block   = 0;
	va_region *pred_region = va_region_get_pred(region);
	va_region *child;

	if (pred_region) {
		pred = vb_build_lane(vbi, vai, pred_region, pred);
	}

	/* If loop_gamma is not NULL, we are at the last region of the lane and
	 * there has to be a child with the same condition as the gamma node. Put
	 * that child last and instead of creating a merge block, wire it back to
	 * the loops entry block. */

	foreach_va_region_child(region, child, it) {
		pred = vb_build_child(vbi, vai, child, pred);
		assert(is_Block(pred));
	}

	/* Start a new block if no children did so and we have a pred region. */
	new_block |= (pred_region && (va_region_get_child_count(region) == 0));

	/* Also if the region contains loop nodes, we have to ensure that there
	 * is exactly one predecessor. So close all merge regions here, to avoid
	 * handling gamma nodes and thetas at the same time. */
	new_block |= va_region_has_loops(region) && (get_Block_n_cfgpreds(pred) > 1);

	/* Create that block, if requested. */
	if (new_block) {
		ir_node *jump = new_r_Jmp(pred);
		pred = new_r_Block(va_get_irg(vai), 1, &jump);
	}

	pmap_new_insert(&vbi->blocks, region, pred);
	return pred;
}

/* Try to move a gamma node to the associated branches merge block. */
static void vb_gamma_move_to_merge_block(vb_info *vbi, va_info *vai,
                                         ir_node *irn)
{
	va_region *branch_region;
	ir_node   *block;

	/* Determine the gammas branch region (implements value selection). */
	assert(is_Gamma(irn));
	branch_region = va_gamma_get_branch_region(vai, irn);
	assert(branch_region);

	/* Obtain the merge block for the branch. */
	block = pmap_new_get(&vbi->merges, branch_region);
	assert(block);

	set_nodes_block(irn, block);
}

/* Try to move a node to the block of its associated region. */
static int vb_node_move_to_region_block(vb_info *vbi, va_info *vai,
                                        ir_node *irn)
{
	va_region *region = va_node_get_region(vai, irn);

	if (region) {
		ir_node *block = pmap_new_get(&vbi->blocks, region);
		assert(block);

		set_nodes_block(irn, block);
		return 1;
	}

	return 0;
}

static void vb_enqueue_loop(vb_info *vbi, ir_node *first_loop)
{
	vb_node *vbn = phase_get_or_set_irn_data(vbi->phase, first_loop);

	/* If the loop is already on the todo list, do nothing. */
	if (!vbn->on_todo) {
		va_loop_loop_it it;
		ir_node        *loop;

		/* Insert the node to the list and mark it. */
		plist_insert_back(vbi->todo, first_loop);
		vbn->on_todo = 1;

		/* Mark all fused loops as being on the todo list, too. */
		foreach_va_loop_loop(first_loop, loop, it) {
			if (loop == first_loop) continue;

			vbn = phase_get_or_set_irn_data(vbi->phase, loop);
			vbn->on_todo = 1;
		}
	}
}

static void vb_place_nodes_walk(vb_info *vbi, va_info *vai, ir_node *irn)
{
	int i;

	if (vb_skip_node(vbi, irn)) return;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	if (is_Loop(irn)) {
		/* Enqueue loop nodes for later processing. */
		vb_enqueue_loop(vbi, irn);
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		vb_place_nodes_walk(vbi, vai, ir_dep);
	}

	if (is_Gamma(irn)) {
		/* Try to move gammas to their merge region and make them phi. */
		vb_gamma_move_to_merge_block(vbi, vai, irn);
	} else if (is_Proj(irn)) {
		/* Move to the same block as pred. */
		set_nodes_block(irn, get_nodes_block(get_Proj_pred(irn)));
	} else {
		/* Move nodes to their regions block. */
		vb_node_move_to_region_block(vbi, vai, irn);
	}
}

static void vb_place_nodes(vb_info *vbi, va_info *vai, ir_node *irn)
{
	inc_irg_visited(va_get_irg(vai));
	vb_place_nodes_walk(vbi, vai, irn);
}

/* Scan the loop and collect information on the non-NULL arrays. */
static void vb_scan_area_walk(vb_info *vbi, ir_node *irn, int loop_depth,
                              plist_t *thetas, plist_t *etas, plist_t *invars)
{
	int i, depth;

	depth = vl_node_get_depth(vbi->vli, irn);

	/* Invariant nodes cause a stop. */
	if (depth < loop_depth) {
		if (invars) plist_insert_back(invars, irn);
		return;
	}

	assert(depth >= loop_depth);

	if (vb_skip_node(vbi, irn)) return;
	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Collect thetas and etas of the current loop on the way. */
	if (depth == loop_depth) {

		/* Eta nodes in the current loop depth. */
		if (etas && is_Eta(irn)) {
			assert(!plist_find_value(etas, irn));
			plist_insert_back(etas, irn);
		}

		if (thetas && is_Theta(irn)) {
			assert(get_Theta_depth(irn) == depth);
			assert(!plist_find_value(thetas, irn));
			plist_insert_back(thetas, irn);
		}
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		vb_scan_area_walk(vbi, ir_dep, loop_depth, thetas, etas, invars);
	}
}

/* Scan a contiguous area of equal depth for several kinds of nodes. Set reset,
 * to begin a new scan, don't set reset, if further nodes shall be added to the
 * current scan. */
static void vb_scan_area(vb_info *vbi, ir_node *root, int reset,
                         plist_t *thetas, plist_t *etas, plist_t *invars)
{
	int depth = vl_node_get_depth(vbi->vli, root);
	if (reset) inc_irg_visited(get_irn_irg(root));
	vb_scan_area_walk(vbi, root, depth, thetas, etas, invars);
}

/* Same as vb_scan_area, but it does scan all the areas belonging to the given
 * list of fused loops. There are a lot of nodes that can be selected, but we
 * usually only need a part of them and most often clear space after that. */
static void vb_scan_loop(vb_info *vbi, ir_node *loop, plist_t *thetas,
                         plist_t *etas, plist_t *invars)
{
	va_loop_loop_it it;

	ir_node  *loop_cur, *eta;
	ir_graph *irg   = get_irn_irg(loop);
	int       depth = vl_node_get_depth(vbi->vli, loop) + 1;

	inc_irg_visited(irg);

	/* Scan the conditions and values of all the fused loops etas. */
	foreach_va_loop_eta(loop, loop_cur, eta, it) {
		vb_scan_area_walk(vbi, get_Eta_cond(eta),  depth, thetas, etas, invars);
		vb_scan_area_walk(vbi, get_Eta_value(eta), depth, thetas, etas, invars);
	}
}

/* Insert nested and unvisited nodes into the list and visit them. */
static void vb_insert_visit_nested(vb_info *vbi, int depth, ir_node *irn,
                                   plist_t *list)
{
	if (irn_visited(irn)) return;
	if (vl_node_get_depth(vbi->vli, irn) < depth) return;

	plist_insert_back(list, irn);
	mark_irn_visited(irn);
}

static void vb_get_header_roots(vb_info *vbi, ir_node *loop, plist_t *thetas,
                                plist_t *roots)
{
	va_loop_loop_it  it;
	plist_element_t *it_theta;

	ir_node  *eta, *loop_cur;
	ir_graph *irg   = get_irn_irg(loop);
	int       depth = vl_node_get_depth(vbi->vli, loop) + 1;

	inc_irg_visited(irg);

	/* Add the eta deps, if they are nested. */
	foreach_va_loop_eta(loop, loop_cur, eta, it) {
		vb_insert_visit_nested(vbi, depth, get_Eta_cond(eta),  roots);
		vb_insert_visit_nested(vbi, depth, get_Eta_value(eta), roots);
	}

	/* And add all theta nodes. */
	foreach_plist(thetas, it_theta) {
		ir_node *theta = plist_element_get_value(it_theta);
		vb_insert_visit_nested(vbi, depth, theta, roots);
	}
}

static void vb_get_body_roots(vb_info *vbi, ir_node *loop, plist_t *thetas,
                              plist_t *roots)
{
	plist_element_t *it;

	ir_graph *irg   = get_irn_irg(loop);
	int       depth = vl_node_get_depth(vbi->vli, loop) + 1;

	inc_irg_visited(irg);
	assert(thetas);

	/* Add all thetas next nodes. */
	foreach_plist(thetas, it) {
		ir_node *theta = plist_element_get_value(it);
		ir_node *next  = get_Theta_next(theta);
		vb_insert_visit_nested(vbi, depth, next, roots);
	}
}

static void vb_set_link(vb_info *vbi, ir_node *irn, ir_node *link)
{
	vb_node *vbn = phase_get_or_set_irn_data(vbi->phase, irn);
	vbn->link = link;
}

static ir_node *vb_get_link(vb_info *vbi, ir_node *irn)
{
	vb_node *vbn = phase_get_irn_data(vbi->phase, irn);
	return !vbn ? NULL : vbn->link;
}

/* Detach all the etas in the given list. This is quite expensive, but we
 * should only do it once for any eta node in the graph and then again for
 * the copie we create out of it. */
static void vb_detach_etas(vb_info *vbi, plist_t *etas)
{
	plist_element_t *it, *it_invar;

	foreach_plist(etas, it) {
		ir_node   *eta = plist_element_get_value(it);
		plist_t   *invars;
		ir_node  **ins, *block, *loop;
		ir_mode   *mode;
		int        count, offset;

		/* Collect the invariant nodes for the given eta. */
		invars = plist_obstack_new(&vbi->temp_obst);
		vb_scan_area(vbi, get_Eta_cond(eta),  1, NULL, NULL, invars);
		vb_scan_area(vbi, get_Eta_value(eta), 0, NULL, NULL, invars);

		/* Create a loop node to detach the eta. */
		count = plist_count(invars);
		ins   = OALLOCN(&vbi->temp_obst, ir_node*, count);
		block = get_nodes_block(eta);
		mode  = get_irn_mode(eta);

		offset = 0;
		foreach_plist(invars, it_invar) {
			ir_node *invar = plist_element_get_value(it_invar);
			ins[offset++] = invar;
		}

		loop = new_r_Loop(block, count, ins, mode, eta, NULL);
		set_Loop_next(loop, loop);
		vl_node_copy_depth(vbi->vli, eta, loop);

		/* Reroute edges to the new loop. */
		edges_reroute(eta, loop, get_irn_irg(eta));

		plist_free(invars);
		obstack_free(&vbi->temp_obst, invars);

		/* Link the nodes. */
		vb_set_link(vbi, eta, loop);
		vb_set_link(vbi, loop, eta);
	}
}

/* Given a loop node, it replaces that node and all fused nodes by the value
 * of the underlying eta node, effectively attaching the loop subgraph into
 * the original graph again. */
static void vb_attach_loop_values(vb_info *vbi, ir_node *first_loop)
{
	ir_node  *loop, *next_loop;
	ir_graph *irg = get_irn_irg(first_loop);

	/* Replace fused loop nodes by their eta values. */
	loop = first_loop;
	do {
		ir_node *eta   = get_Loop_eta(loop);
		ir_node *value = get_Eta_value(eta);

		/* Clear the links (primarily to ease debugging). */
		vb_set_link(vbi, loop, NULL);
		vb_set_link(vbi, eta,  NULL);

		next_loop = get_Loop_next(loop);
		edges_reroute(loop, value, irg);

		loop = next_loop;
	}
	while(loop != first_loop);
}

/* Replace the next edges of the given thetas by weak nodes. */
static void vb_detach_theta_nexts(vb_info *vbi, plist_t *thetas)
{
	plist_element_t *it;
	foreach_plist(thetas, it) {
		ir_node *theta = plist_element_get_value(it);
		ir_node *block = get_nodes_block(theta);
		ir_node *next  = get_Theta_next(theta);
		ir_mode *mode  = get_irn_mode(theta);
		ir_node *weak  = new_r_Weak(block, mode, next);
		assert(!is_Weak(next));

		set_Theta_next(theta, weak);
		assert(!is_Eta(next));

		/* Link the nodes. */
		vb_set_link(vbi, weak, next);
		vb_set_link(vbi, next, weak);
	}
}

static void vb_collect_all_etas(ir_node *irn, plist_t *etas)
{
	int i;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	if (is_Eta(irn)) {
		plist_insert_back(etas, irn);
	}

	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);
		vb_collect_all_etas(ir_dep, etas);
	}
}

static void vb_detach_all_etas(vb_info *vbi, ir_node *root)
{
	/* Can't use the obstack, vb_detach_etas allocates there (the link). */
	plist_t *etas = plist_obstack_new(&vbi->temp_obst);

	inc_irg_visited(get_irn_irg(root));
	vb_collect_all_etas(root, etas);
	vb_detach_etas(vbi, etas);

	plist_free(etas);
	obstack_free(&vbi->temp_obst, etas);
}

/* For all theta nodes in the list, which have a weakened theta, attach the
 * weak links target again to the theta. */
static void vb_attach_theta_nexts(vb_info *vbi, plist_t *thetas)
{
	plist_element_t *it;
	foreach_plist(thetas, it) {
		ir_node *theta = plist_element_get_value(it);
		ir_node *weak  = get_Theta_next(theta);
		ir_node *next;

		assert(is_Weak(weak));
		next = get_Weak_target(weak);

		assert(!is_Eta(next));
		set_Theta_next(theta, next);

		/* Clear the links (primarily to ease debugging). */
		vb_set_link(vbi, weak, NULL);
		vb_set_link(vbi, next, NULL);
	}
}

static ir_node *vb_build_base(vb_info *vbi, ir_node *root, ir_node *pred)
{
	va_info *vai;
	ir_node *last;

	/* Arrange the VFirm graph below root in loop-less regions. */
	assert(get_nodes_block(root) == vbi->old_block);
	vai = va_init_root(vbi->vli, root, 1);

	/* Build the block structure for the partial graph and process nodes. */
	last = vb_build_lane(vbi, vai, va_get_root_region(vai), pred);
	vb_place_nodes(vbi, vai, va_get_root(vai));
	dump_ir_graph(get_irn_irg(root), "base-lane-built");

	va_free(vai);
	return last;
}

#if VB_DEBUG_BUILD
static void vb_print_node_list(plist_t *nodes)
{
	plist_element_t *it;
	foreach_plist(nodes, it) {
		ir_node *irn = plist_element_get_value(it);
		if (it != plist_first(nodes)) printf(", ");
		printf("%ld", get_irn_node_nr(irn));
	}
}
#endif

/* Used by vb_build_loop_root to build the tuples for the new root. */
static ir_node *vb_build_tuple(vb_info *vbi, plist_t *list)
{
	plist_element_t *it;
	int count  = plist_count(list);
	int offset = 0;
	ir_node **ins, *tuple;

	ins = OALLOCN(&vbi->temp_obst, ir_node*, count);

	offset = 0;
	foreach_plist(list, it) {
		ir_node *irn = plist_element_get_value(it);

		/* Pure eta nodes shouldn't end up in the generated tuples. */
		if (is_Eta(irn)) {
			ir_node *link = vb_get_link(vbi, irn);
			assert(link && "Attached eta found.");
			irn = link;
		}

		ins[offset++] = irn;
	}

	tuple = new_r_Tuple(vbi->old_block, count, ins);
	obstack_free(&vbi->temp_obst, ins);

	return tuple;
}

/* Build a root node for the loop. This will also populate the provided list
 * of theta nodes for the loop (and fused loop nodes), that can be used, to
 * weaken and strengthen theta nodes. */
static ir_node *vb_build_loop_root(vb_info *vbi, ir_node *loop,
                                   plist_t *header_roots, plist_t *body_roots)
{
	ir_node *loop_cond, *gamma;
	ir_node *header_tuple, *body_tuple;

#if VB_DEBUG_BUILD
	printf("Header roots: "); vb_print_node_list(header_roots); printf("\n");
	printf("Body roots: ");   vb_print_node_list(body_roots);   printf("\n");
#endif

	/* Build the header and body tuple. Make the body reference the header. */
	header_tuple = vb_build_tuple(vbi, header_roots);
	plist_insert_back(body_roots, header_tuple);
	body_tuple = vb_build_tuple(vbi, body_roots);
	plist_erase(body_roots, plist_last(body_roots));

	/* Create a gamma node to the two blocks with the loop cond. */
	loop_cond = get_Eta_cond(get_Loop_eta(loop));

	gamma = new_r_Gamma(
		vbi->old_block, loop_cond, body_tuple, header_tuple, mode_T
	);

	return gamma;
}

static va_region *vb_region_get_first_child(va_region *region)
{
	va_region_child_it it;
	va_region_child_it_init(&it, region);
	return va_region_child_it_next(&it);
}

static void vb_build_loop(vb_info *vbi, ir_node *first_loop)
{
	plist_t   *thetas, *header_roots, *body_roots;
	va_region *root_region, *header_region, *body_region;
	ir_node   *root, *loop_cond, *cond_jump, *true_proj, *false_proj, *ins[2];
	ir_node   *header_block, *follow_block, *branch_block, *end_block;
	ir_node   *body_block;
	va_info   *vai;

	/* 50-18 = 32 */
#if VB_DEBUG_BUILD
	printf("+================================================+\n");
	printf("| Building loop %-32ld |\n", get_irn_node_nr(first_loop));
	printf("+================================================+\n");
#endif

	/**
	 * Like in vb_build_base, we have to collect eta nodes in the loop, in
	 * order to detach the nested loops, so we can handle them like nodes. But
	 * we also have to do more: first we have to weaken the next edges on all
	 * the theta nodes in the loop and then we have to construct a root node
	 * for the arrangement algorithm. The latter needs distinct root nodes for
	 * the loops header and body. We should also keep the theta list, to ease
	 * cleanup after arranging.
	 */

	thetas       = plist_obstack_new(&vbi->temp_obst);
	header_roots = plist_obstack_new(&vbi->temp_obst);
	body_roots   = plist_obstack_new(&vbi->temp_obst);

	/* Go ahead and collect all those nodes. */
	vb_scan_loop        (vbi, first_loop, thetas, NULL, NULL);
	vb_get_header_roots (vbi, first_loop, thetas, header_roots);
	vb_get_body_roots   (vbi, first_loop, thetas, body_roots);

	/* Detach next nodes of the thetas. */
	vb_detach_theta_nexts(vbi, thetas);
	dump_ir_graph(get_irn_irg(first_loop), "loop-thetas-detached");

	/* Build us a nice loop root for arrangement. */
	root = vb_build_loop_root(vbi, first_loop, header_roots, body_roots);

	/* Clean up the mess. */
	plist_free(body_roots);
	plist_free(header_roots);
	obstack_free(&vbi->temp_obst, header_roots);

	/**
	 * Now things get funny. We can arrange the new graph from the newly built
	 * artificial loop root. From there we can pick out the loops header and
	 * body regions and run the lane building algorithm, to get a CFG. Then
	 * we connect them back together and assign the subgraphs nodes to the new
	 * regions. After all that happened, we can attach the thetas next edges
	 * again and proceed with the nested (but still detached) loops. For better
	 * orientation, this is what the final graph will look like:
	 *
	 *     ...          backedge
	 *      |        /------------\
	 *      |        |            |    (1) vb_build_lane(header_region, ...)
	 *     header_block           |    (2) vb_build_lane(body_region, ...)
	 *          |                 |
	 *     [..header..] (1)       |
	 *          |                 |
	 *     branch_block           |
	 *      |        |            |
	 *      |      body_block     |
	 *      |          |          |
	 *      |      [..body..] (2) |
	 *      |          |          |
	 *      |      end_block      |
	 *      |          |          |
	 *      |          \----------/
	 * follow_block
	 *
	 * Start by initiating arrangment and selecting the proper regions.
	 */

	vai = va_init_root(vbi->vli, root, 1);

	/* Find the regions of the loop header and body. */
	root_region = va_get_root_region(vai);

	/* There should only be the fake gamma child. Get it. */
	assert(va_region_get_child_count(root_region) == 1);
	body_region = vb_region_get_first_child(root_region);

	/* The breaking condition needs to be false for the body. */
	loop_cond = get_Eta_cond(get_Loop_eta(first_loop));
	assert(va_region_get_cond(body_region) == loop_cond);

	if (va_region_get_value(body_region)) {
		body_region = va_region_get_link(body_region);
	}

	assert(body_region);

	/* Now there should be a predecessor of the root that is the header. */
	header_region = va_region_get_pred(root_region);
	assert(header_region);

	/* Okey-doke. Now go down to a block level and get the CFG built. */
	follow_block = get_nodes_block(first_loop);
	header_block = exact_copy(follow_block);

	/* The code above should guarantee a unique predecessor for blocks that
	 * contain a loop. Make sure that this is the case. */
	assert(get_Block_n_cfgpreds(header_block) == 1);

	/* Construct the headers lane down to the loops branch block. */
	branch_block = vb_build_lane(vbi, vai, header_region, header_block);
	vb_place_nodes(vbi, vai, get_Gamma_true(root));
	dump_ir_graph(get_irn_irg(first_loop), "loop-header-lane-built");

	/* Build the branch with the loops condition. */
	cond_jump  = new_r_Cond(branch_block, loop_cond);
	true_proj  = new_r_Proj(cond_jump, mode_X, pn_Cond_true);
	false_proj = new_r_Proj(cond_jump, mode_X, pn_Cond_false);

	/* Connect the follow block and make a start block for the body. */
	set_irn_in(follow_block, 1, &true_proj);
	body_block = new_r_Block(va_get_irg(vai), 1, &false_proj);

	/* Build the bodys lane and connect the end back to the header. */
	end_block = vb_build_lane(vbi, vai, body_region, body_block);
	vb_place_nodes(vbi, vai, get_Gamma_false(root));

	ins[0] = get_Block_cfgpred(header_block, 0);
	ins[1] = new_r_Jmp(end_block);
	set_irn_in(header_block, 2, ins);

	dump_ir_graph(get_irn_irg(first_loop), "loop-body-lane-built");
	va_free(vai);

	/* Now re-attach the thetas next edges and of course the loop itself and
	 * then we are done with the loop. */
	vb_attach_theta_nexts(vbi, thetas);
	vb_attach_loop_values(vbi, first_loop);

	dump_ir_graph(get_irn_irg(first_loop), "loop-thetas-reattached");

	plist_free(thetas);
	obstack_free(&vbi->temp_obst, thetas);
}

/* Clone the given node if needed (the clone or original irn is returned). */
static ir_node *vb_clone_dep(vb_info *vbi, ir_node *ir_dep, ir_node *loop)
{
	vb_node *vb_dep = phase_get_or_set_irn_data(vbi->phase, ir_dep);

	if (vb_dep->loop == NULL) {
		/* Let the loop take ownership of the unowned node. */
		vb_dep->loop      = loop;
		vb_dep->loop_copy = ir_dep; /* Use the original, no copy. */

#if VB_DEBUG_BUILD
		printf("Assigning node %ld to loop %ld.\n",
			get_irn_node_nr(ir_dep), get_irn_node_nr(loop)
		);
#endif
	} else if (vb_dep->loop == loop) {
		/* We already processed that node for the current loop. */
		assert(get_irn_mode(ir_dep) == get_irn_mode(vb_dep->loop_copy));
		assert(get_irn_op(ir_dep) == get_irn_op(vb_dep->loop_copy));

		ir_dep = vb_dep->loop_copy; /* Get the stored copy. */

	} else if (vb_dep->loop != loop) {
		/* The node belongs to another loop. Clone it. */
		ir_node *clone = vl_exact_copy(vbi->vli, ir_dep);

#if VB_DEBUG_BUILD
		printf("Cloning node %ld (now %ld) for loop %ld.\n",
			get_irn_node_nr(ir_dep), get_irn_node_nr(clone),
			get_irn_node_nr(loop)
		);
#endif

		/* Remember the clone on the original node. */
		vb_dep->loop      = loop;
		vb_dep->loop_copy = clone;

		/* And then return the clone. */
		ir_dep = clone;

		if (is_Loop(clone)) {
			set_Loop_next(clone, clone);
		}
	}

	assert(vb_dep->loop == loop);
	return ir_dep;
}

static void vb_clone_walk(vb_info *vbi, ir_node *irn, ir_node *loop)
{
	int i;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	/* Scan dependencies for nodes to copy. */
	for (i = 0; i < get_irn_arity(irn); i++) {
		ir_node *ir_dep = get_irn_n(irn, i);

		/* Skip already assigned nodes. */
		if (vb_skip_node(vbi, ir_dep)) continue;

		/* Do not traverse to other loops. If they need to be cloned, that
		 * was already done at the outer level. */
		if (is_Loop(ir_dep)) continue;

		/* Clone the node if needed, redirect the edge and recurse. */
		ir_dep = vb_clone_dep(vbi, ir_dep, loop);
		set_irn_n(irn, i, ir_dep); /* May be a no-op. */
		vb_clone_walk(vbi, ir_dep, loop);
	}
}

static void vb_clone_loop(vb_info *vbi, ir_node *first_loop)
{
	va_loop_loop_it it;

	ir_node  *loop, *eta;
	ir_graph *irg = get_irn_irg(first_loop);

	inc_irg_visited(irg);

#if VB_DEBUG_BUILD
	printf("Assigning nodes to loop %ld.\n", get_irn_node_nr(first_loop));
#endif

	/* Start to discover nodes from the eta nodes deps. */
	foreach_va_loop_eta(first_loop, loop, eta, it) {
		vb_clone_walk(vbi, eta, first_loop);
	}
}

static void *vb_init_node(ir_phase *phase, const ir_node *irn)
{
	vb_info *vbi = phase_get_private(phase);
	(void)irn;
	return OALLOCZ(&vbi->obst, vb_node);
}

static void vb_ungate_walk(ir_node *irn, void *ctx)
{
	vb_info *vbi = ctx;

	if (is_Gamma(irn) || is_Theta(irn)) {
		ir_node *ins[2], *phi;
		ir_mode *mode  = get_irn_mode(irn);
		ir_node *block = get_nodes_block(irn);

		if (is_Gamma(irn)) {
			/* Order is defined by vb_build_child. */
			ins[0] = get_Gamma_true(irn);
			ins[1] = get_Gamma_false(irn);
		} else if (is_Theta(irn)) {
			/* Order is defined by vb_build_loop. */
			ins[0] = get_Theta_init(irn);
			ins[1] = get_Theta_next(irn);
		}

		phi = new_r_Phi(block, 2, ins, mode);
		vl_exchange(vbi->vli, irn, phi);
	}
}

static void vb_ungate(vb_info *vbi)
{
	ir_graph *irg = phase_get_irg(vbi->phase);
	irg_walk_graph(irg, NULL, vb_ungate_walk, vbi);
}

/* TODO: 1. Clone keeps loop depth.
 * TODO: 2. Replacement of loop nodes (former combine phase).
 * TODO: 3. Fixing gating nodes. Keep a list of thetas? Pass-in to make_root? */

void vb_build(ir_graph *irg)
{
	ir_node *ins[1], *block, *end, *ret;
	vb_info  vbi;
	int      depth;

	/* Construct the first block to use. */
	ins[0] = get_irg_initial_exec(irg);
	block  = new_r_Block(irg, 1, ins);

	/* Get the return node for construction. */
	end = get_irg_end_block(irg);
	ret = get_Block_cfgpred(end, 0);
	assert(is_Return(ret) && "Invalid VFirm graph.");

	vbi.vli = vl_init(irg);
	vbi.old_block = get_nodes_block(ret);

	obstack_init(&vbi.obst);
	obstack_init(&vbi.temp_obst);
	pmap_new_init(&vbi.merges);
	pmap_new_init(&vbi.blocks);

	vbi.todo  = plist_obstack_new(&vbi.obst);
	vbi.phase = new_phase(irg, vb_init_node);
	phase_set_private(vbi.phase, &vbi);

	/* Detach all loops in the graph. */
	vb_detach_all_etas(&vbi, ret);

	/* Construct the base layer first. */
	vb_build_base(&vbi, ret, block);

	/* Process remaining loop nodes. */
	for (depth = 0; plist_count(vbi.todo) > 0; depth++) {
		plist_element_t *it;

		/* Duplicate nodes for loops (needs detached nested loops). */
		foreach_plist(vbi.todo, it) {
			ir_node *loop = plist_element_get_value(it);
			vb_clone_loop(&vbi, loop);
		}

		/* Add a terminator for the current depth. */
		plist_insert_back(vbi.todo, NULL);

		/* Pop off loops until we hit the terminator. */
		while (1) {
			ir_node *loop;
			it = plist_first(vbi.todo);
			plist_erase(vbi.todo, it);

			loop = plist_element_get_value(it);
			if (!loop) break; /* Terminator found. */

			/* Process the loop. */
			vb_build_loop(&vbi, loop);
		}

#if VB_DEBUG_BUILD
		printf("Finished processing loops of depth %d.\n", depth);
#endif
	}

	pmap_new_destroy(&vbi.merges);
	pmap_new_destroy(&vbi.blocks);

	/* Ungate the graph. */
	vb_ungate(&vbi);
	dump_ir_graph(irg, "ungated");

	phase_free(vbi.phase);
	plist_free(vbi.todo);
	vl_free(vbi.vli);

	obstack_free(&vbi.temp_obst, NULL);
	obstack_free(&vbi.obst, NULL);
}
