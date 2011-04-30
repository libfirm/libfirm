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
#include "vf_dstr_build.h"
#include "vf_dstr_arrange.h"
#include "vf_dstr_partition.h"
#include "xmalloc.h"
#include "obstack.h"
#include "pset_new.h"
#include "pmap_new.h"
#include "iredges.h"
#include <assert.h>
#include <string.h>

#define VB_DEBUG_BUILD 1

typedef struct obstack obstack;

typedef struct vb_info {
	obstack     obst;
	pmap_new_t  blocks;
	pmap_new_t  merges;
	plist_t    *gammas;
	ir_node    *old_block; /* The block that hosts the VFirm graph. */
	ir_node    *block;
	plist_t    *todo;      /* Remaining loop subgraphs to process. */
	ir_phase   *phase;

	vl_info *vli; /* TODO: clean this up. */
} vb_info;

typedef struct vb_node {
	char     on_todo;    /* Is the node on the todo list? */
	ir_node *loop_copy;  /* The copy that belongs to the last seen loop. */
	ir_node *loop;       /* The loop that was last seen. */
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
static int vb_gamma_move_to_merge_block(vb_info *vbi, va_info *vai,
                                        ir_node *irn)
{
	va_region *branch_region;
	ir_node   *block;

	/* Determine the gammas branch region (implements value selection). */
	assert(is_Gamma(irn));
	branch_region = va_gamma_get_branch_region(vai, irn);
	if (!branch_region) return 0;

	/* Obtain the merge block for the branch. */
	block = pmap_new_get(&vbi->merges, branch_region);
	if (!block) return 0;

	set_nodes_block(irn, block);
	return 1;
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

static void vb_process_nodes_walk(vb_info *vbi, va_info *vai, ir_node *irn,
                                  ir_node *header_block)
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
		vb_process_nodes_walk(vbi, vai, ir_dep, header_block);
	}

	if (is_Gamma(irn)) {
		/* Try to move gammas to their merge region, if possible. */
		if (!vb_gamma_move_to_merge_block(vbi, vai, irn)) {
			vb_node_move_to_region_block(vbi, vai, irn);
		}

		/* Remember them for post-processing. */
		plist_insert_back(vbi->gammas, irn);
	} else if (is_Proj(irn)) {
		/* Move to the same block as pred. */
		set_nodes_block(irn, get_nodes_block(get_Proj_pred(irn)));
	} else if (is_Theta(irn)) {
		/* Move thetas to the loops first block. */
		assert(header_block);
		set_nodes_block(irn, header_block);
	} else {
		/* Move nodes to their regions block. */
		vb_node_move_to_region_block(vbi, vai, irn);
	}
}

static void vb_process_nodes(vb_info *vbi, va_info *vai, ir_node *irn,
                             ir_node *header_block)
{
	inc_irg_visited(va_get_irg(vai));
	vb_process_nodes_walk(vbi, vai, irn, header_block);
}

static ir_node *vb_build_base(vb_info *vbi, ir_node *root, ir_node *pred)
{
	va_info *vai;
	ir_node *last;

	/* Arrange the VFirm graph below root in loop-less regions. */
	assert(get_nodes_block(root) == vbi->old_block);
	vai = va_init_root(root, 1);

	/* Build the block structure for the partial graph and process nodes. */
	last = vb_build_lane(vbi, vai, va_get_root_region(vai), pred);
	vb_process_nodes(vbi, vai, va_get_root(vai), NULL);

	va_free(vai);
	return last;
}

static ir_node *vb_tuple_from_set(vb_info *vbi, ir_node *block, pset_new_t *set)
{
	pset_new_iterator_t it;

	int       offset = 0;
	ir_node **ins    = OALLOCN(&vbi->obst, ir_node*, pset_new_size(set));
	ir_node  *irn, *tuple;

	/* Fill the ins array with the nodes in the set. */
	foreach_pset_new(set, ir_node*, irn, it) {
		ins[offset] = irn; offset++;
	}

	tuple = new_r_Tuple(block, offset, ins);
	obstack_free(&vbi->obst, ins);

	return tuple;
}

#if VB_DEBUG_BUILD
static void vb_print_node_set(pset_new_t *set)
{
	pset_new_iterator_t it;
	int first = 1;
	ir_node *irn;

	foreach_pset_new(set, ir_node*, irn, it) {
		if (!first) printf(", ");
		printf("%ld", get_irn_node_nr(irn));
		first = 0;
	}
}
#endif

static ir_node *vb_build_loop_root(vb_info *vbi, vl_info *vli,
                                   ir_node *first_loop)
{
	va_loop_loop_it it;

	pset_new_t  roots;
	ir_node    *header_tuple, *body_tuple, *loop, *cond, *gamma, *eta;

	pset_new_init(&roots);

	/* Collect roots for the header. */
	foreach_va_loop_eta(first_loop, loop, eta, it) {
		vl_eta_theta_it  it_theta;
		ir_node         *theta;

		/* Value and condition must be calculated in the header. */
		pset_new_insert(&roots, get_Eta_value(eta));
		pset_new_insert(&roots, get_Eta_cond(eta));

		/* Thetas (later phis) also have to be put in the header. */
		foreach_vl_eta_theta(vli, eta, theta, it_theta) {
			pset_new_insert(&roots, theta);
		}
	}

	/* 50-25 = 25 */
#if VB_DEBUG_BUILD
	printf("+================================================+\n");
	printf("| Building loop header %-25ld |\n", get_irn_node_nr(first_loop));
	printf("+================================================+\n");

	printf("Header roots: ");
	vb_print_node_set(&roots);
	printf("\n");
#endif

	/* Make a tuple for the header. */
	header_tuple = vb_tuple_from_set(vbi, vbi->old_block, &roots);

	/* Reset the root set. */
	pset_new_destroy(&roots);
	pset_new_init(&roots);

	/* Collect roots for the body. */
	foreach_va_loop_eta(first_loop, loop, eta, it) {
		vl_eta_theta_it  it_theta;
		ir_node         *theta;

		foreach_vl_eta_theta(vli, eta, theta, it_theta) {
			ir_node *weak = get_Theta_next(theta);
			assert(is_Weak(weak) && !is_Weak(get_Weak_target(weak)));
			pset_new_insert(&roots, get_Weak_target(weak));
		}
	}

#if VB_DEBUG_BUILD
	printf("+================================================+\n");
	printf("| Building loop body %-27ld |\n", get_irn_node_nr(first_loop));
	printf("+================================================+\n");

	printf("Body roots: ");
	vb_print_node_set(&roots);
	printf("\n");
#endif

	/* Make a tuple for the body that references the header. */
	pset_new_insert(&roots, header_tuple);
	body_tuple = vb_tuple_from_set(vbi, vbi->old_block, &roots);

	/* Create a gamma node to the two blocks with the loop cond. */
	cond  = get_Eta_cond(get_Loop_eta(first_loop));
	gamma = new_r_Gamma(
		vbi->old_block, cond, body_tuple, header_tuple, mode_T
	);

	pset_new_destroy(&roots);

	return gamma;
}

static va_region *vb_region_get_first_child(va_region *region)
{
	va_region_child_it it;
	va_region_child_it_init(&it, region);
	return va_region_child_it_next(&it);
}

static void vb_build_loop(vb_info *vbi, vl_info *vli, ir_node *first_loop)
{
	va_info   *vai;
	ir_node   *root, *cond_jump, *true_proj, *false_proj, *cond, *follow_block;
	ir_node   *header_block, *body_block, *branch_block, *end_block, *ins[2];
	va_region *root_region, *header_region, *body_region;

	/**
	 *     ...
	 *      |        /---------\
	 *      |        |         |
	 *      v        v         |
	 *     header_block        |
	 *          |              |
	 *         ...             |
	 *          |              |
	 *     branch_block        |
	 *      |        |         |
	 *      |        v         |
	 *      |      body_block  |
	 *      |           |      |
	 *      |          ...     |
	 *      |           |      |
	 *      |           v      |
	 *      |       end_block  |
	 *      |           |      |
	 *      |           \------/
	 *      v
	 * follow_block
	 */

	/* Make a unique root for the loop with the condition and arrange. */
	root = vb_build_loop_root(vbi, vli, first_loop);
	vai  = va_init_root(root, 1);

	/* Find the regions of the loop header and body. */
	root_region = va_get_root_region(vai);

	/* There should only be the fake gamma child. Get it. */
	assert(va_region_get_child_count(root_region) == 1);
	body_region = vb_region_get_first_child(root_region);

	/* The breaking condition needs to be false for the body. */
	cond = get_Eta_cond(get_Loop_eta(first_loop));
	assert(va_region_get_cond(body_region) == cond);

	if (va_region_get_value(body_region)) {
		body_region = va_region_get_link(body_region);
	}

	assert(body_region);

	/* Now there should be a predecessor of the root that is the header. */
	header_region = va_region_get_pred(root_region);
	assert(header_region);

	/* Make the current block the follow block of the loop. Create a new block
	 * that should serve as the block to enter the loop. */
	follow_block = get_nodes_block(first_loop);
	header_block = exact_copy(follow_block);

	/* The code above should guarantee a unique predecessor for blocks that
	 * contain a loop. Make sure that this is the case. */
	assert(get_Block_n_cfgpreds(header_block) == 1);

	/* Construct the headers lane down to the loops branch block. */
	branch_block = vb_build_lane(vbi, vai, header_region, header_block);
	vb_process_nodes(vbi, vai, get_Gamma_true(root), header_block);

	/* Build the branch with the loops condition. */
	cond_jump  = new_r_Cond(branch_block, cond);
	true_proj  = new_r_Proj(cond_jump, mode_X, pn_Cond_true);
	false_proj = new_r_Proj(cond_jump, mode_X, pn_Cond_false);

	/* Connect the follow block and make a start block for the body. */
	set_irn_in(follow_block, 1, &true_proj);
	body_block = new_r_Block(va_get_irg(vai), 1, &false_proj);

	/* Build the bodys lane and connect the end back to the header. */
	end_block = vb_build_lane(vbi, vai, body_region, body_block);
	vb_process_nodes(vbi, vai, get_Gamma_false(root), header_block);

	ins[0] = get_Block_cfgpred(header_block, 0);
	ins[1] = new_r_Jmp(end_block);
	set_irn_in(header_block, 2, ins);

	va_free(vai);
}

static void vb_replace_gating_nodes(vb_info *vbi, vl_info *vli)
{
	plist_element_t *it_gamma;

	vl_eta_it  it;
	ir_node   *eta;

	/* Replace gamma nodes. */
	foreach_plist(vbi->gammas, it_gamma) {
		ir_node *ins[2];

		ir_node *gamma = plist_element_get_value(it_gamma);
		ir_mode *mode  = get_irn_mode(gamma);
		ir_node *block = get_nodes_block(gamma);
		ir_node *phi;

		/* Order defined by vb_build_child. */
		ins[0] = get_Gamma_true(gamma);
		ins[1] = get_Gamma_false(gamma);
		phi    = new_r_Phi(block, 2, ins, mode);

		exchange(gamma, phi);
	}

	foreach_vl_eta(vli, eta, it) {
		vl_theta_it  it_theta;
		ir_node     *value, *theta;

		foreach_vl_eta_theta(vli, eta, theta, it_theta) {
			ir_node *ins[2], *block, *phi;
			ir_mode *mode;

			/* Thetas may be shared among etas. */
			if (is_Deleted(theta)) continue;

			mode  = get_irn_mode(theta);
			block = get_nodes_block(theta);

			/* Order defined by vb_build_loop. */
			ins[0] = get_Theta_init(theta);
			ins[1] = get_Theta_next(theta);
			phi = new_r_Phi(block, 2, ins, mode);

			exchange(theta, phi);
		}

		value = get_Eta_value(eta);
		exchange(eta, value);
	}
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
		ir_dep = vb_dep->loop_copy; /* Get the stored copy. */

	} else if (vb_dep->loop != loop) {
		/* The node belongs to another loop. Clone it. */
		ir_node *clone = exact_copy(ir_dep);

#if VB_DEBUG_BUILD
		printf("Cloning node %ld (now %ld) for loop %ld.\n",
			get_irn_node_nr(ir_dep), get_irn_node_nr(clone),
			get_irn_node_nr(loop)
		);
#endif

		if (is_Theta(ir_dep)) {
			va_loop_loop_it it;
			ir_node *it_loop, *eta;

			foreach_va_loop_eta(loop, it_loop, eta, it) {
				vl_eta_theta_change(vbi->vli, eta, ir_dep, clone);
				printf(
					"eta %ld: theta %ld -> theta %ld.\n",
					get_irn_node_nr(eta), get_irn_node_nr(ir_dep),
					get_irn_node_nr(clone)
				);
			}
		}

		/* That shouldn't happen. */
		assert(!is_Eta(ir_dep));

		/* Remember the clone on the original node. */
		vb_dep->loop      = loop;
		vb_dep->loop_copy = clone;

		/* And then return the clone. */
		ir_dep = clone;
	}

	assert(vb_dep->loop == loop);
	return ir_dep;
}

static void vb_clone_walk(vb_info *vbi, ir_node *irn, ir_node *loop)
{
	int i;

	if (irn_visited(irn)) return;
	mark_irn_visited(irn);

	if (is_Weak(irn)) {
		ir_node *ir_dep = get_Weak_target(irn);

		if (!vb_skip_node(vbi, ir_dep)) {
			/* Clone the target if needed, similar to below. */
			ir_dep = vb_clone_dep(vbi, ir_dep, loop);
			set_Weak_target(irn, ir_dep);
			vb_clone_walk(vbi, ir_dep, loop);
		}
	} else {
		/* Scan dependencies for nodes to copy. */
		for (i = 0; i < get_irn_arity(irn); i++) {
			ir_node *ir_dep = get_irn_n(irn, i);

			/* Skip already assigned nodes. */
			if (vb_skip_node(vbi, ir_dep)) continue;

			/* Clone the node if needed, redirect the edge and recurse. */
			ir_dep = vb_clone_dep(vbi, ir_dep, loop);
			set_irn_n(irn, i, ir_dep); /* May be a no-op. */
			vb_clone_walk(vbi, ir_dep, loop);
		}
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

void vb_build(ir_graph *irg)
{
	ir_node *ins[1], *block, *end, *ret;
	vb_info  vbi;
	vp_info *vpi;
	int      depth;

	/* Partition the graph in subgraphs. NOTE: be careful not to use exchange
	 * on the partitioned graph, as weak links won't be updated. Postpone that
	 * until the graph is combined again. */
	vpi = vp_partition(irg);

	/* Construct the first block to use. */
	ins[0] = get_irg_initial_exec(irg);
	block  = new_r_Block(irg, 1, ins);

	/* Get the return node for construction. */
	end = get_irg_end_block(irg);
	ret = get_Block_cfgpred(end, 0);
	assert(is_Return(ret) && "Invalid VFirm graph.");

	vbi.old_block = get_nodes_block(ret);
	vbi.vli = vp_get_vl_info(vpi);

	obstack_init(&vbi.obst);
	pmap_new_init(&vbi.merges);
	pmap_new_init(&vbi.blocks);

	vbi.gammas = plist_obstack_new(&vbi.obst);
	vbi.todo   = plist_obstack_new(&vbi.obst);
	vbi.phase  = new_phase(irg, vb_init_node);
	phase_set_private(vbi.phase, &vbi);

	dump_ir_graph(irg, "build");

	/* Construct the base layer first. */
	vb_build_base(&vbi, ret, block);

	dump_ir_graph(irg, "build");

	/* Process remaining loop nodes. */
	for (depth = 0; plist_count(vbi.todo) > 0; depth++) {
		plist_element_t *it;

		/* Duplicate nodes utilized by unshared loops. */
		foreach_plist(vbi.todo, it) {
			ir_node *loop = plist_element_get_value(it);
			vb_clone_loop(&vbi, loop);
		}

		dump_ir_graph(irg, "clone");

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
			vb_build_loop(&vbi, vp_get_vl_info(vpi), loop);
			dump_ir_graph(irg, "build");
		}

#if VB_DEBUG_BUILD
		printf("Finished processing loops of depth %d.\n", depth);
#endif
	}

	pmap_new_destroy(&vbi.merges);
	pmap_new_destroy(&vbi.blocks);

	/* Combine the subgraphs again into a single graph. */
	vp_combine(vpi);

	dump_ir_graph(irg, "gated");

	/* Replace gating nodes by phis. */
	vb_replace_gating_nodes(&vbi, vp_get_vl_info(vpi));

	phase_free(vbi.phase);
	plist_free(vbi.todo);
	plist_free(vbi.gammas);
	obstack_free(&vbi.obst, NULL);
	vp_free(vpi);
}
