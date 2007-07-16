/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       be transform helper extracted from the ia32 backend.
 * @author      Matthias Braun, Michael Beck
 * @date        14.06.2007
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "pdeq.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "ircons_t.h"
#include "irhooks.h"
#include "iredges.h"
#include "irouts.h"
#include "trouts.h"
#include "cgana.h"
#include "debug.h"

#include "beirg_t.h"
#include "betranshlp.h"

typedef struct be_transform_env_t {
	ir_graph *irg;         /**< The irg, the node should be created in */
	int      visited;      /**< visited count that indicates whether a
	                            node is already transformed */
	waitq    *worklist;    /**< worklist of nodes that still need to be
	                            transformed */
	ir_node  *old_anchor;  /**< the old anchor node in the old irg */
} be_transform_env_t;


static be_transform_env_t env;

void be_set_transformed_node(ir_node *old_node, ir_node *new_node) {
	set_irn_link(old_node, new_node);
}

static INLINE ir_node *be_get_transformed_node(ir_node *old_node) {
	assert(irn_visited(old_node));
	return (ir_node*) get_irn_link(old_node);
}

void be_duplicate_deps(ir_node *old_node, ir_node *new_node) {
	int i;
	int deps = get_irn_deps(old_node);

	for (i = 0; i < deps; ++i) {
		ir_node *dep     = get_irn_dep(old_node, i);
		ir_node *new_dep = be_transform_node(dep);

		add_irn_dep(new_node, new_dep);
	}
}

ir_node *be_duplicate_node(ir_node *node) {
	ir_node  *block = be_transform_node(get_nodes_block(node));
	ir_graph *irg   = env.irg;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_op    *op    = get_irn_op(node);
	ir_node  *new_node;
	int      i, arity;

	arity = get_irn_arity(node);
	if (op->opar == oparity_dynamic) {
		new_node = new_ir_node(dbgi, irg, block, op, mode, -1, NULL);
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			in = be_transform_node(in);
			add_irn_n(new_node, in);
		}
	} else {
		ir_node **ins = alloca(arity * sizeof(ins[0]));
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			ins[i] = be_transform_node(in);
		}

		new_node = new_ir_node(dbgi, irg, block, op, mode, arity, ins);
	}

	copy_node_attr(node, new_node);
	be_duplicate_deps(node, new_node);

#ifdef DEBUG_libfirm
	new_node->node_nr = node->node_nr;
#endif

	return new_node;
}

/**
 * Calls transformation function for given node and marks it visited.
 */
ir_node *be_transform_node(ir_node *node) {
	ir_node *new_node;
	ir_op   *op;

	if (irn_visited(node)) {
		new_node = be_get_transformed_node(node);
		assert(new_node != NULL);
		return new_node;
	}

	mark_irn_visited(node);
	DEBUG_ONLY(be_set_transformed_node(node, NULL));

	op = get_irn_op(node);
	if (op->ops.generic) {
		be_transform_func *transform = (be_transform_func *)op->ops.generic;

		new_node = transform(node);
		assert(new_node != NULL);
	} else {
		new_node = be_duplicate_node(node);
	}

	be_set_transformed_node(node, new_node);
	mark_irn_visited(new_node);
	hook_dead_node_elim_subst(current_ir_graph, node, new_node);
	return new_node;
}

/**
 * enqueue all inputs into the transform queue.
 */
void be_enqueue_preds(ir_node *node) {
	int i, arity;

	/* put the preds in the worklist */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		pdeq_putr(env.worklist, pred);
	}
}

/**
 * Rewire nodes which are potential loops (like Phis) to avoid endless loops.
 */
static void fix_loops(ir_node *node) {
	int i, arity;

	assert(node_is_in_irgs_storage(env.irg, node));

	if (irn_visited(node))
		return;

	mark_irn_visited(node);

	if (! is_Block(node)) {
		ir_node *block     = get_nodes_block(node);
		ir_node *new_block = get_irn_link(block);

		if (new_block != NULL) {
			set_nodes_block(node, new_block);
			block = new_block;
		}

		fix_loops(block);
	}

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(node, i);
		ir_node *nw = get_irn_link(in);

		if (nw != NULL && nw != in) {
			set_irn_n(node, i, nw);
			in = nw;
		}

		fix_loops(in);
	}

	arity = get_irn_deps(node);
	for (i = 0; i < arity; ++i) {
		ir_node *in = get_irn_dep(node, i);
		ir_node *nw = get_irn_link(in);

		if (nw != NULL && nw != in) {
			set_irn_dep(node, i, nw);
			in = nw;
		}

		fix_loops(in);
	}
}

ir_node *be_pre_transform_node(ir_node *place) {
	if (place == NULL)
		return NULL;

	return be_transform_node(place);
}

ir_node *be_get_old_anchor(int anchor)
{
	return get_irn_n(env.old_anchor, anchor);
}

static void pre_transform_anchor(int anchor)
{
	ir_node *old_anchor_node = get_irn_n(env.old_anchor, anchor);
	ir_node *transformed     = be_transform_node(old_anchor_node);
	set_irg_anchor(current_ir_graph, anchor, transformed);
}

/**
 * Transforms all nodes. Deletes the old obstack and creates a new one.
 */
static void transform_nodes(ir_graph *irg, arch_pretrans_nodes *pre_transform,
                            void *cg)
{
	int       i;
	ir_node  *old_end, *new_anchor;

	hook_dead_node_elim(irg, 1);

	inc_irg_visited(irg);

	env.irg         = irg;
	env.visited     = get_irg_visited(irg);
	env.worklist    = new_waitq();
	env.old_anchor  = irg->anchor;

	old_end = get_irg_end(irg);

	/* put all anchor nodes in the worklist */
	for (i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *anchor = get_irg_anchor(irg, i);

		if (anchor == NULL)
			continue;
		waitq_put(env.worklist, anchor);
	}

	new_anchor  = new_Anchor(irg);
	irg->anchor = new_anchor;

	/* pre transform some anchors (so they are available in the other transform
	 * functions) */
	pre_transform_anchor(anchor_bad);
	pre_transform_anchor(anchor_no_mem);
	pre_transform_anchor(anchor_start_block);
	pre_transform_anchor(anchor_start);
	pre_transform_anchor(anchor_frame);

	if (pre_transform)
		(*pre_transform)(cg);

	/* process worklist (this should transform all nodes in the graph) */
	while (! waitq_empty(env.worklist)) {
		ir_node *node = waitq_get(env.worklist);
		be_transform_node(node);
	}

	/* fix loops and set new anchors*/
	inc_irg_visited(irg);
	for (i = get_irg_n_anchors(irg) - 1; i >= 0; --i) {
		ir_node *anchor = get_irn_n(env.old_anchor, i);

		if (anchor == NULL)
			continue;

		anchor = get_irn_link(anchor);
		fix_loops(anchor);
		set_irn_n(new_anchor, i, anchor);
	}
	set_irn_n(new_anchor, -1, get_irg_anchor(irg, anchor_end_block));

	del_waitq(env.worklist);
	free_End(old_end);
	hook_dead_node_elim(irg, 0);
}

/**
 * Transform helper for blocks.
 */
static ir_node *gen_Block(ir_node *node) {
	ir_graph *irg             = current_ir_graph;
	dbg_info *dbgi            = get_irn_dbg_info(node);
	ir_node  *old_start_block = get_irn_n(env.old_anchor, anchor_start_block);
	ir_node  *block;

	/*
	 * We replace the ProjX from the start node with a jump,
	 * so the startblock has no preds anymore now
	 */
	if (node == old_start_block) {
		return new_rd_Block(dbgi, irg, 0, NULL);
	}

	/* we use the old blocks for now, because jumps allow cycles in the graph
	 * we have to fix this later */
	block = new_ir_node(dbgi, irg, NULL, get_irn_op(node), get_irn_mode(node),
	                    get_irn_arity(node), get_irn_in(node) + 1);
	copy_node_attr(node, block);

#ifdef DEBUG_libfirm
	block->node_nr = node->node_nr;
#endif
	be_set_transformed_node(node, block);

	/* put the preds in the worklist */
	be_enqueue_preds(node);

	return block;
}

static ir_node *gen_End(ir_node *node) {
	/* end has to be duplicated manually because we need a dynamic in array */
	ir_graph *irg   = current_ir_graph;
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_node(get_nodes_block(node));
	int      i, arity;
	ir_node  *new_end;

	new_end = new_ir_node(dbgi, irg, block, op_End, mode_X, -1, NULL);
	copy_node_attr(node, new_end);
	be_duplicate_deps(node, new_end);

	set_irg_end(irg, new_end);
	be_set_transformed_node(new_end, new_end);

	/* transform preds */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *in     = get_irn_n(node, i);
		ir_node *new_in = be_transform_node(in);

		add_End_keepalive(new_end, new_in);
	}

	return new_end;
}

void be_transform_graph(be_irg_t *birg, arch_pretrans_nodes *func, void *cg)
{
	ir_graph *irg = birg->irg;
	ir_graph *old_current_ir_graph = current_ir_graph;
	int old_interprocedural_view = get_interprocedural_view();
	struct obstack *old_obst = NULL;
	struct obstack *new_obst = NULL;

	current_ir_graph = irg;
	set_interprocedural_view(0);

	/* most analysis info is wrong after transformation */
	free_callee_info(irg);
	free_irg_outs(irg);
	free_trouts();
	free_loop_information(irg);
	set_irg_doms_inconsistent(irg);

	be_liveness_invalidate(be_get_birg_liveness(birg));
	be_invalidate_dom_front(birg);

	/* create a new obstack */
	old_obst = irg->obst;
	new_obst = xmalloc(sizeof(*new_obst));
	obstack_init(new_obst);
	irg->obst = new_obst;
	irg->last_node_idx = 0;

	/* create new value table for CSE */
	del_identities(irg->value_table);
	irg->value_table = new_identities();

	/* enter special helper */
	op_Block->ops.generic = (op_func)gen_Block;
	op_End->ops.generic   = (op_func)gen_End;

	/* do the main transformation */
	transform_nodes(irg, func, cg);

	/* we don't want the globals anchor anymore */
	set_irg_globals(irg, new_r_Bad(irg));

	/* free the old obstack */
	obstack_free(old_obst, 0);
	xfree(old_obst);

	/* restore state */
	current_ir_graph = old_current_ir_graph;
	set_interprocedural_view(old_interprocedural_view);

	/* recalculate edges */
	edges_deactivate(irg);
	edges_activate(irg);

	if (birg->lv) {
		be_liveness_free(birg->lv);
		birg->lv = be_liveness(birg->irg);
	}
}
