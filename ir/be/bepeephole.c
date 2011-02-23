/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief       Peephole optimisation framework keeps track of which registers contain which values
 * @author      Matthias Braun
 * @version     $Id$
 */
#include "config.h"

#include "bepeephole.h"

#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "ircons.h"
#include "irgmod.h"
#include "error.h"

#include "beirg.h"
#include "belive_t.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "bemodule.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static const arch_env_t *arch_env;
static be_lv_t          *lv;
static ir_node          *current_node;
ir_node               ***register_values;

static void clear_reg_value(ir_node *node)
{
	const arch_register_t       *reg;
	const arch_register_class_t *cls;
	unsigned                     reg_idx;
	unsigned                     cls_idx;

	if (!mode_is_data(get_irn_mode(node)))
		return;

	reg     = arch_get_irn_register(node);
	if (reg == NULL) {
		panic("No register assigned at %+F", node);
	}
	if (reg->type & arch_register_type_virtual)
		return;
	cls     = arch_register_get_class(reg);
	reg_idx = arch_register_get_index(reg);
	cls_idx = arch_register_class_index(cls);

	//assert(register_values[cls_idx][reg_idx] != NULL);
	DB((dbg, LEVEL_1, "Clear Register %s\n", reg->name));
	register_values[cls_idx][reg_idx] = NULL;
}

static void set_reg_value(ir_node *node)
{
	const arch_register_t       *reg;
	const arch_register_class_t *cls;
	unsigned                     reg_idx;
	unsigned                     cls_idx;

	if (!mode_is_data(get_irn_mode(node)))
		return;

	reg = arch_get_irn_register(node);
	if (reg == NULL) {
		panic("No register assigned at %+F", node);
	}
	if (reg->type & arch_register_type_virtual)
		return;
	cls     = arch_register_get_class(reg);
	reg_idx = arch_register_get_index(reg);
	cls_idx = arch_register_class_index(cls);

	DB((dbg, LEVEL_1, "Set Register %s: %+F\n", reg->name, node));
	register_values[cls_idx][reg_idx] = node;
}

static void clear_defs(ir_node *node)
{
	/* clear values defined */
	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			clear_reg_value(proj);
		}
	} else {
		clear_reg_value(node);
	}
}

static void set_uses(ir_node *node)
{
	int i, arity;

	/* set values used */
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(node, i);
		set_reg_value(in);
	}
}

void be_peephole_new_node(ir_node * nw)
{
	be_liveness_introduce(lv, nw);
}

/**
 * must be called from peephole optimisations before a node will be killed
 * and its users will be redirected to new_node.
 * so bepeephole can update it's internal state.
 *
 * Note: killing a node and rewiring os only allowed if new_node produces
 * the same registers as old_node.
 */
static void be_peephole_before_exchange(const ir_node *old_node,
                                        ir_node *new_node)
{
	const arch_register_t       *reg;
	const arch_register_class_t *cls;
	unsigned                     reg_idx;
	unsigned                     cls_idx;

	DB((dbg, LEVEL_1, "About to exchange and kill %+F with %+F\n", old_node, new_node));

	if (current_node == old_node) {
		/* next node to be processed will be killed. Its scheduling predecessor
		 * must be processed next. */
		current_node = sched_next(current_node);
		assert (!is_Bad(current_node));
	}

	if (!mode_is_data(get_irn_mode(old_node)))
		return;

	reg = arch_get_irn_register(old_node);
	if (reg == NULL) {
		panic("No register assigned at %+F", old_node);
	}
	assert(reg == arch_get_irn_register(new_node) &&
	      "KILLING a node and replacing by different register is not allowed");

	cls     = arch_register_get_class(reg);
	reg_idx = arch_register_get_index(reg);
	cls_idx = arch_register_class_index(cls);

	if (register_values[cls_idx][reg_idx] == old_node) {
		register_values[cls_idx][reg_idx] = new_node;
	}

	be_liveness_remove(lv, old_node);
}

void be_peephole_exchange(ir_node *old, ir_node *nw)
{
	be_peephole_before_exchange(old, nw);
	sched_remove(old);
	exchange(old, nw);
	be_peephole_new_node(nw);
}

/**
 * block-walker: run peephole optimization on the given block.
 */
static void process_block(ir_node *block, void *data)
{
	unsigned n_classes;
	unsigned i;
	int l;
	(void) data;

	/* construct initial register assignment */
	n_classes = arch_env->n_register_classes;
	for (i = 0; i < n_classes; ++i) {
		const arch_register_class_t *cls    = &arch_env->register_classes[i];
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		memset(register_values[i], 0, sizeof(ir_node*) * n_regs);
	}

	assert(lv->nodes && "live sets must be computed");
	DB((dbg, LEVEL_1, "\nProcessing block %+F (from end)\n", block));
	be_lv_foreach(lv, block, be_lv_state_end, l) {
		ir_node *node = be_lv_get_irn(lv, block, l);
		set_reg_value(node);
	}
	DB((dbg, LEVEL_1, "\nstart processing\n"));

	/* walk the block from last insn to the first */
	current_node = sched_last(block);
	for ( ; !sched_is_begin(current_node);
			current_node = sched_prev(current_node)) {
		ir_op             *op;
		peephole_opt_func  peephole_node;

		assert(!is_Bad(current_node));
		if (is_Phi(current_node))
			break;

		clear_defs(current_node);
		set_uses(current_node);

		op            = get_irn_op(current_node);
		peephole_node = (peephole_opt_func)op->ops.generic;
		if (peephole_node == NULL)
			continue;

		DB((dbg, LEVEL_2, "optimize %+F\n", current_node));
		peephole_node(current_node);
		assert(!is_Bad(current_node));
	}
}

static void kill_node_and_preds(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	int arity, i;

	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);

		set_irn_n(node, i, new_r_Bad(irg));
		if (get_irn_n_edges(pred) != 0)
			continue;

		kill_node_and_preds(pred);
	}

	if (!is_Proj(node))
		sched_remove(node);
	kill_node(node);
}

static void keep_alive_barrier_operand(ir_node *block, const ir_node* barrier, int pos)
{
	ir_node *operand = get_irn_n(barrier, pos);
	ir_node *keep    = sched_next(skip_Proj(operand));

	/* There already is a keep in the schedule. */
	if (be_is_Keep(keep)) {
		const arch_register_class_t *cls = arch_get_irn_reg_class(barrier, pos);

		be_Keep_add_node(keep, cls, operand);
	}
	else {
		ir_node *in[1] = {operand};

		keep = be_new_Keep(block, 1, in);
		sched_add_after(skip_Proj(operand), keep);
	}
}

/**
 * Walk through the block schedule and skip all barrier nodes.
 */
static void skip_barrier(ir_node *block, ir_graph *irg)
{
	ir_node *irn;

	sched_foreach_reverse(block, irn) {
		int       arity;
		unsigned *used;
		size_t    n_used;
		const ir_edge_t *edge, *next;

		if (!be_is_Barrier(irn))
			continue;

		/* track which outputs are actually used, as we have to create
		 * keep nodes for unused outputs */
		arity = get_irn_arity(irn);
		rbitset_alloca(used, arity);

		foreach_out_edge_safe(irn, edge, next) {
			ir_node *proj = get_edge_src_irn(edge);
			int      pn;
			ir_node *pred;

			if (is_Anchor(proj))
				continue;

			pn   = (int) get_Proj_proj(proj);
			pred = get_irn_n(irn, pn);

			rbitset_set(used, pn);

			/* We may need to reschedule be_Keeps to keep live-ranges short. */
			if (get_irn_n_edges(proj) == 1) {
				const ir_edge_t *proj_edge = get_irn_out_edge_first(proj);
				ir_node         *proj_succ = get_edge_src_irn(proj_edge);

				if (be_is_Keep(proj_succ)) {
					int succ_arity = get_irn_arity(proj_succ);

					keep_alive_barrier_operand(block, irn, pn);

					/* Disconnect old be_Keep. */
					if (succ_arity > 1) {
						int      edge_pos        = get_edge_src_pos(proj_edge);
						int      new_arity       = succ_arity - 1;
						int      pos;
						int      new_pos         = 0;
						ir_node *ins[succ_arity];

						for (pos = 0; pos < succ_arity; ++pos) {
							if (pos != edge_pos)
								ins[new_pos++] = get_irn_n(proj_succ, pos);
						}

						set_irn_in(proj_succ, new_arity, ins);
					}
					else {
						sched_remove(proj_succ);
						kill_node(proj_succ);
					}
				}
			}

			edges_reroute_kind(proj, pred, EDGE_KIND_NORMAL, irg);
			edges_reroute_kind(proj, pred, EDGE_KIND_DEP, irg);
		}

		/* the barrier also had the effect of a Keep for unused inputs.
		 * we now have to create an explicit Keep for them */
		n_used = rbitset_popcount(used, arity);
		if (n_used < (size_t) arity) {
			int i;

			for (i = 0; i < arity; ++i) {
				if (rbitset_is_set(used, i))
					continue;

				keep_alive_barrier_operand(block, irn, i);
			}
		}

		kill_node_and_preds(irn);
		break;
	}
}

/**
 * Kill the Barrier nodes for better peephole optimization.
 */
static void kill_barriers(ir_graph *irg)
{
	ir_node *end_blk = get_irg_end_block(irg);
	ir_node *start_blk = get_irg_start_block(irg);
	int i;

	/* skip the barrier on all return blocks */
	for (i = get_Block_n_cfgpreds(end_blk) - 1; i >= 0; --i) {
		ir_node *be_ret = get_Block_cfgpred(end_blk, i);
		ir_node *ret_blk = get_nodes_block(be_ret);

		if (ret_blk == start_blk)
			continue;

		skip_barrier(ret_blk, irg);
	}

	/* skip the barrier on the start block */
	start_blk = get_irg_start_block(irg);
	skip_barrier(start_blk, irg);
}

/**
 * Check whether the node has only one user.  Explicitly ignore the anchor.
 */
static int has_only_one_user(ir_node *node)
{
	int              n = get_irn_n_edges(node);
	const ir_edge_t *edge;

	if (n <= 1)
		return 1;

	if (n > 2)
		return 0;

	foreach_out_edge(node, edge) {
		ir_node *src = get_edge_src_irn(edge);
		if (is_Anchor(src))
			return 1;
	}

	return 0;
}

/*
 * Tries to optimize a beIncSP node with its previous IncSP node.
 * Must be run from a be_peephole_opt() context.
 */
ir_node *be_peephole_IncSP_IncSP(ir_node *node)
{
	int      pred_offs;
	int      curr_offs;
	int      offs;
	ir_node *pred = be_get_IncSP_pred(node);

	if (!be_is_IncSP(pred))
		return node;

	if (!has_only_one_user(pred))
		return node;

	pred_offs = be_get_IncSP_offset(pred);
	curr_offs = be_get_IncSP_offset(node);

	if (pred_offs == BE_STACK_FRAME_SIZE_EXPAND) {
		if (curr_offs != BE_STACK_FRAME_SIZE_SHRINK) {
			return node;
		}
		offs = 0;
	} else if (pred_offs == BE_STACK_FRAME_SIZE_SHRINK) {
		if (curr_offs != BE_STACK_FRAME_SIZE_EXPAND) {
			return node;
		}
		offs = 0;
	} else if (curr_offs == BE_STACK_FRAME_SIZE_EXPAND ||
	           curr_offs == BE_STACK_FRAME_SIZE_SHRINK) {
		return node;
	} else {
		offs = curr_offs + pred_offs;
	}

	/* add node offset to pred and remove our IncSP */
	be_set_IncSP_offset(pred, offs);

	be_peephole_exchange(node, pred);
	return pred;
}

void be_peephole_opt(ir_graph *irg)
{
	unsigned  n_classes;
	unsigned  i;

	/* barrier nodes are used for register allocations. They hinders
	 * peephole optimizations, so remove them here. */
	kill_barriers(irg);

	/* we sometimes find BadE nodes in float apps like optest_float.c or
	 * kahansum.c for example... */
	be_liveness_invalidate(be_get_irg_liveness(irg));
	be_liveness_assure_sets(be_assure_liveness(irg));

	arch_env = be_get_irg_arch_env(irg);
	lv       = be_get_irg_liveness(irg);

	n_classes = arch_env->n_register_classes;
	register_values = XMALLOCN(ir_node**, n_classes);
	for (i = 0; i < n_classes; ++i) {
		const arch_register_class_t *cls    = &arch_env->register_classes[i];
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		register_values[i] = XMALLOCN(ir_node*, n_regs);
	}

	irg_block_walk_graph(irg, process_block, NULL, NULL);

	for (i = 0; i < n_classes; ++i) {
		xfree(register_values[i]);
	}
	xfree(register_values);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_peephole);
void be_init_peephole(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.peephole");
}
