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

#include "array_t.h"
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

/**
 * Check whether the node has only one user.  Explicitly ignore the anchor.
 */
bool be_has_only_one_user(ir_node *node)
{
	int              n = get_irn_n_edges(node);
	int              n_users;
	const ir_edge_t *edge;

	if (n <= 1)
		return 1;

	n_users = 0;
	foreach_out_edge(node, edge) {
		ir_node *src = get_edge_src_irn(edge);
		/* ignore anchor and keep-alive edges */
		if (is_Anchor(src) || is_End(src))
			continue;
		n_users++;
	}

	return n_users == 1;
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

	if (!be_has_only_one_user(pred))
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
