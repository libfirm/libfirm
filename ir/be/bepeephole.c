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
 * @brief       modifies schedule so flags dependencies are respected.
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "error.h"

#include "beirg_t.h"
#include "belive_t.h"
#include "bearch_t.h"
#include "besched_t.h"

static const arch_env_t *arch_env;
static const be_lv_t    *lv;
ir_node              ***register_values;

static void clear_value(ir_node *node)
{
	const arch_register_t       *reg;
	const arch_register_class_t *cls;
	unsigned                     reg_idx;
	unsigned                     cls_idx;

	if(!mode_is_data(get_irn_mode(node)))
		return;

	reg     = arch_get_irn_register(arch_env, node);
	if(reg == NULL) {
		panic("No register assigned at %+F\n", node);
	}
	if(arch_register_type_is(reg, virtual))
		return;
	cls     = arch_register_get_class(reg);
	reg_idx = arch_register_get_index(reg);
	cls_idx = arch_register_class_index(cls);

	register_values[cls_idx][reg_idx] = NULL;
}

static void set_value(ir_node *node)
{
	const arch_register_t       *reg;
	const arch_register_class_t *cls;
	unsigned                     reg_idx;
	unsigned                     cls_idx;

	if(!mode_is_data(get_irn_mode(node)))
		return;

	reg     = arch_get_irn_register(arch_env, node);
	if(reg == NULL) {
		panic("No register assigned at %+F\n", node);
	}
	if(arch_register_type_is(reg, virtual))
		return;
	cls     = arch_register_get_class(reg);
	reg_idx = arch_register_get_index(reg);
	cls_idx = arch_register_class_index(cls);

#ifdef DEBUG_libfirm
	{
		ir_node *old_value = register_values[cls_idx][reg_idx];
		assert(old_value == NULL || old_value == node);
	}
#endif
	register_values[cls_idx][reg_idx] = node;
}

static void advance(ir_node *node)
{
	int i, arity;

	/* clear values defined */
	if(get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			clear_value(proj);
		}
	} else {
		clear_value(node);
	}

	/* set values used */
	arity = get_irn_arity(node);
	for(i = 0; i < arity; ++i) {
		ir_node *in = get_irn_n(node, i);
		set_value(in);
	}
}

static void process_block(ir_node *block, void *data)
{
	arch_isa_t *isa = arch_env->isa;
	unsigned n_classes;
	unsigned i;
	int l;
	ir_node *node;
	(void) data;

	/* construct initial register assignment */
	n_classes = arch_isa_get_n_reg_class(isa);
	for(i = 0; i < n_classes; ++i) {
		const arch_register_class_t *cls    = arch_isa_get_reg_class(isa, i);
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		memset(register_values[i], 0, sizeof(ir_node*) * n_regs);
	}

	assert(lv->nodes && "live sets must be computed");
	be_lv_foreach(lv, block, be_lv_state_end, l) {
		ir_node *node = be_lv_get_irn(lv, block, l);
		set_value(node);
	}

	/* walk the block */
	sched_foreach_reverse(block, node) {
		if(is_Phi(node))
			break;

		advance(node);
	}
}

void be_peephole_opt(be_irg_t *birg)
{
	arch_isa_t *isa;
	ir_graph   *irg = be_get_birg_irg(birg);
	unsigned n_classes;
	unsigned i;

	/* we sometimes find BadE nodes in float apps like optest_float.c or
	 * kahansum.c for example... */
	be_liveness_invalidate(birg->lv);
	be_liveness_assure_sets(be_assure_liveness(birg));

	arch_env = be_get_birg_arch_env(birg);
	lv       = be_get_birg_liveness(birg);
	isa      = arch_env->isa;

	n_classes = arch_isa_get_n_reg_class(isa);
	register_values = alloca(sizeof(register_values[0]) * n_classes);
	for(i = 0; i < n_classes; ++i) {
		const arch_register_class_t *cls    = arch_isa_get_reg_class(isa, i);
		unsigned                     n_regs = arch_register_class_n_regs(cls);
		register_values[i] = alloca(sizeof(ir_node*) * n_regs);
	}

	irg_block_walk_graph(irg, process_block, NULL, NULL);
}
