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
 * @brief       Spill module selection; Preparation steps
 * @author      Matthias Braun
 * @date        29.09.2005
 * @version     $Id$
 */
#include "config.h"

#include "irtools.h"
#include "debug.h"
#include "iredges_t.h"
#include "adt/raw_bitset.h"
#include "statev.h"
#include "irgwalk.h"

#include "bespill.h"
#include "bemodule.h"
#include "be.h"
#include "belive_t.h"
#include "beirg.h"
#include "bearch.h"
#include "benode_t.h"
#include "besched.h"
#include "bera.h"
#include "beintlive_t.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct be_pre_spill_env_t {
	be_irg_t                    *birg;
	const arch_register_class_t *cls;
} be_pre_spill_env_t;

static void prepare_constr_insn(be_pre_spill_env_t *env, ir_node *node)
{
	const arch_register_class_t *cls = env->cls;
	ir_node  *block      = get_nodes_block(node);
	const be_irg_t *birg = env->birg;
	be_lv_t *lv          = birg->lv;
	unsigned *tmp        = NULL;
	unsigned *def_constr = NULL;
	int       arity      = get_irn_arity(node);

	int i, i2;

	/* Insert a copy for constraint inputs attached to a value which can't
	 * fullfil the constraint
	 * (typical example: stack pointer as input to copyb)
	 * TODO: This really just checks precolored registers at the moment and
	 *       ignore the general case of not matching in/out constraints
	 */
	for (i = 0; i < arity; ++i) {
		ir_node *op = get_irn_n(node, i);
		ir_node *copy;
		const arch_register_t *reg;
		const arch_register_req_t *req;

		req = arch_get_register_req(node, i);
		if (req->cls != cls)
			continue;
		reg = arch_get_irn_register(op);
		if (reg == NULL)
			continue;

		/* precolored with an ignore register (which is not a joker like
		   unknown/noreg) */
		if (arch_register_type_is(reg, joker)
				|| !arch_register_type_is(reg, ignore))
			continue;

		if (! (req->type & arch_register_req_type_limited))
			continue;
		if (rbitset_is_set(req->limited, reg->index))
			continue;

		copy = be_new_Copy(cls, block, op);
		stat_ev_int("constr_copy", 1);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		DBG((dbg, LEVEL_3, "inserting ignore arg copy %+F for %+F pos %d\n", copy, node, i));
	}

	/* insert copies for nodes that occur constrained more than once. */
	for (i = 0; i < arity; ++i) {
		ir_node                   *in;
		ir_node                   *copy;
		const arch_register_req_t *req;

		req = arch_get_register_req(node, i);
		if (req->cls != cls)
			continue;

		if (! (req->type & arch_register_req_type_limited))
			continue;

		in = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, in))
			continue;

		for (i2 = i + 1; i2 < arity; ++i2) {
			ir_node *in2;
			const arch_register_req_t *req2;

			req2 = arch_get_register_req(node, i2);
			if (req2->cls != cls)
				continue;
			if (! (req2->type & arch_register_req_type_limited))
				continue;

			in2 = get_irn_n(node, i2);
			if (in2 != in)
				continue;

			/* if the constraint is the same, no copy is necessary
			 * TODO generalise unequal but overlapping constraints */
			if (rbitset_equal(req->limited, req2->limited, cls->n_regs))
				continue;

#if 0
			/* Matze: looks fishy to me disabled it for now */
			if (be_is_Copy(get_irn_n(insn->irn, a_op->pos)))
				continue;
#endif

			copy = be_new_Copy(cls, block, in);
			stat_ev_int("constr_copy", 1);

			sched_add_before(node, copy);
			set_irn_n(node, i2, copy);
			DBG((dbg, LEVEL_3,
			     "inserting multiple constr copy %+F for %+F pos %d\n",
			     copy, node, i2));
		}
	}

	/* collect all registers occurring in out constraints. */
	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;

		foreach_out_edge(node, edge) {
			ir_node                   *proj = get_edge_src_irn(edge);
			const arch_register_req_t *req  = arch_get_register_req_out(proj);
			if (! (req->type & arch_register_req_type_limited))
				continue;

			if (def_constr == NULL) {
				rbitset_alloca(def_constr, cls->n_regs);
			}
			rbitset_or(def_constr, req->limited, cls->n_regs);
		}
	} else {
		const arch_register_req_t *req = arch_get_register_req_out(node);
		if (req->type & arch_register_req_type_limited) {
			rbitset_alloca(def_constr, cls->n_regs);
			rbitset_or(def_constr, req->limited, cls->n_regs);
		}
	}

	/* no output constraints => we're good */
	if (def_constr == NULL) {
		return;
	}

	/*
	 * insert copies for all constrained arguments living through the node
	 * and being constrained to a register which also occurs in out constraints.
	 */
	rbitset_alloca(tmp, cls->n_regs);
	for (i = 0; i < arity; ++i) {
		const arch_register_req_t *req;
		ir_node                   *in;
		ir_node                   *copy;

		/*
		 * Check, if
		 * 1) the operand is constrained.
		 * 2) lives through the node.
		 * 3) is constrained to a register occurring in out constraints.
		 */
		req = arch_get_register_req(node, i);
		if (req->cls != cls)
			continue;
		if (! (req->type & arch_register_req_type_limited))
			continue;

		in = get_irn_n(node, i);
		if (!arch_irn_consider_in_reg_alloc(cls, in))
			continue;
		if (!be_values_interfere(lv, node, in))
			continue;

		rbitset_copy(tmp, req->limited, cls->n_regs);
		rbitset_and(tmp, def_constr, cls->n_regs);

		if (rbitset_is_empty(tmp, cls->n_regs))
			continue;

		/*
		 * only create the copy if the operand is no copy.
		 * this is necessary since the assure constraints phase inserts
		 * Copies and Keeps for operands which must be different from the
		 * results. Additional copies here would destroy this.
		 */
		if (be_is_Copy(in))
			continue;

		copy = be_new_Copy(cls, block, in);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		DBG((dbg, LEVEL_3, "inserting constr copy %+F for %+F pos %d\n",
		     copy, node, i));
		be_liveness_update(lv, in);
	}
}

static void pre_spill_prepare_constr_walker(ir_node *block, void *data)
{
	be_pre_spill_env_t *env = data;
	ir_node *node;
	sched_foreach(block, node) {
		prepare_constr_insn(env, node);
	}
}

void be_pre_spill_prepare_constr(be_irg_t *birg,
                                 const arch_register_class_t *cls)
{
	ir_graph *irg = birg->irg;
	be_pre_spill_env_t env;
	memset(&env, 0, sizeof(env));
	env.birg = birg;
	env.cls  = cls;
	irg_block_walk_graph(irg, pre_spill_prepare_constr_walker, NULL, &env);
}



int be_coalesce_spill_slots = 1;
int be_do_remats = 1;

static const lc_opt_table_entry_t be_spill_options[] = {
	LC_OPT_ENT_BOOL ("coalesce_slots", "coalesce the spill slots", &be_coalesce_spill_slots),
	LC_OPT_ENT_BOOL ("remat", "try to rematerialize values instead of reloading", &be_do_remats),
	LC_OPT_LAST
};

static be_module_list_entry_t *spillers = NULL;
static const be_spiller_t *selected_spiller = NULL;

void be_register_spiller(const char *name, be_spiller_t *spiller)
{
	if (selected_spiller == NULL)
		selected_spiller = spiller;
	be_add_module_to_list(&spillers, name, spiller);
}

void be_do_spill(be_irg_t *birg, const arch_register_class_t *cls)
{
	assert(selected_spiller != NULL);
	if (selected_spiller != NULL) {
		selected_spiller->spill(birg, cls);
	}
}

void be_init_spilloptions(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
	be_add_module_list_opt(spill_grp, "spiller", "spill algorithm",
	                       &spillers, (void**) &selected_spiller);

	FIRM_DBG_REGISTER(dbg, "firm.be.spillprepare");
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spilloptions);
