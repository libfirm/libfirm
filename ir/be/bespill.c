/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Spill module selection; Preparation steps
 * @author      Matthias Braun
 * @date        29.09.2005
 */
#include "irtools.h"
#include "debug.h"
#include "iredges_t.h"
#include "raw_bitset.h"
#include "statev_t.h"
#include "irgwalk.h"

#include "bespill.h"
#include "bemodule.h"
#include "be.h"
#include "belive_t.h"
#include "beirg.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "bera.h"
#include "beintlive_t.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct be_pre_spill_env_t {
	ir_graph                    *irg;
	const arch_register_class_t *cls;
} be_pre_spill_env_t;

static void prepare_constr_insn(be_pre_spill_env_t *env, ir_node *node)
{
	const arch_register_class_t *cls = env->cls;
	ir_node        *block      = get_nodes_block(node);
	const ir_graph *irg        = env->irg;
	be_irg_t       *birg       = be_birg_from_irg(irg);
	be_lv_t        *lv         = be_get_irg_liveness(irg);
	unsigned       *def_constr = NULL;

	/* Insert a copy for constraint inputs attached to a value which can't
	 * fulfill the constraint
	 * (typical example: stack pointer as input to copyb)
	 * TODO: This really just checks precolored registers at the moment and
	 *       ignores the general case of not matching in/out constraints */
	foreach_irn_in(node, i, op) {
		const arch_register_req_t *req = arch_get_irn_register_req_in(node, i);
		if (req->cls != cls)
			continue;

		const arch_register_t *reg = arch_get_irn_register(op);
		if (reg == NULL)
			continue;

		/* Precolored with an ignore register (which is not virtual). */
		if (reg->type & arch_register_type_virtual ||
		    rbitset_is_set(birg->allocatable_regs, reg->global_index))
			continue;

		if (!arch_register_req_is(req, limited))
			continue;
		if (rbitset_is_set(req->limited, reg->index))
			continue;

		ir_node *copy = be_new_Copy(block, op);
		stat_ev_int("constr_copy", 1);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		DBG((dbg, LEVEL_3, "inserting ignore arg copy %+F for %+F pos %d\n",
		     copy, node, i));
	}

	/* insert copies for nodes that occur constrained more than once. */
	int const arity = get_irn_arity(node);
	be_foreach_use(node, cls, req, in, in_req_,
		if (!arch_register_req_is(req, limited))
			continue;

		for (int i2 = i_ + 1; i2 < arity; ++i2) {
			const arch_register_req_t *req2
				= arch_get_irn_register_req_in(node, i2);
			if (req2->cls != cls)
				continue;
			if (!arch_register_req_is(req2, limited))
				continue;

			ir_node *in2 = get_irn_n(node, i2);
			if (in2 != in)
				continue;

			/* if the constraint is the same, no copy is necessary
			 * TODO generalise unequal but overlapping constraints */
			if (rbitsets_equal(req->limited, req2->limited, cls->n_regs))
				continue;

			ir_node *copy = be_new_Copy(block, in);
			stat_ev_int("constr_copy", 1);

			sched_add_before(node, copy);
			set_irn_n(node, i2, copy);
			DBG((dbg, LEVEL_3,
			     "inserting multiple constr copy %+F for %+F pos %d\n",
			     copy, node, i2));
		}
	);

	/* collect all registers occurring in out constraints. */
	be_foreach_definition(node, cls, def, req,
		(void)def;
		if (!arch_register_req_is(req, limited))
			continue;
		if (def_constr == NULL) {
			def_constr = rbitset_alloca(cls->n_regs);
		}
		rbitset_or(def_constr, req->limited, cls->n_regs);
	);

	/* no output constraints => we're good */
	if (def_constr == NULL) {
		return;
	}

	/*
	 * insert copies for all constrained arguments living through the node
	 * and being constrained to a register which also occurs in out constraints.
	 */
	unsigned *const tmp = rbitset_alloca(cls->n_regs);
	be_foreach_use(node, cls, req, in, in_req_,
		/* Check, if
		 * 1) the operand is constrained.
		 * 2) lives through the node.
		 * 3) is constrained to a register occurring in out constraints.
		 */
		if (!arch_register_req_is(req, limited))
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

		ir_node *copy = be_new_Copy(block, in);
		sched_add_before(node, copy);
		set_irn_n(node, i_, copy);
		DBG((dbg, LEVEL_3, "inserting constr copy %+F for %+F pos %d\n",
		     copy, node, i_));
		be_liveness_update(lv, in);
	);
}

static void pre_spill_prepare_constr_walker(ir_node *block, void *data)
{
	be_pre_spill_env_t *env = (be_pre_spill_env_t*)data;
	sched_foreach(block, node) {
		prepare_constr_insn(env, node);
	}
}

void be_pre_spill_prepare_constr(ir_graph *irg,
                                 const arch_register_class_t *cls)
{
	be_pre_spill_env_t env;
	memset(&env, 0, sizeof(env));
	env.irg = irg;
	env.cls = cls;

	be_assure_live_sets(irg);

	irg_block_walk_graph(irg, pre_spill_prepare_constr_walker, NULL, &env);
}



int be_coalesce_spill_slots = 1;
int be_do_remats = 1;

static const lc_opt_table_entry_t be_spill_options[] = {
	LC_OPT_ENT_BOOL ("coalesce_slots", "coalesce the spill slots", &be_coalesce_spill_slots),
	LC_OPT_ENT_BOOL ("remat", "try to rematerialize values instead of reloading", &be_do_remats),
	LC_OPT_LAST
};

static be_module_list_entry_t *spillers;
static be_spill_func           selected_spiller;

void be_register_spiller(const char *name, be_spill_func spiller)
{
	if (selected_spiller == NULL)
		selected_spiller = spiller;
	be_add_module_to_list(&spillers, name, spiller);
}

void be_do_spill(ir_graph *irg, const arch_register_class_t *cls)
{
	assert(selected_spiller != NULL);
	selected_spiller(irg, cls);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_spilloptions)
void be_init_spilloptions(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *spill_grp = lc_opt_get_grp(be_grp, "spill");

	lc_opt_add_table(spill_grp, be_spill_options);
	be_add_module_list_opt(be_grp, "spiller", "spill algorithm",
	                       &spillers, (void**) &selected_spiller);

	FIRM_DBG_REGISTER(dbg, "firm.be.spillprepare");
}
