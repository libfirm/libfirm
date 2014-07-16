/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        22.11.2004
 */
#include <stdlib.h>

#include "irdom.h"
#include "iredges.h"
#include "irgwalk.h"
#include "irmode.h"
#include "irnode.h"
#include "irtools.h"
#include "statev_t.h"

#include "beirg.h"
#include "belive_t.h"
#include "bemodule.h"
#include "benode.h"
#include "bera.h"
#include "besched.h"
#include "beutil.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static be_irg_t      *birg;
static be_lv_t       *lv;
static unsigned long  precol_copies;
static unsigned long  multi_precol_copies;
static unsigned long  constrained_livethrough_copies;

static void prepare_constr_insn(ir_node *const node)
{
	/* Insert a copy for constraint inputs attached to a value which can't
	 * fulfill the constraint
	 * (typical example: stack pointer as input to copyb)
	 * TODO: This really just checks precolored registers at the moment and
	 *       ignores the general case of not matching in/out constraints */
	foreach_irn_in(node, i, op) {
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		if (req->cls == NULL)
			continue;

		const arch_register_t *const reg = arch_get_irn_register(op);
		if (reg == NULL)
			continue;

		/* Precolored with an ignore register (which is not virtual). */
		if ((reg->type & arch_register_type_virtual) ||
		    rbitset_is_set(birg->allocatable_regs, reg->global_index))
			continue;

		if (!arch_register_req_is(req, limited))
			continue;
		if (rbitset_is_set(req->limited, reg->index))
			continue;

		ir_node *block = get_nodes_block(node);
		ir_node *copy  = be_new_Copy(block, op);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		++precol_copies;
		DBG((dbg, LEVEL_3, "inserting ignore arg copy %+F for %+F pos %d\n",
		     copy, node, i));
	}

	/* insert copies for nodes that occur constrained more than once. */
	for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;

		ir_node *in = get_irn_n(node, i);
		const arch_register_req_t *const in_req
			= arch_get_irn_register_req(in);
		if (arch_register_req_is(in_req, ignore))
			continue;
		for (int i2 = i + 1; i2 < arity; ++i2) {
			const arch_register_req_t *const req2
				= arch_get_irn_register_req_in(node, i2);
			if (req2->cls != cls)
				continue;
			if (!arch_register_req_is(req2, limited))
				continue;

			ir_node *in2 = get_irn_n(node, i2);
			if (in2 != in)
				continue;

			/* if the constraint is the same, no copy is necessary
			 * TODO generalise to unequal but overlapping constraints */
			if (rbitsets_equal(req->limited, req2->limited, cls->n_regs))
				continue;

			ir_node *block = get_nodes_block(node);
			ir_node *copy  = be_new_Copy(block, in);
			sched_add_before(node, copy);
			set_irn_n(node, i2, copy);
			++multi_precol_copies;
			DBG((dbg, LEVEL_3,
			     "inserting multiple constr copy %+F for %+F pos %d\n",
			     copy, node, i2));
		}
	}

	/* collect all registers occurring in out constraints. */
	unsigned *def_constr = NULL;
	be_foreach_value(node, value,
		const arch_register_req_t *const req = arch_get_irn_register_req(value);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;
		if (def_constr == NULL) {
			const arch_env_t *const arch_env = birg->main_env->arch_env;
			def_constr = rbitset_alloca(arch_env->n_registers);
		}
		rbitset_foreach(req->limited, cls->n_regs, e) {
			const arch_register_t *reg = arch_register_for_index(cls, e);
			rbitset_set(def_constr, reg->global_index);
		}
	);
	/* no output constraints => we're good */
	if (def_constr == NULL)
		return;

	/* Insert copies for all constrained arguments living through the node and
	 * being constrained to a register which also occurs in out constraints. */
	for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
		/* Check, if
		 * 1) the operand is constrained.
		 * 2) lives through the node.
		 * 3) is constrained to a register occurring in out constraints. */
		const arch_register_req_t *const req
			= arch_get_irn_register_req_in(node, i);
		const arch_register_class_t *const cls = req->cls;
		if (cls == NULL)
			continue;
		if (!arch_register_req_is(req, limited))
			continue;
		ir_node *in = get_irn_n(node, i);
		const arch_register_req_t *const in_req
			= arch_get_irn_register_req(in);
		if (arch_register_req_is(in_req, ignore))
			continue;
		/* Only create the copy if the operand is no copy.
		 * this is necessary since the assure constraints phase inserts
		 * Copies and Keeps for operands which must be different from the
		 * results. Additional copies here would destroy this. */
		if (be_is_Copy(in))
			continue;
		if (!be_values_interfere(node, in))
			continue;

		bool common_limits = false;
		rbitset_foreach(req->limited, cls->n_regs, e) {
			const arch_register_t *reg = arch_register_for_index(cls, e);
			if (rbitset_is_set(def_constr, reg->global_index)) {
				common_limits = true;
				break;
			}
		}
		if (!common_limits)
			continue;

		ir_node *block = get_nodes_block(node);
		ir_node *copy  = be_new_Copy(block, in);
		sched_add_before(node, copy);
		set_irn_n(node, i, copy);
		++constrained_livethrough_copies;
		DBG((dbg, LEVEL_3, "inserting constr copy %+F for %+F pos %d\n",
		     copy, node, i));
		be_liveness_update(lv, in);
	}
}

static void add_missing_copies_in_block(ir_node *block, void *data)
{
	(void)data;
	sched_foreach(block, node) {
		prepare_constr_insn(node);
	}
}

void be_add_missing_copies(ir_graph *irg)
{
	be_assure_live_sets(irg);

	precol_copies                  = 0;
	multi_precol_copies            = 0;
	constrained_livethrough_copies = 0;

	birg = be_birg_from_irg(irg);
	lv   = be_get_irg_liveness(irg);
	irg_block_walk_graph(irg, add_missing_copies_in_block, NULL, NULL);

	stat_ev_ull("ra_precol_copies", precol_copies);
	stat_ev_ull("ra_multi_precol_copies", multi_precol_copies);
	stat_ev_ull("ra_constrained_livethrough_copies",
	            constrained_livethrough_copies);
}

/** The list of register allocators */
static be_module_list_entry_t *register_allocators;
static be_ra_t                *selected_allocator;

void be_register_allocator(const char *name, be_ra_t *allocator)
{
	if (selected_allocator == NULL)
		selected_allocator = allocator;
	be_add_module_to_list(&register_allocators, name, allocator);
}

void be_allocate_registers(ir_graph *irg)
{
	assert(selected_allocator != NULL);
	if (selected_allocator != NULL) {
		selected_allocator->allocate(irg);
	}
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_ra)
void be_init_ra(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "regalloc", "register allocator",
	                       &register_allocators, (void**) &selected_allocator);
	FIRM_DBG_REGISTER(dbg, "firm.be.regalloc");
}
