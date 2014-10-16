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
#include "belive.h"
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
		if (!be_value_live_after(in, node))
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

static ir_node *add_to_keep(ir_node *last_keep,
                            const arch_register_class_t *cls, ir_node *node)
{
	if (last_keep != NULL) {
		be_Keep_add_node(last_keep, cls, node);
	} else {
		ir_node *in[1] = { node };
		ir_node *block = get_nodes_block(node);
		ir_node *schedpoint;
		last_keep = be_new_Keep(block, 1, in);

		schedpoint = skip_Proj(node);
		if (sched_is_scheduled(schedpoint)) {
			sched_add_after(schedpoint, last_keep);
		}
	}
	return last_keep;
}

/**
 * Tests whether a node has a real user and is not just kept by the End or
 * Anchor node
 */
static bool has_real_user(const ir_node *node)
{
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (!is_End(user) && !is_Anchor(user))
			return true;
	}
	return false;
}

static void add_missing_keep_walker(ir_node *node, void *data)
{
	(void)data;
	ir_mode *mode = get_irn_mode(node);
	ir_node *last_keep;

	if (mode != mode_T) {
		if (!has_real_user(node)) {
			const arch_register_req_t   *req = arch_get_irn_register_req(node);
			const arch_register_class_t *cls = req->cls;
			if (cls == NULL
					|| (cls->flags & arch_register_class_flag_manual_ra)) {
				return;
			}

			add_to_keep(NULL, cls, node);
		}
		return;
	}

	unsigned n_outs = arch_get_irn_n_outs(node);
	if (n_outs <= 0)
		return;

	unsigned *const found_projs    = rbitset_alloca(n_outs);
	ir_node **const existing_projs = ALLOCANZ(ir_node*, n_outs);
	foreach_out_edge(node, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		ir_mode *mode = get_irn_mode(succ);

		/* The node could be kept */
		if (is_End(succ) || is_Anchor(succ))
			continue;
		if (mode == mode_M || mode == mode_X)
			continue;
		unsigned pn = get_Proj_num(succ);
		existing_projs[pn] = succ;
		if (!has_real_user(succ))
			continue;

		assert(pn < n_outs);
		rbitset_set(found_projs, pn);
	}

	/* are keeps missing? */
	last_keep = NULL;
	for (unsigned i = 0; i < n_outs; ++i) {
		ir_node                     *value;
		const arch_register_req_t   *req;
		const arch_register_class_t *cls;

		if (rbitset_is_set(found_projs, i)) {
			continue;
		}

		req = arch_get_irn_register_req_out(node, i);
		cls = req->cls;
		if (cls == NULL || (cls->flags & arch_register_class_flag_manual_ra)) {
			continue;
		}

		value = existing_projs[i];
		if (value == NULL)
			value = new_r_Proj(node, arch_register_class_mode(cls), i);
		last_keep = add_to_keep(last_keep, cls, value);
	}
}

void be_add_missing_keeps(ir_graph *irg)
{
	irg_walk_graph(irg, add_missing_keep_walker, NULL, NULL);
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
