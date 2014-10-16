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
