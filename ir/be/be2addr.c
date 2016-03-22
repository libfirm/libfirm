/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Christoph Mallon
 */

/**
 * @file
 * @brief   Helpers for handling 2-address code instructions
 * @author  Christoph Mallon
 */

#include "be2addr.h"

#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "irgwalk.h"
#include "irnode_t.h"

static bool is_irn_reading_reg(ir_node *const node, arch_register_t const *const reg)
{
	foreach_irn_in(node, k, in) {
		if (arch_get_irn_register(in) == reg)
			return true;
	}
	return false;
}

static void be_handle_2addr_node(ir_node *const node, be_handle_2addr_callback_t *const handle_2addr_callback)
{
	be_foreach_out(node, i) {
		arch_register_req_t const *const req     = arch_get_irn_register_req_out(node, i);
		same_as_t                  const same_as = req->same_as;
		if (same_as == BE_NOT_SAME)
			continue;

		ir_node               *const in_node = get_irn_n(node, same_as);
		arch_register_t const *const out_reg = arch_get_irn_register_out(node, i);
		if (arch_get_irn_register(in_node) == out_reg) {
			continue; /* Requirement already fulfilled. */
		} else if (!be_is_Asm(node) && handle_2addr_callback(node, req, out_reg)) {
			continue; /* Was handled by callback. */
		} else if (!is_irn_reading_reg(node, out_reg)) {
			/* No-one else is reading the out reg, we can simply copy it.  The register
			 * cannot be live since the operation will override it anyway. */
			ir_node *const copy = be_new_Copy_before_reg(in_node, node, out_reg);
			set_irn_n(node, same_as, copy);
		} else {
			panic("unresolvable same-as constraint");
		}
	}
}

static void be_handle_2addr_walker(ir_node *const block, void *const env)
{
	be_handle_2addr_callback_t *const handle_2addr_callback = (be_handle_2addr_callback_t*)env;

	sched_foreach_safe(block, irn) {
		if (be_is_Copy(irn) || be_is_CopyKeep(irn) || be_is_Perm(irn) || is_Phi(irn))
			continue;
		be_handle_2addr_node(irn, handle_2addr_callback);
	}
}

static bool be_default_handle_2addr_callback(ir_node *const node, arch_register_req_t const *const req, arch_register_t const *const reg)
{
	(void)node, (void)req, (void)reg;
	return false;
}

void be_handle_2addr(ir_graph *const irg, be_handle_2addr_callback_t *handle_2addr_callback)
{
	if (!handle_2addr_callback)
		handle_2addr_callback = &be_default_handle_2addr_callback;
	irg_block_walk_graph(irg, NULL, be_handle_2addr_walker, handle_2addr_callback);
}
