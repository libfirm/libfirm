/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Christoph Mallon
 */

/**
 * @file
 * @brief   Helper for handling 2-address code instructions
 * @author  Christoph Mallon
 */
#include "be2addr.h"

#include "bearch.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "iredges.h"
#include "irgwalk.h"
#include "irnode_t.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool is_irn_reading_reg(ir_node *const node, arch_register_t const *const reg)
{
	foreach_irn_in(node, k, in) {
		if (arch_get_irn_register(in) == reg)
			return true;
	}
	return false;
}

static bool is_irn_writing_reg(ir_node *const node, arch_register_t const *const reg)
{
	be_foreach_out(node, i) {
		if (arch_get_irn_register_out(node, i) == reg)
			return true;
	}
	return false;
}

static ir_node *get_result_node(ir_node *const node, unsigned const pos)
{
	return
		get_irn_mode(node) == mode_T ? get_Proj_for_pn(node, pos) :
		pos == 0                     ? node :
		/*                          */ NULL;
}

static void be_handle_2addr_node(ir_node *const node, be_handle_2addr_callback_t *const callback)
{
	be_foreach_out(node, i) {
		arch_register_req_t const *const req = arch_get_irn_register_req_out(node, i);
		if (req->should_be_same == 0)
			continue;

		unsigned               const same_as = ntz(req->should_be_same);
		ir_node               *const in_node = get_irn_n(node, same_as);
		arch_register_t const *const out_reg = arch_get_irn_register_out(node, i);
		arch_register_t const *const in_reg  = arch_get_irn_register(in_node);
		if (in_reg == out_reg) {
			continue; /* Requirement already fulfilled. */
		} else if (!be_is_Asm(node) && callback(node, req, out_reg)) {
			DBG((dbg, LEVEL_1, "backend callback handled should_be_same constraint at input %u of %+F\n", same_as, node));
			continue;
		} else if (!is_irn_reading_reg(node, out_reg)) {
			/* No-one else is reading the out reg, we can simply copy it.  The register
			 * cannot be live since the operation will override it anyway. */
			ir_node *const copy = be_new_Copy_before_reg(in_node, node, out_reg);
			set_irn_n(node, same_as, copy);
			DBG((dbg, LEVEL_1, "created %+F for should_be_same constraint at input %u of %+F\n", copy, same_as, node));
		} else if (arch_get_irn_register_req_in(node, same_as)->kills_value && !is_irn_writing_reg(node, in_reg)) {
			/* The output register is read by some other input, so a Copy to the right
			 * output register before the instruction is not possible.  But the input
			 * register is killed and no output is writing to the input register.
			 * Resolve this by changing the register of the output to the one of the
			 * input (fulfilling the same-as constraint) and inserting a Copy from the
			 * new to the old output register after the instruction. */
			arch_set_irn_register_out(node, i, in_reg);
			/* Insert the Copy only if the result is used. */
			ir_node *const res = get_result_node(node, i);
			if (res) {
				ir_node *const block = get_nodes_block(node);
				ir_node *const copy  = be_new_Copy(block, res);
				arch_set_irn_register_out(copy, 0, out_reg);
				sched_add_after(node, copy);
				edges_reroute_except(res, copy, copy);
				DBG((dbg, LEVEL_1, "created %+F for should_be_same constraint at output %u of %+F\n", copy, i, node));
			}
		} else {
			/* TODO There can be more complex cases which could be resolved by
			 * parallel copies before or after the instruction. */
			panic("unresolvable should_be_same constraint");
		}
	}
}

static void be_handle_2addr_walker(ir_node *const block, void *const env)
{
	be_handle_2addr_callback_t *const callback = (be_handle_2addr_callback_t*)env;

	sched_foreach_safe(block, irn) {
		if (be_is_Copy(irn) || be_is_CopyKeep(irn) || be_is_Perm(irn) || is_Phi(irn))
			continue;
		be_handle_2addr_node(irn, callback);
	}
}

static bool be_default_handle_2addr_callback(ir_node *const node, arch_register_req_t const *const req, arch_register_t const *const reg)
{
	(void)node, (void)req, (void)reg;
	return false;
}

void be_handle_2addr(ir_graph *const irg, be_handle_2addr_callback_t *callback)
{
	if (!callback)
		callback = &be_default_handle_2addr_callback;
	irg_block_walk_graph(irg, NULL, be_handle_2addr_walker, callback);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_2addr)
void be_init_2addr(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.2addr");
}
