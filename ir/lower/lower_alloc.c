/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower (stack-) Alloc nodes to allocate an aligned number of bytes
 * @author  Matthias Braun
 */
#include "lower_alloc.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "ircons.h"
#include "error.h"
#include "irgmod.h"
#include "irnodeset.h"

static bool         lower_constant_sizes;
static unsigned     stack_alignment;
static long         addr_delta;
static ir_nodeset_t transformed;

/**
 * Adjust the size of a node representing a stack alloc to a certain
 * stack_alignment.
 *
 * @param size       the node containing the non-aligned size
 * @param block      the block where new nodes are allocated on
 * @return a node representing the aligned size
 */
static ir_node *adjust_alloc_size(dbg_info *dbgi, ir_node *size, ir_node *block)
{
	if (stack_alignment <= 1)
		return size;
	if (is_Const(size) && !lower_constant_sizes)
		return size;

	ir_mode   *mode = get_irn_mode(size);
	ir_tarval *tv   = new_tarval_from_long(stack_alignment-1, mode);
	ir_graph  *irg  = get_Block_irg(block);
	ir_node   *mask = new_r_Const(irg, tv);
	size = new_rd_Add(dbgi, block, size, mask, mode);
	tv   = new_tarval_from_long(-(long)stack_alignment, mode);
	mask = new_r_Const(irg, tv);
	size = new_rd_And(dbgi, block, size, mask, mode);
	return size;
}

static bool transform_Proj_Alloc(ir_node *node)
{
	/* we might need a result adjustment */
	if (addr_delta == 0)
		return false;
	if (get_Proj_proj(node) != pn_Alloc_res)
		return false;
	if (ir_nodeset_contains(&transformed, node))
		return false;

	ir_node  *const alloc = get_Proj_pred(node);
	dbg_info *const dbgi  = get_irn_dbg_info(alloc);
	ir_graph *const irg   = get_irn_irg(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node  *const delta = new_r_Const_long(irg, mode_P, addr_delta);
	ir_node  *const dummy = new_r_Dummy(irg, mode_P);
	ir_node  *const add   = new_rd_Add(dbgi, block, dummy, delta, mode_P);

	exchange(node, add);
	ir_node *const new_proj = new_r_Proj(alloc, mode_P, pn_Alloc_res);
	set_Add_left(add, new_proj);
	ir_nodeset_insert(&transformed, new_proj);
	return true;
}

/**
 * lower Alloca nodes to allocate "bytes" instead of a certain type
 */
static void lower_alloca_free(ir_node *node, void *data)
{
	bool *changed = (bool*)data;
	(void) data;
	if (is_Alloc(node)) {
	} else if (is_Proj(node)) {
		ir_node *proj_pred = get_Proj_pred(node);
		if (is_Alloc(proj_pred)) {
			*changed |= transform_Proj_Alloc(node);
		}
		return;
	} else {
		return;
	}
	if (!ir_nodeset_insert(&transformed, node))
		return;

	if (stack_alignment <= 1)
		return;

	ir_node  *const size     = get_Alloc_size(node);
	ir_node  *const mem      = get_Alloc_mem(node);
	ir_node  *const block    = get_nodes_block(node);
	dbg_info *const dbgi     = get_irn_dbg_info(node);
	ir_node  *const new_size = adjust_alloc_size(dbgi, size, block);
	ir_node  *const new_node
		= new_rd_Alloc(dbgi, block, mem, new_size, 1);
	ir_nodeset_insert(&transformed, new_node);

	if (new_node != node)
		exchange(node, new_node);
	*changed = true;
}

void lower_alloc(ir_graph *irg, unsigned new_stack_alignment, bool lower_consts,
                 long new_addr_delta)
{
	if (!is_po2(stack_alignment))
		panic("stack alignment not a power of 2");
	addr_delta           = new_addr_delta;
	stack_alignment      = new_stack_alignment;
	lower_constant_sizes = lower_consts;
	bool changed = false;
	ir_nodeset_init(&transformed);
	irg_walk_graph(irg, lower_alloca_free, NULL, &changed);
	ir_nodeset_destroy(&transformed);

	confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_CONTROL_FLOW
	                                    : IR_GRAPH_PROPERTIES_ALL);
}
