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

#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodeset.h"

static unsigned po2_stack_alignment;

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
	/* Example: po2_alignment 4 (align to 16 bytes):
	 *   size = (size+15) & 0xfff...f8 */
	ir_mode   *mode    = get_irn_mode(size);
	ir_graph  *irg     = get_irn_irg(block);
	ir_tarval *allone  = get_mode_all_one(mode);
	ir_tarval *shr     = tarval_shr_unsigned(allone, po2_stack_alignment);
	ir_tarval *mask    = tarval_shl_unsigned(shr, po2_stack_alignment);
	ir_tarval *invmask = tarval_not(mask);
	ir_node   *addv    = new_r_Const(irg, invmask);
	ir_node   *add     = new_rd_Add(dbgi, block, size, addv);
	ir_node   *maskc   = new_r_Const(irg, mask);
	ir_node   *and     = new_rd_And(dbgi, block, add, maskc);
	return and;
}

/**
 * lower Alloca nodes to allocate "bytes" instead of a certain type
 */
static void lower_node(ir_node *node, void *data)
{
	bool *changed = (bool*)data;
	if (!is_Alloc(node))
		return;

	ir_node  *const size     = get_Alloc_size(node);
	ir_node  *const mem      = get_Alloc_mem(node);
	ir_node  *const block    = get_nodes_block(node);
	dbg_info *const dbgi     = get_irn_dbg_info(node);
	ir_node  *const new_size = adjust_alloc_size(dbgi, size, block);
	ir_node  *const new_node
		= new_rd_Alloc(dbgi, block, mem, new_size, 1);

	if (new_node != node)
		exchange(node, new_node);
	*changed = true;
}

void lower_alloc(ir_graph *irg, unsigned new_po2_stack_alignment)
{
	if (new_po2_stack_alignment == 0)
		return;

	po2_stack_alignment = new_po2_stack_alignment;
	bool changed = false;
	irg_walk_graph(irg, NULL, lower_node, &changed);

	confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_CONTROL_FLOW
	                                    : IR_GRAPH_PROPERTIES_ALL);
}
