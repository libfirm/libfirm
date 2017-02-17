/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower some High-level constructs, moved from the firmlower.
 * @author  Boris Boesler, Goetz Lindenmaier, Michael Beck
 */
#include "entity_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irprog_t.h"
#include "lowering.h"
#include "typerep.h"

/**
 * Lower a Sel node. Do not touch Sels accessing entities on the frame type.
 */
static void lower_sel(ir_node *sel)
{
	ir_node *const ptr          = get_Sel_ptr(sel);
	ir_type *const type         = get_Sel_type(sel);
	ir_type *const element_type = get_array_element_type(type);
	unsigned const element_size = get_type_size(element_type);

	ir_node *newn;
	if (element_size == 0) {
		newn = ptr;
	} else {
		dbg_info *const dbg         = get_irn_dbg_info(sel);
		ir_node  *const index       = get_Sel_index(sel);
		ir_node  *const bl          = get_nodes_block(sel);
		ir_mode  *const mode        = get_irn_mode(sel);
		ir_mode  *const offset_mode = get_reference_offset_mode(mode);
		ir_node  *const idx_conv    = new_rd_Conv(dbg, bl, index, offset_mode);

		ir_node  *scaled_index;
		if (element_size == 1) {
			scaled_index = idx_conv;
		} else {
			ir_graph *const irg      = get_irn_irg(sel);
			ir_node  *const el_size  = new_rd_Const_long(dbg, irg, offset_mode,
			                                             element_size);
			scaled_index = new_rd_Mul(dbg, bl, idx_conv, el_size);
		}
		newn = new_rd_Add(dbg, bl, ptr, scaled_index);
	}

	hook_lower(sel);
	exchange(sel, newn);
}

/**
 * Lower a Member node. Do not touch Members accessing entities on the frame
 * type.
 */
static void lower_member(ir_node *member)
{
	ir_graph  *const irg   = get_irn_irg(member);
	ir_entity *const ent   = get_Member_entity(member);
	ir_type   *const owner = get_entity_owner(ent);
	/* we can only replace Members when the owner type layout is decided. */
	if (get_type_state(owner) != layout_fixed)
		return;

	int       const offset = get_entity_offset(ent);
	ir_node  *const ptr   = get_Member_ptr(member);

	/* replace Member by add(ptr, const(ent.offset)) */
	ir_node *newn;
	if (offset != 0) {
		dbg_info *const dbg         = get_irn_dbg_info(member);
		ir_mode  *const mode        = get_irn_mode(member);
		ir_node  *const bl          = get_nodes_block(member);
		ir_mode  *const mode_offset = get_reference_offset_mode(mode);
		ir_node  *const cnst
			= new_r_Const_long(irg, mode_offset, offset);
		newn = new_rd_Add(dbg, bl, ptr, cnst);
	} else {
		newn = ptr;
	}

	hook_lower(member);
	exchange(member, newn);
}

static void replace_by_Const(ir_node *const node, long const value)
{
	ir_graph *const irg  = get_irn_irg(node);
	ir_mode  *const mode = get_irn_mode(node);
	ir_node  *const newn = new_r_Const_long(irg, mode, value);
	/* run the hooks */
	hook_lower(node);
	exchange(node, newn);
}

/**
 * Lower an Offset node.
 */
static void lower_offset(ir_node *const offset)
{
	/* rewrite the Offset node by a Const node */
	ir_entity *const ent = get_Offset_entity(offset);
	assert(get_type_state(get_entity_type(ent)) == layout_fixed);
	replace_by_Const(offset, get_entity_offset(ent));
}

/**
 * Lower an Align node.
 */
static void lower_align(ir_node *const align)
{
	ir_type *const tp = get_Align_type(align);
	assert(get_type_state(tp) == layout_fixed);
	/* rewrite the Align node by a Const node */
	replace_by_Const(align, get_type_alignment(tp));
}

/**
 * Lower a Size node.
 */
static void lower_size(ir_node *const size)
{
	ir_type *const tp = get_Size_type(size);
	assert(get_type_state(tp) == layout_fixed);
	/* rewrite the Size node by a Const node */
	replace_by_Const(size, get_type_size(tp));
}

/**
 * lowers IR-nodes, called from walker
 */
static void lower_irnode(ir_node *irn, void *env)
{
	(void) env;
	switch (get_irn_opcode(irn)) {
	case iro_Align:  lower_align(irn);  break;
	case iro_Member: lower_member(irn); break;
	case iro_Offset: lower_offset(irn); break;
	case iro_Sel:    lower_sel(irn);    break;
	case iro_Size:   lower_size(irn);   break;
	default:         break;
	}
}

void lower_highlevel_graph(ir_graph *irg)
{
	/* Finally: lower Offset/TypeConst-size and Sel nodes, unaligned Load/Stores. */
	irg_walk_graph(irg, NULL, lower_irnode, NULL);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_CONTROL_FLOW);
}

/*
 * does the same as lower_highlevel() for all nodes on the const code irg
 */
void lower_const_code(void)
{
	walk_const_code(NULL, lower_irnode, NULL);
}

void lower_highlevel()
{
	foreach_irp_irg(i, irg) {
		lower_highlevel_graph(irg);
	}
	lower_const_code();
}
