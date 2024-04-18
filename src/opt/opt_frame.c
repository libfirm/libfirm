/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Optimize the frame type.
 * @date    15.03.2006
 * @author  Michael Beck
 * @brief
 *   Optimize the frame type by removing unused type members.
 */
#include "iredges_t.h"
#include "irgraph_t.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "type_t.h"

/*
 * Optimize the frame type of an irg by removing
 * never touched entities.
 */
void opt_frame_irg(ir_graph *irg)
{
	ir_type *frame_tp = get_irg_frame_type(irg);
	size_t   n        = get_compound_n_members(frame_tp);
	if (n <= 0)
		return;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* clear all entity links */
	for (size_t i = n; i-- > 0;) {
		ir_entity *entity = get_compound_member(frame_tp, i);
		set_entity_link(entity, NULL);
	}

	/* mark all used entities */
	ir_node *frame = get_irg_frame(irg);
	foreach_irn_out_r(frame, o, sel) {
		if (is_Member(sel)) {
			ir_entity *entity = get_Member_entity(sel);
			/* only entities on the frame */
			if (get_entity_owner(entity) == frame_tp)
				set_entity_link(entity, entity);
		}
	}

	/* link unused ones */
	ir_entity *list = NULL;
	for (size_t i = n; i-- > 0;) {
		ir_entity *entity = get_compound_member(frame_tp, i);
		if (get_entity_link(entity) == NULL) {
			set_entity_link(entity, list);
			list = entity;
		}
	}

	if (list != NULL) {
		/* delete list members */
		for (ir_entity *entity = list, *next; entity != NULL; entity = next) {
			next = (ir_entity*)get_entity_link(entity);
			free_entity(entity);
		}
		/* we changed the frame type, its layout should be redefined */
		set_type_state(frame_tp, layout_undefined);
	}
	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* we changed the type, this affects none of the currently known graph
	 * properties, but I don't use ALL because I don't know if someone adds
	 * type-based properties at some point */
	confirm_irg_properties(irg,
		IR_GRAPH_PROPERTIES_CONTROL_FLOW
		| IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE
		| IR_GRAPH_PROPERTY_MANY_RETURNS);
}
