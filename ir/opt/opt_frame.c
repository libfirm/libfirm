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
#include "iroptimize.h"
#include "irgraph_t.h"
#include "type_t.h"
#include "irouts.h"
#include "iredges.h"
#include "irpass.h"

/*
 * Optimize the frame type of an irg by removing
 * never touched entities.
 */
void opt_frame_irg(ir_graph *irg)
{
	ir_type   *frame_tp = get_irg_frame_type(irg);
	ir_entity *ent, *list;
	ir_node   *frame, *sel;
	size_t    i, n = get_class_n_members(frame_tp);
	int       o;

	if (n <= 0)
		return;

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* clear all entity links */
	for (i = n; i > 0;) {
		ent = get_class_member(frame_tp, --i);
		set_entity_link(ent, NULL);
	}

	/* look for uses */
	frame = get_irg_frame(irg);

	/* mark all used entities */
	for (o = get_irn_n_outs(frame) - 1; o >= 0; --o) {
		sel = get_irn_out(frame, o);
		if (is_Sel(sel)) {
			ent = get_Sel_entity(sel);
			/* only entities on the frame */
			if (get_entity_owner(ent) == frame_tp)
				set_entity_link(ent, ent);
		}
	}

	/* link unused ones */
	list = NULL;
	for (i = n; i > 0;) {
		ent = get_class_member(frame_tp, --i);
		/* beware of inner functions: those are NOT unused */
		if (get_entity_link(ent) == NULL && !is_method_entity(ent)) {
			set_entity_link(ent, list);
			list = ent;
		}
	}

	if (list != NULL) {
		/* delete list members */
		for (ent = list; ent; ent = list) {
			list = (ir_entity*)get_entity_link(ent);
			free_entity(ent);
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

ir_graph_pass_t *opt_frame_irg_pass(const char *name)
{
	return def_graph_pass(name ? name : "opt_frame_irg", opt_frame_irg);
}
