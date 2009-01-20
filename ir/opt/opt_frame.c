/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Optimize the frame type.
 * @date    15.03.2006
 * @author  Michael Beck
 * @version $Id$
 * @summary
 *   Optimize the frame type by removing unused type members.
 */
#include "config.h"

#include "iroptimize.h"
#include "irgraph_t.h"
#include "type_t.h"
#include "irouts.h"
#include "iredges.h"

/*
 * Optimize the frame type of an irg by removing
 * never touched entities.
 */
void opt_frame_irg(ir_graph *irg) {
	ir_type   *frame_tp = get_irg_frame_type(irg);
	ir_entity *ent, *list;
	ir_node   *frame, *sel;
	int       i, n = get_class_n_members(frame_tp);

	if (n <= 0)
		return;

	irp_reserve_resources(irp, IR_RESOURCE_ENTITY_LINK);

	/* clear all entity links */
	for (i = n - 1; i >= 0; --i) {
		ent = get_class_member(frame_tp, i);
		set_entity_link(ent, NULL);
	}

	/* look for uses */
	frame = get_irg_frame(irg);

	if (edges_activated(irg)) { /* use inplace edges */
		const ir_edge_t *edge;

		/* mark all used entities */
		foreach_out_edge(frame, edge) {
			sel = get_edge_src_irn(edge);
			if (is_Sel(sel)) {
				ent = get_Sel_entity(sel);
				set_entity_link(ent, ent);
			}
		}
	} else {
		/* use traditionally out edges */
		assure_irg_outs(irg);

		/* mark all used entities */
		for (i = get_irn_n_outs(frame) - 1; i >= 0; --i) {
			sel = get_irn_out(frame, i);
			if (is_Sel(sel)) {
				ent = get_Sel_entity(sel);
				/* only entities on the frame */
				if (get_entity_owner(ent) == frame_tp)
					set_entity_link(ent, ent);
			}
		}
	}

	/* link unused ones */
	list = NULL;
	for (i = n - 1; i >= 0; --i) {
		ent = get_class_member(frame_tp, i);
		/* beware of inner functions: those are NOT unused */
		if (get_entity_link(ent) == NULL && !is_method_entity(ent)) {
			set_entity_link(ent, list);
			list = ent;
		}
	}

	if (list != NULL) {
		/* delete list members */
		for (ent = list; ent; ent = list) {
			list = get_entity_link(ent);
			remove_class_member(frame_tp, ent);
		}
		/* we changed the frame type, it's layout should be redefined */
		set_type_state(frame_tp, layout_undefined);
	}
	irp_free_resources(irp, IR_RESOURCE_ENTITY_LINK);
}
