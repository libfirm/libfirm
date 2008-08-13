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
 * @brief    Removal of unreachable methods.
 * @author   Hubert Schmid
 * @date     09.06.2002
 * @version  $Id$
 */

/*
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Abschätzung der
 * Aufrufrelation bestimmt.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "ircgopt.h"

#include "debug.h"
#include "array.h"
#include "irprog.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irflag_t.h"
#include "ircons.h"
#include "irtools.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg);

/**
 * Walker: adds Call operations to a head's link list.
 */
static void collect_call(ir_node *node, void *env) {
	ir_node *head = env;

	if (is_Call(node)) {
		set_irn_link(node, get_irn_link(head));
		set_irn_link(head, node);
	}
}

/**
 * Type walker, set the peculiarity of entities which graphs
 * gets removed to peculiarity_description.
 */
static void make_entity_to_description(type_or_ent tore, void *env) {
	if (is_entity(tore.ent)) {
		ir_entity *ent = tore.ent;

		if ((is_Method_type(get_entity_type(ent)))                        &&
			(get_entity_peculiarity(ent) != peculiarity_description)      &&
			(get_entity_visibility(ent)  != visibility_external_allocated)   ) {
				ir_entity *impl = get_SymConst_entity(get_atomic_ent_value(ent));
				if (get_entity_link(impl) != env) {
					set_entity_peculiarity(ent, peculiarity_description);
				}
		}
	}
}

/* garbage collect methods: mark and remove */
void gc_irgs(int n_keep, ir_entity ** keep_arr) {
	void * MARK = &MARK; /* @@@ gefaehrlich!!! Aber wir markieren hoechstens zu viele ... */
	int i;

	FIRM_DBG_REGISTER(dbg, "firm.opt.cgopt");

	if (n_keep >= get_irp_n_irgs()) {
		/* Shortcut. Obviously we have to keep all methods. */
		return;
	}

	DB((dbg, LEVEL_1, "dead method elimination\n"));

	/* Mark entities that are alive.  */
	if (n_keep > 0) {
		ir_entity **marked = NEW_ARR_F(ir_entity *, n_keep);
		for (i = 0; i < n_keep; ++i) {
			marked[i] = keep_arr[i];
			set_entity_link(marked[i], MARK);
			DB((dbg, LEVEL_1, "  method %+F kept alive.\n",	marked[i]));
		}

		for (i = 0; i < ARR_LEN(marked); ++i) {
			/* check for extern methods, these don't have an IRG */
			if (get_entity_visibility(marked[i]) != visibility_external_allocated) {
				ir_graph *irg = get_entity_irg(marked[i]);
				ir_node *node = get_irg_end(irg);

				/* collect calls */
				ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
				irg_walk_graph(irg, firm_clear_link, collect_call, node);

				/* iterate calls */
				for (node = get_irn_link(node); node; node = get_irn_link(node)) {
					int i;
					assert(is_Call(node));

					for (i = get_Call_n_callees(node) - 1; i >= 0; --i) {
						ir_entity *ent = get_Call_callee(node, i);

						if (get_entity_irg(ent) && get_entity_link(ent) != MARK) {
							set_entity_link(ent, MARK);
							ARR_APP1(ir_entity *, marked, ent);

							DB((dbg, LEVEL_1, "  method %+F can be called from Call %+F: kept alive.\n",
							    ent, node));
						}
					}
				}
				ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
			}
		}
		DEL_ARR_F(marked);
	}

	/* clean */
	type_walk(make_entity_to_description, NULL, MARK);
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph  *irg = get_irp_irg(i);
		ir_entity *ent = get_irg_entity(irg);
		/* Removing any graph invalidates all interprocedural loop trees. */
		if (get_irg_loopinfo_state(irg) == loopinfo_ip_consistent ||
		    get_irg_loopinfo_state(irg) == loopinfo_ip_inconsistent) {
			free_loop_information(irg);
		}
		if ((get_entity_visibility(ent) == visibility_local) && (get_entity_link(ent) != MARK)) {
			remove_irp_irg(irg);
			set_entity_peculiarity(ent, peculiarity_description);

			DB((dbg, LEVEL_1, "  freeing method %+F\n",	ent));
		}
		set_entity_link(ent, NULL);
	}
}
