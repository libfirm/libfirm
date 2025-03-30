/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Removal of unreachable methods.
 * @author   Hubert Schmid
 * @date     09.06.2002
 */

/*
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Abschätzung der
 * Aufrufrelation bestimmt.
 */
#include "ircgopt.h"

#include "array.h"
#include "cgana.h"
#include "debug.h"
#include "ircons.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irprog_t.h"
#include "irtools.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Walker: adds Call operations to a head's link list.
 */
static void collect_call(ir_node *node, void *env)
{
	ir_node *head = (ir_node*)env;
	if (is_Call(node)) {
		set_irn_link(node, get_irn_link(head));
		set_irn_link(head, node);
	}
}

/* garbage collect methods: mark and remove */
void gc_irgs(size_t n_keep, ir_entity ** keep_arr)
{
	void *MARK = &MARK; /* @@@ gefaehrlich!!! Aber wir markieren hoechstens zu viele ... */

	FIRM_DBG_REGISTER(dbg, "firm.opt.cgopt");

	if (n_keep >= get_irp_n_irgs()) {
		/* Shortcut. Obviously we have to keep all methods. */
		return;
	}

	DB((dbg, LEVEL_1, "dead method elimination\n"));

	/* Mark entities that are alive.  */
	if (n_keep > 0) {
		ir_entity **marked = NEW_ARR_F(ir_entity *, n_keep);
		for (size_t idx = 0; idx < n_keep; ++idx) {
			marked[idx] = keep_arr[idx];
			set_entity_link(marked[idx], MARK);
			DB((dbg, LEVEL_1, "  method %+F kept alive.\n", marked[idx]));
		}

		for (size_t idx = 0; idx < ARR_LEN(marked); ++idx) {
			ir_graph *irg = get_entity_irg(marked[idx]);
			if (irg == NULL)
				continue;

			/* collect calls */
			ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
			ir_node *end = get_irg_end(irg);
			irg_walk_graph(irg, firm_clear_link, collect_call, end);

			/* iterate calls */
			for (ir_node *node = (ir_node*)get_irn_link(end); node != NULL;
			     node = (ir_node*)get_irn_link(node)) {
				for (size_t i = cg_get_call_n_callees(node); i-- > 0;) {
					ir_entity *ent = cg_get_call_callee(node, i);

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
		DEL_ARR_F(marked);
	}

	/* clean */
	foreach_irp_irg_r(i, irg) {
		ir_entity *ent = get_irg_entity(irg);
		if (get_entity_link(ent) == MARK)
			continue;

		DB((dbg, LEVEL_1, "  freeing method %+F\n", ent));
		free_ir_graph(irg);
	}
}
