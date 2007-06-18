/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Change the calling conventions of "local" methods
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "iroptimize.h"
#include "debug.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irmemory.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Copy the calling conventions from the entities to the call type.
 */
static void update_calls(ir_node *call, void *env) {
	(void) env;
	if (is_Call(call)) {
		ir_node *ptr = get_Call_ptr(call);

		if (is_SymConst(ptr)) {
			ir_entity *ent = get_SymConst_entity(ptr);
			ir_type *mtp = get_entity_type(ent);
			ir_type *ctp = get_Call_type(call);
			unsigned cc  = get_method_calling_convention(mtp);

			if (cc != get_method_calling_convention(ctp)) {
				set_method_calling_convention(ctp, cc);
				DB((dbg, LEVEL_1, "changed calling convention of calll to %+F\n", ent));
			}
		}
	}
}

/**
 * Change the calling conventions for all local methods.
 *
 * @param cc  new calling convention
 */
void opt_change_calling_conventions(unsigned cc) {
	int i;
	int changed = 0;

	FIRM_DBG_REGISTER(dbg, "firm.opt.cc");

	assure_irp_globals_address_taken_computed();

	/* first step: change the calling conventions of the local non-escaped entities */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph               *irg = get_irp_irg(i);
		ir_entity              *ent = get_irg_entity(irg);
		ir_address_taken_state state = get_entity_address_taken(ent);

		if (get_entity_visibility(ent) == visibility_local &&
		    state == ir_address_not_taken) {
			ir_type *mtp = get_entity_type(ent);

			unsigned new_cc, old_cc = get_method_calling_convention(mtp) & ~cc_bits;

			new_cc = old_cc | (cc & cc_bits);

			if (new_cc != old_cc) {
				set_method_calling_convention(mtp, new_cc);
				changed = 1;
				DB((dbg, LEVEL_1, "changed calling convention of %+F\n", ent));
			}
		}
	}

	if (changed)
		all_irg_walk(NULL, update_calls, NULL);
}
