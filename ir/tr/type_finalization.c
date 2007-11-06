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
 * @file    type_finalization.c
 * @brief   Calculate finalization of classes and entities by
 *          inspecting the class hierarchy.
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "typerep.h"
#include "irprog_t.h"
#include "irflag_t.h"
#include "entity_t.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static void do_finalization(type_or_ent *tore, void *env) {
	ir_type *glob_tp = env;

	if (is_type(tore)) {
		ir_type *cls = (ir_type *)tore;

		if (! is_Class_type(cls) || cls == glob_tp)
			return;

		if (is_class_final(cls))
			return;
		if (get_class_n_subtypes(cls) == 0) {
			/* Note that we set the final property even for the
			   frame/tls types this way. Should not made any problems. */
			set_class_final(cls, 1);
			DB((dbg, LEVEL_1, " made final Class %s\n",
				get_type_name(cls)));
		}
	} else {
		ir_entity *ent = (ir_entity *)tore;
		ir_type *owner;

		if (is_entity_final(ent))
			return;

		owner = get_entity_owner(ent);
		/* beware of array entities */
		if (! is_Class_type(owner) || owner == glob_tp)
			return;

		if (is_class_final(owner)) {
			assert(get_entity_n_overwrittenby(ent) == 0);
			set_entity_final(ent, 1);
			DB((dbg, LEVEL_1, " made final %s::%s\n",
				get_type_name(owner), get_entity_name(ent)));
		} else if (get_entity_n_overwrittenby(ent) == 0) {
			set_entity_final(ent, 1);
			DB((dbg, LEVEL_1, " made final %s::%s\n",
				get_type_name(owner), get_entity_name(ent)));
		}
	}
}  /* do_finalization */

/**
 * If we have the closed world assumption, we can calculate the
 * finalization of classes and entities by inspecting the class hierarchy.
 * After this is done, all classes and entities that are not overridden
 * anymore have the final property set.
 */
void types_calc_finalization(void) {
 	if (! get_opt_closed_world())
 		return;

	FIRM_DBG_REGISTER(dbg, "firm.tr.finalization");
//	firm_dbg_set_mask(dbg, SET_LEVEL_1);

	/* types must be visited before it's entities */
	type_walk(do_finalization, NULL, get_glob_type());
}
