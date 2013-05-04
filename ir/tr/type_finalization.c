/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Calculate finalization of classes and entities by
 *          inspecting the class hierarchy.
 * @author  Michael Beck
 */
#include "typerep.h"
#include "irprog_t.h"
#include "irflag_t.h"
#include "entity_t.h"
#include "debug.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static void do_finalization(type_or_ent tore, void *env)
{
	ir_type *glob_tp = (ir_type*)env;

	if (is_type(tore.typ)) {
		ir_type *cls = tore.typ;
		if (!is_Class_type(cls))
			return;

		if (is_class_final(cls))
			return;
		if (get_class_n_subtypes(cls) == 0) {
			/* Note that we set the final property even for the
			   frame/global types this way. Should not made any problems. */
			set_class_final(cls, 1);
			DB((dbg, LEVEL_1, " made final Class %s\n",
				get_class_name(cls)));
		}
	} else {
		ir_entity *ent = tore.ent;
		if (is_entity_final(ent))
			return;

		ir_type *owner = get_entity_owner(ent);
		/* beware of array entities */
		if (! is_Class_type(owner) || owner == glob_tp)
			return;

		if (is_class_final(owner)) {
			assert(get_entity_n_overwrittenby(ent) == 0);
			set_entity_final(ent, 1);
			DB((dbg, LEVEL_1, " made final %s::%s\n",
				get_compound_name(owner), get_entity_name(ent)));
		} else if (get_entity_n_overwrittenby(ent) == 0) {
			set_entity_final(ent, 1);
			DB((dbg, LEVEL_1, " made final %s::%s\n",
				get_compound_name(owner), get_entity_name(ent)));
		}
	}
}

void types_calc_finalization(void)
{
	if (! get_opt_closed_world())
		return;

	FIRM_DBG_REGISTER(dbg, "firm.tr.finalization");

	/* types must be visited before their entities */
	type_walk(do_finalization, NULL, get_glob_type());
}
