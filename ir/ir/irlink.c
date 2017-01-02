/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

/**
 * @brief   Provide means to link (i.e. merge) entities and types
 *          from multiple IR files.
 * @author  Mark Weinreuter, Manuel Mohr
 */

#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>

#include "entity_t.h"
#include "firm_types.h"
#include "irflag.h"
#include "irlink_t.h"
#include "irmode.h"
#include "irprog.h"
#include "util.h"

static ir_visibility get_min_visibility(ir_visibility v1, ir_visibility v2)
{
	if (v1 == v2)
		return v1;

	// if there are different visibilities, choose the non external visibility
	if (v1 == ir_visibility_external) {
		return v2;
	} else if (v2 == ir_visibility_external) {
		return v1;
	}

	panic("Incompatible visibilities: %+F vs. %+F\n", v1, v2);
}

/**
 * If the function type differs from the already known function type
 * figure out which is the correct type. This method assumes that implicit
 * declared functions have zero parameters. Two function types with different
 * non zero parameter count are regarded as invalid.
 */
static ir_type *handle_implicit_declared_functions(ir_type *old_type, ir_type *new_type)
{
	size_t new_n  = get_method_n_params(new_type);
	size_t old_n  = get_method_n_params(old_type);
	size_t new_rn = get_method_n_ress(new_type);
	size_t old_rn = get_method_n_ress(old_type);

	//TODO: what about variadic method types? Can they cause trouble
	bool new_variadic = is_method_variadic(new_type);
	bool old_variadic = is_method_variadic(old_type);

	// Since there is no information if a method type is from a definition,
	// a delceration or an impplicit declaration we try to infer this
	// information from the amout of parameters and return types
	// an implicit declaration has no parameters and an integer return value
	// Compare each parameter type is also not an option since ir_type s
	// are not merge and cannot be easily compared
	bool new_maybe_implicit = new_n == 0 && new_rn == 1 && get_type_mode(get_method_res_type(new_type, 0)) == get_modeIs();
	bool old_maybe_implicit = old_n == 0 && old_rn == 1 && get_type_mode(get_method_res_type(old_type, 0)) == get_modeIs();

	// Implicit method type check
	if (new_rn != old_rn || new_n != old_n || new_variadic != old_variadic) {
		// Different amount of parameters or return values.
		// This is either an implicit declared function or an error
		// If it is an implicit declaration we go with the other type

		if (old_maybe_implicit) {
			return new_type;
		} else if (new_maybe_implicit) {
			return old_type;
		}

		// TODO: since there could be a declaration X foo() and X foo(p1, p2, p3)
		// TODO: we trust the frontend to do its job
		if (new_n > old_n)
			return new_type;

		return old_type;
	}

	// TODO: figure out if one if them is the definition
	return old_type;
}

ir_entity *ir_link_entity(ident *name, ir_type *type, ir_entity_kind kind,
                          ir_type *owner, ir_linkage linkage,
                          ir_volatility volatility, ir_visibility visibility)
{
	ir_entity *entity = ir_get_global(name);

	// TODO: more comparison checks needed?
	if (entity == NULL || entity->kind != kind || entity->owner != owner)
		return NULL;

	visibility  = get_min_visibility(get_entity_visibility(entity), visibility);
	volatility  = MAX(get_entity_volatility(entity), volatility); // is_volatile > not_volatile
	linkage    |= get_entity_linkage(entity); // Linkage values accumulate

	if (kind == IR_ENTITY_METHOD) {
		ir_type *tp = get_entity_type(entity);
		tp = handle_implicit_declared_functions(tp, type);
		set_entity_type(entity, tp);
	}

	set_entity_linkage(entity, linkage);
	set_entity_volatility(entity, volatility);
	set_entity_visibility(entity, visibility);

	return entity;
}

void ir_adjust_visibility(ir_entity *entity)
{
	ir_visibility  visibility            = get_entity_visibility(entity);
	const char    *name                  = get_id_str(get_entity_ld_name(entity));
	const bool     can_adjust_visibility = get_opt_closed_world();
	const bool     is_externally_visible = visibility != ir_visibility_private && visibility != ir_visibility_local;

	if (can_adjust_visibility && is_externally_visible
	    && entity_has_definition(entity)
	    && name != NULL && !streq(name, "main")) {

		set_entity_visibility(entity, ir_visibility_local);
		remove_entity_linkage(entity, IR_LINKAGE_MERGE);
	}
}
