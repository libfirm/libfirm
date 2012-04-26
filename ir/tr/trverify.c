/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @file    tr_inheritance.c
 * @brief   Check types and entities for correctness.
 * @date    29.1.2003
 * @author  Michael Beck, Goetz Lindenmaier
 */
#include "config.h"

#include "irgraph_t.h"
#include "irflag_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "error.h"
#include "tv.h"
#include "ircons.h"

static void report_error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "Verify warning: ");
	va_start(ap, fmt);
	ir_vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
}

static bool check_class_member(const ir_type *tp, const ir_entity *entity)
{
	bool fine = true;
	if (get_entity_n_overwrites(entity) > get_class_n_supertypes(tp)) {
		report_error("member %+F of %+F has too many overwrites", entity, tp);
		fine = false;
	}
	return fine;
}

static bool check_compound_type(const ir_type *tp)
{
	bool   fine     = true;
	bool   is_class = is_Class_type(tp);
	size_t n        = get_compound_n_members(tp);
	size_t i;

	for (i = 0; i < n; ++i) {
		ir_entity *member = get_compound_member(tp, i);
		ir_type   *owner;

		if (member == NULL) {
			report_error("%+F has a NULL member\n", tp);
			fine = false;
			continue;
		}
		owner = get_entity_owner(member);
		if (owner != tp) {
			report_error("member %+F of %+F has owner %+F\n", member, tp, owner);
			fine = false;
		}
		if (is_class) {
			fine &= check_class_member(tp, member);
		}
	}
	return fine;
}

static bool check_array_type(const ir_type *tp)
{
	bool   fine  = true;
	size_t n_dim = get_array_n_dimensions(tp);
	size_t i;

	for (i = 0; i < n_dim; ++i) {
		if (!has_array_lower_bound(tp, i) && !has_array_upper_bound(tp, i)) {
			report_error("missing array bound in %+F in dimension %zu", tp, i);
			fine = false;
		}
	}
	return fine;
}

static bool check_type_mode(const ir_type *tp)
{
	bool fine = true;
	if (get_type_mode(tp) == NULL) {
		report_error("type %+F has no mode", tp);
		fine = false;
	}
	return fine;
}

static bool check_primitive_type(const ir_type *tp)
{
	return check_type_mode(tp);
}

static bool check_pointer_type(const ir_type *tp)
{
	return check_type_mode(tp);
}

int check_type(const ir_type *tp)
{
	switch (get_type_tpop_code(tp)) {
	case tpo_union:
	case tpo_struct:
	case tpo_class:     return check_compound_type(tp);
	case tpo_array:     return check_array_type(tp);
	case tpo_primitive: return check_primitive_type(tp);
	case tpo_pointer:   return check_pointer_type(tp);
	case tpo_enumeration:
	case tpo_method:
	case tpo_uninitialized:
	case tpo_unknown:
	case tpo_none:
	case tpo_code:
		break;
	}
	return true;
}

static bool check_visited_flag(ir_graph *irg, ir_node *n)
{
	bool fine = true;
	if (get_irn_visited(n) > get_irg_visited(irg)) {
		report_error("visited flag of %+F is larger than that of corresponding irg %+F", n, irg);
		fine = false;
	}
	return fine;
}

typedef struct myenv {
	ir_graph *irg;
	bool      fine;
} myenv;

static void on_irg_storage(ir_node *n, void *data)
{
	myenv *env = (myenv*)data;

	/* We also test whether the setting of the visited flag is legal. */
	env->fine &= node_is_in_irgs_storage(env->irg, n);
	env->fine &= check_visited_flag(env->irg, n);
}

static bool constant_on_wrong_irg(ir_node *n)
{
	myenv env;

	env.fine = true;
	env.irg  = get_const_code_irg();

	irg_walk(n, on_irg_storage, NULL, (void *)&env);
	return env.fine;
}

static bool initializer_constant_on_wrong_irg(const ir_initializer_t *initializer)
{
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return 0;
	case IR_INITIALIZER_TARVAL:
		return 0;
	case IR_INITIALIZER_CONST:
		return constant_on_wrong_irg(get_initializer_const_value(initializer));
	case IR_INITIALIZER_COMPOUND: {
		bool   fine = true;
		size_t n    = get_initializer_compound_n_entries(initializer);
		size_t i;
		for (i = 0; i < n; ++i) {
			const ir_initializer_t *sub
				= get_initializer_compound_value(initializer, i);
			fine &= initializer_constant_on_wrong_irg(sub);
		}
		return fine;
	}
	}
	panic("invalid initializer in initializer_on_wrong_irg");
}

static bool constants_on_wrong_irg(const ir_entity *ent)
{
	if (ent->initializer != NULL) {
		return initializer_constant_on_wrong_irg(ent->initializer);
	} else if (entity_has_compound_ent_values(ent)) {
		bool   fine = true;
		size_t n    = get_compound_ent_n_values(ent);
		size_t i;
		for (i = 0; i < n; ++i) {
			fine &= constant_on_wrong_irg(get_compound_ent_value(ent, i));
		}
		return fine;
	}
	return true;
}

int check_entity(const ir_entity *ent)
{
	bool     fine = true;
	ir_type *tp   = get_entity_type(ent);

	fine &= constants_on_wrong_irg(ent);

	if (is_method_entity(ent)) {
		ir_graph *irg = get_entity_irg(ent);
		if (irg != NULL) {
			ir_entity *irg_entity = get_irg_entity(irg);
			if (irg_entity != ent) {
				report_error("entity(%+F)->irg->entity(%+F) relation invalid",
				             ent, irg_entity);
				fine = false;
			}
		}
		if (get_entity_peculiarity(ent) == peculiarity_existent) {
			ir_entity *impl = get_SymConst_entity(get_atomic_ent_value(ent));
			if (impl == NULL) {
				report_error("inherited method entity %+F must have constant pointing to existent entity.", ent);
				fine = false;
			}
		}
	}

	if (is_atomic_entity(ent) && ent->initializer != NULL) {
		ir_mode *mode = NULL;
		ir_initializer_t *initializer = ent->initializer;
		switch (initializer->kind) {
		case IR_INITIALIZER_CONST:
			mode = get_irn_mode(get_initializer_const_value(initializer));
			break;
		case IR_INITIALIZER_TARVAL:
			mode = get_tarval_mode(get_initializer_tarval_value(initializer));
			break;
		case IR_INITIALIZER_NULL:
		case IR_INITIALIZER_COMPOUND:
			break;
		}
		if (mode != NULL && mode != get_type_mode(tp)) {
			report_error("initializer of entity %+F has wrong mode.", ent);
			fine = false;
		}
	}
	return fine;
}

static void check_tore(type_or_ent tore, void *env)
{
	bool *fine = (bool*)env;

	if (is_type(tore.typ)) {
		*fine &= check_type(tore.typ);
	} else {
		assert(is_entity(tore.ent));
		*fine &= check_entity(tore.ent);
	}
}

int tr_verify(void)
{
	bool          fine = true;
	ir_type      *constructors;
	ir_type      *destructors;
	ir_type      *thread_locals;
	size_t        i, n;
	ir_segment_t  s;

	type_walk(check_tore, NULL, &fine);

	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		const ir_type *type = get_segment_type(s);
		size_t         e;
		for (e = 0; e < get_compound_n_members(type); ++e) {
			ir_entity *entity = get_compound_member(type, e);
			if (get_entity_ld_ident(entity) == NULL &&
				get_entity_visibility(entity) != ir_visibility_private) {
				report_error("public segment member %+F has no name",
				             entity);
				fine = false;
			}
		}
	}

	constructors = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
	for (i = 0, n = get_compound_n_members(constructors); i < n; ++i) {
		const ir_entity *entity = get_compound_member(constructors, i);
		if ((get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER) == 0) {
			report_error("entity %+F in constructors without LINKAGE_HIDDEN_USER",
			             entity);
			fine = false;
		}
		/* Mach-O doesn't like labels in this section */
		if (get_entity_ld_name(entity)[0] != '\0') {
			report_error("entity %+F in constructors must not have an ld_name",
			             entity);
			fine = false;
		}
	}
	destructors = get_segment_type(IR_SEGMENT_DESTRUCTORS);
	for (i = 0, n = get_compound_n_members(destructors); i < n; ++i) {
		const ir_entity *entity = get_compound_member(destructors, i);
		if ((get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER) == 0) {
			report_error("entity %+F in destructors without LINKAGE_HIDDEN_USER",
			             entity);
			fine = false;
		}
		/* Mach-O doesn't like labels in this section */
		if (get_entity_ld_name(entity)[0] != '\0') {
			report_error("entity %+F in destructors must not have an ld_name",
			             entity);
			fine = false;
		}
	}
	thread_locals = get_segment_type(IR_SEGMENT_THREAD_LOCAL);
	for (i = 0, n = get_compound_n_members(thread_locals); i < n; ++i) {
		const ir_entity *entity = get_compound_member(thread_locals, i);
		/* this is odd and should not be allowed I think */
		if (is_method_entity(entity)) {
			report_error("method %+F in thread local segment");
			fine = false;
		}
		if (get_entity_linkage(entity) & IR_LINKAGE_CONSTANT) {
			report_error("entity %+F in thread local segment is constant");
			fine = false;
		}
	}

	return fine;
}
