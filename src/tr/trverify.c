/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Check types and entities for correctness.
 * @date    29.1.2003
 * @author  Michael Beck, Goetz Lindenmaier
 */
#include "ircons.h"
#include "irflag_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "tv.h"

static void report_error(const char *fmt, ...)
{
	fprintf(stderr, "Verify warning: ");
	va_list ap;
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
	bool fine     = true;
	bool is_class = is_Class_type(tp);
	for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity *member = get_compound_member(tp, i);
		if (member == NULL) {
			report_error("%+F has a NULL member\n", tp);
			fine = false;
			continue;
		}
		ir_type *owner = get_entity_owner(member);
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
	switch (get_type_opcode(tp)) {
	case tpo_union:
	case tpo_struct:
	case tpo_class:
	case tpo_segment:   return check_compound_type(tp);
	case tpo_primitive: return check_primitive_type(tp);
	case tpo_pointer:   return check_pointer_type(tp);
	case tpo_array:
	case tpo_method:
	case tpo_uninitialized:
	case tpo_unknown:
	case tpo_code:
		return true;
	}
	report_error("Invalid type opcode");
	return false;
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

static bool constant_on_correct_irg(ir_node *n)
{
	myenv env;
	env.fine = true;
	env.irg  = get_const_code_irg();

	irg_walk(n, on_irg_storage, NULL, (void *)&env);
	return env.fine;
}

static bool check_initializer(const ir_initializer_t *initializer,
                              const ir_type *type,
                              const ir_entity *context)
{
	bool fine = true;
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return fine;
	case IR_INITIALIZER_TARVAL: {
		ir_tarval *tv = get_initializer_tarval_value(initializer);
		if (get_type_mode(type) != get_tarval_mode(tv)) {
			report_error("tarval initializer for entity %+F has wrong mode: %+F vs %+F", context, get_type_mode(type), get_tarval_mode(tv));
			fine = false;
		}
		return fine;
	}
	case IR_INITIALIZER_CONST: {
		ir_node *value = get_initializer_const_value(initializer);
		if (get_type_mode(type) != get_irn_mode(value)) {
			report_error("const initializer for entity %+F has wrong mode: %+F vs %+F", context, get_type_mode(type), get_irn_mode(value));
			fine = false;
		}
		if (!constant_on_correct_irg(value)) {
			report_error("initializer const value %+F for entity %+F not on const-code irg", value, context);
			fine = false;
		}
		return fine;
	}
	case IR_INITIALIZER_COMPOUND: {
		size_t n_entries = get_initializer_compound_n_entries(initializer);
		if (is_Array_type(type)) {
			ir_type *element_type = get_array_element_type(type);
			/* TODO: check array bounds? */
			for (size_t i = 0; i < n_entries; ++i) {
				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);
				check_initializer(sub_initializer, element_type, context);
			}
		} else if (is_compound_type(type)) {
			size_t n_members = get_compound_n_members(type);
			if (n_entries > n_members) {
				report_error("too many values in compound initializer of %+F",
				             context);
				fine = false;
			}
			for (size_t i = 0; i < n_entries; ++i) {
				if (i >= n_members)
					break;
				ir_entity *member      = get_compound_member(type, i);
				ir_type   *member_type = get_entity_type(member);
				const ir_initializer_t *sub_initializer
					= get_initializer_compound_value(initializer, i);
				check_initializer(sub_initializer, member_type, context);
			}
		} else {
			report_error("compound initiailizer for non-array/compound type in entity %+F",
			             context);
			fine = false;
		}
		return fine;
	}
	}
	report_error("invalid initializer for entity %+F", context);
	return false;
}

static bool is_externally_visible(const ir_entity *entity)
{
	switch (get_entity_visibility(entity)) {
	case ir_visibility_external:
	case ir_visibility_external_private:
	case ir_visibility_external_protected:
		return true;
	case ir_visibility_private:
	case ir_visibility_local:
		return false;
	}
	/* Return true here, the verification will fail elsewhere for this */
	return true;
}

static bool check_external_linkage(const ir_entity *entity, ir_linkage linkage,
                                   const char *linkage_name)
{
	bool fine = true;
	if ((get_entity_linkage(entity) & linkage) == 0)
		return true;
	if (!is_externally_visible(entity)) {
		report_error("entity %+F has IR_LINKAGE_%s but is not externally visible", entity, linkage_name);
		fine = false;
	}
	if (!entity_has_definition(entity)) {
		report_error("entity %+F has IR_LINKAGE_%s but is just a declaration", entity, linkage_name);
		fine = false;
	}
	return fine;
}

static bool is_data_type(const ir_type *type)
{
	return type != get_code_type() && !is_Method_type(type);
}

int check_entity(const ir_entity *entity)
{
	bool fine = true;

	ir_linkage linkage = get_entity_linkage(entity);
	if (linkage & IR_LINKAGE_NO_CODEGEN) {
		if (!is_method_entity(entity)) {
			report_error("entity %+F has IR_LINKAGE_NO_CODEGEN but is not a function", entity);
			fine = false;
		} else if (get_entity_irg(entity) == NULL) {
			report_error("entity %+F has IR_LINKAGE_NO_CODEGEN but has no ir-graph anyway", entity);
			fine = false;
		}
		if (!is_externally_visible(entity)) {
			report_error("entity %+F has IR_LINKAGE_NO_CODEGEN but is not externally visible", entity);
			fine = false;
		}
	}
	check_external_linkage(entity, IR_LINKAGE_WEAK, "WEAK");
	check_external_linkage(entity, IR_LINKAGE_GARBAGE_COLLECT,
	                       "GARBAGE_COLLECT");
	check_external_linkage(entity, IR_LINKAGE_MERGE, "MERGE");

	ir_type const *const type  = get_entity_type(entity);
	ir_type const *const owner = get_entity_owner(entity);
	switch (get_entity_kind(entity)) {
	case IR_ENTITY_ALIAS:
		if (!is_segment_type(owner)) {
			report_error("alias entity %+F has non-segment owner %+F", entity,
			             owner);
			fine = false;
		}
		break;

	case IR_ENTITY_NORMAL: {
		ir_initializer_t const *const init = get_entity_initializer(entity);
		if (init)
			fine &= check_initializer(init, type, entity);

		if (!is_data_type(type)) {
			report_error("normal entity %+F has non-data type %+F", entity,
			             type);
			fine = false;
		}
		break;
	}

	case IR_ENTITY_COMPOUND_MEMBER:
		if (!is_compound_type(owner)) {
			report_error("compound member entity %+F has non-compound owner %+F",
			             entity, owner);
			fine = false;
		}
		break;

	case IR_ENTITY_LABEL:
		if (type != get_code_type()) {
			report_error("label entity %+F has non-code type %+F", entity,
			             type);
			fine = false;
		}
		break;

	case IR_ENTITY_METHOD:
		if (!is_Method_type(type)) {
			report_error("method entity %+F has non-method type %+F", entity,
			             type);
			fine = false;
		}
		ir_graph *irg = get_entity_irg(entity);
		if (irg != NULL) {
			ir_entity *irg_entity = get_irg_entity(irg);
			if (irg_entity != entity) {
				report_error("entity(%+F)->irg->entity(%+F) relation invalid",
				             entity, irg_entity);
				fine = false;
			}
		}
		break;
	case IR_ENTITY_PARAMETER:
		if (!is_frame_type(owner)) {
			report_error("parameter entity %+F has non-frame owner %+F",
			             entity, owner);
			fine = false;
		}
		if (!is_data_type(type)) {
			report_error("parameter entity %+F has non-data type %+F", entity,
			             type);
			fine = false;
		}
		break;

	case IR_ENTITY_UNKNOWN:
		break;

	case IR_ENTITY_SPILLSLOT:
		if (is_frame_type(owner)) {
			report_error("spillslot %+F must be on frame type", entity);
			fine = false;
		}
		break;
	}
	if (is_frame_type(owner) && entity_has_definition(entity)) {
		report_error("entity %+F on frame %+F has initialized", entity, owner);
		fine = false;
	}

	return fine;
}

static void check_tore(ir_type *const type, ir_entity *const entity, void *const env)
{
	bool *fine = (bool*)env;

	if (type) {
		*fine &= check_type(type);
	} else {
		*fine &= check_entity(entity);
	}
}

static bool verify_info_member(ir_type const *const segment,
                               ir_entity const *const entity)
{
	bool fine = true;
	/* Mach-O does not allow labels in segments like constructors,
	 * destructors, ... */
	ident *const ld_ident = get_entity_ld_ident(entity);
	if (ld_ident[0] != '\0') {
		report_error("entity %+F in %s segment must not have an ld_name",
		             entity, get_compound_name(segment));
		fine = false;
	}
	if ((get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER) == 0) {
		report_error("entity %+F in segment %s without LINKAGE_HIDDEN_USER",
		             entity, get_compound_name(segment));
		fine = false;
	}
	return fine;
}

int tr_verify(void)
{
	bool     fine = true;

	type_walk(check_tore, NULL, &fine);

	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type const *const segment = get_segment_type(s);
		for (size_t e = 0; e < get_compound_n_members(segment); ++e) {
			ir_entity const *const entity = get_compound_member(segment, e);
			ident           *const ld_ident = get_entity_ld_ident(entity);
			if (ld_ident == NULL &&
				get_entity_visibility(entity) != ir_visibility_private) {
				report_error("public segment member %+F has no name",
				             entity);
				fine = false;
				continue;
			}
			if (segment->flags & tf_info)
				fine &= verify_info_member(segment, entity);
		}
	}

	ir_type const *const thread_locals
		= get_segment_type(IR_SEGMENT_THREAD_LOCAL);
	for (size_t i = 0, n = get_compound_n_members(thread_locals); i < n; ++i) {
		const ir_entity *entity = get_compound_member(thread_locals, i);
		/* this is odd and should not be allowed I think */
		if (is_method_entity(entity)) {
			report_error("method %+F in thread local segment", entity);
			fine = false;
		}
		if (get_entity_linkage(entity) & IR_LINKAGE_CONSTANT) {
			report_error("entity %+F in thread local segment is constant",
			             entity);
			fine = false;
		}
	}

	return fine;
}
