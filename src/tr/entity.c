/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of all program known entities.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include "entity_t.h"

#include "array.h"
#include "callgraph.h"
#include "irdump.h"
#include "irgraph_t.h"
#include "irhooks.h"
#include "irprog_t.h"
#include "panic.h"
#include "util.h"
#include "xmalloc.h"
#include <stdlib.h>

/** The name of the unknown entity. */
#define UNKNOWN_ENTITY_NAME "unknown_entity"

ir_entity *get_unknown_entity(void)
{
	return irp->unknown_entity;
}

static ir_entity *intern_new_entity(ir_type *owner, ir_entity_kind kind,
                                    ident *name, ir_type *type,
                                    ir_visibility visibility)
{
	assert(owner != NULL);

	ir_entity *res = XMALLOCZ(ir_entity);
	res->firm_tag    = k_entity;
	res->name        = name;
	res->ld_name     = name;
	res->type        = type;
	res->owner       = owner;
	res->kind        = kind;
	res->volatility  = volatility_non_volatile;
	res->aligned     = align_is_aligned;
	res->usage       = ir_usage_unknown;
	res->visibility  = visibility;
	res->nr          = get_irp_new_node_nr();

	/* Remember entity in its owner. */
	add_compound_member(owner, res);

	res->visit = 0;
	return res;
}

static ir_entity *new_entity_vis(ir_type *owner, ident *name, ir_type *type,
                                 ir_visibility vis)
{
	ir_entity *res;
	if (is_Method_type(type)) {
		res = intern_new_entity(owner, IR_ENTITY_METHOD, name, type, vis);
		res->linkage                     = IR_LINKAGE_CONSTANT;
		res->attr.global.jit_addr        = (void*)-1;
		res->attr.global.properties      = get_method_additional_properties(type);
		res->attr.mtd_attr.vtable_number = IR_VTABLE_NUM_NOT_SET;
		res->attr.mtd_attr.param_access  = NULL;
		res->attr.mtd_attr.param_weight  = NULL;
		res->attr.mtd_attr.irg           = NULL;
	} else if (is_compound_type(owner) && !is_segment_type(owner)) {
		res = intern_new_entity(owner, IR_ENTITY_COMPOUND_MEMBER, name, type,
		                        vis);
		res->attr.compound_member.offset = INVALID_OFFSET;
	} else {
		res = intern_new_entity(owner, IR_ENTITY_NORMAL, name, type, vis);
		res->attr.global.jit_addr = (void*)-1;
	}

	hook_new_entity(res);
	return res;
}

ir_entity *new_entity(ir_type *owner, ident *name, ir_type *type)
{
	return new_entity_vis(owner, name, type, ir_visibility_external);
}

ir_entity *new_global_entity(ir_type *segment, ident *ld_name, ir_type *type,
                             ir_visibility visibility, ir_linkage linkage)
{
	assert(is_segment_type(segment) || segment == irp->dummy_owner);
	ir_entity *res = new_entity_vis(segment, ld_name, type, visibility);
	add_entity_linkage(res, linkage);
	hook_new_entity(res);
	return res;
}

ir_entity *new_parameter_entity(ir_type *owner, size_t pos, ir_type *type)
{
	ir_entity *res  = intern_new_entity(owner, IR_ENTITY_PARAMETER, NULL, type,
	                                    ir_visibility_private);
	res->attr.compound_member.offset = INVALID_OFFSET;
	res->attr.parameter.number = pos;
	hook_new_entity(res);
	return res;
}

ir_entity *new_label_entity(ir_label_t label)
{
	ir_type *global_type = get_glob_type();
	ir_entity *res = intern_new_entity(global_type, IR_ENTITY_LABEL, NULL,
	                                   get_code_type(), ir_visibility_private);
	res->attr.code_attr.label = label;
	hook_new_entity(res);
	return res;
}

ir_entity *new_spillslot(ir_type *const frame, unsigned const size,
                         unsigned const po2align)
{
	assert(is_frame_type(frame));
	ir_type   *const type = get_unknown_type();
	ir_entity *const res  = intern_new_entity(frame, IR_ENTITY_SPILLSLOT, NULL,
	                                          type, ir_visibility_private);
	set_entity_alignment(res, 1u << po2align);
	res->attr.spillslot.base.offset = INVALID_OFFSET;
	res->attr.spillslot.size = size;
	return res;
}

ir_entity *new_alias_entity(ir_type *owner, ident *name, ir_entity *aliased,
                            ir_type *type, ir_visibility visibility)
{
	ir_entity *res = intern_new_entity(owner, IR_ENTITY_ALIAS, name, type,
	                                   visibility);
	res->attr.alias.base.jit_addr = (void*)-1;
	res->attr.alias.aliased       = aliased;
	hook_new_entity(res);
	return res;
}

void set_entity_alias(ir_entity *entity, ir_entity *aliased)
{
	assert(get_entity_kind(entity) == IR_ENTITY_ALIAS);
	entity->attr.alias.aliased = aliased;
}

ir_entity *get_entity_alias(const ir_entity *entity)
{
	assert(get_entity_kind(entity) == IR_ENTITY_ALIAS);
	return entity->attr.alias.aliased;
}

/**
 * Free entity attributes.
 */
static void free_entity_attrs(ir_entity *ent)
{
	if (ent->overwrites != NULL) {
		DEL_ARR_F(ent->overwrites);
		ent->overwrites = NULL;
	}
	if (ent->overwrittenby != NULL) {
		DEL_ARR_F(ent->overwrittenby);
		ent->overwrittenby = NULL;
	}

	/* TODO: free initializers */

	if (ent->kind == IR_ENTITY_METHOD) {
		if (ent->attr.mtd_attr.param_access) {
			DEL_ARR_F(ent->attr.mtd_attr.param_access);
			ent->attr.mtd_attr.param_access = NULL;
		}
		if (ent->attr.mtd_attr.param_weight) {
			DEL_ARR_F(ent->attr.mtd_attr.param_weight);
			ent->attr.mtd_attr.param_weight = NULL;
		}
	}
}

ir_entity *clone_entity(ir_entity const *const old, ident *const name,
                        ir_type *const owner)
{
	ir_entity *res = XMALLOC(ir_entity);

	*res = *old;
	/* FIXME: the initializers are NOT copied */
	if (is_method_entity(old)) {
		/* do NOT copy them, reanalyze. This might be the best solution */
		res->attr.mtd_attr.param_access = NULL;
		res->attr.mtd_attr.param_weight = NULL;
	}
	res->overwrites    = NULL;
	res->overwrittenby = NULL;

	res->nr      = get_irp_new_node_nr();
	res->name    = name;
	res->ld_name = name;
	res->visit   = 0;
	res->usage   = ir_usage_unknown;
	res->owner   = owner;
	add_compound_member(owner, res);
	hook_new_entity(res);
	return res;
}

void free_entity(ir_entity *ent)
{
	remove_compound_member(ent->owner, ent);

	assert(ent->firm_tag == k_entity);
	free_entity_attrs(ent);
#ifdef DEBUG_libfirm
	ent->firm_tag = k_BAD;
#endif
	free(ent);
}

long get_entity_nr(const ir_entity *ent)
{
	assert(ent->firm_tag == k_entity);
	return ent->nr;
}

const char *(get_entity_name)(const ir_entity *ent)
{
	return _get_entity_name(ent);
}

ident *(get_entity_ident)(const ir_entity *ent)
{
	return _get_entity_ident(ent);
}

void (set_entity_ident)(ir_entity *ent, ident *id)
{
	_set_entity_ident(ent, id);
}

ir_type *(get_entity_owner)(const ir_entity *ent)
{
	return _get_entity_owner(ent);
}

void set_entity_owner(ir_entity *ent, ir_type *owner)
{
	assert(ent->firm_tag == k_entity);
	assert(is_compound_type(owner));

	remove_compound_member(ent->owner, ent);
	add_compound_member(owner, ent);
	ent->owner = owner;
}

ident *(get_entity_ld_ident)(const ir_entity *ent)
{
	return _get_entity_ld_ident(ent);
}

void set_entity_ld_ident(ir_entity *const ent, ident *const ld_ident)
{
	ident *old_ident = get_entity_ld_ident(ent);
	ent->ld_name = ld_ident;
	if (old_ident != ld_ident) {
		ir_type *owner = get_entity_owner(ent);
		if (is_segment_type(owner) && !(owner->flags & tf_info)
		 && get_entity_visibility(ent) != ir_visibility_private) {
			pmap *globals = irp->globals;
			pmap_insert(globals, old_ident, NULL);
			assert(NULL == pmap_get(ir_entity, globals, ld_ident));
			pmap_insert(globals, ld_ident, ent);
		}
	}
}

const char *(get_entity_ld_name)(const ir_entity *ent)
{
	return _get_entity_ld_name(ent);
}

int entity_has_ld_ident(const ir_entity *entity)
{
	return entity->ld_name != NULL;
}

ir_type *(get_entity_type)(const ir_entity *ent)
{
	return _get_entity_type(ent);
}

void set_entity_type(ir_entity *ent, ir_type *type)
{
	switch (ent->kind) {
	case IR_ENTITY_METHOD:
		assert(is_Method_type(type));
		break;
	case IR_ENTITY_NORMAL:
		assert(!is_Method_type(type));
		break;
	case IR_ENTITY_LABEL:
		assert(type == get_code_type());
		break;
	case IR_ENTITY_ALIAS:
	case IR_ENTITY_PARAMETER:
	case IR_ENTITY_UNKNOWN:
	case IR_ENTITY_COMPOUND_MEMBER:
		break;
	case IR_ENTITY_SPILLSLOT:
		panic("Cannot set type of this entity");
	}
	ent->type = type;
}

ir_volatility (get_entity_volatility)(const ir_entity *ent)
{
	return _get_entity_volatility(ent);
}

void (set_entity_volatility)(ir_entity *ent, ir_volatility vol)
{
	_set_entity_volatility(ent, vol);
}

const char *get_volatility_name(ir_volatility var)
{
#define X(a)    case a: return #a
	switch (var) {
	X(volatility_non_volatile);
	X(volatility_is_volatile);
	}
#undef X
	return "BAD VALUE";
}

ir_align (get_entity_aligned)(const ir_entity *ent)
{
	return _get_entity_aligned(ent);
}

void (set_entity_aligned)(ir_entity *ent, ir_align a)
{
	_set_entity_aligned(ent, a);
}

unsigned (get_entity_alignment)(const ir_entity *ent)
{
	return _get_entity_alignment(ent);
}

void (set_entity_alignment)(ir_entity *ent, unsigned alignment)
{
	_set_entity_alignment(ent, alignment);
}

const char *get_align_name(ir_align a)
{
#define X(a)    case a: return #a
	switch (a) {
	X(align_non_aligned);
	X(align_is_aligned);
	}
#undef X
	return "BAD VALUE";
}

void set_entity_label(ir_entity *ent, ir_label_t label)
{
	assert(ent->kind == IR_ENTITY_LABEL);
	ent->attr.code_attr.label = label;
}

ir_label_t get_entity_label(const ir_entity *ent)
{
	assert(ent->kind == IR_ENTITY_LABEL);
	return ent->attr.code_attr.label;
}

void set_entity_visibility(ir_entity *entity, ir_visibility visibility)
{
	entity->visibility = visibility;
}

ir_visibility get_entity_visibility(const ir_entity *entity)
{
	return (ir_visibility)entity->visibility;
}

void set_entity_linkage(ir_entity *entity, ir_linkage linkage)
{
	entity->linkage = linkage;
}

ir_linkage (get_entity_linkage)(const ir_entity *entity)
{
	return get_entity_linkage(entity);
}

void add_entity_linkage(ir_entity *entity, ir_linkage linkage)
{
	entity->linkage |= linkage;
}

void remove_entity_linkage(ir_entity *entity, ir_linkage linkage)
{
	entity->linkage &= ~linkage;
}

ir_entity_usage (get_entity_usage)(const ir_entity *ent)
{
	return _get_entity_usage(ent);
}

void (set_entity_usage)(ir_entity *ent, ir_entity_usage flags)
{
	_set_entity_usage(ent, flags);
}

const char *get_initializer_kind_name(ir_initializer_kind_t ini)
{
#define X(a)    case a: return #a
	switch (ini) {
	X(IR_INITIALIZER_CONST);
	X(IR_INITIALIZER_TARVAL);
	X(IR_INITIALIZER_NULL);
	X(IR_INITIALIZER_COMPOUND);
	}
#undef X
	return "BAD VALUE";
}

static ir_initializer_t null_initializer = { IR_INITIALIZER_NULL };

ir_initializer_t *get_initializer_null(void)
{
	return &null_initializer;
}

ir_initializer_t *create_initializer_const(ir_node *value)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	ir_initializer_t *initializer
		= (ir_initializer_t*)OALLOC(obst, ir_initializer_const_t);
	initializer->kind         = IR_INITIALIZER_CONST;
	initializer->consti.value = value;

	return initializer;
}

ir_initializer_t *create_initializer_tarval(ir_tarval *tv)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	ir_initializer_t *initializer
		= (ir_initializer_t*)OALLOC(obst, ir_initializer_tarval_t);
	initializer->kind         = IR_INITIALIZER_TARVAL;
	initializer->tarval.value = tv;

	return initializer;
}

ir_initializer_t *create_initializer_compound(size_t n_entries)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	size_t size = sizeof(ir_initializer_compound_t)
	            + n_entries * sizeof(ir_initializer_t*)
	            - sizeof(ir_initializer_t*);

	ir_initializer_t *initializer
		= (ir_initializer_t*)obstack_alloc(obst, size);
	initializer->kind                    = IR_INITIALIZER_COMPOUND;
	initializer->compound.n_initializers = n_entries;

	for (size_t i = 0; i < n_entries; ++i) {
		initializer->compound.initializers[i] = get_initializer_null();
	}

	return initializer;
}

ir_node *get_initializer_const_value(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_CONST);
	return skip_Id(initializer->consti.value);
}

ir_tarval *get_initializer_tarval_value(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_TARVAL);
	return initializer->tarval.value;
}

size_t get_initializer_compound_n_entries(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	return initializer->compound.n_initializers;
}

void set_initializer_compound_value(ir_initializer_t *initializer,
                                    size_t index, ir_initializer_t *value)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	assert(index < initializer->compound.n_initializers);

	initializer->compound.initializers[index] = value;
}

ir_initializer_t *get_initializer_compound_value(
		const ir_initializer_t *initializer, size_t index)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	assert(index < initializer->compound.n_initializers);

	return initializer->compound.initializers[index];
}

ir_initializer_kind_t get_initializer_kind(const ir_initializer_t *initializer)
{
	return initializer->kind;
}

static void check_entity_initializer(ir_entity *entity)
{
#ifndef NDEBUG
	ir_initializer_t *initializer = get_entity_initializer(entity);
	if (initializer == NULL)
		return;
	ir_type          *entity_tp   = get_entity_type(entity);
	switch (initializer->kind) {
	case IR_INITIALIZER_COMPOUND:
		assert(is_aggregate_type(entity_tp));
		break;

	case IR_INITIALIZER_CONST:
	case IR_INITIALIZER_TARVAL:
		assert(is_atomic_type(entity_tp));
		break;

	case IR_INITIALIZER_NULL:
		break;
	}
#else
	(void)entity;
#endif
}

void set_entity_initializer(ir_entity *entity, ir_initializer_t *initializer)
{
	assert(get_entity_kind(entity) == IR_ENTITY_NORMAL);
	entity->attr.normal.initializer = initializer;
	check_entity_initializer(entity);
}

ir_initializer_t *(get_entity_initializer)(const ir_entity *entity)
{
	return _get_entity_initializer(entity);
}

int (get_entity_offset)(const ir_entity *ent)
{
	return _get_entity_offset(ent);
}

void (set_entity_offset)(ir_entity *ent, int offset)
{
	_set_entity_offset(ent, offset);
}

unsigned (get_entity_bitfield_offset)(const ir_entity *ent)
{
	return _get_entity_bitfield_offset(ent);
}

void (set_entity_bitfield_offset)(ir_entity *ent, unsigned offset)
{
	_set_entity_bitfield_offset(ent, offset);
}

unsigned (get_entity_bitfield_size)(const ir_entity *ent)
{
	return _get_entity_bitfield_size(ent);
}

void (set_entity_bitfield_size)(ir_entity *ent, unsigned size)
{
	_set_entity_bitfield_size(ent, size);
}

void add_entity_overwrites(ir_entity *ent, ir_entity *overwritten)
{
	if (ent->overwrites == NULL) {
		ent->overwrites = NEW_ARR_F(ir_entity*, 0);
	}
	ARR_APP1(ir_entity *, ent->overwrites, overwritten);
	if (overwritten->overwrittenby == NULL) {
		overwritten->overwrittenby = NEW_ARR_F(ir_entity*, 0);
	}
	ARR_APP1(ir_entity *, overwritten->overwrittenby, ent);
}

size_t get_entity_n_overwrites(const ir_entity *ent)
{
	if (ent->overwrites == NULL)
		return 0;
	return ARR_LEN(ent->overwrites);
}

size_t get_entity_overwrites_index(const ir_entity *ent, ir_entity *overwritten)
{
	for (size_t i = 0, n = get_entity_n_overwrites(ent); i < n; ++i) {
		if (get_entity_overwrites(ent, i) == overwritten)
			return i;
	}
	return (size_t)-1;
}

ir_entity *get_entity_overwrites(const ir_entity *ent, size_t pos)
{
	assert(pos < get_entity_n_overwrites(ent));
	return ent->overwrites[pos];
}

void set_entity_overwrites(ir_entity *ent, size_t pos, ir_entity *overwritten)
{
	assert(pos < get_entity_n_overwrites(ent));
	ent->overwrites[pos] = overwritten;
}

void remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten)
{
	for (size_t i = 0, n = get_entity_n_overwrites(ent); i < n; ++i) {
		if (ent->overwrites[i] == overwritten) {
			for (; i < n - 1; i++)
				ent->overwrites[i] = ent->overwrites[i+1];
			ARR_SETLEN(ir_entity*, ent->overwrites, n - 1);
			break;
		}
	}
}


size_t get_entity_n_overwrittenby(const ir_entity *ent)
{
	if (ent->overwrittenby == NULL)
		return 0;
	return ARR_LEN(ent->overwrittenby);
}

size_t get_entity_overwrittenby_index(const ir_entity *ent,
                                      ir_entity *overwrites)
{
	for (size_t i = 0, n = get_entity_n_overwrittenby(ent); i < n; ++i) {
		if (get_entity_overwrittenby(ent, i) == overwrites)
			return i;
	}
	return (size_t)-1;
}

ir_entity *get_entity_overwrittenby(const ir_entity *ent, size_t pos)
{
	assert(pos < get_entity_n_overwrittenby(ent));
	return ent->overwrittenby[pos];
}

void set_entity_overwrittenby(ir_entity *ent, size_t pos, ir_entity *overwrites)
{
	assert(pos < get_entity_n_overwrittenby(ent));
	ent->overwrittenby[pos] = overwrites;
}

void remove_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites)
{
	for (size_t i = 0, n = get_entity_n_overwrittenby(ent); i < n; ++i) {
		if (ent->overwrittenby[i] == overwrites) {
			for (; i < n - 1; ++i)
				ent->overwrittenby[i] = ent->overwrittenby[i+1];
			ARR_SETLEN(ir_entity*, ent->overwrittenby, n - 1);
			break;
		}
	}
}

void *(get_entity_link)(const ir_entity *ent)
{
	return _get_entity_link(ent);
}

void (set_entity_link)(ir_entity *ent, void *l)
{
	_set_entity_link(ent, l);
}

ir_graph *(get_entity_irg)(const ir_entity *ent)
{
	return _get_entity_irg(ent);
}

ir_graph *(get_entity_linktime_irg)(const ir_entity *ent)
{
	return _get_entity_linktime_irg(ent);
}

void set_entity_irg(ir_entity *ent, ir_graph *irg)
{
	assert(is_method_entity(ent));
	ent->attr.mtd_attr.irg = irg;
}

int (is_parameter_entity)(const ir_entity *entity)
{
	return _is_parameter_entity(entity);
}

size_t (get_entity_parameter_number)(const ir_entity *entity)
{
	return _get_entity_parameter_number(entity);
}

void set_entity_parameter_number(ir_entity *entity, size_t n)
{
	assert(is_parameter_entity(entity));
	entity->attr.parameter.number = n;
}

unsigned get_entity_vtable_number(const ir_entity *ent)
{
	assert(is_method_entity(ent));
	return ent->attr.mtd_attr.vtable_number;
}

void set_entity_vtable_number(ir_entity *ent, unsigned vtable_number)
{
	assert(is_method_entity(ent));
	ent->attr.mtd_attr.vtable_number = vtable_number;
}

int is_unknown_entity(const ir_entity *entity)
{
	return entity->kind == IR_ENTITY_UNKNOWN;
}

int is_compound_entity(const ir_entity *ent)
{
	ir_type const *const type = get_entity_type(ent);
	return is_compound_type(type);
}

int is_method_entity(const ir_entity *ent)
{
	return ent->kind == IR_ENTITY_METHOD;
}

int is_alias_entity(const ir_entity *entity)
{
	return entity->kind == IR_ENTITY_ALIAS;
}

ir_visited_t (get_entity_visited)(const ir_entity *ent)
{
	return _get_entity_visited(ent);
}

void (set_entity_visited)(ir_entity *ent, ir_visited_t num)
{
	_set_entity_visited(ent, num);
}

void (mark_entity_visited)(ir_entity *ent)
{
	_mark_entity_visited(ent);
}

int (entity_visited)(const ir_entity *ent)
{
	return _entity_visited(ent);
}

int (entity_not_visited)(const ir_entity *ent)
{
	return _entity_not_visited(ent);
}

int entity_has_additional_properties(const ir_entity *entity)
{
	return entity->kind == IR_ENTITY_METHOD || entity->kind == IR_ENTITY_ALIAS;
}

mtp_additional_properties get_entity_additional_properties(const ir_entity *ent)
{
	assert(entity_has_additional_properties(ent));
	return ent->attr.global.properties;
}

void set_entity_additional_properties(ir_entity *ent,
                                      mtp_additional_properties property_mask)
{
	assert(entity_has_additional_properties(ent));
	/* you mustn't set fewer properties than the entities type */
	assert((get_method_additional_properties(get_entity_type(ent)) & ~property_mask) == 0);

	/* do not allow to set the mtp_property_inherited flag or
	 * the automatic inheritance of flags will not work */
	ent->attr.global.properties = property_mask;
}

void add_entity_additional_properties(ir_entity *ent,
                                      mtp_additional_properties properties)
{
	assert(entity_has_additional_properties(ent));

	/* do not allow to set the mtp_property_inherited flag or
	 * the automatic inheritance of flags will not work */
	ent->attr.global.properties |= properties;
}

dbg_info *(get_entity_dbg_info)(const ir_entity *ent)
{
	return _get_entity_dbg_info(ent);
}

void (set_entity_dbg_info)(ir_entity *ent, dbg_info *db)
{
	_set_entity_dbg_info(ent, db);
}

int entity_is_externally_visible(const ir_entity *entity)
{
	ir_visibility visibility = get_entity_visibility(entity);
	switch (visibility) {
	case ir_visibility_local:
	case ir_visibility_private:
		return (get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER) != 0;
	case ir_visibility_external:
	case ir_visibility_external_private:
	case ir_visibility_external_protected:
		return true;
	}
	panic("Invalid visibility for entity %+F", entity);
}

int entity_has_definition(const ir_entity *entity)
{
	switch (get_entity_kind(entity)) {
	case IR_ENTITY_METHOD:
		return get_entity_irg(entity) != NULL
		    && (get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN) == 0;

	case IR_ENTITY_NORMAL:
		return get_entity_initializer(entity) != NULL;

	case IR_ENTITY_LABEL:
	case IR_ENTITY_ALIAS:
		return true;
	case IR_ENTITY_PARAMETER:
	case IR_ENTITY_UNKNOWN:
	case IR_ENTITY_COMPOUND_MEMBER:
	case IR_ENTITY_SPILLSLOT:
		return false;
	}
	panic("invalid entity kind");
}

void ir_init_entity(ir_prog *irp)
{
	ident   *const id    = new_id_from_str(UNKNOWN_ENTITY_NAME);
	ir_type *const utype = get_unknown_type();
	irp->unknown_entity = intern_new_entity(irp->dummy_owner, IR_ENTITY_UNKNOWN,
	                                        id, utype, ir_visibility_external);
	hook_new_entity(irp->unknown_entity);
}
