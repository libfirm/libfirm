/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of all program known entities.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#include <stdlib.h>
#include <stddef.h>

#include "xmalloc.h"
#include "entity_t.h"
#include "array.h"
#include "util.h"
#include "irhooks.h"
#include "irprog_t.h"
#include "ircons_t.h"
#include "tv_t.h"
#include "irdump.h"
#include "irgraph_t.h"
#include "callgraph.h"
#include "error.h"

/** The name of the unknown entity. */
#define UNKNOWN_ENTITY_NAME "unknown_entity"

ir_entity *get_unknown_entity(void)
{
	return irp->unknown_entity;
}

static ir_entity *intern_new_entity(ir_type *owner, ir_entity_kind kind,
                                    ident *name, ir_type *type)
{
	assert(owner != NULL);

	ir_entity *res = XMALLOCZ(ir_entity);
	res->kind        = k_entity;
	res->name        = name;
	res->type        = type;
	res->owner       = owner;
	res->entity_kind = kind;
	res->volatility  = volatility_non_volatile;
	res->aligned     = align_is_aligned;
	res->usage       = ir_usage_unknown;
	res->visibility  = ir_visibility_external;
#ifdef DEBUG_libfirm
	res->nr          = get_irp_new_node_nr();
#endif

	/* Remember entity in its owner. */
	if (is_compound_type(owner))
		add_compound_member(owner, res);

	res->visit = 0;
	return res;
}

ir_entity *new_entity(ir_type *owner, ident *name, ir_type *type)
{
	ir_entity *res;
	if (is_Method_type(type)) {
		ir_graph *irg = get_const_code_irg();
		res                = intern_new_entity(owner, IR_ENTITY_METHOD, name, type);
		ir_node *const val = new_r_Address(irg, res);
		set_atomic_ent_value(res, val);
		res->linkage                   = IR_LINKAGE_CONSTANT;
		res->attr.mtd_attr.properties  = get_method_additional_properties(type);
		res->attr.mtd_attr.vtable_number = IR_VTABLE_NUM_NOT_SET;
		res->attr.mtd_attr.param_access  = NULL;
		res->attr.mtd_attr.param_weight  = NULL;
		res->attr.mtd_attr.irg           = NULL;
	} else if (is_compound_type(owner) && !(owner->flags & tf_segment)) {
		res = intern_new_entity(owner, IR_ENTITY_COMPOUND_MEMBER, name, type);
		res->attr.compound_member.offset = -1;
	} else {
		res = intern_new_entity(owner, IR_ENTITY_NORMAL, name, type);
	}

	hook_new_entity(res);
	return res;
}

static ident *make_parameter_entity_name(size_t pos)
{
	char buf[64];
	snprintf(buf, sizeof(buf), "parameter.%lu", (unsigned long) pos);
	return new_id_from_str(buf);
}

ir_entity *new_parameter_entity(ir_type *owner, size_t pos, ir_type *type)
{
	ident     *name = make_parameter_entity_name(pos);
	ir_entity *res  = intern_new_entity(owner, IR_ENTITY_PARAMETER, name, type);
	res->attr.compound_member.offset = -1;
	res->attr.parameter.number = pos;
	hook_new_entity(res);
	return res;
}

ir_entity *new_label_entity(ir_label_t label)
{
	ident *name = id_unique("label_%u");
	ir_type *global_type = get_glob_type();
	ir_entity *res = intern_new_entity(global_type, IR_ENTITY_LABEL, name,
	                                   get_code_type());
	res->attr.code_attr.label = label;
	hook_new_entity(res);
	return res;
}

ir_entity *new_got_entry_entity(ir_entity *referenced)
{
	ir_type *reftype = get_entity_type(referenced);
	ir_type *pointer = new_type_pointer(reftype);
	ir_type *global_type = get_glob_type();
	ir_entity *res = intern_new_entity(global_type, IR_ENTITY_GOTENTRY, NULL,
	                                   pointer);
	res->attr.got.referenced = referenced;
	hook_new_entity(res);
	return res;
}

ir_entity *new_alias_entity(ir_type *owner, ident *name, ir_entity *aliased,
                            ir_type *type)
{
	ir_entity *res = intern_new_entity(owner, IR_ENTITY_ALIAS, name, type);
	res->attr.alias.aliased = aliased;
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

	if (ent->initializer != NULL) {
		/* TODO: free initializers */
	}
	if (ent->entity_kind == IR_ENTITY_METHOD) {
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

/**
 * Creates a deep copy of an entity.
 */
static ir_entity *deep_entity_copy(ir_entity *old)
{
	ir_entity *newe = XMALLOC(ir_entity);

	*newe = *old;
	if (old->initializer != NULL) {
		/* FIXME: the initializers are NOT copied */
	} else if (is_method_entity(old)) {
		/* do NOT copy them, reanalyze. This might be the best solution */
		newe->attr.mtd_attr.param_access = NULL;
		newe->attr.mtd_attr.param_weight = NULL;
	}
	newe->overwrites    = NULL;
	newe->overwrittenby = NULL;

#ifdef DEBUG_libfirm
	newe->nr = get_irp_new_node_nr();
#endif
	hook_new_entity(newe);
	return newe;
}

ir_entity *copy_entity_own(ir_entity *old, ir_type *new_owner)
{
	assert(is_entity(old));
	assert(is_compound_type(new_owner));
	assert(get_type_state(new_owner) != layout_fixed);
	if (old->owner == new_owner)
		return old;

	/* create a deep copy so we are safe of aliasing and double-freeing. */
	ir_entity *newe = deep_entity_copy(old);
	newe->owner = new_owner;
	add_compound_member(new_owner, newe);

	return newe;
}

ir_entity *copy_entity_name(ir_entity *old, ident *new_name)
{
	assert(old->kind == k_entity);
	if (old->name == new_name)
		return old;

	ir_entity *newe = deep_entity_copy(old);
	newe->name    = new_name;
	newe->ld_name = NULL;
	add_compound_member(old->owner, newe);

	return newe;
}

void free_entity(ir_entity *ent)
{
	if (is_compound_type(ent->owner))
		remove_compound_member(ent->owner, ent);

	assert(ent->kind == k_entity);
	free_entity_attrs(ent);
#ifdef DEBUG_libfirm
	ent->kind = k_BAD;
#endif
	free(ent);
}

long get_entity_nr(const ir_entity *ent)
{
	assert(ent->kind == k_entity);
#ifdef DEBUG_libfirm
	return ent->nr;
#else
	return (long)PTR_TO_INT(ent);
#endif
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
	assert(is_entity(ent));
	assert(is_compound_type(owner));

	remove_compound_member(ent->owner, ent);
	add_compound_member(owner, ent);
	ent->owner = owner;
}

ident *(get_entity_ld_ident)(const ir_entity *ent)
{
	return _get_entity_ld_ident(ent);
}

void (set_entity_ld_ident)(ir_entity *ent, ident *ld_ident)
{
	_set_entity_ld_ident(ent, ld_ident);
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
	switch (ent->entity_kind) {
	case IR_ENTITY_METHOD:
		assert(is_Method_type(type));
		break;
	case IR_ENTITY_NORMAL:
		assert(!is_Method_type(type));
		break;
	case IR_ENTITY_LABEL:
		assert(type == get_code_type());
		break;
	case IR_ENTITY_COMPOUND_MEMBER:
		break;
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
	assert(ent->entity_kind == IR_ENTITY_LABEL);
	ent->attr.code_attr.label = label;
}

ir_label_t get_entity_label(const ir_entity *ent)
{
	assert(ent->entity_kind == IR_ENTITY_LABEL);
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

int (is_entity_compiler_generated)(const ir_entity *ent)
{
	return _is_entity_compiler_generated(ent);
}

void (set_entity_compiler_generated)(ir_entity *ent, int flag)
{
	_set_entity_compiler_generated(ent, flag);
}

ir_entity_usage (get_entity_usage)(const ir_entity *ent)
{
	return _get_entity_usage(ent);
}

void (set_entity_usage)(ir_entity *ent, ir_entity_usage flags)
{
	_set_entity_usage(ent, flags);
}

ir_node *get_atomic_ent_value(const ir_entity *entity)
{
	assert(is_atomic_entity(entity));

	ir_initializer_t *initializer = get_entity_initializer(entity);
	if (initializer == NULL) {
		ir_type *type = get_entity_type(entity);
		return new_r_Unknown(get_const_code_irg(), get_type_mode(type));
	}

	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL: {
		ir_type *type = get_entity_type(entity);
		ir_mode *mode = get_type_mode(type);
		return new_r_Const_null(get_const_code_irg(), mode);
	}
	case IR_INITIALIZER_TARVAL: {
		ir_tarval *tv = get_initializer_tarval_value(initializer);
		return new_r_Const(get_const_code_irg(), tv);
	}
	case IR_INITIALIZER_CONST:
		return get_initializer_const_value(initializer);
	case IR_INITIALIZER_COMPOUND:
		panic("compound initializer in atomic entity not allowed (%+F)", entity);
	}

	panic("invalid initializer kind (%+F)", entity);
}

void set_atomic_ent_value(ir_entity *entity, ir_node *val)
{
	assert(is_atomic_entity(entity));
	assert(is_Dummy(val) || get_irn_mode(val) == get_type_mode(entity->type));
	ir_initializer_t *initializer = create_initializer_const(val);
	entity->initializer = initializer;
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
	ir_initializer_t *initializer = entity->initializer;
	if (initializer == NULL)
		return;
	ir_type          *entity_tp   = get_entity_type(entity);
	switch (initializer->kind) {
	case IR_INITIALIZER_COMPOUND:
		assert(is_aggregate_type(entity_tp));
		break;
	case IR_INITIALIZER_CONST:
		/* methods are initialized by an Address */
		assert(is_atomic_type(entity_tp) || is_Method_type(entity_tp));
		break;
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
	entity->initializer = initializer;
	check_entity_initializer(entity);
}

int has_entity_initializer(const ir_entity *entity)
{
	return entity->initializer != NULL;
}

ir_initializer_t *get_entity_initializer(const ir_entity *entity)
{
	return entity->initializer;
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
	return entity->entity_kind == IR_ENTITY_UNKNOWN;
}

int (is_entity)(const void *thing)
{
	return _is_entity(thing);
}

int is_atomic_entity(const ir_entity *ent)
{
	ir_type     *t  = get_entity_type(ent);
	const tp_op *op = get_type_tpop(t);
	return op == type_primitive || op == type_pointer
	    || op == type_method;
}

int is_compound_entity(const ir_entity *ent)
{
	ir_type     *t  = get_entity_type(ent);
	const tp_op *op = get_type_tpop(t);
	return (op == type_class || op == type_struct ||
	        op == type_array || op == type_union);
}

int is_method_entity(const ir_entity *ent)
{
	return ent->entity_kind == IR_ENTITY_METHOD;
}

int is_alias_entity(const ir_entity *entity)
{
	return entity->entity_kind == IR_ENTITY_ALIAS;
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
	return entity->entity_kind == IR_ENTITY_METHOD
	    || entity->entity_kind == IR_ENTITY_ALIAS;
}

mtp_additional_properties get_entity_additional_properties(const ir_entity *ent)
{
	assert(entity_has_additional_properties(ent));
	return ent->attr.properties;
}

void set_entity_additional_properties(ir_entity *ent,
                                      mtp_additional_properties property_mask)
{
	assert(entity_has_additional_properties(ent));
	/* you mustn't set fewer properties than the entities type */
	assert((get_method_additional_properties(get_entity_type(ent)) & ~property_mask) == 0);

	/* do not allow to set the mtp_property_inherited flag or
	 * the automatic inheritance of flags will not work */
	ent->attr.properties = property_mask;
}

void add_entity_additional_properties(ir_entity *ent,
                                      mtp_additional_properties properties)
{
	assert(entity_has_additional_properties(ent));

	/* do not allow to set the mtp_property_inherited flag or
	 * the automatic inheritance of flags will not work */
	ent->attr.properties |= properties;
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
	return get_entity_visibility(entity) != ir_visibility_local
		|| (get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER);
}

int entity_has_definition(const ir_entity *entity)
{
	switch (get_entity_kind(entity)) {
	case IR_ENTITY_METHOD:
		return get_entity_irg(entity) != NULL
		    && (get_entity_linkage(entity) & IR_LINKAGE_NO_CODEGEN) == 0;
	case IR_ENTITY_NORMAL:
		return entity->initializer != NULL;
	case IR_ENTITY_LABEL:
	case IR_ENTITY_ALIAS:
		return true;
	case IR_ENTITY_PARAMETER:
	case IR_ENTITY_UNKNOWN:
	case IR_ENTITY_COMPOUND_MEMBER:
	case IR_ENTITY_GOTENTRY:
		return false;
	}
	panic("invalid entity kind");
}

void ir_init_entity(ir_prog *irp)
{
	ident   *const id    = new_id_from_str(UNKNOWN_ENTITY_NAME);
	ir_type *const utype = get_unknown_type();
	irp->unknown_entity = intern_new_entity(utype, IR_ENTITY_UNKNOWN, id, utype);
	set_entity_visibility(irp->unknown_entity, ir_visibility_external);
	set_entity_ld_ident(irp->unknown_entity, id);
	hook_new_entity(irp->unknown_entity);
}

void ir_finish_entity(ir_prog *irp)
{
	free_entity(irp->unknown_entity);
}
