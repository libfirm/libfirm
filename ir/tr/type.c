/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of types.
 * @author  Goetz Lindenmaier, Michael Beck
 * @brief
 *
 *  Implementation of the datastructure to hold
 *  type information.
 *
 *  This module supplies a datastructure to represent all types
 *  known in the compiled program.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  A type is described by a set of attributes.  Some of
 *  these attributes are common to all types, others depend on the
 *  kind of the type.
 *
 *  Types are different from the modes defined in irmode:  Types are
 *  on the level of the programming language, modes at the level of
 *  the target processor.
 */
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include "type_t.h"

#include "xmalloc.h"
#include "irprog_t.h"
#include "ircons.h"
#include "tpop_t.h"
#include "tv_t.h"
#include "irhooks.h"
#include "util.h"
#include "entity_t.h"
#include "error.h"
#include "dbginfo.h"
#include "irprog_t.h"

#include "array.h"

static ir_type *new_type(tp_op const *type_op, ir_mode *mode, type_dbg_info *db);

ir_type *get_code_type(void)
{
	return irp->code_type;
}

ir_type *get_unknown_type(void)
{
	return irp->unknown_type;
}

void ir_init_type(ir_prog *irp)
{
	irp->code_type = new_type(tpop_code, mode_ANY, NULL);
	set_type_state(irp->code_type, layout_fixed);

	irp->unknown_type = new_type(tpop_unknown, mode_ANY, NULL);
	set_type_size_bytes(irp->unknown_type, 0);
	set_type_state (irp->unknown_type, layout_fixed);
}

void ir_finish_type(ir_prog *irp)
{
	/** nothing todo. (The code, unknown types are in the global type list
	 * and freed there */
	(void)irp;
}

ir_visited_t firm_type_visited;

void (set_master_type_visited)(ir_visited_t val)
{
	_set_master_type_visited(val);
}

ir_visited_t (get_master_type_visited)(void)
{
	return _get_master_type_visited();
}

void (inc_master_type_visited)(void)
{
	_inc_master_type_visited();
}

/**
 *   Creates a new type representation:
 *
 *   @param type_op  the kind of this type.  May not be type_id.
 *   @param mode     the mode to be used for this type, may be NULL
 *   @param db       debug info
 *
 *   @return A new type of the given type.  The remaining private attributes are
 *           not initialized.  The type is in state layout_undefined.
 */
static ir_type *new_type(tp_op const *type_op, ir_mode *mode, type_dbg_info *db)
{
	size_t   const node_size = offsetof(ir_type, attr) +  type_op->attr_size;
	ir_type *const res       = (ir_type*)xmalloc(node_size);
	memset(res, 0, node_size);

	res->kind       = k_type;
	res->type_op    = type_op;
	res->mode       = mode;
	res->visibility = ir_visibility_external;
	res->flags      = tf_none;
	res->size       = 0;
	res->align      = 0;
	res->visit      = 0;
	res->link       = NULL;
	res->dbi        = db;
#ifdef DEBUG_libfirm
	res->nr         = get_irp_new_node_nr();
#endif /* defined DEBUG_libfirm */

	add_irp_type(res);   /* Remember the new type global. */

	return res;
}

void free_type_entities(ir_type *tp)
{
	const tp_op *op = get_type_tpop(tp);
	if (op->ops.free_entities != NULL)
		op->ops.free_entities(tp);
}

static void free_type_attrs(ir_type *tp)
{
	const tp_op *tpop = get_type_tpop(tp);
	if (tpop->ops.free_attrs)
		tpop->ops.free_attrs(tp);
}

void free_type(ir_type *tp)
{
	const tp_op *op = get_type_tpop(tp);

	free_type_entities(tp);
	/* Remove from list of all types */
	remove_irp_type(tp);
	/* Free the attributes of the type. */
	free_type_attrs(tp);
	/* Free entities automatically allocated with the ir_type */
	if (op->ops.free_auto_entities)
		op->ops.free_auto_entities(tp);
	/* And now the type itself... */
#ifdef DEBUG_libfirm
	tp->kind = k_BAD;
#endif
	free(tp);
}

void *(get_type_link)(const ir_type *tp)
{
	return _get_type_link(tp);
}

void (set_type_link)(ir_type *tp, void *l)
{
	_set_type_link(tp, l);
}

const tp_op *(get_type_tpop)(const ir_type *tp)
{
	return _get_type_tpop(tp);
}

ident *(get_type_tpop_nameid)(const ir_type *tp)
{
	return _get_type_tpop_nameid(tp);
}

const char* get_type_tpop_name(const ir_type *tp)
{
	assert(tp && tp->kind == k_type);
	return get_id_str(tp->type_op->name);
}

tp_opcode (get_type_tpop_code)(const ir_type *tp)
{
	return _get_type_tpop_code(tp);
}

ir_mode *(get_type_mode)(const ir_type *tp)
{
	return _get_type_mode(tp);
}

void set_type_mode(ir_type *tp, ir_mode *mode)
{
	const tp_op *tpop = get_type_tpop(tp);
	if (tpop->ops.set_type_mode) {
		tpop->ops.set_type_mode(tp, mode);
	} else {
		panic("setting a mode is NOT allowed for this type");
	}
}

long get_type_nr(const ir_type *tp)
{
	assert(tp);
#ifdef DEBUG_libfirm
	return tp->nr;
#else
	return (long)PTR_TO_INT(tp);
#endif
}

unsigned (get_type_size_bytes)(const ir_type *tp)
{
	return _get_type_size_bytes(tp);
}

ir_visibility get_type_visibility(const ir_type *tp)
{
	assert(is_type(tp));
	return tp->visibility;
}

void set_type_visibility(ir_type *tp, ir_visibility v)
{
	assert(is_type(tp));
	tp->visibility = v;
}

void set_type_size_bytes(ir_type *tp, unsigned size)
{
	const tp_op *tpop = get_type_tpop(tp);
	tpop->ops.set_type_size(tp, size);
}

unsigned get_type_alignment_bytes(ir_type *tp)
{
	unsigned align = 1;
	if (tp->align > 0)
		return tp->align;

	/* alignment NOT set calculate it "on demand" */
	if (tp->mode)
		align = (get_mode_size_bits(tp->mode) + 7) >> 3;
	else if (is_Array_type(tp))
		align = get_type_alignment_bytes(get_array_element_type(tp));
	else if (is_compound_type(tp)) {
		align = 0;
		for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
			ir_type  *t = get_entity_type(get_compound_member(tp, i));
			unsigned  a = get_type_alignment_bytes(t);

			if (a > align)
				align = a;
		}
	} else if (is_Method_type(tp)) {
		align = 0;
	}

	/* write back */
	tp->align = align;

	return align;
}

void set_type_alignment_bytes(ir_type *tp, unsigned align)
{
	assert(tp->kind == k_type);
	/* Methods don't have an alignment. */
	if (tp->type_op != type_method) {
		tp->align = align;
	}
}

const char *get_type_state_name(ir_type_state s)
{
#define X(a)    case a: return #a
	switch (s) {
		X(layout_undefined);
		X(layout_fixed);
	}
	return "<unknown>";
#undef X
}

ir_type_state (get_type_state)(const ir_type *tp)
{
	return _get_type_state(tp);
}

void set_type_state(ir_type *tp, ir_type_state state)
{
	assert(tp && tp->kind == k_type);

	if (tp->type_op == type_pointer || tp->type_op == type_primitive
	    || tp->type_op == type_method)
		return;

#ifndef NDEBUG
	/* Just a correctness check: */
	if (state == layout_fixed) {
		switch (get_type_tpop_code(tp)) {
		case tpo_class:
			if (tp != get_glob_type()) {
				for (size_t i = 0, n_mem = get_class_n_members(tp);
				     i < n_mem; i++) {
					ir_entity *entity = get_class_member(tp, i);
					if (is_Method_type(get_entity_type(entity)))
						continue;
					assert(get_entity_offset(entity) > -1);
				}
			}
			break;
		case tpo_struct:
			for (size_t i = 0, n_members = get_struct_n_members(tp);
			     i < n_members; i++) {
				assert(get_entity_offset(get_struct_member(tp, i)) > -1);
			}
			break;
		case tpo_union:
		case tpo_array:
		default:
			break;
		}
	}
#endif
	if (state == layout_fixed)
		tp->flags |= tf_layout_fixed;
	else
		tp->flags &= ~tf_layout_fixed;
}

ir_visited_t (get_type_visited)(const ir_type *tp)
{
	return _get_type_visited(tp);
}

void (set_type_visited)(ir_type *tp, ir_visited_t num)
{
	_set_type_visited(tp, num);
}

void (mark_type_visited)(ir_type *tp)
{
	_mark_type_visited(tp);
}

int (type_visited)(const ir_type *tp)
{
	return _type_visited(tp);
}

int (type_not_visited)(const ir_type *tp)
{
	return _type_not_visited(tp);
}

type_dbg_info *(get_type_dbg_info)(const ir_type *tp)
{
	return _get_type_dbg_info(tp);
}

void (set_type_dbg_info)(ir_type *tp, type_dbg_info *db)
{
	_set_type_dbg_info(tp, db);
}

int (is_type)(const void *thing)
{
	return _is_type(thing);
}

ir_type *new_d_type_class(ident *name, type_dbg_info *db)
{
	ir_type *res = new_type(type_class, NULL, db);
	res->name = name;

	res->attr.ca.members     = NEW_ARR_F (ir_entity *, 0);
	res->attr.ca.subtypes    = NEW_ARR_F (ir_type *, 0);
	res->attr.ca.supertypes  = NEW_ARR_F (ir_type *, 0);
	res->attr.ca.peculiarity = peculiarity_existent;
	res->attr.ca.vtable_size = 0;
	res->attr.ca.clss_flags  = cf_none;
	res->attr.ca.dfn         = 0;
	hook_new_type(res);
	return res;
}

ir_type *new_type_class(ident *name)
{
	return new_d_type_class(name, NULL);
}

void free_class_entities(ir_type *clss)
{
	assert(clss && clss->type_op == type_class);
	/* we must iterate backward here */
	for (size_t i = get_class_n_members(clss); i-- > 0;) {
		free_entity(get_class_member(clss, i));
	}
}

void free_class_attrs(ir_type *clss)
{
	assert(clss && (clss->type_op == type_class));
	DEL_ARR_F(clss->attr.ca.members);
	DEL_ARR_F(clss->attr.ca.subtypes);
	DEL_ARR_F(clss->attr.ca.supertypes);
}

ident *get_class_ident(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->name;
}

const char *get_class_name(const ir_type *clss)
{
	if (get_class_ident(clss) == NULL)
		return NULL;
	return get_id_str(get_class_ident(clss));
}

static void add_class_member(ir_type *clss, ir_entity *member)
{
	assert(clss->type_op == type_class);
	assert(clss != get_entity_type(member));
	ARR_APP1(ir_entity *, clss->attr.ca.members, member);
}

size_t (get_class_n_members)(const ir_type *clss)
{
	return _get_class_n_members(clss);
}

size_t get_class_member_index(const ir_type *clss, ir_entity *mem)
{
	assert(clss->type_op == type_class);
	for (size_t i = 0, n = get_class_n_members(clss); i < n; ++i) {
		if (get_class_member(clss, i) == mem)
			return i;
	}
	return INVALID_MEMBER_INDEX;
}

ir_entity *(get_class_member)(const ir_type *clss, size_t pos)
{
	return _get_class_member(clss, pos);
}

ir_entity *get_class_member_by_name(ir_type *clss, ident *name)
{
	assert(clss->type_op == type_class);
	for (size_t i = 0, n_mem = get_class_n_members(clss); i < n_mem; ++i) {
		ir_entity *mem = get_class_member(clss, i);
		if (get_entity_ident(mem) == name)
			return mem;
	}
	return NULL;
}

static void remove_class_member(ir_type *clss, ir_entity *member)
{
	assert(clss && (clss->type_op == type_class));
	for (size_t i = 0; i < ARR_LEN(clss->attr.ca.members); ++i) {
		if (clss->attr.ca.members[i] == member) {
			for (; i < ARR_LEN(clss->attr.ca.members) - 1; ++i)
				clss->attr.ca.members[i] = clss->attr.ca.members[i + 1];
			ARR_SETLEN(ir_entity*, clss->attr.ca.members, ARR_LEN(clss->attr.ca.members) - 1);
			break;
		}
	}
}

void add_class_subtype(ir_type *clss, ir_type *subtype)
{
	assert(clss->type_op == type_class);
	ARR_APP1(ir_type *, clss->attr.ca.subtypes, subtype);
	for (size_t i = 0, n_supertypes = get_class_n_supertypes(subtype);
	     i < n_supertypes; i++) {
		if (get_class_supertype(subtype, i) == clss)
			/* Class already registered */
			return;
	}
	ARR_APP1(ir_type *, subtype->attr.ca.supertypes, clss);
}

size_t get_class_n_subtypes(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return ARR_LEN(clss->attr.ca.subtypes);
}

ir_type *get_class_subtype(const ir_type *clss, size_t pos)
{
	assert(clss->type_op == type_class);
	assert(pos < get_class_n_subtypes(clss));
	return clss->attr.ca.subtypes[pos];
}

size_t get_class_subtype_index(const ir_type *clss, const ir_type *subclass)
{
	assert(is_Class_type(subclass));
	for (size_t i = 0, n_subtypes = get_class_n_subtypes(clss);
	     i < n_subtypes; ++i) {
		if (get_class_subtype(clss, i) == subclass)
			return i;
	}
	return (size_t)-1;
}

void set_class_subtype(ir_type *clss, ir_type *subtype, size_t pos)
{
	assert(clss->type_op == type_class);
	assert(pos < get_class_n_subtypes(clss));
	clss->attr.ca.subtypes[pos] = subtype;
}

void remove_class_subtype(ir_type *clss, ir_type *subtype)
{
	assert(clss && (clss->type_op == type_class));
	for (size_t i = 0; i < ARR_LEN(clss->attr.ca.subtypes); ++i) {
		if (clss->attr.ca.subtypes[i] == subtype) {
			for (; i < ARR_LEN(clss->attr.ca.subtypes) - 1; ++i)
				clss->attr.ca.subtypes[i] = clss->attr.ca.subtypes[i+1];
			ARR_SETLEN(ir_type*, clss->attr.ca.subtypes, ARR_LEN(clss->attr.ca.subtypes) - 1);
			break;
		}
	}
}

void add_class_supertype(ir_type *clss, ir_type *supertype)
{
	assert(clss->type_op == type_class);
	assert(supertype->type_op == type_class);
	ARR_APP1(ir_type *, clss->attr.ca.supertypes, supertype);
	for (size_t i = 0, n = get_class_n_subtypes(supertype); i < n; ++i) {
		if (get_class_subtype(supertype, i) == clss)
			/* Class already registered */
			return;
	}
	ARR_APP1(ir_type *, supertype->attr.ca.subtypes, clss);
}

size_t get_class_n_supertypes(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return ARR_LEN(clss->attr.ca.supertypes);
}

size_t get_class_supertype_index(const ir_type *clss, const ir_type *super_clss)
{
	assert(super_clss->type_op == type_class);
	for (size_t i = 0, n_supertypes = get_class_n_supertypes(clss);
	     i < n_supertypes; i++) {
		if (get_class_supertype(clss, i) == super_clss)
			return i;
	}
	return (size_t)-1;
}

ir_type *get_class_supertype(const ir_type *clss, size_t pos)
{
	assert(clss->type_op == type_class);
	assert(pos < get_class_n_supertypes(clss));
	return clss->attr.ca.supertypes[pos];
}

void set_class_supertype(ir_type *clss, ir_type *supertype, size_t pos)
{
	assert(clss->type_op == type_class);
	assert(pos < get_class_n_supertypes(clss));
	clss->attr.ca.supertypes[pos] = supertype;
}

void remove_class_supertype(ir_type *clss, ir_type *supertype)
{
	assert(clss->type_op == type_class);
	for (size_t i = 0; i < ARR_LEN(clss->attr.ca.supertypes); ++i) {
		if (clss->attr.ca.supertypes[i] == supertype) {
			for (; i < ARR_LEN(clss->attr.ca.supertypes) - 1; ++i)
				clss->attr.ca.supertypes[i] = clss->attr.ca.supertypes[i+1];
			ARR_SETLEN(ir_type*, clss->attr.ca.supertypes, ARR_LEN(clss->attr.ca.supertypes) - 1);
			break;
		}
	}
}

ir_peculiarity get_class_peculiarity(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->attr.ca.peculiarity;
}

void set_class_peculiarity(ir_type *clss, ir_peculiarity pec)
{
	assert(clss->type_op == type_class);
	assert(pec != peculiarity_inherited);  /* There is no inheritance of types in libFirm. */
	clss->attr.ca.peculiarity = pec;
}

unsigned (get_class_vtable_size)(const ir_type *clss)
{
	return _get_class_vtable_size(clss);
}

void (set_class_vtable_size)(ir_type *clss, unsigned size)
{
	_set_class_vtable_size(clss, size);
}

int (is_class_final)(const ir_type *clss)
{
	return _is_class_final(clss);
}

void (set_class_final)(ir_type *clss, int flag)
{
	_set_class_final(clss, flag);
}

int (is_class_interface)(const ir_type *clss)
{
	return _is_class_interface(clss);
}

void (set_class_interface)(ir_type *clss, int flag)
{
	_set_class_interface(clss, flag);
}

int (is_class_abstract)(const ir_type *clss)
{
	 return _is_class_abstract(clss);
}

void (set_class_abstract)(ir_type *clss, int final)
{
	_set_class_abstract(clss, final);
}

void set_class_dfn(ir_type *clss, int dfn)
{
	clss->attr.ca.dfn = dfn;
}

int get_class_dfn(const ir_type *clss)
{
	return (clss->attr.ca.dfn);
}

int (is_Class_type)(const ir_type *clss)
{
	return _is_class_type(clss);
}

void set_class_mode(ir_type *tp, ir_mode *mode)
{
	/* for classes and structs we allow to set a mode if the layout is fixed AND the size matches */
	assert(get_type_state(tp) == layout_fixed &&
	       tp->size == get_mode_size_bytes(mode) && "mode don't match class layout");
	tp->mode = mode;
}

void set_class_size(ir_type *tp, unsigned size)
{
	tp->size = size;
}


ir_type *new_d_type_struct(ident *name, type_dbg_info *db)
{
	ir_type *res = new_type(type_struct, NULL, db);
	res->name = name;

	res->attr.sa.members = NEW_ARR_F(ir_entity *, 0);
	hook_new_type(res);
	return res;
}

ir_type *new_type_struct(ident *name)
{
	return new_d_type_struct (name, NULL);
}

void free_struct_entities(ir_type *strct)
{
	assert(strct->type_op == type_struct);
	/* we must iterate backward here */
	for (size_t i = get_struct_n_members(strct); i > 0;)
		free_entity(get_struct_member(strct, --i));
}

void free_struct_attrs(ir_type *strct)
{
	assert(strct->type_op == type_struct);
	DEL_ARR_F(strct->attr.sa.members);
}

ident *get_struct_ident(const ir_type *strct)
{
	assert(strct->type_op == type_struct);
	return strct->name;
}

const char *get_struct_name(const ir_type *strct)
{
	ident *id = get_struct_ident(strct);
	if (id == NULL)
		return NULL;
	return get_id_str(id);
}

size_t get_struct_n_members(const ir_type *strct)
{
	assert(strct->type_op == type_struct);
	return ARR_LEN(strct->attr.sa.members);
}

static void add_struct_member(ir_type *strct, ir_entity *member)
{
	assert(strct->type_op == type_struct);
	assert(get_type_tpop(get_entity_type(member)) != type_method);
	assert(strct != get_entity_type(member));
	ARR_APP1(ir_entity *, strct->attr.sa.members, member);
}

ir_entity *get_struct_member(const ir_type *strct, size_t pos)
{
	assert(strct->type_op == type_struct);
	assert(pos < get_struct_n_members(strct));
	return strct->attr.sa.members[pos];
}

size_t get_struct_member_index(const ir_type *strct, ir_entity *mem)
{
	assert(strct->type_op == type_struct);
	for (size_t i = 0, n = get_struct_n_members(strct); i < n; ++i) {
		if (get_struct_member(strct, i) == mem)
			return i;
	}
	return (size_t)-1;
}

static void remove_struct_member(ir_type *strct, ir_entity *member)
{
	assert(strct->type_op == type_struct);
	for (size_t i = 0; i < ARR_LEN(strct->attr.sa.members); ++i) {
		if (strct->attr.sa.members[i] == member) {
			for (; i < ARR_LEN(strct->attr.sa.members) - 1; ++i)
				strct->attr.sa.members[i] = strct->attr.sa.members[i+1];
			ARR_SETLEN(ir_entity*, strct->attr.sa.members, ARR_LEN(strct->attr.sa.members) - 1);
			break;
		}
	}
}

int (is_Struct_type)(const ir_type *strct)
{
	return _is_struct_type(strct);
}

void set_struct_mode(ir_type *tp, ir_mode *mode)
{
	/* for classes and structs we allow to set a mode if the layout is fixed AND the size matches */
	assert(get_type_state(tp) == layout_fixed &&
	       tp->size == get_mode_size_bytes(mode) && "mode don't match struct layout");
	tp->mode = mode;
}

void set_struct_size(ir_type *tp, unsigned size)
{
	tp->size = size;
}

ir_type *new_d_type_method(size_t n_param, size_t n_res, type_dbg_info *db)
{
	assert((get_mode_size_bits(mode_P_code) % 8 == 0) && "unorthodox modes not implemented");
	ir_type *res = new_type(type_method, mode_P_code, db);
	res->flags               |= tf_layout_fixed;
	res->size                 = get_mode_size_bytes(mode_P_code);
	res->attr.ma.n_params     = n_param;
	res->attr.ma.params       = XMALLOCNZ(tp_ent_pair, n_param);
	res->attr.ma.n_res        = n_res;
	res->attr.ma.res_type     = XMALLOCNZ(tp_ent_pair, n_res);
	res->attr.ma.variadicity  = variadicity_non_variadic;
	res->attr.ma.properties   = mtp_no_property;
	hook_new_type(res);
	return res;
}

ir_type *new_type_method(size_t n_param, size_t n_res)
{
	return new_d_type_method(n_param, n_res, NULL);
}

ir_type *clone_type_method(ir_type *tp)
{
	assert(is_Method_type(tp));
	ir_mode       *mode     = tp->mode;
	size_t         n_params = tp->attr.ma.n_params;
	size_t         n_res    = tp->attr.ma.n_res;
	type_dbg_info *db       = tp->dbi;
	ir_type       *res      = new_type(type_method, mode, db);

	res->flags                    = tp->flags;
	res->higher_type              = tp->higher_type;
	res->size                     = tp->size;
	res->attr.ma.n_params         = n_params;
	res->attr.ma.params           = XMALLOCN(tp_ent_pair, n_params);
	memcpy(res->attr.ma.params, tp->attr.ma.params, n_params * sizeof(res->attr.ma.params[0]));
	res->attr.ma.n_res            = n_res;
	res->attr.ma.res_type         = XMALLOCN(tp_ent_pair, n_res);
	memcpy(res->attr.ma.res_type, tp->attr.ma.res_type, n_res * sizeof(res->attr.ma.res_type[0]));
	res->attr.ma.variadicity      = tp->attr.ma.variadicity;
	res->attr.ma.properties       = tp->attr.ma.properties;
	res->attr.ma.irg_calling_conv = tp->attr.ma.irg_calling_conv;
	hook_new_type(res);
	return res;
}

void free_method_entities(ir_type *method)
{
	(void) method;
	assert(method->type_op == type_method);
}

void free_method_attrs(ir_type *method)
{
	assert(method->type_op == type_method);
	free(method->attr.ma.params);
	free(method->attr.ma.res_type);
}

size_t (get_method_n_params)(const ir_type *method)
{
	return _get_method_n_params(method);
}

ir_type *get_method_param_type(const ir_type *method, size_t pos)
{
	assert(method->type_op == type_method);
	assert(pos < get_method_n_params(method));
	ir_type *res = method->attr.ma.params[pos].tp;
	return res;
}

void set_method_param_type(ir_type *method, size_t pos, ir_type *tp)
{
	assert(method->type_op == type_method);
	assert(pos < get_method_n_params(method));
	method->attr.ma.params[pos].tp = tp;
}

size_t (get_method_n_ress)(const ir_type *method)
{
	return _get_method_n_ress(method);
}

ir_type *get_method_res_type(const ir_type *method, size_t pos)
{
	assert(method->type_op == type_method);
	assert(pos < get_method_n_ress(method));
	ir_type *res = method->attr.ma.res_type[pos].tp;
	return res;
}

void set_method_res_type(ir_type *method, size_t pos, ir_type *tp)
{
	assert(method->type_op == type_method);
	assert(pos < get_method_n_ress(method));
	method->attr.ma.res_type[pos].tp = tp;
}

const char *get_variadicity_name(ir_variadicity vari)
{
#define X(a)    case a: return #a
	switch (vari) {
	X(variadicity_non_variadic);
	X(variadicity_variadic);
	default:
		return "BAD VALUE";
	}
#undef X
}

ir_variadicity get_method_variadicity(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.variadicity;
}

void set_method_variadicity(ir_type *method, ir_variadicity vari)
{
	assert(method->type_op == type_method);
	method->attr.ma.variadicity = vari;
}

mtp_additional_properties (get_method_additional_properties)(const ir_type *method)
{
	return _get_method_additional_properties(method);
}

void (set_method_additional_properties)(ir_type *method, mtp_additional_properties mask)
{
	_set_method_additional_properties(method, mask);
}

void (add_method_additional_properties)(ir_type *method,
                                        mtp_additional_properties flag)
{
	_add_method_additional_properties(method, flag);
}

unsigned (get_method_calling_convention)(const ir_type *method)
{
	return _get_method_calling_convention(method);
}

void (set_method_calling_convention)(ir_type *method, unsigned cc_mask)
{
	_set_method_calling_convention(method, cc_mask);
}

unsigned get_method_n_regparams(ir_type *method)
{
	unsigned cc = get_method_calling_convention(method);
	assert(IS_FASTCALL(cc));

	return cc & ~cc_bits;
}

void set_method_n_regparams(ir_type *method, unsigned n_regs)
{
	unsigned cc = get_method_calling_convention(method);
	assert(IS_FASTCALL(cc));

	set_method_calling_convention(method, (cc & cc_bits) | (n_regs & ~cc_bits));
}

int (is_Method_type)(const ir_type *method)
{
	return _is_method_type(method);
}


ir_type *new_d_type_union(ident *name, type_dbg_info *db)
{
	ir_type *res = new_type(type_union, NULL, db);
	res->name = name;

	res->attr.ua.members = NEW_ARR_F(ir_entity *, 0);
	hook_new_type(res);
	return res;
}

ir_type *new_type_union(ident *name)
{
	return new_d_type_union(name, NULL);
}

void free_union_entities(ir_type *uni)
{
	assert(uni->type_op == type_union);
	/* we must iterate backward here */
	for (size_t i = get_union_n_members(uni); i > 0;)
		free_entity(get_union_member(uni, --i));
}

void free_union_attrs(ir_type *uni)
{
	assert(uni->type_op == type_union);
	DEL_ARR_F(uni->attr.ua.members);
}

ident *get_union_ident(const ir_type *uni)
{
	assert(uni->type_op == type_union);
	return uni->name;
}

const char *get_union_name(const ir_type *uni)
{
	ident *id = get_union_ident(uni);
	if (id == NULL)
		return NULL;
	return get_id_str(id);
}

size_t get_union_n_members(const ir_type *uni)
{
	assert(uni->type_op == type_union);
	return ARR_LEN(uni->attr.ua.members);
}

static void add_union_member(ir_type *uni, ir_entity *member)
{
	assert(uni->type_op == type_union);
	assert(uni != get_entity_type(member));
	ARR_APP1(ir_entity *, uni->attr.ua.members, member);
}

ir_entity *get_union_member(const ir_type *uni, size_t pos)
{
	assert(uni->type_op == type_union);
	assert(pos < get_union_n_members(uni));
	return uni->attr.ua.members[pos];
}

size_t get_union_member_index(const ir_type *uni, ir_entity *mem)
{
	assert(uni->type_op == type_union);
	for (size_t i = 0, n = get_union_n_members(uni); i < n; ++i) {
		if (get_union_member(uni, i) == mem)
			return i;
	}
	return (size_t)-1;
}

static void remove_union_member(ir_type *uni, ir_entity *member)
{
	assert(uni->type_op == type_union);
	for (size_t i = 0; i < ARR_LEN(uni->attr.ua.members); ++i) {
		if (uni->attr.ua.members[i] == member) {
			for (; i < ARR_LEN(uni->attr.ua.members) - 1; i++)
				uni->attr.ua.members[i] = uni->attr.ua.members[i+1];
			ARR_SETLEN(ir_entity*, uni->attr.ua.members, ARR_LEN(uni->attr.ua.members) - 1);
			break;
		}
	}
}

int (is_Union_type)(const ir_type *uni)
{
	return _is_union_type(uni);
}

void set_union_size(ir_type *tp, unsigned size)
{
	tp->size = size;
}


ir_type *new_type_segment(ident *const name, type_flags const flags)
{
	ir_type *const seg = new_type_class(name);
	seg->flags |= tf_segment | flags;
	set_class_final(seg, true);
	return seg;
}


ir_type *new_d_type_array(size_t n_dimensions, ir_type *element_type,
                          type_dbg_info *db)
{
	assert(!is_Method_type(element_type));

	ir_type *res = new_type(type_array, NULL, db);
	res->attr.aa.n_dimensions = n_dimensions;
	res->attr.aa.lower_bound  = XMALLOCNZ(ir_node*, n_dimensions);
	res->attr.aa.upper_bound  = XMALLOCNZ(ir_node*, n_dimensions);
	res->attr.aa.order        = XMALLOCNZ(size_t,   n_dimensions);

	ir_graph *irg = get_const_code_irg();
	ir_node  *unk = new_r_Unknown(irg, mode_Iu);
	for (size_t i = 0; i < n_dimensions; i++) {
		res->attr.aa.lower_bound[i] =
		res->attr.aa.upper_bound[i] = unk;
		res->attr.aa.order[i]       = i;
	}

	ident *const id = new_id_from_chars("elem_ent", 8);
	res->attr.aa.element_type = element_type;
	res->attr.aa.element_ent  = new_entity(res, id, element_type);

	hook_new_type(res);
	return res;
}

ir_type *new_type_array(size_t n_dimensions, ir_type *element_type)
{
	return new_d_type_array(n_dimensions, element_type, NULL);
}

void free_array_automatic_entities(ir_type *array)
{
	assert(array->type_op == type_array);
	free_entity(get_array_element_entity(array));
}

void free_array_entities(ir_type *array)
{
	(void) array;
	assert(array->type_op == type_array);
}

void free_array_attrs(ir_type *array)
{
	assert(array->type_op == type_array);
	free(array->attr.aa.lower_bound);
	free(array->attr.aa.upper_bound);
	free(array->attr.aa.order);
}

size_t get_array_n_dimensions(const ir_type *array)
{
	assert(array->type_op == type_array);
	return array->attr.aa.n_dimensions;
}

void set_array_bounds(ir_type *array, size_t dimension, ir_node *lower_bound,
                      ir_node *upper_bound)
{
	assert(array->type_op == type_array);
	assert(lower_bound && "lower_bound node may not be NULL.");
	assert(upper_bound && "upper_bound node may not be NULL.");
	assert(dimension < array->attr.aa.n_dimensions);
	array->attr.aa.lower_bound[dimension] = lower_bound;
	array->attr.aa.upper_bound[dimension] = upper_bound;
}

void set_array_bounds_int(ir_type *array, size_t dimension, int lower_bound,
                          int upper_bound)
{
	ir_graph *irg = get_const_code_irg();
	set_array_bounds(array, dimension,
	          new_r_Const_long(irg, mode_Iu, lower_bound),
	          new_r_Const_long(irg, mode_Iu, upper_bound));
}

void set_array_lower_bound(ir_type *array, size_t dimension,
                           ir_node *lower_bound)
{
	assert(array->type_op == type_array);
	assert(lower_bound != NULL);
	array->attr.aa.lower_bound[dimension] = lower_bound;
}

void set_array_lower_bound_int(ir_type *array, size_t dimension, int lower_bound)
{
	ir_graph *irg = get_const_code_irg();
	set_array_lower_bound(array, dimension,
	     new_r_Const_long(irg, mode_Iu, lower_bound));
}

void set_array_upper_bound(ir_type *array, size_t dimension, ir_node *upper_bound)
{
  assert(array->type_op == type_array);
  assert(upper_bound != NULL);
  array->attr.aa.upper_bound[dimension] = upper_bound;
}

void set_array_upper_bound_int(ir_type *array, size_t dimension, int upper_bound)
{
	ir_graph *irg = get_const_code_irg();
	set_array_upper_bound(array, dimension,
	                      new_r_Const_long(irg, mode_Iu, upper_bound));
}

int has_array_lower_bound(const ir_type *array, size_t dimension)
{
	assert(array->type_op == type_array);
	return !is_Unknown(array->attr.aa.lower_bound[dimension]);
}

ir_node *get_array_lower_bound(const ir_type *array, size_t dimension)
{
	assert(array->type_op == type_array);
	return array->attr.aa.lower_bound[dimension];
}

long get_array_lower_bound_int(const ir_type *array, size_t dimension)
{
	ir_node *node;
	assert(array->type_op == type_array);
	node = array->attr.aa.lower_bound[dimension];
	return get_tarval_long(get_Const_tarval(node));
}

int has_array_upper_bound(const ir_type *array, size_t dimension)
{
	assert(array->type_op == type_array);
	return !is_Unknown(array->attr.aa.upper_bound[dimension]);
}

ir_node *get_array_upper_bound(const ir_type *array, size_t dimension)
{
	assert(array->type_op == type_array);
	return array->attr.aa.upper_bound[dimension];
}

long get_array_upper_bound_int(const ir_type *array, size_t dimension)
{
	ir_node *node;
	assert(array->type_op == type_array);
	node = array->attr.aa.upper_bound[dimension];
	return get_tarval_long(get_Const_tarval(node));
}

void set_array_order(ir_type *array, size_t dimension, size_t order)
{
	assert(array->type_op == type_array);
	array->attr.aa.order[dimension] = order;
}

size_t get_array_order(const ir_type *array, size_t dimension)
{
	assert(array->type_op == type_array);
	return array->attr.aa.order[dimension];
}

size_t find_array_dimension(const ir_type *array, size_t order)
{
	assert(array->type_op == type_array);
	for (size_t dim = 0; dim < array->attr.aa.n_dimensions; ++dim) {
		if (array->attr.aa.order[dim] == order)
			return dim;
	}
	return (size_t)-1;
}

void set_array_element_type(ir_type *array, ir_type *tp)
{
	assert(array->type_op == type_array);
	assert(!is_Method_type(tp));
	array->attr.aa.element_type = tp;
}

ir_type *get_array_element_type(const ir_type *array)
{
	assert(array->type_op == type_array);
	return array->attr.aa.element_type;
}

void set_array_element_entity(ir_type *array, ir_entity *ent)
{
	assert(array->type_op == type_array);
	assert((get_entity_type(ent)->type_op != type_method));
	array->attr.aa.element_ent = ent;
	array->attr.aa.element_type = get_entity_type(ent);
}

ir_entity *get_array_element_entity(const ir_type *array)
{
	assert(array->type_op == type_array);
	return array->attr.aa.element_ent;
}

int is_array_variable_size(const ir_type *array)
{
	assert(array->type_op == type_array);
	return (array->flags & tf_variable_size) != 0;
}

void set_array_variable_size(ir_type *array, int flag)
{
	assert(array->type_op == type_array);
	array->flags = (array->flags & ~tf_variable_size)
	               | (flag != 0 ? tf_variable_size : 0);
}

int (is_Array_type)(const ir_type *array)
{
	return _is_array_type(array);
}

void set_array_size(ir_type *tp, unsigned size)
{
	/* FIXME: Here we should make some checks with the element type size */
	tp->size = size;
}


ir_type *new_d_type_pointer(ir_type *points_to, type_dbg_info *db)
{
	ir_mode *mode;
	if (is_Method_type(points_to) || is_code_type(points_to)) {
		mode = mode_P_code;
	} else {
		mode = mode_P_data;
	}

	ir_type *res = new_type(type_pointer, mode, db);
	res->attr.pa.points_to = points_to;
	assert((get_mode_size_bits(res->mode) % 8 == 0) && "unorthodox modes not implemented");
	res->size = get_mode_size_bytes(res->mode);
	res->flags |= tf_layout_fixed;
	hook_new_type(res);
	return res;
}

ir_type *new_type_pointer(ir_type *points_to)
{
	return new_d_type_pointer(points_to, NULL);
}

void free_pointer_entities(ir_type *pointer)
{
	(void) pointer;
	assert(pointer->type_op == type_pointer);
}

void free_pointer_attrs(ir_type *pointer)
{
	(void) pointer;
	assert(pointer->type_op == type_pointer);
}

void set_pointer_points_to_type(ir_type *pointer, ir_type *tp)
{
	assert(pointer->type_op == type_pointer);
	pointer->attr.pa.points_to = tp;
}

ir_type *get_pointer_points_to_type(const ir_type *pointer)
{
	assert(pointer->type_op == type_pointer);
	return pointer->attr.pa.points_to;
}

int (is_Pointer_type)(const ir_type *pointer)
{
	return _is_pointer_type(pointer);
}

void set_pointer_mode(ir_type *tp, ir_mode *mode)
{
	assert(mode_is_reference(mode));
	tp->size = get_mode_size_bytes(mode);
	tp->mode = mode;
}

ir_type *find_pointer_type_to_type(ir_type *tp)
{
	for (size_t i = 0, n = get_irp_n_types(); i < n; ++i) {
		ir_type *found = get_irp_type(i);
		if (is_Pointer_type(found) && get_pointer_points_to_type(found) == tp)
			return (found);
	}
	return get_unknown_type();
}


ir_type *new_d_type_primitive(ir_mode *mode, type_dbg_info *db)
{
	ir_type *res = new_type(type_primitive, mode, db);
	res->size  = get_mode_size_bytes(mode);
	res->flags |= tf_layout_fixed;
	hook_new_type(res);
	return res;
}

ir_type *new_type_primitive(ir_mode *mode)
{
	return new_d_type_primitive(mode, NULL);
}

int (is_Primitive_type)(const ir_type *primitive)
{
	return _is_primitive_type(primitive);
}

void set_primitive_mode(ir_type *tp, ir_mode *mode)
{
	/* Modes of primitives must be data */
	assert(mode_is_data(mode));

	/* For primitive size depends on the mode. */
	tp->size = get_mode_size_bytes(mode);
	tp->mode = mode;
}


int (is_atomic_type)(const ir_type *tp)
{
	return _is_atomic_type(tp);
}

size_t get_compound_n_members(const ir_type *tp)
{
	const tp_op *op = get_type_tpop(tp);
	return op->ops.get_n_members(tp);
}

ir_entity *get_compound_member(const ir_type *tp, size_t pos)
{
	const tp_op *op = get_type_tpop(tp);
	return op->ops.get_member(tp, pos);
}

size_t get_compound_member_index(const ir_type *tp, ir_entity *member)
{
	const tp_op *op = get_type_tpop(tp);
	return op->ops.get_member_index(tp, member);
}

void set_compound_variable_size(ir_type *tp, int variable_size_flag)
{
	assert(is_compound_type(tp));
	tp->flags = (tp->flags & ~tf_variable_size)
	            | (variable_size_flag != 0 ? tf_variable_size : 0);
}

int is_compound_variable_size(const ir_type *tp)
{
	assert(is_compound_type(tp));
	return (tp->flags & tf_variable_size) != 0;
}

int is_compound_type(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->type_op->flags & TP_OP_FLAG_COMPOUND;
}

ident *get_compound_ident(const ir_type *tp)
{
	assert(is_compound_type(tp));
	return tp->name;
}

const char *get_compound_name(const ir_type *tp)
{
	if (get_compound_ident(tp) == NULL)
		return NULL;
	return get_id_str(get_compound_ident(tp));
}

void remove_compound_member(ir_type *compound, ir_entity *entity)
{
	switch (get_type_tpop_code(compound)) {
	case tpo_class:  remove_class_member(compound, entity);  break;
	case tpo_struct: remove_struct_member(compound, entity); break;
	case tpo_union:  remove_union_member(compound, entity);  break;
	default:
		panic("argument for remove_compound_member not a compound type");
	}
}

void add_compound_member(ir_type *compound, ir_entity *entity)
{
	switch (get_type_tpop_code(compound)) {
	case tpo_class:  add_class_member(compound, entity);  break;
	case tpo_struct: add_struct_member(compound, entity); break;
	case tpo_union:  add_union_member(compound, entity);  break;
	default:
		panic("argument for add_compound_member not a compound type");
	}
}

int is_code_type(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->type_op == tpop_code;
}

int is_unknown_type(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->type_op == tpop_unknown;
}

int is_frame_type(const ir_type *tp)
{
	return tp->flags & tf_frame_type;
}

ir_type *new_type_frame(void)
{
	ir_type *res = new_type_class(new_id_from_str("<frame_type>"));
	res->flags |= tf_frame_type;
	set_class_final(res, 1);

	return res;
}

ir_type *clone_frame_type(ir_type *type)
{
	assert(is_frame_type(type));
	/* the entity link resource should be allocated if this function is called */
	assert(irp_resources_reserved(irp) & IRP_RESOURCE_ENTITY_LINK);

	ir_type *res = new_type_frame();
	for (size_t i = 0, n = get_class_n_members(type); i < n; ++i) {
		ir_entity *ent  = get_class_member(type, i);
		ir_entity *nent = copy_entity_own(ent, res);
		set_entity_link(ent, nent);
		set_entity_link(nent, ent);
	}
	return res;
}

void set_default_size(ir_type *tp, unsigned size)
{
	tp->size = size;
}

void default_layout_compound_type(ir_type *type)
{
	int      size      = 0;
	unsigned align_all = 1;
	bool     var_size  = is_compound_variable_size(type);
	for (size_t i = 0, n = get_compound_n_members(type); i < n; ++i) {
		ir_entity *entity      = get_compound_member(type, i);
		ir_type   *entity_type = get_entity_type(entity);
		if (is_Method_type(entity_type))
			continue;

		if (get_entity_bitfield_size(entity) > 0) {
			panic("default_layout_compound_type() cannot handle bitfield members (in %+F)", type);
		}

		unsigned entity_size;
		if (i+1 < n || !var_size) {
			assert(get_type_state(entity_type) == layout_fixed);
			entity_size = get_type_size_bytes(entity_type);
		} else {
			entity_size = 0;
		}

		unsigned align     = get_type_alignment_bytes(entity_type);
		unsigned misalign  = (align ? size % align : 0);
		size     += (misalign ? align - misalign : 0);
		align_all = align > align_all ? align : align_all;

		set_entity_offset(entity, size);
		if (!is_Union_type(type)) {
			size += entity_size;
		}
	}
	if (align_all > 0 && size % align_all) {
		size += align_all - (size % align_all);
	}
	if (align_all > get_type_alignment_bytes(type)) {
		set_type_alignment_bytes(type, align_all);
	}
	set_type_size_bytes(type, size);
	set_type_state(type, layout_fixed);
}

ir_entity *frame_alloc_area(ir_type *frame_type, int size, unsigned alignment,
                            int at_start)
{
	static unsigned area_cnt = 0;

	assert(is_frame_type(frame_type));
	assert(get_type_state(frame_type) == layout_fixed);
	assert(get_type_alignment_bytes(frame_type) > 0);
	set_type_state(frame_type, layout_undefined);

	if (irp->byte_type == NULL)
		irp->byte_type = new_type_primitive(mode_Bu);

	char buf[32];
	snprintf(buf, sizeof(buf), "area%u", area_cnt++);
	ident *name = new_id_from_str(buf);

	ir_type *tp = new_type_array(1, irp->byte_type);
	set_array_bounds_int(tp, 0, 0, size);
	set_type_alignment_bytes(tp, alignment);
	set_type_size_bytes(tp, size);

	unsigned frame_size  = get_type_size_bytes(frame_type);
	unsigned frame_align = get_type_alignment_bytes(frame_type);
	int      offset;
	if (at_start) {
		unsigned delta = (size + frame_align - 1) & ~(frame_align - 1);
		/* fix all offsets so far */
		for (size_t i = 0, n = get_class_n_members(frame_type); i < n; ++i) {
			ir_entity *ent = get_class_member(frame_type, i);

			set_entity_offset(ent, get_entity_offset(ent) + delta);
		}
		/* calculate offset and new type size */
		offset = 0;
		frame_size += delta;
	} else {
		/* calculate offset and new type size */
		offset = (frame_size + alignment - 1) & ~(alignment - 1);
		frame_size = offset + size;
	}

	ir_entity *area = new_entity(frame_type, name, tp);
	set_entity_offset(area, offset);
	set_type_size_bytes(frame_type, frame_size);
	if (alignment > frame_align) {
		set_type_alignment_bytes(frame_type, alignment);
	}

	/* mark this entity as compiler generated */
	set_entity_compiler_generated(area, 1);

	set_type_state(frame_type, layout_fixed);
	return area;
}

void ir_print_type(char *buffer, size_t buffer_size, const ir_type *type)
{
	type_dbg_info *tdbgi = get_type_dbg_info(type);
	if (tdbgi != NULL) {
		ir_retrieve_type_dbg_info(buffer, buffer_size, tdbgi);
		return;
	}

	/* we have to construct some name... */
	switch (get_type_tpop_code(type)) {
	case tpo_uninitialized:
		break;
	case tpo_code:
		snprintf(buffer, buffer_size, "code");
		return;

	case tpo_class: {
		ident *id = get_class_ident(type);
		snprintf(buffer, buffer_size, "class '%s'", get_id_str(id));
		return;
	}

	case tpo_struct: {
		ident *id = get_struct_ident(type);
		snprintf(buffer, buffer_size, "struct '%s'", get_id_str(id));
		return;
	}

	case tpo_union: {
		ident *id = get_union_ident(type);
		snprintf(buffer, buffer_size, "union '%s'", get_id_str(id));
		return;
	}

	case tpo_unknown:
		snprintf(buffer, buffer_size, "unknown type");
		return;

	case tpo_pointer: {
		int p = snprintf(buffer, buffer_size, "pointer to ");
		buffer      += p;
		buffer_size -= p;
		ir_print_type(buffer, buffer_size, get_pointer_points_to_type(type));
		return;
	}

	case tpo_array: {
		int p = snprintf(buffer, buffer_size, "array of ");
		buffer      += p;
		buffer_size -= p;
		ir_print_type(buffer, buffer_size, get_array_element_type(type));
		return;
	}

	case tpo_primitive: {
		ident *id = get_mode_ident(get_type_mode(type));
		snprintf(buffer, buffer_size, "%s", get_id_str(id));
		return;
	}

	case tpo_method:
		/* TODO: we should print argument and return types here... */
		snprintf(buffer, buffer_size, "method type");
		return;
	}
	snprintf(buffer, buffer_size, "invalid type");
}
