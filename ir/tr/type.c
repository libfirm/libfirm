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

#include "irnode_t.h"
#include "type_t.h"

#include "xmalloc.h"
#include "irprog_t.h"
#include "ircons.h"
#include "tv_t.h"
#include "irhooks.h"
#include "util.h"
#include "entity_t.h"
#include "panic.h"
#include "dbginfo.h"
#include "irprog_t.h"
#include "bitfiddle.h"

#include "array.h"

static ir_type *new_type(tp_opcode opcode, size_t attr_size, ir_mode *mode);
static void free_compound_entities(ir_type *type);

const char *get_type_opcode_name(tp_opcode const opcode)
{
	switch (opcode) {
	case tpo_array:         return "array";
	case tpo_class:         return "class";
	case tpo_code:          return "code";
	case tpo_method:        return "method";
	case tpo_pointer:       return "pointer";
	case tpo_primitive:     return "primitive";
	case tpo_struct:        return "struct";
	case tpo_uninitialized: return "uninitialized";
	case tpo_union:         return "union";
	case tpo_unknown:       return "unknown";
	}
	panic("Invalid type opcode");
}

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
	irp->code_type = new_type(tpo_code, 0, mode_ANY);
	set_type_state(irp->code_type, layout_fixed);

	irp->unknown_type = new_type(tpo_unknown, 0, mode_ANY);
	set_type_state (irp->unknown_type, layout_fixed);

	irp->dummy_owner = new_type_struct(new_id_from_str("$dummy_owner$"));
}

void ir_finish_type(ir_prog *irp)
{
	/** nothing todo. (The code, unknown types are in the global type list
	 * and freed there */
	(void)irp;
}

ir_visited_t firm_type_visited;

void set_master_type_visited(ir_visited_t val)
{
	firm_type_visited = val;
}

ir_visited_t (get_master_type_visited)(void)
{
	return get_master_type_visited_();
}

void inc_master_type_visited(void)
{
	++firm_type_visited;
}

/**
 *   Creates a new type representation:
 *   @return A new type of the given type.  The remaining private attributes are
 *           not initialized.  The type is in state layout_undefined.
 */
static ir_type *new_type(tp_opcode const opcode, size_t attr_size,
                         ir_mode *const mode)
{
	size_t   const node_size = offsetof(ir_type, attr) +  attr_size;
	ir_type *const res       = (ir_type*)xmalloc(node_size);
	memset(res, 0, node_size);

	res->kind       = k_type;
	res->opcode     = opcode;
	res->mode       = mode;
	res->visibility = ir_visibility_external;
	res->flags      = tf_none;
	res->size       = 0;
	res->align      = 0;
	res->visit      = 0;
	res->link       = NULL;
	res->nr         = get_irp_new_node_nr();

	add_irp_type(res);   /* Remember the new type global. */

	return res;
}

void free_type_entities(ir_type *const type)
{
	if (is_compound_type(type))
		free_compound_entities(type);
}

static void free_type_attrs(ir_type *const type)
{
	switch (get_type_opcode(type)) {
	case tpo_class:
		free_class_attrs(type);
		return;
	case tpo_union:
	case tpo_struct:
		free_compound_attrs(type);
		return;
	case tpo_method:
		free_method_attrs(type);
		return;
	case tpo_code:
	case tpo_primitive:
	case tpo_pointer:
	case tpo_array:
	case tpo_unknown:
	case tpo_uninitialized:
		return;
	}
	panic("Invalid type");
}

void free_type(ir_type *tp)
{
	free_type_entities(tp);
	/* Remove from list of all types */
	remove_irp_type(tp);
	/* Free the attributes of the type. */
	free_type_attrs(tp);
	/* And now the type itself... */
#ifdef DEBUG_libfirm
	tp->kind = k_BAD;
#endif
	free(tp);
}

void *(get_type_link)(const ir_type *tp)
{
	return get_type_link_(tp);
}

void (set_type_link)(ir_type *tp, void *l)
{
	set_type_link_(tp, l);
}

static inline bool is_type(const void *thing)
{
	return get_kind(thing) == k_type;
}

tp_opcode (get_type_opcode)(ir_type const *const type)
{
	return get_type_opcode_(type);
}

ir_mode *(get_type_mode)(const ir_type *tp)
{
	return get_type_mode_(tp);
}

long get_type_nr(const ir_type *tp)
{
	assert(is_type(tp));
	return tp->nr;
}

unsigned (get_type_size_bytes)(const ir_type *tp)
{
	return get_type_size_bytes_(tp);
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
	tp->size = size;
}

unsigned (get_type_alignment_bytes)(const ir_type *type)
{
	return get_type_alignment_bytes_(type);
}

void set_type_alignment_bytes(ir_type *type, unsigned align)
{
	assert(is_type(type));
	assert(align > 0);
	type->align = align;
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
	return get_type_state_(tp);
}

void set_type_state(ir_type *tp, ir_type_state state)
{
	assert(is_type(tp));

	tp_opcode opcode = get_type_opcode(tp);
	if (opcode == tpo_pointer || opcode == tpo_primitive
	 || opcode == tpo_method)
		return;

#ifndef NDEBUG
	/* Just a correctness check: */
	if (state == layout_fixed && is_compound_type(tp)
	 && !(tp->flags & tf_segment)) {
		for (size_t i = 0, n_mem = get_compound_n_members(tp);
			 i < n_mem; i++) {
			ir_entity *entity = get_compound_member(tp, i);
			if (is_Method_type(get_entity_type(entity)))
				continue;
			assert(get_entity_offset(entity) > -1);
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
	return get_type_visited_(tp);
}

void (set_type_visited)(ir_type *tp, ir_visited_t num)
{
	set_type_visited_(tp, num);
}

void (mark_type_visited)(ir_type *tp)
{
	mark_type_visited_(tp);
}

int (type_visited)(const ir_type *tp)
{
	return type_visited_(tp);
}

type_dbg_info *(get_type_dbg_info)(const ir_type *tp)
{
	return get_type_dbg_info_(tp);
}

void (set_type_dbg_info)(ir_type *tp, type_dbg_info *db)
{
	set_type_dbg_info_(tp, db);
}

static void compound_init(ir_type *const type, ident *const name)
{
	type->flags          |= tf_compound;
	type->name            = name;
	type->attr.ca.members = NEW_ARR_F(ir_entity*, 0);
}

void free_compound_attrs(ir_type *type)
{
	DEL_ARR_F(type->attr.ca.members);
}

static void free_compound_entities(ir_type *type)
{
	for (size_t i = get_compound_n_members(type); i-- > 0; )
		free_entity(get_compound_member(type, i));
}

ir_type *new_type_class(ident *name)
{
	ir_type *res = new_type(tpo_class, sizeof(cls_attr), NULL);
	compound_init(res, name);
	res->attr.cla.subtypes   = NEW_ARR_F(ir_type*, 0);
	res->attr.cla.supertypes = NEW_ARR_F(ir_type*, 0);
	hook_new_type(res);
	return res;
}

void free_class_attrs(ir_type *clss)
{
	assert(is_Class_type(clss));
	free_compound_attrs(clss);
	DEL_ARR_F(clss->attr.cla.subtypes);
	DEL_ARR_F(clss->attr.cla.supertypes);
}

ident *get_class_ident(const ir_type *clss)
{
	assert(is_Class_type(clss));
	return clss->name;
}

const char *get_class_name(const ir_type *clss)
{
	if (get_class_ident(clss) == NULL)
		return NULL;
	return get_id_str(get_class_ident(clss));
}

size_t get_class_n_members(const ir_type *clss)
{
	assert(is_Class_type(clss));
	return get_compound_n_members(clss);
}

ir_entity *get_class_member(ir_type const *const clss, size_t const pos)
{
	assert(is_Class_type(clss));
	return get_compound_member(clss, pos);
}

size_t get_class_member_index(ir_type const *clss, ir_entity const *const mem)
{
	assert(is_Class_type(clss));
	return get_compound_member_index(clss, mem);
}

void add_class_subtype(ir_type *clss, ir_type *subtype)
{
	assert(is_Class_type(clss));
	ARR_APP1(ir_type *, clss->attr.cla.subtypes, subtype);
	for (size_t i = 0, n_supertypes = get_class_n_supertypes(subtype);
	     i < n_supertypes; i++) {
		if (get_class_supertype(subtype, i) == clss)
			/* Class already registered */
			return;
	}
	ARR_APP1(ir_type *, subtype->attr.cla.supertypes, clss);
}

size_t get_class_n_subtypes(const ir_type *clss)
{
	assert(is_Class_type(clss));
	return ARR_LEN(clss->attr.cla.subtypes);
}

ir_type *get_class_subtype(const ir_type *clss, size_t pos)
{
	assert(is_Class_type(clss));
	assert(pos < get_class_n_subtypes(clss));
	return clss->attr.cla.subtypes[pos];
}

size_t get_class_subtype_index(const ir_type *clss, const ir_type *subclass)
{
	assert(is_Class_type(clss) && is_Class_type(subclass));
	for (size_t i = 0, n_subtypes = get_class_n_subtypes(clss);
	     i < n_subtypes; ++i) {
		if (get_class_subtype(clss, i) == subclass)
			return i;
	}
	return (size_t)-1;
}

void set_class_subtype(ir_type *clss, ir_type *subtype, size_t pos)
{
	assert(is_Class_type(clss));
	assert(pos < get_class_n_subtypes(clss));
	clss->attr.cla.subtypes[pos] = subtype;
}

void remove_class_subtype(ir_type *clss, ir_type *subtype)
{
	assert(is_Class_type(clss));
	for (size_t i = 0; i < ARR_LEN(clss->attr.cla.subtypes); ++i) {
		if (clss->attr.cla.subtypes[i] == subtype) {
			for (; i < ARR_LEN(clss->attr.cla.subtypes) - 1; ++i)
				clss->attr.cla.subtypes[i] = clss->attr.cla.subtypes[i+1];
			ARR_SETLEN(ir_type*, clss->attr.cla.subtypes, ARR_LEN(clss->attr.cla.subtypes) - 1);
			break;
		}
	}
}

void add_class_supertype(ir_type *clss, ir_type *supertype)
{
	assert(is_Class_type(clss));
	assert(is_Class_type(supertype));
	ARR_APP1(ir_type *, clss->attr.cla.supertypes, supertype);
	for (size_t i = 0, n = get_class_n_subtypes(supertype); i < n; ++i) {
		if (get_class_subtype(supertype, i) == clss)
			/* Class already registered */
			return;
	}
	ARR_APP1(ir_type *, supertype->attr.cla.subtypes, clss);
}

size_t get_class_n_supertypes(const ir_type *clss)
{
	assert(is_Class_type(clss));
	return ARR_LEN(clss->attr.cla.supertypes);
}

size_t get_class_supertype_index(const ir_type *clss, const ir_type *super_clss)
{
	assert(is_Class_type(clss) && is_Class_type(super_clss));
	for (size_t i = 0, n_supertypes = get_class_n_supertypes(clss);
	     i < n_supertypes; i++) {
		if (get_class_supertype(clss, i) == super_clss)
			return i;
	}
	return (size_t)-1;
}

ir_type *get_class_supertype(const ir_type *clss, size_t pos)
{
	assert(is_Class_type(clss));
	assert(pos < get_class_n_supertypes(clss));
	return clss->attr.cla.supertypes[pos];
}

void set_class_supertype(ir_type *clss, ir_type *supertype, size_t pos)
{
	assert(is_Class_type(clss));
	assert(pos < get_class_n_supertypes(clss));
	clss->attr.cla.supertypes[pos] = supertype;
}

void remove_class_supertype(ir_type *clss, ir_type *supertype)
{
	assert(is_Class_type(clss));
	for (size_t i = 0; i < ARR_LEN(clss->attr.cla.supertypes); ++i) {
		if (clss->attr.cla.supertypes[i] == supertype) {
			for (; i < ARR_LEN(clss->attr.cla.supertypes) - 1; ++i)
				clss->attr.cla.supertypes[i] = clss->attr.cla.supertypes[i+1];
			ARR_SETLEN(ir_type*, clss->attr.cla.supertypes, ARR_LEN(clss->attr.cla.supertypes) - 1);
			break;
		}
	}
}

int (is_Class_type)(const ir_type *clss)
{
	return is_class_type_(clss);
}


ir_type *new_type_struct(ident *name)
{
	ir_type *res = new_type(tpo_struct, sizeof(compound_attr), NULL);
	compound_init(res, name);
	hook_new_type(res);
	return res;
}

ident *get_struct_ident(const ir_type *strct)
{
	assert(is_Struct_type(strct));
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
	assert(is_Struct_type(strct));
	return get_compound_n_members(strct);
}

ir_entity *get_struct_member(const ir_type *strct, size_t pos)
{
	assert(is_Struct_type(strct));
	return get_compound_member(strct, pos);
}

size_t get_struct_member_index(ir_type const *strct, ir_entity const *const mem)
{
	assert(is_Struct_type(strct));
	return get_compound_member_index(strct, mem);
}

int (is_Struct_type)(const ir_type *strct)
{
	return is_struct_type_(strct);
}

ir_type *new_type_method(size_t n_param, size_t n_res)
{
	ir_type *res = new_type(tpo_method, sizeof(mtd_attr), mode_P);
	res->flags             |= tf_layout_fixed;
	res->size               = get_mode_size_bytes(mode_P);
	res->attr.ma.n_params   = n_param;
	res->attr.ma.params     = XMALLOCNZ(ir_type*, n_param);
	res->attr.ma.n_res      = n_res;
	res->attr.ma.res_type   = XMALLOCNZ(ir_type*, n_res);
	res->attr.ma.variadic   = false;
	res->attr.ma.properties = mtp_no_property;
	set_type_alignment_bytes(res, 1);
	hook_new_type(res);
	return res;
}

ir_type *clone_type_method(ir_type *tp)
{
	assert(is_Method_type(tp));
	ir_mode       *mode     = tp->mode;
	size_t         n_params = tp->attr.ma.n_params;
	size_t         n_res    = tp->attr.ma.n_res;
	type_dbg_info *db       = tp->dbi;
	ir_type       *res      = new_type(tpo_method, sizeof(mtd_attr), mode);
	set_type_dbg_info(res, db);

	res->flags                    = tp->flags;
	res->higher_type              = tp->higher_type;
	res->size                     = tp->size;
	res->attr.ma.n_params         = n_params;
	res->attr.ma.params           = XMALLOCN(ir_type*, n_params);
	MEMCPY(res->attr.ma.params, tp->attr.ma.params, n_params);
	res->attr.ma.n_res            = n_res;
	res->attr.ma.res_type         = XMALLOCN(ir_type*, n_res);
	MEMCPY(res->attr.ma.res_type, tp->attr.ma.res_type, n_res);
	res->attr.ma.variadic         = tp->attr.ma.variadic;
	res->attr.ma.properties       = tp->attr.ma.properties;
	res->attr.ma.irg_calling_conv = tp->attr.ma.irg_calling_conv;
	set_type_alignment_bytes(res, get_type_alignment_bytes(tp));
	hook_new_type(res);
	return res;
}

void free_method_attrs(ir_type *method)
{
	assert(is_Method_type(method));
	free(method->attr.ma.params);
	free(method->attr.ma.res_type);
}

size_t (get_method_n_params)(const ir_type *method)
{
	return get_method_n_params_(method);
}

ir_type *get_method_param_type(const ir_type *method, size_t pos)
{
	assert(is_Method_type(method));
	assert(pos < get_method_n_params(method));
	ir_type *res = method->attr.ma.params[pos];
	return res;
}

void set_method_param_type(ir_type *method, size_t pos, ir_type *tp)
{
	assert(is_Method_type(method));
	assert(pos < get_method_n_params(method));
	method->attr.ma.params[pos] = tp;
}

size_t (get_method_n_ress)(const ir_type *method)
{
	return get_method_n_ress_(method);
}

ir_type *get_method_res_type(const ir_type *method, size_t pos)
{
	assert(is_Method_type(method));
	assert(pos < get_method_n_ress(method));
	ir_type *res = method->attr.ma.res_type[pos];
	return res;
}

void set_method_res_type(ir_type *method, size_t pos, ir_type *tp)
{
	assert(is_Method_type(method));
	assert(pos < get_method_n_ress(method));
	method->attr.ma.res_type[pos] = tp;
}

int is_method_variadic(ir_type const *const method)
{
	assert(is_Method_type(method));
	return method->attr.ma.variadic;
}

void set_method_variadic(ir_type *const method, int const is_variadic)
{
	assert(is_Method_type(method));
	method->attr.ma.variadic = is_variadic;
}

mtp_additional_properties (get_method_additional_properties)(const ir_type *method)
{
	return get_method_additional_properties_(method);
}

void (set_method_additional_properties)(ir_type *method, mtp_additional_properties mask)
{
	set_method_additional_properties_(method, mask);
}

void (add_method_additional_properties)(ir_type *method,
                                        mtp_additional_properties flag)
{
	add_method_additional_properties_(method, flag);
}

unsigned (get_method_calling_convention)(const ir_type *method)
{
	return get_method_calling_convention_(method);
}

void (set_method_calling_convention)(ir_type *method, unsigned cc_mask)
{
	set_method_calling_convention_(method, cc_mask);
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
	return is_method_type_(method);
}

ir_type *new_type_union(ident *name)
{
	ir_type *res = new_type(tpo_union, sizeof(compound_attr), NULL);
	compound_init(res, name);
	hook_new_type(res);
	return res;
}

ident *get_union_ident(const ir_type *uni)
{
	assert(is_Union_type(uni));
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
	assert(is_Union_type(uni));
	return get_compound_n_members(uni);
}

ir_entity *get_union_member(const ir_type *uni, size_t pos)
{
	assert(is_Union_type(uni));
	return get_compound_member(uni, pos);
}

size_t get_union_member_index(ir_type const *uni, ir_entity const *const mem)
{
	assert(is_Union_type(uni));
	return get_compound_member_index(uni, mem);
}

int (is_Union_type)(const ir_type *uni)
{
	return is_union_type_(uni);
}

ir_type *new_type_segment(ident *const name, type_flags const flags)
{
	ir_type *const seg = new_type_class(name);
	seg->flags |= tf_segment | flags;
	return seg;
}

int is_segment_type(const ir_type *type)
{
	return (type->flags & tf_segment) != 0;
}

ir_type *new_type_array(ir_type *element_type)
{
	assert(!is_Method_type(element_type));

	ir_type  *res = new_type(tpo_array, sizeof(arr_attr), NULL);
	res->attr.aa.element_type = element_type;
	res->attr.aa.size         = new_r_Unknown(get_const_code_irg(), mode_Iu);
	set_type_alignment_bytes(res, get_type_alignment_bytes(element_type));

	hook_new_type(res);
	return res;
}

void set_array_size(ir_type *array, ir_node *size)
{
	assert(is_Array_type(array));
	assert(size != NULL);
	array->attr.aa.size = size;
}

void set_array_size_int(ir_type *array, unsigned size)
{
	ir_graph *irg = get_const_code_irg();
	set_array_size(array, new_r_Const_long(irg, mode_Iu, size));
}

int has_array_size(const ir_type *array)
{
	assert(is_Array_type(array));
	return !is_Unknown(array->attr.aa.size);
}

ir_node *get_array_size(const ir_type *array)
{
	assert(is_Array_type(array));
	return array->attr.aa.size;
}

unsigned get_array_size_int(const ir_type *array)
{
	assert(is_Array_type(array));
	ir_node *node = array->attr.aa.size;
	return get_Const_long(node);
}

void set_array_element_type(ir_type *array, ir_type *tp)
{
	assert(is_Array_type(array));
	assert(!is_Method_type(tp));
	array->attr.aa.element_type = tp;
	set_type_alignment_bytes(array, get_type_alignment_bytes(tp));
}

ir_type *get_array_element_type(const ir_type *array)
{
	assert(is_Array_type(array));
	return array->attr.aa.element_type;
}

int is_array_variable_size(const ir_type *array)
{
	assert(is_Array_type(array));
	return (array->flags & tf_variable_size) != 0;
}

void set_array_variable_size(ir_type *array, int flag)
{
	assert(is_Array_type(array));
	array->flags = (array->flags & ~tf_variable_size)
	               | (flag != 0 ? tf_variable_size : 0);
}

int (is_Array_type)(const ir_type *array)
{
	return is_array_type_(array);
}


ir_type *new_type_pointer(ir_type *points_to)
{
	ir_mode *const mode = mode_P;
	ir_type *const res  = new_type(tpo_pointer, sizeof(ptr_attr), mode);
	res->attr.pa.points_to = points_to;
	unsigned size = get_mode_size_bytes(mode);
	res->size = size;
	res->flags |= tf_layout_fixed;
	set_type_alignment_bytes(res, size);
	hook_new_type(res);
	return res;
}

void set_pointer_points_to_type(ir_type *pointer, ir_type *tp)
{
	assert(is_Pointer_type(pointer));
	pointer->attr.pa.points_to = tp;
}

ir_type *get_pointer_points_to_type(const ir_type *pointer)
{
	assert(is_Pointer_type(pointer));
	return pointer->attr.pa.points_to;
}

int (is_Pointer_type)(const ir_type *pointer)
{
	return is_pointer_type_(pointer);
}

void set_pointer_mode(ir_type *tp, ir_mode *mode)
{
	assert(is_Pointer_type(tp));
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


ir_type *new_type_primitive(ir_mode *mode)
{
	unsigned size  = get_mode_size_bytes(mode);
	unsigned align = (size > 0 && size != (unsigned)-1)
	               ? ceil_po2(size) : 1;

	ir_type *res = new_type(tpo_primitive, 0, mode);
	res->size  = size;
	res->flags |= tf_layout_fixed;
	set_type_alignment_bytes(res, align);
	hook_new_type(res);
	return res;
}

int (is_Primitive_type)(const ir_type *primitive)
{
	return is_primitive_type_(primitive);
}

int (is_atomic_type)(const ir_type *tp)
{
	return is_atomic_type_(tp);
}

size_t (get_compound_n_members)(ir_type const *const type)
{
	return get_compound_n_members_(type);
}

ir_entity *(get_compound_member)(ir_type const *const type, size_t const pos)
{
	return get_compound_member_(type, pos);
}

size_t get_compound_member_index(ir_type const *const type,
                                 ir_entity const *const entity)
{
	assert(is_compound_type(type));
	for (size_t i = 0, n = get_compound_n_members(type); i < n; ++i) {
		if (get_compound_member(type, i) == entity)
			return i;
	}
	return INVALID_MEMBER_INDEX;
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
	return (tp->flags & tf_compound) != 0;
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

void remove_compound_member(ir_type *type, ir_entity *member)
{
	assert(is_compound_type(type));
	for (size_t i = 0, n = ARR_LEN(type->attr.ca.members); i < n; ++i) {
		if (get_compound_member(type, i) == member) {
			for (; i < n - 1; ++i)
				type->attr.ca.members[i] = type->attr.ca.members[i+1];
			ARR_SETLEN(ir_entity*, type->attr.ca.members, n-1);
			break;
		}
	}
}

void add_compound_member(ir_type *type, ir_entity *entity)
{
	assert(is_compound_type(type));
	/* try to detect double-add */
	assert(get_entity_type(entity) != type);
	ARR_APP1(ir_entity *, type->attr.ca.members, entity);
}

int is_code_type(ir_type const *const type)
{
	return get_type_opcode(type) == tpo_code;
}

int is_unknown_type(ir_type const *const type)
{
	return get_type_opcode(type) == tpo_unknown;
}

int is_frame_type(ir_type const *const type)
{
	assert(type->kind == k_type);
	return (type->flags & tf_frame_type) != 0;
}

ir_type *new_type_frame(void)
{
	ir_type *res = new_type_class(new_id_from_str("<frame_type>"));
	res->flags |= tf_frame_type;

	return res;
}

ir_type *clone_frame_type(ir_type *type)
{
	assert(is_frame_type(type));
	/* the entity link resource should be allocated if this function is called */
	assert(irp_resources_reserved(irp) & IRP_RESOURCE_ENTITY_LINK);

	ir_type *res = new_type_frame();
	for (size_t i = 0, n = get_compound_n_members(type); i < n; ++i) {
		ir_entity *ent  = get_compound_member(type, i);
		ir_entity *nent = copy_entity_own(ent, res);
		set_entity_link(ent, nent);
		set_entity_link(nent, ent);
	}
	return res;
}

void default_layout_compound_type(ir_type *type)
{
	unsigned size      = 0;
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

		unsigned const align = get_type_alignment_bytes(entity_type);
		align_all = MAX(align, align_all);

		unsigned offset;
		if (is_Union_type(type)) {
			offset = 0;
			size   = MAX(size, entity_size);
		} else {
			if (align != 0) {
				unsigned const misalign = size % align;
				if (misalign != 0)
					size += align - misalign;
			}
			offset = size;
			size  += entity_size;
		}
		set_entity_offset(entity, offset);
	}

	if (align_all > 0 && size % align_all) {
		size += align_all - (size % align_all);
	}
	set_type_alignment_bytes(type, align_all);
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

	ident *const name = new_id_fmt("area%u", area_cnt++);

	ir_type *tp = new_type_array(irp->byte_type);
	set_array_size_int(tp, size);
	set_type_alignment_bytes(tp, alignment);
	set_type_size_bytes(tp, size);

	unsigned frame_size  = get_type_size_bytes(frame_type);
	unsigned frame_align = get_type_alignment_bytes(frame_type);
	int      offset;
	if (at_start) {
		unsigned delta = (size + frame_align - 1) & ~(frame_align - 1);
		/* fix all offsets so far */
		for (size_t i = 0, n = get_compound_n_members(frame_type); i < n; ++i) {
			ir_entity *ent = get_compound_member(frame_type, i);

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
	set_entity_visibility(area, ir_visibility_private);
	set_entity_offset(area, offset);
	set_type_size_bytes(frame_type, frame_size);
	if (alignment > frame_align) {
		set_type_alignment_bytes(frame_type, alignment);
	}

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
	switch (get_type_opcode(type)) {
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
