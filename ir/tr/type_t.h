/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of types -- private header.
 * @author  Goetz Lindenmaier, Michael Beck
 * @see     type.h tpop_t.h tpop.h
 */
#ifndef FIRM_TR_TYPE_T_H
#define FIRM_TR_TYPE_T_H

#include <stdbool.h>
#include "typerep.h"
#include "tpop_t.h"
#include "firm_common.h"

#include "array.h"

#define get_master_type_visited()         get_master_type_visited_()
#define get_type_link(tp)                 get_type_link_(tp)
#define set_type_link(tp, l)              set_type_link_(tp, l)
#define get_type_tpop(tp)                 get_type_tpop_(tp)
#define get_type_tpop_nameid(tp)          get_type_tpop_nameid_(tp)
#define get_type_tpop_code(tp)            get_type_tpop_code_(tp)
#define get_type_mode(tp)                 get_type_mode_(tp)
#define get_type_alignment_bytes(tp)      get_type_alignment_bytes_(tp)
#define get_type_size_bytes(tp)           get_type_size_bytes_(tp)
#define get_type_state(tp)                get_type_state_(tp)
#define get_type_visited(tp)              get_type_visited_(tp)
#define set_type_visited(tp, num)         set_type_visited_(tp, num)
#define mark_type_visited(tp)             mark_type_visited_(tp)
#define type_visited(tp)                  type_visited_(tp)
#define get_type_dbg_info(tp)             get_type_dbg_info_(tp)
#define set_type_dbg_info(tp, db)         set_type_dbg_info_(tp, db)
#define get_compound_n_members(type)      get_compound_n_members_(type)
#define get_compound_member(type, pos)    get_compound_member_(type, pos)
#define is_Class_type(clss)               is_class_type_(clss)
#define is_Struct_type(strct)             is_struct_type_(strct)
#define is_Method_type(method)            is_method_type_(method)
#define is_Union_type(uni)                is_union_type_(uni)
#define is_Array_type(array)              is_array_type_(array)
#define is_Pointer_type(pointer)          is_pointer_type_(pointer)
#define is_Primitive_type(primitive)      is_primitive_type_(primitive)
#define is_atomic_type(tp)                is_atomic_type_(tp)
#define get_method_n_params(method)       get_method_n_params_(method)
#define get_method_n_ress(method)         get_method_n_ress_(method)
#define get_method_additional_properties(method)        get_method_additional_properties_(method)
#define set_method_additional_properties(method, mask)  set_method_additional_properties_(method, mask)
#define add_method_additional_properties(method, flag)  add_method_additional_properties_(method, flag)
#define get_method_calling_convention(method)           get_method_calling_convention_(method)
#define set_method_calling_convention(method, cc_mask)  set_method_calling_convention_(method, cc_mask)

/** Compound type attributes. */
typedef struct {
	ir_entity **members;
} compound_attr;

/** Class type attributes. */
typedef struct {
	compound_attr base;
	ir_type     **subtypes;   /**< Array containing the direct subtypes. */
	ir_type     **supertypes; /**< Array containing the direct supertypes */
} cls_attr;

/** Method type attributes. */
typedef struct {
	size_t                    n_params;         /**< Number of parameters. */
	ir_type                 **params;           /**< Array of parameter types. */
	size_t                    n_res;            /**< Number of results. */
	ir_type                 **res_type;         /**< Array of result types. */
	bool                      variadic;         /**< The variadicity of the method. */
	mtp_additional_properties properties;       /**< Set of additional method properties. */
	unsigned                  irg_calling_conv; /**< A set of calling convention flags. */
} mtd_attr;

/** Array type attributes. */
typedef struct {
	ir_node *size;         /**< number of elements in the array. */
	ir_type *element_type; /**< The type of the array elements. */
} arr_attr;

/** Pointer type attributes. */
typedef struct {
	ir_type *points_to;  /**< The type of the ir_entity the pointer points to. */
} ptr_attr;

/** General type attributes. */
typedef union {
	compound_attr ca; /**< Attributes of a compount type */
	cls_attr      cla;
	mtd_attr      ma; /**< Attributes of a method type */
	arr_attr      aa; /**< Attributes of an array type */
	ptr_attr      pa; /**< Attributes of a pointer type */
} tp_attr;

/** Additional type flags. */
typedef enum type_flags {
	tf_none          = 0,       /**< No flags. */
	tf_lowered_type  = 1U << 0, /**< Set if this is a lowered type. */
	tf_layout_fixed  = 1U << 1, /**< Set if the layout of a type is fixed */

	tf_frame_type    = 1U << 2, /**< Set if this is a frame type. */
	tf_segment       = 1U << 3, /**< type represents a linker segment */
	tf_global_type   = 1U << 4, /**< Set only for the global type */
	tf_tls_type      = 1U << 5, /**< Set only for the tls type */
	tf_info          = 1U << 6, /**< infos (for example constructor, destructor pointers) */
	tf_variable_size = 1U << 7, /**< compound or array type may have variable size last element */
	tf_lowered_dw    = 1U << 8, /**< hack to identify lowered doubleword params */
} type_flags;
ENUM_BITSET(type_flags)

/**
 *  An abstract data type to represent types.
 *
 *  This is the abstract data type with which any type known in the
 *  compiled program can be represented.  This includes types specified
 *  in the program as well as types defined by the language.  In the
 *  view of the intermediate representation there is no difference
 *  between these types.
 *
 *  There exist several kinds of types, arranged by the structure of
 *  the type.  These are distinguished by a type opcode.
 *  A type is described by a set of attributes.  Some of these attributes
 *  are common to all types, others depend on the kind of the type.
 */
struct ir_type {
	firm_kind kind;          /**< the firm kind, must be k_type */
	const tp_op *type_op;    /**< the type operation of the type */
	ident *name;             /**< The name of the type */
	ir_visibility visibility;/**< Visibility of entities of this type. */
	unsigned flags;          /**< Type flags, a bitmask of enum type_flags. */
	unsigned size;           /**< Size of an ir_entity of this type. This is
	                              determined when fixing the layout of this
	                              class.  Size must be given in bytes. */
	unsigned align;          /**< Alignment of an ir_entity of this type. This
	                              should be set according to the source
	                              language needs. If not set, it's calculated
	                              automatically by get_type_alignment().
	                              Alignment must be given in bytes. */
	ir_mode *mode;           /**< The mode for atomic types */
	ir_visited_t visit;      /**< visited counter for walks of the type information */
	void *link;              /**< holds temporary data - like in irnode_t.h */
	type_dbg_info *dbi;      /**< A pointer to information for debug support. */
	ir_type *higher_type;    /**< link to highlevel type in case of lowered
	                              types */
	long nr;                 /**< An unique number for each type. */
	tp_attr attr;            /**< Type kind specific fields. This must be the
	                              last entry in this struct!  Varying size! */
};

void free_type_entities(ir_type *tp);

void free_compound_entities(ir_type *type);

void free_class_attrs(ir_type *clss);
void free_struct_attrs(ir_type *strct);
void free_method_attrs(ir_type *method);
void free_union_attrs(ir_type *uni);

void add_compound_member(ir_type *compound, ir_entity *entity);

/** Initialize the type module. */
void ir_init_type(ir_prog *irp);

/** free internal datastructures of type module */
void ir_finish_type(ir_prog *irp);

/** Clone an existing method type.
 *
 * @param tp      the method type to clone.
 * @param prefix  if non-null, will be the prefix for the name of
 *                the cloned type
 *
 * @return the cloned method type.
 */
ir_type *clone_type_method(ir_type *tp);

extern ir_visited_t firm_type_visited;

static inline ir_visited_t get_master_type_visited_(void)
{
	return firm_type_visited;
}

static inline int is_lowered_type(const ir_type *tp)
{
	return tp->flags & tf_lowered_type;
}

static inline ir_type *get_higher_type(const ir_type *tp)
{
	return tp->higher_type;
}

static inline void set_higher_type(ir_type *tp, ir_type *higher_type)
{
	tp->flags |= tf_lowered_type;
	tp->higher_type = higher_type;
}

static inline void *get_type_link_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return(tp -> link);
}

static inline void set_type_link_(ir_type *tp, void *l)
{
	assert(tp->kind == k_type);
	tp -> link = l;
}

static inline const tp_op *get_type_tpop_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->type_op;
}

static inline ident *get_type_tpop_nameid_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return get_tpop_ident(tp->type_op);
}

static inline tp_opcode get_type_tpop_code_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return get_tpop_code(tp->type_op);
}

static inline ir_mode *get_type_mode_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->mode;
}

static inline unsigned get_type_alignment_bytes_(const ir_type *type)
{
	assert(type->kind == k_type);
	return type->align;
}

static inline unsigned get_type_size_bytes_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->size;
}

static inline ir_type_state get_type_state_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->flags & tf_layout_fixed ? layout_fixed : layout_undefined;
}

static inline ir_visited_t get_type_visited_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->visit;
}

static inline void set_type_visited_(ir_type *tp, ir_visited_t num)
{
	assert(tp->kind == k_type);
	tp->visit = num;
}

static inline void mark_type_visited_(ir_type *tp)
{
	assert(tp->kind == k_type);
	assert(tp->visit < firm_type_visited);
	tp->visit = firm_type_visited;
}

static inline int type_visited_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->visit >= firm_type_visited;
}

static inline type_dbg_info *get_type_dbg_info_(const ir_type *tp)
{
	return tp->dbi;
}

static inline void set_type_dbg_info_(ir_type *tp, type_dbg_info *db)
{
	tp->dbi = db;
}

static inline int is_class_type_(const ir_type *clss)
{
	return clss->type_op == type_class;
}

static inline size_t get_compound_n_members_(const ir_type *type)
{
	assert(is_compound_type(type));
	return ARR_LEN(type->attr.ca.members);
}

static inline ir_entity *get_compound_member_(ir_type const *const type,
                                             size_t const pos)
{
	assert(is_compound_type(type));
	assert(pos < get_compound_n_members(type));
	return type->attr.ca.members[pos];
}

static inline int is_struct_type_(const ir_type *strct)
{
	return (strct->type_op == type_struct);
}

static inline int is_method_type_(const ir_type *method)
{
	return (method->type_op == type_method);
}

static inline int is_union_type_(const ir_type *uni)
{
	return (uni->type_op == type_union);
}

static inline int is_array_type_(const ir_type *array)
{
	return (array->type_op == type_array);
}

static inline int is_pointer_type_(const ir_type *pointer)
{
	return (pointer->type_op == type_pointer);
}

/** Returns true if a type is a primitive type. */
static inline int is_primitive_type_(const ir_type *primitive)
{
	assert(primitive->kind == k_type);
	return (primitive->type_op == type_primitive);
}

static inline int is_atomic_type_(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return is_Primitive_type(tp) || is_Pointer_type(tp);
}

static inline size_t get_method_n_params_(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.n_params;
}

static inline size_t get_method_n_ress_(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.n_res;
}

static inline mtp_additional_properties get_method_additional_properties_(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.properties;
}

static inline void set_method_additional_properties_(ir_type *method, mtp_additional_properties properties)
{
	assert(method->type_op == type_method);
	method->attr.ma.properties = properties;
}

static inline void add_method_additional_properties_(ir_type *method, mtp_additional_properties properties)
{
	assert(method->type_op == type_method);
	method->attr.ma.properties |= properties;
}

static inline unsigned get_method_calling_convention_(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.irg_calling_conv;
}

static inline void set_method_calling_convention_(ir_type *method, unsigned cc_mask)
{
	assert(method->type_op == type_method);
	method->attr.ma.irg_calling_conv = cc_mask;
}

/**
 * Check if type is a compound or array type.
 * This function returns true iff a value of this type cannot be represented by
 * a firm mode and need therefore special handling in lower_calls when used as
 * a parameter or return type.
 */
static inline bool is_aggregate_type(const ir_type *type)
{
	return is_compound_type(type) || is_Array_type(type);
}

ir_type *new_type_segment(ident *name, type_flags flags);

static inline void copy_method_properties(ir_type *const dst, ir_type const *const src)
{
	set_method_variadic(dst, is_method_variadic(src));
	set_method_calling_convention(dst, get_method_calling_convention(src));
	set_method_additional_properties(dst, get_method_additional_properties(src));
}

#endif
