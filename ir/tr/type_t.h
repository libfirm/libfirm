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

#define set_master_type_visited(val)      _set_master_type_visited(val)
#define get_master_type_visited()         _get_master_type_visited()
#define inc_master_type_visited()         _inc_master_type_visited()
#define get_type_link(tp)                 _get_type_link(tp)
#define set_type_link(tp, l)              _set_type_link(tp, l)
#define get_type_tpop(tp)                 _get_type_tpop(tp)
#define get_type_tpop_nameid(tp)          _get_type_tpop_nameid(tp)
#define get_type_tpop_code(tp)            _get_type_tpop_code(tp)
#define get_type_mode(tp)                 _get_type_mode(tp)
#define get_type_size_bytes(tp)           _get_type_size_bytes(tp)
#define get_type_state(tp)                _get_type_state(tp)
#define get_type_visited(tp)              _get_type_visited(tp)
#define set_type_visited(tp, num)         _set_type_visited(tp, num)
#define mark_type_visited(tp)             _mark_type_visited(tp)
#define type_visited(tp)                  _type_visited(tp)
#define type_not_visited(tp)              _type_not_visited(tp)
#define get_type_dbg_info(tp)             _get_type_dbg_info(tp)
#define set_type_dbg_info(tp, db)         _set_type_dbg_info(tp, db)
#define is_type(thing)                    _is_type(thing)
#define is_Class_type(clss)               _is_class_type(clss)
#define get_class_n_members(clss)         _get_class_n_members(clss)
#define get_class_member(clss, pos)       _get_class_member(clss, pos)
#define get_class_vtable_size(clss)       _get_class_vtable_size(clss)
#define set_class_vtable_size(clss, size) _set_class_vtable_size(clss, size)
#define is_class_final(clss)              _is_class_final(clss)
#define set_class_final(clss, flag)       _set_class_final(clss, flag)
#define is_class_interface(clss)          _is_class_interface(clss)
#define set_class_interface(clss, flag)   _set_class_interface(clss, flag)
#define is_class_abstract(clss)           _is_class_abstract(clss)
#define set_class_abstract(clss, flag)    _set_class_abstract(clss, flag)
#define is_Struct_type(strct)             _is_struct_type(strct)
#define is_Method_type(method)            _is_method_type(method)
#define is_Union_type(uni)                _is_union_type(uni)
#define is_Array_type(array)              _is_array_type(array)
#define is_Pointer_type(pointer)          _is_pointer_type(pointer)
#define is_Primitive_type(primitive)      _is_primitive_type(primitive)
#define is_atomic_type(tp)                _is_atomic_type(tp)
#define get_method_n_params(method)       _get_method_n_params(method)
#define get_method_n_ress(method)         _get_method_n_ress(method)
#define get_method_additional_properties(method)        _get_method_additional_properties(method)
#define set_method_additional_properties(method, mask)  _set_method_additional_properties(method, mask)
#define add_method_additional_properties(method, flag)  _add_method_additional_properties(method, flag)
#define get_method_calling_convention(method)           _get_method_calling_convention(method)
#define set_method_calling_convention(method, cc_mask)  _set_method_calling_convention(method, cc_mask)

/** Class flags. */
enum class_flags {
	cf_none            = 0,  /**< No flags. */
	cf_final_class     = 1,  /**< Set if a class is an final class */
	cf_interface_class = 2,  /**< Set if a class is an "interface" */
	cf_absctract_class = 4,  /**< Set if a class is "abstract" */
};

/** Class type attributes. */
typedef struct {
	ir_entity  **members;           /**< Array containing the fields and methods of this class. */
	ir_type **subtypes;          /**< Array containing the direct subtypes. */
	ir_type **supertypes;        /**< Array containing the direct supertypes */
	ir_peculiarity peculiarity;  /**< The peculiarity of this class. */
	int      dfn;                /**< A number that can be used for 'instanceof' operator. */
	unsigned vtable_size;        /**< The size of the vtable for this class. */
	unsigned clss_flags;         /**< Additional class flags. */
} cls_attr;

/** Struct type attributes. */
typedef struct {
	ir_entity **members; /**< Fields of this struct. No method entities allowed. */
} stc_attr;

/** Method type attributes. */
typedef struct {
	size_t                    n_params;         /**< Number of parameters. */
	ir_type                 **params;           /**< Array of parameter types. */
	size_t                    n_res;            /**< Number of results. */
	ir_type                 **res_type;         /**< Array of result types. */
	ir_variadicity            variadicity;      /**< The variadicity of the method. */
	mtp_additional_properties properties;       /**< Set of additional method properties. */
	unsigned                  irg_calling_conv; /**< A set of calling convention flags. */
} mtd_attr;

/** Union type attributes. */
typedef struct {
	ir_entity **members;    /**< Fields of this union. No method entities allowed. */
} uni_attr;

/** Array type attributes. */
typedef struct {
	size_t  n_dimensions;   /**< Number of array dimensions.  */
	ir_node **lower_bound;  /**< Lower bounds of dimensions.  Usually all 0. */
	ir_node **upper_bound;  /**< Upper bounds or dimensions. */
	size_t  *order;         /**< Ordering of dimensions. */
	ir_type *element_type;  /**< The type of the array elements. */
	ir_entity *element_ent; /**< entity for the array elements, to be used for
	                             element selection with a Sel node. */
} arr_attr;

/** Pointer type attributes. */
typedef struct {
	ir_type *points_to;  /**< The type of the ir_entity the pointer points to. */
} ptr_attr;

/** General type attributes. */
typedef union {
	cls_attr ca;      /**< Attributes of a class type */
	stc_attr sa;      /**< Attributes of a struct type */
	mtd_attr ma;      /**< Attributes of a method type */
	uni_attr ua;      /**< Attributes of an union type */
	arr_attr aa;      /**< Attributes of an array type */
	ptr_attr pa;      /**< Attributes of a pointer type */
} tp_attr;

/** Additional type flags. */
typedef enum type_flags {
	tf_none             = 0, /**< No flags. */
	tf_lowered_type     = 1U << 0, /**< Set if this is a lowered type. */
	tf_layout_fixed     = 1U << 1, /**< Set if the layout of a type is fixed */

	tf_frame_type       = 1U << 2, /**< Set if this is a frame type. */
	tf_segment          = 1U << 3, /**< type represents a linker segment */
	tf_global_type      = 1U << 4, /**< Set only for the global type */
	tf_tls_type         = 1U << 5, /**< Set only for the tls type */
	tf_constructors     = 1U << 6, /**< Set only for the constructors segment type */
	tf_destructors      = 1U << 7, /**< Set only for the destructors segment type */
	tf_variable_size    = 1U << 8, /**< compound or array type may have variable size last element */
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

#ifdef DEBUG_libfirm
	long nr;                 /**< An unique node number for each node to make
	                              output readable. */
#endif
	tp_attr attr;            /**< Type kind specific fields. This must be the
	                              last entry in this struct!  Varying size! */
};

void free_type_entities(ir_type *tp);

void free_class_entities      (ir_type *clss);
void free_struct_entities     (ir_type *strct);
void free_method_entities     (ir_type *method);
void free_union_entities      (ir_type *uni);
void free_array_entities      (ir_type *array);
void free_pointer_entities    (ir_type *pointer);

void free_array_automatic_entities(ir_type *array);

void free_class_attrs      (ir_type *clss);
void free_struct_attrs     (ir_type *strct);
void free_method_attrs     (ir_type *method);
void free_union_attrs      (ir_type *uni);
void free_array_attrs      (ir_type *array);
void free_pointer_attrs    (ir_type *pointer);

void set_class_mode(ir_type *tp, ir_mode *mode);
void set_struct_mode(ir_type *tp, ir_mode *mode);
void set_pointer_mode(ir_type *tp, ir_mode *mode);
void set_primitive_mode(ir_type *tp, ir_mode *mode);

void set_class_size(ir_type *tp, unsigned bytes);
void set_struct_size(ir_type *tp, unsigned bytes);
void set_union_size(ir_type *tp, unsigned bytes);
void set_array_size(ir_type *tp, unsigned bytes);
void set_default_size(ir_type *tp, unsigned bytes);

/** Set and get a class' dfn --
 * This is an undocumented field, subject to change! */
void set_class_dfn(ir_type *clss, int dfn);
int  get_class_dfn(const ir_type *clss);

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

static inline void _set_master_type_visited(ir_visited_t val)
{
	firm_type_visited = val;
}

static inline ir_visited_t _get_master_type_visited(void)
{
	return firm_type_visited;
}

static inline void _inc_master_type_visited(void)
{
	++firm_type_visited;
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

static inline void *_get_type_link(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return(tp -> link);
}

static inline void _set_type_link(ir_type *tp, void *l)
{
	assert(tp->kind == k_type);
	tp -> link = l;
}

static inline const tp_op *_get_type_tpop(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->type_op;
}

static inline ident *_get_type_tpop_nameid(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return get_tpop_ident(tp->type_op);
}

static inline tp_opcode _get_type_tpop_code(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return get_tpop_code(tp->type_op);
}

static inline ir_mode *_get_type_mode(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->mode;
}

static inline unsigned _get_type_size_bytes(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->size;
}

static inline ir_type_state _get_type_state(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->flags & tf_layout_fixed ? layout_fixed : layout_undefined;
}

static inline ir_visited_t _get_type_visited(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->visit;
}

static inline void _set_type_visited(ir_type *tp, ir_visited_t num)
{
	assert(tp->kind == k_type);
	tp->visit = num;
}

static inline void _mark_type_visited(ir_type *tp)
{
	assert(tp->kind == k_type);
	assert(tp->visit < firm_type_visited);
	tp->visit = firm_type_visited;
}

static inline int _type_visited(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->visit >= firm_type_visited;
}

static inline int _type_not_visited(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return tp->visit  < firm_type_visited;
}

static inline type_dbg_info *_get_type_dbg_info(const ir_type *tp)
{
	return tp->dbi;
}

static inline void _set_type_dbg_info(ir_type *tp, type_dbg_info *db)
{
	tp->dbi = db;
}

static inline int _is_type(const void *thing)
{
	return get_kind(thing) == k_type;
}

static inline int _is_class_type(const ir_type *clss)
{
	return clss->type_op == type_class;
}

static inline size_t _get_class_n_members(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return ARR_LEN(clss->attr.ca.members);
}

static inline ir_entity *_get_class_member(const ir_type *clss, size_t pos)
{
	assert(clss->type_op == type_class);
	assert(pos < _get_class_n_members(clss));
	return clss->attr.ca.members[pos];
}

static inline unsigned _get_class_vtable_size(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->attr.ca.vtable_size;
}

static inline void _set_class_vtable_size(ir_type *clss, unsigned vtable_size)
{
	assert(clss->type_op == type_class);
	clss->attr.ca.vtable_size = vtable_size;
}

static inline int _is_class_final(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->attr.ca.clss_flags & cf_final_class;
}

static inline void _set_class_final(ir_type *clss, int final)
{
	assert(clss->type_op == type_class);
	if (final)
		clss->attr.ca.clss_flags |= cf_final_class;
	else
		clss->attr.ca.clss_flags &= ~cf_final_class;
}

static inline int _is_class_interface(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->attr.ca.clss_flags & cf_interface_class;
}

static inline void _set_class_interface(ir_type *clss, int final)
{
	assert(clss->type_op == type_class);
	if (final)
		clss->attr.ca.clss_flags |= cf_interface_class;
	else
		clss->attr.ca.clss_flags &= ~cf_interface_class;
}

static inline int _is_class_abstract(const ir_type *clss)
{
	assert(clss->type_op == type_class);
	return clss->attr.ca.clss_flags & cf_absctract_class;
}

static inline void _set_class_abstract(ir_type *clss, int final)
{
	assert(clss->type_op == type_class);
	if (final)
		clss->attr.ca.clss_flags |= cf_absctract_class;
	else
		clss->attr.ca.clss_flags &= ~cf_absctract_class;
}

static inline int _is_struct_type(const ir_type *strct)
{
	return (strct->type_op == type_struct);
}

static inline int _is_method_type(const ir_type *method)
{
	return (method->type_op == type_method);
}

static inline int _is_union_type(const ir_type *uni)
{
	return (uni->type_op == type_union);
}

static inline int _is_array_type(const ir_type *array)
{
	return (array->type_op == type_array);
}

static inline int _is_pointer_type(const ir_type *pointer)
{
	return (pointer->type_op == type_pointer);
}

/** Returns true if a type is a primitive type. */
static inline int _is_primitive_type(const ir_type *primitive)
{
	assert(primitive->kind == k_type);
	return (primitive->type_op == type_primitive);
}

static inline int _is_atomic_type(const ir_type *tp)
{
	assert(tp->kind == k_type);
	return _is_primitive_type(tp) || _is_pointer_type(tp);
}

static inline size_t _get_method_n_params(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.n_params;
}

static inline size_t _get_method_n_ress(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.n_res;
}

static inline mtp_additional_properties _get_method_additional_properties(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.properties;
}

static inline void _set_method_additional_properties(ir_type *method, mtp_additional_properties properties)
{
	assert(method->type_op == type_method);
	method->attr.ma.properties = properties;
}

static inline void _add_method_additional_properties(ir_type *method, mtp_additional_properties properties)
{
	assert(method->type_op == type_method);
	method->attr.ma.properties |= properties;
}

static inline unsigned _get_method_calling_convention(const ir_type *method)
{
	assert(method->type_op == type_method);
	return method->attr.ma.irg_calling_conv;
}

static inline void _set_method_calling_convention(ir_type *method, unsigned cc_mask)
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

static inline ir_mode *get_type_pointer_mode(ir_type *const type)
{
	return is_Method_type(type) || is_code_type(type) ? mode_P_code : mode_P_data;
}

#endif /* FIRM_TR_TYPE_T_H */
