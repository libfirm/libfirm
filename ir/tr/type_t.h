/*
 * Project:     libFIRM
 * File name:   ir/tr/type_t.h
 * Purpose:     Representation of types -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _TYPE_T_H_
# define _TYPE_T_H_
# ifdef HAVE_CONFIG_H
# include "config.h"
# endif
# include "type.h"
# include "tpop_t.h"

/**
 * @file type_t.h
 * This file contains the datatypes hidden in type.h.
 *
 * @author Goetz Lindenmaier
 * @see  type.h tpop_t.h tpop.h
 */

/** class attributes */
typedef struct {
  entity **members;    /**< fields and methods of this class */
  type   **subtypes;   /**< direct subtypes */
  type   **supertypes; /**< direct supertypes */
  peculiarity peculiarity;
  int dfn;             /**< number used for 'instanceof' operator */
} cls_attr;

/** struct attributs */
typedef struct {
  entity **members;    /**< fields of this struct. No method entities
			  allowed. */
} stc_attr;

/** method attributes */
typedef struct {
  int n_params;              /**< number of parameters */
  type **param_type;         /**< code generation needs this information. */
  type *value_params;        /**< A type whose entities represent copied value arguments. */
  int n_res;                 /**< number of results */
  type **res_type;           /**< array with result types */
  type *value_ress;          /**< A type whose entities represent copied value results. */
  variadicity variadicity;   /**< variadicity of the method. */
  int first_variadic_param;  /**< index of the first variadic param or -1 if non-variadic .*/
} mtd_attr;

/** union attributs */
typedef struct {
  int     n_types;
  /* type  **unioned_type; * a list of unioned types. */
  /* ident **delim_names;  * names of the union delimiters. */
  entity **members;    /**< fields of this union. No method entities
			  allowed.  */

} uni_attr;

/** array attributs */
typedef struct {
  int   n_dimensions;  /**< Number of array dimensions.  */
  ir_node **lower_bound;   /**< Lower bounds of dimensions.  Usually all 0. */
  ir_node **upper_bound;   /**< Upper bounds or dimensions. */
  int *order;              /**< Ordering of dimensions. */
  type *element_type;  /**< The type of the array elements. */
  entity *element_ent; /**< Entity for the array elements, to be used for
			  element selection with Sel. */
} arr_attr;

/** enum attributs */
typedef struct {
  int      n_enums;    /**< Number of enumerators. */
  tarval **enumer;     /**< Contains all constants that represent a member
                          of the enum -- enumerators. */
  ident  **enum_nameid;/**< Contains the names of the enum fields as specified by
                          the source program */
} enm_attr;

/** pointer attributs */
typedef struct {
  type *points_to;     /**< The type of the enitity the pointer points to. */
} ptr_attr;

/*
typedef struct {   * No private attr yet! *
} pri_attr; */


/*
typedef struct {        * No private attr, must be smaller than others! *
} id_attr;
*/

/** General type attributs. */
typedef union {
  cls_attr ca;  	/**< attributes of a class type */
  stc_attr sa;		/**< attributes of a struct type */
  mtd_attr ma;		/**< attributes of a method type */
  uni_attr ua;		/**< attributes of an union type */
  arr_attr aa;		/**< attributes of an array type */
  enm_attr ea;		/**< attributes of an enumeration type */
  ptr_attr pa;		/**< attributes of a pointer type */
} tp_attr;

/** the structure of a type */
struct type {
  firm_kind kind;
  tp_op *type_op;
  ident *name;
  type_state state;        /**< Represents the types state: layout undefined or
			      fixed. */
  int size;                /**< Size of an entity of this type.  This is determined
			      when fixing the layout of this class.  Size must be
			      given in bytes. */
  ir_mode *mode;           /**< The mode for atomic types */
  unsigned long visit;     /**< visited counter for walks of the type information */
  void *link;              /**< holds temporary data - like in irnode_t.h */
  struct dbg_info* dbi;    /**< A pointer to information for debug support. */

#ifdef DEBUG_libfirm
  int nr;             /**< a unique node number for each node to make output
			      readable. */
#endif
  tp_attr attr;            /* type kind specific fields. This must be the last
			      entry in this struct!  Varying size! */
};

/**
 *   Creates a new type representation:
 *
 *   @param type_op - the kind of this type.  May not be type_id.
 *   @param mode    - the mode to be used for this type, may be NULL
 *   @param name    - an ident for the name of this type.
 *   @return a new type of the given type.  The remaining private attributes are not
 *   @return initalized.  The type is in state layout_undefined.
 *
 */
INLINE type *
new_type(tp_op *type_op,
	 ir_mode *mode,
	 ident* name);
void free_type_attrs       (type *tp);

INLINE void free_class_entities      (type *clss);
INLINE void free_struct_entities     (type *strct);
INLINE void free_method_entities     (type *method);
INLINE void free_union_entities      (type *uni);
INLINE void free_array_entities      (type *array);
INLINE void free_enumeration_entities(type *enumeration);
INLINE void free_pointer_entities    (type *pointer);
INLINE void free_primitive_entities  (type *primitive);

INLINE void free_class_attrs      (type *clss);
INLINE void free_struct_attrs     (type *strct);
INLINE void free_method_attrs     (type *method);
INLINE void free_union_attrs      (type *uni);
INLINE void free_array_attrs      (type *array);
INLINE void free_enumeration_attrs(type *enumeration);
INLINE void free_pointer_attrs    (type *pointer);
INLINE void free_primitive_attrs  (type *primitive);


/** initialize the type module */
void init_type (void);


/* ------------------- *
 *  inline functions   *
 * ------------------- */

extern unsigned long type_visited;

static INLINE void __set_master_type_visited(unsigned long val) { type_visited = val; }
static INLINE unsigned long __get_master_type_visited(void)     { return type_visited; }
static INLINE void __inc_master_type_visited(void)              { type_visited++; }

static INLINE void *
__get_type_link(type *tp) {
  assert(tp && tp->kind == k_type);
  return(tp -> link);
}

static INLINE void
__set_type_link(type *tp, void *l) {
  assert(tp && tp->kind == k_type);
  tp -> link = l;
}

static INLINE tp_op*
__get_type_tpop(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->type_op;
}

static INLINE ident*
__get_type_tpop_nameid(type *tp) {
  assert(tp && tp->kind == k_type);
  return get_tpop_ident(tp->type_op);
}

static INLINE tp_opcode
__get_type_tpop_code(type *tp) {
  assert(tp && tp->kind == k_type);
  return get_tpop_code(tp->type_op);
}

static INLINE ir_mode *
__get_type_mode(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->mode;
}

static INLINE ident *
__get_type_ident(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->name;
}

static INLINE void
__set_type_ident(type *tp, ident* id) {
  assert(tp && tp->kind == k_type);
  tp->name = id;
}

static INLINE long
__get_type_nr(type *tp) {
  assert(tp);
#ifdef DEBUG_libfirm
  return tp->nr;
#else
  return (long)tp;
#endif
}

static INLINE int
__get_type_size(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->size;
}

static INLINE type_state
__get_type_state(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->state;
}

static INLINE unsigned long
__get_type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit;
}

static INLINE void
__set_type_visited(type *tp, unsigned long num) {
  assert(tp && tp->kind == k_type);
  tp->visit = num;
}

static INLINE void
__mark_type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  assert(tp->visit < type_visited);
  tp->visit = type_visited;
}

static INLINE int
__type_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit >= type_visited;
}

static INLINE int
__type_not_visited(type *tp) {
  assert(tp && tp->kind == k_type);
  return tp->visit  < type_visited;
}

static INLINE int
__is_type(void *thing) {
  return (get_kind(thing) == k_type);
}

static INLINE int
__is_class_type(type *clss) {
  assert(clss);
  return (clss->type_op == type_class);
}

static INLINE int
__is_struct_type(type *strct) {
  assert(strct);
  return (strct->type_op == type_struct);
}

static INLINE int
__is_method_type(type *method) {
  assert(method);
  return (method->type_op == type_method);
}

static INLINE int
__is_union_type(type *uni) {
  assert(uni);
  return (uni->type_op == type_union);
}

static INLINE int
__is_array_type(type *array) {
  assert(array);
  return (array->type_op == type_array);
}

static INLINE int
__is_enumeration_type(type *enumeration) {
  assert(enumeration);
  return (enumeration->type_op == type_enumeration);
}

static INLINE int
__is_pointer_type(type *pointer) {
  assert(pointer);
  return (pointer->type_op == type_pointer);
}

/** Returns true if a type is a primitive type. */
static INLINE int
__is_primitive_type(type *primitive) {
  assert(primitive && primitive->kind == k_type);
  return (primitive->type_op == type_primitive);
}

static INLINE int
__is_atomic_type(type *tp) {
  assert(tp && tp->kind == k_type);
  return (is_primitive_type(tp) || is_pointer_type(tp) ||
	  is_enumeration_type(tp));
}


#define set_master_type_visited(val)      __set_master_type_visited(val)
#define get_master_type_visited()         __get_master_type_visited()
#define inc_master_type_visited()         __inc_master_type_visited()
#define get_type_link(tp)                 __get_type_link(tp)
#define set_type_link(tp, l)              __set_type_link(tp, l)
#define get_type_tpop(tp)                 __get_type_tpop(tp)
#define get_type_tpop_nameid(tp)          __get_type_tpop_nameid(tp)
#define get_type_tpop_code(tp)            __get_type_tpop_code(tp)
#define get_type_mode(tp)                 __get_type_mode(tp)
#define get_type_ident(tp)                __get_type_ident(tp)
#define set_type_ident(tp, id)            __set_type_ident(tp, id)
#define get_type_nr(tp)                   __get_type_nr(tp)
#define get_type_size(tp)                 __get_type_size(tp)
#define get_type_state(tp)                __get_type_state(tp)
#define get_type_visited(tp)              __get_type_visited(tp)
#define set_type_visited(tp, num)         __set_type_visited(tp, num)
#define mark_type_visited(tp)             __mark_type_visited(tp)
#define type_visited(tp)                  __type_visited(tp)
#define type_not_visited(tp)              __type_not_visited(tp)
#define is_type(thing)                    __is_type(thing)
#define is_class_type(clss)               __is_class_type(clss)
#define is_struct_type(strct)             __is_struct_type(strct)
#define is_method_type(method)            __is_method_type(method)
#define is_union_type(uni)                __is_union_type(uni)
#define is_array_type(array)              __is_array_type(array)
#define is_enumeration_type(enumeration)  __is_enumeration_type(enumeration)
#define is_pointer_type(pointer)          __is_pointer_type(pointer)
#define is_primitive_type(primitive)      __is_primitive_type(primitive)
#define is_atomic_type(tp)                __is_atomic_type(tp)

# endif /* _TYPE_T_H_ */
