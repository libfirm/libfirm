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
  int n_params;        /**< number of parameters */
  type **param_type;   /**< code generation needs this information. */
  type *value_params;  /**< A type whose entities represent copied value arguments. */
  int n_res;           /**< number of results */
  type **res_type;     /**< array with result types */
  type *value_ress;    /**< A type whose entities represent copied value results. */
  variadicity variadicity; /**< variadicity of the method */
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

/** general type attributs */
typedef union {
  cls_attr ca;
  stc_attr sa;
  mtd_attr ma;
  uni_attr ua;
  arr_attr aa;
  enm_attr ea;
  ptr_attr pa;
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

# endif /* _TYPE_T_H_ */
