/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**

@@@@@@@  Improve documentation: distinguish fields that are
set by the frontend and contain knowledge specified by the source
program from fields containing information derived by analysis/optimization
or lowering phases.


**  type.h: datastructures to hold type information.
**
**  This module supplies datastructures to represent all types
**  known in the compiled program.  This includes types specified
**  in the program as well as types defined by the language.  In the
**  view of the intermediate representation there is no difference
**  between these types.
**  Types are different from the modes defined in irmode:  Types are
**  on the level of the programming language, modes at the level of
**  the target processor.
**
**
**  General datastructure
**  =====================
**
**  Firm distinguishes several different type constructs.  These are
**  implemented as structs.  A union of the individual structs constructs
**  the firm node "type".
**
**  All type constructs have the following fields:
**
**  kind         A firm_kind tag containing k_type_class.  This is useful
**               for dynamically checking the sort of a type.  Automatically
**               generated.
**
**  name         An identifier specifying the type name.  Set by the frontend.
**
**  visit        A counter for walks of the type information.
**
**
**  General functionality
**  =====================
**
**  is_type(t)   Returns true if t is a type node, else false.
**
**
**  type_class
**  ==========
**
**  Type_class represents class types.  A list of fields and
**  methods is associated with a class.  Further a class can
**  inherit from and bequest to other classes.
**
**  fields:
**  -------
**
**  **member     All entities belonging to this class.  This are methodes
**               which have type_method or fields that can have any of the
**               following types: k_type_class, k_type_strct, k_type_union,
**               k_type_array, k_type_enumeration, k_type_pointer, k_type_primitive.
**
**  **subtypes   A list of direct subclasses.
**
**  **supertypes A list of direct superclasses.
**
**
**  type_strct
**  ==========
**
**  Type_strct represents aggregate types that consist of a list
**  of fields.
**
**  fields:
**  -------
**
**  **member     All entities belonging to this class.  This are the fields
**               that can have any of the following types:  k_type_class,
**               k_type_strct, k_type_union, k_type_array, k_type_enumeration,
**  	         k_type_pointer, k_type_primitive.
**
**  type_method
**  ===========
**
**  Type_method represents method, function and procedure types.
**
**  fields:
**  -------
**
**  arity        Number of parameters to the procedure. @@@ better n_params
**               A procedure in FIRM has only call by value parameters.
**
**  **param_type A list with the types of parameters.  This list is ordered.
**               The nth type in this list corresponds to the nth element
**               in the parameter tuple that is a result of the start node.
**  	         (See ircons.h for more information.)
**
**  n_res        The number of results of the method.  In general, procedures
**               have zero results, functions one.
**
**  **res_type   A list with the types of parameters.  This list is ordered.
**  	         The nth type in this list corresponds to the nth input to
**  	         Return nodes.  (See ircons.h for more information.)
**
**
**  type_union
**  ==========
**
**  Type_union represents union types.
**
**  fields:
**  -------
**
**  **unioned_type   A list of unioned types.
**
**
**  type_array
**  ==========
**
**  Type_array represents rectangular multi dimensional arrays.
**
**  fields:
**  -------
**
**  n_dimensions     Number of array dimensions.
**
**  *lower_bound     Lower bounds of dimensions.  Mostly all 0.
**
**  *upper_bound     Upper bounds or dimensions.
**
**  *element_type    The type of the array elements.
**
**
**  type_enumeration
**  ================
**
**  Enumeration types.  These need not necessarily be represented explicitly
**  by Firm types, as the frontend can lower them to integer constants as
**  well.  For debugging purposes or similar tasks this information is useful.
**
**  fields:
**  -------
**
**  **enum           The target values representing the constants used to
**                   represent individual enumerations.
**
**  **enum_name      Idents containing the source program name of the enumeration
**  		     constants
**
**  type_pointer
**  ============
**
**  Pointer types.
**
**  fields:
**  -------
**
**  *mode            The mode used to implement a pointer.  @@@ So far this field
**                   is constant and set to mode_P.  Maybe we will move this
**    		     to a global constant (irprog), or are there processors
**  		     that require a set of different pointer modes?
**
**  *points_to       The type of the entity this pointer points to.
**
**  type_primitive
**  ==============
**
**  Primitive types are types that represent indivisible data values that
**  map directly to modes.
**
**  fields:
**  -------
**
**  mode             The mode to be used for this type.
**
*/

# ifndef _TYPE_H_
# define _TYPE_H_

# include "common.h"
# include "ident.h"
# include "irmode.h"
/*CS*/
//# include "entity.h"

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
/* to resolve recursion between entity.h and type.h */
typedef struct entity entity;
#endif

/* for recursive type definiton */
//#ifndef _TYPE_TYPEDEF_
//#define _TYPE_TYPEDEF_
/* to resolve recursion between entity.h and irgraph.h */
typedef union type type;
//#endif


/* visited flag to traverse the type information */
extern unsigned long type_visited;

/*******************************************************************/
/** TYPE_CLASS                                                    **/
/*******************************************************************/

typedef struct type_class type_class;

struct type_class {
  firm_kind kind;
  ident *name;             /* needs list with it's entities
			      does it really??
			      Entities can be added during their creation. */
  struct entity **members;        /* to represent inheritance */
  type_class **subtypes;   /* direct subtypes */
  type_class **supertypes; /* direct supertypes */
  unsigned long visit;     /* visited counter for walks of
			      the type information */
};


/* create a new type_class */
type_class *new_type_class (ident *name);

/* manipulate fields of type_class */

const char  *get_class_name  (type_class *class);
ident       *get_class_ident (type_class *class);

/* Not necessary now!
void   set_class_name  (type_class *class, char *name);
void   set_class_ident (type_class *class, ident* ident);
*/

void    add_class_member (type_class *class, entity *member);
int     get_class_n_member (type_class *class);
entity *get_class_member (type_class *class, int pos);
void    set_class_member (type_class *class, entity *member, int pos);

void        add_class_subtype (type_class *class,  type_class *subtype);
int         get_class_n_subtype (type_class *class);
type_class *get_class_subtype (type_class *class, int pos);
void        set_class_subtype (type_class *class, type_class *subtype, int pos);

void        add_class_supertype (type_class *class, type_class *supertype);
int         get_class_n_supertype (type_class *class);
type_class *get_class_supertype (type_class *class, int pos);
void        set_class_supertype (type_class *class, type_class *supertype, int pos);

/*******************************************************************/
/** TYPE_STRCT                                                   **/
/*******************************************************************/

typedef struct {
  firm_kind kind;
  ident *name;
  entity **members;
  unsigned long visit;     /* visited counter for walks of the type information */
} type_strct;


/* create a new type_strct */
type_strct *new_type_strct (ident *name);

/* manipulate fields of type_strct */
const char  *get_strct_name  (type_strct *strct);
ident       *get_strct_ident (type_strct *strct);

void         add_strct_member (type_strct *strct, entity *member);
int          get_strct_n_member (type_strct *strct);
entity      *get_strct_member (type_strct *strct, int pos);
void         set_strct_member (type_strct *strct, int pos, entity *member);

/*
void   set_strct_name  (type_strct *strct, char *name);
void   set_strct_ident (type_strct *strct, ident* ident);
*/


/*******************************************************************/
/** TYPE_METHOD                                                   **/
/*******************************************************************/

typedef struct {
  firm_kind kind;
  ident *name;         /* Name of the method type.  Usually method
			  types are not explicitly named (but the entity). */
  int arity;           /* number of parameters, better n_params */
  type **param_type;   /* code generation needs this information.
                          Should it be generated by the frontend,
                          or does this impose unnecessary work for
                          optimizations that change the parameters of
                          methods? */
  int n_res;           /* number of results */
  type **res_type;     /* array with result types */
  unsigned long visit; /* visited counter for walks of the type information */
} type_method;

/* Create a new type_method.
   Arity is the number of parameters. */
type_method *new_type_method (ident *name, int arity, int n_res);

/* manipulate fields of type_method */
const char  *get_method_name  (type_method *method);
ident       *get_method_ident (type_method *method);
/*
void   set_method_name  (type_method *method, char *name);
void   set_method_ident (type_method *method, ident* ident); */

inline int   get_method_arity (type_method *method);
/*inline void  set_method_arity (type_method *method, int arity);*/
inline type *get_method_param_type(type_method *method, int pos);
inline void  set_method_param_type(type_method *method, int pos, type* type);

inline int   get_method_n_res (type_method *method);
/*inline void  set_method_n_res (type_method *method, int n_res);*/
inline type *get_method_res_type(type_method *method, int pos);
inline void  set_method_res_type(type_method *method, int pos, type* type);


/*******************************************************************/
/** TYPE_UNION                                                    **/
/*******************************************************************/

typedef struct {
  firm_kind kind;
  ident *name;             /* do I need a name? */
  int n_types;
  /* type **unioned_type;    ... or something like that? */
  unsigned long visit;     /* visited counter for walks of the type information */
} type_union;

/* create a new type_union -- set unioned types by hand. */
type_union *new_type_union (ident *name, int n_types);

/* manipulate fields of type_union */
const char  *get_union_name  (type_union *uni);
ident       *get_union_ident (type_union *uni);
/*
void   set_union_name  (type_union *union, char *name);
void   set_union_ident (type_union *union, ident* ident);
*/
/*
int    get_union_n_types (type_union *union);
void   set_union_n_types (type_union *union, int n);
type  *get_union_unioned_type (type_union *union, int pos);
void   set_union_unioned_type (type_union *union, int pos, type *type);
*/

/*******************************************************************/
/** TYPE_ARRAY                                                    **/
/*******************************************************************/

/* multidimensional, polyhedric arrays */
typedef struct {
  firm_kind kind;
  ident *name;
  int n_dimensions;
  int *lower_bound;
  int *upper_bound;
  type *element_type;
  unsigned long visit;     /* visited counter for walks of the type information */
} type_array;

/* create a new type array -- set dimension sizes independently */
type_array *new_type_array (ident *name, int n_dimensions);

/* manipulate fields of type_array */
const char  *get_array_name  (type_array *array);
ident       *get_array_ident (type_array *array);
/*
void   set_array_name  (type_array *array, char *name);
void   set_array_ident (type_array *array, ident* ident);
*/
void  set_array_n_dimensions  (type_array *array, int n);
int   get_array_n_dimensions  (type_array *array);

void  set_array_bounds      (type_array *array, int dimension, int lower_bound,
                                                              int upper_bound);
void  set_array_lower_bound (type_array *array, int dimension, int lower_bound);
void  set_array_upper_bound (type_array *array, int dimension, int upper_bound);
int   get_array_lower_bound (type_array *array, int dimension);
int   get_array_upper_bound (type_array *array, int dimension);

void  set_array_element_type (type_array *array, type *type);
type *get_array_element_type (type_array *array);

/*******************************************************************/
/** TYPE_ENUMERATION                                              **/
/*******************************************************************/
/** Enums are needed to keep debugging information.  They can as well
    be lowered to integers. **/

typedef struct {
  firm_kind kind;
  ident *name;
  /*
  tarval **enum     * Contains all constant nodes that represent a member
                      of the enum -- enumerators. */
  /*
  ident **enum_name * Contains the names of the enum fields as specified by
                      the source program */
  /* is ir_node the propper array member? */
  unsigned long visit;     /* visited counter for walks of the type information */
} type_enumeration;

/* create a new type enumeration -- set the enumerators independently */
type_enumeration *new_type_enumeration (ident *name /* , int n_enums */);

/* manipulate fields of type_enumeration */
const char  *get_enumeration_name  (type_enumeration *enumeration);
ident       *get_enumeration_ident (type_enumeration *enumeration);
/*
void   set_enumeration_name  (type_enumeration *enumeration, char *name);
void   set_enumeration_ident (type_enumeration *enumeration, ident* ident);
*/
/*
void     set_enumeration_n_enums (type_enumeration *enumeration, int n);
int     *get_enumeration_n_enums (type_enumeration *enumeration);
void     set_enumeration_enum    (type_enumeration *enumeration, int pos,
                                 ir_node const);
ir_node *get_enumeration_enum    (type_enumeration *enumeration, int pos);
*/

/*******************************************************************/
/** TYPE_POINTER                                                  **/
/*******************************************************************/

typedef struct {
  firm_kind kind;
  ident *name;
  /* ir_mode *mode;      * The mode to be used for this type.
                            Not here as there might be several pointer types?
                            A method get_pointer_mode should read a unique,
                            global variable. */
  type *points_to;
  unsigned long visit;     /* visited counter for walks of the type information */
} type_pointer;

/* create a new type pointer */
type_pointer *new_type_pointer (ident *name, type *points_to);

/* manipulate fields of type_pointer */
const char  *get_pointer_name  (type_pointer *pointer);
ident *get_pointer_ident (type_pointer *pointer);
/*
void   set_pointer_name  (type_pointer *pointer, char *name);
void   set_pointer_ident (type_pointer *pointer, ident* ident);
*/
void  set_pointer_points_to_type (type_pointer *pointer, type *type);
type *get_pointer_points_to_type (type_pointer *pointer);

/*******************************************************************/
/** TYPE_PRIMITIVE                                                **/
/*******************************************************************/

/* primitive, language-defined types */
/* What is the type of an entity if it is atomic?  Are alle basic data
   types classses in Sather? Else this is needed. */
typedef struct {
  firm_kind kind;
  ident *name;
  ir_mode *mode;           /* The mode to be used for this type */
  unsigned long visit;     /* visited counter for walks of the type information */
} type_primitive;

/* create a new type primitive */
type_primitive *new_type_primitive (ident *name, ir_mode *mode);

/* manipulate fields of type_primitive */
const char  *get_primitive_name  (type_primitive *primitive);
ident *get_primitive_ident (type_primitive *primitive);
/*
void   set_primitive_name  (type_primitive *primitive, char *name);
void   set_primitive_ident (type_primitive *primitive, ident* ident);
*/
ir_mode *get_primitive_mode (type_primitive *primitive);
void     set_primitive_mode (type_primitive *primitive, ir_mode *mode);




/*******************************************************************/
/**  To manage all different types the same                       **/
/*******************************************************************/

union type {
  firm_kind kind;
  type_class clss;
  type_strct strct;
  type_method method;
  type_array array;
  type_union uni;  /* union is keyword */
  type_enumeration enumeration;
  type_pointer pointer;
  type_primitive primitive;
};


int is_type(void *thing);




# endif /* _TYPE_H_ */
