/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer &
**          Goetz Lindenmaier
**
** type.c: datastructures to hold type information.
*/

# include "type.h"
# include "irprog.h"  /* So that constructors can add the type to global
			 data structure. */
# include "array.h"
# include "ident_t.h"

unsigned long type_visited = 0;

void
init (void)
{
}

/*******************************************************************/
/** TYPE_CLASS                                                    **/
/*******************************************************************/

type_class *
new_type_class (ident *name)//, int members)
{
  type_class *res;

  res = (type_class *) xmalloc (sizeof (type_class));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_class;
  res->name = name;

  res->members = NEW_ARR_F (entity *, 1);
  res->subtypes = NEW_ARR_F (type_class *, 1);
  res->supertypes = NEW_ARR_F (type_class *, 1);

  res->visit = 0;

  return res;
}

/* manipulate fields of type_class */
const char *
get_class_name  (type_class *class) {
  assert(class);
  return id_to_str(class->name);
}

/* field: ident */
ident *
get_class_ident (type_class *class) {
  assert(class);
  return class->name;
}

/*
void   set_class_name  (type_class *class, char *name);
void   set_class_ident (type_class *class, ident* ident);
*/

/* field: member */
void
add_class_member (type_class *class, entity *member)
{
  ARR_APP1 (entity *, class->members, member);
}

entity *
get_class_member (type_class *class, int pos)
{
  assert (class);
  return class->members[pos+1];
}

void
set_class_member (type_class *class, entity *member, int pos)
{
  class->members[pos+1] = member;
}

int
get_class_n_member (type_class *class)
{
  int res;

  assert(class);
  res = (ARR_LEN (class->members))-1;
  return res;
}

/* field: subtype */
void
add_class_subtype (type_class *class,  type_class *subtype)
{
  ARR_APP1 (type_class *, class->subtypes, subtype);
}

type_class *
get_class_subtype (type_class *class, int pos)
{
  assert (class);
  return class->subtypes[pos+1];
}

void
set_class_subtype (type_class *class, type_class *subtype, int pos)
{
  class->subtypes[pos+1] = subtype;
}

int
get_class_n_subtype (type_class *class)
{
  assert(class);
  return (ARR_LEN (class->subtypes))-1;
}

/* field: supertype */
void
add_class_supertype (type_class *class, type_class *supertype)
{
  ARR_APP1 (type_class *, class->supertypes, supertype);
}

type_class *
get_class_supertype (type_class *class, int pos)
{
  assert (class);
  return class->supertypes[pos+1];
}

void
set_class_supertype (type_class *class, type_class *supertype, int pos)
{
  class->supertypes[pos+1] = supertype;
}

int
get_class_n_supertype (type_class *class)
{
  assert(class);
  return (ARR_LEN (class->supertypes))-1;
}

/*******************************************************************/
/** TYPE_STRCT                                                   **/
/*******************************************************************/

type_strct *
new_type_strct (ident *name)//, int members)
{
  type_strct *res;

  res = (type_strct *) xmalloc (sizeof (type_strct));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_strct;
  res->name = name;

  res->members = NEW_ARR_F (entity *, 1);
  res->visit = 0;

  return res;
}

/* manipulate fields of type_strct */

const char *
get_strct_name  (type_strct *strct) {
  assert(strct);
  return ID_TO_STR(strct->name);
}


ident *
get_strct_ident (type_strct *strct) {
  assert(strct);
  return strct->name;
}

int
get_strct_n_member (type_strct *strct)
{
  int res;

  assert(strct);
  res = (ARR_LEN (strct->members))-1;
  return res;
}

void
add_strct_member (type_strct *strct, entity *member)
{
  ARR_APP1 (type_strct *, strct->members, member);
}

entity *
get_strct_member (type_strct *strct, int pos)
{
  assert (strct);
  return strct->members[pos+1];
}

void
set_strct_member (type_strct *strct, int pos, entity *member)
{
  strct->members[pos+1] = member;
}

/*
void   set_strct_name  (type_strct *strct, char *name);
void   set_strct_ident (type_strct *strct, ident* ident);
*/


/*******************************************************************/
/** TYPE_METHOD                                                   **/
/*******************************************************************/

/* create a new type_method */
type_method *
new_type_method (ident *name, int arity, int n_res)
{
  type_method *res;

  res = (type_method *) xmalloc (sizeof (type_method));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_method;

  res->name = name;   // do I need the name, or is the name in entity sufficient?
  res->arity = arity;
  res->param_type = (type **) xmalloc (sizeof (type *) * arity);
  res->n_res  = n_res;
  res->res_type = (type **) xmalloc (sizeof (type *) * n_res);

  res->visit = 0;

  return res;
}

/* manipulate fields of type_method */
const char *
get_method_name  (type_method *method) {
  assert(method);
  return ID_TO_STR(method->name);
}

ident *
get_method_ident (type_method *method) {
  assert(method);
  return method->name;
}

/*
void   set_method_name  (type_method *method, char *name);
void   set_method_ident (type_method *method, ident* ident);
*/


inline int
get_method_n_params (type_method *method) {
  return method->arity;
}

inline int
get_method_arity (type_method *method) {
  return method->arity;
}

/*
inline void
set_method_arity (type_method *method, int arity) {
  method->arity = arity;
  / change array size, somehow copy.  *
}
*/

inline type *
get_method_param_type(type_method *method, int pos) {
  return method->param_type[pos];
}

inline void
set_method_param_type(type_method *method, int pos, type* type) {
  method->param_type[pos] = type;
}


inline int
get_method_n_res (type_method *method) {
  return method->n_res;
}

/*
inline void
set_method_n_res (type_method *method, int n_res) {
  method->n_res = n_res;
}
*/

inline type *
get_method_res_type(type_method *method, int pos) {
  return method->res_type[pos];
}

inline void
set_method_res_type(type_method *method, int pos, type* type) {
  method->res_type[pos] = type;
}


/*******************************************************************/
/** TYPE_UNION                                                    **/
/*******************************************************************/

/* create a new type_union -- set unioned types by hand. */
type_union *
new_type_union (ident *name, int n_types)
{
  type_union *res;

  res = (type_union *) xmalloc (sizeof (type_union));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_union;
  res->name = name;   // do I need a name?
  res->n_types = n_types;
  /*
  res->unioned_type = (int *) xmalloc (sizeof (int) * n_types);
  */

  res->visit = 0;

  return res;
}

/* manipulate fields of type_union */
/*
char *
get_union_name  (type_union *uni) {
  assert(uni);
  return ID_TO_STR(uni->name);
}
*/

ident *
get_union_ident (type_union *uni) {
  assert(uni);
  return uni->name;
}

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

/* create a new type_array */
inline type_array *
new_type_array (ident *name, int n_dimensions)
{
  type_array *res;

  res = (type_array *) xmalloc (sizeof (type_array));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_array;
  res->name = name;
  res->n_dimensions = n_dimensions;
  res->lower_bound = (int *) xmalloc (sizeof (int) * n_dimensions);
  res->upper_bound = (int *) xmalloc (sizeof (int) * n_dimensions);

  res->visit = 0;

  return res;
}

/* manipulate fields of type_array */
/*
char *
get_array_name  (type_array *array) {
  assert(array);
  return ID_TO_STR(array->name);
}
*/

ident *
get_array_ident (type_array *array) {
  assert(array);
  return array->name;
}

/*
void   set_array_name  (type_array *array, char *name);
void   set_array_ident (type_array *array, ident* ident);
*/

inline void
set_array_dimensions (type_array* array, int n) {
  array->n_dimensions = n;
}

inline int
get_array_dimensions (type_array* array) {
  return array->n_dimensions;
}

inline void
set_array_bounds (type_array* array, int dimension, int lower_bound,
		  int upper_bound) {
  array->lower_bound[dimension-1] = lower_bound;
  array->upper_bound[dimension-1] = upper_bound;
}

inline void
set_array_lower_bound (type_array* array, int dimension, int lower_bound) {
  array->lower_bound[dimension-1] = lower_bound;
}

inline void
set_array_upper_bound (type_array* array, int dimension, int upper_bound) {
  array->upper_bound[dimension-1] = upper_bound;
}

inline int
get_array_lower_bound (type_array* array, int dimension) {
  return array->lower_bound[dimension-1];
}

inline int
get_array_upper_bound (type_array* array, int dimension) {
  return array->upper_bound[dimension-1];
}

inline void set_array_element_type (type_array *array, type *type) {
  array->element_type = type;
}

inline type *
get_array_element_type (type_array *array) {
  return array->element_type;
}


/*******************************************************************/
/** TYPE_ENUMERATION                                              **/
/*******************************************************************/

/* create a new type enumeration -- set the enumerators independently */
type_enumeration *
new_type_enumeration (ident *name /* , int n_enums */)
{
  type_enumeration *res;

  res = (type_enumeration *) xmalloc (sizeof (type_enumeration));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_enumeration;
  res->name = name;
  /*
  res->n_enums = n_enums;
  res->enum = (int *) xmalloc (sizeof (int) * n_enums);
  */

  res->visit = 0;

  return res;
}

/* manipulate fields of type_enumeration */
/*
char *
get_enumeration_name  (type_enumeration *enumeration) {
  assert(enumeration);
  return ID_TO_STR(enumeration->name);
}
*/

ident *
get_enumeration_ident (type_enumeration *enumeration) {
  assert(enumeration);
  return enumeration->name;
}

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

/* create a new type pointer */
type_pointer *
new_type_pointer (ident *name, type *points_to)
{
  type_pointer *res;

  res = (type_pointer *) xmalloc (sizeof (type_pointer));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_pointer;
  res->name = name;
  res->points_to = points_to;

  res->visit = 0;

  return res;
}

/* manipulate fields of type_pointer */
/*
char *
get_pointer_name  (type_pointer *pointer) {
  assert(pointer);
  return ID_TO_STR(pointer->name);
}
*/

ident *
get_pointer_ident (type_pointer *pointer) {
  assert(pointer);
  return pointer->name;
}

/*
void   set_pointer_name  (type_pointer *pointer, char *name);
void   set_pointer_ident (type_pointer *pointer, ident* ident);
*/

inline void
set_pointer_points_to_type (type_pointer *pointer, type* type) {
  pointer->points_to = type;
}

inline type *
get_pointer_points_to_type (type_pointer *pointer) {
  return pointer->points_to;
}


/*******************************************************************/
/** TYPE_PRIMITIVE                                                **/
/*******************************************************************/

/* create a new type_primitive */
inline type_primitive *
new_type_primitive (ident *name, ir_mode *mode)
{
  type_primitive *res;

  res = (type_primitive *) xmalloc (sizeof (type_primitive));
  add_irp_type((type *) res);   /* Remember the new type global. */
  res->kind = k_type_primitive;
  res->name = name;
  res->mode = mode;

  res->visit = 0;

  return res;
}

/* manipulate fields of type_primitive */

const char  *
get_primitive_name  (type_primitive *primitive) {
  assert(primitive);
  return ID_TO_STR(primitive->name);
}


ident *
get_primitive_ident (type_primitive *primitive) {
  assert(primitive);
  return primitive->name;
}
/*
void   set_primitive_name  (type_primitive *primitive, char *name);
void   set_primitive_ident (type_primitive *primitive, ident* ident);
*/

inline ir_mode *
get_primitive_mode (type_primitive *primitive) {
  return primitive->mode;
}

inline void
set_primitive_mode (type_primitive *primitive, ir_mode *mode) {
  primitive->mode = mode;
}




/*******************************************************************/
/**  To manage all different types the same                       **/
/*******************************************************************/


int
is_type(void *thing) {
  firm_kind kind;

  kind = get_kind(thing);
  if (   (kind == k_type_class)
      || (kind == k_type_strct)
      || (kind == k_type_method)
      || (kind == k_type_union)
      || (kind == k_type_array)
      || (kind == k_type_enumeration)
      || (kind == k_type_pointer)
      || (kind == k_type_primitive))
    return 1;
  else
    return 0;
}

int
is_type_class(void *thing) {
  if (get_kind(thing) == k_type_class) return 1;
  else return 0;
}

int
is_type_strct(void *thing) {
  if (get_kind(thing) == k_type_strct) return 1;
  else return 0;
}

int
is_type_method(void *thing) {
  if (get_kind(thing) == k_type_method) return 1;
  else return 0;
}

int
is_type_union(void *thing) {
  if (get_kind(thing) == k_type_union) return 1;
  else return 0;
}

int
is_type_array(void *thing) {
  if (get_kind(thing) == k_type_array) return 1;
  else return 0;
}

int
is_type_pointer(void *thing) {
  if (get_kind(thing) == k_type_pointer) return 1;
  else return 0;
}

int
is_type_enumeration(void *thing) {
  if (get_kind(thing) == k_type_enumeration) return 1;
  else return 0;
}

int
is_type_primitive(void *thing) {
  if (get_kind(thing) == k_type_primitive) return 1;
  else return 0;
}
