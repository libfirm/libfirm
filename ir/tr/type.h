/****h* libfirm/type
 *
 * NAME
 *   file type.h - datastructure to hold type information.
 * COPYRIGHT
 *  (C) 2001 by Universitaet Karlsruhe
 * AUTHORS
 *  Goetz Lindenmaier
 *
 * NOTES
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
 *
 * SEE ALSO
 *   tpop.h
 *****
 */
# ifndef _TYPE_H_
# define _TYPE_H_

# include "tpop.h"
# include "common.h"
# include "ident.h"
# include "irmode.h"
# include "bool.h"

#ifndef _ENTITY_TYPEDEF_
#define _ENTITY_TYPEDEF_
/* to resolve recursion between entity.h and type.h */
typedef struct entity entity;
#endif

/****s* type/type
 *
 * NAME
 *   type - An abstract data type to represent types.
 * NOTE
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
 *
 *  The following describes the common attributes.  They can only be
 *  accessed by the functions given below.
 *
 * ATTRIBUTES
 *  The common fields are:
 *
 *  firm_kind    A firm_kind tag containing k_type.  This is useful
 *               for dynamically checking whether a node is a type node.
 *  type_op      A tp_op specifying the kind of the type.
 *  mode         The mode to be used to represent the type on a machine.
 *               @@@ maybe not global field??
 *  name         An identifier specifying the name of the type.  To be
 *               set by the frontend.
 *  size         The size of the type, i.e. an entity of this type will
 *               occupy size bytes in memory.  In several cases this is
 *               determined when fixing the layout of this type (class,
 *               struct, union, array, enumeration).
 *  visit        A counter for walks of the type information.
 *
 *  These fields can only be accessed via access functions.
 *
 *  Depending on the value of type_op, i.e., depending on the kind of the
 *  type the adt contains further attributes.  These are documented below.
 * SEE ALSO
 *   class, struct, method, union, array, enumeration, pointer, primitive
 * SOURCE
 */
typedef struct type type;

tp_op*      get_type_tpop(type *tp);
ident*      get_type_tpop_nameid(type *tp);
const char* get_type_tpop_name(type *tp);
tp_opcode   get_type_tpop_code(type *tp);

ir_mode*    get_type_mode(type *tp);
void        set_type_mode(type *tp, ir_mode* m);

ident*      get_type_nameid(type *tp);
void        set_type_nameid(type *tp, ident* id);
const char* get_type_name(type *tp);

int         get_type_size(type *tp);
void        set_type_size(type *tp, int size);

unsigned long get_type_visited(type *tp);
void        set_type_visited(type *tp, unsigned long num);
/* Sets visited field in type to type_visited. */
void        mark_type_visited(type *tp);
/*****/

/****v* type/visited
 *
 * NAME
 *   type_visited -  visited flag to traverse the type information
 * PURPOSE
 *   Increase this flag by one before traversing the type information.
 *   Mark type nodes as visited by set_type_visited(type, type_visited).
 *   Check whether node was already visited by comparing get_type_visited(type)
 *   and type_visited.
 *   Or use the function to walk all types.
 * SEE ALSO
 *   typewalk
 * SOURCE
 */
extern unsigned long type_visited;
/*****/

/****f* type/is_type
 *
 * NAME
 *   is_type - Checks whether a pointer points to a type.
 * SYNOPSIS
 *   bool is_type            (void *thing);
 * INPUTS
 *   thing - a pointer
 * RESULT
 *   true if the thing is a type, else false
 ***
 */
int is_type            (void *thing);

/****** type/class
 * NAME
 *  Representation of a class type.
 * NOTE
 *  If the type opcode is set to type_class the type represents class
 *  types.  A list of fields and methods is associated with a class.
 *  Further a class can inherit from and bequest to other classes.
 *  @@@ value class???
 * ATTRIBUTES
 *  The following attributes are private to this type kind.
 *  member     All entities belonging to this class.  This are methode entities
 *             which have type_method or fields that can have any of the
 *             following type kinds: type_class, type_struct, type_union,
 *             type_array, type_enumeration, type_pointer, type_primitive.
 *
 *  subtypes   A list of direct subclasses.
 *
 *  supertypes A list of direct superclasses.
 *
 *  These are dynamic lists that can be grown with an "add_" function,
 *  but not shrinked.
 * SOURCE
 */
/* create a new class type */
type   *new_type_class (ident *name);

/* manipulate private fields of class type  */
void    add_class_member   (type *clss, entity *member);
int     get_class_n_member (type *clss);
entity *get_class_member   (type *clss, int pos);
void    set_class_member   (type *clss, entity *member, int pos);

void    add_class_subtype   (type *clss, type *subtype);
int     get_class_n_subtype (type *clss);
type   *get_class_subtype   (type *clss, int pos);
void    set_class_subtype   (type *clss, type *subtype, int pos);

void    add_class_supertype   (type *clss, type *supertype);
int     get_class_n_supertype (type *clss);
type   *get_class_supertype   (type *clss, int pos);
void    set_class_supertype   (type *clss, type *supertype, int pos);

/* typecheck */
bool    is_class_type(type *clss);
/*****/

/****** type/struct
 * NAME
 *  Representation of a struct type.
 * NOTE
 *  Type_strct represents aggregate types that consist of a list
 *  of fields.
 * ATTRIBUTES
 *  member   All entities belonging to this class.  This are the fields
 *           that can have any of the following types:  type_class,
 *           type_struct, type_union, type_array, type_enumeration,
 *  	     type_pointer, type_primitive.
 *           This is a dynamic list that can be grown with an "add_" function,
 *           but not shrinked.
 *           This is a dynamic list that can be grown with an "add_" function,
 *           but not shrinked.
 * SOURCE
 */
/* create a new type struct */
type   *new_type_struct (ident *name);

/* manipulate private fields of struct */
void    add_struct_member   (type *strct, entity *member);
int     get_struct_n_member (type *strct);
entity *get_struct_member   (type *strct, int pos);
void    set_struct_member   (type *strct, int pos, entity *member);

/* typecheck */
bool    is_struct_type(type *strct);
/*****/

/****** type/method
 * NAME
 *  Representation of a method type.
 * NOTE
 *  A method type represents a method, function or procedure type.
 *  It contains a list of the parameter and result types, as these
 *  are part of the type description.  These lists should not
 *  be changed by a optimization, as a change creates a new method
 *  type.  Therefore optimizations should allocated new method types.
 *  The set_ routines are only for construction by a frontend.
 * ATTRIBUTES
 *  n_params    Number of parameters to the procedure.
 *              A procedure in FIRM has only call by value parameters.
 *
 *  param_type  A list with the types of parameters.  This list is ordered.
 *              The nth type in this list corresponds to the nth element
 *              in the parameter tuple that is a result of the start node.
 *  	        (See ircons.h for more information.)
 *
 *  n_res       The number of results of the method.  In general, procedures
 *              have zero results, functions one.
 *
 *  res_type    A list with the types of parameters.  This list is ordered.
 *              The nth type in this list corresponds to the nth input to
 *  	        Return nodes.  (See ircons.h for more information.)
 * SOURCE
 */

/* Create a new method type.
   N_param is the number of parameters, n_res the number of results.
   The arrays for the parameter and result types are not initialized by
   the constructor. */
type *new_type_method (ident *name, int n_param, int n_res);

/* manipulate private fields of method. */
int   get_method_n_params  (type *method);
type *get_method_param_type(type *method, int pos);
void  set_method_param_type(type *method, int pos, type* type);

int   get_method_n_res   (type *method);
type *get_method_res_type(type *method, int pos);
void  set_method_res_type(type *method, int pos, type* type);

/* typecheck */
bool  is_method_type     (type *method);
/*****/

/****** type/union
 * NAME
 *   Representation of a union type.
 * NOTE
 *   The union type represents union types.
 * ATTRIBUTES
 *   n_types        Number of unioned types.
 *   members        Entities for unioned types.  Fixed length array.
 *                  This is a dynamic list that can be grown with an "add_" function,
 *                  but not shrinked.
 * SOURCE
 */
/* create a new type union  */
type   *new_type_union (ident *name);

/* manipulate private fields of struct */
int     get_union_n_members      (type *uni);
void    add_union_member (type *uni, entity *member);
entity *get_union_member (type *uni, int pos);
void    set_union_member (type *uni, int pos, entity *member);

/* typecheck */
bool    is_union_type          (type *uni);
/*****/

#if 0
/* We don't need these if the union has entities, which it now
   does. The entities are necessary for the analysis algorithms. */
type  *get_union_unioned_type (type *uni, int pos);
void   set_union_unioned_type (type *uni, int pos, type *type);

ident *get_union_delim_nameid (type *uni, int pos);
const char *get_union_delim_name (type *uni, int pos);
void   set_union_delim_nameid (type *uni, int pos, ident *id);
#endif

/****** type/array
 * NAME
 *   Representation of an array type.
 * NOTE
 *   The array type represents rectangular multi dimensional arrays.
 * ATTRIBUTES
 *   n_dimensions     Number of array dimensions.
 *   *lower_bound     Lower bounds of dimensions.  Usually all 0.
 *   *upper_bound     Upper bounds or dimensions.
 *   *element_type    The type of the array elements.
 *   *element_ent     An entity for the array elements to be used for
 *                    element selection with Sel.
 * SOURCE
 */
/* create a new type array --
   Set dimension sizes after call to constructor with set_* routines.
   Entity for array elements is built automatically. */
type *new_type_array         (ident *name, int n_dimensions,
			      type *element_type);

/* manipulate private fields of array type */
int   get_array_n_dimensions (type *array);
void  set_array_bounds       (type *array, int dimension, int lower_bound,
                                                          int upper_bound);
void  set_array_lower_bound  (type *array, int dimension, int lower_bound);
void  set_array_upper_bound  (type *array, int dimension, int upper_bound);
int   get_array_lower_bound  (type *array, int dimension);
int   get_array_upper_bound  (type *array, int dimension);

void  set_array_element_type (type *array, type *type);
type *get_array_element_type (type *array);

void  set_array_element_entity (type *array, entity *ent);
entity *get_array_element_entity (type *array);

/* typecheck */
bool   is_array_type         (type *array);
/*****/

/****** type/enumeration
 * NAME
 *  Representation of an enumeration type.
 * NOTE
 *  Enumeration types need not necessarily be represented explicitly
 *  by Firm types, as the frontend can lower them to integer constants as
 *  well.  For debugging purposes or similar tasks this information is useful.
 * ATTRIBUTES
 *   *enum           The target values representing the constants used to
 *                   represent individual enumerations.
 *   *enum_nameid    Idents containing the source program name of the enumeration
 *  		     constants
 *
*****
*/
/* create a new type enumeration -- set the enumerators independently */
type   *new_type_enumeration    (ident *name, int n_enums);

/* manipulate fields of enumeration type. */
int     get_enumeration_n_enums (type *enumeration);

void    set_enumeration_enum    (type *enumeration, int pos, tarval *con);
tarval *get_enumeration_enum    (type *enumeration, int pos);

void    set_enumeration_nameid  (type *enumeration, int pos, ident *id);
ident  *get_enumeration_nameid  (type *enumeration, int pos);
const char *get_enumeration_name(type *enumeration, int pos);

/* typecheck */
bool    is_enumeration_type     (type *enumeration);
/*****/

/****** type/pointer
 * NAME
 *   Representation of a pointer type.
 * NOTE
 *   Pointer types.
 * ATTRIBUTES
 *   points_to       The type of the entity this pointer points to.
 * SOURCE
 */
/* Create a new type pointer */
type *new_type_pointer           (ident *name, type *points_to);

/* manipulate fields of type_pointer */
void  set_pointer_points_to_type (type *pointer, type *type);
type *get_pointer_points_to_type (type *pointer);

/* typecheck */
bool  is_pointer_type            (type *pointer);
/*****/

/****** type/primitive
 * NAME
 *   Representation of a primitive type.
 * NOTE
 *   Primitive types are types that represent indivisible data values that
 *   map directly to modes.  They don't have a private attribute.  The
 *   important information they carry is held in the common mode field.
 * SOURCE
*/
/* create a new type primitive */
type *new_type_primitive (ident *name, ir_mode *mode);

/* typecheck */
bool  is_primitive_type  (type *primitive);
/*****/

# endif /* _TYPE_H_ */
