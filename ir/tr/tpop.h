
/* $Id$ */

# ifndef _TYPEOP_H_
# define _TYPEOP_H_

#include "ident.h"

/****h* libfirm/tpop
 *
 * NAME
 *  file tpop.h
 * COPYRIGHT
 *   (C) 2001 by Universitaet Karlsruhe
 * AUTHORS
 *   Goetz Lindenmaier
 * NOTES
 *  This module specifies the kinds of types available in firm.  They are
 *  called type opcodes. These include classes, structs, methods, unions,
 *  arrays, enumerations, pointers and primitive types.
 *****
 */

/****** tpop/typecode
 *
 * NAME
 *   typecode -- an enum for the type kinds
 * PURPOSE
 *   For each type kind exists a typecode to identify it.
 * SOURCE
 */
typedef enum {
  tpo_class,
  tpo_struct,
  tpo_method,
  tpo_union,
  tpo_array,
  tpo_enumeration,
  tpo_pointer,
  tpo_primitive,
  tpo_id
} tp_opcode;
/******/

/****s* tpop/tp_op
 *
 * NAME
 *   tp_op  - a structure containing information about a kind of type.
 * PURPOSE
 *   A structure containing information about a kind of type.  So far
 *   this is only the kind name, an enum for case-switching and some
 *   internal values.
 * SEE ALSO
 *   get_tpop_name, get_tpop_code, get_tpop_ident
 * SOURCE
 */
typedef struct tp_op tp_op;
/******/


/****f* tpop/get_tpop_name
 *
 * NAME
 *   get_tpop_name - Returns the string for the type opcode.
 * SYNOPSIS
 *   const char *get_tpop_name (tp_op *op)
 * INPUTS
 *   op - The type opcode to get the string from.
 * RESULT
 *   a string.  (@@@ Null terminated???)
 ***
 */
const char *get_tpop_name (tp_op *op);

/****f* tpop/get_tpop_code
 *
 * NAME
 *   get_tpop_code - Returns an enum for the type opcode.
 * SYNOPSIS
 *   tp_opcode get_tpop_code (tp_op *op);
 * INPUTS
 *   op - The type opcode to get the enum from.
 * RESULT
 *   the enum.
 ***
 */
tp_opcode get_tpop_code (tp_op *op);

/****f* tpop/get_tpop_ident
 *
 * NAME
 *   get_tpop_ident - Returns the ident for the type opcode.
 * SYNOPSIS
 *   ident *get_tpop_ident (tp_op *op);
 * INPUTS
 *   op - The type opcode to get the ident from.
 * RESULT
 *   The ident.
 ***
 */
ident *get_tpop_ident (tp_op *op);

/****d* tpop/type_class
 *
 * NAME
 *   type_class  -
 * PURPOSE
 *   This type opcode marks that the corresponding type is a class type.
 *   Consequently the type refers to supertypes, subtypes and entities.
 *   Entities can be any fields, but also methods.
 *   @@@ value class or not???
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_class;
tp_op *get_type_class();
/******/

/****d* tpop/type_struct
 *
 * NAME
 *   type_struct
 * PURPOSE
 *   This type opcode marks that the corresponding type is a compound type
 *   as a struct in C.  Consequently the type refers to a list of entities
 *   which may not be methods (but pointers to methods).
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_struct;
tp_op *get_type_struct();
/******/

/****d* tpop/type_method
 *
 * NAME
 *   type_method
 * PURPOSE
 *   This type opcode marks that the corresponding type is a method type.
 *   Consequently it refers to a list of arguments and results.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_method;
tp_op *get_type_method();
/******/

/****d* tpop/type_union
 *
 * NAME
 *   type_union
 * PURPOSE
 *   This type opcode marks that the corresponding type is a union type.
 *   Consequently it refers to a list of unioned types.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_union;
tp_op *get_type_union();
/******/

/****d* tpop/type_array
 *
 * NAME
 *   type_array
 * PURPOSE
 *   This type opcode marks that the corresponding type is an array type.
 *   Consequently it contains a list of dimensions (lower and upper bounds)
 *   and an element type.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_array;
tp_op *get_type_array();
/******/

/****d* tpop/type_enumeration
 *
 * NAME
 *   type_enumeration
 * PURPOSE
 *   This type opcode marks that the corresponding type is an enumeration type.
 *   Consequently it contains a list of idents for the enumeration identifiers
 *   and a list of tarbet values that are the constants used to implement
 *   the enumerators.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_enumeration;
tp_op *get_type_enumeration();
/******/

/****d* tpop/type_pointer
 *
 * NAME
 *   type_pointer
 * PURPOSE
 *   This type opcode marks that the corresponding type is a pointer type.
 *   It contains a reference to the type the pointer points to.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_pointer;
tp_op *get_type_pointer();
/******/

/****d* tpop/type_primitive
 *
 * NAME
 *   type_primitive
 * PURPOSE
 *   This type opcode marks that the corresponding type is a primitive type.
 *   Primitive types are types that are directly mapped to target machine
 *   modes.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_primitive;
tp_op *get_type_primitive();
/******/

/****d* tpop/type_id
 *
 * NAME
 *   type_id
 * PURPOSE
 *   This type opcode is an auxiliary opcode dedicated to support transformations
 *   of the type structure.  If a type is changed to another type with another
 *   opcode the new type will be allocated with new memory.  All nodes refering
 *   to the old type need to be changed to refer the new one.  This is simplified
 *   by turning the old type into an id type that merely forwards to the new type
 *   that now replaces the old one.
 *   type_ids should never be visible out of the type module.  All access routines
 *   should automatically check for type_id and eventually follow the forward in
 *   type_id.  Two types are exchanged by a call to exchange_types.
 *   If a type_id is visible externally report this as bug.  If it is assured that
 *   this never happens this extern variable can be moved to tpop_t.h.
 * NOTES
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 * SOURCE
 */
extern tp_op *type_id;
tp_op *get_type_id();
/******/

# endif /*_TYPEOP_H_ */
