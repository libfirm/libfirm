/* (C) 2001 by Universitaet Karlsruhe */

/* $Id$ */

# ifndef _TYPEOP_H_
# define _TYPEOP_H_

#include "ident.h"

/**
 *  @file tpop.h
 *
 *  This module specifies the kinds of types available in firm.
 *
 *  @author  Goetz Lindenmaier
 *
 *  They are called type opcodes. These include classes, structs, methods, unions,
 *  arrays, enumerations, pointers and primitive types.
 */

/**
 *   an enum for the type kinds.
 *   For each type kind exists a typecode to identify it.
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

/**
 *   A structure containing information about a kind of type.
 *   A structure containing information about a kind of type.  So far
 *   this is only the kind name, an enum for case-switching and some
 *   internal values.
 *
 * @see  get_tpop_name(), get_tpop_code(), get_tpop_ident()
 */
typedef struct tp_op tp_op;


/**
 *   Returns the string for the type opcode.
 *
 *   @param op  The type opcode to get the string from.
 *   @return a string.  (@todo Null terminated???)
 */
const char *get_tpop_name (tp_op *op);

/**
 *   Returns an enum for the type opcode.
 *
 *   @param op   The type opcode to get the enum from.
 *   @return the enum.
 */
tp_opcode get_tpop_code (tp_op *op);

/**
 *   Returns the ident for the type opcode.
 *
 *   @param op   The type opcode to get the ident from.
 *   @return The ident.
 */
ident *get_tpop_ident (tp_op *op);

/**
 *   This type opcode marks that the corresponding type is a class type.
 *
 *   Consequently the type refers to supertypes, subtypes and entities.
 *   Entities can be any fields, but also methods.
 *   @@@ value class or not???
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_class;
tp_op *get_type_class(void);

/**
 *   This type opcode marks that the corresponding type is a compound type
 *   as a struct in C.
 *
 *   Consequently the type refers to a list of entities
 *   which may not be methods (but pointers to methods).
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_struct;
tp_op *get_type_struct(void);

/**
 *   This type opcode marks that the corresponding type is a method type.
 *
 *   Consequently it refers to a list of arguments and results.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_method;
tp_op *get_type_method(void);

/**
 *   This type opcode marks that the corresponding type is a union type.
 *
 *   Consequently it refers to a list of unioned types.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_union;
tp_op *get_type_union(void);

/**
 *   This type opcode marks that the corresponding type is an array type.
 *
 *   Consequently it contains a list of dimensions (lower and upper bounds)
 *   and an element type.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_array;
tp_op *get_type_array(void);

/**
 *   This type opcode marks that the corresponding type is an enumeration type.
 *
 *   Consequently it contains a list of idents for the enumeration identifiers
 *   and a list of tarbet values that are the constants used to implement
 *   the enumerators.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_enumeration;
tp_op *get_type_enumeration(void);

/**
 *   This type opcode marks that the corresponding type is a pointer type.
 *
 *   It contains a reference to the type the pointer points to.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_pointer;
tp_op *get_type_pointer(void);

/**
 *   This type opcode marks that the corresponding type is a primitive type.
 *
 *   Primitive types are types that are directly mapped to target machine
 *   modes.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_primitive;
tp_op *get_type_primitive(void);

/**
 *   This type opcode is an auxiliary opcode dedicated to support transformations
 *   of the type structure.
 *
 *   If a type is changed to another type with another
 *   opcode the new type will be allocated with new memory.  All nodes refering
 *   to the old type need to be changed to refer the new one.  This is simplified
 *   by turning the old type into an id type that merely forwards to the new type
 *   that now replaces the old one.
 *   type_ids should never be visible out of the type module.  All access routines
 *   should automatically check for type_id and eventually follow the forward in
 *   type_id.  Two types are exchanged by a call to exchange_types.
 *   If a type_id is visible externally report this as bug.  If it is assured that
 *   this never happens this extern variable can be moved to tpop_t.h.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_id;
tp_op *get_type_id(void);

# endif /*_TYPEOP_H_ */
