/**
 *
 * @file tpop.h
 *
 * Project:     libFIRM
 * File name:   ir/tr/tpop.h
 * Purpose:     Opcode of types.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-ID:      $Id$
 *
 *  This module specifies the kinds of types available in firm.
 *
 *  They are called type opcodes. These include classes, structs, methods, unions,
 *  arrays, enumerations, pointers and primitive types.
 *  Special types with own opcodes are the id type, a type representing an unknown
 *  type and a type used to specify that something has no type.
 *
 *  @see type.h
 */

# ifndef _TYPEOP_H_
# define _TYPEOP_H_

#include "ident.h"

/**
 *   An enum for the type kinds.
 *   For each type kind exists a typecode to identify it.
 */
typedef enum {
  tpo_uninitialized = 0,   /* not a type opcode */
  tpo_class,
  tpo_struct,
  tpo_method,
  tpo_union,
  tpo_array,
  tpo_enumeration,
  tpo_pointer,
  tpo_primitive,
  tpo_id,
  tpo_none,
  tpo_unknown,
  tpo_max                  /* not a type opcode */
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
tp_op *get_tpop_class(void);

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
tp_op *get_tpop_struct(void);

/**
 *   This type opcode marks that the corresponding type is a method type.
 *
 *   Consequently it refers to a list of arguments and results.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_method;
tp_op *get_tpop_method(void);

/**
 *   This type opcode marks that the corresponding type is a union type.
 *
 *   Consequently it refers to a list of unioned types.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_union;
tp_op *get_tpop_union(void);

/**
 *   This type opcode marks that the corresponding type is an array type.
 *
 *   Consequently it contains a list of dimensions (lower and upper bounds)
 *   and an element type.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_array;
tp_op *get_tpop_array(void);

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
tp_op *get_tpop_enumeration(void);

/**
 *   This type opcode marks that the corresponding type is a pointer type.
 *
 *   It contains a reference to the type the pointer points to.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_pointer;
tp_op *get_tpop_pointer(void);

/**
 *   This type opcode marks that the corresponding type is a primitive type.
 *
 *   Primitive types are types that are directly mapped to target machine
 *   modes.
 *   This struct is dynamically allocated but constant for the lifetime
 *   of the library.
 */
extern tp_op *type_primitive;
tp_op *get_tpop_primitive(void);

/**
 *   This type opcode is an auxiliary opcode dedicated to support transformations
 *   of the type structure.
 *
 *   If a type is changed to another type with another
 *   opcode the new type will be allocated with new memory.  All nodes refering
 *   to the old type need to be changed to refer to the new one.  This is simplified
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
tp_op *get_tpop_id(void);

/**
 *   This type opcode is an auxiliary opcode dedicated to support type analyses.
 *
 *   Types with this opcode represents that there is no type.
 *   The type can be used to initialize fields of the type* that actually can not
 *   contain a type or that are initialized for an analysis. There exists exactly
 *   one type with this opcode.
 */
extern tp_op *tpop_none;
tp_op *get_tpop_none(void);

/**
 *   This type opcode is an auxiliary opcode dedicated to support type analyses.
 *
 *   Types with this opcode represents that there could be a type, but it is not
 *   known.  This type can be used to initialize fields before an analysis (not known
 *   yet) or to represent the top of a lattice (could not be determined).  There exists
 *   exactly one type with this opcode.
 */
extern tp_op *tpop_unknown;
tp_op *get_tpop_unknown(void);

# endif /*_TYPEOP_H_ */
