/**
 *
 * @file type.h
 *
 * Project:     libFIRM                                                   <br>
 * File name:   ir/tr/type.h                                              <br>
 * Purpose:     Representation of types.                                  <br>
 * Author:      Goetz Lindenmaier                                         <br>
 * Modified by:                                                           <br>
 * Created:                                                               <br>
 * Copyright:   (c) 2001-2003 Universität Karlsruhe                       <br>
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE. <br>
 * CVS-ID:      $Id$
 *
 *
 */

# ifndef _TYPE_IDENTIFY_H_
# define _TYPE_IDENTIFY_H_

#ifndef _TYPE_TYPEDEF_
#define _TYPE_TYPEDEF_
typedef struct type type;
#endif

/* ------------------------------------------------------------------------ */

/**  Type for a function that compares two types.
 *
 *   @param tp1  The first type to compare.
 *   @param tp2  The second type to compare.
 */
//typedef int (*compare_types_func_tp) (type *tp1, type *tp2);
typedef int (*compare_types_func_tp) (const void *tp1, const void *tp2);

/** Compares two types by their name.
 *
 * Compares the opcode and the name of the types. If these are
 * equal returns true, else false.
 */
int compare_names (const void *tp1, const void *tp2);

/** Compares two types strict.
 *
 * returns true if tp1 == tp2
 */
int compare_strict (const void *tp1, const void *tp2);

/** A variable that holds a compare function for types.
 *
 *  The compare function is used to identify equal types.  The
 *  variable is initialized with the function compare_strict().
 *
 *  The variable must be set before calling init_firm()! Later changes
 *  have no effect.
 */
extern compare_types_func_tp compare_types_func;


/* ------------------------------------------------------------------------ */

/**  Type for a function that computes a hash value for a type.
 *
 *   @param tp The type to compute a hash for.
 */
typedef int (*hash_types_func_tp)(type *tp);

/** Computes a hash value by the type name.
 *
 * Uses the name of the type and the type opcode to compute the hash.
 */
int hash_name (type *tp);

/** A variable that holds a hash function for a type.
 *
 *  The hash function is used to identify equal types.  The
 *  variable is initialized with the function hash_name().
 *
 *  The variable must be set before calling init_firm()! Later changes
 *  have no effect.
 */
extern hash_types_func_tp hash_types_func;


/* ------------------------------------------------------------------------ */

/** Finalize type construction.
 *
 * Indicate that a type is so far completed that it can be
 * distinguished from other types.  Mature_type hashes the type into a
 * table.  It uses the function in compare_types_func to compare the
 * types.  If it find a type identical to tp it returns this type.  In
 * this case it also turns tp into the Id type.  The caller should free
 * tp if he knows that there are no other references to tp.  The memory
 * of tp is not lost, but will remain alive for an unknown time.
 *
 * @param tp The type to mature.
 */
type *       mature_type(type *tp);

# endif /* _TYPE_IDENTIFY_H_ */
