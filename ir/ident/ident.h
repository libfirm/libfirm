/* Declarations for ident.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*/

/**
 * @file ident.h
 *
 * Declarations for identifiers in the firm library
 *
 * Identifiers are used in the firm library. This is the interface to it.
 */

/* $Id$ */

# ifndef _IDENT_H_
# define _IDENT_H_

# include <stdio.h>
# include <assert.h>
# include "firm_common.h"

/* Identifiers */

/**
 * Initialises the ident handling.
 *
 * Must be called before any id_*() function can be called.
 */
void id_init(void);

/**
 *  The abstract data type ident.
 *
 *  An ident represents an unique string. The == operator
 *  is sufficient to compare two idents.
 */
typedef const struct set_entry ident;

/**
 *  Store a string and create an ident.
 *
 *  Stores a string in the ident module and returns a handle for the string.
 *
 *  Copies the string. @p str must be zero terminated
 *
 * @param str - the string which shall be stored
 *
 * @return id - a handle for the generated ident
 *
 * @see id_to_str(), id_to_strlen()
 */
ident *new_id_from_str (const char *str);

/**
 *  Store a string and create an ident.
 *
 *  Stores a string in the ident module and returns a handle for the string.
 *
 *  Copies the string. This version can take non-zero-terminated strings
 *
 * @param str - the string (or whatever) which shall be stored
 * @param len - the length of the data in bytes
 *
 * @return id - a handle for the generated ident
 *
 * @see new_id_to_str(), id_to_strlen()
 */
INLINE ident      *id_from_str (const char *str, int len);

/**
 * Returns a string represented by an ident.
 *
 * Returns the string represented by id. This string is
 * NULL terminated. The string may not be changed.
 *
 * @param id - the ident
 *
 * @return cp - a string
 *
 * @see new_id_to_str(), id_from_str(), id_to_strlen()
 */
INLINE const char *id_to_str   (ident *id);

/**
 * Returns the length of the string represented by an ident.
 *
 * @param id - the ident
 *
 * @return len - the length of the string
 *
 * @see new_id_to_str(), id_from_str(), id_to_str()
 */
INLINE int  id_to_strlen(ident *id);

/**
 * Returns true if prefix is a prefix of an ident.
 *
 * @param prefix - the prefix
 * @param id     - the ident
 *
 * @see new_id_to_str(), id_from_str(), id_to_str(), id_is_prefix()
 */
int id_is_prefix (ident *prefix, ident *id);

/**
 * Returns true if suffix is a suffix of an ident.
 *
 * @param suffix - the suffix
 * @param id     - the ident
 *
 * @see new_id_to_str(), id_from_str(), id_to_str(), id_is_prefix()
 */
int id_is_suffix (ident *suffix, ident *id);

/**
 * Return true if an ident contains a given character.
 *
 * @param id     - the ident
 * @param c      - the character
 *
 * @see new_id_to_str(), id_from_str(), id_to_str()
 */
int id_contains_char (ident *id, char c);

/**
 * Prints the ident to stdout.
 *
 * @param id - The ident to be printed.
 *
 * @return
 *    number of btes written
 *
 * @see new_id_to_str(), id_from_str(), id_to_str(), id_is_prefix(), fprint_id()
 */
int print_id (ident *id);

/**
 * Prints the ident to the file passed.
 *
 * @param F  - file pointer to print the ident to.
 * @param id - The ident to print and the file.
 *
 * @return
 *    number of btes written
 *
 * @see new_id_to_str(), id_from_str(), id_to_str(), id_is_prefix(), print_id()
 */
int fprint_id (FILE *F, ident *id);

# endif /* _IDENT_H_ */
