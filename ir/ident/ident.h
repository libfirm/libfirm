/*
 * Project:     libFIRM
 * File name:   ir/common/ident_t.h
 * Purpose:     Data type for unique names.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
/**
 * @file ident.h
 *
 * Declarations for identifiers in the firm library
 *
 * Identifiers are used in the firm library. This is the interface to it.
 */


# ifndef _IDENT_H_
# define _IDENT_H_

# include <stdio.h>
# include <assert.h>
# include "firm_common.h"

/* Identifiers */

/**
 *  The abstract data type ident.
 *
 *  An ident represents an unique string. The == operator
 *  is sufficient to compare two idents.
 */
typedef const struct ident ident;

/**
 * The ident module interface.
 */
typedef struct _ident_if_t {
  /**
   * Store a string and create an ident.
   * This function may be NULL, new_id_from_chars()
   * is then used to emulate it's behavior.
   *
   * @param str - the string which shall be stored
   */
  ident *(*new_id_from_str)(void *handle, const char *str);

  /**
   * Store a string and create an ident.
   *
   * @param str - the string (or whatever) which shall be stored
   * @param len - the length of the data in bytes
   */
  ident *(*new_id_from_chars)(void *handle, const char *str, int len);

  /**
   * Returns a string represented by an ident.
   */
  const char *(*get_id_str)(void *handle, ident *id);

  /**
   * Returns the length of the string represented by an ident.
   * This function may be NULL, get_id_str() is then used
   * to emulate it's behavior.
   *
   * @param id - the ident
   */
  int  (*get_id_strlen)(void *handle, ident *id);

  /**
   * Finish the ident module and frees all idents, may be NULL.
   */
  void (*finish_ident)(void *handle);

  /** The handle. */
  void *handle;

} ident_if_t;

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
 * @see get_id_str(), get_id_strlen()
 */
ident *new_id_from_str (const char *str);

/** Store a string and create an ident.
 *
 * Stores a string in the ident module and returns a handle for the string.
 * Copies the string. This version takes non-zero-terminated strings.
 *
 * @param str - the string (or whatever) which shall be stored
 * @param len - the length of the data in bytes
 *
 * @return id - a handle for the generated ident
 *
 * @see new_id_from_str(), get_id_strlen()
 */
ident *new_id_from_chars (const char *str, int len);

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
 * @see new_id_from_str(), new_id_from_chars(), get_id_strlen()
 */
const char *get_id_str  (ident *id);

/**
 * Returns the length of the string represented by an ident.
 *
 * @param id - the ident
 *
 * @return len - the length of the string
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str()
 */
int  get_id_strlen(ident *id);
/**
 * Returns true if prefix is a prefix of an ident.
 *
 * @param prefix - the prefix
 * @param id     - the ident
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
int id_is_prefix (ident *prefix, ident *id);

/**
 * Returns true if suffix is a suffix of an ident.
 *
 * @param suffix - the suffix
 * @param id     - the ident
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
int id_is_suffix (ident *suffix, ident *id);

/**
 * Returns true if infix is contained in id.  (Can be suffix or prefix)
 *
 * @param infix  - the infix
 * @param id     - the ident to search in
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
/* int id_contains(ident *infix, ident *id); */

/**
 * Return true if an ident contains a given character.
 *
 * @param id     - the ident
 * @param c      - the character
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str()
 */
int id_contains_char (ident *id, char c);

/**
 * Prints the ident to stdout.
 *
 * @param id - The ident to be printed.
 *
 * @return
 *    number of bytes written
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix(), fprint_id()
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
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix(), print_id()
 */
int fprint_id (FILE *F, ident *id);

# endif /* _IDENT_H_ */
