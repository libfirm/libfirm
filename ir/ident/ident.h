/* Declarations for ident.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
*/

/* $Id$ */

# ifndef _IDENT_H_
# define _IDENT_H_

# include "assert.h"

/****h* libfirm/ident
 *
 * NAME
 *   ident -- identifiers in the firm library
 * NOTES
 *  Identifiers are used in the firm library. This is the interface to it.
 *
 ******
 */

/* Identifiers */
/****s* ident/ident
 *
 * NAME
 *  ident - the abstract data type ident
 * SOURCE
 */
typedef const struct set_entry ident;
/*****/

/****f* ident/id_from_str
 *
 * NAME
 *  id_from_str - store a string and create an ident
 * SYNOPSIS
 *  ident *id = id_from_str (char *str, int len);
 * FUNCTION
 *  Stores a string in the ident module and returns a handle for the string.
 * INPUTS
 *  str - the string (or whatever) which shall be stored
 *  len - the length of the data in bytes
 * RESULT
 *  id - a handle for the generated ident
 * SEE ALSO
 *  id_to_str, id_to_strlen
 ***
 */
inline ident      *id_from_str (char *str, int len);

/****f* ident/id_to_str
 *
 * NAME
 *  id_to_str - return a string represented by an ident
 * SYNOPSIS
 *  char *cp = id_to_str (ident *id);
 * FUNCTION
 *  Returns the string cp represented by id. This string cp is not
 *  Null terminated!
 * INPUTS
 *  id - the ident
 * RESULT
 *  cp - a string
 * SEE ALSO
 *  id_from_str, id_to_strlen
 ***
 */
inline const char *id_to_str   (ident *id);

/****f* ident/id_to_strlen
 *
 * NAME
 *  id_to_strlen - return the length of a string represented by an ident
 * SYNOPSIS
 *  int len = id_to_strlen (ident *id);
 * FUNCTION
 *  Returns the length of string represented by id.
 * INPUTS
 *  id - the ident
 * RESULT
 *  len - the length of the string
 * SEE ALSO
 *  id_from_str, id_to_str
 ***
 */
inline int         id_to_strlen(ident *id);

# endif /* _IDENT_H_ */
