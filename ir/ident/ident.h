/* Declarations for ident.
   Copyright (C) 1995, 1996 Markus Armbruster */

/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
*/

# ifndef _IDENT_H_
# define _IDENT_H_

# include "assert.h"

/* Identifiers */
typedef const struct set_entry ident;

inline ident      *id_from_str (char *str, int len);
inline const char *id_to_str   (ident *id);
inline int         id_to_strlen(ident *id);

# endif /* _IDENT_H_ */
