/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Goetz Lindenmaier.
**
** bool.h: Datatype bool.
**
*/

/* $Id$ */

# ifndef _BOOL_H_
# define _BOOL_H_

# ifndef __cplusplus

typedef unsigned char bool;

# endif /* __cplusplus */

# ifndef TRUE
#  define TRUE  1
#  define FALSE 0
# endif /* ndef TRUE */

# ifndef true
#  define true  1
#  define false 0
# endif /* ndef TRUE */

# endif /* _BOOL_H_ */
