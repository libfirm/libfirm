/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** firm.h: initialize the firm tree with all its components
*/

# ifndef _FIRM_H_
# define _FIRM_H_

/* The representations */
# include "irprog.h"
# include "type.h"
# include "entity.h"
/* Functionality */
# include "ircons.h"
# include "irgopt.h"

/* */
# include "xprintf.h"


/** Global flags.  Set these by autoconf?? **/
/* If this is defined debuging aids are created, e.g. a field in
   ir_node uniquely numbering the nodes. */
/* @@@???? this is also set in irnode.h */
#define DEBUG_libfirm


/* initialize firm */
void init_firm (void);

# endif /* _FIRM_H_ */
