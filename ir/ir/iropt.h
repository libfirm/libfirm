/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** Declarations for optimizations intertwined with IR construction.
*/

# ifndef _IROPT_H_
# define _IROPT_H_

# include "irnode.h"
# include "irgraph.h"
# include "irflag.h"

/* optimize_in_place (n) may change the contents of the ir_node itself,
   [e.g. by making it a Id-node], but does not change its identity.
   So it is safe to be called on already referenced nodes.

   optimize_in_place (n) returns a pointer to a node equivalent to `n'
   which should be used instead of `n'.

   optimize (n) may deallocate `n' and everything allocated after `n'! */

tarval *computed_value (ir_node *n);

ir_node *optimize (ir_node *n);
ir_node *optimize_in_place (ir_node *n);

# endif /* _IROPT_H_ */
