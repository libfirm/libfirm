/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** Declarations for optimizations intertwined with IR construction.
*/

# ifndef _IROPT_T_H_
# define _IROPT_T_H_

# include "pset.h"
# include "iropt.h"

pset *new_identities (void);
void del_identities (pset *value_table);
void add_identity (pset *value_table, ir_node *node);

# endif /* _IROPT_T_H_ */
