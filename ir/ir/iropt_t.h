/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** Declarations for optimizations intertwined with IR construction.
*/

/* $Id$ */

# ifndef _IROPT_T_H_
# define _IROPT_T_H_

# include "pset.h"
# include "iropt.h"

ir_node *equivalent_node (ir_node *n);

/* For cse */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identity (pset *value_table, ir_node *node);

ir_node *optimize_in_place_2 (ir_node *n);


# endif /* _IROPT_T_H_ */
