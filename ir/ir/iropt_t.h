/*
 * Project:     libFIRM
 * File name:   ir/ir/iropt_t.h
 * Purpose:     iropt --- optimizations intertwined with IR construction -- private header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file iropt_t.h
*
* Declarations for optimizations intertwined with IR construction.
*
* @author Martin Trapp, Christian Schaefer
*/

# ifndef _IROPT_T_H_
# define _IROPT_T_H_

# include "pset.h"
# include "iropt.h"

ir_node *equivalent_node (ir_node *n);

/*@{*/

/** For cse */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identities (pset *value_table, ir_node *node);
/*@}*/

ir_node *optimize_node (ir_node *n);

ir_node *optimize_in_place_2 (ir_node *n);

/* Calculate a hash value of a node. */
unsigned ir_node_hash (ir_node *node);


# endif /* _IROPT_T_H_ */
