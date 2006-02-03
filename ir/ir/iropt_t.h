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
# include "tv.h"

ir_node *equivalent_node (ir_node *n);

/*@{*/

/** For cse */
pset *new_identities (void);
void  del_identities (pset *value_table);
void  add_identities (pset *value_table, ir_node *node);
/*@}*/

ir_node *optimize_node (ir_node *n);

ir_node *optimize_in_place_2 (ir_node *n);

/** Calculate a hash value of a node. */
unsigned ir_node_hash (ir_node *node);

/**
 * Compare function for two nodes in the hash table. Gets two
 * nodes as parameters.  Returns 0 if the nodes are a cse.
 */
int identities_cmp(const void *elt, const void *key);

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table, enters it in the table
 * if it isn't there yet.
 */
ir_node *identify_remember(pset *value_table, ir_node *n);

/**
 * Returns the tarval of a Const node or tarval_bad for all other nodes.
 */
static INLINE tarval *
value_of(ir_node *n) {
  if ((n != NULL) && (get_irn_op(n) == op_Const))
    return get_Const_tarval(n); /* might return tarval_bad */
  else
    return tarval_bad;
}

/**
 * Sets the default operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_operations(opcode code, ir_op_ops *ops);

# endif /* _IROPT_T_H_ */
