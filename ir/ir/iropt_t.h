/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    iropt --- optimizations intertwined with IR construction -- private header.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version  $Id$
 */
#ifndef FIRM_IR_IROPT_T_H
#define FIRM_IR_IROPT_T_H

#include "iropt.h"
#include "irnode_t.h"
#include "pset.h"
#include "tv.h"

ir_node *equivalent_node(ir_node *n);

/**
 * Calculate a hash value of a node.
 * The hash value is calculated from the nodes predecessors.
 * Special handling for Const and SymConst nodes (these don't have predecessors).
 *
 * @param node  The IR-node
 */
unsigned ir_node_hash(ir_node *node);

/**
 * Creates a new value table used for storing CSE identities.
 * The value table is used to identify common expressions.
 *
 */
pset *new_identities(void);

/**
 * Deletes a identities value table.
 *
 * @param value_table  the identity set
 */
void  del_identities(pset *value_table);

/**
 * Add a node to the identities value table.
 */
void  add_identities(pset *value_table, ir_node *node);

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

/** Visit each node in the value table of a graph. */
void visit_all_identities(ir_graph *irg, irg_walk_func visit, void *env);

ir_node *optimize_node(ir_node *n);

ir_node *optimize_in_place_2(ir_node *n);

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
 * Sets the default operations for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_operations(ir_opcode code, ir_op_ops *ops);

#endif
