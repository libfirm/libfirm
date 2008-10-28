/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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

#include "irop_t.h"
#include "iropt.h"
#include "irnode_t.h"
#include "pset.h"
#include "tv.h"

/**
 * Calculate a hash value of a node.
 *
 * @param node  The IR-node
 */
unsigned ir_node_hash(const ir_node *node);

/**
 * equivalent_node() returns a node equivalent to input n. It skips all nodes that
 * perform no actual computation, as, e.g., the Id nodes.  It does not create
 * new nodes.  It is therefore safe to free n if the node returned is not n.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., Div).
 */
ir_node *equivalent_node(ir_node *n);

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
void del_identities(pset *value_table);

/**
 * Add a node to the identities value table.
 */
void add_identities(pset *value_table, ir_node *node);

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

/**
 * Normalize a node by putting constants (and operands with larger
 * node index) on the right (operator side).
 *
 * @param n   The node to normalize
 */
void ir_normalize_node(ir_node *n);

ir_node *optimize_node(ir_node *n);

ir_node *optimize_in_place_2(ir_node *n);

/**
 * The value_of operation.
 * This operation returns for every IR node an associated tarval if existing,
 * returning tarval_bad otherwise.
 * No calculations are done here, just a lookup.
 */
typedef tarval *(*value_of_func)(const ir_node *self);

extern value_of_func value_of_ptr;

/**
 * Set a new value_of function.
 *
 * @param func  the function, NULL restores the default behavior
 */
void set_value_of_func(value_of_func func);

/**
 * Returns the associated tarval of a node.
 */
static inline tarval *
value_of(const ir_node *n) {
	return value_of_ptr(n);
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
