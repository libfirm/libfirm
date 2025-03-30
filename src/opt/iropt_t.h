/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    iropt --- optimizations intertwined with IR construction -- private header.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IROPT_T_H
#define FIRM_IR_IROPT_T_H

#include <stdbool.h>
#include "irop_t.h"
#include "iropt.h"
#include "irnode_t.h"
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
 */
void new_identities(ir_graph *irg);

/**
 * Deletes a identities value table.
 *
 * @param value_table  the identity set
 */
void del_identities(ir_graph *irg);

/**
 * Add a node to the identities value table.
 */
void add_identities(ir_node *node);

/**
 * Return the canonical node computing the same value as n.
 * Looks up the node in a hash table, enters it in the table
 * if it isn't there yet.
 */
ir_node *identify_remember(ir_node *n);

/** Visit each node in the value table of a graph. */
void visit_all_identities(ir_graph *irg, irg_walk_func visit, void *env);

/**
 * Normalize a node by putting constants (and operands with larger
 * node index) on the right (operator side).
 *
 * @param n   The node to normalize
 */
void ir_normalize_node(ir_node *n);

ir_node *optimize_in_place_2(ir_node *n);

/**
 * The value_of operation.
 * This operation returns for every IR node an associated tarval if existing,
 * returning tarval_bad otherwise.
 * No calculations are done here, just a lookup.
 */
typedef ir_tarval *(*value_of_func)(const ir_node *self);

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
static inline ir_tarval *value_of(const ir_node *n)
{
	return value_of_ptr(n);
}

/**
 * returns true if a value becomes zero when converted to mode @p mode
 */
bool ir_zero_when_converted(const ir_node *node, ir_mode *dest_mode);

int ir_mux_is_abs(const ir_node *sel, const ir_node *mux_false,
                  const ir_node *mux_true);

ir_node *ir_get_abs_op(const ir_node *sel, ir_node *mux_false,
                       ir_node *mux_true);

/**
 * return true if the Mux node will be optimized away. This can be used for
 * the if-conversion callback. Allowing these Muxes should be always safe, even
 * if the backend cannot handle them.
 */
int ir_is_optimizable_mux(const ir_node *sel, const ir_node *mux_false,
                          const ir_node *mux_true);

/**
 * Returns true if Conv_m0(Conv_m1( x_m2)) is equivalent to Conv_m0(x_m2)
 */
bool may_leave_out_middle_conv(ir_mode *m0, ir_mode *m1, ir_mode *m2);

/**
 * Try to predict the value of a Load operation.
 * This can usually be done if it loads from a known global entity with a
 * constant value.
 */
ir_node *predict_load(ir_node *ptr, ir_mode *mode);

void ir_register_opt_node_ops(void);

#endif
