/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       be transform helper extracted from the ia32 backend.
 * @author      Matthias Braun, Michael Beck
 * @date        14.06.2007
 */
#ifndef FIRM_BE_BETRANSHLP_H
#define FIRM_BE_BETRANSHLP_H

#include "be_types.h"
#include "firm_types.h"

/**
 * A callback to pre-transform some nodes before the transformation starts.
 */
typedef void (arch_pretrans_nodes)(void);

/**
 * The type of a transform function.
 */
typedef ir_node *(be_transform_func)(ir_node *node);

/** pre-transform a node */
ir_node *be_pre_transform_node(ir_node *place);

/**
 * Calls transformation function for given node and marks it visited.
 */
ir_node *be_transform_node(ir_node *node);

/**
 * Creates a new phi (needs some special handling since we can't transform
 * all predecessors yet).
 */
ir_node *be_transform_phi(ir_node *node, const arch_register_req_t *req);

/**
 * Duplicate all dependency edges of a node.
 */
void be_duplicate_deps(ir_node *old_node, ir_node *new_node);

/**
 * Duplicate a node during transformation.
 */
ir_node *be_duplicate_node(ir_node *node);

/** clear transform functions and sets some virtual nodes like
 * Start, Sync, Pin to the duplication transformer */
void be_start_transform_setup(void);

/** register a transform function for a specific node type */
void be_set_transform_function(ir_op *op, be_transform_func func);

/** register a transform function for a Proj attached to a specific node */
void be_set_transform_proj_function(ir_op *pred_op, be_transform_func func);

/**
 * Associate an old node with a transformed node. Uses link field.
 */
void be_set_transformed_node(ir_node *old_node, ir_node *new_node);

/**
 * returns 1 if the node is already transformed
 */
int be_is_transformed(const ir_node *node);

/**
 * enqueue all inputs into the transform queue.
 */
void be_enqueue_preds(ir_node *node);

/**
 * Transform a graph. Transformers must be registered first.
 */
void be_transform_graph(ir_graph *irg, arch_pretrans_nodes *func);

typedef bool (*upper_bits_clean_func)(const ir_node *node, ir_mode *mode);

/**
 * register a test function for be_upper_bits_clean for a specific node
 * type.
 */
void be_set_upper_bits_clean_function(ir_op *op, upper_bits_clean_func func);

/**
 * returns true if it is assured, that the upper bits of a node are "clean"
 * which means for a 16 or 8 bit value, that the upper bits in the register
 * are 0 for unsigned and a copy of the last significant bit for signed
 * numbers.
 */
bool be_upper_bits_clean(const ir_node *node, ir_mode *mode);

/**
 * returns true if node is the root pattern of a (left) rotation.
 * The root @p node must be an Add or Or node.
 */
bool be_pattern_is_rotl(const ir_node *node, ir_node **left, ir_node **right);

#endif
