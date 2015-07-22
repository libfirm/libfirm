/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Helper functions for code selection.
 * @author   Matthias Braun, Michael Beck
 * @date     14.06.2007
 */
#ifndef FIRM_BE_BETRANSHLP_H
#define FIRM_BE_BETRANSHLP_H

#include "be_types.h"
#include "firm_types.h"

typedef struct be_stackorder_t be_stackorder_t;

/**
 * A callback to pre-transform some nodes before the transformation starts.
 */
typedef void (arch_pretrans_nodes)(ir_graph*);

/**
 * The type of a transform function.
 */
typedef ir_node *(be_transform_func)(ir_node *node);

/**
 * Calls transformation function for given node and marks it visited.
 */
ir_node *be_transform_node(ir_node *node);

/**
 * Transform the node's block.
 */
ir_node *be_transform_nodes_block(ir_node const *node);

/**
 * Creates a new phi (needs some special handling since we can't transform
 * all predecessors yet).
 */
ir_node *be_transform_phi(ir_node *node, const arch_register_req_t *req);

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
 * returns true if the node is already transformed
 */
bool be_is_transformed(const ir_node *node);

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

/**
 * maps an operation which potentially produces exceptions (like Div,Mod) to a
 * runtime call.
 */
void be_map_exc_node_to_runtime_call(ir_node *node, ir_mode *res_mode,
                                     ir_entity *runtime_entity,
                                     long pn_M, long pn_X_regular,
                                     long pn_X_except, long pn_res);

/**
 * In the normal firm representation some nodes like pure calls, builtins
 * have no memory inputs+outputs. However in the backend these sometimes have to
 * access the stack to work and therefore suddenly need to be enqueued into the
 * memory edge again.
 * This API creates a possible order to enqueue them so we can be sure to create
 * a legal dependency graph when transforming them.
 */
be_stackorder_t *be_collect_stacknodes(ir_graph *irg);

/**
 * return node that should produce the predecessor stack node in a block.
 * returns NULL if there's no predecessor in the current block.
 */
ir_node *be_get_stack_pred(const be_stackorder_t *env, const ir_node *node);

/**
 * free memory associated with a stackorder structure
 */
void be_free_stackorder(be_stackorder_t *env);

/**
 * In case where a parameter is transmitted via register but someone takes its
 * address a store to the frame which can be references is necessary.
 * This function can be used as a preprocessing phase before transformation to
 * do this. The assumption is that all parameter_entities which are passed
 * through the stack are already moved to the arg_type and all remaining
 * parameter_entities on the frame type need stores.
 */
void be_add_parameter_entity_stores(ir_graph *irg);

uint32_t be_get_tv_bits32(ir_tarval *tv, unsigned offset);

/**
 * Skip integer truncations.
 *
 * @param node         the node
 * @param single_user  only skip, if a down-conv has a single user
 * @return the node after skipping down-convs
 */
ir_node *be_skip_downconv(ir_node *node, bool single_user);

/** Skip all signedness convs */
ir_node *be_skip_sameconv(ir_node *node);

bool be_match_immediate(ir_node const *node, ir_tarval **tarval_out,
                        ir_entity **entity_out, unsigned *reloc_kind_out);

#endif
