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

#include <stdbool.h>
#include <stdint.h>

#include "be_types.h"
#include "firm_types.h"
#include "irmode_t.h"

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

ir_node *be_gen_Proj_default(ir_node *node);

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
void be_enqueue_operands(ir_node *node);

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

typedef struct be_stack_change_t be_stack_change_t;

typedef struct be_stack_env_t {
	be_stack_change_t *changes;
} be_stack_env_t;

/**
 * Initialize a stack change environment.
 *
 * Usually architectures use a machine stack to store local information, e.g.
 * arguments of function calls.  This concept is not present in the middleend,
 * appears during code selection and causes chains of stack changes, which may
 * not be interleaved.  To prevent interleaving, the instruction scheduler has
 * to be aware of these chains or a total order has to be established for them
 * beforehand.  This interface performs the latter.
 * The change chains are recorded during code selection and wired afterwards.
 *
 * @param env  The stack environment to initialize.
 */
void be_stack_init(be_stack_env_t *env);

/**
 * Record one stack change chain.
 *
 * @param before  The first node of the stack change chain.
 * @param pos     The operand number of the stack of @p before.
 * @param after   The stack value produced by this change, or NULL for the last
 *                change, e.g. return.
 */
void be_stack_record_chain(be_stack_env_t *env, ir_node *before, unsigned pos, ir_node *after);

/**
 * Wire all recorded stack change chains within each block and free all recorded
 * information.
 *
 * @param env  The stack environment.
 */
void be_stack_finish(be_stack_env_t *env);

ir_entity **be_collect_parameter_entities(ir_graph *irg);

/**
 * In case where a parameter is transmitted via register but someone takes its
 * address a store to the frame which can be references is necessary.
 * This function can be used as a preprocessing phase before transformation to
 * do this. The assumption is that all parameter_entities on the stackframe
 * without an offset need such stores.
 */
void be_add_parameter_entity_stores(ir_graph *irg);

void be_add_parameter_entity_stores_list(ir_graph *irg, unsigned n_entities,
                                         ir_entity **entities);

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

ir_node *be_make_Sync(ir_node *block, int arity, ir_node **ins);

/**
 * Returns true if mode should be stored in a general purpose register
 */
static inline bool be_mode_needs_gp_reg(ir_mode *const mode)
{
	return get_mode_arithmetic(mode) == irma_twos_complement;
}

/**
 * Finds number of output value of a node which is constrained to a single
 * specific register.
 */
unsigned be_get_out_for_reg(ir_node const *node, arch_register_t const *reg);

#endif
