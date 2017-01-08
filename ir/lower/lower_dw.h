/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   doubleword lowering operations
 * @author  Michael Beck, Matthias Braun
 */
#ifndef FIRM_LOWER_LOWER_DW_H
#define FIRM_LOWER_LOWER_DW_H

#include "firm_types.h"

/**
 * Every double word node will be replaced,
 * we need some store to hold the replacement:
 */
typedef struct lower64_entry_t {
	ir_node *low_word;    /**< the low word */
	ir_node *high_word;   /**< the high word */
} lower64_entry_t;


/**
 * A callback type for creating an intrinsic entity for a given opcode.
 *
 * @param method   the method type of the emulation function entity
 * @param op       the emulated ir_op
 * @param imode    the input mode of the emulated opcode
 * @param omode    the output mode of the emulated opcode
 * @param context  the context parameter
 */
typedef ir_entity *(create_intrinsic_fkt)(ir_type *method, const ir_op *op,
                                          const ir_mode *imode,
                                          const ir_mode *omode, void *context);

/**
 * The lowering parameter description.
 */
typedef struct lwrdw_param_t {
	create_intrinsic_fkt *create_intrinsic; /**< callback that creates the intrinsic entity */
	void                 *ctx;              /**< context parameter for the creator function */
	ir_mode              *word_unsigned;    /**< mode for unsigned word */
	ir_mode              *word_signed;      /**< mode for signed word */
	unsigned short        doubleword_size;  /**< bitsize of the doubleword mode */
} lwrdw_param_t;

/**
 * Prepare the doubleword lowering algorithm. Creates an environment
 * which can be used to register custom lowering functions
 */
void ir_prepare_dw_lowering(const lwrdw_param_t *param);

/**
 * Lower all doubleword operations in the program.
 * Must be called after ir_prepare_dw_lowering()
 */
void ir_lower_dw_ops(void);

typedef void (*lower_dw_func)(ir_node *node, ir_mode *mode);

/**
 * register a custom lowering function.
 * After lowering the custom function should call ir_set_dw_lowered()
 */
void ir_register_dw_lower_function(ir_op *op, lower_dw_func func);

/**
 * After lowering a node a custom doubleword lowering function has to call this.
 * It registers 2 new values for the high and low part of the lowered value.
 */
void ir_set_dw_lowered(ir_node *old, ir_node *new_low, ir_node *new_high);

/**
 * Query lowering results of a node. In a lowering callback you can use this
 * on all predecessors of a node. The only exception are block and phi nodes.
 * Their predecessors are not necessarily transformed yet.
 */
lower64_entry_t *get_node_entry(ir_node *node);

static inline ir_node *get_lowered_low(ir_node *node)
{
	return get_node_entry(node)->low_word;
}

static inline ir_node *get_lowered_high(ir_node *node)
{
	return get_node_entry(node)->high_word;
}

void ir_default_lower_dw_Conv(ir_node *node, ir_mode *mode);

/**
 * We need a custom version of part_block_edges because during transformation
 * not all data-dependencies are explicit yet if a lowered nodes users are not
 * lowered yet.
 * We can fix this by modifying move to look for such implicit dependencies.
 * Additionally we have to keep the proj_2_block map updated
 */
ir_node *part_block_dw(ir_node *node);

/**
 * (External) lowering callbacks must call this to announce that they changed
 * the control flow.
 */
void set_dw_control_flow_changed(void);

/**
 * Return the unsigned variant of the lowered mode
 * Note: you must only call this during a dw_lowering (= in a lowering callback)
 */
ir_mode *ir_get_low_unsigned_mode(void);

#endif
