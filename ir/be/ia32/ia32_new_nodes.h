/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Handling of ia32 specific firm opcodes.
 * @author      Christian Wuerdig
 *
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the ia32 assembler irg.
 */
#ifndef FIRM_BE_IA32_IA32_NEW_NODES_H
#define FIRM_BE_IA32_IA32_NEW_NODES_H

#include "ia32_nodes_attr.h"

/** indices for AM inputs */
enum {
	n_ia32_base         = 0,
	n_ia32_index        = 1,
	n_ia32_mem          = 2,
	n_ia32_unary_op     = 3,
	n_ia32_binary_left  = 3,
	n_ia32_binary_right = 4,
};

/** proj numbers for "normal" one-result nodes (for the complicated cases where we not only
 * need the result) */
enum {
	pn_ia32_res          = 0,
	pn_ia32_flags        = 1,
	pn_ia32_M            = 2,
	pn_ia32_X_regular    = 3,
	pn_ia32_X_except     = 4,
	pn_ia32_st_M         = 0,
	pn_ia32_st_X_regular = 1,
	pn_ia32_st_X_except  = 2,
};

extern struct obstack opcodes_obst;

/**
 * Returns the attributes of an ia32 node.
 */
ia32_attr_t *get_ia32_attr(ir_node *node);
const ia32_attr_t *get_ia32_attr_const(const ir_node *node);

ia32_x87_attr_t *get_ia32_x87_attr(ir_node *node);
const ia32_x87_attr_t *get_ia32_x87_attr_const(const ir_node *node);

ia32_immediate_attr_t *get_ia32_immediate_attr(ir_node *node);
const ia32_immediate_attr_t *get_ia32_immediate_attr_const(const ir_node *node);

/**
 * Gets the condcode attributes of a node.
 */
ia32_condcode_attr_t *get_ia32_condcode_attr(ir_node *node);
const ia32_condcode_attr_t *get_ia32_condcode_attr_const(const ir_node *node);

/**
 * Gets the Call node attributes.
 */
ia32_call_attr_t *get_ia32_call_attr(ir_node *node);
const ia32_call_attr_t *get_ia32_call_attr_const(const ir_node *node);

/**
 * Gets the CopyB node attributes.
 */
ia32_copyb_attr_t *get_ia32_copyb_attr(ir_node *node);
const ia32_copyb_attr_t *get_ia32_copyb_attr_const(const ir_node *node);

ia32_switch_attr_t *get_ia32_switch_attr(ir_node *node);
const ia32_switch_attr_t *get_ia32_switch_attr_const(const ir_node *node);

ia32_return_attr_t *get_ia32_return_attr(ir_node *node);
const ia32_return_attr_t *get_ia32_return_attr_const(const ir_node *node);

/**
 * Gets the type of an ia32 node.
 */
ia32_op_type_t get_ia32_op_type(const ir_node *node);

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp);

/**
 * Gets the supported address mode of an ia32 node
 */
ia32_am_type_t get_ia32_am_support(const ir_node *node);

/**
 * Sets the supported addrmode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t am_arity);

/**
 * copies all address-mode attributes from one node to the other
 */
void ia32_copy_am_attrs(ir_node *to, const ir_node *from);

/**
 * Sets node to commutative.
 */
void set_ia32_commutative(ir_node *node);

/**
 * Checks if node is commutative.
 */
int is_ia32_commutative(const ir_node *node);

static inline void set_ia32_frame_use(ir_node *const node,
                                      ia32_frame_use_t const val)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	if (attr->frame_use == val)
		return;
	/* Only allow more specific, the same or reset. */
	assert(attr->frame_use == IA32_FRAME_USE_NONE
	    || attr->frame_use == IA32_FRAME_USE_AUTO
	    || val == IA32_FRAME_USE_NONE);
	attr->frame_use = val;
	if (val != IA32_FRAME_USE_NONE) {
		assert(attr->addr.immediate.kind == X86_IMM_VALUE ||
		       attr->addr.immediate.kind == X86_IMM_FRAMEENT);
		attr->addr.immediate.kind = X86_IMM_FRAMEENT;
	}
}

static inline ia32_frame_use_t get_ia32_frame_use(ir_node const *const node)
{
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	return (ia32_frame_use_t)attr->frame_use;
}

void set_ia32_is_reload(ir_node *node);
int is_ia32_is_reload(const ir_node *node);

void set_ia32_is_spill(ir_node *node);
int is_ia32_is_spill(const ir_node *node);

void set_ia32_is_remat(ir_node *node);
int is_ia32_is_remat(const ir_node *node);

/**
 * Returns the condition code of a node.
 */
x86_condition_code_t get_ia32_condcode(const ir_node *node);

/**
 * Sets the condition code of a node
 */
void set_ia32_condcode(ir_node *node, x86_condition_code_t code);

unsigned get_ia32_copyb_size(const ir_node *node);

/**
 * Gets the instruction latency.
 */
unsigned get_ia32_latency(const ir_node *node);

/**
 * Get the exception label attribute.
 */
unsigned get_ia32_exc_label(const ir_node *node);

/**
 * Set the exception label attribute.
 */
void set_ia32_exc_label(ir_node *node, unsigned flag);

/**
 * Return the exception label id.
 */
ir_label_t get_ia32_exc_label_id(const ir_node *node);

/**
 * Assign the exception label id.
 */
void set_ia32_exc_label_id(ir_node *node, ir_label_t id);

/**
 * Swaps left/right input of a node (and sets ins_permuted accordingly)
 */
void ia32_swap_left_right(ir_node *node);

/* Include the generated headers */
#include "gen_ia32_new_nodes.h"

#endif
