/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Representation of opcode of intermediate operation -- private header.
 * @author   Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IROP_T_H
#define FIRM_IR_IROP_T_H

#include "irop.h"

#include <stdbool.h>

#include "tv.h"

#define get_op_code(op)         get_op_code_(op)
#define get_op_pinned(op)       get_op_pinned_(op)
#define set_op_tag(op, tag)     set_op_tag_((op), (tag))
#define get_op_tag(op)          get_op_tag_(op)
#define set_op_attr(op, attr)   set_op_attr_((op), (attr))
#define get_op_attr(op)         get_op_attr_(op)

#define set_generic_function_ptr(op, func) set_generic_function_ptr_((op), (op_func)(func))
#define get_generic_function_ptr(type, op) ((type*)get_generic_function_ptr_((op)))

/**
 * Operation specific callbacks.
 */
typedef struct {
	hash_func             hash;                 /**< Calculate a hash value for an IR node. */
	computed_value_func   computed_value;       /**< Evaluates a node into a tarval if possible. */
	computed_value_func   computed_value_Proj;  /**< Evaluates a Proj node into a tarval if possible. */
	equivalent_node_func  equivalent_node;      /**< Optimizes the node by returning an equivalent one. */
	equivalent_node_func  equivalent_node_Proj; /**< Optimizes the Proj node by returning an equivalent one. */
	transform_node_func   transform_node;       /**< Optimizes the node by transforming it. */
	transform_node_func   transform_node_Proj;  /**< Optimizes the Proj node by transforming it. */
	node_attrs_equal_func  attrs_equal;         /**< Compares two node attributes. */
	reassociate_func      reassociate;          /**< Reassociate a tree. */
	copy_attr_func        copy_attr;            /**< Copy node attributes. */
	get_type_attr_func    get_type_attr;        /**< Returns the type attribute of a node. */
	get_entity_attr_func  get_entity_attr;      /**< Returns the entity attribute of a node. */
	verify_node_func      verify_node;          /**< Verify the node. */
	verify_proj_node_func verify_proj_node;     /**< Verify the Proj node. */
	dump_node_func        dump_node;            /**< Dump a node. */
	op_func               generic;              /**< A generic function pointer. */
	op_func               generic1;             /**< A generic function pointer. */
	op_func               generic2;             /**< A generic function pointer. */
} ir_op_ops;

/** The type of an ir_op. */
struct ir_op {
	unsigned     code;         /**< The unique opcode of the op. */
	const char  *name;         /**< The name of the op. */
	size_t       attr_size;    /**< Space needed in memory for private
	                                attributes */
	op_pin_state pin_state;    /**< How to deal with the node in CSE, PRE. */
	op_arity     opar;         /**< The arity of operator. */
	int          op_index;     /**< The index of the first data operand, 0 for
	                                most cases, 1 for Div etc. */
	int          memory_index; /**< index of memory input for memory nodes */
	unsigned     pn_x_regular; /**< for fragile ops the position of the
	                                X_regular output */
	unsigned     pn_x_except;  /**< for fragile ops the position of the
	                                X_except output */
	unsigned     flags;        /**< Flags describing the behavior of the ir_op,
	                                a bitmasks of irop_flags. */
	unsigned     tag;          /**< Custom TAG from the op's creator */
	void        *attr;         /**< custom pointer where op's creator can attach
	                                attribute stuff to. */
	ir_op_ops    ops;          /**< The operations of the this op. */
};

/** Initialize the irop module. */
void firm_init_op(void);

/** frees memory allocated by irop module */
void firm_finish_op(void);

/**
 * Returns the attribute size of nodes of this opcode.
 * @note Use not encouraged, internal feature.
 */
static inline size_t get_op_attr_size(const ir_op *op)
{
	return op->attr_size;
}

/**
 * Returns non-zero if op is a control flow opcode,
 * like Start, End, Jmp, Cond, Return, Raise or Bad.
 */
static inline bool is_op_cfopcode(const ir_op *op)
{
	return op->flags & irop_flag_cfopcode;
}

static inline bool is_op_unknown_jump(const ir_op *op)
{
	return op->flags & irop_flag_unknown_jump;
}

/** Returns non-zero if operation is commutative */
static inline bool is_op_commutative(const ir_op *op)
{
	return op->flags & irop_flag_commutative;
}

/** Returns non-zero if operation is fragile */
static inline bool is_op_fragile(const ir_op *op)
{
	return op->flags & irop_flag_fragile;
}

/** Returns non-zero if operation is forking control flow */
static inline bool is_op_forking(const ir_op *op)
{
	return op->flags & irop_flag_forking;
}

/** Returns non-zero if operation is a const-like op */
static inline bool is_op_constlike(const ir_op *op)
{
	return op->flags & irop_flag_constlike;
}

static inline bool is_op_uses_memory(const ir_op *op)
{
	return op->flags & irop_flag_uses_memory;
}

/** Returns non-zero if operation is a keep-like op */
static inline bool is_op_keep(const ir_op *op)
{
	return op->flags & irop_flag_keep;
}

/** Returns non-zero if operation must always be placed in the start block. */
static inline bool is_op_start_block_placed(const ir_op *op)
{
	return op->flags & irop_flag_start_block;
}

static inline unsigned get_op_code_(const ir_op *op)
{
	return op->code;
}

static inline op_pin_state get_op_pinned_(const ir_op *op)
{
	return op->pin_state;
}

static inline void set_generic_function_ptr_(ir_op *op, op_func func)
{
	op->ops.generic = func;
}

static inline op_func get_generic_function_ptr_(const ir_op *op)
{
	return op->ops.generic;
}

static inline ir_op_ops const *get_op_ops(ir_op const *const op)
{
	return &op->ops;
}

static inline void set_op_tag_(ir_op *op, unsigned tag)
{
	op->tag = tag;
}

static inline unsigned get_op_tag_(const ir_op *op)
{
	return op->tag;
}

static inline void set_op_attr_(ir_op *op, void *attr)
{
	op->attr = attr;
}

static inline void *get_op_attr_(const ir_op *op)
{
	return op->attr;
}

/** An attr_equals function that always returns 0/false */
int attrs_equal_false(const ir_node *a, const ir_node *b);

void default_copy_attr(ir_graph *irg, ir_node const *old_node, ir_node *new_node);

#endif
