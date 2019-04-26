/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @brief   Operators of firm nodes.
 */
#ifndef FIRM_IR_IROP_H
#define FIRM_IR_IROP_H

#include <stdio.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup ir_node
 * @defgroup ir_op  Node Opcodes
 *
 * This module specifies the opcodes for ir nodes. Each node has an associated
 * opcode. An opcode specifies basic attributes and properties of a node.
 * @{
 */

/** The allowed arities. */
typedef enum {
	oparity_invalid = 0,
	oparity_binary,             /**< A binary operator  -- considering 'numeric' arguments.*/
	oparity_dynamic,            /**< The arity depends on state of Firm representation.
	                                 Can be changed by optimizations...
	                                 We must allocate a dynamic in array for the node! */
	/** Any other arity, either fixed for the opcode or known when creating the node. */
	oparity_any
} op_arity;


/** The irop flags */
typedef enum {
	irop_flag_none         = 0, /**< Nothing. */
	irop_flag_commutative  = 1U <<  0, /**< This operation is commutative. */
	irop_flag_cfopcode     = 1U <<  1, /**< This operation is a control flow operation. */
	irop_flag_fragile      = 1U <<  2, /**< Set if the operation can change the
	                                       control flow because of an exception.
	                                   */
	irop_flag_forking      = 1U <<  3, /**< Forking control flow at this operation. */
	irop_flag_constlike    = 1U <<  5, /**< This operation has no arguments and is some
	                                       kind of a constant. */
	irop_flag_keep         = 1U <<  6, /**< This operation can be kept in End's keep-alive list. */
	irop_flag_start_block  = 1U <<  7, /**< This operation is always placed in the Start block. */
	irop_flag_uses_memory  = 1U <<  8, /**< This operation has a memory input and may change the memory state. */
	irop_flag_dump_noblock = 1U <<  9, /**< node should be dumped outside any blocks */
	/** This operation jumps to an unknown destination. The CFG is a
	 * conservative approximation in this case. You cannot change the destination
	 * of an unknown_jump */
	irop_flag_unknown_jump = 1U << 11,
	/** The opcode has a memory input/output but does not actually change the
	 * contents of any memory block known to the program. The input/output is
	 * just necessary for scheduling reasons. Implies irop_flag_uses_memory. */
	irop_flag_const_memory = 1U << 12,
} irop_flags;
ENUM_BITSET(irop_flags)

/** Returns the string for the opcode. */
FIRM_API const char *get_op_name(const ir_op *op);

/** Returns the enum for the opcode */
FIRM_API unsigned get_op_code(const ir_op *op);

/** Returns a human readable name of an op_pin_state. */
FIRM_API const char *get_op_pin_state_name(op_pin_state s);

/** Returns pinned state of an opcode. */
FIRM_API op_pin_state get_op_pinned(const ir_op *op);

/** Returns the next free IR opcode number, allows to register user ops. */
FIRM_API unsigned get_next_ir_opcode(void);

/** Returns the next free n IR opcode number, allows to register a bunch of user ops. */
FIRM_API unsigned get_next_ir_opcodes(unsigned num);

/**
 * A generic function pointer type.
 */
typedef void (*op_func)(void);

/**
 * Returns the generic function pointer from an IR operation.
 */
FIRM_API op_func get_generic_function_ptr(const ir_op *op);

/**
 * Stores a generic function pointer into an IR operation.
 */
FIRM_API void set_generic_function_ptr(ir_op *op, op_func func);

/**
 * Returns the irop flags of an IR opcode.
 */
FIRM_API irop_flags get_op_flags(const ir_op *op);

/**
 * The hash operation.
 * This operation calculates a hash value for a given IR node.
 */
typedef unsigned (*hash_func)(const ir_node *self);

/**
 * The compute value operation.
 * If a node is known to always produce a specific value, this value is
 * returned, if it is known to always throw an exception or produce undefined
 * behaviour (like division by zero) tarval_bad is returned, otherwise
 * tarval_unknown is returned.
 */
typedef ir_tarval *(*computed_value_func)(const ir_node *self);

/**
 * The equivalent node operation.
 * This operation returns an equivalent node for the input node.
 * It does not create new nodes.  It is therefore safe to free self
 * if the node returned is not self.
 * If a node returns a Tuple we can not just skip it.  If the size of the
 * in array fits, we transform n into a tuple (e.g., possible for Div).
 */
typedef ir_node *(*equivalent_node_func)(ir_node *self);

/**
 * The transform node operation.
 * This operation tries several [inplace] [optimizing] transformations
 * and returns an equivalent node.
 * The difference to equivalent_node() is that these
 * transformations _do_ generate new nodes, and thus the old node must
 * not be freed even if the equivalent node isn't the old one.
 */
typedef ir_node *(*transform_node_func)(ir_node *self);

/**
 * The node attribute compare operation.
 * Compares the nodes attributes of two nodes of identical opcode
 * and returns 1 if the attributes are identical, 0 if they differ.
 */
typedef int (*node_attrs_equal_func)(const ir_node *a, const ir_node *b);

/**
 * The reassociation operation.
 * Called from a walker.  Returns non-zero if
 * a reassociation rule was applied.
 * The pointer n is set to the newly created node, if some reassociation
 * was applied.
 */
typedef int (*reassociate_func)(ir_node **n);

/**
 * The copy attribute operation.
 * Copy the node attributes from an 'old' node to a 'new' one.
 */
typedef void (*copy_attr_func)(ir_graph *irg, const ir_node *old_node, ir_node *new_node);

/**
 * The get_type_attr operation. Used to traverse all types that can be
 * accessed from an ir_graph.
 * Returns the type attribute of the node self.
 */
typedef ir_type *(*get_type_attr_func)(const ir_node *self);

/**
 * The get_entity_attr operation. Used to traverse all entities that can be
 * accessed from an ir_graph.
 * Returns the entity attribute of the node self.
 */
typedef ir_entity *(*get_entity_attr_func)(const ir_node *self);

/**
 * The verify_node operation.
 * Returns non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_node_func)(const ir_node *node);

/**
 * The verify_node operation for Proj(X).
 * Returns non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_proj_node_func)(const ir_node *proj);

/**
 * Reasons to call the dump_node operation:
 */
typedef enum {
	dump_node_opcode_txt,   /**< Dump the opcode. */
	dump_node_mode_txt,     /**< Dump the mode. */
	dump_node_nodeattr_txt, /**< Dump node attributes to be shown in the label. */
	dump_node_info_txt      /**< Dump node attributes into info1. */
} dump_reason_t;

/**
 * The dump_node operation.
 * Writes several informations requested by reason to
 * an output file
 */
typedef void (*dump_node_func)(FILE *out, const ir_node *self, dump_reason_t reason);

/** Sets hash callback @p func for operation @p op */
FIRM_API void set_op_hash(ir_op *op, hash_func func);
/** Sets computed_value callback @p func for operation @p op */
FIRM_API void set_op_computed_value(ir_op *op, computed_value_func func);
/** Sets compute_value for Proj callback @p func for operation @p op */
FIRM_API void set_op_computed_value_proj(ir_op *op, computed_value_func func);
/** Sets equivalent node callback @p func for operation @p op */
FIRM_API void set_op_equivalent_node(ir_op *op, equivalent_node_func func);
/** Sets equivalent Proj node callback @p func for operation @p op */
FIRM_API void set_op_equivalent_node_proj(ir_op *op, equivalent_node_func func);
/** Sets transform node callback @p func for operation @p op */
FIRM_API void set_op_transform_node(ir_op *op, transform_node_func func);
/** Sets transform Proj node callback @p func for operation @p op */
FIRM_API void set_op_transform_node_proj(ir_op *op, transform_node_func func);
/** Sets attrs_equal callback @p func for operation @p op */
FIRM_API void set_op_attrs_equal(ir_op *op, node_attrs_equal_func func);
/** Sets reassociation callback @p func for operation @p op */
FIRM_API void set_op_reassociate(ir_op *op, reassociate_func func);
/** Sets attribute copy callback @p func for operation @p op */
FIRM_API void set_op_copy_attr(ir_op *op, copy_attr_func func);
/** Sets get type callback @p func for operation @p op */
FIRM_API void set_op_get_type_attr(ir_op *op, get_type_attr_func func);
/** Sets get entity callback @p func for operation @p op */
FIRM_API void set_op_get_entity_attr(ir_op *op, get_entity_attr_func func);
/** Sets verify callback @p func for operation @p op */
FIRM_API void set_op_verify(ir_op *op, verify_node_func func);
/** Sets proj verify callback @p func for operation @p op */
FIRM_API void set_op_verify_proj(ir_op *op, verify_proj_node_func func);
/** Sets dump callback @p func for operation @p op */
FIRM_API void set_op_dump(ir_op *op, dump_node_func func);

/**
 * Creates a new IR operation.
 *
 * @param code      the opcode, one of type \c opcode
 * @param name      the printable name of this opcode
 * @param p         whether operations of this opcode are op_pin_state_pinned or floating
 * @param flags     a bitmask of irop_flags describing the behavior of the IR operation
 * @param opar      the parity of this IR operation
 * @param op_index  if the parity is oparity_unary, oparity_binary or oparity_trinary the index
 *                  of the left operand
 * @param attr_size attribute size for this IR operation
 *
 * @return The generated IR operation.
 *
 * This function can create all standard Firm opcode as well as new ones.
 * The behavior of new opcode depends on the operations \c ops and the \c flags.
 */
FIRM_API ir_op *new_ir_op(unsigned code, const char *name, op_pin_state p,
                          irop_flags flags, op_arity opar, int op_index,
                          size_t attr_size);

/**
 * Frees an ir operation.
 *
 * Obviously nodes with this op mustn't exist at this point.
 */
FIRM_API void free_ir_op(ir_op *code);

/** Returns one more than the highest opcode code in use. */
FIRM_API unsigned ir_get_n_opcodes(void);

/**
 * Returns the opcode with code @p code.
 *
 * @p code has to be smaller than get_irp_n_opcode(), returns NULL if
 * no opcode with the code exists.
 */
FIRM_API ir_op *ir_get_opcode(unsigned code);

/** Sets the generic function pointer of all opcodes to NULL */
FIRM_API void ir_clear_opcodes_generic_func(void);

/**
 * Sets memory input of operation using memory
 */
FIRM_API void ir_op_set_memory_index(ir_op *op, int memory_index);

/**
 * Sets proj-number for X_regular and X_except projs of fragile nodes.
 * Note: should only be used immediately after new_ir_op
 */
FIRM_API void ir_op_set_fragile_indices(ir_op *op, unsigned pn_x_regular,
                                        unsigned pn_x_except);

/** @} */

#include "end.h"

#endif
