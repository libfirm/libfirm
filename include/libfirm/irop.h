/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Representation of opcode of intermediate operation.
 * @author  Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 * @brief
 *  Operators of firm nodes.
 *
 *  This module specifies the opcodes possible for ir nodes.  Their
 *  definition is close to the operations specified in UKA Tech-Report
 *  1999-14
 */
#ifndef FIRM_IR_IROP_H
#define FIRM_IR_IROP_H

#include <stdio.h>
#include "firm_types.h"
#include "ident.h"
#include "begin.h"
#include "opcodes.h"

/** The allowed arities. */
typedef enum {
	oparity_invalid = 0,
	oparity_unary,              /**< An unary operator -- considering 'numeric' arguments. */
	oparity_binary,             /**< A binary operator  -- considering 'numeric' arguments.*/
	oparity_trinary,            /**< A trinary operator  -- considering 'numeric' arguments.*/
	oparity_zero,               /**< A zero arity operator, e.g. a Const. */
	oparity_variable,           /**< The arity is not fixed by opcode, but statically
	                                 known.  E.g., number of arguments to call. */
	oparity_dynamic,            /**< The arity depends on state of Firm representation.
	                                 Can be changed by optimizations...
	                                 We must allocate a dynamic in array for the node! */
	oparity_any                 /**< Any other arity. */
} op_arity;


/** The irop flags */
typedef enum {
	irop_flag_none         = 0, /**< Nothing. */
	irop_flag_labeled      = 1U << 0, /**< If set, output edge labels on in-edges in vcg graph. */
	irop_flag_commutative  = 1U << 1, /**< This operation is commutative. */
	irop_flag_cfopcode     = 1U << 2, /**< This operation is a control flow operation. */
	irop_flag_fragile      = 1U << 3, /**< Set if the operation can change the
	                                       control flow because of an exception.
	                                   */
	irop_flag_forking      = 1U << 4, /**< Forking control flow at this operation. */
	irop_flag_highlevel    = 1U << 5, /**< This operation is a pure high-level one and can be
	                                      skipped in low-level optimizations. */
	irop_flag_constlike    = 1U << 6, /**< This operation has no arguments and is some
	                                       kind of a constant. */
	irop_flag_always_opt   = 1U << 7, /**< This operation must always be optimized .*/
	irop_flag_keep         = 1U << 8, /**< This operation can be kept in End's keep-alive list. */
	irop_flag_start_block  = 1U << 9, /**< This operation is always placed in the Start block. */
	irop_flag_uses_memory  = 1U << 10, /**< This operation has a memory input and may change the memory state. */
	irop_flag_dump_noblock = 1U << 11, /**< node should be dumped outside any blocks */
	irop_flag_dump_noinput = 1U << 12, /**< node is a placeholder for "no input" */
	irop_flag_machine      = 1U << 13, /**< This operation is a machine operation. */
	irop_flag_machine_op   = 1U << 14, /**< This operation is a machine operand. */
	irop_flag_cse_neutral  = 1U << 15, /**< This operation is CSE neutral to its users. */
	/** This operation jumps to an unknown destination. The CFG is a
	 * conservative aproximation in this case. You cannot change the destination
	 * of an unknown_jump */
	irop_flag_unknown_jump = 1U << 16,
	irop_flag_user         = 1U << 17, /**< This flag and all higher ones are free for machine user. */
} irop_flags;

/** Returns the ident for the opcode name */
FIRM_API ident *get_op_ident(const ir_op *op);

/** Returns the string for the opcode. */
FIRM_API const char *get_op_name(const ir_op *op);

/** Returns the enum for the opcode */
FIRM_API unsigned get_op_code(const ir_op *op);

/** Returns a human readable name of an op_pin_state. */
FIRM_API const char *get_op_pin_state_name(op_pin_state s);

/** Gets pinned state of an opcode. */
FIRM_API op_pin_state get_op_pinned(const ir_op *op);

/** Sets pinned in the opcode.  Setting it to floating has no effect
    for Block, Phi and control flow nodes. */
FIRM_API void set_op_pinned(ir_op *op, op_pin_state pinned);

/** Returns the next free IR opcode number, allows to register user ops. */
FIRM_API unsigned get_next_ir_opcode(void);

/** Returns the next free n IR opcode number, allows to register a bunch of user ops. */
FIRM_API unsigned get_next_ir_opcodes(unsigned num);

/**
 * A generic function pointer type.
 */
typedef void (*op_func)(void);

/** The NULL-function. */
#define NULL_FUNC       ((generic_func)(NULL))

/**
 * Returns the generic function pointer from an IR operation.
 */
FIRM_API op_func get_generic_function_ptr(const ir_op *op);

/**
 * Store a generic function pointer into an IR operation.
 */
FIRM_API void set_generic_function_ptr(ir_op *op, op_func func);

/**
 * Return the irop flags of an IR opcode.
 */
FIRM_API irop_flags get_op_flags(const ir_op *op);

/**
 * The hash operation.
 * This operation calculates a hash value for a given IR node.
 */
typedef unsigned (*hash_func)(const ir_node *self);

/**
 * The compute value operation.
 * This operation evaluates an IR node into a tarval if possible,
 * returning tarval_bad otherwise.
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
 * and returns 0 if the attributes are identical, 1 if they differ.
 */
typedef int (*node_cmp_attr_func)(const ir_node *a, const ir_node *b);

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
 * Return the type attribute of the node self.
 */
typedef ir_type *(*get_type_attr_func)(const ir_node *self);

/**
 * The get_entity_attr operation. Used to traverse all entities that can be
 * accessed from an ir_graph.
 * Return the entity attribute of the node self.
 */
typedef ir_entity *(*get_entity_attr_func)(const ir_node *self);

/**
 * The verify_node operation.
 * Return non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_node_func)(const ir_node *node);

/**
 * The verify_node operation for Proj(X).
 * Return non-zero if the node verification is ok, else 0.
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
typedef void (*dump_node_func)(FILE *out, ir_node *self, dump_reason_t reason);

/**
 * io_op Operations.
 */
typedef struct {
	hash_func             hash;                 /**< Calculate a hash value for an IR node. */
	computed_value_func   computed_value;       /**< Evaluates a node into a tarval if possible. */
	computed_value_func   computed_value_Proj;  /**< Evaluates a Proj node into a tarval if possible. */
	equivalent_node_func  equivalent_node;      /**< Optimizes the node by returning an equivalent one. */
	equivalent_node_func  equivalent_node_Proj; /**< Optimizes the Proj node by returning an equivalent one. */
	transform_node_func   transform_node;       /**< Optimizes the node by transforming it. */
	equivalent_node_func  transform_node_Proj;  /**< Optimizes the Proj node by transforming it. */
	node_cmp_attr_func    node_cmp_attr;        /**< Compares two node attributes. */
	reassociate_func      reassociate;          /**< Reassociate a tree. */
	copy_attr_func        copy_attr;            /**< Copy node attributes. */
	get_type_attr_func    get_type_attr;        /**< Return the type attribute of a node. */
	get_entity_attr_func  get_entity_attr;      /**< Return the entity attribute of a node. */
	verify_node_func      verify_node;          /**< Verify the node. */
	verify_proj_node_func verify_proj_node;     /**< Verify the Proj node. */
	dump_node_func        dump_node;            /**< Dump a node. */
	op_func               generic;              /**< A generic function pointer. */
	const arch_irn_ops_t *be_ops;               /**< callbacks used by the backend. */
} ir_op_ops;

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
 * @param ops       operations for this opcode, iff NULL default operations are used
 * @param attr_size attribute size for this IR operation
 *
 * @return The generated IR operation.
 *
 * This function can create all standard Firm opcode as well as new ones.
 * The behavior of new opcode depends on the operations \c ops and the \c flags.
 */
FIRM_API ir_op *new_ir_op(unsigned code, const char *name, op_pin_state p,
                          unsigned flags, op_arity opar, int op_index,
                          size_t attr_size, const ir_op_ops *ops);

/**
 * Set proj-number for X_regular and X_except projs of fragile nodes.
 * Note: should only be used immediately after new_ir_op
 */
FIRM_API void ir_op_set_fragile_indices(ir_op *op, int fragile_mem_index,
                                        int pn_x_regular, int pn_x_except);

/** Returns the ir_op_ops of an ir_op. */
FIRM_API const ir_op_ops *get_op_ops(const ir_op *op);

#include "end.h"

#endif
