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
	irop_flag_none         = 0x00000000, /**< Nothing. */
	irop_flag_labeled      = 0x00000001, /**< If set, output edge labels on in-edges in vcg graph. */
	irop_flag_commutative  = 0x00000002, /**< This operation is commutative. */
	irop_flag_cfopcode     = 0x00000004, /**< This operation is a control flow operation. */
	irop_flag_ip_cfopcode  = 0x00000008, /**< This operation manipulates the interprocedural control flow. */
	irop_flag_fragile      = 0x00000010, /**< Set if the operation can change the control flow because
	                                          of an exception. */
	irop_flag_forking      = 0x00000020, /**< Forking control flow at this operation. */
	irop_flag_highlevel    = 0x00000040, /**< This operation is a pure high-level one and can be
	                                          skipped in low-level optimizations. */
	irop_flag_constlike    = 0x00000080, /**< This operation has no arguments and is some
	                                          kind of a constant. */
	irop_flag_always_opt   = 0x00000100, /**< This operation must always be optimized .*/
	irop_flag_keep         = 0x00000200, /**< This operation can be kept in End's keep-alive list. */
	irop_flag_start_block  = 0x00000400, /**< This operation is always placed in the Start block. */
	irop_flag_uses_memory  = 0x00000800, /**< This operation has a memory input and may change the memory state. */
	irop_flag_dump_noblock = 0x00001000, /**< node should be dumped outside any blocks */
	irop_flag_dump_noinput = 0x00002000, /**< node is a placeholder for "no input" */
	irop_flag_machine      = 0x00010000, /**< This operation is a machine operation. */
	irop_flag_machine_op   = 0x00020000, /**< This operation is a machine operand. */
	irop_flag_cse_neutral  = 0x00040000, /**< This operation is CSE neutral to its users. */
	irop_flag_user         = 0x00080000  /**< This flag and all higher ones are free for machine user. */
} irop_flags;

/** The opcodes of the libFirm predefined operations. */
typedef enum {
	iro_Block,
	iro_First = iro_Block,
	iro_Start, iro_End, iro_Jmp, iro_IJmp, iro_Cond, iro_Return,
	iro_Const, iro_SymConst,
	iro_Sel,
	iro_Call, iro_Add, iro_Sub, iro_Minus, iro_Mul, iro_Mulh, iro_Quot, iro_DivMod,
	iro_Div,  iro_Mod, iro_Abs, iro_And, iro_Or, iro_Eor, iro_Not,
	iro_Cmp,  iro_Shl, iro_Shr, iro_Shrs, iro_Rotl, iro_Conv, iro_Cast,
	iro_Carry, iro_Borrow,
	iro_Phi,
	iro_Load, iro_Store, iro_Alloc, iro_Free, iro_Sync,
	iro_Proj, iro_Tuple, iro_Id, iro_Bad, iro_Confirm,
	iro_Unknown, iro_Filter, iro_Break, iro_CallBegin, iro_EndReg, iro_EndExcept,
	iro_NoMem, iro_Mux, iro_CopyB,
	iro_InstOf, iro_Raise, iro_Bound,
	iro_Pin,
	iro_ASM, iro_Builtin,
	iro_Dummy,
	iro_Anchor,
	/* first not middleend node number */
	iro_Last = iro_Anchor,
	/* first backend node number */
	beo_First,
	/* backend specific nodes */
	beo_Spill = beo_First,
	beo_Reload,
	beo_Perm,
	beo_MemPerm,
	beo_Copy,
	beo_Keep,
	beo_CopyKeep,
	beo_Call,
	beo_Return,
	beo_AddSP,
	beo_SubSP,
	beo_IncSP,
	beo_Start,
	beo_FrameAddr,
	beo_Barrier,
	/* last backend node number */
	beo_Last = beo_Barrier,
	/* first unfixed number. Dynamic node numbers start here */
	iro_MaxOpcode
} ir_opcode;

FIRM_API ir_op *op_Abs;
FIRM_API ir_op *op_Add;
FIRM_API ir_op *op_Alloc;
FIRM_API ir_op *op_Anchor;
FIRM_API ir_op *op_And;
FIRM_API ir_op *op_ASM;
FIRM_API ir_op *op_Bad;
FIRM_API ir_op *op_Block;
FIRM_API ir_op *op_Borrow;
FIRM_API ir_op *op_Bound;
FIRM_API ir_op *op_Break;
FIRM_API ir_op *op_Builtin;
FIRM_API ir_op *op_Call;
FIRM_API ir_op *op_CallBegin;
FIRM_API ir_op *op_Carry;
FIRM_API ir_op *op_Cast;
FIRM_API ir_op *op_Cmp;
FIRM_API ir_op *op_Cond;
FIRM_API ir_op *op_Confirm;
FIRM_API ir_op *op_Const;
FIRM_API ir_op *op_Conv;
FIRM_API ir_op *op_CopyB;
FIRM_API ir_op *op_Div;
FIRM_API ir_op *op_DivMod;
FIRM_API ir_op *op_Dummy;
FIRM_API ir_op *op_End;
FIRM_API ir_op *op_EndExcept;
FIRM_API ir_op *op_EndReg;
FIRM_API ir_op *op_Eor;
FIRM_API ir_op *op_Filter;
FIRM_API ir_op *op_Free;
FIRM_API ir_op *op_Id;
FIRM_API ir_op *op_IJmp;
FIRM_API ir_op *op_InstOf;
FIRM_API ir_op *op_Jmp;
FIRM_API ir_op *op_Load;
FIRM_API ir_op *op_Minus;
FIRM_API ir_op *op_Mod;
FIRM_API ir_op *op_Mul;
FIRM_API ir_op *op_Mulh;
FIRM_API ir_op *op_Mux;
FIRM_API ir_op *op_NoMem;
FIRM_API ir_op *op_Not;
FIRM_API ir_op *op_Or;
FIRM_API ir_op *op_Phi;
FIRM_API ir_op *op_Pin;
FIRM_API ir_op *op_Proj;
FIRM_API ir_op *op_Quot;
FIRM_API ir_op *op_Raise;
FIRM_API ir_op *op_Return;
FIRM_API ir_op *op_Rotl;
FIRM_API ir_op *op_Sel;
FIRM_API ir_op *op_Shl;
FIRM_API ir_op *op_Shrs;
FIRM_API ir_op *op_Shr;
FIRM_API ir_op *op_Start;
FIRM_API ir_op *op_Store;
FIRM_API ir_op *op_Sub;
FIRM_API ir_op *op_SymConst;
FIRM_API ir_op *op_Sync;
FIRM_API ir_op *op_Tuple;
FIRM_API ir_op *op_Unknown;

FIRM_API ir_op *get_op_Abs       (void);
FIRM_API ir_op *get_op_Add       (void);
FIRM_API ir_op *get_op_Alloc     (void);
FIRM_API ir_op *get_op_Anchor    (void);
FIRM_API ir_op *get_op_And       (void);
FIRM_API ir_op *get_op_ASM       (void);
FIRM_API ir_op *get_op_Bad       (void);
FIRM_API ir_op *get_op_Block     (void);
FIRM_API ir_op *get_op_Borrow    (void);
FIRM_API ir_op *get_op_Bound     (void);
FIRM_API ir_op *get_op_Break     (void);
FIRM_API ir_op *get_op_Builtin   (void);
FIRM_API ir_op *get_op_CallBegin (void);
FIRM_API ir_op *get_op_Call      (void);
FIRM_API ir_op *get_op_Carry     (void);
FIRM_API ir_op *get_op_Cast      (void);
FIRM_API ir_op *get_op_Cmp       (void);
FIRM_API ir_op *get_op_Cond      (void);
FIRM_API ir_op *get_op_Confirm   (void);
FIRM_API ir_op *get_op_Const     (void);
FIRM_API ir_op *get_op_Conv      (void);
FIRM_API ir_op *get_op_CopyB     (void);
FIRM_API ir_op *get_op_DivMod    (void);
FIRM_API ir_op *get_op_Div       (void);
FIRM_API ir_op *get_op_Dummy     (void);
FIRM_API ir_op *get_op_EndExcept (void);
FIRM_API ir_op *get_op_EndReg    (void);
FIRM_API ir_op *get_op_End       (void);
FIRM_API ir_op *get_op_Eor       (void);
FIRM_API ir_op *get_op_Filter    (void);
FIRM_API ir_op *get_op_Free      (void);
FIRM_API ir_op *get_op_Id        (void);
FIRM_API ir_op *get_op_IJmp      (void);
FIRM_API ir_op *get_op_InstOf    (void);
FIRM_API ir_op *get_op_Jmp       (void);
FIRM_API ir_op *get_op_Load      (void);
FIRM_API ir_op *get_op_Minus     (void);
FIRM_API ir_op *get_op_Mod       (void);
FIRM_API ir_op *get_op_Mulh      (void);
FIRM_API ir_op *get_op_Mul       (void);
FIRM_API ir_op *get_op_Mux       (void);
FIRM_API ir_op *get_op_NoMem     (void);
FIRM_API ir_op *get_op_Not       (void);
FIRM_API ir_op *get_op_Or        (void);
FIRM_API ir_op *get_op_Phi       (void);
FIRM_API ir_op *get_op_Pin       (void);
FIRM_API ir_op *get_op_Proj      (void);
FIRM_API ir_op *get_op_Quot      (void);
FIRM_API ir_op *get_op_Raise     (void);
FIRM_API ir_op *get_op_Return    (void);
FIRM_API ir_op *get_op_Rotl      (void);
FIRM_API ir_op *get_op_Sel       (void);
FIRM_API ir_op *get_op_Shl       (void);
FIRM_API ir_op *get_op_Shrs      (void);
FIRM_API ir_op *get_op_Shr       (void);
FIRM_API ir_op *get_op_Start     (void);
FIRM_API ir_op *get_op_Store     (void);
FIRM_API ir_op *get_op_Sub       (void);
FIRM_API ir_op *get_op_SymConst  (void);
FIRM_API ir_op *get_op_Sync      (void);
FIRM_API ir_op *get_op_Tuple     (void);
FIRM_API ir_op *get_op_Unknown   (void);

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
typedef tarval *(*computed_value_func)(const ir_node *self);

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
typedef int (*node_cmp_attr_func)(ir_node *a, ir_node *b);

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
 * The get_type operation.
 * Return the type of the node self.
 */
typedef ir_type *(*get_type_func)(const ir_node *self);

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
typedef int (*verify_node_func)(ir_node *self, ir_graph *irg);

/**
 * The verify_node operation for Proj(X).
 * Return non-zero if the node verification is ok, else 0.
 * Depending on the node verification settings, may even assert.
 *
 * @see do_node_verification()
 */
typedef int (*verify_proj_node_func)(ir_node *self, ir_node *proj);

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
	get_type_func         get_type;             /**< Return the type of a node. */
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

/** Returns the ir_op_ops of an ir_op. */
FIRM_API const ir_op_ops *get_op_ops(const ir_op *op);

#include "end.h"

#endif
