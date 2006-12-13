/*
 * Project:     libFIRM
 * File name:   ir/ir/irop.h
 * Purpose:     Representation of opcode of intermediate operation.
 * Author:      Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irop.h
 *
 * Operators of firm nodes.
 *
 * @author Christian Schaefer
 *
 *  This module specifies the opcodes possible for ir nodes.  Their
 *  definition is close to the operations specified in UKA Tech-Report
 *  1999-14
 */
#ifndef _FIRM_IR_IROP_H_
#define _FIRM_IR_IROP_H_

#include "firm_types.h"

#include <stdio.h>
#include "ident.h"

/** The allowed parities */
typedef enum {
  oparity_invalid = 0,
  oparity_unary,              /**< an unary operator -- considering 'numeric' arguments. */
  oparity_binary,             /**< an binary operator  -- considering 'numeric' arguments.*/
  oparity_trinary,            /**< an trinary operator  -- considering 'numeric' arguments.*/
  oparity_zero,               /**< no operators, as e.g. Const. */
  oparity_variable,           /**< arity not fixed by opcode, but statically
                                   known.  E.g., number of arguments to call. */
  oparity_dynamic,            /**< arity depends on state of firm representation.
                                   Can change by optimizations...
                                   We must allocate a dynamic in array for the node! */
  oparity_any                 /**< other arity */
} op_arity;


/** The irop flags */
typedef enum {
  irop_flag_none        = 0x00000000, /**< Nothing. */
  irop_flag_labeled     = 0x00000001, /**< If set, output edge labels on in-edges in vcg graph. */
  irop_flag_commutative = 0x00000002, /**< This operation is commutative. */
  irop_flag_cfopcode    = 0x00000004, /**< This operation is a control flow operation. */
  irop_flag_ip_cfopcode = 0x00000008, /**< This operation manipulates the interprocedural control flow. */
  irop_flag_fragile     = 0x00000010, /**< Set if the operation can change the control flow because
                                           of an exception. */
  irop_flag_forking     = 0x00000020, /**< Forking control flow at this operation. */
  irop_flag_highlevel   = 0x00000040, /**< This operation is a pure high-level one and can be
                                           skipped in low-level optimizations. */
  irop_flag_constlike   = 0x00000080, /**< This operation has no arguments and is some
                                           kind of a constant. */
  irop_flag_always_opt  = 0x00000100, /**< This operation must always be optimized .*/
  irop_flag_keep        = 0x00000200, /**< This operation can be kept in End's keep-alive list. */
  irop_flag_start_block = 0x00000400, /**< This operation is always placed in the Start block. */
  irop_flag_machine     = 0x00000800, /**< This operation is a machine operation. */
  irop_flag_machine_op  = 0x00001000, /**< This operation is a machine operand. */
  irop_flag_user        = 0x00002000  /**< This flag and all higher ones are free for machine user. */
} irop_flags;

/** The opcodes of the libFirm predefined operations. */
typedef enum {
  iro_Block,
  iro_Start, iro_End, iro_Jmp, iro_IJmp, iro_Cond, iro_Return,
  iro_Const, iro_SymConst,
  iro_Sel,
  iro_Call, iro_Add, iro_Sub, iro_Minus, iro_Mul, iro_Quot, iro_DivMod,
  iro_Div,  iro_Mod, iro_Abs, iro_And, iro_Or, iro_Eor, iro_Not,
  iro_Cmp,  iro_Shl, iro_Shr, iro_Shrs, iro_Rot, iro_Conv, iro_Cast,
  iro_Carry, iro_Borrow,
  iro_Phi,
  iro_Load, iro_Store, iro_Alloc, iro_Free, iro_Sync,
  iro_Proj, iro_Tuple, iro_Id, iro_Bad, iro_Confirm,
  iro_Unknown, iro_Filter, iro_Break, iro_CallBegin, iro_EndReg, iro_EndExcept,
  iro_NoMem, iro_Mux, iro_Psi, iro_CopyB,
  iro_InstOf, iro_Raise, iro_Bound,
  iro_Pin,
  iro_MaxOpcode
} opcode;

#ifndef _IR_OP_TYPEDEF_
#define _IR_OP_TYPEDEF_
typedef struct ir_op ir_op;
#endif

extern ir_op *op_Block;           ir_op *get_op_Block     (void);

extern ir_op *op_Start;           ir_op *get_op_Start     (void);
extern ir_op *op_End;             ir_op *get_op_End       (void);
extern ir_op *op_Jmp;             ir_op *get_op_Jmp       (void);
extern ir_op *op_IJmp;            ir_op *get_op_IJmp      (void);
extern ir_op *op_Cond;            ir_op *get_op_Cond      (void);
extern ir_op *op_Return;          ir_op *get_op_Return    (void);
extern ir_op *op_Sel;             ir_op *get_op_Sel       (void);

extern ir_op *op_Const;           ir_op *get_op_Const     (void);
extern ir_op *op_SymConst;        ir_op *get_op_SymConst  (void);

extern ir_op *op_Call;            ir_op *get_op_Call      (void);
extern ir_op *op_Add;             ir_op *get_op_Add       (void);
extern ir_op *op_Sub;             ir_op *get_op_Sub       (void);
extern ir_op *op_Minus;           ir_op *get_op_Minus     (void);
extern ir_op *op_Mul;             ir_op *get_op_Mul       (void);
extern ir_op *op_Quot;            ir_op *get_op_Quot      (void);
extern ir_op *op_DivMod;          ir_op *get_op_DivMod    (void);
extern ir_op *op_Div;             ir_op *get_op_Div       (void);
extern ir_op *op_Mod;             ir_op *get_op_Mod       (void);
extern ir_op *op_Abs;             ir_op *get_op_Abs       (void);
extern ir_op *op_And;             ir_op *get_op_And       (void);
extern ir_op *op_Or;              ir_op *get_op_Or        (void);
extern ir_op *op_Eor;             ir_op *get_op_Eor       (void);
extern ir_op *op_Not;             ir_op *get_op_Not       (void);
extern ir_op *op_Cmp;             ir_op *get_op_Cmp       (void);
extern ir_op *op_Shl;             ir_op *get_op_Shl       (void);
extern ir_op *op_Shr;             ir_op *get_op_Shr       (void);
extern ir_op *op_Shrs;            ir_op *get_op_Shrs      (void);
extern ir_op *op_Rot;             ir_op *get_op_Rot       (void);
extern ir_op *op_Conv;            ir_op *get_op_Conv      (void);
extern ir_op *op_Cast;            ir_op *get_op_Cast      (void);
extern ir_op *op_Carry;           ir_op *get_op_Carry     (void);
extern ir_op *op_Borrow;          ir_op *get_op_Borrow    (void);

extern ir_op *op_Phi;             ir_op *get_op_Phi       (void);

extern ir_op *op_Load;            ir_op *get_op_Load      (void);
extern ir_op *op_Store;           ir_op *get_op_Store     (void);
extern ir_op *op_Alloc;           ir_op *get_op_Alloc     (void);
extern ir_op *op_Free;            ir_op *get_op_Free      (void);

extern ir_op *op_Sync;            ir_op *get_op_Sync      (void);

extern ir_op *op_Tuple;           ir_op *get_op_Tuple     (void);
extern ir_op *op_Proj;            ir_op *get_op_Proj      (void);
extern ir_op *op_Id;              ir_op *get_op_Id        (void);
extern ir_op *op_Bad;             ir_op *get_op_Bad       (void);
extern ir_op *op_Confirm;         ir_op *get_op_Confirm   (void);

extern ir_op *op_Unknown;         ir_op *get_op_Unknown   (void);
extern ir_op *op_Filter;          ir_op *get_op_Filter    (void);
extern ir_op *op_Break;           ir_op *get_op_Break     (void);
extern ir_op *op_CallBegin;       ir_op *get_op_CallBegin (void);
extern ir_op *op_EndReg;          ir_op *get_op_EndReg    (void);
extern ir_op *op_EndExcept;       ir_op *get_op_EndExcept (void);

extern ir_op *op_NoMem;           ir_op *get_op_NoMem     (void);
extern ir_op *op_Mux;             ir_op *get_op_Mux       (void);
extern ir_op *op_Psi;             ir_op *get_op_Psi       (void);
extern ir_op *op_CopyB;           ir_op *get_op_CopyB     (void);

extern ir_op *op_InstOf;          ir_op *get_op_InstOf    (void);
extern ir_op *op_Raise;           ir_op *get_op_Raise     (void);
extern ir_op *op_Bound;           ir_op *get_op_Bound     (void);

extern ir_op *op_Pin;             ir_op *get_op_Pin       (void);

/** Returns the ident for the opcode name */
ident *get_op_ident(const ir_op *op);

/** Returns the string for the opcode. */
const char *get_op_name(const ir_op *op);

/** Returns the enum for the opcode */
opcode get_op_code(const ir_op *op);

/** op_pin_state_pinned states */
typedef enum {
  op_pin_state_floats = 0,    /**< Nodes of this opcode can be placed in any basic block. */
  op_pin_state_pinned = 1,    /**< Nodes must remain in this basic block. */
  op_pin_state_exc_pinned,    /**< Node must be remain in this basic block if it can throw an
                                   exception, else can float. Used internally. */
  op_pin_state_mem_pinned     /**< Node must be remain in this basic block if it can throw an
                                   exception or uses memory, else can float. Used internally. */
} op_pin_state;

const char *get_op_pin_state_name(op_pin_state s);

/** gets pinned state of an opcode */
op_pin_state get_op_pinned(const ir_op *op);

/** Sets pinned in the opcode.  Setting it to floating has no effect
   for Block, Phi and control flow nodes. */
void set_op_pinned(ir_op *op, op_pin_state pinned);

/** Returns the next free IR opcode number, allows to register user ops */
unsigned get_next_ir_opcode(void);

/** Returns the next free n IR opcode number, allows to register a bunch of user ops */
unsigned get_next_ir_opcodes(unsigned num);

/**
 * A generic function pointer.
 */
typedef void (*op_func)(void);

/** The NULL-function. */
#define NULL_FUNC       ((generic_func)(NULL))

/**
 * Returns the generic function pointer from an ir operation.
 */
op_func get_generic_function_ptr(const ir_op *op);

/**
 * Store a generic function pointer into an ir operation.
 */
void set_generic_function_ptr(ir_op *op, op_func func);

/**
 * The compute value operation.
 * This operation evaluates an IR node into a tarval if possible,
 * returning tarval_bad otherwise.
 */
typedef tarval *(*computed_value_func)(ir_node *self);

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
 * Copy the node attributes from a 'old' node to a 'new' one.
 */
typedef void (*copy_attr_func)(const ir_node *old_node, ir_node *new_node);

/**
 * The get_type operation.
 * Return the type of the node self.
 */
typedef ir_type *(*get_type_func)(ir_node *self);

/**
 * The get_type_attr operation. Used to traverse all types that can be
 * accessed from an ir_graph.
 * Return the type attribute of the node self.
 */
typedef ir_type *(*get_type_attr_func)(ir_node *self);

/**
 * The get_entity_attr operation. Used to traverse all entities that can be
 * accessed from an ir_graph.
 * Return the entity attribute of the node self.
 */
typedef ir_entity *(*get_entity_attr_func)(ir_node *self);

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
  dump_node_opcode_txt,   /**< dump the opcode */
  dump_node_mode_txt,     /**< dump the mode */
  dump_node_nodeattr_txt, /**< dump node attributes to be shown in the label */
  dump_node_info_txt      /**< dump node attributes into info1 */
} dump_reason_t;

/**
 * The dump_node operation.
 * Writes several informations requested by reason to
 * an output file
 */
typedef int (*dump_node_func)(ir_node *self, FILE *F, dump_reason_t reason);

/**
 * io_op Operations.
 */
typedef struct {
  computed_value_func   computed_value;   /**< evaluates a node into a tarval if possible. */
  equivalent_node_func  equivalent_node;  /**< optimizes the node by returning an equivalent one. */
  transform_node_func   transform_node;   /**< optimizes the node by transforming it. */
  node_cmp_attr_func    node_cmp_attr;    /**< compares two node attributes. */
  reassociate_func      reassociate;      /**< reassociate a tree */
  copy_attr_func        copy_attr;        /**< copy node attributes */
  get_type_func         get_type;         /**< return the type of a node */
  get_type_attr_func    get_type_attr;    /**< return the type attribute of a node */
  get_entity_attr_func  get_entity_attr;  /**< return the entity attribute of a node */
  verify_node_func      verify_node;      /**< verify the node */
  verify_proj_node_func verify_proj_node; /**< verify the Proj node */
  dump_node_func        dump_node;        /**< dump a node */
  op_func               generic;          /**< a generic function */
} ir_op_ops;

/**
 * Creates a new ir operation.
 *
 * @param code      the opcode, one of type \c opcode
 * @param name      the printable name of this opcode
 * @param p         whether operations of this opcode are op_pin_state_pinned or floating
 * @param flags     a bitmask of irop_flags describing the behavior of the ir operation
 * @param opar      the parity of this ir operation
 * @param op_index  if the parity is oparity_unary, oparity_binary or oparity_trinary the index
 *                  of the left operand
 * @param ops       operations for this opcode, iff NULL default operations are used
 * @param attr_size attribute size for this ir operation
 *
 * @return The generated ir operation.
 *
 * This function can create all standard Firm opcode as well as new ones.
 * The behavior of new opcode depends on the operations \c ops and the \c flags.
 */
ir_op * new_ir_op(opcode code, const char *name, op_pin_state p,
		   unsigned flags, op_arity opar, int op_index, size_t attr_size,
       const ir_op_ops *ops);

/** Returns the ir_op_ops of an ir_op. */
const ir_op_ops *get_op_ops(const ir_op *op);

#endif /* _FIRM_IR_IROP_H_ */
