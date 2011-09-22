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
 * @brief   Representation of an intermediate operation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_IR_IRNODE_H
#define FIRM_IR_IRNODE_H

#include <stddef.h>

#include "firm_common.h"
#include "typerep.h"
#include "irop.h"
#include "irmode.h"
#include "begin.h"
#include "nodeops.h"

/**
 * @defgroup ir_node Declarations of an ir node.
 *
 * The type definition of ir_node is also in irgraph.h to resolve
 *  recursion between irnode.h and irgraph.h
 *
 * ir_node - a datatype representing a Firm node
 *
 *  The common fields are:
 *
 *  - arity     - The number of predecessors in the Firm graph.
 *  - in        - A list with the predecessors in the Firm graph.  There are
 *                routines to access individual elements and to obtain the
 *                array.  The method returning the array should not be used.
 *  - mode      - The mode of the node.  There are routines to get the mode
 *                but also to access the mode's fields directly.
 *  - opcode    - The opcode of the node. There are routines to get the opcode
 *                but also to access the opcode's fields directly.
 *  - node_nr   - A unique number for the node.  Available only if debugging
 *                is turned on.
 * @{
 */

/**
 * Checks whether a pointer points to a ir node. This is guessed by looking
 * at the few bytes of the thing. Most things used in firm have a firm_kind
 * attribute there. This function might falsely return true though for things
 * without a firm_kind at the beginning.
 *
 * @param thing   an arbitrary pointer
 * @return        non-zero if the thing is a ir mode, else zero
 */
FIRM_API int is_ir_node(const void *thing);

/**
 * Returns the number of predecessors without the block predecessor.
 *
 * @param node   the IR-node
 */
FIRM_API int get_irn_arity(const ir_node *node);

/**
 * Get the n-th predecessor of a node.
 * This function removes Id predecessors.
 */
FIRM_API ir_node *get_irn_n(const ir_node *node, int n);

/**
 * Replaces the old in array by a new one that will contain the ins given in
 * the parameters. Conserves the block predecessor. It copies the array passed.
 * This function is necessary to adjust in arrays of blocks, calls and phis.
 * Assumes that current_ir_graph is set to the graph containing "node".
 * "in" must contain all predecessors except the block that are required for
 * the nodes opcode. */
FIRM_API void set_irn_in(ir_node *node, int arity, ir_node *in[]);

/**
 * Add a artificial dependency to the node.
 * The dependency is only inserted if it is not there already.
 * This is only allowed in phase_backend!
 *
 * @param node The node.
 * @param dep  The dependency target.
 *
 * @return The index in the array (get_irn_dep with that index returns @p dep).
 */
FIRM_API int add_irn_dep(ir_node *node, ir_node *dep);

/**
 * Copy all dependencies from a node to another.
 * This is only allowed in phase_backend!
 *
 * @param tgt The node which should be enriched.
 * @param src The node whose dependencies shall be copied.
 */
FIRM_API void add_irn_deps(ir_node *tgt, ir_node *src);

/**
 * Get the length of the dependency array.
 * @param node The node.
 * @return The length of the dependency array or 0 if it has not yet been allocated.
 */
FIRM_API int get_irn_deps(const ir_node *node);

/**
 * Get an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @return The node at that position.
 */
FIRM_API ir_node *get_irn_dep(const ir_node *node, int pos);

/**
 * Set an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @param dep  The dependency target.
 */
FIRM_API void set_irn_dep(ir_node *node, int pos, ir_node *dep);

/** Replace the n-th predecessor of a node with a new one. */
FIRM_API void set_irn_n(ir_node *node, int n, ir_node *in);
/**
 * Appends a new predecessor to a node. This only works for nodes with
 * variable arity!
 * @returns   the number of the new input
 */
FIRM_API int add_irn_n(ir_node *node, ir_node *in);
/** Remove predecessor i from Sync n */
FIRM_API void del_Sync_n(ir_node *n, int i);
/** Sets the mode struct of node.  */
FIRM_API void set_irn_mode(ir_node *node, ir_mode *mode);
/** Gets the mode struct of a node.  */
FIRM_API ir_mode *get_irn_mode(const ir_node *node);
/** Gets the opcode struct of the node. */
FIRM_API ir_op *get_irn_op(const ir_node *node);
/** Sets the opcode struct of the node. */
FIRM_API void set_irn_op(ir_node *node, ir_op *op);
/** Gets the opcode-enum of the node. */
FIRM_API unsigned get_irn_opcode(const ir_node *node);
/** Get the string representation of the opcode. */
FIRM_API const char *get_irn_opname(const ir_node *node);
/** Get the ident for a string representation of the opcode. */
FIRM_API ident *get_irn_opident(const ir_node *node);
/** If arg is an argument of the node, returns its position, -1 otherwise */
FIRM_API int get_irn_pred_pos(ir_node *node, ir_node *arg);
/** Gets the visited counter of a node. */
FIRM_API ir_visited_t get_irn_visited(const ir_node *node);
/** Sets the visited counter of a node. */
FIRM_API void set_irn_visited(ir_node *node, ir_visited_t visited);
/** Sets visited to get_irg_visited(current_ir_graph). */
FIRM_API void mark_irn_visited(ir_node *node);
/** Returns 1 if visited >= get_irg_visited(current_ir_graph). */
FIRM_API int irn_visited(const ir_node *node);
/** Returns 1 if visited >= get_irg_visited(current_ir_graph). Marks the node
 * visited, if it was not. */
FIRM_API int irn_visited_else_mark(ir_node *node);

/**
 * Sets the link of a node.
 * Only allowed if the graph is NOT in phase_building.
 */
FIRM_API void set_irn_link(ir_node *node, void *link);

/** Returns the link of a node.  */
FIRM_API void *get_irn_link(const ir_node *node);

/** Returns the ir_graph this node belongs to. */
FIRM_API ir_graph *get_irn_irg(const ir_node *node);

/** Outputs a unique number for this node if libFIRM is compiled for
   debugging, (configure with --enable-debug) else returns address
   of node cast to long. */
FIRM_API long get_irn_node_nr(const ir_node *node);

/** Returns the pinned state of a node.
 *
 *  Returns whether the node _always_ must be pinned.
 *  I.e., the node is not floating after global cse.
 *
 * @returns Either state op_pin_state_pinned or op_pin_state_floats.
 */
FIRM_API op_pin_state get_irn_pinned(const ir_node *node);

/** Set pin state for nodes with op pin state op_pin_state_exc_pinned */
FIRM_API void set_irn_pinned(ir_node *node, op_pin_state state);

/** Returns whether the node is currently pinned.
 *
 * If get_irn_pinned returns op_pin_state_floats and the graph the
 * node belongs to is in state op_poin_state_floats then this function
 * returns 'floats', else 'pinned'.
 */
FIRM_API op_pin_state is_irn_pinned_in_irg(const ir_node *node);

/**
 * IR node constructor.
 * Create a new IR node in irg, with an op, mode, arity and
 * some incoming IR nodes.
 * This constructor is used in every specific IR node constructor.
 *
 * @param db    Debug info.
 * @param irg   IR-graph on with this new node should be constructed.
 * @param block The block the new node belongs to
 * @param op    The opcode of the new node.
 * @param mode  The mode of the new node.
 * @param arity The arity of the new node, <0 if can be changed dynamically.
 * @param in    An array of arity predecessor nodes.
 */
FIRM_API ir_node *new_ir_node(dbg_info *db, ir_graph *irg, ir_node *block,
                              ir_op *op, ir_mode *mode,
                              int arity, ir_node *const *in);

/**
 * Return the block the node belongs to.  This is only
 * possible for pinned nodes or if the graph is in pinned state.
 * Otherwise the block may be incorrect.  This condition is
 * now checked by an assertion.
 *
 * This works for all except Block.  It can return Blocks or the Bad node.
 *
 * To express the difference to access routines that work for all
 * nodes we use infix "nodes" and do not name this function
 * get_irn_block(). */
FIRM_API ir_node *get_nodes_block(const ir_node *node);

/** Sets the Block of a node. */
FIRM_API void set_nodes_block(ir_node *node, ir_node *block);

/** Test whether arbitrary node is frame pointer.
 *
 * Test whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
FIRM_API ir_type *is_frame_pointer(const ir_node *n);

/** Return the number of control flow predecessors of a block. */
FIRM_API int get_Block_n_cfgpreds(const ir_node *block);
/** Return the control flow predecessor of a block at a given position. */
FIRM_API ir_node *get_Block_cfgpred(const ir_node *block, int pos);
/** Set the control flow predecessor of a block at a given position. */
FIRM_API void set_Block_cfgpred(ir_node *block, int pos, ir_node *pred);

/**
 * Return the position of the predecessor block pred in the inputs
 * of the block block.
 *
 * @param block  the block
 * @param pred   a predecessor block of block
 *
 * @return the position of pred in block or -1
 *
 * @note When using the old extended basic block form for blocks
 * with exception exists, a predecessor block might have more
 * than one position. In that case it is not specified, with is returned.
 */
FIRM_API int get_Block_cfgpred_pos(const ir_node *block, const ir_node *pred);

/** Get the predecessor block.
 *
 *  Returns the block corresponding to the predecessor pos of block.
 *
 *  There are several ambiguities we resolve with this function:
 *  - The direct predecessor can be a Proj, which is not pinned.
 *    We walk from the predecessor to the next pinned node
 *    (skip_Proj) and return the block that node is in.
 *  - If we encounter the Bad node, this function does not return
 *    Start block, but the Bad node.
 */
FIRM_API ir_node *get_Block_cfgpred_block(const ir_node *node, int pos);

/** Return the matured flag of a block */
FIRM_API int get_Block_matured(const ir_node *block);
/** set the matured flag of a block. */
FIRM_API void set_Block_matured(ir_node *block, int matured);

/** A visited flag only for block nodes.
 *  @see also: get_irn_visited() inc_irg_visited() inc_irg_block_visited()*/
FIRM_API ir_visited_t get_Block_block_visited(const ir_node *block);
/** set block visited flag */
FIRM_API void set_Block_block_visited(ir_node *block, ir_visited_t visit);

/** mark a block as visited by setting its visited counter */
FIRM_API void mark_Block_block_visited(ir_node *node);
/** returns 1 if a block is marked as visited */
FIRM_API int Block_block_visited(const ir_node *node);

/** Returns the extended basic block a block belongs to. */
FIRM_API ir_extblk *get_Block_extbb(const ir_node *block);
/** Sets the extended basic block a block belongs to. */
FIRM_API void set_Block_extbb(ir_node *block, ir_extblk *extblk);
/** Returns the ir_graph this Block belongs to. */
FIRM_API ir_graph *get_Block_irg(const ir_node *block);
/** Returns non-zero if the block has an entity assigned */
FIRM_API int has_Block_entity(const ir_node *block);
/** Returns the entity for a Block */
FIRM_API ir_entity *get_Block_entity(const ir_node *block);
/** Returns the entity for a Block (creating it if necessary) */
FIRM_API ir_entity *create_Block_entity(ir_node *block);
/** Set a new entity for a block */
FIRM_API void set_Block_entity(ir_node *block, ir_entity *entity);
/** Gets the head of the Phi list for this block. */
FIRM_API ir_node *get_Block_phis(const ir_node *block);
/** Sets the head of the Phi list for this block. */
FIRM_API void set_Block_phis(ir_node *block, ir_node *phi);
/** Add a Phi node to the list of Block Phi's. */
FIRM_API void add_Block_phi(ir_node *block, ir_node *phi);
/** Get the Block mark (single bit). */
FIRM_API unsigned get_Block_mark(const ir_node *block);
/** Set the Block mark (single bit). */
FIRM_API void set_Block_mark(ir_node *block, unsigned mark);

/** Return the number of Keep alive node. */
FIRM_API int get_End_n_keepalives(const ir_node *end);
/** Return the Keep alive node a position pos. */
FIRM_API ir_node *get_End_keepalive(const ir_node *end, int pos);
/** Keep alive dedicated nodes.  These must be either PhiM or Block nodes. */
FIRM_API void add_End_keepalive(ir_node *end, ir_node *ka);
/** Set the Keep alive node at position pos. */
FIRM_API void set_End_keepalive(ir_node *end, int pos, ir_node *ka);

/**
 * Set new keep-alives.
 * Beware: This might be an expensive operation if dynamic edges are enabled,
 * so avoid it in the backend.
 */
FIRM_API void set_End_keepalives(ir_node *end, int n, ir_node *in[]);

/** Remove irn from the keep-alive set. */
FIRM_API void remove_End_keepalive(ir_node *end, ir_node *irn);

/** Remove Bads, NoMem and doublets from the keep-alive set. */
FIRM_API void remove_End_Bads_and_doublets(ir_node *end);

/** Some parts of the End node are allocated separately -- their memory
   is not recovered by dead_node_elimination if a End node is dead.
   free_End() frees these data structures. */
FIRM_API void free_End(ir_node *end);

FIRM_API ir_node **get_Return_res_arr(ir_node *node);
FIRM_API size_t    get_Return_n_ress(const ir_node *node);
FIRM_API ir_node  *get_Return_res(const ir_node *node, int pos);
FIRM_API void      set_Return_res(ir_node *node, int pos, ir_node *res);

/** Return non-zero if the given Const node represents the 0 constant. */
FIRM_API int is_Const_null(const ir_node *node);

/** Return non-zero if the given Const node represents the 1 constant. */
FIRM_API int is_Const_one(const ir_node *node);

/** Return non-zero if the given Const node represents the constant with all bits set. */
FIRM_API int is_Const_all_one(const ir_node *node);

/** Returns true if a node is a Conv node with strict attribute set. */
FIRM_API int is_strictConv(const ir_node *node);

/** Returns true if node is a SymConst node with kind symconst_addr_ent. */
FIRM_API int is_SymConst_addr_ent(const ir_node *node);

/** Returns non-zero if s symconst kind has a type attribute */
#define SYMCONST_HAS_TYPE(kind) ((kind) <= symconst_type_align)

/** Returns non-zero if s symconst kind has an entity attribute */
#define SYMCONST_HAS_ENT(kind) ((kind) == symconst_addr_ent || (kind) == symconst_ofs_ent)

/** Returns non-zero if s symconst kind has an enum_const attribute */
#define SYMCONST_HAS_ENUM(kind) ((kind) == symconst_enum_const)

/** Get the kind of the SymConst. */
FIRM_API symconst_kind get_SymConst_kind(const ir_node *node);
/** Set the kind of the SymConst. */
FIRM_API void          set_SymConst_kind(ir_node *node, symconst_kind num);

/** Only to access SymConst of kind type_tag or size.  Else assertion: */
FIRM_API ir_type  *get_SymConst_type(const ir_node *node);
FIRM_API void     set_SymConst_type(ir_node *node, ir_type *tp);

/** Only to access SymConst of kind addr_ent.  Else assertion: */
FIRM_API ir_entity *get_SymConst_entity(const ir_node *node);
FIRM_API void       set_SymConst_entity(ir_node *node, ir_entity *ent);

/** Only to access SymConst of kind symconst_enum_const.  Else assertion: */
FIRM_API ir_enum_const *get_SymConst_enum(const ir_node *node);
FIRM_API void           set_SymConst_enum(ir_node *node, ir_enum_const *ec);

FIRM_API union symconst_symbol get_SymConst_symbol(const ir_node *node);
FIRM_API void                  set_SymConst_symbol(ir_node *node,
                                                   union symconst_symbol sym);

FIRM_API ir_node   **get_Sel_index_arr(ir_node *node);
FIRM_API int        get_Sel_n_indexs(const ir_node *node);
FIRM_API ir_node   *get_Sel_index(const ir_node *node, int pos);
FIRM_API void       set_Sel_index(ir_node *node, int pos, ir_node *index);

FIRM_API ir_node **get_Call_param_arr(ir_node *node);
/** Gets the number of parameters of a call. */
FIRM_API size_t   get_Call_n_params(const ir_node *node);
/** Gets the call parameter at position pos. */
FIRM_API ir_node *get_Call_param(const ir_node *node, int pos);
/** Sets the call parameter at position pos. */
FIRM_API void     set_Call_param(ir_node *node, int pos, ir_node *param);

/** Set, get and remove the callee information for a Call node.
 *
 *  The callee information lists all method entities that can be called
 *  from this node.  If the address expression can not be analyzed fully,
 *  e.g., as entities can be called that are not in the compilation unit,
 *  the array contains the unknown_entity.  The array contains only entities
 *  with peculiarity_existent, but with all kinds of visibility.  The entities
 *  not necessarily contain an irg.
 *
 *  The array is only accessible if callee information is valid.  See flag
 *  in graph.
 *
 *  The memory allocated for the array is managed automatically, i.e., it must
 *  not be freed if the Call node is removed from the graph.
 *
 *  @param node A Call node.
 */
FIRM_API int        Call_has_callees(const ir_node *node);
FIRM_API size_t     get_Call_n_callees(const ir_node *node);
FIRM_API ir_entity *get_Call_callee(const ir_node *node, size_t pos);

/** Set the full callee array.
 *
 *  The passed array is copied. Assumes current_ir_graph set properly! */
FIRM_API void set_Call_callee_arr(ir_node *node, size_t n, ir_entity **arr);
FIRM_API void remove_Call_callee_arr(ir_node *node);

FIRM_API ir_node         **get_Builtin_param_arr(ir_node *node);
/** Gets the number of parameters of a Builtin. */
FIRM_API int             get_Builtin_n_params(const ir_node *node);
/** Gets the Builtin parameter at position pos. */
FIRM_API ir_node         *get_Builtin_param(const ir_node *node, int pos);
/** Sets the Builtin parameter at position pos. */
FIRM_API void            set_Builtin_param(ir_node *node, int pos, ir_node *param);
/** Returns a human readable string for the ir_builtin_kind. */
FIRM_API const char *get_builtin_kind_name(ir_builtin_kind kind);

FIRM_API int      is_unop(const ir_node *node);
FIRM_API ir_node *get_unop_op(const ir_node *node);
FIRM_API void     set_unop_op(ir_node *node, ir_node *op);
FIRM_API int      is_binop(const ir_node *node);
FIRM_API ir_node *get_binop_left(const ir_node *node);
FIRM_API void     set_binop_left(ir_node *node, ir_node *left);
FIRM_API ir_node *get_binop_right(const ir_node *node);
FIRM_API void     set_binop_right(ir_node *node, ir_node *right);

/**
 * Test wether a node is the X_except Proj of a fragile operation
 */
FIRM_API int      is_x_except_Proj(const ir_node *node);

/**
 * Test wether a node is the X_regular Proj of a fragile operation
 */
FIRM_API int      is_x_regular_Proj(const ir_node *node);

/**
 * Set throws exception attribute of a fragile node
 * @p throws_exception must be 0 or 1
 */
FIRM_API void     ir_set_throws_exception(ir_node *node, int throws_exception);

/**
 * Returns throws_exception attribute of a fragile node
 */
FIRM_API int      ir_throws_exception(const ir_node *node);

/** returns the name of an ir_relation */
FIRM_API const char *get_relation_string(ir_relation relation);

/** Calculates the negated (Complement(R)) relation, i.e. "<" --> ">=" */
FIRM_API ir_relation get_negated_relation(ir_relation relation);

/** Calculates the inversed (R^-1) relation, i.e., "<" --> ">" */
FIRM_API ir_relation get_inversed_relation(ir_relation relation);

/** Checks for upcast.
 *
 * Returns true if the Cast node casts a class type to a super type.
 * Works also for pointers to classes (recursively).
 *
 * Needs typeinfo calculated.
 */
FIRM_API int is_Cast_upcast(ir_node *node);

/** Checks for downcast.
 *
 * Returns true if the Cast node casts a class type to a sub type.
 * Works also for pointers to classes (recursively).
 *
 * Needs typeinfo calculated.
 */
FIRM_API int is_Cast_downcast(ir_node *node);

/** Returns true if irg in phase phase_building and the Phi has zero
   predecessors. It's a Phi0 then. */
FIRM_API int       is_Phi0(const ir_node *n);
FIRM_API ir_node **get_Phi_preds_arr(ir_node *node);
FIRM_API int       get_Phi_n_preds(const ir_node *node);
FIRM_API ir_node  *get_Phi_pred(const ir_node *node, int pos);
FIRM_API void      set_Phi_pred(ir_node *node, int pos, ir_node *pred);
/**
 * Returns the next element of a block phi list.
 */
FIRM_API ir_node  *get_Phi_next(const ir_node *phi);
/**
 * Sets the next link of a block Phi list.
 */
FIRM_API void      set_Phi_next(ir_node *phi, ir_node *next);

/** Return true if parameter is a memory operation.
 *
 *  A memory operation is an operation that changes the
 *  memory.  I.e., a Load or a Store operation.
 */
FIRM_API int      is_memop(const ir_node *node);
FIRM_API ir_node *get_memop_mem(const ir_node *node);
FIRM_API void     set_memop_mem(ir_node *node, ir_node *mem);
FIRM_API ir_node *get_memop_ptr(const ir_node *node);
FIRM_API void     set_memop_ptr(ir_node *node, ir_node *ptr);

FIRM_API ir_node **get_Sync_preds_arr(ir_node *node);
FIRM_API int       get_Sync_n_preds(const ir_node *node);
FIRM_API ir_node  *get_Sync_pred(const ir_node *node, int pos);
FIRM_API void      set_Sync_pred(ir_node *node, int pos, ir_node *pred);
FIRM_API void      add_Sync_pred(ir_node *node, ir_node *pred);

/**
 * Returns non-zero if a node is a routine parameter.
 *
 * @param node  the Proj node to test
 */
FIRM_API int is_arg_Proj(const ir_node *node);

FIRM_API ir_node **get_Tuple_preds_arr(ir_node *node);
FIRM_API int       get_Tuple_n_preds(const ir_node *node);
FIRM_API ir_node  *get_Tuple_pred(const ir_node *node, int pos);
FIRM_API void      set_Tuple_pred(ir_node *node, int pos, ir_node *pred);

/** Return the number of input constraints for an ASM node. */
FIRM_API int get_ASM_n_input_constraints(const ir_node *node);
/** Return the number of output constraints for an ASM node.  */
FIRM_API int get_ASM_n_output_constraints(const ir_node *node);
/** Return the number of clobbered registers for an ASM node.  */
FIRM_API int get_ASM_n_clobbers(const ir_node *node);

/** Returns operand of node if node is a Proj. */
FIRM_API ir_node *skip_Proj(ir_node *node);
/** Returns operand of node if node is a Proj. */
FIRM_API const ir_node *skip_Proj_const(const ir_node *node);
/** Returns operand of node if node is a Id. */
FIRM_API ir_node *skip_Id(ir_node *node);
/** Returns corresponding operand of Tuple if node is a Proj from a Tuple. */
FIRM_API ir_node *skip_Tuple(ir_node *node);
/** Returns operand of node if node is a Cast. */
FIRM_API ir_node *skip_Cast(ir_node *node);
FIRM_API const ir_node *skip_Cast_const(const ir_node *node);
/** Returns operand of node if node is a Pin. */
FIRM_API ir_node *skip_Pin(ir_node *node);
/** Returns operand of node if node is a Confirm */
FIRM_API ir_node *skip_Confirm(ir_node *node);
/** Skip all high-level Operations (including Cast, Confirm). */
FIRM_API ir_node *skip_HighLevel_ops(ir_node *node);
/** Returns true if the operation manipulates control flow */
FIRM_API int is_cfop(const ir_node *node);
/** returns true if the operation jumps to an unknown destination.
 * See irop_flag_unknown_jump for a detailed explanation */
FIRM_API int is_unknown_jump(const ir_node *node);

/**
 * Returns true if the operation can change the control flow because
 * of an exception: Call, Div, Mod, Load, Store, Alloc,
 * Bad. Raise is not fragile, but a unconditional jump.
 */
FIRM_API int is_fragile_op(const ir_node *node);
/** Returns the memory operand of fragile operations. */
FIRM_API ir_node *get_fragile_op_mem(ir_node *node);

/** Returns true if the operation is a forking control flow
 *  operation: Cond. */
FIRM_API int is_irn_forking(const ir_node *node);

/**
 * Copies attributes stored in the old node to a new node.
 * Assumes both have the same opcode and sufficient size.
 *
 * @param irg       The irg of the new_node (get_irn_irg on it might not work
 *                  yet)
 * @param old_node  the node where the attributes are copied from
 * @param new_node  node the attributes get copies to.
 *
 * This copies all essential information to the new node. It does not copy
 * temporal or calculated information like visited flags or results of dominance
 * or loop calculations
 */
FIRM_API void copy_node_attr(ir_graph *irg, const ir_node *old_node, ir_node *new_node);

/** Return the type attribute of a node n (SymConst, Call, Alloc, Free,
 *  Cast) or NULL.*/
FIRM_API ir_type *get_irn_type_attr(ir_node *n);

/** Return the entity attribute of a node n (SymConst, Sel) or NULL. */
FIRM_API ir_entity *get_irn_entity_attr(ir_node *n);

/** Returns non-zero for constant-like nodes. */
FIRM_API int is_irn_constlike(const ir_node *node);

/**
 * Returns non-zero for nodes that are allowed to have keep-alives and
 * are neither Block nor PhiM.
 */
FIRM_API int is_irn_keep(const ir_node *node);

/**
 * Returns non-zero for nodes that are always placed in the start block.
 */
FIRM_API int is_irn_start_block_placed(const ir_node *node);

/**
 * Returns non-zero for nodes that are machine operations.
 */
FIRM_API int is_irn_machine_op(const ir_node *node);

/**
 * Returns non-zero for nodes that are machine operands.
 */
FIRM_API int is_irn_machine_operand(const ir_node *node);

/**
 * Returns non-zero for nodes that have the n'th user machine flag set.
 */
FIRM_API int is_irn_machine_user(const ir_node *node, unsigned n);

/**
 * Returns non-zero for nodes that are CSE neutral to its users.
 */
FIRM_API int is_irn_cse_neutral(const ir_node *node);

/** Gets the string representation of the jump prediction. */
FIRM_API const char *get_cond_jmp_predicate_name(cond_jmp_predicate pred);

/** Checks whether a node represents a global address. */
FIRM_API int is_Global(const ir_node *node);

/** Returns the entity of a global address. */
FIRM_API ir_entity *get_Global_entity(const ir_node *node);

/**
 * Access custom node data.
 * The data must have been registered with
 * register_additional_node_data() before.
 * @param node The ir node to get the data from.
 * @param type The type of the data you registered.
 * @param off The value returned by register_additional_node_data().
 * @return A pointer of type @p type.
 */
#define get_irn_data(node,type,off) \
  (assert(off > 0 && "Invalid node data offset"), (type *) ((char *) (node) - (off)))

/**
 * Get the pointer to the node some custom data belongs to.
 * @param data The pointer to the custom data.
 * @param off The number as returned by register_additional_node_data().
 * @return A pointer to the ir node the custom data belongs to.
 */
#define get_irn_data_base(data,off) \
  (assert(off > 0 && "Invalid node data offset"), (ir_node *) ((char *) (data) + (off)))

/**
 * Request additional data to be allocated with an ir node.
 * @param size The size of the additional data required.
 * @return A positive number, if the operation was successful, which
 * must be passed to the access macro get_irn_data(), 0 if the
 * registration failed.
 */
FIRM_API unsigned firm_register_additional_node_data(unsigned size);

/**
 * Return a pointer to the node attributes.
 * Needed for user-defined nodes.
 */
FIRM_API void *get_irn_generic_attr(ir_node *node);
FIRM_API const void *get_irn_generic_attr_const(const ir_node *node);

/**
 * Returns the unique node index for the node in its graph.
 * This index is used to access phase information for this node.
 */
FIRM_API unsigned get_irn_idx(const ir_node *node);

/**
 * Sets the debug information of a node.
 *
 * @param n   The node.
 * @param db  The debug info.
 */
FIRM_API void set_irn_dbg_info(ir_node *n, dbg_info *db);

/**
 * Returns the debug information of an node.
 *
 * @param n   The node.
 */
FIRM_API dbg_info *get_irn_dbg_info(const ir_node *n);

/**
 * Calculate a hash value of a node. Only inputs, mode and opcode are used.
 *
 * @param node  the node to hash
 */
FIRM_API unsigned firm_default_hash(const ir_node *node);

/**
 * returns a descriptive name of a node (containing type+number)
 */
FIRM_API const char *gdb_node_helper(void *firm_object);

/*@}*/

#include "end.h"

#endif
