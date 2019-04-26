/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Representation of an intermediate operation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IRNODE_H
#define FIRM_IR_IRNODE_H

#include <stddef.h>
#include "firm_common.h"
#include "irmode.h"
#include "irop.h"
#include "nodes.h"
#include "typerep.h"

#include "begin.h"

/**
 * @ingroup ir_graph
 * @defgroup ir_node Nodes
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
 * Returns the number of predecessors without the block predecessor.
 *
 * @param node   the IR-node
 */
FIRM_API int get_irn_arity(const ir_node *node);

/**
 * Returns the n-th predecessor of a node.
 * This function removes Id predecessors.
 */
FIRM_API ir_node *get_irn_n(const ir_node *node, int n);

/**
 * Replaces the old in array by a new one that will contain the ins given in
 * the parameters. Conserves the block predecessor. It copies the array passed.
 * This function is necessary to adjust in arrays of blocks, calls and phis.
 * "in" must contain all predecessors except the block that are required for
 * the nodes opcode. */
FIRM_API void set_irn_in(ir_node *node, int arity, ir_node *const in[]);

/** Replaces the n-th predecessor of a node with a new one. */
FIRM_API void set_irn_n(ir_node *node, int n, ir_node *in);
/**
 * Appends a new predecessor to a node. This only works for nodes with
 * dynamic arity!
 * @returns   the number of the new input
 */
FIRM_API int add_irn_n(ir_node *node, ir_node *in);
/** Sets the mode struct of node.  */
FIRM_API void set_irn_mode(ir_node *node, ir_mode *mode);
/** Returns the mode struct of a node.  */
FIRM_API ir_mode *get_irn_mode(const ir_node *node);
/** Returns the opcode struct of the node. */
FIRM_API ir_op *get_irn_op(const ir_node *node);
/** Returns the opcode-enum of the node. */
FIRM_API unsigned get_irn_opcode(const ir_node *node);
/** Returns the string representation of the opcode. */
FIRM_API const char *get_irn_opname(const ir_node *node);
/** Returns the ident for a string representation of the opcode. */
FIRM_API ident *get_irn_opident(const ir_node *node);
/** Returns the visited counter of a node. */
FIRM_API ir_visited_t get_irn_visited(const ir_node *node);
/** Sets the visited counter of a node. */
FIRM_API void set_irn_visited(ir_node *node, ir_visited_t visited);
/** Sets visited to get_irg_visited(get_irn_irg(node)). */
FIRM_API void mark_irn_visited(ir_node *node);
/** Returns 1 if visited >= get_irg_visited(get_irn_irg(node)). */
FIRM_API int irn_visited(const ir_node *node);
/** Returns 1 if visited >= get_irg_visited(get_irn_irg(node)). Marks the node
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

/**
 * Returns whether a node is pinned.
 * A pinned node must not be moved to a different block even if the operands
 * would allow this.
 * Returns 1 if node is pinned, 0 otherwise.
 */
FIRM_API int get_irn_pinned(const ir_node *node);

/** Sets pin state for nodes with op pin state op_pin_state_exc_pinned.
 * @p pinned should be 0 or 1, @see get_irn_pinned() */
FIRM_API void set_irn_pinned(ir_node *node, int pinned);

/**
 * IR node constructor.
 * Create a new IR node in irg, with an op, mode, arity and
 * some incoming IR nodes.
 * Normally you should not use this constructor directly unless you registered
 * custom opcodes. For the default opcodes firm provides specific constructors.
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
 * Creates an exact copy of @p node with same inputs and attributes in the
 * same block. The copied node will not be optimized (so no CSE is performed).
 *
 * @param node the node to copy
 */
FIRM_API ir_node *exact_copy(const ir_node *node);

/**
 * Create an exact copy of @p node with same inputs and attributes in
 * the same block but stored in the graph @p irg. The copied node will
 * still point to inputs and a block in the graph of the original
 * node. You must fix up these pointers after copying the node.
 *
 * @param node the node to copy
 * @param irg the graph in which to store the copied node
 */
FIRM_API ir_node *irn_copy_into_irg(const ir_node *node, ir_graph *irg);

/**
 * Returns the block the node belongs to.  This is only
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

/**
 * @addtogroup Block
 * @{
 */

/**
 * Returns the predecessor block.
 *
 * Returns the block corresponding to the predecessor pos of block.
 *
 * If we encounter the Bad node as controlflow predecessor NULL is returned.
 */
FIRM_API ir_node *get_Block_cfgpred_block(const ir_node *node, int pos);

/** Returns the matured flag of a block */
FIRM_API int get_Block_matured(const ir_node *block);
/** set the matured flag of a block. */
FIRM_API void set_Block_matured(ir_node *block, int matured);

/** A visited flag only for block nodes.
 *  @see also: get_irn_visited() inc_irg_visited() inc_irg_block_visited()*/
FIRM_API ir_visited_t get_Block_block_visited(const ir_node *block);
/** set block visited flag */
FIRM_API void set_Block_block_visited(ir_node *block, ir_visited_t visit);

/** Marks a block as visited by setting its visited counter */
FIRM_API void mark_Block_block_visited(ir_node *node);
/** Returns 1 if a block is marked as visited */
FIRM_API int Block_block_visited(const ir_node *node);

/** Returns the entity for a Block (creating it if necessary) */
FIRM_API ir_entity *create_Block_entity(ir_node *block);
/** Returns the head of the Phi list for this block. */
FIRM_API ir_node *get_Block_phis(const ir_node *block);
/** Sets the head of the Phi list for this block. */
FIRM_API void set_Block_phis(ir_node *block, ir_node *phi);
/** Add a Phi node to the list of Block Phi's. */
FIRM_API void add_Block_phi(ir_node *block, ir_node *phi);
/** Returns the Block mark (single bit). */
FIRM_API unsigned get_Block_mark(const ir_node *block);
/** Sets the Block mark (single bit). */
FIRM_API void set_Block_mark(ir_node *block, unsigned mark);

/** @} */

/** @addtogroup End
 * @{
 */

/** Keep alive dedicated nodes.  These must be either PhiM or Block nodes. */
FIRM_API void add_End_keepalive(ir_node *end, ir_node *ka);

/**
 * Sets new keep-alives.
 * Beware: This might be an expensive operation if dynamic edges are enabled,
 * so avoid it in the backend.
 */
FIRM_API void set_End_keepalives(ir_node *end, int n, ir_node *in[]);

/** Removes irn from the keep-alive set. */
FIRM_API void remove_End_keepalive(ir_node *end, const ir_node *irn);

/** Removes predecessor (a keepalive) at index @p idx from End node @p end. */
FIRM_API void remove_End_n(ir_node *end, int idx);

/** Removes Bads, NoMem and doublets from the keep-alive set. */
FIRM_API void remove_End_Bads_and_doublets(ir_node *end);

/** Some parts of the End node are allocated separately -- their memory
 * is not recovered by dead_node_elimination if a End node is dead.
 * free_End() frees these data structures.
 */
FIRM_API void free_End(ir_node *end);

/** @} */

/** @addtogroup Const
 * @{
 */

/** Returns non-zero if the given Const node represents the 0 constant. */
FIRM_API int is_Const_null(const ir_node *node);

/** Returns non-zero if the given Const node represents the 1 constant. */
FIRM_API int is_Const_one(const ir_node *node);

/** Returns non-zero if the given Const node represents the constant with all bits set. */
FIRM_API int is_Const_all_one(const ir_node *node);

/** @} */

/**
 * @addtogroup Call
 * @{
 */

/**
 * Convenience function: Return method that will be called by a call.
 *
 * This matches for an address at the Call ptr input, return
 * the referenced entity if it has a method type.
 */
FIRM_API ir_entity *get_Call_callee(const ir_node *call);

/** @} */


/** Returns a human readable string for the ir_builtin_kind. */
FIRM_API const char *get_builtin_kind_name(ir_builtin_kind kind);

/** Returns left operand of binary operation @p node. */
FIRM_API ir_node *get_binop_left(const ir_node *node);
/** Sets left operand of binary operation @p node. */
FIRM_API void set_binop_left(ir_node *node, ir_node *left);
/** Returns rights operand of binary operation @p node. */
FIRM_API ir_node *get_binop_right(const ir_node *node);
/** Sets right operand of binary operation @p node. */
FIRM_API void set_binop_right(ir_node *node, ir_node *right);

/**
 * Tests whether a node is the X_except Proj of a fragile operation
 */
FIRM_API int is_x_except_Proj(const ir_node *node);

/**
 * Tests whether a node is the X_regular Proj of a fragile operation
 */
FIRM_API int is_x_regular_Proj(const ir_node *node);

/**
 * Sets throws exception attribute of a fragile node
 * @p throws_exception must be 0 or 1
 */
FIRM_API void ir_set_throws_exception(ir_node *node, int throws_exception);

/** Returns throws_exception attribute of a fragile node */
FIRM_API int ir_throws_exception(const ir_node *node);

/** Returns the name of an ir_relation */
FIRM_API const char *get_relation_string(ir_relation relation);

/** Calculates the negated (Complement(R)) relation, i.e. "<" --> ">=" */
FIRM_API ir_relation get_negated_relation(ir_relation relation);

/** Calculates the inversed (R^-1) relation, i.e., "<" --> ">" */
FIRM_API ir_relation get_inversed_relation(ir_relation relation);

/**
 * @addtogroup Phi
 * @{
 */

/**
 * Returns the next element of a block phi list.
 */
FIRM_API ir_node *get_Phi_next(const ir_node *phi);
/**
 * Sets the next link of a block Phi list.
 */
FIRM_API void set_Phi_next(ir_node *phi, ir_node *next);

/** @} */

/** Returns true if @p node is a memory operation.
 *
 * A memory operation is a node with an opcode that has irop_flag_uses_memory
 * set. It is guaranteed to have (exactly) one memory input.
 */
FIRM_API int is_memop(const ir_node *node);
/**
 * Returns the memory input of a memory operation.
 */
FIRM_API ir_node *get_memop_mem(const ir_node *node);
/**
 * Sets the memory input of a memory operation.
 */
FIRM_API void set_memop_mem(ir_node *node, ir_node *mem);

/** @addtogroup Sync
 * @{
 */

/** Adds @p pred to predecessor list of Sync node @p node. */
FIRM_API void add_Sync_pred(ir_node *node, ir_node *pred);
/** Removes predecessor @p i from Sync @p n */
FIRM_API void remove_Sync_n(ir_node *n, int i);

/** @} */

/** @addtogroup ASM
 * @{
 */

/** Returns the number of output constraints for an ASM node.  */
FIRM_API size_t get_ASM_n_constraints(const ir_node *node);
/** Returns the number of clobbered registers for an ASM node.  */
FIRM_API size_t get_ASM_n_clobbers(const ir_node *node);

/** @} */

/** Returns operand of node if node is a Proj. */
FIRM_API ir_node *skip_Proj(ir_node *node);
/** Returns operand of node if node is a Proj. */
FIRM_API const ir_node *skip_Proj_const(const ir_node *node);
/** Returns operand of node if node is a Id. */
FIRM_API ir_node *skip_Id(ir_node *node);
/** Returns corresponding operand of Tuple if node is a Proj from a Tuple. */
FIRM_API ir_node *skip_Tuple(ir_node *node);
/** Returns operand of node if node is a Pin. */
FIRM_API ir_node *skip_Pin(ir_node *node);
/** Returns operand of node if node is a Confirm */
FIRM_API ir_node *skip_Confirm(ir_node *node);
/** Returns true if the operation manipulates control flow */
FIRM_API int is_cfop(const ir_node *node);
/** Returns true if the operation jumps to an unknown destination.
 * See irop_flag_unknown_jump for a detailed explanation */
FIRM_API int is_unknown_jump(const ir_node *node);

/**
 * Returns true if the operation can change the control flow because
 * of an exception: Call, Div, Mod, Load, Store, Alloc,
 * Bad. Raise is not fragile, but a unconditional jump.
 */
FIRM_API int is_fragile_op(const ir_node *node);

/** Returns true if the operation is a forking control flow
 *  operation: Cond. */
FIRM_API int is_irn_forking(const ir_node *node);

/** Returns true if the operation does not change anymemory contents although
 * it has a memory input/output. */
FIRM_API int is_irn_const_memory(const ir_node *node);

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
 * temporary or calculated information like visited flags or results of
 * dominance or loop calculations.
 */
FIRM_API void copy_node_attr(ir_graph *irg, const ir_node *old_node, ir_node *new_node);

/** Returns the type attribute of a node n (TypeConst, Call, Alloc, Free)
 *  or NULL.*/
FIRM_API ir_type *get_irn_type_attr(ir_node *n);

/** Returns the entity attribute of a node n (Address, Offset, Sel) or NULL. */
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

/** Returns the string representation of the jump prediction. */
FIRM_API const char *get_cond_jmp_predicate_name(cond_jmp_predicate pred);

/**
 * Returns a pointer to the node attributes.
 * Used for accessing attributes of user-defined nodes.
 */
FIRM_API void *get_irn_generic_attr(ir_node *node);
/**
 * Returns a pointer to the node attributes.
 * Used for accessing attributes of user-defined nodes.
 */
FIRM_API const void *get_irn_generic_attr_const(const ir_node *node);

/**
 * Returns the unique node index for the node in its graph.
 * This index is used to access phase information for this node.
 * @see get_idx_irn()
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
 * Returns a descriptive name of a node (containing type+number)
 */
FIRM_API const char *gdb_node_helper(const void *firm_object);

/**
 * @addtogroup Switch
 * @{
 */

/**
 * Creates a new switch_table data structure with @p n_entries entries.
 * The data structure is allocated on the obstack of @p irg.
 */
FIRM_API ir_switch_table *ir_new_switch_table(ir_graph *irg, size_t n_entries);

/**
 * Returns number of entries available in switch table @p table.
 */
FIRM_API size_t ir_switch_table_get_n_entries(const ir_switch_table *table);

/**
 * Sets entry number @p entry in the switch table @p table.
 * @param table  the switch table
 * @param entry  entry number to set
 * @param min    The minimum tarval that matches this entry
 * @param max    The maximum tarval that matches this entry
 * @param pn     Proj number taken on match
 */
FIRM_API void ir_switch_table_set(ir_switch_table *table, size_t entry,
                                  ir_tarval *min, ir_tarval *max, unsigned pn);

/** Returns maximum tarval value of switch table entry @p entry */
FIRM_API ir_tarval *ir_switch_table_get_max(const ir_switch_table *table,
                                            size_t entry);

/** Returns minimum tarval value of switch table entry @p entry */
FIRM_API ir_tarval *ir_switch_table_get_min(const ir_switch_table *table,
                                            size_t entry);

/** Returns proj number taken if switch table entry @p entry matches */
FIRM_API unsigned ir_switch_table_get_pn(const ir_switch_table *table,
                                         size_t entry);

/** Duplicates switch table @p table on obstack of @p irg */
FIRM_API ir_switch_table *ir_switch_table_duplicate(ir_graph *irg, const ir_switch_table *table);
/** @} */

/** @} */

#include "end.h"

#endif
