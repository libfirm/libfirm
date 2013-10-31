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
#include "typerep.h"
#include "irop.h"
#include "irmode.h"
#include "begin.h"
#include "nodes.h"

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

/**
 * Add an artificial dependency to the node.
 *
 * @param node The node.
 * @param dep  The dependency target.
 */
FIRM_API void add_irn_dep(ir_node *node, ir_node *dep);

/**
 * Copy all dependencies from a node to another.
 * This is only allowed in phase_backend!
 *
 * @param tgt The node which should be enriched.
 * @param src The node whose dependencies shall be copied.
 */
FIRM_API void add_irn_deps(ir_node *tgt, ir_node *src);

/**
 * Returns the length of the dependency array.
 * @param node The node.
 * @return The length of the dependency array or 0 if it has not yet been allocated.
 */
FIRM_API int get_irn_deps(const ir_node *node);

/**
 * Returns an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @return The node at that position.
 */
FIRM_API ir_node *get_irn_dep(const ir_node *node, int pos);

/**
 * Sets an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @param dep  The dependency target.
 */
FIRM_API void set_irn_dep(ir_node *node, int pos, ir_node *dep);

/**
 * Deletes the entry of the dependency array, that points to dep. Does nothing
 * if no dependency exists.
 *
 * @param node the node to delete the dependency at
 * @param dep the target of the dependency to delete
 */
FIRM_API void delete_irn_dep(ir_node *node, ir_node *dep);

/** Replaces the n-th predecessor of a node with a new one. */
FIRM_API void set_irn_n(ir_node *node, int n, ir_node *in);
/**
 * Appends a new predecessor to a node. This only works for nodes with
 * variable arity!
 * @returns   the number of the new input
 */
FIRM_API int add_irn_n(ir_node *node, ir_node *in);
/** Sets the mode struct of node.  */
FIRM_API void set_irn_mode(ir_node *node, ir_mode *mode);
/** Returns the mode struct of a node.  */
FIRM_API ir_mode *get_irn_mode(const ir_node *node);
/** Returns the opcode struct of the node. */
FIRM_API ir_op *get_irn_op(const ir_node *node);
/** Sets the opcode struct of the node. */
FIRM_API void set_irn_op(ir_node *node, ir_op *op);
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

/** Returns the pinned state of a node.
 *
 *  Returns whether the node _always_ must be pinned.
 *  I.e., the node is not floating after global cse.
 *
 * @returns Either state op_pin_state_pinned or op_pin_state_floats.
 */
FIRM_API op_pin_state get_irn_pinned(const ir_node *node);

/** Sets pin state for nodes with op pin state op_pin_state_exc_pinned */
FIRM_API void set_irn_pinned(ir_node *node, op_pin_state state);

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
 * Returns the position of the predecessor block pred in the inputs
 * of the block block.
 *
 * @param block  the block
 * @param pred   a predecessor block of block
 *
 * @return the position of pred in block or -1
 */
FIRM_API int get_Block_cfgpred_pos(const ir_node *block, const ir_node *pred);

/**
 * Returns the predecessor block.
 *
 * Returns the block corresponding to the predecessor pos of block.
 *
 * If we encounter the Bad node, this function does not return Start block, but
 * the Bad node.
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

/** Returns the ir_graph this Block belongs to. */
FIRM_API ir_graph *get_Block_irg(const ir_node *block);
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

/** Tests whether arbitrary node is frame pointer.
 *
 * Tests whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
FIRM_API ir_type *is_frame_pointer(const ir_node *n);

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
FIRM_API void remove_End_keepalive(ir_node *end, ir_node *irn);

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
 * @addtogroup SymConst
 * @{
 */

/**
 * Returns true if node is a SymConst node with kind symconst_addr_ent.
 */
FIRM_API int is_SymConst_addr_ent(const ir_node *node);

/** Returns non-zero if s symconst kind has a type attribute */
#define SYMCONST_HAS_TYPE(kind) ((kind) <= symconst_type_align)

/** Returns non-zero if s symconst kind has an entity attribute */
#define SYMCONST_HAS_ENT(kind) ((kind) == symconst_addr_ent || (kind) == symconst_ofs_ent)

/** Returns the kind of the SymConst. */
FIRM_API symconst_kind get_SymConst_kind(const ir_node *node);
/** Sets the kind of the SymConst. */
FIRM_API void set_SymConst_kind(ir_node *node, symconst_kind num);

/** Returns the type attribute of SymConst node @p node.
 * @note Only to access SymConst of kind type_siz, else assertion.
 */
FIRM_API ir_type *get_SymConst_type(const ir_node *node);
/** Sets the type attribute of SymConst node @p node. */
FIRM_API void set_SymConst_type(ir_node *node, ir_type *tp);

/** Returns the entity attribute of SymConst node @p node.
 * @note Only to access SymConst of kind addr_ent, else assertion.
 */
FIRM_API ir_entity *get_SymConst_entity(const ir_node *node);
/** Sets the entity attribute of Symconst node @p node. */
FIRM_API void set_SymConst_entity(ir_node *node, ir_entity *ent);

/** Returns the symbol attribute of SymConst node @p node. */
FIRM_API union symconst_symbol get_SymConst_symbol(const ir_node *node);
/** Sets the symbol attribute of SymConst node @p node. */
FIRM_API void set_SymConst_symbol(ir_node *node, union symconst_symbol sym);

/** @} */

/** Returns a human readable string for the ir_builtin_kind. */
FIRM_API const char *get_builtin_kind_name(ir_builtin_kind kind);

/** Tests whether node is a binary operation (opcode arity is #oparity_binary)
 * @returns 1 if @p node is an binary operation, 0 otherwise
 */
FIRM_API int is_binop(const ir_node *node);
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

/**
 * Returns non-zero if a node is a routine parameter.
 *
 * @param node  the Proj node to test
 */
FIRM_API int is_arg_Proj(const ir_node *node);

/** @addtogroup ASM
 * @{
 */

/** Returns the number of output constraints for an ASM node.  */
FIRM_API size_t get_ASM_n_output_constraints(const ir_node *node);
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
/** Skip all high-level Operations (including Confirm). */
FIRM_API ir_node *skip_HighLevel_ops(ir_node *node);
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

/** Returns the type attribute of a node n (SymConst, Call, Alloc, Free)
 *  or NULL.*/
FIRM_API ir_type *get_irn_type_attr(ir_node *n);

/** Returns the entity attribute of a node n (SymConst, Sel) or NULL. */
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
 * Returns non-zero for nodes that are CSE neutral to its users.
 */
FIRM_API int is_irn_cse_neutral(const ir_node *node);

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
FIRM_API const char *gdb_node_helper(void *firm_object);

/**
 * @addtogroup Switch
 * @{
 */

/**
 * Creates a new switch_table datastructure with @p n_entries entries.
 * The datastructure is allocated on the obstack of @p irg.
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
                                  ir_tarval *min, ir_tarval *max, long pn);

/** Returns maximum tarval value of switch table entry @p entry */
FIRM_API ir_tarval *ir_switch_table_get_max(const ir_switch_table *table,
                                            size_t entry);

/** Returns minimum tarval value of switch table entry @p entry */
FIRM_API ir_tarval *ir_switch_table_get_min(const ir_switch_table *table,
                                            size_t entry);

/** Returns proj number taken if switch table entry @p entry matches */
FIRM_API long ir_switch_table_get_pn(const ir_switch_table *table, size_t entry);

/** Duplicates switch table @p table on obstack of @p irg */
FIRM_API ir_switch_table *ir_switch_table_duplicate(ir_graph *irg, const ir_switch_table *table);
/** @} */

/** @} */

#include "end.h"

#endif
