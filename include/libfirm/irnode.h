/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

/**
 * @file irnode.h
 *
 * @author Martin Trapp, Christian Schaefer
 *
 * Declarations of an ir node.
 */

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
 *  - firm_kind - A firm_kind tag containing k_type.  This is useful
 *                for dynamically checking whether a node is a ir_node.
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
 * Some projection numbers must be always equal to support automatic phi construction
 */
enum pn_generic {
	pn_Generic_M_regular = 0,  /**< The memory result. */
	pn_Generic_X_regular = 1,  /**< Execution result if no exception occurred. */
	pn_Generic_X_except  = 2,  /**< The control flow result branching to the exception handler */
	pn_Generic_other     = 3   /**< First free projection number */
};

/**
 *   Checks whether a pointer points to a ir node.
 *
 *   @param thing   an arbitrary pointer
 *   @return        non-zero if the thing is a ir mode, else zero
 */
int is_ir_node (const void *thing);

/**
 * Returns the number of predecessors without the block predecessor.
 *
 * @param node   the IR-node
 */
int get_irn_arity      (const ir_node *node);
int get_irn_intra_arity(const ir_node *node);
int get_irn_inter_arity(const ir_node *node);

/** Replaces the old in array by a new one that will contain the ins given in
   the parameters.  Conserves the block predecessor.  It copies the array passed.
   This function is necessary to adjust in arrays of blocks, calls and phis.
   Assumes that current_ir_graph is set to the graph containing "node".
   "in" must contain all predecessors except the block that are required for
   the nodes opcode. */
void set_irn_in(ir_node *node, int arity, ir_node *in[]);

/* to iterate through the predecessors without touching the array. No
   order of predecessors guaranteed.
   To iterate over the operands iterate from 0 to i < get_irn_arity(),
   to iterate including the Block predecessor iterate from i = -1 to
   i < get_irn_arity. */
/* Access predecessor n */

/**
 * Get the n-th predecessor of a node.
 * This function removes Id predecessors.
 * This function automatically handles intra- and interprocedural views.
 */
ir_node *get_irn_n(const ir_node *node, int n);

/**
* Add a artificial dependency to the node.
* The dependency is only inserted if it is not there already.
* @param node The node.
* @param dep  The dependency target.
* @return The index in the array (get_irn_dep() with that index returns @p dep).
*/
int add_irn_dep(ir_node *node, ir_node *dep);

/**
 * Copy all dependencies from a node to another.
 * @param tgt The node which should be enriched.
 * @param src The node whose dependencies shall be copied.
 */
void add_irn_deps(ir_node *tgt, ir_node *src);

/**
 * Get the length of the dependency array.
 * @param node The node.
 * @return The length of the dependency array or 0 if it has not yet been allocated.
 */
int get_irn_deps(const ir_node *node);

/**
 * Get an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @return The node at that position.
 */
ir_node *get_irn_dep(const ir_node *node, int pos);

/**
 * Set an entry of the dependency array.
 * @param node The node.
 * @param pos  The position.
 * @param dep  The dependency target.
 */
void set_irn_dep(ir_node *node, int pos, ir_node *dep);


/**
 * Get the n-th predecessor of a node in intraprocedural view.
 * Can be used always if it's know that node is not a split one.
 */
ir_node *get_irn_intra_n(const ir_node *node, int n);

/**
 * Get the n-th predecessor of a node in interprocedural view.
 */
ir_node *get_irn_inter_n(const ir_node *node, int n);

/** Replace the n-th predecessor of a node with a new one. */
void set_irn_n(ir_node *node, int n, ir_node *in);
/**
 * Appends a new predecessor to a node. This only works for nodes with
 * variable arity!
 * @returns   the number of the new input
 */
int add_irn_n(ir_node *node, ir_node *in);
/* Sets the mode struct of node.  */
void set_irn_mode(ir_node *node, ir_mode *mode);
/** Gets the mode struct of a node.  */
ir_mode *get_irn_mode(const ir_node *node);
/** Gets the mode-enum modecode. */
modecode get_irn_modecode(const ir_node *node);
/** Gets the ident for a string representation of the mode .*/
ident *get_irn_modeident(const ir_node *node);
/** Gets the string representation of the mode .*/
const char *get_irn_modename(const ir_node *node);
/** Gets the opcode struct of the node. */
ir_op *get_irn_op(const ir_node *node);
/** Sets the opcode struct of the node. */
void set_irn_op(ir_node *node, ir_op *op);
/** Gets the opcode-enum of the node. */
unsigned get_irn_opcode(const ir_node *node);
/** Get the string representation of the opcode. */
const char *get_irn_opname(const ir_node *node);
/** Get the ident for a string representation of the opcode. */
ident *get_irn_opident(const ir_node *node);
/** If arg is an argument of the node, returns it's position, -1 otherwise */
int get_irn_pred_pos(ir_node *node, ir_node *arg);
/** Gets the visited counter of a node. */
unsigned long get_irn_visited(const ir_node *node);
/** Sets the visited counter of a node. */
void set_irn_visited(ir_node *node, unsigned long visited);
/** Sets visited to get_irg_visited(current_ir_graph). */
void mark_irn_visited(ir_node *node);
/** Returns 1 if visited < get_irg_visited(current_ir_graph). */
int irn_not_visited(const ir_node *node);
/** Returns 1 if visited >= get_irg_visited(current_ir_graph). */
int irn_visited(const ir_node *node);

/**
 * Sets the link of a node.
 * Only allowed if the graph is NOT in phase_building.
 */
void set_irn_link(ir_node *node, void *link);

/** Returns the link of a node.  */
void *get_irn_link(const ir_node *node);

/** Returns the ir_graph this node belongs to. */
ir_graph *get_irn_irg(const ir_node *node);

/** Outputs a unique number for this node if libFIRM is compiled for
   debugging, (configure with --enable-debug) else returns address
   of node cast to long. */
long get_irn_node_nr(const ir_node *node);

/** Returns the pinned state of a node.
 *
 *  Returns whether the node _always_ must be pinned.
 *  I.e., the node is not floating after global cse.
 *
 * @returns Either state op_pin_state_pinned or op_pin_state_floats.
 */
op_pin_state get_irn_pinned(const ir_node *node);

/** Set pin state for nodes with op pin state op_pin_state_exc_pinned */
void set_irn_pinned(ir_node *node, op_pin_state state);

/** Returns whether the node is currently pinned.
 *
 * If get_irn_pinned returns op_pin_state_floats and the graph the
 * node belongs to is in state op_poin_state_floats then this function
 * returns 'floats', else 'pinned'.
 */
op_pin_state is_irn_pinned_in_irg(const ir_node *node);

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
ir_node *
new_ir_node(dbg_info *db,
         ir_graph *irg,
         ir_node *block,
         ir_op *op,
         ir_mode *mode,
         int arity,
         ir_node *in[]);

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
ir_node  *get_nodes_block (const ir_node *node);

/** Sets the Block of a node. */
void      set_nodes_block (ir_node *node, ir_node *block);

/**
 * Return the MacroBlock the node belongs to.  This is only
 * possible for pinned nodes or if the graph is in pinned state.
 * Otherwise the MacroBlock may be incorrect.  This condition is
 * now checked by an assertion.
 *
 * This works for all except Block.  It can return Blocks or the Bad node.
 *
 * To express the difference to access routines that work for all
 * nodes we use infix "nodes" and do not name this function
 * get_irn_MacroBlock(). */
ir_node  *get_nodes_MacroBlock(const ir_node *node);

/**
 * @function get_irn_block()
 * @see get_nodes_block()
 */

/**
 * Projection numbers for result of Start node: use for Proj nodes!
 */
typedef enum {
	pn_Start_X_initial_exec,   /**< Projection on the initial control flow. */
	pn_Start_M,                /**< Projection on the initial memory. */
	pn_Start_P_frame_base,     /**< Projection on the frame base pointer. */
	pn_Start_P_globals,        /**< Projection on the pointer to the data segment
	                                containing _all_ global entities.  Use for
	                                position independent data/code access. */
	pn_Start_P_tls,            /**< Projection on the pointer to the thread local store
	                                segment containing _all_thread local variables. */
	pn_Start_T_args,           /**< Projection on all arguments. */
	pn_Start_P_value_arg_base, /**< Pointer to region of compound value arguments as defined by
	                                type of this method. */
	pn_Start_max               /**< number of projections from a Start */
} pn_Start; /* Projection numbers for Start. */

/** Test whether arbitrary node is frame pointer.
 *
 * Test whether arbitrary node is frame pointer, i.e. Proj(pn_Start_P_frame_base)
 * from Start.  If so returns frame type, else Null. */
ir_type *is_frame_pointer(ir_node *n);

/** Test whether arbitrary node is globals pointer.
 *
 * Test whether arbitrary node is globals pointer, i.e. Proj(pn_Start_P_globals)
 * from Start.  If so returns global type, else Null. */
ir_type *is_globals_pointer(ir_node *n);

/** Test whether arbitrary node is tls pointer.
 *
 * Test whether arbitrary node is tls pointer, i.e. Proj(pn_Start_P_tls)
 * from Start.  If so returns tls type, else Null. */
ir_type *is_tls_pointer(ir_node *n);

/** Test whether arbitrary node is value arg base.
 *
 * Test whether arbitrary node is value arg base, i.e. Proj(pn_Start_P_value_arg_base)
 * from Start.   If so returns 1, else 0. */
int   is_value_arg_pointer(ir_node *n);


/* @@@ no more supported  */
ir_node **get_Block_cfgpred_arr(ir_node *node);
int       get_Block_n_cfgpreds(const ir_node *node);
ir_node  *get_Block_cfgpred(const ir_node *node, int pos);
void      set_Block_cfgpred(ir_node *node, int pos, ir_node *pred);
/** Get the predecessor block.
 *
 *  Returns the block corresponding to the predecessor pos of block.
 *
 *  There are several ambiguities we resolve with this function:
 *  - The direct predecessor can be a Proj, which is not pinned.
 *    We walk from the predecessor to the next pinned node
 *    (skip_Proj) and return the block that node is in.
 *  - If we encounter the Bad node, this function does not return
 *    Start, but the Bad node.
 */
ir_node  *get_Block_cfgpred_block(const ir_node *node, int pos);
int       get_Block_matured(const ir_node *node);
void      set_Block_matured(ir_node *node, int matured);

/** A visited flag only for block nodes.
 *  @see also: get_irn_visited() inc_irg_visited() inc_irg_block_visited()*/
unsigned long get_Block_block_visited(const ir_node *node);
void      set_Block_block_visited(ir_node *node, unsigned long visit);

/**
 * Marks a block as dead but do not replace it with a Bad node.
 * Dead blocks are removed in the con
 */
ir_node  *set_Block_dead(ir_node *block);
int       is_Block_dead(const ir_node *block);

/* For this current_ir_graph must be set. */
void      mark_Block_block_visited(ir_node *node);
int       Block_not_block_visited(const ir_node *node);
int       Block_block_visited(const ir_node *node);

#ifdef INTERPROCEDURAL_VIEW
/* Set and remove interprocedural predecessors. If the interprocedural
 * predecessors are removed, the node has the same predecessors in
 * both views.
 * @@@ Maybe better:  arity is zero if no cg preds. */
void      set_Block_cg_cfgpred_arr(ir_node *node, int arity, ir_node *in[]);
void      set_Block_cg_cfgpred(ir_node *node, int pos, ir_node *pred);
/* @@@ not supported */
ir_node **get_Block_cg_cfgpred_arr(ir_node *node);
/** Returns the number of interprocedural predecessors.  0 if none. */
int       get_Block_cg_n_cfgpreds(ir_node *node);
/** Return the interprocedural predecessor at position pos. */
ir_node  *get_Block_cg_cfgpred(ir_node *node, int pos);
/** Frees the memory allocated for interprocedural predecessors. */
void      remove_Block_cg_cfgpred_arr(ir_node *node);
#endif

/** Returns the extended basic block a block belongs to. */
ir_extblk *get_Block_extbb(const ir_node *block);
/** Sets the extended basic block a block belongs to. */
void set_Block_extbb(ir_node *block, ir_extblk *extblk);
/** Get the Macro Block header of a (sub-) block. */
ir_node *get_Block_MacroBlock(const ir_node *block);
/** Returns the ir_graph this Block belongs to. */
ir_graph *get_Block_irg(const ir_node *block);
/** Returns non-zero if the block has an assigned label. */
int has_Block_label(const ir_node *block);
/** Returns the label of a Block. */
ir_label_t get_Block_label(const ir_node *block);
/** Sets a label to a block. */
void set_Block_label(ir_node *block, ir_label_t label);

/** Return the number of Keep alive node. */
int  get_End_n_keepalives(ir_node *end);
/** Return the Keep alive node a position pos. */
ir_node *get_End_keepalive(ir_node *end, int pos);
/** Keep alive dedicated nodes.  These must be either PhiM or Block nodes. */
void add_End_keepalive(ir_node *end, ir_node *ka);
/** Set the Keep alive node at position pos. */
void set_End_keepalive(ir_node *end, int pos, ir_node *ka);
/** Set new keep-alives. */
void set_End_keepalives(ir_node *end, int n, ir_node *in[]);
/** Set new keep-alives from old keep-alives, skipping irn. */
void remove_End_keepalive(ir_node *end, ir_node *irn);

/** Some parts of the End node are allocated separately -- their memory
   is not recovered by dead_node_elimination if a End node is dead.
   free_End() frees these data structures. */
void free_End(ir_node *end);

/** Return the target address of an IJmp */
ir_node *get_IJmp_target(ir_node *ijmp);
/** Sets the target address of an IJmp */
void set_IJmp_target(ir_node *ijmp, ir_node *tgt);

/* We distinguish three kinds of Cond nodes.  These can be distinguished
   by the mode of the selector operand and an internal flag of type cond_kind.
   First we distinguish binary Conds and switch Conds.
   A binary Cond has as selector a boolean value.  Proj(0) projects the control
   flow for case "False", Proj(1) the control flow for "True".  A binary Cond
   is recognized by the boolean selector.
   The switch Cond has as selector an unsigned integer.  It produces as result
   an n+1 Tuple (cf0, ... , cfn) of control flows.
   We differ two flavors of this Cond.  The first, the dense Cond, passes
   control along output i if the selector value is i, 0 <= i <= n.  If the
   selector value is >n it passes control along output n.
   The second Cond flavor differences in the treatment of cases not specified in
   the source program.  It magically knows about the existence of Proj nodes.
   It only passes control along output i, 0 <= i <= n, if a node Proj(Cond, i)
   exists.  Else it passes control along output n (even if this Proj does not
   exist.)  This Cond we call "fragmentary".  There is a special constructor
   new_defaultProj that automatically sets the flavor.
   The two switch flavors are distinguished by a flag of type cond_kind.
   Default flavor is "dense"
*/
typedef enum {
	dense,        /**< Default. Missing Proj nodes are dead control flow. */
	fragmentary   /**< Special. No control flow optimizations allowed.  Missing
	                   Proj nodes mean default control flow, i.e., Proj(n). */
} cond_kind;

ir_node  *get_Cond_selector(ir_node *node);
void      set_Cond_selector(ir_node *node, ir_node *selector);
cond_kind get_Cond_kind(ir_node *node);
void      set_Cond_kind(ir_node *node, cond_kind kind);
long      get_Cond_defaultProj(ir_node *node);

/**
 * Projection numbers for conditions.
 */
typedef enum {
	pn_Cond_false,    /**< Control flow if operand is "false". */
	pn_Cond_true,     /**< Control flow if operand is "true".  */
	pn_Cond_max       /**< number of projections from a Cond */
} pn_Cond;  /* Projection numbers for Cond. */

ir_node  *get_Return_mem(ir_node *node);
void      set_Return_mem(ir_node *node, ir_node *mem);
ir_node **get_Return_res_arr(ir_node *node);
int       get_Return_n_ress(ir_node *node);
ir_node  *get_Return_res(ir_node *node, int pos);
void      set_Return_res(ir_node *node, int pos, ir_node *res);

tarval  *get_Const_tarval(const ir_node *node);
void     set_Const_tarval(ir_node *node, tarval *con);

/** Return non-zero if the given Const node represents the 0 constant. */
int is_Const_null(const ir_node *node);

/** Return non-zero if the given Const node represents the 1 constant. */
int is_Const_one(const ir_node *node);

/** Return non-zero if the given Const node represents the constant with all bits set. */
int is_Const_all_one(const ir_node *node);

/** Returns the source language type of a Const node.
 * Must be an atomic type.  Mode of type must be mode of node.
 */
ir_type  *get_Const_type(ir_node *node);

/** Sets the source language type of a Const node. */
void     set_Const_type(ir_node *node, ir_type *tp);

/**  This enum names the three different kinds of symbolic Constants
     represented by SymConst.  The content of the attribute type_or_id
     depends on this tag.  Use the proper access routine after testing
     this flag. */
typedef enum {
	symconst_type_tag,    /**< The SymConst is a type tag for the given type.
	                           symconst_symbol is type *. */
	symconst_type_size,   /**< The SymConst is the size of the given type.
	                           symconst_symbol is type *. */
	symconst_type_align,  /**< The SymConst is the alignment of the given type.
	                           symconst_symbol is type *. */
	symconst_addr_name,   /**< The SymConst is a symbolic pointer to be filled in
	                           by the linker.  The pointer is represented by a string.
	                           symconst_symbol is ident *. */
	symconst_addr_ent,    /**< The SymConst is a symbolic pointer to be filled in
	                           by the linker.  The pointer is represented by an entity.
	                           symconst_symbol is entity *. */
	symconst_ofs_ent,     /**< The SymConst is the offset of its entity in the entities
	                           owner type. */
	symconst_enum_const,  /**< The SymConst is a enumeration constant of an
	                           enumeration type. */
	symconst_label        /**< The SymConst is a label address. */
} symconst_kind;

/** Returns non-zero if s symconst kind has a type attribute */
#define SYMCONST_HAS_TYPE(kind) ((kind) <= symconst_type_align)

/** Returns non-zero if s symconst kind has an ident attribute */
#define SYMCONST_HAS_ID(kind) ((kind) == symconst_addr_name)

/** Returns non-zero if s symconst kind has an entity attribute */
#define SYMCONST_HAS_ENT(kind) ((kind) == symconst_addr_ent || (kind) == symconst_ofs_ent)

/** Returns non-zero if s symconst kind has an enum_const attribute */
#define SYMCONST_HAS_ENUM(kind) ((kind) == symconst_enum_const)

/** Returns non-zero if s symconst kind has a label attribute */
#define SYMCONST_HAS_LABEL(kind) ((kind) == symconst_label)

/** SymConst attribute.
 *
 *  This union contains the symbolic information represented by the node.
 */
typedef union symconst_symbol {
	ir_type       *type_p;    /**< The type of a SymConst. */
	ident         *ident_p;   /**< The ident of a SymConst. */
	ir_entity     *entity_p;  /**< The entity of a SymConst. */
	ir_enum_const *enum_p;    /**< The enumeration constant of a SymConst. */
	ir_label_t    label;      /**< The label of a SymConst. */
} symconst_symbol;

/** Get the kind of the SymConst. */
symconst_kind get_SymConst_kind(const ir_node *node);
/** Set the kind of the SymConst. */
void          set_SymConst_kind(ir_node *node, symconst_kind num);

/** Only to access SymConst of kind type_tag or size.  Else assertion: */
ir_type  *get_SymConst_type(ir_node *node);
void     set_SymConst_type(ir_node *node, ir_type *tp);

/** Only to access SymConst of kind addr_name.  Else assertion: */
ident   *get_SymConst_name(const ir_node *node);
void     set_SymConst_name(ir_node *node, ident *name);

/** Only to access SymConst of kind addr_ent.  Else assertion: */
ir_entity *get_SymConst_entity(const ir_node *node);
void       set_SymConst_entity(ir_node *node, ir_entity *ent);

/** Only to access SymConst of kind symconst_enum_const.  Else assertion: */
ir_enum_const *get_SymConst_enum(const ir_node *node);
void           set_SymConst_enum(ir_node *node, ir_enum_const *ec);

/** Sets both: type and ptrinfo.  Needed to treat the node independent of
   its semantics.  Does a memcpy for the memory sym points to. */
/* write 'union': firmjni then does not create a method... */
union symconst_symbol get_SymConst_symbol(const ir_node *node);
void                  set_SymConst_symbol(ir_node *node,
                                          union symconst_symbol sym);

/** Only to access SymConst of kind symconst_label.  Else assertion: */
ir_label_t get_SymConst_label(const ir_node *node);
void       set_SymConst_label(ir_node *node, ir_label_t label);


/** Access the type of the value represented by the SymConst.
 *
 *  Example: primitive type int for SymConst size. */
ir_type *get_SymConst_value_type(ir_node *node);
void    set_SymConst_value_type(ir_node *node, ir_type *tp);

ir_node   *get_Sel_mem(ir_node *node);
void       set_Sel_mem(ir_node *node, ir_node *mem);
ir_node   *get_Sel_ptr(ir_node *node);  /* ptr to the object to select from */
void       set_Sel_ptr(ir_node *node, ir_node *ptr);
ir_node   **get_Sel_index_arr(ir_node *node);
int        get_Sel_n_indexs(ir_node *node);
ir_node   *get_Sel_index(ir_node *node, int pos);
void       set_Sel_index(ir_node *node, int pos, ir_node *index);
ir_entity *get_Sel_entity(ir_node *node); /* entity to select */
void       set_Sel_entity (ir_node *node, ir_entity *ent);

/**
 * Projection numbers for result of Call node: use for Proj nodes!
 */
typedef enum {
	pn_Call_M_regular = pn_Generic_M_regular, /**< The memory result. */
	pn_Call_X_regular = pn_Generic_X_regular, /**< The control flow result when no exception occurs. */
	pn_Call_X_except  = pn_Generic_X_except,  /**< The control flow result branching to the exception handler. */
	pn_Call_T_result  = pn_Generic_other,     /**< The tuple containing all (0, 1, 2, ...) results. */
	pn_Call_M_except,                         /**< The memory result in case the called method terminated with
	                                               an exception. */
	pn_Call_P_value_res_base,                 /**< A pointer to the memory region containing copied results
	                                               passed by value (for compound result types). */
	pn_Call_max                               /**< number of projections from a Call */
} pn_Call;   /* Projection numbers for Call. */
#define pn_Call_M pn_Call_M_regular

ir_node *get_Call_mem(ir_node *node);
void     set_Call_mem(ir_node *node, ir_node *mem);
ir_node *get_Call_ptr(ir_node *node);
void     set_Call_ptr(ir_node *node, ir_node *ptr);
ir_node **get_Call_param_arr(ir_node *node);
/** Gets the number of parameters of a call. */
int      get_Call_n_params(ir_node *node);
/** Gets the call parameter at position pos. */
ir_node *get_Call_param(ir_node *node, int pos);
/** Sets the call parameter at position pos. */
void     set_Call_param(ir_node *node, int pos, ir_node *param);
/** Gets the type of a call. */
ir_type *get_Call_type(ir_node *node);
/** Sets the type of a call. */
void     set_Call_type(ir_node *node, ir_type *tp);
/** Gets the arity of a call. Identical to get_Call_n_params(). */
int      get_Call_arity(ir_node *node);

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
int        Call_has_callees(ir_node *node);
int        get_Call_n_callees(ir_node *node);
ir_entity *get_Call_callee(ir_node *node, int pos);

/** Set the full callee array.
 *
 *  The passed array is copied. Assumes current_ir_graph set properly! */
void    set_Call_callee_arr(ir_node *node, const int n, ir_entity **arr);
void    remove_Call_callee_arr(ir_node *node);

ir_node  *get_CallBegin_ptr(ir_node *node);
void      set_CallBegin_ptr(ir_node *node, ir_node *ptr);
ir_node  *get_CallBegin_call(ir_node *node);
void      set_CallBegin_call(ir_node *node, ir_node *call);

/* For unary and binary arithmetic operations the access to the
   operands can be factored out.  Left is the first, right the
   second arithmetic value  as listed in tech report 1999-44.
   unops are: Minus, Abs, Not, Conv, Cast
   binops are: Add, Sub, Mul, Quot, DivMod, Div, Mod, And, Or, Eor, Shl,
   Shr, Shrs, Rot, Cmp */
int      is_unop(const ir_node *node);
ir_node *get_unop_op(const ir_node *node);
void     set_unop_op(ir_node *node, ir_node *op);
int      is_binop(const ir_node *node);
ir_node *get_binop_left(const ir_node *node);
void     set_binop_left(ir_node *node, ir_node *left);
ir_node *get_binop_right(const ir_node *node);
void     set_binop_right(ir_node *node, ir_node *right);

ir_node *get_Add_left(const ir_node *node);
void     set_Add_left(ir_node *node, ir_node *left);
ir_node *get_Add_right(const ir_node *node);
void     set_Add_right(ir_node *node, ir_node *right);

ir_node *get_Sub_left(const ir_node *node);
void     set_Sub_left(ir_node *node, ir_node *left);
ir_node *get_Sub_right(const ir_node *node);
void     set_Sub_right(ir_node *node, ir_node *right);

ir_node *get_Minus_op(const ir_node *node);
void     set_Minus_op(ir_node *node, ir_node *op);

ir_node *get_Mul_left(const ir_node *node);
void     set_Mul_left(ir_node *node, ir_node *left);
ir_node *get_Mul_right(const ir_node *node);
void     set_Mul_right(ir_node *node, ir_node *right);

ir_node *get_Mulh_left(const ir_node *node);
void     set_Mulh_left(ir_node *node, ir_node *left);
ir_node *get_Mulh_right(const ir_node *node);
void     set_Mulh_right(ir_node *node, ir_node *right);

ir_node *get_Quot_left(const ir_node *node);
void     set_Quot_left(ir_node *node, ir_node *left);
ir_node *get_Quot_right(const ir_node *node);
void     set_Quot_right(ir_node *node, ir_node *right);
ir_node *get_Quot_mem(ir_node *node);
void     set_Quot_mem(ir_node *node, ir_node *mem);
ir_mode *get_Quot_resmode(const ir_node *node);
void     set_Quot_resmode(ir_node *node, ir_mode *mode);

/**
 * Projection numbers for Quot: use for Proj nodes!
 */
typedef enum {
	pn_Quot_M         = pn_Generic_M_regular, /**< Memory result. */
	pn_Quot_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Quot_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_Quot_res       = pn_Generic_other,     /**< Result of computation. */
	pn_Quot_max                               /**< number of projections from a Quot */
} pn_Quot;  /* Projection numbers for Quot. */

ir_node *get_DivMod_left(const ir_node *node);
void     set_DivMod_left(ir_node *node, ir_node *left);
ir_node *get_DivMod_right(const ir_node *node);
void     set_DivMod_right(ir_node *node, ir_node *right);
ir_node *get_DivMod_mem(ir_node *node);
void     set_DivMod_mem(ir_node *node, ir_node *mem);
ir_mode *get_DivMod_resmode(const ir_node *node);
void     set_DivMod_resmode(ir_node *node, ir_mode *mode);

/**
 * Projection numbers for DivMod: use for Proj nodes!
 */
typedef enum {
	pn_DivMod_M         = pn_Generic_M_regular, /**< Memory result. */
	pn_DivMod_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_DivMod_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_DivMod_res_div   = pn_Generic_other,     /**< Result of computation a / b. */
	pn_DivMod_res_mod,                          /**< Result of computation a % b. */
	pn_DivMod_max                               /**< number of projections from a DivMod */
} pn_DivMod;  /* Projection numbers for DivMod. */

ir_node *get_Div_left(const ir_node *node);
void     set_Div_left(ir_node *node, ir_node *left);
ir_node *get_Div_right(const ir_node *node);
void     set_Div_right(ir_node *node, ir_node *right);
ir_node *get_Div_mem(ir_node *node);
void     set_Div_mem(ir_node *node, ir_node *mem);
ir_mode *get_Div_resmode(const ir_node *node);
void     set_Div_resmode(ir_node *node, ir_mode *mode);

/**
 * Projection numbers for Div: use for Proj nodes!
 */
typedef enum {
	pn_Div_M         = pn_Generic_M_regular, /**< Memory result. */
	pn_Div_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Div_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_Div_res       = pn_Generic_other,     /**< Result of computation. */
	pn_Div_max                               /**< number of projections from a Div */
} pn_Div;  /* Projection numbers for Div. */

ir_node *get_Mod_left(const ir_node *node);
void     set_Mod_left(ir_node *node, ir_node *left);
ir_node *get_Mod_right(const ir_node *node);
void     set_Mod_right(ir_node *node, ir_node *right);
ir_node *get_Mod_mem(ir_node *node);
void     set_Mod_mem(ir_node *node, ir_node *mem);
ir_mode *get_Mod_resmode(const ir_node *node);
void     set_Mod_resmode(ir_node *node, ir_mode *mode);

/**
 * Projection numbers for Mod: use for Proj nodes!
 */
typedef enum {
	pn_Mod_M         = pn_Generic_M_regular, /**< Memory result.    */
	pn_Mod_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Mod_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_Mod_res       = pn_Generic_other,     /**< Result of computation. */
	pn_Mod_max                               /**< number of projections from a Mod */
} pn_Mod;  /* Projection numbers for Mod. */

ir_node *get_Abs_op(const ir_node *node);
void     set_Abs_op(ir_node *node, ir_node *op);

ir_node *get_And_left(const ir_node *node);
void     set_And_left(ir_node *node, ir_node *left);
ir_node *get_And_right(const ir_node *node);
void     set_And_right(ir_node *node, ir_node *right);

ir_node *get_Or_left(const ir_node *node);
void     set_Or_left(ir_node *node, ir_node *left);
ir_node *get_Or_right(const ir_node *node);
void     set_Or_right(ir_node *node, ir_node *right);

ir_node *get_Eor_left(const ir_node *node);
void     set_Eor_left(ir_node *node, ir_node *left);
ir_node *get_Eor_right(const ir_node *node);
void     set_Eor_right(ir_node *node, ir_node *right);

ir_node *get_Not_op(const ir_node *node);
void     set_Not_op(ir_node *node, ir_node *op);

/**
 * Projection numbers for Cmp are defined several times.
 * The bit patterns are used for various tests, so don't change.
 * The "unordered" values are possible results of comparing
 * floating point numbers.
 * Note that the encoding is imported, so do NOT change the order.
 */
typedef enum {
	pn_Cmp_False = 0,                             /**< false */
	pn_Cmp_Eq    = 1,                             /**< equal */
	pn_Cmp_Lt    = 2,                             /**< less */
	pn_Cmp_Le    = pn_Cmp_Eq|pn_Cmp_Lt,           /**< less or equal */
	pn_Cmp_Gt    = 4,                             /**< greater */
	pn_Cmp_Ge    = pn_Cmp_Eq|pn_Cmp_Gt,           /**< greater or equal */
	pn_Cmp_Lg    = pn_Cmp_Lt|pn_Cmp_Gt,           /**< less or greater */
	pn_Cmp_Leg   = pn_Cmp_Lt|pn_Cmp_Eq|pn_Cmp_Gt, /**< less, equal or greater = ordered */
	pn_Cmp_Uo    = 8,                             /**< unordered */
	pn_Cmp_Ue    = pn_Cmp_Uo|pn_Cmp_Eq,           /**< unordered or equal */
	pn_Cmp_Ul    = pn_Cmp_Uo|pn_Cmp_Lt,           /**< unordered or less */
	pn_Cmp_Ule   = pn_Cmp_Uo|pn_Cmp_Eq|pn_Cmp_Lt, /**< unordered, less or equal */
	pn_Cmp_Ug    = pn_Cmp_Uo|pn_Cmp_Gt,           /**< unordered or greater */
	pn_Cmp_Uge   = pn_Cmp_Uo|pn_Cmp_Eq|pn_Cmp_Gt, /**< unordered, greater or equal */
	pn_Cmp_Ne    = pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Gt, /**< unordered, less or greater = not equal */
	pn_Cmp_True  = 15                             /**< true */
	/* not_mask = Leg*/   /* bits to flip to negate comparison * @@ hack for JNI interface */
} pn_Cmp;   /* Projection numbers for Cmp */
/* #define not_mask pn_Cmp_Leg */

/** returns the pnc name from an pnc constant */
const char *get_pnc_string(int pnc);

/** Calculates the negated (Complement(R)) pnc condition. */
int         get_negated_pnc(int pnc, ir_mode *mode);

/** Calculates the inversed (R^-1) pnc condition, i.e., "<" --> ">" */
int         get_inversed_pnc(int pnc);

/** An alternative name for get_inversed_pnc() that can be better memorized. */
#define get_mirrored_pnc(pnc)  get_inversed_pnc(pnc)

ir_node *get_Cmp_left(const ir_node *node);
void     set_Cmp_left(ir_node *node, ir_node *left);
ir_node *get_Cmp_right(const ir_node *node);
void     set_Cmp_right(ir_node *node, ir_node *right);

ir_node *get_Shl_left(const ir_node *node);
void     set_Shl_left(ir_node *node, ir_node *left);
ir_node *get_Shl_right(const ir_node *node);
void     set_Shl_right(ir_node *node, ir_node *right);

ir_node *get_Shr_left(const ir_node *node);
void     set_Shr_left(ir_node *node, ir_node *left);
ir_node *get_Shr_right(const ir_node *node);
void     set_Shr_right(ir_node *node, ir_node *right);

ir_node *get_Shrs_left(const ir_node *node);
void     set_Shrs_left(ir_node *node, ir_node *left);
ir_node *get_Shrs_right(const ir_node *node);
void     set_Shrs_right(ir_node *node, ir_node *right);

ir_node *get_Rot_left(const ir_node *node);
void     set_Rot_left(ir_node *node, ir_node *left);
ir_node *get_Rot_right(const ir_node *node);
void     set_Rot_right(ir_node *node, ir_node *right);

ir_node *get_Conv_op(const ir_node *node);
void     set_Conv_op(ir_node *node, ir_node *op);
int      get_Conv_strict(const ir_node *node);
void     set_Conv_strict(ir_node *node, int flag);

/* Does Cast need a mem operator?
 * Cast should only depend on the type, not on the state of an
 * entity.  But:  we initialize various fields after Alloc, that
 * are accessed in the cast.  This required some precaution, to
 * get the right memory into the Loads generated from the cast.
 */
ir_node *get_Cast_op(const ir_node *node);
void     set_Cast_op(ir_node *node, ir_node *op);
ir_type *get_Cast_type(const ir_node *node);
void     set_Cast_type(ir_node *node, ir_type *to_tp);

/** Checks for upcast.
 *
 * Returns true if the Cast node casts a class type to a super type.
 * Works also for pointers to classes (recursively).
 *
 * Needs typeinfo calculated.
 */
int is_Cast_upcast(ir_node *node);

/** Checks for downcast.
 *
 * Returns true if the Cast node casts a class type to a sub type.
 * Works also for pointers to classes (recursively).
 *
 * Needs typeinfo calculated.
 */
int is_Cast_downcast(ir_node *node);


/** Returns true if n is Phi or Filter in interprocedural_view.
   Returns false if irg in phase building and the Phi has zero
   predecessors: it's a Phi0. */
int       is_Phi(const ir_node *n);
/** Returns true if irg in phase building and the Phi has zero
   predecessors. It's a Phi0 then. */
int       is_Phi0(const ir_node *n);
/* These routines also work for Filter nodes in interprocedural view. */
ir_node **get_Phi_preds_arr(ir_node *node);
int       get_Phi_n_preds(const ir_node *node);
ir_node  *get_Phi_pred(const ir_node *node, int pos);
void      set_Phi_pred(ir_node *node, int pos, ir_node *pred);

ir_node  *get_Filter_pred(ir_node *node);
void      set_Filter_pred(ir_node *node, ir_node *pred);
long      get_Filter_proj(ir_node *node);
void      set_Filter_proj(ir_node *node, long proj);
/* set the interprocedural predecessors, ...d_arr uses current_ir_graph.
 * @@@ Maybe better:  arity is zero if no cg preds. */
void     set_Filter_cg_pred_arr(ir_node * node, int arity, ir_node ** in);
void     set_Filter_cg_pred(ir_node * node, int pos, ir_node * pred);
int      get_Filter_n_cg_preds(ir_node *node);
ir_node *get_Filter_cg_pred(ir_node *node, int pos);

/** Return true if parameter is a memory operation.
 *
 *  A memory operation is an operation that changes the
 *  memory.  I.e., a Load or a Store operation.
 */
int is_memop(ir_node *node);
ir_node *get_memop_mem(ir_node *node);
void     set_memop_mem(ir_node *node, ir_node *mem);
ir_node *get_memop_ptr(ir_node *node);
void     set_memop_ptr(ir_node *node, ir_node *ptr);

/**
 * Projection numbers for Load: use for Proj nodes!
 */
typedef enum {
	pn_Load_M         = pn_Generic_M_regular, /**< Memory result. */
	pn_Load_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Load_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_Load_res       = pn_Generic_other,     /**< Result of load operation. */
	pn_Load_max                               /**< number of projections from a Load */
} pn_Load;  /* Projection numbers for Load. */

ir_node       *get_Load_mem(ir_node *node);
void           set_Load_mem(ir_node *node, ir_node *mem);
ir_node       *get_Load_ptr(ir_node *node);
void           set_Load_ptr(ir_node *node, ir_node *ptr);
ir_mode       *get_Load_mode(ir_node *node);
void           set_Load_mode(ir_node *node, ir_mode *mode);
ir_volatility  get_Load_volatility(ir_node *node);
void           set_Load_volatility(ir_node *node, ir_volatility volatility);
ir_align       get_Load_align(ir_node *node);
void           set_Load_align(ir_node *node, ir_align align);

/**
 * Projection numbers for Store: use for Proj nodes!
 */
typedef enum {
  pn_Store_M         = pn_Generic_M_regular, /**< Memory result. */
  pn_Store_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
  pn_Store_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
  pn_Store_max       = pn_Generic_other      /**< number of projections from a Store */
} pn_Store;  /* Projection numbers for Store. */

ir_node       *get_Store_mem(ir_node *node);
void           set_Store_mem(ir_node *node, ir_node *mem);
ir_node       *get_Store_ptr(ir_node *node);
void           set_Store_ptr(ir_node *node, ir_node *ptr);
ir_node       *get_Store_value(ir_node *node);
void           set_Store_value(ir_node *node, ir_node *value);
ir_volatility  get_Store_volatility(ir_node *node);
void           set_Store_volatility(ir_node *node, ir_volatility volatility);
ir_align       get_Store_align(ir_node *node);
void           set_Store_align(ir_node *node, ir_align align);

/**
 * Projection numbers for Alloc: use for Proj nodes!
 */
typedef enum {
	pn_Alloc_M         = pn_Generic_M_regular, /**< Memory result. */
	pn_Alloc_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Alloc_X_except  = pn_Generic_X_except,  /**< Execution result if exception occurred. */
	pn_Alloc_res       = pn_Generic_other,     /**< Result of allocation. */
	pn_Alloc_max                               /**< number of projections from an Alloc */
} pn_Alloc;  /* Projection numbers for Alloc. */

ir_node *get_Alloc_mem(ir_node *node);
void     set_Alloc_mem(ir_node *node, ir_node *mem);
ir_node *get_Alloc_size(ir_node *node);
void     set_Alloc_size(ir_node *node, ir_node *size);
ir_type *get_Alloc_type(ir_node *node);
void     set_Alloc_type(ir_node *node, ir_type *tp);

/** The allocation place. */
typedef enum {
  stack_alloc,          /**< Alloc allocates the object on the stack. */
  heap_alloc            /**< Alloc allocates the object on the heap. */
} where_alloc;

where_alloc  get_Alloc_where(ir_node *node);
void         set_Alloc_where(ir_node *node, where_alloc where);

ir_node *get_Free_mem(ir_node *node);
void     set_Free_mem(ir_node *node, ir_node *mem);
ir_node *get_Free_ptr(ir_node *node);
void     set_Free_ptr(ir_node *node, ir_node *ptr);
ir_node *get_Free_size(ir_node *node);
void     set_Free_size(ir_node *node, ir_node *size);
ir_type *get_Free_type(ir_node *node);
void     set_Free_type(ir_node *node, ir_type *tp);

where_alloc  get_Free_where(ir_node *node);
void         set_Free_where(ir_node *node, where_alloc where);

ir_node **get_Sync_preds_arr(ir_node *node);
int       get_Sync_n_preds(ir_node *node);
ir_node  *get_Sync_pred(ir_node *node, int pos);
void      set_Sync_pred(ir_node *node, int pos, ir_node *pred);
void      add_Sync_pred(ir_node *node, ir_node *pred);

/** Returns the source language type of a Proj node.
 * Must be an atomic type.  Mode of type must be mode of node.
 */
ir_type  *get_Proj_type(ir_node *node);

/** Return the predecessor of a Proj node. */
ir_node  *get_Proj_pred(const ir_node *node);
void      set_Proj_pred(ir_node *node, ir_node *pred);
/** Return the projection number of a Proj node. */
long      get_Proj_proj(const ir_node *node);
void      set_Proj_proj(ir_node *node, long proj);

ir_node **get_Tuple_preds_arr(ir_node *node);
int       get_Tuple_n_preds(ir_node *node);
ir_node  *get_Tuple_pred(ir_node *node, int pos);
void      set_Tuple_pred(ir_node *node, int pos, ir_node *pred);

ir_node  *get_Id_pred(ir_node *node);
void      set_Id_pred(ir_node *node, ir_node *pred);

/** Confirm has a single result and returns 'value' unchanged.
 *  The node expresses a restriction on 'value':
 *  'value' 'cmp' 'bound' == true.                                 */
ir_node      *get_Confirm_value(ir_node *node);
void          set_Confirm_value(ir_node *node, ir_node *value);
ir_node      *get_Confirm_bound(ir_node *node);
void          set_Confirm_bound(ir_node *node, ir_node *bound);
pn_Cmp        get_Confirm_cmp(const ir_node *node);
void          set_Confirm_cmp(ir_node *node, pn_Cmp cmp);

/*
 * Mux Support: Note that Psi nodes with one condition can be handled
 * like Mux nodes, and the access functions work as expected.
 */
ir_node *get_Mux_sel(ir_node *node);
void     set_Mux_sel(ir_node *node, ir_node *sel);
ir_node *get_Mux_false(ir_node *node);
void     set_Mux_false(ir_node *node, ir_node *ir_false);
ir_node *get_Mux_true (ir_node *node);
void     set_Mux_true (ir_node *node, ir_node *ir_true);

ir_node *get_Psi_cond(ir_node *node, int pos);
void     set_Psi_cond(ir_node *node, int pos, ir_node *cond);
ir_node *get_Psi_val(ir_node *node, int pos);
void     set_Psi_val(ir_node *node, int pos, ir_node *val);
ir_node *get_Psi_default(ir_node *node);
void     set_Psi_default(ir_node *node, ir_node *val);
int      get_Psi_n_conds(ir_node *node);

/**
 * Projection numbers for result of CopyB node: use for Proj nodes!
 */
typedef enum {
	pn_CopyB_M_regular = pn_Generic_M_regular, /**< The memory result. */
	pn_CopyB_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_CopyB_X_except  = pn_Generic_X_except,  /**< The control flow result branching to the exception handler */
	pn_CopyB_M_except  = pn_Generic_other,     /**< The memory result in case the runtime function terminated with
	                                                an exception */
	pn_CopyB_max                               /**< number of projections from a CopyB */
} pn_CopyB;   /* Projection numbers for CopyB. */
#define pn_CopyB_M pn_CopyB_M_regular

ir_node *get_CopyB_mem(ir_node *node);
void     set_CopyB_mem(ir_node *node, ir_node *mem);
ir_node *get_CopyB_dst(ir_node *node);
void     set_CopyB_dst(ir_node *node, ir_node *dst);
ir_node *get_CopyB_src(ir_node *node);
void     set_CopyB_src(ir_node *node, ir_node *src);
ir_type *get_CopyB_type(ir_node *node);
void     set_CopyB_type(ir_node *node, ir_type *data_type);

/**
 * Projection numbers for result of InstOf node: use for Proj nodes!
 */
typedef enum {
	pn_InstOf_M_regular = pn_Generic_M_regular, /**< The memory result. */
	pn_InstOf_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_InstOf_X_except  = pn_Generic_X_except,  /**< The control flow result branching to the exception handler */
	pn_InstOf_res       = pn_Generic_other,     /**< The checked object pointer. */
	pn_InstOf_M_except,                         /**< The memory result in case the runtime function terminated with
	                                                 an exception */
	pn_InstOf_max                               /**< number of projections from an InstOf */
} pn_InstOf;
#define pn_InstOf_M pn_InstOf_M_regular

/** InstOf access */
ir_type *get_InstOf_type(ir_node *node);
void    set_InstOf_type(ir_node *node, ir_type *type);
ir_node *get_InstOf_store(ir_node *node);
void    set_InstOf_store(ir_node *node, ir_node *obj);
ir_node *get_InstOf_obj(ir_node *node);
void    set_InstOf_obj(ir_node *node, ir_node *obj);

/**
 * Projection numbers for Raise.
 */
typedef enum {
	pn_Raise_X = 0,  /**< The control flow to the exception handler. */
	pn_Raise_M = 1,  /**< The Memory result. */
	pn_Raise_max     /**< number of projections from a Raise */
} pn_Raise;  /* Projection numbers for Raise. */

ir_node *get_Raise_mem(ir_node *node);
void     set_Raise_mem(ir_node *node, ir_node *mem);
ir_node *get_Raise_exo_ptr(ir_node *node);  /* PoinTeR to EXception Object */
void     set_Raise_exo_ptr(ir_node *node, ir_node *exoptr);

/**
 * Projection numbers for result of Bound node: use for Proj nodes!
 */
typedef enum {
	pn_Bound_M         = pn_Generic_M_regular, /**< The memory result. */
	pn_Bound_X_regular = pn_Generic_X_regular, /**< Execution result if no exception occurred. */
	pn_Bound_X_except  = pn_Generic_X_except,  /**< The control flow result branching to the exception handler */
	pn_Bound_res       = pn_Generic_other,     /**< The checked index. */
	pn_Bound_max                               /**< number of projections from a Bound */
} pn_Bound;

/** Returns the memory input of a Bound operation. */
ir_node *get_Bound_mem(ir_node *bound);
void     set_Bound_mem(ir_node *bound, ir_node *mem);

/** Returns the index input of a Bound operation. */
ir_node *get_Bound_index(ir_node *bound);
void     set_Bound_index(ir_node *bound, ir_node *idx);

/** Returns the lower bound input of a Bound operation. */
ir_node *get_Bound_lower(ir_node *bound);
void     set_Bound_lower(ir_node *bound, ir_node *lower);

/** Returns the upper bound input of a Bound operation. */
ir_node *get_Bound_upper(ir_node *bound);
void     set_Bound_upper(ir_node *bound, ir_node *upper);

/** Return the operand of a Pin node. */
ir_node *get_Pin_op(const ir_node *pin);
void    set_Pin_op(ir_node *pin, ir_node *node);

/** A input/output constraint attribute */
typedef struct {
	unsigned       pos;           /**< The inputs/output position for this constraint. */
	ident          *constraint;   /**< The constraint for this input/output. */
	ir_mode        *mode;         /**< The mode of the constraint. */
} ir_asm_constraint;

/** Return the assembler text of an ASM pseudo node. */
ident *get_ASM_text(const ir_node *node);
/** Return the number of input constraints for an ASM node. */
int get_ASM_n_input_constraints(const ir_node *node);
/** Return the input constraints for an ASM node. */
const ir_asm_constraint *get_ASM_input_constraints(const ir_node *node);
/** Return the number of output constraints for an ASM node.  */
int get_ASM_n_output_constraints(const ir_node *node);
/** Return the output constraints for an ASM node. */
const ir_asm_constraint *get_ASM_output_constraints(const ir_node *node);
/** Return the number of clobbered registers for an ASM node.  */
int get_ASM_n_clobbers(const ir_node *node);
/** Return the list of clobbered registers for an ASM node. */
ident **get_ASM_clobbers(const ir_node *node);

/*
 *
 * NAME Auxiliary routines
 *
 */

/** Returns operand of node if node is a Proj. */
ir_node *skip_Proj(ir_node *node);
/** Returns operand of node if node is a Proj. */
const ir_node *skip_Proj_const(const ir_node *node);
/** Returns operand of node if node is a Id. */
ir_node *skip_Id(ir_node *node);   /* Old name is skip_nop(). */
/** Returns corresponding operand of Tuple if node is a Proj from
   a Tuple. */
ir_node *skip_Tuple(ir_node *node);
/** returns operand of node if node is a Cast. */
ir_node *skip_Cast(ir_node *node);
/** Returns operand of node if node is a Confirm */
ir_node *skip_Confirm(ir_node *node);
/** Skip all high-level Operations. */
ir_node *skip_HighLevel(ir_node *node);
/** Returns true if irn is a Const node. */
int      is_Const(const ir_node *node);
/** Returns true if a node is a Conv node. */
int      is_Conv(const ir_node *node);
/** Returns true if a node is a strictConv node. */
int      is_strictConv(const ir_node *node);
/** Returns true if a node is a Cast node. */
int      is_Cast(const ir_node *node);
/** Returns true if node is a Bad node. */
int      is_Bad(const ir_node *node);
/** Returns true if node is a NoMem node. */
int      is_NoMem(const ir_node *node);
/** Returns true if node is a Start node. */
int      is_Start(const ir_node *node);
/** Returns true if node is a Minus node. */
int      is_Minus(const ir_node *node);
/** Returns true if node is a Mod node. */
int      is_Mod(const ir_node *node);
/** Returns true if node is a Div node. */
int      is_Div(const ir_node *node);
/** Returns true if node is a DivMod node. */
int      is_DivMod(const ir_node *node);
/** Returns true if node is a Quot node. */
int      is_Quot(const ir_node *node);
/** Returns true if node is an Add node. */
int      is_Add(const ir_node *node);
/** Returns true if node is an And node. */
int      is_And(const ir_node *node);
/** Returns true if node is an Or node. */
int      is_Or(const ir_node *node);
/** Returns true if node is an Eor node. */
int      is_Eor(const ir_node *node);
/** Returns true if node is a Sub node. */
int      is_Sub(const ir_node *node);
/** Returns true if node is a Not node. */
int      is_Not(const ir_node *node);
/** Returns true if node is a Shl node. */
int      is_Shl(const ir_node *node);
/** Returns true if node is a Shr node. */
int      is_Shr(const ir_node *node);
/** Returns true if node is a Shrs node. */
int      is_Shrs(const ir_node *node);
/** Returns true if node is a Rot node. */
int      is_Rot(const ir_node *node);
/** Returns true if node is a Psi node. */
int      is_Psi(const ir_node *node);
/** Returns true if node is a Tuple node. */
int      is_Tuple(const ir_node *node);
/** Returns true if the node is not a Block */
int      is_no_Block(const ir_node *node);
/** Returns true if the node is a Block */
int      is_Block(const ir_node *node);
/** Returns true if node is an Unknown node. */
int      is_Unknown(const ir_node *node);
/** Returns true if node is a Return node. */
int      is_Return(const ir_node *node);
/** Returns true if node is a Call node. */
int      is_Call(const ir_node *node);
/** Returns true if node is a Sel node. */
int      is_Sel(const ir_node *node);
/** Returns true if node is a Mul node. */
int      is_Mul(const ir_node *node);
/** Returns true if node is a Mulh node. */
int      is_Mulh(const ir_node *node);
/** Returns true if node is a Mux node or a Psi with only one condition. */
int      is_Mux(const ir_node *node);
/** Returns true if node is a Load node. */
int      is_Load(const ir_node *node);
/** Returns true if node is a Store node. */
int      is_Store(const ir_node *node);
/** Returns true if node is a Sync node. */
int      is_Sync(const ir_node *node);
/** Returns true if node is a Confirm node. */
int      is_Confirm(const ir_node *node);
/** Returns true if node is a Pin node. */
int      is_Pin(const ir_node *node);
/** Returns true if node is a SymConst node. */
int      is_SymConst(const ir_node *node);
/** Returns true if node is a Cond node. */
int      is_Cond(const ir_node *node);
/** Returns true of node is a CopyB node */
int      is_CopyB(const ir_node *node);
/** Returns true if node is a Cmp node. */
int      is_Cmp(const ir_node *node);
/** Returns true if node is an Alloc node */
int      is_Alloc(const ir_node *node);
/** Returns true if a node is a Jmp node */
int      is_Jmp(const ir_node *node);
/** Returns true if a node is a Raise node */
int      is_Raise(const ir_node *node);
/** Returns true if a node is an ASM node */
int      is_ASM(const ir_node *node);
/** Returns true if node is a Proj node or a Filter node in
 * intraprocedural view */
int      is_Proj(const ir_node *node);
/** Returns true if the operation manipulates control flow:
   Start, End, Jmp, Cond, Return, Raise, Bad, CallBegin, EndReg, EndExcept */
int is_cfop(const ir_node *node);

/** Returns true if the operation manipulates interprocedural control flow:
    CallBegin, EndReg, EndExcept */
int is_ip_cfop(const ir_node *node);
/** Returns true if the operation can change the control flow because
    of an exception: Call, Quot, DivMod, Div, Mod, Load, Store, Alloc,
    Bad. Raise is not fragile, but a unconditional jump. */
int is_fragile_op(const ir_node *node);
/** Returns the memory operand of fragile operations. */
ir_node *get_fragile_op_mem(ir_node *node);
/** Returns the result mode of a Div operation. */
ir_mode *get_divop_resmod(const ir_node *node);

/** Returns true if the operation is a forking control flow
 *  operation: Cond. */
int is_irn_forking(const ir_node *node);

/** Return the type associated with the value produced by n
 *  if the node remarks this type as it is the case for
 *  Cast, Const, SymConst and some Proj nodes or unknown_type. */
ir_type *get_irn_type(ir_node *n);

/** Return the type attribute of a node n (SymConst, Call, Alloc, Free,
    Cast) or NULL.*/
ir_type *get_irn_type_attr(ir_node *n);

/** Return the entity attribute of a node n (SymConst, Sel) or NULL. */
ir_entity *get_irn_entity_attr(ir_node *n);

/** Returns non-zero for constant-like nodes. */
int is_irn_constlike(const ir_node *node);

/**
 * Returns non-zero for nodes that must be always optimized
 * (Phi, Id. Proj, Cond, Block, Confirm ...).
 */
int is_irn_always_opt(const ir_node *node);

/**
 * Returns non-zero for nodes that are allowed to have keep-alives and
 * are neither Block nor PhiM.
 */
int is_irn_keep(const ir_node *node);

/**
 * Returns non-zero for nodes that are always placed in the start block.
 */
int is_irn_start_block_placed(const ir_node *node);

/**
 * Returns non-zero for nodes that are machine operations.
 */
int is_irn_machine_op(const ir_node *node);

/**
 * Returns non-zero for nodes that are machine operands.
 */
int is_irn_machine_operand(const ir_node *node);

/**
 * Returns non-zero for nodes that have the n'th user machine flag set.
 */
int is_irn_machine_user(const ir_node *node, unsigned n);

/**
 * A type to express conditional jump predictions.
 */
typedef enum {
	COND_JMP_PRED_NONE,        /**< No jump prediction. Default. */
	COND_JMP_PRED_TRUE,        /**< The True case is predicted. */
	COND_JMP_PRED_FALSE        /**< The False case is predicted. */
} cond_jmp_predicate;

/** Gets the string representation of the jump prediction .*/
const char *get_cond_jmp_predicate_name(cond_jmp_predicate pred);

/** Returns the conditional jump prediction of a Cond node. */
cond_jmp_predicate get_Cond_jmp_pred(const ir_node *cond);

/** Sets a new conditional jump prediction. */
void set_Cond_jmp_pred(ir_node *cond, cond_jmp_predicate pred);

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
unsigned firm_register_additional_node_data(unsigned size);

/**
 * Return a pointer to the node attributes.
 * Needed for user-defined nodes.
 */
void *get_irn_generic_attr(ir_node *node);
const void *get_irn_generic_attr_const(const ir_node *node);

/**
 * Returns the unique node index for the node in its graph.
 * This index is used to access phase information for this node.
 */
unsigned get_irn_idx(const ir_node *node);

/**
 * Sets the debug information of a node.
 *
 * @param n   The node.
 * @param db  The debug info.
 */
void set_irn_dbg_info(ir_node *n, dbg_info *db);

/**
 * Returns the debug information of an node.
 *
 * @param n   The node.
 */
dbg_info *get_irn_dbg_info(const ir_node *n);

/*-----------------------------------------------------------------*/
/** Debug aides                                                   **/
/*-----------------------------------------------------------------*/


/** Debug print the node.
 *
 *  Writes the node, all its attributes and the predecessors to stdout if DEBUG_libfirm
 *  is set.  Else does nothing. */
void    dump_irn(ir_node *n);

/*@}*/ /* end of ir_node group definition */

#endif
