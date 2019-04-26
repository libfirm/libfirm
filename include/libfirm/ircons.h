/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Various irnode constructors. Automatic construction of SSA
 *          representation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Boris Boesler,
 *          Michael Beck
 */

/**
 *  @file
 *
 *  documentation no more supported since 2001
 *
 *  IR node construction.
 *
 *    This file documents all datatypes and constructors needed to
 *    build a FIRM representation of a procedure.  The constructors are
 *    also implemented in this file.
 *
 *    The documentation also gives a short manual how to use the library.
 *
 *    For extensive documentation of FIRM see UKA Techreport 1999-14.
 *
 *
 *    Three kinds of nodes
 *    --------------------
 *
 *      There are three kinds of nodes known to the IR:  entities,
 *      types, and ir_nodes
 *
 *      + ir_nodes are the actual nodes of the FIRM intermediate representation.
 *        They represent operations on the data of the program and control flow
 *        operations.
 *
 *      + entity ==> implemented in entity.h
 *        Refers to a single entity of the compiled program, e.g. a field of a
 *        class or a method.  If a method or variable can not be assigned to
 *        a method or class or the like, it is a global object.
 *
 *      + types ==> implemented in type.h
 *        With types type information is represented.  There are several type
 *       nodes.
 *
 *    Implementation of the FIRM operations: ir_node
 *    ----------------------------------------------
 *
 *      Ir_nodes represent operations on the data of the program and control
 *      flow operations.  Examples of ir_nodes:  Add, Jmp, Cmp
 *
 *      FIRM is a dataflow graph.  A dataflow graph is a directed graph, so that
 *      every node has incoming and outgoing edges.  A node is executable if
 *      every input at its incoming edges is available.  Execution of the
 *      dataflow graph is started at the Start node which has no incoming edges
 *      and ends when the End node executes, even if there are still executable
 *      or not executed nodes.  (Is this true, or must all executable nodes be
 *      executed?)  (There are exceptions to the dataflow paradigma that all
 *      inputs have to be available before a node can execute: Phi, Block.  See
 *      UKA Techreport 1999-14.)
 *
 *      The implementation of FIRM differs from the view as a dataflow graph.
 *      To allow fast traversion of the graph edges are implemented as
 *      C-pointers.  Inputs to nodes are not ambiguous, the results can be used
 *      by several other nodes.  Each input can be implemented as a single
 *      pointer to a predecessor node, outputs need to be lists of pointers to
 *      successors.  Therefore a node contains pointers to its predecessors so
 *      that the implementation is a dataflow graph with reversed edges.  It has
 *      to be traversed bottom up.
 *
 *      All nodes of the IR have the same basic structure.  They are
 *      distinguished by a field containing the opcode.
 *
 *      The fields of an ir_node:
 *
 *      kind             A firm_kind tag containing k_ir_node.  This is useful for
 *                       dynamically checking the type of a node.
 *
 *      *op              This ir_op gives the opcode as a tag and a string
 *                       and the number of attributes of an ir_node.  There is
 *                       one statically allocated struct ir_op for each opcode.
 *
 *      *mode            The ir_mode of the operation represented by this firm
 *                       node.  The mode of the operation is the mode of its
 *                       result.  A Firm mode is a datatype as known to the
 *                       target, not a type of the source language.
 *
 *      visit            A flag for traversing the IR.
 *
 *      **in             An array with pointers to the node's predecessors.
 *
 *      *link            A pointer to an ir_node.  With this pointer all Phi nodes
 *                       are attached to a Block, i.e. a Block points to its
 *                       first Phi node, this node points to the second Phi node
 *                       in the Block and so forth.  Used in mature_immBlock
 *                       to find all Phi nodes to be matured.  It's also used to
 *                       annotate a node with a better, optimized version of it.
 *
 *      attr             An attr struct containing the attributes of the nodes. The
 *                       attributes depend on the opcode of the node.  The number
 *                       of these attributes is given in op.
 *
 *    The struct ir_op
 *    ----------------
 *                       Not yet documented. See irop.h.
 *
 *    The struct ir_mode
 *    ------------------
 *                       Not yet documented. See irmode.h.
 *
 *    GLOBAL VARIABLES -- now also fields of ir_graph.
 *    ================
 *
 *    current_ir_graph   Points to the current ir_graph.  All constructors for
 *                       nodes add nodes to this graph.
 *
 *    ir_visited         An int used as flag to traverse the ir_graph.
 *
 *    block_visited      An int used as a flag to traverse block nodes in the
 *                       graph.
 *
 *                       Others not yet documented.
 *
 *
 *
 *    CONSTRUCTOR FOR IR_GRAPH --> see irgraph.h
 *    ========================
 *
 *
 *    PROCEDURE TO CONSTRUCT AN IR GRAPH --> see also Firm tutorial
 *    ==================================
 *
 *    This library supplies several interfaces to construct a FIRM graph for
 *    a program:
 *    - A "comfortable" interface generating SSA automatically.  Automatically
 *      computed predecessors of nodes need not be specified in the constructors.
 *      (new_<Node> constructurs and a set of additional routines.)
 *    - A less comfortable interface where all predecessors except the block
 *      an operation belongs to need to be specified.  SSA must be constructed
 *      by hand.  (new_<Node> constructors and set_cur_block()).  This interface
 *      is called "block oriented".  It automatically calles the local optimizations
 *      for each new node.
 *    - An even less comfortable interface where the block needs to be specified
 *      explicitly.  This is called the "raw" interface. (new_r_<Node>
 *      constructors).
 *
 *    To use the functionality of the comfortable interface correctly the front
 *    end needs to follow certain protocols.  This is explained in the
 *    following.  To build a correct IR with the other interfaces study the
 *    semantics of the firm node (See tech-reprot UKA 1999-14).  For the
 *    construction of types and entities see the documentation in these modules.
 *
 *    First the front end needs to decide which variables and values used in a
 *    procedure can be represented by dataflow edges.  These are variables that
 *    need not be saved to memory as they cause no side effects visible out of
 *    the procedure.  Often these are all compiler generated variables and
 *    simple local variables of the procedure as integers, reals and pointers.
 *    The front end has to count and number these variables.
 *
 *    First an ir_graph needs to be constructed with new_ir_graph.  The
 *    constructor gets the number of local variables.  The graph is held in the
 *    global variable irg.
 *
 *    Now the construction of the procedure can start.  Several basic blocks can
 *    be constructed in parallel, but the code within each block needs to be
 *    constructed (almost) in program order.
 *
 *    A global variable holds the current basic block.  All (non block) nodes
 *    generated are added to this block.  The current block can be set with
 *    set_cur_block(block).  If several blocks are constructed in parallel block
 *    switches need to be performed constantly.
 *
 *    To generate a Block node (with the comfortable interface), its predecessor
 *    control flow nodes need not be known.  In case of cyclic control flow
 *    these can not be known when the block is constructed.  With
 *    add_immBlock_pred(block, cfnode) predecessors can be added to the block.
 *    If all predecessors are added to the block mature_immBlock(b) needs to be
 *    called.  Calling mature_immBlock early improves the efficiency of the Phi
 *    node construction algorithm.  But if several  blocks are constructed at
 *    once, mature_immBlock must only be called after performing all set_values
 *    and set_stores in the block!  (See documentation of new_immBlock
 *    constructor.)
 *
 *    The constructors of arithmetic nodes require that their predecessors are
 *    mentioned.  Sometimes these are available in the Frontend as the
 *    predecessors have just been generated by the front end.  If they are local
 *    values, the predecessors can be obtained from the library with a call to
 *    get_value(local_val_nr).  (local_val_nr needs to be administered by the
 *    Frontend.)  A call to get_value triggers the generation of Phi nodes.  If
 *    an arithmetic operation produces a local value, this value needs to be
 *    passed to the library by set_value(node, local_val_nr).
 *    In straight line code these two operations just remember and return the
 *    pointer to nodes producing the value.  If the value passes block
 *    boundaries Phi nodes can be inserted.
 *    Similar routines exist to manage the Memory operands: set_store and
 *    get_store.
 *
 *    Several nodes produce more than one result.  An example is the Div node.
 *    Such nodes return tuples of values.  From these individual values can be
 *    extracted by proj nodes.
 *
 *    The following example illustrates the construction of a simple basic block
 *    with two predecessors stored in variables cf_pred1 and cf_pred2,
 *    containing the code
 *      a = a div a;
 *    and finally jumping to an other block.  The variable a got the local_val_nr
 *    42 by the front end.
 *
 *    ir_node *example(ir_node *cf_pred1, ir_node *cf_pred2)
 *    {
 *      ir_node *this_block = new_immBlock();
 *      add_immBlock_pred(this_block, cf_pred1);
 *      add_immBlock_pred(this_block, cf_pred2);
 *      mature_immBlock(this_block);
 *      set_cur_block(this_block);
 *      ir_node *a_val = get_value(42, mode_Iu);
 *      ir_node *div   = new_Div(get_store(), a_val, a_val, mode_Iu);
 *      ir_node *mem   = new_Proj(div, mode_M, pn_Div_M);
 *      ir_node *res   = new_Proj(div, mode_Iu, pn_Div_res);
 *      set_store(mem);
 *      set_value(res, 42);
 *      ir_node *cf_op = new_Jmp();
 *      return cf_op;
 *    }
 *
 *    For further information look at the documentation of the nodes and
 *    constructors and at the paragraph COPING WITH DATA OBJECTS at the end of
 *    this documentation.
 *
 *    IR_NODES AND CONSTRUCTORS FOR IR_NODES
 *    =======================================
 *
 *    All ir_nodes are defined by a common data structure.  They are
 *    distinguished by their opcode and differ in the number of their
 *    attributes.
 *
 *    Const nodes are always added to the start block.  All other constructors
 *    add the created node to the current_block.  set_cur_block(block) allows to
 *    set the current block to block.
 *
 *    Watch for my inconsistent use of input and predecessor (dataflow view) and
 *    `the node points to' (implementation view).
 *
 *    The following description of the nodes lists four properties them if these
 *    are of interest:
 *     - the parameters to the constructor
 *     - the inputs of the Firm node
 *     - the outputs of the Firm node
 *     - attributes to the node
 *
 *    ------------
 *
 *    COPING WITH DATA OBJECTS
 *    ========================
 *
 *    Two kinds of data objects have to be distinguished for generating FIRM.
 *    First there are local variables other than arrays that are known to be
 *    alias free.  Second there are all other data objects.  For the first a
 *    common SSA representation is built, the second are modeled by saving them
 *    to memory.  The memory is treated as a single local variable, the alias
 *    problem is hidden in the content of this variable.
 *
 *    All values known in a Block are listed in the block's attribute
 *    block.graph_arr which is used to automatically insert Phi nodes.  The
 *    following two functions can be used to add a newly computed value to the
 *    array, or to get the producer of a value, i.e., the current live value.
 *
 *    void set_value(int pos, ir_node *value)
 *    -----------------------------------------------
 *
 *    Has to be called for every assignment to a local variable.  It adds the
 *    value to the array of used values at position pos.  Pos has to be a unique
 *    identifier for an entry in the procedure's definition table.  It can be
 *    used to access the value again.  Requires current_block to be set
 *    correctly.
 *
 *    ir_node *get_value(int pos, ir_mode *mode)
 *    -------------------------------------------
 *
 *    Returns the node defining the value referred to by pos. If the value is
 *    not defined in this block a Phi node is generated and all definitions
 *    reaching this Phi node are collected.  It can happen that the algorithm
 *    allocates an unnecessary Phi node, e.g. if there is only one definition of
 *    this value, but this definition reaches the currend block on several
 *    different paths.  This Phi node will be eliminated if optimizations are
 *    turned on right after its creation.  Requires current_block to be set
 *    correctly.
 *
 *    There are two special routines for the global store:
 */
#ifndef FIRM_IR_IRCONS_H
#define FIRM_IR_IRCONS_H

#include "firm_types.h"
#include "irnode.h"

#include "begin.h"

/** @addtogroup Const
 * @{
 */

/**
 * Constructor for a Const node.
 *
 * Adds the node to the start block.
 *
 * The constant represents a target value.
 *
 * @param db     A pointer for debug information.
 * @param irg    The IR graph the node  belongs to.
 * @param mode   The mode of the operands and results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_rd_Const_long(dbg_info *db, ir_graph *irg,
                                    ir_mode *mode, long value);

/** Constructor for a Const node.
 *
 * Adds the node to the start block.
 *
 * The constant represents a target value.
 *
 * @param irg    The IR graph the node  belongs to.
 * @param mode   The mode of the operands and the results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_r_Const_long(ir_graph *irg, ir_mode *mode, long value);

/**
 * @see new_rd_Const_long()
 *
 * @param db     A pointer for debug information.
 * @param mode   The mode of the operands and results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_d_Const_long(dbg_info *db, ir_mode *mode, long value);

/**
 * Make a const from a long.
 * This is just convenience for the usual
 * <code>
 * new_Const(mode, tarval_from_long(mode, ...))
 * </code>
 * pain.
 * @param mode The mode for the const.
 * @param value The value of the constant.
 * @return A new const node.
 */
FIRM_API ir_node *new_Const_long(ir_mode *mode, long value);

/** @} */

/** @addtogroup Phi
 * @{
 */

/** Constructor for memory Phi with keep-alive edge. */
FIRM_API ir_node *new_rd_Phi_loop(dbg_info *db, ir_node *block,
                                      int arity, ir_node *in[]);

/** Constructor for memory Phi with keep-alive edge. */
FIRM_API ir_node *new_r_Phi_loop(ir_node *block, int arity, ir_node *in[]);

/** Constructor for memory Phi with keep-alive edge. */
FIRM_API ir_node *new_d_Phi_loop(dbg_info *db, int arity, ir_node *in[]);

/** Constructor for memory Phi with keep-alive edge. */
FIRM_API ir_node *new_Phi_loop(int arity, ir_node *in[]);

/** @} */

/** @addtogroup Div
 * @{
 */

/** Constructor for a remainderless Div node.
 *
 * @param db      A pointer for debug information.
 * @param block   The IR block the node belongs to.
 * @param memop   The store needed to model exceptions
 * @param op1     The first operand.
 * @param op2     The second operand.
 * @param pinned  Whether the node is pinned in its block.
 */
FIRM_API ir_node *new_rd_DivRL(dbg_info *db, ir_node *block, ir_node *memop,
                               ir_node *op1, ir_node *op2, int pinned);

/** Constructor for a remainderless Div node.
 *
 * @param block   The IR block the node belongs to.
 * @param memop   The store needed to model exceptions
 * @param op1     The first operand.
 * @param op2     The second operand.
 * @param pinned  Whether the node is pinned in its block.
 */
FIRM_API ir_node *new_r_DivRL(ir_node *block, ir_node *memop,
                              ir_node *op1, ir_node *op2, int pinned);

/** Constructor for a remainderless Div node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param db      A pointer for debug information.
 * @param memop   The store needed to model exceptions
 * @param op1     The first operand.
 * @param op2     The second operand.
 * @param pinned  Whether the node is pinned in its block.
 */
FIRM_API ir_node *new_d_DivRL(dbg_info *db, ir_node *memop,
                              ir_node *op1, ir_node *op2, int pinned);

/** Constructor for a remainderless Div node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param memop   The store needed to model exceptions
 * @param op1     The first operand.
 * @param op2     The second operand.
 * @param pinned  Whether the node is pinned in its block.
 */
FIRM_API ir_node *new_DivRL(ir_node *memop, ir_node *op1, ir_node *op2,
                            int pinned);

/** @} */

/**
 * @ingroup ir_graph
 * @defgroup ir_cons Construction Support
 * @{
 */

/**
 * Global variable holding the graph which is currently constructed.
 */
FIRM_API ir_graph *current_ir_graph;

/**
 * Returns graph which is currently constructed
 */
FIRM_API ir_graph *get_current_ir_graph(void);

/**
 * Sets graph which is currently constructed
 */
FIRM_API void set_current_ir_graph(ir_graph *graph);

/** Create an immature Block.
 *
 * An immature Block has an unknown number of predecessors.  Predecessors
 * can be added with add_immBlock_pred().  Once all predecessors are
 * added the block must be matured.
 *
 * Adds the block to the graph in current_ir_graph.
 * This constructor can only be used if the graph is in state_building.
 */
FIRM_API ir_node *new_d_immBlock(dbg_info *db);
/** Create an immature Block.
 *
 * An immature Block has an unknown number of predecessors.  Predecessors
 * can be added with add_immBlock_pred().  Once all predecessors are
 * added the block must be matured.
 *
 * Adds the block to the graph in current_ir_graph.
 * This constructor can only be used if the graph is in state_building.
 */
FIRM_API ir_node *new_immBlock(void);
/** Create an immature Block.
 *
 * An immature Block has an unknown number of predecessors.  Predecessors
 * can be added with add_immBlock_pred().  Once all predecessors are
 * added the block must be matured.
 *
 * This constructor can only be used if the graph is in state_building.
 */
FIRM_API ir_node *new_r_immBlock(ir_graph *irg);
/** Create an immature Block.
 *
 * An immature Block has an unknown number of predecessors.  Predecessors
 * can be added with add_immBlock_pred().  Once all predecessors are
 * added the block must be matured.
 *
 * This constructor can only be used if the graph is in state_building.
 */
FIRM_API ir_node *new_rd_immBlock(dbg_info *db, ir_graph *irg);

/** Add a control flow edge to an immature block. */
FIRM_API void add_immBlock_pred(ir_node *immblock, ir_node *jmp);

/** Finalize a Block node, when all control flows are known. */
FIRM_API void mature_immBlock(ir_node *block);

/**
 * Sets the current block in which the following constructors place the
 * nodes they construct.
 *
 * @param target  The new current block.
 */
FIRM_API void set_cur_block(ir_node *target);
/**
 * Sets current block of a given graph.
 * @see set_cur_block()
 */
FIRM_API void set_r_cur_block(ir_graph *irg, ir_node *target);

/** Returns the current block of the current graph. */
FIRM_API ir_node *get_cur_block(void);
/** Returns current block of a given graph */
FIRM_API ir_node *get_r_cur_block(ir_graph *irg);

/** Returns the current value of a local variable.
 *
 * Use this function to obtain the last definition of the local variable
 * associated with pos.  pos must be less than the value passed as n_loc
 * to new_ir_graph.  This call automatically inserts Phi nodes.
 *
 * @param pos   The position/id of the local variable.
 * @param mode  The mode of the value to get.
 */
FIRM_API ir_node *get_value(int pos, ir_mode *mode);
/** Returns the current value of a local variable in given graph
 * @see get_value() */
FIRM_API ir_node *get_r_value(ir_graph *irg, int pos, ir_mode *mode);

/**
 * Try to guess the mode of a local variable.
 * This is done by recursively going up the control flow graph until
 * we find a definition for the variable. The mode of the first found
 * definition is returned. NULL in case no definition is found.
 *
 * @param  pos   The position/id of the local variable.
 */
FIRM_API ir_mode *ir_guess_mode(int pos);
/**
 * Try to guess the mode of a local variable in a given graph.
 */
FIRM_API ir_mode *ir_r_guess_mode(ir_graph *irg, int pos);

/** Memorize a new definition of a variable.
 *
 * Use this function to remember a new definition of the value
 * associated with pos.  pos must be less than the value passed as n_loc
 * to new_ir_graph.  This call is needed to automatically inserts Phi
 * nodes.
 *
 * @param pos    The position/id of the local variable.
 * @param value  The new value written to the local variable.
 */
FIRM_API void set_value(int pos, ir_node *value);
/** Sets current value of a variable in a given graph */
FIRM_API void set_r_value(ir_graph *irg, int pos, ir_node *value);

/** Returns the current memory state.
 *
 * Use this function to obtain the last definition of the memory
 * state.  This call automatically inserts Phi nodes for the memory
 * state value.
 */
FIRM_API ir_node *get_store(void);
/** Returns current memory state for a given graph
 * @see get_store() */
FIRM_API ir_node *get_r_store(ir_graph *irg);

/** Memorize a new definition of the memory state.
 *
 * Use this function to remember a new definition of the memory state.
 * This call is needed to automatically inserts Phi nodes.
 *
 * @param store  The new memory state.
 */
FIRM_API void set_store(ir_node *store);
/** Sets current memory state for a given graph
 * @see set_store() */
FIRM_API void set_r_store(ir_graph *irg, ir_node *store);

/** keep this node alive even if End is not control-reachable from it
 *
 * @param ka The node to keep alive.
 */
FIRM_API void keep_alive(ir_node *ka);

/** Puts the graph into state "phase_high" */
FIRM_API void irg_finalize_cons(ir_graph *irg);

/**
 * If firm is built in debug mode, verify that a newly created node is fine.
 * The normal node constructors already call this function, you only need to
 * call this yourself if you create new node constructors on your own.
 */
FIRM_API void verify_new_node(ir_node *node);

/**
 * Register a new callback for the case that the value of an uninitialized
 * variable is requested.
 */
FIRM_API void ir_set_uninitialized_local_variable_func(
		uninitialized_local_variable_func_t *func);

/** @} */

#include "end.h"

#endif
