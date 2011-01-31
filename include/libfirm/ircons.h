/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief   Various irnode constructors. Automatic construction of SSA
 *          representation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Boris Boesler,
 *          Michael Beck
 * @version $Id$
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
 *      Ir_nodes represent operations on the data of the program and control flow
 *      operations.  Examples of ir_nodes:  Add, Jmp, Cmp
 *
 *      FIRM is a dataflow graph.  A dataflow graph is a directed graph,
 *      so that every node has incoming and outgoing edges.  A node is
 *      executable if every input at its incoming edges is available.
 *      Execution of the dataflow graph is started at the Start node which
 *      has no incoming edges and ends when the End node executes, even if
 *      there are still executable or not executed nodes.  (Is this true,
 *      or must all executable nodes be executed?)  (There are exceptions
 *      to the dataflow paradigma that all inputs have to be available
 *      before a node can execute: Phi, Block.  See UKA Techreport
 *      1999-14.)
 *
 *      The implementation of FIRM differs from the view as a dataflow
 *      graph.  To allow fast traversion of the graph edges are
 *      implemented as C-pointers.  Inputs to nodes are not ambiguous, the
 *      results can be used by several other nodes.  Each input can be
 *      implemented as a single pointer to a predecessor node, outputs
 *      need to be lists of pointers to successors.  Therefore a node
 *      contains pointers to its predecessors so that the implementation is a
 *      dataflow graph with reversed edges.  It has to be traversed bottom
 *      up.
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
 *      constructors).  These nodes are not optimized.
 *
 *    To use the functionality of the comfortable interface correctly the Front
 *    End needs to follow certain protocols.  This is explained in the following.
 *    To build a correct IR with the other interfaces study the semantics of
 *    the firm node (See tech-reprot UKA 1999-14).  For the construction of
 *    types and entities see the documentation in those modules.
 *
 *    First the Frontend needs to decide which variables and values used in
 *    a procedure can be represented by dataflow edges.  These are variables
 *    that need not be saved to memory as they cause no side effects visible
 *    out of the procedure.  Often these are all compiler generated
 *    variables and simple local variables of the procedure as integers,
 *    reals and pointers.  The frontend has to count and number these variables.
 *
 *    First an ir_graph needs to be constructed with new_ir_graph.  The
 *    constructor gets the number of local variables.  The graph is held in the
 *    global variable irg.
 *
 *    Now the construction of the procedure can start.  Several basic blocks can
 *    be constructed in parallel, but the code within each block needs to
 *    be constructed (almost) in program order.
 *
 *    A global variable holds the current basic block.  All (non block) nodes
 *    generated are added to this block.  The current block can be set with
 *    set_cur_block(block).  If several blocks are constructed in parallel block
 *    switches need to be performed constantly.
 *
 *    To generate a Block node (with the comfortable interface), its predecessor
 *    control flow nodes need not be known.  In case of cyclic control flow these
 *    can not be known when the block is constructed.  With add_immBlock_pred(block,
 *    cfnode) predecessors can be added to the block.  If all predecessors are
 *    added to the block mature_immBlock(b) needs to be called.  Calling mature_immBlock
 *    early improves the efficiency of the Phi node construction algorithm.
 *    But if several  blocks are constructed at once, mature_immBlock must only
 *    be called after performing all set_values and set_stores in the block!
 *    (See documentation of new_immBlock constructor.)
 *
 *    The constructors of arithmetic nodes require that their predecessors
 *    are mentioned.  Sometimes these are available in the Frontend as the
 *    predecessors have just been generated by the frontend.  If they are local
 *    values, the predecessors can be obtained from the library with a call to
 *    get_value(local_val_nr).  (local_val_nr needs to be administered by
 *    the Frontend.)  A call to get_value triggers the generation of Phi nodes.
 *    If an arithmetic operation produces a local value, this value needs to be
 *    passed to the library by set_value(node, local_val_nr).
 *    In straight line code these two operations just remember and return the
 *    pointer to nodes producing the value.  If the value passes block boundaries
 *    Phi nodes can be inserted.
 *    Similar routines exist to manage the Memory operands: set_store and
 *    get_store.
 *
 *    Several nodes produce more than one result.  An example is the Div node.
 *    Such nodes return tuples of values.  From these individual values can be
 *    extracted by proj nodes.
 *
 *    The following example illustrates the construction of a simple basic block
 *    with two predecessors stored in variables cf_pred1 and cf_pred2, containing
 *    the code
 *      a = a div a;
 *    and finally jumping to an other block.  The variable a got the local_val_nr
 *    42 by the frontend.
 *
 *    ir_node *this_block, *cf_pred1, *cf_pred2, *a_val, *mem, *div, *res, *cf_op;
 *
 *    this_block = new_immBlock();
 *    add_immBlock_pred(this_block, cf_pred1);
 *    add_immBlock_pred(this_block, cf_pred2);
 *    mature_immBlock(this_block);
 *    a_val = get_value(42, mode_Iu);
 *    mem = get_store();
 *    div = new_Div(mem, a_val, a_val, mode_Iu);
 *    mem = new_Proj(div, mode_M, pn_Div_M);   * for the numbers for Proj see docu *
 *    res = new_Proj(div, mode_Iu, pn_Div_res);
 *    set_store(mem);
 *    set_value(res, 42);
 *    cf_op = new_Jmp();
 *
 *    For further information look at the documentation of the nodes and
 *    constructors and at the paragraph COPING WITH DATA OBJECTS at the
 *    end of this documentation.
 *
 *    The comfortable interface contains the following routines further explained
 *    below:
 *
 *    ir_node *new_immBlock (void);
 *    ir_node *new_Start    (void);
 *    ir_node *new_End      (void);
 *    ir_node *new_Jmp      (void);
 *    ir_node *new_IJmp     (ir_node *tgt);
 *    ir_node *new_Cond     (ir_node *c);
 *    ir_node *new_Return   (ir_node *store, int arity, ir_node **in);
 *    ir_node *new_Const    (ir_tarval *con);
 *    ir_node *new_SymConst (ir_mode *mode, symconst_symbol value, symconst_kind kind);
 *    ir_node *new_simpleSel (ir_node *store, ir_node *objptr, ir_entity *ent);
 *    ir_node *new_Sel    (ir_node *store, ir_node *objptr, int arity,
 *                         ir_node **in, ir_entity *ent);
 *    ir_node *new_Call   (ir_node *store, ir_node *callee, int arity,
 *                         ir_node **in, type_method *type);
 *    ir_node *new_Builtin(ir_node *store, ir_builtin_kind kind, int arity,
 *                         ir_node **in, type_method *type);
 *    ir_node *new_Add    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Sub    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Minus  (ir_node *op,  ir_mode *mode);
 *    ir_node *new_Mul    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Mulh   (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Div    (ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state);
 *    ir_node *new_Mod    (ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state;
 *    ir_node *new_And    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Or     (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Eor    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Not    (ir_node *op,                ir_mode *mode);
 *    ir_node *new_Shl    (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Shr    (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Shrs   (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Rotl   (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Cmp    (ir_node *op1, ir_node *op2);
 *    ir_node *new_Conv   (ir_node *op, ir_mode *mode);
 *    ir_node *new_Cast   (ir_node *op, ir_type *to_tp);
 *    ir_node *new_Carry  (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Borrow (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Load   (ir_node *store, ir_node *addr, ir_mode *mode, ir_cons_flags flags);
 *    ir_node *new_Store  (ir_node *store, ir_node *addr, ir_node *val, ir_cons_flags flags);
 *    ir_node *new_Alloc  (ir_node *store, ir_node *count, ir_type *alloc_type,
 *                         where_alloc where);
 *    ir_node *new_Free   (ir_node *store, ir_node *ptr, ir_node *size,
 *               ir_type *free_type, where_alloc where);
 *    ir_node *new_Proj   (ir_node *arg, ir_mode *mode, long proj);
 *    ir_node *new_NoMem  (void);
 *    ir_node *new_Mux    (ir_node *sel, ir_node *ir_false, ir_node *ir_true, ir_mode *mode);
 *    ir_node *new_CopyB  (ir_node *store, ir_node *dst, ir_node *src, ir_type *data_type);
 *    ir_node *new_InstOf (ir_node *store, ir_node obj, ir_type *ent);
 *    ir_node *new_Raise  (ir_node *store, ir_node *obj);
 *    ir_node *new_Bound  (ir_node *store, ir_node *idx, ir_node *lower, ir_node *upper);
 *    ir_node *new_Pin    (ir_node *node);
 *
 *    void add_immBlock_pred (ir_node *block, ir_node *jmp);
 *    void mature_immBlock (ir_node *block);
 *    void set_cur_block (ir_node *target);
 *    ir_node *get_value (int pos, ir_mode *mode);
 *    void set_value (int pos, ir_node *value);
 *    ir_node *get_store (void);
 *    void set_store (ir_node *store);
 *    keep_alive (ir_node ka)
 *
 *    IR_NODES AND CONSTRUCTORS FOR IR_NODES
 *    =======================================
 *
 *    All ir_nodes are defined by a common data structure.  They are distinguished
 *    by their opcode and differ in the number of their attributes.
 *
 *    Const nodes are always added to the start block.
 *    All other constructors add the created node to the current_block.
 *    swich_block(block) allows to set the current block to block.
 *
 *    Watch for my inconsistent use of input and predecessor (dataflow view)
 *    and `the node points to' (implementation view).
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
 *    ir_node *new_immBlock (void)
 *    ----------------------------
 *
 *    Creates a new block. When a new block is created it cannot be known how
 *    many predecessors this block will have in the control flow graph.
 *    Therefore the list of inputs can not be fixed at creation.  Predecessors
 *    can be added with add_immBlock_pred (block, control flow operation).
 *    With every added predecessor the number of inputs to Phi nodes also
 *    changes.
 *
 *    The block can be completed by mature_immBlock(block) if all predecessors are
 *    known.  If several blocks are built at once, mature_immBlock can only be called
 *    after set_value has been called for all values that are life at the end
 *    of the block.  This is necessary so that Phi nodes created mature_immBlock
 *    get the right predecessors in case of cyclic dependencies.  If all set_values
 *    of this block are called after maturing it and before calling get_value
 *    in some block that is control flow dependent on this block, the construction
 *    is correct.
 *
 *    Example for faulty IR construction:  (draw the graph on a paper and you'll
 *                                          get it ;-)
 *
 *      block_before_loop = new_immBlock();
 *      set_cur_block(block_before_loop);
 *      set_value(x);
 *      mature_immBlock(block_before_loop);
 *      before2header = new_Jmp;
 *
 *      loop_header = new_immBlock ();
 *      set_cur_block(loop_header);
 *      header2body - new_Jmp();
 *
 *      loop_body = new_immBlock ();
 *      set_cur_block(loop_body);
 *      body2header = new_Jmp();
 *
 *      add_immBlock_pred(loop_header, before2header);
 *      add_immBlock_pred(loop_header, body2header);
 *      add_immBlock_pred(loop_body, header2body);
 *
 *      mature_immBlock(loop_header);
 *      mature_immBlock(loop_body);
 *
 *      get_value(loop_body, x);   //  gets the Phi in loop_header
 *      set_value(loop_header, x); //  sets the value the above get_value should
 *                                 //  have returned!!!
 *
 *    Mature_immBlock also fixes the number of inputs to the Phi nodes.  Mature_immBlock
 *    should be called as early as possible, as afterwards the generation of Phi
 *    nodes is more efficient.
 *
 *    Inputs:
 *      There is an input for each control flow predecessor of the block.
 *      The input points to an instruction producing an output of type X.
 *      Possible predecessors:  Start, Jmp, Cond, Raise or Return or any node
 *      possibly causing an exception.  (Often the real predecessors are Projs.)
 *    Output:
 *      Mode BB (R), all nodes belonging to this block should consume this output.
 *      As they are strict (except Block and Phi node) it is a necessary condition
 *      that the block node executed before any other node in this block executes.
 *    Attributes:
 *      block.matured  Indicates whether the block is mature.
 *      block.**graph_arr
 *                      This attribute contains all local values valid in this
 *                      block. This is needed to build the Phi nodes and removed
 *                      if the graph is complete.  This field is used by the
 *              internal construction algorithm and should not be accessed
 *              from outside.
 *
 *
 *    ir_node *new_Block (int arity, ir_node **in)
 *    --------------------------------------------
 *
 *    Creates a new Block with the given list of predecessors.  This block
 *    is mature.  As other constructors calls optimization and verify for the
 *    block.  If one of the predecessors is Unknown (as it has to be filled in
 *    later) optimizations are skipped.  This is necessary to
 *    construct Blocks in loops.
 *
 *
 *    CONTROL FLOW OPERATIONS
 *    -----------------------
 *
 *    In each block there must be exactly one of the control flow
 *    operations Start, End, Jmp, Cond, Return or Raise.  The output of a
 *    control flow operation points to the block to be executed next.
 *
 *    ir_node *new_Start (void)
 *    -------------------------
 *
 *    Creates a start node.  Not actually needed public.  There is only one such
 *   node in each procedure which is automatically created by new_ir_graph.
 *
 *    Inputs:
 *      No inputs except the block it belongs to.
 *    Output:
 *      A tuple of 4 (5, 6) distinct values. These are labeled by the following
 *      projection numbers (pn_Start):
 *      * pn_Start_X_initial_exec    mode X, points to the first block to be exe *                                   cuted.
 *      * pn_Start_M                 mode M, the global store
 *      * pn_Start_P_frame_base      mode P, a pointer to the base of the proce  *                                   dures stack frame.
 *      * pn_Start_P_globals         mode P, a pointer to the part of the memory *                                   containing_all_ global things.
 *      * pn_Start_T_args            mode T, a tuple containing all arguments of *                                   the procedure.
 *
 *
 *    ir_node *new_End (void)
 *    -----------------------
 *
 *    Creates an end node.  Not actually needed public.  There is only one such
 *   node in each procedure which is automatically created by new_ir_graph.
 *
 *    Inputs:
 *      No inputs except the block it belongs to.
 *    Output:
 *      No output.
 *
 *    ir_node *new_Jmp (void)
 *    -----------------------
 *
 *    Creates a Jmp node.
 *
 *    Inputs:
 *      The block the node belongs to
 *    Output:
 *      Control flow to the next block.
 *
 *    ir_node *new_IJmp (ir_node *tgt)
 *    -----------------------
 *
 *    Creates an IJmp node.
 *
 *    Inputs:
 *      The node that represents the target jump address
 *    Output:
 *      Control flow to an unknown target, must be pinned by
 *      the End node.
 *
 *    ir_node *new_Cond (ir_node *c)
 *    ------------------------------
 *
 *    Creates a Cond node.  There are two versions of this node.
 *
 *    The Boolean Cond:
 *    Input:
 *      A value of mode b.
 *    Output:
 *      A tuple of two control flows.  The first is taken if the input is
 *      false, the second if it is true.
 *
 *    The Switch Cond:
 *    Input:
 *      A value of mode I_u. (i)
 *    Output:
 *      A tuple of n control flows.  If the Cond's input is i, control
 *      flow will proceed along output i. If the input is >= n control
 *      flow proceeds along output n.
 *
 *    ir_node *new_Return (ir_node *store, int arity, ir_node **in)
 *    -------------------------------------------------------------
 *
 *    The Return node has as inputs the results of the procedure.  It
 *    passes the control flow to the end_block.
 *
 *    Inputs:
 *      The memory state.
 *      All results.
 *    Output
 *      Control flow to the end block.
 *
 *
 *    ir_node *new_Const (ir_tarval *con)
 *    -----------------------------------------------
 *
 *    Creates a constant in the constant table and adds a Const node
 *    returning this value to the start block. The mode is derived
 *    from the tarval.
 *
 *    Parameters:
 *      *con             Points to an entry in the constant table.
 *                       This pointer is added to the attributes of
 *                       the node (self->attr.con)
 *    Inputs:
 *      No inputs except the block it belogns to.
 *    Output:
 *      The constant value.
 *    Attribute:
 *      attr.con   A tarval* pointer to the proper entry in the constant
 *                 table.
 *
 *    ir_node *new_SymConst (ir_mode *mode, union symconst_symbol value, symconst_addr_ent kind)
 *    -----------------------------------------------------------------------------------------
 *
 *    There are several symbolic constants:
 *     symconst_type_tag   The symbolic constant represents a type tag.
 *     symconst_type_size  The symbolic constant represents the size of a type.
 *     symconst_type_align The symbolic constant represents the alignment of a type.
 *     symconst_addr_ent   The symbolic constant represents the address of an entity.
 *     symconst_ofs_ent    The symbolic constant represents the offset of an
 *                         entity in its owner type.
 *     symconst_enum_const The symbolic constant is a enumeration constant of an
 *                         enumeration type.
 *
 *    Parameters
 *      mode        P for SymConsts representing addresses, Iu otherwise.
 *      value       The type, ident, entity or enum constant, depending on the
 *                  kind
 *      kind        The kind of the symbolic constant, see the list above.
 *
 *    Inputs:
 *      No inputs except the block it belongs to.
 *    Output:
 *      A symbolic constant.
 *
 *    Attributes:
 *      attr.i.num       The symconst_addr_ent, i.e. one of
 *                        -symconst_type_tag
 *                        -symconst_type_size
 *                        -symconst_type_align
 *                        -symconst_addr_ent
 *
 *    If the attr.i.num is symconst_type_tag, symconst_type_size or symconst_type_align,
 *    the node contains an attribute:
 *
 *      attr.i.*type,    a pointer to a type_class.
 *        if it is linkage_ptr_info it contains
 *      attr.i.*ptrinfo,  an ident holding information for the linker.
 *
 *    ---------------
 *
 *    ir_node *new_simpleSel (ir_node *store, ir_node *frame, ir_entity *sel)
 *    -----------------------------------------------------------------------
 *
 *
 *    Selects an entity from a compound type. This entity can be a field or
 *    a method.
 *
 *    Parameters:
 *      *store     The memory in which the object the entity should be selected
 *                 from is allocated.
 *      *frame     The pointer to the object.
 *      *sel       The entity to select.
 *
 *    Inputs:
 *      The memory containing the object.
 *      A pointer to the object.
 *      An unsigned integer.
 *    Output:
 *      A pointer to the selected entity.
 *    Attributes:
 *      attr.sel   Pointer to the entity
 *
 *
 *    ir_node *new_Sel (ir_node *store, ir_node *frame, int arity, ir_node **in,
 *    --------------------------------------------------------------------------
 *                      ir_entity *sel)
 *                      ---------------
 *
 *    Selects a field from an array type.  The entity has as owner the array, as
 *    type the arrays element type.  The indices to access an array element are
 *    given also.
 *
 *    Parameters:
 *      *store     The memory in which the object the entity should be selected from
 *                 is allocated.
 *      *frame     The pointer to the object.
 *      *arity     number of array indices.
 *      *in        array with index inputs to the node.
 *      *sel       The entity to select.
 *
 *    Inputs:
 *      The memory containing the object.
 *      A pointer to the object.
 *      As much unsigned integer as there are array expressions.
 *    Output:
 *      A pointer to the selected entity.
 *    Attributes:
 *      attr.sel   Pointer to the entity
 *
 *    The constructors new_Sel and new_simpleSel generate the same IR nodes.
 *    simpleSel just sets the arity of the index inputs to zero.
 *
 *
 *    ARITHMETIC OPERATIONS
 *    ---------------------
 *
 *    ir_node *new_Call (ir_node *store, ir_node *callee, int arity, ir_node **in,
 *    ----------------------------------------------------------------------------
 *                       type_method *type)
 *                       ------------------
 *
 *    Creates a procedure call.
 *
 *    Parameters
 *      *store           The actual store.
 *      *callee          A pointer to the called procedure.
 *      arity            The number of procedure parameters.
 *      **in             An array with the pointers to the parameters.
 *                       The constructor copies this array.
 *      *type            Type information of the procedure called.
 *
 *    Inputs:
 *      The store, the callee and the parameters.
 *    Output:
 *      A tuple containing the eventually changed store and the procedure
 *      results.
 *    Attributes:
 *      attr.call        Contains the attributes for the procedure.
 *
 *    ir_node *new_Builtin(ir_node *store, ir_builtin_kind kind, int arity, ir_node **in,
 *    -----------------------------------------------------------------------------------
 *                       type_method *type)
 *                       ------------------
 *
 *    Creates a builtin call.
 *
 *    Parameters
 *      *store           The actual store.
 *      kind             Describes the called builtin.
 *      arity            The number of procedure parameters.
 *      **in             An array with the pointers to the parameters.
 *                       The constructor copies this array.
 *      *type            Type information of the procedure called.
 *
 *    Inputs:
 *      The store, the kind and the parameters.
 *    Output:
 *      A tuple containing the eventually changed store and the procedure
 *      results.
 *    Attributes:
 *      attr.builtin     Contains the attributes for the called builtin.
 *
 *    ir_node *new_Add (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Sub (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Minus (ir_node *op, ir_mode *mode)
 *    -----------------------------------------------
 *
 *    Unary Minus operations on integer and floating point values.
 *
 *    ir_node *new_Mul (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Mulh (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Returns the high order bits of a n*n=2n multiplication.
 *
 *    ir_node *new_Div (ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state)
 *    ------------------------------------------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Mod (ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state)
 *    ------------------------------------------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_And (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Or (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    -----------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Eor (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Not (ir_node *op, ir_mode *mode)
 *    ---------------------------------------------
 *
 *    This node constructs a constant where all bits are set to one
 *    and a Eor of this constant and the operator.  This simulates a
 *    Not operation.
 *
 *    ir_node *new_Shl (ir_node *op, ir_node *k, ir_mode *mode)
 *    ---------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Shr (ir_node *op, ir_node *k, ir_mode *mode)
 *    ---------------------------------------------------------
 *
 *    Logic shift right, i.e., zero extended.
 *
 *
 *    ir_node *new_Shrs (ir_node *op, ir_node *k, ir_mode *mode)
 *    ----------------------------------------------------------
 *
 *    Arithmetic shift right, i.e., sign extended.
 *
 *    ir_node *new_Rotl (ir_node *op, ir_node *k, ir_mode *mode)
 *    ---------------------------------------------------------
 *
 *    Rotates the operand to the left by k bits.
 *
 *    ir_node *new_Carry (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Calculates the Carry value for integer addition. Used only
 *    in lowering code.
 *
 *    ir_node *new_Borrow (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Calculates the Borrow value for integer substraction. Used only
 *    in lowering code.
 *
 *    ir_node *new_Conv (ir_node *op, ir_mode *mode)
 *    ---------------------------------------------
 *
 *    Mode conversion.  For allowed conversions see UKA Tech Report
 *    1999-14.
 *
 *    ir_node *new_Cmp (ir_node *op1, ir_node *op2)
 *    ---------------------------------------------
 *
 *    Input:
 *      The two values to be compared.
 *    Output:
 *      A 16-tuple containing the results of the 16 different comparisons.
 *      The following is a list giving the comparisons and a projection
 *      number (pn_Cmp) to use in Proj nodes to extract the proper result.
 *        pn_Cmp_False false
 *        pn_Cmp_Eq    equal
 *        pn_Cmp_Lt    less
 *        pn_Cmp_Le    less or equal
 *        pn_Cmp_Gt    greater
 *        pn_Cmp_Ge    greater of equal
 *        pn_Cmp_Lg    less or greater
 *        pn_Cmp_Leg   less, equal or greater = ordered
 *        pn_Cmp_Uo    unordered
 *        pn_Cmp_Ue    unordered or equal
 *        pn_Cmp_Ul    unordered or less
 *        pn_Cmp_Ule   unordered, less or equal
 *        pn_Cmp_Ug    unordered or greater
 *        pn_Cmp_Uge   unordered, greater or equal
 *        pn_Cmp_Ne    unordered, less or greater = not equal
 *        pn_Cmp_True  true
 *
 *
 *
 *    ------------
 *
 *    In general, Phi nodes are automaitcally inserted.  In some cases, if
 *    all predecessors of a block are known, an explicit Phi node constructor
 *    is needed.  E.g., to construct a FIRM graph for a statement as
 *      a = (b==c) ? 2 : 5;
 *
 *    ir_node *new_Phi (int arity, ir_node **in, ir_mode *mode)
 *    ---------------------------------------------------------
 *
 *    Creates a Phi node. The in's order has to correspond to the order
 *    of in's of current_block.  This is not checked by the library!
 *    If one of the predecessors is Unknown (as it has to be filled in
 *    later) optimizations are skipped.  This is necessary to
 *    construct Phi nodes in loops.
 *
 *    Parameter
 *      arity            number of predecessors
 *      **in             array with predecessors
 *      *mode            The mode of its inputs and output.
 *    Inputs:
 *      A Phi node has as many inputs as the block it belongs to.
 *      Each input points to a definition of the same value on a
 *      different path in the control flow.
 *    Output
 *      The definition valid in this block.
 *
 *    ir_node *new_Mux (ir_node *sel, ir_node *ir_false, ir_node *ir_true, ir_mode *mode)
 *    -----------------------------------------------------------------------------------
 *
 *    Creates a Mux node. This node implements the following semantic:
 *    If the sel node (which must be of mode_b) evaluates to true, its value is
 *    ir_true, else ir_false;
 *
 *
 *
 *    OPERATIONS TO MANAGE MEMORY EXPLICITLY
 *    --------------------------------------
 *
 *    ir_node *new_Load (ir_node *store, ir_node *addr, ir_mode *mode, ir_cons_flags flags)
 *    -------------------------------------------------------------------------------------
 *
 *    The Load operation reads a value from memory.
 *
 *    Parameters:
 *    *store        The current memory.
 *    *addr         A pointer to the variable to be read in this memory.
 *    *mode         The mode of the value to be loaded.
 *     flags        Additional flags for alignment, volatility and pin state.
 *
 *    Inputs:
 *      The memory and a pointer to a variable in this memory.
 *    Output:
 *      A tuple of the memory, a control flow to be taken in case of
 *      an exception and the loaded value.
 *
 *    ir_node *new_Store (ir_node *store, ir_node *addr, ir_node *val, ir_cons_flags flags)
 *    -------------------------------------------------------------------------------------
 *
 *    The Store operation writes a value to a variable in memory.
 *
 *    Inputs:
 *      The memory, a pointer to a variable in this memory and the value
 *      to write to this variable.
 *    Output:
 *      A tuple of the changed memory and a control flow to be taken in
 *      case of an exception.
 *
 *    ir_node *new_Alloc (ir_node *store, ir_node *count, ir_type *alloc_type,
 *    -----------------------------------------------------------------------
 *                        where_alloc where)
 *                        ------------------
 *
 *    The Alloc node allocates a new variable.  It can be specified whether the
 *    variable should be allocated to the stack or to the heap.
 *
 *    Parameters:
 *      *store       The memory which shall contain the new variable.
 *      *count       This field is for allocating arrays, it specifies how
 *                   many array elements are to be allocated.
 *      *alloc_type  The type of the allocated variable. In case of allocating
 *                   arrays this has to be the array type, not the type of the
 *                   array elements.
 *      where        Where to allocate the variable, either heap_alloc or stack_alloc.
 *
 *    Inputs:
 *      A memory and an unsigned integer.
 *    Output:
 *      A tuple of the changed memory, a control flow to be taken in
 *      case of an exception and the pointer to the new variable.
 *    Attributes:
 *      a.where          Indicates where the variable is allocated.
 *      a.*type          A pointer to the class the allocated data object
 *                       belongs to.
 *
 *    ir_node *new_Free (ir_node *store, ir_node *ptr, ir_node *size, ir_type *free_type,
 *    -----------------------------------------------------------------------------------
 *                        where_alloc where)
 *                        ------------------
 *
 *    The Free node frees memory of the given variable.
 *
 *    Parameters:
 *      *store       The memory which shall contain the new variable.
 *      *ptr         The pointer to the object to free.
 *      *size        The number of objects of type free_type to free in a sequence.
 *      *free_type   The type of the freed variable.
 *      where        Where the variable was allocated, either heap_alloc or stack_alloc.
 *
 *    Inputs:
 *      A memory, a pointer and an unsigned integer.
 *    Output:
 *      The changed memory.
 *    Attributes:
 *      f.*type          A pointer to the type information of the freed data object.
 *
 *    Not Implemented!
 *
 *    ir_node *new_Sync (int arity, ir_node **in)
 *    -------------------------------------------
 *
 *    The Sync operation unifies several partial memory blocks.  These blocks
 *    have to be pairwise disjunct or the values in common locations have to
 *    be identical.  This operation allows to specify all operations that eventually
 *    need several partial memory blocks as input with a single entrance by
 *    unifying the memories with a preceding Sync operation.
 *
 *    Parameters
 *      arity    The number of memories to synchronize.
 *      **in     An array of pointers to nodes that produce an output of
 *               type memory.
 *    Inputs
 *      Several memories.
 *    Output
 *      The unified memory.
 *
 *
 *    SPECIAL OPERATIONS
 *    ------------------
 *
 *    ir_node *new_NoMem (void)
 *    -----------------------------------------------------------------------------------
 *
 *    Returns the unique NoMem node current_ir_graph->no_mem.
 *    This node is used as input for operations that need a Memory, but do not
 *    change it like Div by const != 0, analyzed calls etc.
 *
 *    ir_node *new_Proj (ir_node *arg, ir_mode *mode, long proj)
 *    ----------------------------------------------------------
 *
 *    Selects one entry of a tuple.  This is a hidden edge with attributes.
 *
 *    Parameters
 *      *arg      A node producing a tuple.
 *      *mode     The mode of the value to project.
 *      proj      The position of the value in the tuple.
 *    Input:
 *      The tuple.
 *    Output:
 *      The value.
 *
 *    ir_node *new_Tuple (int arity, ir_node **in)
 *    --------------------------------------------
 *
 *    Builds a Tuple from single values.  This is needed to implement
 *    optimizations that remove a node that produced a tuple.  The node can be
 *    replaced by the Tuple operation so that the following Proj nodes have not to
 *    be changed.  (They are hard to find due to the implementation with pointers
 *    in only one direction.)  The Tuple node is smaller than any other
 *    node, so that a node can be changed into a Tuple by just changing its
 *    opcode and giving it a new in array.
 *
 *    Parameters
 *      arity    The number of tuple elements.
 *      **in     An array containing pointers to the nodes producing the
 *               tuple elements.
 *
 *    ir_node *new_Id (ir_node *val, ir_mode *mode)
 *    ---------------------------------------------
 *
 *    The single output of the Id operation is its input.  Also needed
 *    for optimizations.
 *
 *
 *    HIGH LEVEL OPERATIONS
 *    ---------------------
 *
 *    ir_node *new_CopyB (ir_node *store, ir_node *dst, ir_node *src, ir_type *data_type)
 *    -----------------------------------------------------------------------------------
 *
 *    Describes a high level block copy of a compound type from address src to
 *    address dst. Must be lowered to a Call to a runtime memory copy function.
 *
 *
 *    HIGH LEVEL OPERATIONS: Exception Support
 *    ----------------------------------------
 *    See TechReport 1999-14, chapter Exceptions.
 *
 *    ir_node *new_InstOf(ir_node *store, ir_node *ptr, ir_type *type);
 *    -----------------------------------------------------------------------------------
 *
 *    Describes a high level type check. Must be lowered to a Call to a runtime check
 *    function.
 *
 *    ir_node *new_Raise (ir_node *store, ir_node *obj)
 *    -------------------------------------------------
 *
 *    Raises an exception.  Unconditional change of control flow.  Writes
 *    an explicit Except variable to memory to pass it to the exception
 *    handler.  Must be lowered to a Call to a runtime check
 *    function.
 *
 *    Inputs:
 *      The memory state.
 *      A pointer to the Except variable.
 *    Output:
 *      A tuple of control flow and the changed memory state.  The control flow
 *      points to the exception handler if it is definied in this procedure,
 *      else it points to the end_block.
 *
 *    ir_node *new_Bound  (ir_node *store, ir_node *idx, ir_node *lower, ir_node *upper);
 *    -----------------------------------------------------------------------------------
 *
 *    Describes a high level bounds check. Must be lowered to a Call to a runtime check
 *    function.
 *
 *    ir_node *new_Pin  (ir_node *node);
 *    -----------------------------------------------------------------------------------
 *
 *    Pin the value of the node node in the current block  No users of the Pin node can
 *    float above the Block of the Pin. The node cannot float behind this block. Often
 *    used to Pin the NoMem node.
 *
 *
 *    COPING WITH DATA OBJECTS
 *    ========================
 *
 *    Two kinds of data objects have to be distinguished for generating
 *    FIRM.  First there are local variables other than arrays that are
 *    known to be alias free.  Second there are all other data objects.
 *    For the first a common SSA representation is built, the second
 *    are modeled by saving them to memory.  The memory is treated as
 *    a single local variable, the alias problem is hidden in the
 *    content of this variable.
 *
 *    All values known in a Block are listed in the block's attribute,
 *    block.**graph_arr which is used to automatically insert Phi nodes.
 *    The following two functions can be used to add a newly computed value
 *    to the array, or to get the producer of a value, i.e., the current
 *    live value.
 *
 *    inline void set_value (int pos, ir_node *value)
 *    -----------------------------------------------
 *
 *    Has to be called for every assignment to a local variable.  It
 *    adds the value to the array of used values at position pos.  Pos
 *    has to be a unique identifier for an entry in the procedure's
 *    definition table.  It can be used to access the value again.
 *    Requires current_block to be set correctly.
 *
 *    ir_node *get_value (int pos, ir_mode *mode)
 *    -------------------------------------------
 *
 *    Returns the node defining the value referred to by pos. If the
 *    value is not defined in this block a Phi node is generated and
 *    all definitions reaching this Phi node are collected.  It can
 *    happen that the algorithm allocates an unnecessary Phi node,
 *    e.g. if there is only one definition of this value, but this
 *    definition reaches the currend block on several different
 *    paths.  This Phi node will be eliminated if optimizations are
 *    turned on right after its creation.
 *    Requires current_block to be set correctly.
 *
 *    There are two special routines for the global store:
 *
 *    void set_store (ir_node *store)
 *    -------------------------------
 *
 *    Adds the store to the array of known values at a reserved
 *    position.
 *    Requires current_block to be set correctly.
 *
 *    ir_node *get_store (void)
 *    -------------------------
 *
 *    Returns the node defining the actual store.
 *    Requires current_block to be set correctly.
 *
 *
 *    inline void keep_alive (ir_node *ka)
 *    ------------------------------------
 *
 *    Keep this node alive because it is (might be) not in the control
 *    flow from Start to End.  Adds the node to the list in the end
 *   node.
 *
 */
#ifndef FIRM_IR_IRCONS_H
#define FIRM_IR_IRCONS_H

#include "firm_types.h"
#include "begin.h"
#include "irnode.h"

/*-------------------------------------------------------------------------*/
/* The raw interface                                                       */
/*-------------------------------------------------------------------------*/

/**
 * Constructor for a Const node.
 *
 * Adds the node to the start block.
 *
 * Constructor for a Const node. The constant represents a target
 * value.  Sets the type information to type_unknown.  (No more
 * supported: If tv is entity derives a somehow useful type.)
 *
 * @param *db    A pointer for debug information.
 * @param *irg   The IR graph the node  belongs to.
 * @param *mode  The mode of the operands and results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_rd_Const_long(dbg_info *db, ir_graph *irg,
                                    ir_mode *mode, long value);

/** Constructor for a SymConst node.
 *
 *  This is the constructor for a symbolic constant.
 *    There are several kinds of symbolic constants:
 *    - symconst_type_tag   The symbolic constant represents a type tag.  The
 *                          type the tag stands for is given explicitly.
 *    - symconst_type_size  The symbolic constant represents the size of a type.
 *                          The type of which the constant represents the size
 *                          is given explicitly.
 *    - symconst_type_align The symbolic constant represents the alignment of a
 *                          type.  The type of which the constant represents the
 *                          size is given explicitly.
 *    - symconst_addr_ent   The symbolic constant represents the address of an
 *                          entity (variable or method).  The variable is given
 *                          explicitly by a firm entity.
 *    - symconst_ofs_ent    The symbolic constant represents the offset of an
 *                          entity in its owner type.
 *    - symconst_enum_const The symbolic constant is a enumeration constant of
 *                          an enumeration type.
 *
 *    Inputs to the node:
 *      No inputs except the block it belongs to.
 *    Outputs of the node.
 *      An unsigned integer (I_u) or a pointer (P).
 *
 *    Mention union in declaration so that the firmjni generator recognizes that
 *    it can not cast the argument to an int.
 *
 * @param *db     A pointer for debug information.
 * @param *irg    The IR graph the node  belongs to.
 * @param mode    The mode for the SymConst.
 * @param value   A type, ident, entity or enum constant depending on the
 *                SymConst kind.
 * @param kind    The kind of the symbolic constant, see the list above
 */
FIRM_API ir_node *new_rd_SymConst(dbg_info *db, ir_graph *irg, ir_mode *mode,
                                  union symconst_symbol value,
                                  symconst_kind kind);

/** Constructor for a SymConst addr_ent node.
 *
 * Same as new_rd_SymConst, except that the constructor is tailored for
 * symconst_addr_ent.
 * Adds the SymConst to the start block of irg. */
FIRM_API ir_node *new_rd_SymConst_addr_ent(dbg_info *db, ir_graph *irg,
                                           ir_mode *mode, ir_entity *symbol);

/** Constructor for a SymConst ofs_ent node.
 *
 * Same as new_rd_SymConst, except that the constructor is tailored for
 * symconst_ofs_ent.
 * Adds the SymConst to the start block of irg.
 */
FIRM_API ir_node *new_rd_SymConst_ofs_ent(dbg_info *db, ir_graph *irg,
                                          ir_mode *mode, ir_entity *symbol);

/** Constructor for a SymConst type_tag node.
 *
 * Same as new_rd_SymConst, except that the constructor is tailored for
 * symconst_type_tag.
 * Adds the SymConst to the start block of irg.
 */
FIRM_API ir_node *new_rd_SymConst_type_tag(dbg_info *db, ir_graph *irg,
                                           ir_mode *mode, ir_type *symbol);

/** Constructor for a SymConst size node.
 *
 * Same as new_rd_SymConst, except that the constructor is tailored for
 * symconst_type_size.
 * Adds the SymConst to the start block of irg. */
FIRM_API ir_node *new_rd_SymConst_size(dbg_info *db, ir_graph *irg,
                                       ir_mode *mode, ir_type *symbol);

/** Constructor for a SymConst size node.
 *
 * Same as new_rd_SymConst, except that the constructor is tailored for
 * symconst_type_align.
 * Adds the SymConst to the start block of irg.
 */
FIRM_API ir_node *new_rd_SymConst_align(dbg_info *db, ir_graph *irg,
                                        ir_mode *mode, ir_type *symbol);

/** Constructor for a simpleSel node.
 *
 *  This is a shortcut for the new_rd_Sel() constructor.  To be used for
 *  Sel nodes that do not select from an array, i.e., have no index
 *  inputs.  It adds the two parameters 0, NULL.
 *
 * @param   *db        A pointer for debug information.
 * @param   *block     The IR block the node belongs to.
 * @param   *store     The memory in which the object the entity should be
 *                     selected from is allocated.
 * @param   *objptr    The object from that the Sel operation selects a
 *                     single attribute out.
 * @param   *ent       The entity to select.
 */
FIRM_API ir_node *new_rd_simpleSel(dbg_info *db, ir_node *block, ir_node *store,
                                   ir_node *objptr, ir_entity *ent);

/** Constructor for a remainderless Div node.
 *
 * @param   *db    A pointer for debug information.
 * @param   *block The IR block the node belongs to.
 * @param   *memop The store needed to model exceptions
 * @param   *op1   The first operand.
 * @param   *op2   The second operand.
 * @param   *mode  The mode of the result.
 * @param   state  The pinned state.
 */
FIRM_API ir_node *new_rd_DivRL(dbg_info *db, ir_node *block, ir_node *memop,
                               ir_node *op1, ir_node *op2, ir_mode *mode,
                               op_pin_state state);

/** Constructor for a strictConv node.
 *
 * @param   *db    A pointer for debug information.
 * @param   *block The IR block the node belongs to.
 * @param   *op    The operand.
 * @param   *mode  The mode of this the operand muss be converted .
 */
FIRM_API ir_node *new_rd_strictConv(dbg_info *db, ir_node *block,
                                    ir_node *op, ir_mode *mode);

/** Constructor for a defaultProj node.
 *
 * Represents the default control flow of a Switch-Cond node.
 *
 * @param *db       A pointer for debug information.
 * @param arg       A node producing a tuple.
 * @param max_proj  The end position of the value in the tuple.
 */
FIRM_API ir_node *new_rd_defaultProj(dbg_info *db, ir_node *arg, long max_proj);

/** Constructor for an ASM pseudo node.
 *
 * @param *db         A pointer for debug information.
 * @param *block      The block the node belong to.
 * @param arity       The number of data inputs to the node.
 * @param *in         The array of length arity of data inputs.
 * @param *inputs     The array of length arity of input constraints.
 * @param n_outs      The number of data outputs to the node.
 * @param *outputs    The array of length n_outs of output constraints.
 * @param n_clobber   The number of clobbered registers.
 * @param *clobber    The array of length n_clobber of clobbered registers.
 * @param *asm_text   The assembler text.
 */
FIRM_API ir_node *new_rd_ASM(dbg_info *db, ir_node *block,
                            int arity, ir_node *in[], ir_asm_constraint *inputs,
                            int n_outs, ir_asm_constraint *outputs,
                            int n_clobber, ident *clobber[], ident *asm_text);

/** Constructor for a Gamma node.
 *
 * A value selection node similar to Mux, with lazy evaluation semantics.
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *block      The IR block the node belongs to.
 * @param *cond       The condition used to select the true of false value.
 * @param *ir_false   The false value.
 * @param *ir_true    The true value.
 * @param *mode       The mode of the node, ir_true and ir_false.
 */
FIRM_API ir_node *new_rd_Gamma(dbg_info *db, ir_node *block, ir_node *cond,
                               ir_node *ir_false, ir_node *ir_true,
                               ir_mode *mode);

/** Constructor for a Theta node.
 *
 * Used to define recursive data flow to describe data in loops. Every node
 * that depends on the theta is nested with the thetas nesting depth. Those
 * nodes will act as infinite lists to nodes with a smaller nesting depth. Each
 * nesting level adds one level of indirection. So a theta node with level two
 * will look like an infinite list of lists to the return node.
 *
 * A value can be fetched from those lists by using an extract node, for each
 * level of indirection.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *block      The IR block the node belongs to.
 * @param *init       The initial value of the recursion.
 * @param *next       The recursively defined next value.
 * @param *mode       The mode of the node, init and next.
 * @param depth       The nesting depth of the node.
 */
FIRM_API ir_node *new_rd_Theta(dbg_info *db, ir_node *block, ir_node *init,
                               ir_node *next, ir_mode *mode, int depth);

/** Constructor for a Proxy node.
 *
 * The sole purpose of the proxy node is to serve as a placeholder/proxy for
 * its value. It can be used, when a node is needed to reference a given value.
 *
 * @param *block      The IR block the node belongs to.
 * @param *value      The value to represent.
 * @param *mode       The mode of the proxy.
 */
FIRM_API ir_node *new_rd_Proxy(dbg_info *db, ir_node *block, ir_node *init,
                               ir_mode *mode);

/** Constructor for a Eta node.
 *
 * Extracts a value from a list of values produced by a "nested" node. Given
 * two nodes of the same nesting depth, the according theta nodes are iterated
 * simultaneously until the condition value evaluates to true and the value
 * nodes current value is returned. The eta node decreases the nesting depth
 * by one.
 *
 * In other terms: given an infinite condition list, the first index with value
 * "true" is determined and used to access the value from the value list.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *block      The IR block the node belongs to.
 * @param *value      The "nested" value node.
 * @param *cond       The "nested" condition node.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_rd_Eta(dbg_info *db, ir_node *block, ir_node *value,
                             ir_node *cond, ir_mode *mode);

/** Constructor for an "acyclic" Eta node.
 *
 * A variant of the eta node, used in acyclic PEGs, which are needed inside
 * the PEG to CFG transformation phase. It takes control of a loop by repeated
 * evaluation of its dependencies and by replacing the linked theta values.
 *
 * @param *db         A pointer for debug information.
 * @param *block      The IR block the node belongs to.
 * @param *header     A tuple of nodes to evaluate in the header.
 * @param *repeat     A tuple of values to calculate on repeat.
 * @param *result     The value to calculate on exit.
 * @param *cond       The condition of the loop.
 * @param *force      A tuple of nodes to evaluate before the loop.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_rd_EtaA(dbg_info *db, ir_node *block, ir_node *header,
                              ir_node *repeat, ir_node *result, ir_node *cond,
                              ir_node *force, ir_mode *mode);

/*-------------------------------------------------------------------------*/
/* The raw interface without debug support                                 */
/*-------------------------------------------------------------------------*/

/** Constructor for a Const node.
 *
 * Adds the node to the start block.
 *
 * Constructor for a Const node. The constant represents a target
 * value.  Sets the type information to type_unknown.  (No more
 * supported: If tv is entity derives a somehow useful type.)
 *
 * @param *irg   The IR graph the node  belongs to.
 * @param *mode  The mode of the operands and the results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_r_Const_long(ir_graph *irg, ir_mode *mode, long value);

/** Constructor for a SymConst node.
 *
 *  This is the constructor for a symbolic constant.
 *    There are several kinds of symbolic constants:
 *    - symconst_type_tag   The symbolic constant represents a type tag.  The
 *                          type the tag stands for is given explicitly.
 *    - symconst_type_size  The symbolic constant represents the size of a type.
 *                          The type of which the constant represents the size
 *                          is given explicitly.
 *    - symconst_type_align The symbolic constant represents the alignment of a
 *                          type.  The type of which the constant represents the
 *                          size is given explicitly.
 *    - symconst_addr_ent   The symbolic constant represents the address of an
 *                          entity (variable or method).  The variable is given
 *                          explicitly by a firm entity.
 *    - symconst_ofs_ent    The symbolic constant represents the offset of an
 *                          entity in its owner type.
 *    - symconst_enum_const The symbolic constant is a enumeration constant of
 *                          an enumeration type.
 *
 *    Inputs to the node:
 *      No inputs except the block it belongs to.
 *    Outputs of the node.
 *      An unsigned integer (I_u) or a pointer (P).
 *
 *    Mention union in declaration so that the firmjni generator recognizes that
 *    it can not cast the argument to an int.
 *
 * @param *irg    The IR graph the node  belongs to.
 * @param mode    The mode for the SymConst.
 * @param value   A type, ident, entity or enum constant depending on the
 *                SymConst kind.
 * @param kind    The kind of the symbolic constant, see the list above
 */
FIRM_API ir_node *new_r_SymConst(ir_graph *irg, ir_mode *mode,
                                 union symconst_symbol value,
                                 symconst_kind kind);

/** Constructor for a simpleSel node.
 *
 *  This is a shortcut for the new_d_Sel() constructor.  To be used for
 *  Sel nodes that do not select from an array, i.e., have no index
 *  inputs.  It adds the two parameters 0, NULL.
 *
 * @param *block     The IR block the node belongs to.
 * @param *store     The memory in which the object the entity should be selected
 *                   from is allocated.
 * @param *objptr    The object from that the Sel operation selects a
 *                   single attribute out.
 * @param *ent       The entity to select.
 */
FIRM_API ir_node *new_r_simpleSel(ir_node *block, ir_node *store,
                                  ir_node *objptr, ir_entity *ent);

/** Constructor for a remainderless Div node.
 *
 * @param *block The IR block the node belongs to.
 * @param *memop The store needed to model exceptions
 * @param *op1   The first operand.
 * @param *op2   The second operand.
 * @param *mode  The mode of the result.
 * @param state  The pinned state.
 */
FIRM_API ir_node *new_r_DivRL(ir_node *block, ir_node *memop,
                              ir_node *op1, ir_node *op2, ir_mode *mode,
                              op_pin_state state);
/** Constructor for a strict Conv node.
 *
 * @param *block The IR block the node belongs to.
 * @param *op    The operand.
 * @param *mode  The mode of this the operand muss be converted .
 */
FIRM_API ir_node *new_r_strictConv(ir_node *block, ir_node *op, ir_mode *mode);

/** Constructor for a defaultProj node.
 *
 * Represents the default control flow of a Switch-Cond node.
 *
 * @param arg       A node producing a tuple.
 * @param max_proj  The end  position of the value in the tuple.
 */
FIRM_API ir_node *new_r_defaultProj(ir_node *arg, long max_proj);

/** Constructor for an ASM pseudo node.
 *
 * @param *block      The block the node belong to.
 * @param arity       The number of data inputs to the node.
 * @param *in         The array of length arity of data inputs.
 * @param *inputs     The array of length arity of input constraints.
 * @param n_outs      The number of data outputs to the node.
 * @param *outputs    The array of length n_outs of output constraints.
 * @param n_clobber   The number of clobbered registers.
 * @param *clobber    The array of length n_clobber of clobbered registers.
 * @param *asm_text   The assembler text.
 */
FIRM_API ir_node *new_r_ASM(ir_node *block,
                            int arity, ir_node *in[], ir_asm_constraint *inputs,
                            int n_outs, ir_asm_constraint *outputs,
                            int n_clobber, ident *clobber[], ident *asm_text);

/** Constructor for a Gamma node.
 *
 * A value selection node similar to Mux, with lazy evaluation semantics.
 * One of the basic nodes of the PEG representation.
 *
 * @param *block      The IR block the node belongs to.
 * @param *cond       The condition used to select the true of false value.
 * @param *ir_false   The false value.
 * @param *ir_true    The true value.
 * @param *mode       The mode of the node, ir_true and ir_false.
 */
FIRM_API ir_node *new_r_Gamma(ir_node *block, ir_node *cond, ir_node *ir_false,
                              ir_node *ir_true, ir_mode *mode);

/** Constructor for a Theta node.
 *
 * Used to define recursive data flow to describe data in loops. Every node
 * that depends on the theta is nested with the thetas nesting depth. Those
 * nodes will act as infinite lists to nodes with a smaller nesting depth. Each
 * nesting level adds one level of indirection. So a theta node with level two
 * will look like an infinite list of lists to the return node.
 *
 * A value can be fetched from those lists by using an extract node, for each
 * level of indirection.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *block      The IR block the node belongs to.
 * @param *init       The initial value of the recursion.
 * @param *next       The recursively defined next value.
 * @param *mode       The mode of the node, init and next.
 * @param depth       The nesting depth of the node.
 */
FIRM_API ir_node *new_r_Theta(ir_node *block, ir_node *init, ir_node *next,
                              ir_mode *mode, int depth);

/** Constructor for a Proxy node.
 *
 * The sole purpose of the proxy node is to serve as a placeholder/proxy for
 * its value. It can be used, when a node is needed to reference a given value.
 *
 * @param *block      The IR block the node belongs to.
 * @param *value      The value to represent.
 * @param *mode       The mode of the proxy.
 */
FIRM_API ir_node *new_r_Proxy(ir_node *block, ir_node *init, ir_mode *mode);

/** Constructor for a Eta node.
 *
 * Extracts a value from a list of values produced by a "nested" node. Given
 * two nodes of the same nesting depth, the according theta nodes are iterated
 * simultaneously until the condition value evaluates to true and the value
 * nodes current value is returned. The eta node decreases the nesting depth
 * by one.
 *
 * In other terms: given an infinite condition list, the first index with value
 * "true" is determined and used to access the value from the value list.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *block      The IR block the node belongs to.
 * @param *value      The "nested" value node.
 * @param *cond       The "nested" condition node.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_r_Eta(ir_node *block, ir_node *value, ir_node *cond,
                            ir_mode *mode);

/** Constructor for an "acyclic" Eta node.
 *
 * A variant of the eta node, used in acyclic PEGs, which are needed inside
 * the PEG to CFG transformation phase. It takes control of a loop by repeated
 * evaluation of its dependencies and by replacing the linked theta values.
 *
 * @param *block      The IR block the node belongs to.
 * @param *header     A tuple of nodes to evaluate in the header.
 * @param *repeat     A tuple of values to calculate on repeat.
 * @param *result     The value to calculate on exit.
 * @param *cond       The condition of the loop.
 * @param *force      A tuple of nodes to evaluate before the loop.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_r_EtaA(ir_node *block, ir_node *header, ir_node *repeat,
                             ir_node *result, ir_node *cond, ir_node *force,
                             ir_mode *mode);

/*-----------------------------------------------------------------------*/
/* The block oriented interface                                          */
/*-----------------------------------------------------------------------*/

/** Sets the current block in which the following constructors place the
 *  nodes they construct.
 *
 *  @param target  The new current block.
 */
FIRM_API void set_cur_block(ir_node *target);
FIRM_API void set_r_cur_block(ir_graph *irg, ir_node *target);

/** Returns the current block of the current graph. */
FIRM_API ir_node *get_cur_block(void);
FIRM_API ir_node *get_r_cur_block(ir_graph *irg);

/**
 * @see new_rd_Const_long()
 *
 * @param *db    A pointer for debug information.
 * @param *mode  The mode of the operands and results.
 * @param value  A value from which the tarval is made.
 */
FIRM_API ir_node *new_d_Const_long(dbg_info *db, ir_mode *mode, long value);

/** Constructor for a SymConst node.
 *
 *  This is the constructor for a symbolic constant.
 *    There are several kinds of symbolic constants:
 *    - symconst_type_tag   The symbolic constant represents a type tag.  The
 *                          type the tag stands for is given explicitly.
 *    - symconst_type_size  The symbolic constant represents the size of a type.
 *                          The type of which the constant represents the size
 *                          is given explicitly.
 *    - symconst_type_align The symbolic constant represents the alignment of a
 *                          type.  The type of which the constant represents the
 *                          size is given explicitly.
 *    - symconst_addr_ent   The symbolic constant represents the address of an
 *                          entity (variable or method).  The variable is given
 *                          explicitly by a firm entity.
 *    - symconst_ofs_ent    The symbolic constant represents the offset of an
 *                          entity in its owner type.
 *    - symconst_enum_const The symbolic constant is a enumeration constant of
 *                          an enumeration type.
 *
 *    Inputs to the node:
 *      No inputs except the block it belongs to.
 *    Outputs of the node.
 *      An unsigned integer (I_u) or a pointer (P).
 *
 *    Mention union in declaration so that the firmjni generator recognizes that
 *    it can not cast the argument to an int.
 *
 * @param *db     A pointer for debug information.
 * @param mode    The mode for the SymConst.
 * @param value   A type, ident, entity or enum constant depending on the
 *                SymConst kind.
 * @param kind    The kind of the symbolic constant, see the list above
 */
FIRM_API ir_node *new_d_SymConst(dbg_info *db, ir_mode *mode,
                                 union symconst_symbol value,
                                 symconst_kind kind);

/** Constructor for a simpleSel node.
 *
 *  This is a shortcut for the new_d_Sel() constructor.  To be used for
 *  Sel nodes that do not select from an array, i.e., have no index
 *  inputs.  It adds the two parameters 0, NULL.
 *
 * @param   *db        A pointer for debug information.
 * @param   *store     The memory in which the object the entity should be
 *                     selected from is allocated.
 * @param   *objptr    The object from that the Sel operation selects a
 *                     single attribute out.
 * @param   *ent       The entity to select.
 */
FIRM_API ir_node *new_d_simpleSel(dbg_info *db, ir_node *store, ir_node *objptr,
                                  ir_entity *ent);
/** Constructor for a remainderless Div node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param   *db    A pointer for debug information.
 * @param   *memop The store needed to model exceptions
 * @param   *op1   The first operand.
 * @param   *op2   The second operand.
 * @param   *mode  The mode of the result.
 * @param   state  The pinned state.
 */
FIRM_API ir_node *new_d_DivRL(dbg_info *db, ir_node *memop,
                              ir_node *op1, ir_node *op2, ir_mode *mode,
                              op_pin_state state);
/** Constructor for a strict Conv node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param   *db    A pointer for debug information.
 * @param   *op    The operand.
 * @param   *mode  The mode of this the operand muss be converted .
 */
FIRM_API ir_node *new_d_strictConv(dbg_info *db, ir_node *op, ir_mode *mode);

/** Constructor for a defaultProj node.
 *
 * Represents the default control flow of a Switch-Cond node.
 * Adds the node to the block in current_ir_block.
 *
 * @param *db       A pointer for debug information.
 * @param arg       A node producing a tuple.
 * @param max_proj  The end  position of the value in the tuple.
 */
FIRM_API ir_node *new_d_defaultProj(dbg_info *db, ir_node *arg, long max_proj);

/** Constructor for an ASM pseudo node.
 *
 * @param *db         A pointer for debug information.
 * @param arity       The number of data inputs to the node.
 * @param *in         The array of length arity of data inputs.
 * @param *inputs     The array of length arity of input constraints.
 * @param n_outs      The number of data outputs to the node.
 * @param *outputs    The array of length n_outs of output constraints.
 * @param n_clobber   The number of clobbered registers.
 * @param *clobber    The array of length n_clobber of clobbered registers.
 * @param *asm_text   The assembler text.
 */
FIRM_API ir_node *new_d_ASM(dbg_info *db, int arity, ir_node *in[],
                            ir_asm_constraint *inputs,
                            int n_outs, ir_asm_constraint *outputs,
                            int n_clobber, ident *clobber[], ident *asm_text);

/** Constructor for a Gamma node.
 *
 * A value selection node similar to Mux, with lazy evaluation semantics.
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *cond       The condition used to select the true of false value.
 * @param *ir_false   The false value.
 * @param *ir_true    The true value.
 * @param *mode       The mode of the node, ir_true and ir_false.
 */
FIRM_API ir_node *new_d_Gamma(dbg_info *db, ir_node *cond, ir_node *ir_false,
                              ir_node *ir_true, ir_mode *mode);

/** Constructor for a Theta node.
 *
 * Used to define recursive data flow to describe data in loops. Every node
 * that depends on the theta is nested with the thetas nesting depth. Those
 * nodes will act as infinite lists to nodes with a smaller nesting depth. Each
 * nesting level adds one level of indirection. So a theta node with level two
 * will look like an infinite list of lists to the return node.
 *
 * A value can be fetched from those lists by using an extract node, for each
 * level of indirection.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *init       The initial value of the recursion.
 * @param *next       The recursively defined next value.
 * @param *mode       The mode of the node, init and next.
 * @param depth       The nesting depth of the node.
 */
FIRM_API ir_node *new_d_Theta(dbg_info *db, ir_node *init, ir_node *next,
                              ir_mode *mode, int depth);

/** Constructor for a Proxy node.
 *
 * The sole purpose of the proxy node is to serve as a placeholder/proxy for
 * its value. It can be used, when a node is needed to reference a given value.
 *
 * @param *db         A pointer for debug information.
 * @param *value      The value to represent.
 * @param *mode       The mode of the proxy.
 */
FIRM_API ir_node *new_d_Proxy(dbg_info *db, ir_node *value, ir_mode *mode);

/** Constructor for a Eta node.
 *
 * Extracts a value from a list of values produced by a "nested" node. Given
 * two nodes of the same nesting depth, the according theta nodes are iterated
 * simultaneously until the condition value evaluates to true and the value
 * nodes current value is returned. The eta node decreases the nesting depth
 * by one.
 *
 * In other terms: given an infinite condition list, the first index with value
 * "true" is determined and used to access the value from the value list.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *db         A pointer for debug information.
 * @param *value      The "nested" value node.
 * @param *cond       The "nested" condition node.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_d_Eta(dbg_info *db, ir_node *value, ir_node *cond,
                            ir_mode *mode);

/** Constructor for an "acyclic" Eta node.
 *
 * A variant of the eta node, used in acyclic PEGs, which are needed inside
 * the PEG to CFG transformation phase. It takes control of a loop by repeated
 * evaluation of its dependencies and by replacing the linked theta values.
 *
 * @param *db         A pointer for debug information.
 * @param *header     A tuple of nodes to evaluate in the header.
 * @param *repeat     A tuple of values to calculate on repeat.
 * @param *result     The value to calculate on exit.
 * @param *cond       The condition of the loop.
 * @param *force      A tuple of nodes to evaluate before the loop.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_d_EtaA(dbg_info *db, ir_node *header, ir_node *repeat,
                             ir_node *result, ir_node *cond, ir_node *force,
                             ir_mode *mode);

/*-----------------------------------------------------------------------*/
/* The block oriented interface without debug support                    */
/*-----------------------------------------------------------------------*/

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

/** Constructor for a SymConst node.
 *
 *  This is the constructor for a symbolic constant.
 *    There are several kinds of symbolic constants:
 *    - symconst_type_tag   The symbolic constant represents a type tag.  The
 *                          type the tag stands for is given explicitly.
 *    - symconst_type_size  The symbolic constant represents the size of a type.
 *                          The type of which the constant represents the size
 *                          is given explicitly.
 *    - symconst_type_align The symbolic constant represents the alignment of a
 *                          type.  The type of which the constant represents the
 *                          size is given explicitly.
 *    - symconst_addr_ent   The symbolic constant represents the address of an
 *                          entity (variable or method).  The variable is given
 *                          explicitly by a firm entity.
 *    - symconst_ofs_ent    The symbolic constant represents the offset of an
 *                          entity in its owner type.
 *    - symconst_enum_const The symbolic constant is a enumeration constant of
 *                          an enumeration type.
 *
 *    Inputs to the node:
 *      No inputs except the block it belongs to.
 *    Outputs of the node.
 *      An unsigned integer (I_u) or a pointer (P).
 *
 *    Mention union in declaration so that the firmjni generator recognizes that
 *    it can not cast the argument to an int.
 *
 * @param mode    The mode for the SymConst.
 * @param value   A type, ident, entity or enum constant depending on the
 *                SymConst kind.
 * @param kind    The kind of the symbolic constant, see the list above
 */
FIRM_API ir_node *new_SymConst(ir_mode *mode, union symconst_symbol value,
                               symconst_kind kind);

/** Constructor for a simpelSel node.
 *
 *  This is a shortcut for the new_Sel() constructor.  To be used for
 *  Sel nodes that do not select from an array, i.e., have no index
 *  inputs.  It adds the two parameters 0, NULL.
 *
 * @param   *store     The memory in which the object the entity should be selected from is allocated.
 * @param   *objptr    The object from that the Sel operation selects a single attribute out.
 * @param   *ent       The entity to select.
 */
FIRM_API ir_node *new_simpleSel(ir_node *store, ir_node *objptr,
                                ir_entity *ent);

/** Constructor for a remainderless Div node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param   *memop The store needed to model exceptions
 * @param   *op1   The first operand.
 * @param   *op2   The second operand.
 * @param   *mode  The mode of the result.
 * @param   state  The pinned state.
 */
FIRM_API ir_node *new_DivRL(ir_node *memop, ir_node *op1, ir_node *op2,
                            ir_mode *mode, op_pin_state state);

/** Constructor for a strict Conv node.
 *
 * Adds the node to the block in current_ir_block.
 *
 * @param   *op          The operand.
 * @param   *mode        The mode of this the operand muss be converted.
 */
FIRM_API ir_node *new_strictConv(ir_node *op, ir_mode *mode);

/** Constructor for a defaultProj node.
 *
 * Represents the default control flow of a Switch-Cond node.
 * Adds the node to the block in current_ir_block.
 *
 * @param arg       A node producing a tuple.
 * @param max_proj  The end  position of the value in the tuple.
 */
FIRM_API ir_node *new_defaultProj(ir_node *arg, long max_proj);

/** Constructor for an ASM pseudo node.
 *
 * @param arity       The number of data inputs to the node.
 * @param *in         The array of length arity of data inputs.
 * @param *inputs     The array of length arity of input constraints.
 * @param n_outs      The number of data outputs to the node.
 * @param *outputs    The array of length n_outs of output constraints.
 * @param n_clobber   The number of clobbered registers.
 * @param *clobber    The array of length n_clobber of clobbered registers.
 * @param *asm_text   The assembler text.
 */
FIRM_API ir_node *new_ASM(int arity, ir_node *in[], ir_asm_constraint *inputs,
                          int n_outs, ir_asm_constraint *outputs,
                          int n_clobber, ident *clobber[], ident *asm_text);

/** Constructor for a Gamma node.
 *
 * A value selection node similar to Mux, with lazy evaluation semantics.
 * One of the basic nodes of the PEG representation.
 *
 * @param *cond       The condition used to select the true of false value.
 * @param *ir_false   The false value.
 * @param *ir_true    The true value.
 * @param *mode       The mode of the node, ir_true and ir_false.
 */
FIRM_API ir_node *new_Gamma(ir_node *cond, ir_node *ir_false, ir_node *ir_true,
                            ir_mode *mode);

/** Constructor for a Theta node.
 *
 * Used to define recursive data flow to describe data in loops. Every node
 * that depends on the theta is nested with the thetas nesting depth. Those
 * nodes will act as infinite lists to nodes with a smaller nesting depth. Each
 * nesting level adds one level of indirection. So a theta node with level two
 * will look like an infinite list of lists to the return node.
 *
 * A value can be fetched from those lists by using an extract node, for each
 * level of indirection.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *init       The initial value of the recursion.
 * @param *next       The recursively defined next value.
 * @param *mode       The mode of the node, init and next.
 * @param depth       The nesting depth of the node.
 */
FIRM_API ir_node *new_Theta(ir_node *init, ir_node *next, ir_mode *mode,
                            int depth);

/** Constructor for a Proxy node.
 *
 * The sole purpose of the proxy node is to serve as a placeholder/proxy for
 * its value. It can be used, when a node is needed to reference a given value.
 *
 * @param *value      The value to represent.
 * @param *mode       The mode of the proxy.
 */
FIRM_API ir_node *new_Proxy(ir_node *init, ir_mode *mode);

/** Constructor for a Eta node.
 *
 * Extracts a value from a list of values produced by a "nested" node. Given
 * two nodes of the same nesting depth, the according theta nodes are iterated
 * simultaneously until the condition value evaluates to true and the value
 * nodes current value is returned. The eta node decreases the nesting depth
 * by one.
 *
 * In other terms: given an infinite condition list, the first index with value
 * "true" is determined and used to access the value from the value list.
 *
 * One of the basic nodes of the PEG representation.
 *
 * @param *value      The "nested" value node.
 * @param *cond       The "nested" condition node.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_Eta(ir_node *value, ir_node *cond, ir_mode *mode);

/** Constructor for an "acyclic" Eta node.
 *
 * A variant of the eta node, used in acyclic PEGs, which are needed inside
 * the PEG to CFG transformation phase. It takes control of a loop by repeated
 * evaluation of its dependencies and by replacing the linked theta values.
 *
 * @param *header     A tuple of nodes to evaluate in the header.
 * @param *repeat     A tuple of values to calculate on repeat.
 * @param *result     The value to calculate on exit.
 * @param *cond       The condition of the loop.
 * @param *force      A tuple of nodes to evaluate before the loop.
 * @param *mode       The mode of the node and value.
 */
FIRM_API ir_node *new_EtaA(ir_node *header, ir_node *repeat, ir_node *result,
                           ir_node *cond, ir_node *force, ir_mode *mode);

/*---------------------------------------------------------------------*/
/* The comfortable interface.                                          */
/* Supports automatic Phi node construction.                           */
/* All routines of the block oriented interface except new_Block are   */
/* needed also.                                                        */
/*---------------------------------------------------------------------*/

/** Create an immature Block.
 *
 * An immature Block has an unknown number of predecessors.  Predecessors
 * can be added with add_immBlock_pred().  Once all predecessors are
 * added the block must be matured.
 *
 * Adds the block to the graph in current_ir_graph. Can be used with automatic
 * Phi node construction.
 * This constructor can only be used if the graph is in state_building.
 */
FIRM_API ir_node *new_d_immBlock(dbg_info *db);
FIRM_API ir_node *new_immBlock(void);
FIRM_API ir_node *new_r_immBlock(ir_graph *irg);
FIRM_API ir_node *new_rd_immBlock(dbg_info *db, ir_graph *irg);

/** Add a control flow edge to an immature block. */
FIRM_API void add_immBlock_pred(ir_node *immblock, ir_node *jmp);

/** Finalize a Block node, when all control flows are known. */
FIRM_API void mature_immBlock(ir_node *block);

/** Get the current value of a local variable.
 *
 * Use this function to obtain the last definition of the local variable
 * associated with pos.  Pos may not exceed the value passed as n_loc
 * to new_ir_graph.  This call automatically inserts Phi nodes.
 *
 * @param  pos   The position/id of the local variable.
 * @param *mode  The mode of the value to get.
 */
FIRM_API ir_node *get_value(int pos, ir_mode *mode);
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
FIRM_API ir_mode *ir_r_guess_mode(ir_graph *irg, int pos);

/** Remark a new definition of a variable.
 *
 * Use this function to remember a new definition of the value
 * associated with pos. Pos may not exceed the value passed as n_loc
 * to new_ir_graph.  This call is needed to automatically inserts Phi
 * nodes.
 *
 * @param  pos   The position/id of the local variable.
 * @param *value The new value written to the local variable.
 */
FIRM_API void set_value(int pos, ir_node *value);
FIRM_API void set_r_value(ir_graph *irg, int pos, ir_node *value);

/**
 * Find the value number for a node in the current block.
 *
 * @param value  the searched value
 *
 * @return the value number of the value or -1 if this value has
 * no value number in the current block.
 */
FIRM_API int find_value(ir_node *value);
FIRM_API int r_find_value(ir_graph *irg, ir_node *value);

/** Get the current memory state.
 *
 * Use this function to obtain the last definition of the memory
 * state.  This call automatically inserts Phi nodes for the memory
 * state value.
 */
FIRM_API ir_node *get_store(void);
FIRM_API ir_node *get_r_store(ir_graph *irg);

/** Remark a new definition of the memory state.
 *
 * Use this function to remember a new definition of the memory state.
 * This call is needed to automatically inserts Phi nodes.
 *
 * @param *store The new memory state.
 */
FIRM_API void set_store(ir_node *store);
FIRM_API void set_r_store(ir_graph *irg, ir_node *store);

/** keep this node alive even if End is not control-reachable from it
 *
 * @param ka The node to keep alive.
 */
FIRM_API void keep_alive(ir_node *ka);

/* --- initialize and finalize IR construction --- */

/** Puts the graph into state "phase_high" */
FIRM_API void irg_finalize_cons(ir_graph *irg);

/** Puts the program and all graphs into state phase_high.
 *
 * This also remarks, the construction of types is finished,
 * e.g., that no more subtypes will be added.  */
FIRM_API void irp_finalize_cons(void);

FIRM_API void ir_set_uninitialized_local_variable_func(
		uninitialized_local_variable_func_t *func);

#include "end.h"

#endif
