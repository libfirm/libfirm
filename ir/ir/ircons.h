/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer,
**          Goetz Lindenmaier
**
** ircons.h ir node construction
*/

/* $Id$ */

/** !!!
*** Ideas for imrovement:
***
 Handle construction of exceptions more comfortable:
 Add new constructors that pass the exception region (or better the
 Phi for the memories, the ex. region can be found from there) as parameter,
 constructor then adds all Proj nodes and returns the pointer
 to the Proj node that selects the result of the arithmetic operation.

 Maybe hide the exception region in a global variable, especially if
 it is always unambiguous.
**/

/****h* libfirm/ircons
 *
 * NAME
 *   file ircons.h
 *
 * NOTES
 *    This file documents all datatypes and constructors needed to
 *    build a FIRM representation of a pocedure.  The constructors are
 *    also implemented in this file.
 *
 *    The documentation also gives a short manual how to use the library.
 *
 *    For extensive documentation of FIRM see UKA Techreport 1999-14.
 *
 *    DATATYPES
 *    =========
 *
 *    The struct ir_graph
 *    -------------------
 *
 *      This struct contains all information about a procedure.
 *      It's allocated directly to memory.
 *
 *      The fields of ir_graph:
 *
 *      *ent             The entity describing this procedure.
 *
 *      The beginning and end of a graph:
 *
 *      *start_block     This ir_node is the block that contains the unique
 *                       start node of the procedure.  With it it contains
 *                       the Proj's on starts results.
 *                       Further all Const nodes are placed in the start block.
 *      *start           This ir_node is the unique start node of the procedure.
 *
 *      *end_block       This ir_node is the block that contains the unique
 *                       end node of the procedure.  This block contains no
 *                       further nodes.
 *      *end             This ir_node is the unique end node of the procedure.
 *
 *      The following nodes are Projs from the start node, held in ir_graph for
 *      simple access:
 *
 *      *frame           The ir_node producing the pointer to the stack frame of
 *                       the procedure as output.  This is the Proj node on the
 *                       third output of the start node.  This output of the start
 *                       node is tagged as pns_frame_base.  In FIRM most lokal
 *                       variables are modeled as data flow edges.  Static
 *                       allocated arrays can not be represented as dataflow
 *                       edges. Therefore FIRM has to represent them in the stack
 *                       frame.
 *
 *      *globals	     This models a pointer to a space in the memory where
 *  		     _all_ global things are held.  Select from this pointer
 *  		     with a Sel node the pointer to a global variable /
 *  		     procedure / compiler known function... .
 *
 *      *args	     The ir_node that produces the arguments of the method as
 *  		     it's result.  This is a Proj node on the fourth output of
 *  		     the start node.  This output is tagged as pns_args.
 *
 *      *bad             The bad node is an auxiliary node. It is needed only once,
 *                       so there is this globally reachable node.
 *
 *      Datastructures that are private to a graph:
 *
 *      *obst            An obstack that contains all nodes.
 *
 *      *current_block   A pointer to the current block.  Any node created with
 *                       one of the node constructors (new_<opcode>) are assigned
 *                       to this block.  It can be set with switch_block(block).
 *                       Only needed for ir construction.
 *
 *      params/n_loc     An int giving the number of local variables in this
 *  		     procedure.  This is neede for ir construction. Name will
 *  		     be changed.
 *
 *      *value_table     This hash table (pset) is used for global value numbering
 *  		     for optimizing use in iropt.c.
 *
 *      *Phi_in_stack;   a stack needed for automatic Phi construction, needed only
 *  		     during ir construction.
 *
 *      visited          A int used as flag to traverse the ir_graph.
 *
 *      block_visited    A int used as a flag to traverse block nodes in the graph.
 *
 *    Three kinds of nodes
 *    --------------------
 *
 *      There are three kinds of nodes known to the ir:  entities,
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
 *        nodes.
 *
 *    Implementation of the FIRM operations: ir_node
 *    ----------------------------------------------
 *
 *      Ir_nodes represent operations on the data of the program and control flow
 *      operations.  Examples of ir_nodes:  Add, Jmp, Cmp
 *
 *      FIRM is a dataflow graph.  A dataflow graph is a directed graph,
 *      so that every node has incoming and outgoing edges.  A node is
 *      executable if every input at it's incoming edges is available.
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
 *      contains pointers to it's predecessor so that the implementation is a
 *      dataflow graph with reversed edges.  It has to be traversed bottom
 *      up.
 *
 *      All nodes of the ir have the same basic structure.  They are
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
 *                       node.  The mode of the operation is the mode of it's
 *                       result.  A Firm mode is a datatype as known to the target,
 *  		     not a type of the source language.
 *
 *      visit            A flag for traversing the ir.
 *
 *      **in             An array with pointers to the node's predecessors.
 *
 *      *link            A pointer to an ir_node.  With this pointer all Phi nodes
 *                       are attached to a Block, i.e., a Block points to it's
 *                       first Phi node, this node points to the second Phi node
 *                       in the Block and so fourth.  Used in mature_block
 *                       to find all Phi nodes to be matured.  It's also used to
 *  		     annotate a node with a better, optimized version of it.
 *
 *      attr             An attr struct containing the attributes of the nodes. The
 *                       attributes depend on the opcode of the node.  The number
 *  		     of these attributes is given in op.
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
 *    * A "comfortable" interface generating SSA automatically.  Automatically
 *      computed predecessors of nodes need not be specified in the constructors.
 *      (new_<Node> constructurs and a set of additional routines.)
 *    * A less comfortable interface where all predecessors except the block
 *      an operation belongs to need to be specified.  SSA must be constructed
 *      by hand.  (new_<Node> constructors and switch_block()).  This interface
 *      is called "block oriented".  It automatically calles the local optimizations
 *      for each new node.
 *    * An even less comfortable interface where the block needs to be specified
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
 *    constructor gets the number of local variables.  The graph is hold in the
 *    global variable irg.
 *
 *    Now the construction of the procedure can start.  Several basic blocks can
 *    be constructed in parallel, but the code within each block needs to
 *    be constructed (almost) in program order.
 *
 *    A global variable holds the current basic block.  All (non block) nodes
 *    generated are added to this block.  The current block can be set with
 *    switch_block(block).  If several blocks are constructed in parallel block
 *    switches need to be performed constantly.
 *
 *    To generate a Block node (with the comfortable interface) it's predecessor
 *    control flow nodes need not be known.  In case of cyclic control flow these
 *    can not be known when the block is constructed.  With add_in_edge(block,
 *    cfnode) predecessors can be added to the block.  If all predecessors are
 *    added to the block mature_block(b) needs to be called.  Calling mature_block
 *    early improves the efficiency of the Phi node construction algorithm.
 *    But if several  blocks are constructed at once, mature_block must only
 *    be called after performing all set_values and set_stores in the block!
 *    (See documentation of new_immBlock constructor.)
 *
 *    The constructors of arithmetic nodes require that their predecessors
 *    are mentioned.  Sometimes these are available in the Frontend as the
 *    predecessors have just been generated by the frontend.  If they are local
 *    values the predecessors can be obtained from the library with a call to
 *    get_value(local_val_nr).  (local_val_nr needs to be administered by
 *    the Frontend.)  A call to get_value triggers the generation of Phi nodes.
 *    If an arithmetic operation produces a local value this value needs to be
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
 *    add_in_edge(this_block, cf_pred1);
 *    add_in_edge(this_block, cf_pred2);
 *    mature_block(this_block);
 *    a_val = get_value(42, mode_I);
 *    mem = get_store();
 *    div = new_Div(mem, a_val, a_val);
 *    mem = new_Proj(div, mode_M, 0);   * for the numbers for Proj see docu *
 *    res = new_Proj(div, mode_I, 2);
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
 *    ir_node *new_immBlock  (void);
 *    ir_node *new_Start  (void);
 *    ir_node *new_End    (void);
 *    ir_node *new_Jmp    (void);
 *    ir_node *new_Cond   (ir_node *c);
 *    ir_node *new_Return (ir_node *store, int arity, ir_node **in);
 *    ir_node *new_Raise  (ir_node *store, ir_node *obj);
 *    ir_node *new_Const  (ir_mode *mode, tarval *con);
 *    ir_node *new_SymConst (type_or_id *value, symconst_kind kind);
 *    ir_node *new_simpleSel (ir_node *store, ir_node *objptr, entity *ent);
 *    ir_node *new_Sel    (ir_node *store, ir_node *objptr, int arity,
 *                         ir_node **in, entity *ent);
 *    ir_node *new_Call   (ir_node *store, ir_node *callee, int arity,
 *  		       ir_node **in, type_method *type);
 *    ir_node *new_Add    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Sub    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Minus  (ir_node *op,  ir_mode *mode);
 *    ir_node *new_Mul    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Quot   (ir_node *memop, ir_node *op1, ir_node *op2);
 *    ir_node *new_DivMod (ir_node *memop, ir_node *op1, ir_node *op2);
 *    ir_node *new_Div    (ir_node *memop, ir_node *op1, ir_node *op2);
 *    ir_node *new_Mod    (ir_node *memop, ir_node *op1, ir_node *op2);
 *    ir_node *new_Abs    (ir_node *op,                ir_mode *mode);
 *    ir_node *new_And    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Or     (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Eor    (ir_node *op1, ir_node *op2, ir_mode *mode);
 *    ir_node *new_Not    (ir_node *op,                ir_mode *mode);
 *    ir_node *new_Shl    (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Shr    (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Shrs   (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Rot    (ir_node *op,  ir_node *k,   ir_mode *mode);
 *    ir_node *new_Cmp    (ir_node *op1, ir_node *op2);
 *    ir_node *new_Conv   (ir_node *op, ir_mode *mode);
 *    ir_node *new_Load   (ir_node *store, ir_node *addr);
 *    ir_node *new_Store  (ir_node *store, ir_node *addr, ir_node *val);
 *    ir_node *new_Alloc  (ir_node *store, ir_node *size, type *alloc_type,
 *                         where_alloc where);
 *    ir_node *new_Free   (ir_node *store, ir_node *ptr, ir_node *size,
 *    		     type *free_type);
 *    ir_node *new_Proj   (ir_node *arg, ir_mode *mode, long proj);
 *
 *    void add_in_edge (ir_node *block, ir_node *jmp);
 *    void     mature_block (ir_node *block);
 *    void switch_block (ir_node *target);
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
 *    The constructor for the block node sets current_block to itself.
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
 *    BASIC BLOCKS
 *    ------------
 *
 *    ir_node *new_immBlock (void)
 *    ----------------------------
 *
 *    Creates a new block.  Sets current_block to itself.  When a new block is
 *    created it cannot be known how many predecessors this block will have in the
 *    control flow graph. Therefore the list of inputs can not be fixed at
 *    creation.  Predecessors can be added with add_in_edge (block, control flow
 *    operation).  With every added predecessor the number of inputs to Phi nodes
 *    also changes.
 *
 *    The block can be completed by mature_block(block) if all predecessors are
 *    known.  If several blocks are built at once, mature_block can only be called
 *    after set_value has been called for all values that are life at the end
 *    of the block.  This is necessary so that Phi nodes created by mature_block
 *    get the right predecessors in case of cyclic dependencies.  If all set_values
 *    of this block are called after maturing it and before calling get_value
 *    in some block that is control flow dependent on this block, the construction
 *    is correct.
 *
 *    Example for faulty ir construction:  (draw the graph on a paper and you'll
 *                                          get it ;-)
 *
 *      block_before_loop = new_block();
 *      set_value(x);
 *      mature_block(block_before_loop);
 *      before2header = new_Jmp;
 *
 *      loop_header = new_block ();
 *      header2body - new_Jmp();
 *
 *      loop_body = new_block ();
 *      body2header = new_Jmp();
 *
 *      add_in_edge(loop_header, before2header);
 *      add_in_edge(loop_header, body2header);
 *      add_in_edge(loop_body, header2body);
 *
 *      mature_block(loop_header);
 *      mature_block(loop_body);
 *
 *      get_value(loop_body, x);   // gets the Phi in loop_header
 *      set_value(loop_header, x); // sets the value the above get_value should
 *                                 // have returned!!!
 *
 *    Mature_block also fixes the number of inputs to the Phi nodes.  Mature_block
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
 *  		    internal construction algorithm and should not be accessed
 *  		    from outside.
 *
 *
 *    ir_node *new_Block (int arity, ir_node **in)
 *    --------------------------------------------
 *
 *    Creates a new Block with the given list of predecessors.  This block
 *    is mature.
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
 *    node in each procedure which is automatically created by new_ir_graph.
 *
 *    Inputs:
 *      No inputs except the block it belogns to.
 *    Output:
 *      A tuple of 4 (5, 6) distinct values. These are labeled by the following
 *      projection numbers (pns_number):
 *      * pns_initial_exec
 *                       mode X, points to the first block to be executed.
 *      * pns_global_store
 *                       mode M, the global store
 *      * pns_frame_base mode P, a pointer to the base of the procedures
 *   		             stack frame.
 *      * pns_globals    mode P, a pointer to the part of the memory containing
 *                               _all_ global things.
 *      * pns_args       mode T, a tuple containing all arguments of the procedure.
 *
 *
 *    ir_node *new_End (void)
 *    -----------------------
 *
 *    Creates an end node.  Not actually needed public.  There is only one such
 *    node in each procedure which is automatically created by new_ir_graph.
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
 *      flow will procede along output i. If the input is >= n control
 *      flow proceeds along output n.
 *
 *    ir_node *new_Return (in_node *store, int arity, ir_node **in)
 *    -------------------------------------------------------------
 *
 *    The return node has as inputs the results of the procedure.  It
 *    passes the control flow to the end_block.
 *
 *    Inputs:
 *      The memory state.
 *      All results.
 *    Output
 *      Control flow to the end block.
 *
 *    ir_node *new_Raise (ir_node *store, ir_node *obj)
 *    -------------------------------------------------
 *
 *    Raises an exception.  Unconditional change of control flow.  Writes
 *    an explicit Except variable to memory to pass it to the exception
 *    handler.  See TechReport 1999-14, chapter Exceptions.
 *
 *    Inputs:
 *      The memory state.
 *      A pointer to the Except variable.
 *    Output:
 *      A tuple of control flow and the changed memory state.  The control flow
 *      points to the exception handler if it is definied in this procedure,
 *      else it points to the end_block.
 *
 *
 *    CONSTANTS
 *    ---------
 *
 *    ir_node *new_Const (ir_mode *mode, tarval *con)
 *    -----------------------------------------------
 *
 *    Creates a constant in the constant table and adds a Const node
 *    returning this value to the start block.
 *
 *    Parameters:
 *      *mode            The mode of the constant.
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
 *    ir_node *new_SymConst (type *type, symconst_kind kind)
 *    ------------------------------------------------------------
 *
 *    There are three kinds of symbolic constants:
 *      type_tag  The symbolic constant represents a type tag.
 *      size      The symbolic constant represents the size of a class.
 *      link_info Information for the linker, e.g. the name of a global
 *                variable.
 *
 *    Parameters
 *      kind        The kind of the symbolic constant: type_tag, size or link_info.
 *      *type_or_id Points to the type the tag stands for or to the type
 *                  whose size is represented by the constant or to an ident
 *                  representing the linkage info.
 *
 *    Inputs:
 *      No inputs except the block it belogns to.
 *    Output:
 *      An unsigned integer (I_u) or a pointer (P).
 *
 *    Attributes:
 *      attr.i.num       The symconst_kind, i.e. one of
 *                        - type_tag
 *                        - size
 *  		      - linkage_ptr_info
 *        If the attr.i.num is type_tag or size, the node contains an attribute
 *      attr.i.*type,    a pointer to a type_class.  The mode of the node is mode_i.
 *        if it is linkage_ptr_info it contains
 *      attr.i.*ptrinfo,  an ident holding information for the linker.  The mode
 *        of the node is mode_p.
 *
 *    THE SELECT NODE
 *    ---------------
 *
 *    ir_node *new_simpleSel (ir_node *store, ir_node *frame, entity *sel)
 *    --------------------------------------------------------------------
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
 *                      entity *sel)
 *                      ------------
 *
 *    Selects a field from an array type.  The entity has as owner the array, as
 *    type the arrays element type.  The indexes to access an array element are
 *    given also.
 *
 *    Parameters:
 *      *store     The memory in which the object the entity should be selected from
 *                 is allocated.
 *      *frame     The pointer to the object.
 *      *arity     number of array indexes.
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
 *    The constructors new_Sel and new_simpleSel generate the same ir nodes.
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
 *      attr.call        Contains the type information for the procedure.
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
 *    Unary Minus operations on floating point values.
 *
 *    ir_node *new_Mul (ir_node *op1, ir_node *op2, ir_mode *mode)
 *    ------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Quot (ir_node *memop, ir_node *op1, ir_node *op2)
 *    --------------------------------------------------------------
 *
 *    Quot performs exact division of floating point numbers.  It's mode
 *    is Tuple, the mode of the result must be annotated to the Proj
 *    that extracts the result of the arithmetic operations.
 *
 *    Inputs:
 *      The store needed to model exceptions and the two operands.
 *    Output:
 *      A tuple contaning a memory and a execution for modeling exceptions
 *      and the result of the arithmetic operation.
 *
 *    ir_node *new_DivMod (ir_node *memop, ir_node *op1, ir_node *op2)
 *    ----------------------------------------------------------------
 *
 *    Performs Div and Mod on interger values.
 *
 *    Output:
 *      A tuple contaning a memory and a execution for modeling exceptions
 *      and the two result of the arithmetic operations.
 *
 *    ir_node *new_Div (ir_node *memop, ir_node *op1, ir_node *op2)
 *    -------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Mod (ir_node *memop, ir_node *op1, ir_node *op2)
 *    -------------------------------------------------------------
 *
 *    Trivial.
 *
 *    ir_node *new_Abs (ir_node *op, ir_mode *mode)
 *    ---------------------------------------------
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
 *    ir_node *new_Rot (ir_node *op, ir_node *k, ir_mode *mode)
 *    ---------------------------------------------------------
 *
 *    Rotates the operand to the (right??) by k bits.
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
 *      number (pnc_number) to use in Proj nodes to extract the proper result.
 *        False     false
 *        Eq        equal
 *        Lt	less
 *        Le	less or equal
 *        Gt	greater
 *        Ge	greater of equal
 *        Lg	less or greater
 *        Leg	less, equal or greater = ordered
 *        Uo	unordered
 *        Ue	unordered or equal
 *        Ul	unordered or less
 *        Ule	unordered, less or equal
 *        Ug	unordered or greater
 *        Uge	unordered, greater or equal
 *        Ne	unordered, less or greater = not equal
 *        True	true
 *
 *
 *
 *    THE PHI NODE
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
 *
 *    Parameter
 *      arity            number of predecessors
 *      **in             array with predecessors
 *      *mode            The mode of it's inputs and output.
 *    Inputs:
 *      A Phi node has as many inputs as the block it belongs to.
 *      Each input points to a definition of the same value on a
 *      different path in the control flow.
 *    Output
 *      The definition valid in this block.
 *
 *
 *    OPERATIONS TO MANAGE MEMORY EXPLICITLY
 *    --------------------------------------
 *
 *    ir_node *new_Load (ir_node *store, ir_node *addr)
 *    ----------------------------------------------------------------
 *
 *    The Load operation reads a value from memory.
 *
 *    Parameters:
 *    *store        The current memory.
 *    *addr         A pointer to the variable to be read in this memory.
 *
 *    Inputs:
 *      The memory and a pointer to a variable in this memory.
 *    Output:
 *      A tuple of the memory, a control flow to be taken in case of
 *      an exception and the loaded value.
 *
 *    ir_node *new_Store (ir_node *store, ir_node *addr, ir_node *val)
 *    ----------------------------------------------------------------
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
 *    ir_node *new_Alloc (ir_node *store, ir_node *size, type *alloc_type,
 *    --------------------------------------------------------------------
 *                        where_alloc where)
 *                        ------------------
 *
 *    The Alloc node allocates a new variable.  It can be specified whether the
 *    variable should be allocated to the stack or to the heap.
 *
 *    Parameters:
 *      *store       The memory which shall contain the new variable.
 *      **    *size        The number of bytes to allocate. Old. **
 *      *size        We decided that the size easily can be derived from the type.
 *                   This field is for allocating arrays, i.e., it gives the multiple
 *  		 of the size of alloc_type to allocate memory for.
 *      *alloc_type  The type of the allocated variable.
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
 *    ir_node *new_Free (ir_node *store, ir_node *ptr, type *free_type)
 *    ------------------------------------------------------------------
 *
 *    The Free node frees memory of the given variable.
 *
 *    Parameters:
 *      *store       The memory which shall contain the new variable.
 *      *ptr         The pointer to the object to free.
 *      *size        The number of objects of type free_type to free in a sequence.
 *      *free_type   The type of the freed variable.
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
 *      arity    The number of memories to syncronize.
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
 *    ir_node *new_Bad (void)
 *    -----------------------
 *
 *    Returns the unique Bad node current_ir_graph->bad.
 *    This node is used to express results of dead code elimination.
 *
 *    ir_node *new_Proj (ir_node *arg, ir_mode *mode, long proj)
 *    ----------------------------------------------------------
 *
 *    Selects one entry of a tuple.  This is a hidden `fat edge'.
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
 *    node, so that a node can be changed into a Tuple by just changing it's
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
 *    The single output of the Id operation is it's input.  Also needed
 *    for optimizations.
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
 *    The following two funcions can be used to add a newly computed value
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
 *    turned on right after it's creation.
 *    Requires current_block to be set correctly.
 *
 *    There are two special routines for the global store:
 *
 *    inline void set_store (ir_node *store)
 *    --------------------------------------
 *
 *    Adds the store to the array of known values at a reserved
 *    position.
 *    Requires current_block to be set correctly.
 *
 *    inline ir_node *get_store (void)
 *    --------------------------------
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
 *    node.
 *
 *****
 */


# ifndef _IRCONS_H_
# define _IRCONS_H_

# include "common.h"
# include "irgraph.h"
# include "irnode.h"
# include "irmode.h"
# include "entity.h"
# include "tv.h"
# include "type.h"

/***************************************************************************/
/* The raw interface                                                       */
/***************************************************************************/

/* Constructs a Block with a fixed number of predecessors.
   Does not set current_block.  Can not be used with automatic
   Phi node construction. */
ir_node *new_r_Block  (ir_graph *irg,  int arity, ir_node **in);
ir_node *new_r_Start  (ir_graph *irg, ir_node *block);
ir_node *new_r_End    (ir_graph *irg, ir_node *block);
ir_node *new_r_Jmp    (ir_graph *irg, ir_node *block);
ir_node *new_r_Cond   (ir_graph *irg, ir_node *block, ir_node *c);
ir_node *new_r_Return (ir_graph *irg, ir_node *block,
		       ir_node *store, int arity, ir_node **in);
ir_node *new_r_Raise  (ir_graph *irg, ir_node *block,
		       ir_node *store, ir_node *obj);
ir_node *new_r_Const  (ir_graph *irg, ir_node *block,
		       ir_mode *mode, tarval *con);
ir_node *new_r_SymConst (ir_graph *irg, ir_node *block,
                       type_or_id_p value, symconst_kind symkind);
ir_node *new_r_Sel    (ir_graph *irg, ir_node *block, ir_node *store,
                       ir_node *objptr, int n_index, ir_node **index,
		       entity *ent);
ir_node *new_r_Call   (ir_graph *irg, ir_node *block, ir_node *store,
		       ir_node *callee, int arity, ir_node **in,
		       type *type);
ir_node *new_r_Add    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Sub    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Minus  (ir_graph *irg, ir_node *block,
		       ir_node *op,  ir_mode *mode);
ir_node *new_r_Mul    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Quot   (ir_graph *irg, ir_node *block,
		       ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_r_DivMod (ir_graph *irg, ir_node *block,
		       ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_r_Div    (ir_graph *irg, ir_node *block,
		       ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_r_Mod    (ir_graph *irg, ir_node *block,
		       ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_r_Abs    (ir_graph *irg, ir_node *block,
                       ir_node *op, ir_mode *mode);
ir_node *new_r_And    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Or     (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Eor    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_r_Not    (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_mode *mode);
ir_node *new_r_Cmp    (ir_graph *irg, ir_node *block,
		       ir_node *op1, ir_node *op2);
ir_node *new_r_Shl    (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_node *k, ir_mode *mode);
ir_node *new_r_Shr    (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_node *k, ir_mode *mode);
ir_node *new_r_Shrs   (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_node *k, ir_mode *mode);
ir_node *new_r_Rot    (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_node *k, ir_mode *mode);
ir_node *new_r_Conv   (ir_graph *irg, ir_node *block,
		       ir_node *op, ir_mode *mode);
ir_node *new_r_Phi    (ir_graph *irg, ir_node *block, int arity,
		       ir_node **in, ir_mode *mode);
ir_node *new_r_Load   (ir_graph *irg, ir_node *block,
		       ir_node *store, ir_node *adr);
ir_node *new_r_Store  (ir_graph *irg, ir_node *block,
		       ir_node *store, ir_node *adr, ir_node *val);
ir_node *new_r_Alloc  (ir_graph *irg, ir_node *block, ir_node *store,
		       ir_node *size, type *alloc_type, where_alloc where);
ir_node *new_r_Free   (ir_graph *irg, ir_node *block, ir_node *store,
		       ir_node *ptr, ir_node *size, type *free_type);
ir_node *new_r_Sync   (ir_graph *irg, ir_node *block, int arity, ir_node **in);
ir_node *new_r_Proj   (ir_graph *irg, ir_node *block, ir_node *arg,
		       ir_mode *mode, long proj);
ir_node *new_r_defaultProj (ir_graph *irg, ir_node *block, ir_node *arg,
			    long max_proj);
ir_node *new_r_Tuple  (ir_graph *irg, ir_node *block,
		       int arity, ir_node **in);
ir_node *new_r_Id     (ir_graph *irg, ir_node *block,
		       ir_node *val, ir_mode *mode);
ir_node *new_r_Bad    ();


/*************************************************************************/
/* The block oriented interface                                          */
/*************************************************************************/

/* Sets the current block in which the following constructors place the
   nodes they construct. */
void switch_block (ir_node *target);

/* Constructs a Block with a fixed number of predecessors.
   Does set current_block.  Can be used with automatic Phi
   node construction. */
ir_node *new_Block(int arity, ir_node **in);
ir_node *new_Start  (void);
ir_node *new_End    (void);
ir_node *new_Jmp    (void);
ir_node *new_Cond   (ir_node *c);
ir_node *new_Return (ir_node *store, int arity, ir_node **in);
ir_node *new_Raise  (ir_node *store, ir_node *obj);
ir_node *new_Const  (ir_mode *mode, tarval *con);
ir_node *new_SymConst (type_or_id_p value, symconst_kind kind);
ir_node *new_simpleSel(ir_node *store, ir_node *objptr, entity *ent);
ir_node *new_Sel    (ir_node *store, ir_node *objptr, int arity, ir_node **in,
                     entity *ent);
ir_node *new_Call   (ir_node *store, ir_node *callee, int arity, ir_node **in,
		     type *type);
ir_node *new_Add    (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Sub    (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Minus  (ir_node *op,  ir_mode *mode);
ir_node *new_Mul    (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Quot   (ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_DivMod (ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_Div    (ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_Mod    (ir_node *memop, ir_node *op1, ir_node *op2);
ir_node *new_Abs    (ir_node *op,                ir_mode *mode);
ir_node *new_And    (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Or     (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Eor    (ir_node *op1, ir_node *op2, ir_mode *mode);
ir_node *new_Not    (ir_node *op,                ir_mode *mode);
ir_node *new_Shl    (ir_node *op,  ir_node *k,   ir_mode *mode);
ir_node *new_Shr    (ir_node *op,  ir_node *k,   ir_mode *mode);
ir_node *new_Shrs   (ir_node *op,  ir_node *k,   ir_mode *mode);
ir_node *new_Rot    (ir_node *op,  ir_node *k,   ir_mode *mode);
ir_node *new_Cmp    (ir_node *op1, ir_node *op2);
ir_node *new_Conv   (ir_node *op, ir_mode *mode);
ir_node *new_Phi    (int arity, ir_node **in, ir_mode *mode);
ir_node *new_Load   (ir_node *store, ir_node *addr);
ir_node *new_Store  (ir_node *store, ir_node *addr, ir_node *val);
ir_node *new_Alloc  (ir_node *store, ir_node *size, type *alloc_type,
                     where_alloc where);
ir_node *new_Free   (ir_node *store, ir_node *ptr, ir_node *size,
		     type *free_type);
ir_node *new_Sync   (int arity, ir_node **in);
ir_node *new_Proj   (ir_node *arg, ir_mode *mode, long proj);
ir_node *new_defaultProj (ir_node *arg, long max_proj);
ir_node *new_Tuple  (int arity, ir_node **in);
ir_node *new_Id     (ir_node *val, ir_mode *mode);
ir_node *new_Bad    (void);

/***********************************************************************/
/* The comfortable interface.                                          */
/* Supports automatic Phi node construction.                           */
/* All routines of the block oriented interface except new_Block are   */
/* needed also.                                                        */
/***********************************************************************/

/** Block construction **/
/* immature Block without predecessors */
ir_node *new_immBlock (void);

/* Add a control flow edge to an immature block. */
void add_in_edge (ir_node *immblock, ir_node *jmp);

/* fixes the number of predecessors of a block. */
void mature_block (ir_node *block);

/** Parameter administration **/
/* Read a value from the array with the local variables.  Use this
   function to obtain the last definition of the value associated with
   pos.  Pos may not exceed the value passed as n_loc to new_ir_graph. */
ir_node *get_value (int pos, ir_mode *mode);

/* Write a value in the array with the local variables. Use this function
   to remember a new definition of the value associated with pos. Pos may
   not exceed the value passed as n_loc to new_ir_graph. */
void set_value (int pos, ir_node *value);

/* Read a store.
   Use this function to get the most recent version of the store (type M).
   Internally it does the same as get_value. */
ir_node *get_store (void);

/* Write a store. */
void set_store (ir_node *store);

/* keep this node alive even if End is not control-reachable from it */
inline void keep_alive (ir_node *ka);

/** Useful access routines **/
/* Returns the current block of the current graph.  To set the current
   block use switch_block(). */
ir_node *get_cur_block();

/* Returns the frame type of the current graph */
type *get_cur_frame_type();


/***********************************************************************/
/* initialize and finalize ir construction                             */
/***********************************************************************/

/* Puts the graph into state "phase_high" */
void finalize_cons (ir_graph *irg);


# endif /* _IRCONS_H_ */
