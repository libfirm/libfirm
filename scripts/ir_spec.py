# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
#
# Firm node specifications
# The comments are in (standard python) restructured text format and are used
# to generate documentation.
from irops import Node, abstract, op, Attribute, prepare_nodes
from jinjautil import export


@abstract
@op
class Binop(Node):
    """Binary nodes have exactly 2 inputs"""
    name = "binop"
    ins = [
        ("left", "first operand"),
        ("right", "second operand"),
    ]
    op_index = 0
    arity_override = "oparity_binary"


@abstract
@op
class EntConst(Node):
    """Symbolic constant that represents an aspect of an entity"""
    name = "entconst"
    flags = ["constlike", "start_block"]
    attrs = [
        Attribute("entity", type="ir_entity*", comment="entity to operate on"),
    ]
    attr_struct = "entconst_attr"
    attrs_name = "entc"


@abstract
@op
class TypeConst(Node):
    """A symbolic constant that represents an aspect of a type"""
    name = "typeconst"
    flags = ["constlike", "start_block"]
    attrs = [
        Attribute("type", type="ir_type*", comment="type to operate on"),
    ]
    attr_struct = "typeconst_attr"
    attrs_name = "typec"


@op
class Add(Binop):
    """returns the sum of its operands"""
    mode = "get_irn_mode(mode_is_reference(get_irn_mode(irn_right)) ? irn_right : irn_left)"
    flags = ["commutative"]


@op
class Address(EntConst):
    """Symbolic constant that represents the address of an entity (variable or
    method)"""
    mode = "mode_P"


@op
class Align(TypeConst):
    """A symbolic constant that represents the alignment of a type"""


@op
class Alloc(Node):
    """Allocates a block of memory on the stack."""
    ins = [
        ("mem", "memory dependency"),
        ("size", "size of the block in bytes"),
    ]
    outs = [
        ("M", "memory result"),
        ("res", "pointer to newly allocated memory"),
    ]
    attrs = [
        Attribute("alignment", type="unsigned",
                  comment="alignment of the memory block (must be a power of 2)"),
    ]
    flags = ["uses_memory", "const_memory"]
    pinned = "yes"
    attr_struct = "alloc_attr"


@op
class Anchor(Node):
    """Utility node used to "hold" nodes in a graph that might possibly not be
    reachable by other means or which should be reachable immediately without
    searching through the graph.
    Each firm-graph contains exactly one anchor node whose address is always
    known. All other well-known graph-nodes like Start, End, NoMem, ...
    are found by looking at the respective Anchor operand."""
    ins = [
        ("end_block", "block the end node belongs to"),
        ("start_block", "block the start node belongs to"),
        ("end", "end node of this ir_graph"),
        ("start", "start node of this ir_graph"),
        ("frame", "frame of this ir_graph"),
        ("initial_mem", "initial memory of this ir_graph"),
        ("args", "argument proj of the start node"),
        ("no_mem", "the only NoMem node of this ir_graph"),
    ]
    mode = "mode_ANY"
    flags = ["dump_noblock"]
    pinned = "yes"
    singleton = True
    constructor = False


@op
class And(Binop):
    """returns the result of a bitwise and operation of its operands"""
    mode = "get_irn_mode(irn_left)"
    flags = ["commutative"]


@op
class ASM(Node):
    """executes assembler fragments of the target machine.

    The node contains a template for an assembler snippet. The compiler will
    replace occurrences of %0, %1, ... with input/output operands, %% with a
    single % char. Some backends allow additional specifiers (for example %w3,
    %b3, %h3 on x86 to get a 16bit, 8hit low, 8bit high part of a register).
    After the replacements the text is emitted into the final assembly.

    The clobber list contains names of registers which have an undefined value
    after the assembler instruction is executed; it may also contain 'memory'
    or 'cc' if global state/memory changes or the condition code registers
    (some backends implicitly set cc on all ASM statements).

    Example (an i386 instruction):

        ASM(text="btsl %1, %0",
            constraints = ["+m", "r"],
            clobbers = ["cc"])

    %0 references a memory reference which the operation both reads and writes.
    For this the node has an address input operand.  %1 references an input
    which is passed as a register. The condition code register has an unknown
    value after the instruction.

    (This format is inspired by the gcc extended asm syntax)
    """
    mode = "mode_T"
    arity = "variable"
    input_name = "input"
    flags = ["fragile", "keep", "uses_memory"]
    only_regular = True
    pinned = "exception"
    pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
    throws_init = "(flags & cons_throws_exception) != 0"
    attr_struct = "asm_attr"
    attrs_name = "assem"
    ins = [
        ("mem", "memory dependency"),
    ]
    outs = [
        ("M", "memory result"),
        ("X_regular", "control flow when no jump occurs"),
        ("first_out", "first output"),
    ]
    attrs = [
        Attribute("constraints", type="ir_asm_constraint*", init="NULL",
                  comment="constraints"),
        Attribute("clobbers", type="ident**", init="NULL",
                  comment="list of clobbered registers/memory"),
        Attribute("text", type="ident*", comment="assembler text"),
    ]
    constructor_args = [
        Attribute("n_constraints", type="size_t",
                  comment="number of constraints"),
        Attribute("constraints", type="ir_asm_constraint*",
                  comment="constraints"),
        Attribute("n_clobbers", type="size_t",
                  comment="number of clobbered registers/memory"),
        Attribute("clobbers", type="ident**",
                  comment="list of clobbered registers/memory"),
        Attribute("flags", type="ir_cons_flags",
                  comment="specifies alignment, volatility and pin state"),
    ]
    init = '''
    struct obstack *const obst = get_irg_obstack(irg);
    attr->constraints = NEW_ARR_D(ir_asm_constraint, obst, n_constraints);
    attr->clobbers    = NEW_ARR_D(ident*,            obst, n_clobbers);

    MEMCPY(attr->constraints, constraints, n_constraints);
    MEMCPY(attr->clobbers,    clobbers,    n_clobbers);
    '''
    serializer = False


@op
class Bad(Node):
    """
    Bad nodes indicate invalid input, which is values which should never be
    computed.

    The typical use case for the Bad node is removing unreachable code.
    Frontends should set the current_block to Bad when it is clear that
    following code must be unreachable (i.e. after a goto or return statement).
    Optimizations also set block predecessors to Bad when it becomes clear,
    that a control flow edge can never be executed.

    The gigo optimizations ensures that nodes with Bad as their block, get
    replaced by Bad themselves. Nodes with at least 1 Bad input get exchanged
    with Bad too. Exception to this rule are Block, Phi, Tuple and End node;
    This is because removing inputs from a Block is hairy operation (requiring,
    Phis to be shortened too for example). So instead of removing block inputs
    they are set to Bad, and the actual removal is left to the control flow
    optimization phase. Block, Phi, Tuple with only Bad inputs however are
    replaced by Bad right away.

    In the future we may use the Bad node to model poison values that arise
    from undefined behaviour like reading uninitialized local variables in C.
    """
    flags = ["start_block", "dump_noblock"]
    pinned = "yes"


@op
class Deleted(Node):
    """Internal node which is temporary set to nodes which are already removed
    from the graph."""
    mode = "mode_Bad"
    flags = []
    pinned = "yes"
    constructor = False


@op
class Block(Node):
    """A basic block"""
    mode = "mode_BB"
    block = "NULL"
    pinned = "yes"
    arity = "variable"
    input_name = "cfgpred"
    flags = []
    attr_struct = "block_attr"
    attrs = [
        Attribute("entity", type="ir_entity*", init="NULL",
                  comment="entity representing this block"),
    ]
    serializer = False

    init = '''
    res->attr.block.backedge = new_backedge_arr(get_irg_obstack(irg), arity);
    set_Block_matured(res, 1);

    /* Create and initialize array for Phi-node construction. */
    if (irg_is_constrained(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION)) {
        res->attr.block.graph_arr = NEW_ARR_DZ(ir_node*, get_irg_obstack(irg), irg->n_loc);
    }
    '''


@op
class Builtin(Node):
    """performs a backend-specific builtin."""
    ins = [
        ("mem", "memory dependency"),
    ]
    arity = "variable"
    input_name = "param"
    outs = [
        ("M", "memory result"),
        # results follow here
    ]
    mode = "mode_T"
    flags = ["uses_memory"]
    attrs = [
        Attribute("kind", type="ir_builtin_kind", comment="kind of builtin"),
        Attribute("type", type="ir_type*",
                  comment="method type for the builtin call"),
    ]
    pinned = "exception"
    pinned_init = "op_pin_state_pinned"
    attr_struct = "builtin_attr"
    init = '''
    assert((get_unknown_type() == type) || is_Method_type(type));
    '''


@op
class Call(Node):
    """Calls other code. Control flow is transferred to ptr, additional
    operands are passed to the called code. Called code usually performs a
    return operation. The operands of this return operation are the result
    of the Call node."""
    ins = [
        ("mem", "memory dependency"),
        ("ptr", "pointer to called code"),
    ]
    arity = "variable"
    input_name = "param"
    outs = [
        ("M", "memory result"),
        ("T_result", "tuple containing all results"),
        ("X_regular", "control flow when no exception occurs"),
        ("X_except", "control flow when exception occurred"),
    ]
    flags = ["fragile", "uses_memory"]
    attrs = [
        Attribute("type", type="ir_type*",
                  comment="type of the call (usually type of the called procedure)"),
    ]
    attr_struct = "call_attr"
    pinned = "exception"
    pinned_init = "op_pin_state_pinned"
    throws_init = "false"
    init = '''
    assert((get_unknown_type() == type) || is_Method_type(type));
    '''


@op
class Cmp(Binop):
    """Compares its two operands and checks whether a specified
       relation (like less or equal) is fulfilled."""
    flags = []
    mode = "mode_b"
    attrs = [
        Attribute("relation", type="ir_relation",
                  comment="Comparison relation"),
    ]
    attr_struct = "cmp_attr"


@op
class Cond(Node):
    """Conditionally change control flow."""
    ins = [
        ("selector", "condition parameter"),
    ]
    outs = [
        ("false", "control flow if operand is \"false\""),
        ("true", "control flow if operand is \"true\""),
    ]
    flags = ["cfopcode", "forking"]
    pinned = "yes"
    attrs = [
        Attribute("jmp_pred", type="cond_jmp_predicate",
                  init="COND_JMP_PRED_NONE",
                  comment="can indicate the most likely jump"),
    ]
    attr_struct = "cond_attr"


@op
class Switch(Node):
    """Change control flow. The destination is chosen based on an integer
    input value which is looked up in a table.

    Backends can implement this efficiently using a jump table."""
    ins = [
        ("selector", "input selector"),
    ]
    outs = [
        ("default", "control flow if no other case matches"),
    ]
    mode = "mode_T"
    flags = ["cfopcode", "forking"]
    pinned = "yes"
    attrs = [
        Attribute("n_outs", type="unsigned",
                  comment="number of outputs (including pn_Switch_default)"),
        Attribute("table", type="ir_switch_table*",
                  comment="table describing mapping from input values to Proj numbers"),
    ]
    attr_struct = "switch_attr"
    attrs_name = "switcha"


@op
class Confirm(Node):
    """Specifies constraints for a value. This allows explicit representation
    of path-sensitive properties. (Example: This value is always >= 0 on 1
    if-branch then all users within that branch are rerouted to a confirm-node
    specifying this property).

    A constraint is specified for the relation between value and bound.
    value is always returned.
    Note that this node does NOT check or assert the constraint, it merely
    specifies it."""
    ins = [
        ("value", "value to express a constraint for"),
        ("bound", "value to compare against"),
    ]
    mode = "get_irn_mode(irn_value)"
    flags = []
    pinned = "yes"
    attrs = [
        Attribute("relation", type="ir_relation",
                  comment="relation of value to bound"),
    ]
    attr_struct = "confirm_attr"


@op
class Const(Node):
    """Returns a constant value."""
    flags = ["constlike", "start_block"]
    mode = "get_tarval_mode(tarval)"
    attrs = [
        Attribute("tarval", type="ir_tarval*",
                  comment="constant value (a tarval object)"),
    ]
    attr_struct = "const_attr"
    attrs_name = "con"


@op
class Conv(Node):
    """Converts values between modes"""
    flags = []
    ins = [
        ("op", "operand")
    ]


@op
class Bitcast(Node):
    """Converts a value between modes with different arithmetics but same
    number of bits by reinterpreting the bits in the new mode"""
    flags = []
    ins = [
        ("op", "operand")
    ]


@op
class CopyB(Node):
    """Copies a block of memory with statically known size/type."""
    ins = [
        ("mem", "memory dependency"),
        ("dst", "destination address"),
        ("src", "source address"),
    ]
    mode = "mode_M"
    flags = ["uses_memory"]
    attrs = [
        Attribute("type", type="ir_type*", comment="type of copied data"),
        Attribute("volatility", type="ir_volatility",
                  init="flags & cons_volatile ? volatility_is_volatile : volatility_non_volatile",
                  to_flags="%s == volatility_is_volatile ? cons_volatile : cons_none",
                  comment="volatile CopyB nodes have a visible side-effect and may not be optimized"),
    ]
    attr_struct = "copyb_attr"
    constructor_args = [
        Attribute("flags", type="ir_cons_flags",
                  comment="specifies volatility"),
    ]


@op
class Div(Node):
    """returns the quotient of its 2 operands"""
    ins = [
        ("mem", "memory dependency"),
        ("left", "first operand"),
        ("right", "second operand"),
    ]
    outs = [
        ("M", "memory result"),
        ("res", "result of computation"),
        ("X_regular", "control flow when no exception occurs"),
        ("X_except", "control flow when exception occurred"),
    ]
    flags = ["fragile", "uses_memory", "const_memory"]
    attrs = [
        Attribute("resmode", type="ir_mode*",
                  init="get_irn_mode(irn_left)",
                  comment="mode of the result value"),
        Attribute("no_remainder", type="int", init="0",
                  comment="Set when division remainder is known to be zero"),
    ]
    attr_struct = "div_attr"
    pinned = "exception"
    throws_init = "false"
    op_index = 1
    arity_override = "oparity_binary"


@op
class Dummy(Node):
    """A placeholder value. This is used when constructing cyclic graphs where
    you have cases where not all predecessors of a phi-node are known. Dummy
    nodes are used for the unknown predecessors and replaced later."""
    ins = []
    flags = ["cfopcode", "start_block", "constlike", "dump_noblock"]
    pinned = "yes"


@op
class End(Node):
    """Last node of a graph. It references nodes in endless loops (so called
    keepalive edges)"""
    mode = "mode_X"
    pinned = "yes"
    arity = "dynamic"
    input_name = "keepalive"
    flags = ["cfopcode"]
    block = "get_irg_end_block(irg)"
    singleton = True


@op
class Eor(Binop):
    """returns the result of a bitwise exclusive or operation of its operands.

    This is also known as the Xor operation."""
    mode = "get_irn_mode(irn_left)"
    flags = ["commutative"]


@op
class Free(Node):
    """Frees a block of memory previously allocated by an Alloc node"""
    ins = [
        ("mem", "memory dependency"),
        ("ptr", "pointer to the object to free"),
    ]
    mode = "mode_M"
    flags = ["uses_memory", "const_memory"]
    pinned = "yes"


@op
class Id(Node):
    """Returns its operand unchanged.

    This is mainly used when exchanging nodes. Usually you shouldn't see Id
    nodes since the getters/setters for node inputs skip them automatically."""
    ins = [
        ("pred", "the value which is returned unchanged"),
    ]
    flags = []
    constructor = False


@op
class IJmp(Node):
    """Jumps to the code in its argument. The code has to be in the same
    function and the destination must be one of the blocks reachable
    by the tuple results"""
    mode = "mode_X"
    pinned = "yes"
    ins = [
        ("target", "target address of the jump"),
    ]
    flags = ["cfopcode", "forking", "keep", "unknown_jump"]


@op
class Jmp(Node):
    """Jumps to the block connected through the out-value"""
    mode = "mode_X"
    pinned = "yes"
    ins = []
    flags = ["cfopcode"]


@op
class Load(Node):
    """Loads a value from memory (heap or stack)."""
    ins = [
        ("mem", "memory dependency"),
        ("ptr", "address to load from"),
    ]
    outs = [
        ("M", "memory result"),
        ("res", "result of load operation"),
        ("X_regular", "control flow when no exception occurs"),
        ("X_except", "control flow when exception occurred"),
    ]
    flags = ["fragile", "uses_memory", "const_memory"]
    pinned = "exception"
    attrs = [
        Attribute("mode", type="ir_mode*",
                  comment="mode of the value to be loaded"),
        Attribute("type", type="ir_type*",
                  comment="The type of the object which is stored at ptr (need not match with mode)"),
        Attribute("volatility", type="ir_volatility",
                  init="flags & cons_volatile ? volatility_is_volatile : volatility_non_volatile",
                  to_flags="%s == volatility_is_volatile ? cons_volatile : cons_none",
                  comment="volatile loads are a visible side-effect and may not be optimized"),
        Attribute("unaligned", type="ir_align",
                  init="flags & cons_unaligned ? align_non_aligned : align_is_aligned",
                  to_flags="%s == align_non_aligned ? cons_unaligned : cons_none",
                  comment="pointers to unaligned loads don't need to respect the load-mode/type alignments"),
    ]
    attr_struct = "load_attr"
    constructor_args = [
        Attribute("flags", type="ir_cons_flags",
                  comment="specifies alignment, volatility and pin state"),
    ]
    pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
    throws_init = "(flags & cons_throws_exception) != 0"


@op
class Minus(Node):
    """returns the additive inverse of its operand"""
    mode = "get_irn_mode(irn_op)"
    flags = []
    ins = [
        ("op", "operand")
    ]


@op
class Mod(Node):
    """returns the remainder of its operands from an implied division.

    Examples:

    * mod(5,3)   produces 2
    * mod(5,-3)  produces 2
    * mod(-5,3)  produces -2
    * mod(-5,-3) produces -2
    """
    ins = [
        ("mem", "memory dependency"),
        ("left", "first operand"),
        ("right", "second operand"),
    ]
    outs = [
        ("M", "memory result"),
        ("res", "result of computation"),
        ("X_regular", "control flow when no exception occurs"),
        ("X_except", "control flow when exception occurred"),
    ]
    flags = ["fragile", "uses_memory", "const_memory"]
    attrs = [
        Attribute("resmode", type="ir_mode*",
                  init="get_irn_mode(irn_left)",
                  comment="mode of the result"),
    ]
    attr_struct = "mod_attr"
    pinned = "exception"
    throws_init = "false"
    op_index = 1
    arity_override = "oparity_binary"


class Mul(Binop):
    """returns the product of its operands"""
    mode = "get_irn_mode(irn_left)"
    flags = ["commutative"]


class Mulh(Binop):
    """returns the upper word of the product of its operands (the part which
    would not fit into the result mode of a normal Mul anymore)"""
    mode = "get_irn_mode(irn_left)"
    flags = ["commutative"]


@op
class Mux(Node):
    """returns the false or true operand depending on the value of the sel
    operand"""
    ins = [
        ("sel", "value making the output selection"),
        ("false", "selected if sel input is false"),
        ("true", "selected if sel input is true"),
    ]
    mode = "get_irn_mode(irn_false)"
    flags = []


@op
class NoMem(Node):
    """Placeholder node for cases where you don't need any memory input"""
    mode = "mode_M"
    flags = ["dump_noblock", "start_block"]
    pinned = "yes"
    singleton = True


@op
class Not(Node):
    """returns the bitwise complement of a value. Works for boolean values, too."""
    mode = "get_irn_mode(irn_op)"
    flags = []
    ins = [
        ("op", "operand")
    ]


@op
class Offset(EntConst):
    """Symbolic constant that represents the offset of an entity in its owner type."""


@op
class Or(Binop):
    """returns the result of a bitwise or operation of its operands"""
    mode = "get_irn_mode(irn_left)"
    flags = ["commutative"]


@op
class Phi(Node):
    """Choose a value based on control flow. A phi node has 1 input for each
    predecessor of its block. If a block is entered from its nth predecessor
    all phi nodes produce their nth input as result."""
    pinned = "yes"
    arity = "variable"
    input_name = "pred"
    flags = []
    attrs = [
        Attribute("loop", type="int", init="0",
                  comment="whether Phi represents the observable effect of a (possibly) nonterminating loop"),
    ]
    attr_struct = "phi_attr"
    init = '''
    res->attr.phi.u.backedge = new_backedge_arr(get_irg_obstack(irg), arity);'''
    serializer = False


@op
class Pin(Node):
    """Pin the value of the node node in the current block. No users of the Pin
    node can float above the Block of the Pin. The node cannot float behind
    this block. Often used to Pin the NoMem node."""
    ins = [
        ("op", "value which is pinned"),
    ]
    mode = "get_irn_mode(irn_op)"
    flags = []
    pinned = "yes"


@op
class Proj(Node):
    """returns an entry of a tuple value"""
    ins = [
        ("pred", "the tuple value from which a part is extracted"),
    ]
    flags = []
    block = "get_nodes_block(irn_pred)"
    usesGraph = False
    attrs = [
        Attribute("num", type="unsigned",
                  comment="number of tuple component to be extracted"),
    ]
    attr_struct = "proj_attr"


@op
class Raise(Node):
    """Raises an exception. Unconditional change of control flow. Writes an
    explicit Except variable to memory to pass it to the exception handler.
    Must be lowered to a Call to a runtime check function."""
    ins = [
        ("mem", "memory dependency"),
        ("exo_ptr", "pointer to exception object to be thrown"),
    ]
    outs = [
        ("M", "memory result"),
        ("X", "control flow to exception handler"),
    ]
    flags = ["cfopcode"]
    pinned = "yes"


@op
class Return(Node):
    """Returns from the current function. Takes memory and return values as
    operands."""
    ins = [
        ("mem", "memory dependency"),
    ]
    arity = "variable"
    input_name = "res"
    mode = "mode_X"
    flags = ["cfopcode"]
    pinned = "yes"


@op
class Sel(Node):
    """Computes the address of an array element from the array base pointer and
    an index.

    A Sel node must only produce a NULL pointer if the ptr input is NULL."""
    ins = [
        ("ptr", "pointer to array to select from"),
        ("index", "index to select"),
    ]
    flags = []
    mode = "mode_P"
    attrs = [
        Attribute("type", type="ir_type*", comment="array type"),
    ]
    attr_struct = "sel_attr"


@op
class Member(Node):
    """Computes the address of a compound type member given the base address
    of an instance of the compound type.

    A Member node must only produce a NULL pointer if the ptr input is NULL."""
    ins = [
        ("ptr", "pointer to object to select from"),
    ]
    flags = []
    mode = "mode_P"
    attrs = [
        Attribute("entity", type="ir_entity*",
                  comment="entity which is selected"),
    ]
    attr_struct = "member_attr"


@op
class Shl(Binop):
    """Returns its first operands bits shifted left by the amount of the 2nd
    operand.
    The right input (shift amount) must be an unsigned integer value.
    If the result mode has modulo_shift!=0, then the effective shift amount is
    the right input modulo this modulo_shift amount."""
    mode = "get_irn_mode(irn_left)"
    flags = []


@op
class Shr(Binop):
    """Returns its first operands bits shifted right by the amount of the 2nd
    operand. No special handling for the sign bit is performed (zero extension).
    The right input (shift amount) must be an unsigned integer value.
    If the result mode has modulo_shift!=0, then the effective shift amount is
    the right input modulo this modulo_shift amount."""
    mode = "get_irn_mode(irn_left)"
    flags = []


@op
class Shrs(Binop):
    """Returns its first operands bits shifted right by the amount of the 2nd
    operand. The leftmost bit (usually the sign bit) stays the same
    (sign extension).
    The right input (shift amount) must be an unsigned integer value.
    If the result mode has modulo_shift!=0, then the effective shift amount is
    the right input modulo this modulo_shift amount."""
    mode = "get_irn_mode(irn_left)"
    flags = []


@op
class Start(Node):
    """The first node of a graph. Execution starts with this node."""
    outs = [
        ("M", "initial memory"),
        ("P_frame_base", "frame base pointer"),
        ("T_args", "function arguments")
    ]
    mode = "mode_T"
    pinned = "yes"
    flags = ["start_block"]
    singleton = True


@op
class Store(Node):
    """Stores a value into memory (heap or stack)."""
    ins = [
        ("mem", "memory dependency"),
        ("ptr", "address to store to"),
        ("value", "value to store"),
    ]
    outs = [
        ("M", "memory result"),
        ("X_regular", "control flow when no exception occurs"),
        ("X_except", "control flow when exception occurred"),
    ]
    flags = ["fragile", "uses_memory"]
    pinned = "exception"
    attr_struct = "store_attr"
    pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
    throws_init = "(flags & cons_throws_exception) != 0"
    attrs = [
        Attribute("type", type="ir_type*",
                  comment="The type of the object which is stored at ptr (need not match with value's type)"),
        Attribute("volatility", type="ir_volatility",
                  init="flags & cons_volatile ? volatility_is_volatile : volatility_non_volatile",
                  to_flags="%s == volatility_is_volatile ? cons_volatile : cons_none",
                  comment="volatile stores are a visible side-effect and may not be optimized"),
        Attribute("unaligned", type="ir_align",
                  init="flags & cons_unaligned ? align_non_aligned : align_is_aligned",
                  to_flags="%s == align_non_aligned ? cons_unaligned : cons_none",
                  comment="pointers to unaligned stores don't need to respect the load-mode/type alignments"),
    ]
    constructor_args = [
        Attribute("flags", type="ir_cons_flags",
                  comment="specifies alignment, volatility and pin state"),
    ]


@op
class Sub(Binop):
    """returns the difference of its operands"""
    mode = "mode_is_reference(get_irn_mode(irn_right)) ? get_reference_offset_mode(get_irn_mode(irn_left)) : get_irn_mode(irn_left)"
    flags = []


@op
class Size(TypeConst):
    """A symbolic constant that represents the size of a type"""


@op
class Sync(Node):
    """The Sync operation unifies several partial memory blocks. These blocks
    have to be pairwise disjunct or the values in common locations have to
    be identical.  This operation allows to specify all operations that
    eventually need several partial memory blocks as input with a single
    entrance by unifying the memories with a preceding Sync operation."""
    mode = "mode_M"
    flags = []
    arity = "dynamic"
    input_name = "pred"


@op
class Tuple(Node):
    """Builds a Tuple from single values.

    This is needed to implement optimizations that remove a node that produced
    a tuple.  The node can be replaced by the Tuple operation so that the
    following Proj nodes have not to be changed. (They are hard to find due to
    the implementation with pointers in only one direction.) The Tuple node is
    smaller than any other node, so that a node can be changed into a Tuple by
    just changing its opcode and giving it a new in array."""
    arity = "variable"
    input_name = "pred"
    mode = "mode_T"
    flags = []


@op
class Unknown(Node):
    """Returns an unknown (at compile- and runtime) value. It is a valid
    optimization to replace an Unknown by any other constant value.

    Be careful when optimising Unknown values, you cannot simply replace
    Unknown+x or Unknown<x with a new Unknown node if there are multiple
    users of the original unknown node!"""
    pinned = "yes"
    flags = ["start_block", "constlike", "dump_noblock"]


name = "ir"
(nodes, abstract_nodes) = prepare_nodes(globals())
export(nodes, "nodes")
export(abstract_nodes, "abstract_nodes")
export(globals(), "spec")
