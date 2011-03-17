from spec_util import abstract, setnodedefaults

class Op(object):
	"""Base class for firm nodes"""
abstract(Op)

class Unop(Op):
	"""Unary nodes have exactly 1 input"""
	name     = "unop"
	ins      = [
		("op",  "operand"),
	]
	op_index = 0
	pinned   = "no"
abstract(Unop)

class Binop(Op):
	"""Binary nodes have exactly 2 inputs"""
	name     = "binop"
	ins      = [
		( "left",   "first operand" ),
		( "right", "second operand" ),
	]
	op_index = 0
	pinned   = "no"
abstract(Binop)

class Add(Binop):
	"""returns the sum of its operands"""
	flags = [ "commutative" ]

class Alloc(Op):
	"""allocates a block of memory.
	It can be specified whether the variable should be allocated to the stack
	or to the heap."""
	ins   = [
		("mem",   "memory dependency" ),
		("count", "number of objects to allocate" ),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "pointer to newly allocated memory",     "pn_Generic_other"),
	]
	attrs = [
		dict(
			name    = "type",
			type    = "ir_type*",
			comment = "type of the allocated variable",
		),
		dict(
			name    = "where",
			type    = "ir_where_alloc",
			comment = "whether to allocate the variable on the stack or heap",
		)
	]
	flags       = [ "fragile", "uses_memory" ]
	pinned      = "yes"
	attr_struct = "alloc_attr"

class Anchor(Op):
	"""utiliy node used to "hold" nodes in a graph that might possibly not be
	reachable by other means or which should be reachable immediately without
	searching through the graph.
	Each firm-graph contains exactly one anchor node whose address is always
	known. All other well-known graph-nodes like Start, End, NoMem, Bad, ...
	are found by looking at the respective Anchor operand."""
	mode             = "mode_ANY"
	arity            = "variable"
	flags            = [ "dump_noblock" ]
	pinned           = "yes"
	attr_struct      = "irg_attr"
	knownBlock       = True
	singleton        = True
	noconstructor    = True
	customSerializer = True

class And(Binop):
	"""returns the result of a bitwise and operation of its operands"""
	flags    = [ "commutative" ]

class ASM(Op):
	"""executes assembler fragments of the target machine"""
	mode             = "mode_T"
	arity            = "variable"
	flags            = [ "keep", "uses_memory" ]
	pinned           = "memory"
	pinned_init      = "op_pin_state_pinned"
	attr_struct      = "asm_attr"
	attrs_name       = "assem"
	customSerializer = True
	attrs = [
		dict(
			name    = "input_constraints",
			type    = "ir_asm_constraint*",
			comment = "input constraints",
		),
		dict(
			name    = "n_output_constraints",
			type    = "int",
			noprop  = True,
			comment = "number of output constraints",
		),
		dict(
			name    = "output_constraints",
			type    = "ir_asm_constraint*",
			comment = "output constraints",
		),
		dict(
			name    = "n_clobbers",
			type    = "int",
			noprop  = True,
			comment = "number of clobbered registers/memory",
		),
		dict(
			name    = "clobbers",
			type    = "ident**",
			comment = "list of clobbered registers/memory",
		),
		dict(
			name    = "text",
			type    = "ident*",
			comment = "assembler text",
		),
	]
	# constructor is written manually at the moment, because of the clobbers+
	# constraints arrays needing special handling (2 arguments for 1 attribute)
	noconstructor = True

class Bad(Op):
	"""Bad nodes indicate invalid input, which is values which should never be
	computed.

	The typical use case for the Bad node is removing unreachable code.
	Frontends should set the current_block to Bad when it is clear that
	following code must be unreachable (ie. after a goto or return statement).
	Optimisations also set block predecessors to Bad when it becomes clear,
	that a control flow edge can never be executed.

	The gigo optimisations ensures that nodes with Bad as their block, get
	replaced by Bad themselfes. Nodes with at least 1 Bad input get exchanged
	with Bad too. Exception to this rule are Block, Phi, Tuple and End node;
	This is because removing inputs from a Block is hairy operation (requiring,
	Phis to be shortened too for example). So instead of removing block inputs
	they are set to Bad, and the actual removal is left to the control flow
	optimisation phase. Block, Phi, Tuple with only Bad inputs however are
	replaced by Bad right away."""
	mode          = "mode_T"
	flags         = [ "cfopcode", "start_block", "dump_noblock" ]
	pinned        = "yes"
	knownBlock    = True
	block         = "get_irg_start_block(irg)"
	singleton     = True
	attr_struct   = "bad_attr"
	init = '''
	res->attr.bad.irg.irg = irg;
	'''

class Deleted(Op):
	"""Internal node which is temporary set to nodes which are already removed
	from the graph."""
	mode             = "mode_Bad"
	flags            = [ ]
	pinned           = "yes"
	noconstructor    = True
	customSerializer = True

class Block(Op):
	"""A basic block"""
	mode             = "mode_BB"
	knownBlock       = True
	block            = "NULL"
	pinned           = "yes"
	arity            = "variable"
	flags            = [ "labeled" ]
	attr_struct      = "block_attr"
	customSerializer = True

	init = '''
	res->attr.block.irg.irg     = irg;
	res->attr.block.backedge    = new_backedge_arr(irg->obst, arity);
	set_Block_matured(res, 1);

	/* Create and initialize array for Phi-node construction. */
	if (get_irg_phase_state(irg) == phase_building) {
		res->attr.block.graph_arr = NEW_ARR_D(ir_node *, irg->obst, irg->n_loc);
		memset(res->attr.block.graph_arr, 0, irg->n_loc * sizeof(ir_node*));
	}
	'''

class Borrow(Binop):
	"""Returns the borrow bit from and implied subtractions of its 2 operands"""
	flags = []

class Bound(Op):
	"""Performs a bounds-check: if lower <= index < upper then return index,
	otherwise throw an exception."""
	ins    = [
		("mem",    "memory dependency"),
		("index",  "value to test"),
		("lower",  "lower bound (inclusive)"),
		("upper",  "upper bound (exclusive)"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "the checked index",                     "pn_Generic_other"),
	]
 	flags  = [ "fragile", "highlevel" ]
	pinned = "exception"
	pinned_init = "op_pin_state_pinned"
	attr_struct = "bound_attr"
	attrs_name  = "bound"

class Builtin(Op):
	"""performs a backend-specific builtin."""
	ins      = [
		("mem", "memory dependency"),
	]
	arity    = "variable"
	outs     = [
		("M",        "memory result", "pn_Generic_M"),
		("1_result", "first result",  "pn_Generic_other"),
	]
	flags    = [ "uses_memory" ]
	attrs    = [
		dict(
			type    = "ir_builtin_kind",
			name    = "kind",
			comment = "kind of builtin",
		),
		dict(
			type    = "ir_type*",
			name    = "type",
			comment = "method type for the builtin call",
		)
	]
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	attr_struct = "builtin_attr"
	init   = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''

class Call(Op):
	"""Calls other code. Control flow is transfered to ptr, additional
	operands are passed to the called code. Called code usually performs a
	return operation. The operands of this return operation are the result
	of the Call node."""
	ins      = [
		("mem",   "memory dependency"),
		("ptr",   "pointer to called code"),
	]
	arity    = "variable"
	outs     = [
		("M",                "memory result",                         "pn_Generic_M"),
		("X_regular",        "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",         "control flow when exception occured",   "pn_Generic_X_except"),
		("T_result",         "tuple containing all results",          "pn_Generic_other"),
	]
	flags    = [ "fragile", "uses_memory" ]
	attrs    = [
		dict(
			type    = "ir_type*",
			name    = "type",
			comment = "type of the call (usually type of the called procedure)",
		),
		dict(
			type = "unsigned",
			name = "tail_call",
			# the tail call attribute can only be set by analysis
			init = "0",
		)
	]
	attr_struct = "call_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	init = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''

class Carry(Binop):
	"""Computes the value of the carry-bit that would result when adding the 2
	operands"""
	flags = [ "commutative" ]

class Cast(Unop):
	"""perform a high-level type cast"""
	mode     = "get_irn_mode(irn_op)"
	flags    = [ "highlevel" ]
	attrs    = [
		dict(
			type    = "ir_type*",
			name    = "type",
			comment = "target type of the case",
		)
	]
	attr_struct = "cast_attr"
	init     = "assert(is_atomic_type(type));"

class Cmp(Binop):
	"""Compares its two operands and checks whether a specified
	   relation (like less or equal) is fulfilled."""
	flags = []
	mode  = "mode_b"
	attrs = [
		dict(
			type    = "ir_relation",
			name    = "relation",
			comment = "Comparison relation"
		)
	]
	attr_struct = "cmp_attr"

class Cond(Op):
	"""Conditionally change control flow. There are two versions of this node:

	Boolean Cond:
	Input:  A value of mode_b
	Output: A tuple of two control flows. The first is taken if the input is
	        false, the second if it is true.

	Switch Cond:
	Input:  A value of mode_Iu
	Output: A tuple of n control flows. If the Cond's input is i, control flow
	will proceed along output i. If the input is >= n control flow proceeds
	along output def_proj.
	"""
	ins      = [
		("selector",  "condition parameter"),
	]
	outs     = [
		("false", "control flow if operand is \"false\""),
		("true",  "control flow if operand is \"true\""),
	]
	flags    = [ "cfopcode", "forking" ]
	pinned   = "yes"
	attrs    = [
		dict(
			name    = "default_proj",
			type    = "long",
			init    = "0",
			comment = "Proj-number of default case for switch-Cond",
		),
		dict(
			name    = "jmp_pred",
			type    = "cond_jmp_predicate",
			init    = "COND_JMP_PRED_NONE",
			comment = "can indicate the most likely jump",
		)
	]
	attr_struct = "cond_attr"

class Confirm(Op):
	"""Specifies constraints for a value. This allows explicit representation
	of path-sensitive properties. (Example: This value is always >= 0 on 1
	if-branch then all users within that branch are rerouted to a confirm-node
	specifying this property).

	A constraint is specified for the relation between value and bound.
	value is always returned.
	Note that this node does NOT check or assert the constraint, it merely
	specifies it."""
	ins      = [
		("value",  "value to express a constraint for"),
		("bound",  "value to compare against"),
	]
	mode     = "get_irn_mode(irn_value)"
	flags    = [ "highlevel" ]
	pinned   = "yes"
	attrs    = [
		dict(
			name    = "relation",
			type    = "ir_relation",
			comment = "relation of value to bound",
		),
	]
	attr_struct = "confirm_attr"
	attrs_name  = "confirm"

class Const(Op):
	"""Returns a constant value."""
	flags      = [ "constlike", "start_block" ]
	block      = "get_irg_start_block(irg)"
	mode       = "get_tarval_mode(tarval)"
	knownBlock = True
	pinned     = "no"
	attrs      = [
		dict(
			type    = "ir_tarval*",
			name    = "tarval",
			comment = "constant value (a tarval object)",
		)
	]
	attr_struct = "const_attr"
	attrs_name  = "con"

class Conv(Unop):
	"""Converts values between modes"""
	flags = []
	attrs = [
		dict(
			name    = "strict",
			type    = "int",
			init    = "0",
			comment = "force floating point to restrict precision even if backend computes in higher precision (deprecated)",
		)
	]
	attr_struct = "conv_attr"
	attrs_name  = "conv"

class CopyB(Op):
	"""Copies a block of memory"""
	ins   = [
		("mem",  "memory dependency"),
		("dst",  "destination address"),
		("src",  "source address"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs = [
		dict(
			name    = "type",
			type    = "ir_type*",
			comment = "type of copied data",
		)
	]
	attr_struct = "copyb_attr"
	attrs_name  = "copyb"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"

class Div(Op):
	"""returns the quotient of its 2 operands"""
	ins   = [
		("mem",   "memory dependency"),
		("left",  "first operand"),
		("right", "second operand"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of computation",                 "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "div"
	attrs = [
		dict(
			type    = "ir_mode*",
			name    = "resmode",
			comment = "mode of the result value",
		),
		dict(
			name = "no_remainder",
			type = "int",
			init = "0",
		)
	]
	attr_struct = "div_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"

class Dummy(Op):
	"""A placeholder value. This is used when constructing cyclic graphs where
	you have cases where not all predecessors of a phi-node are known. Dummy
	nodes are used for the unknown predecessors and replaced later."""
	ins        = []
	flags      = [ "cfopcode", "start_block", "constlike", "dump_noblock" ]
	knownBlock = True
	pinned     = "yes"
	block      = "get_irg_start_block(irg)"

class End(Op):
	"""Last node of a graph. It references nodes in endless loops (so called
	keepalive edges)"""
	mode             = "mode_X"
	pinned           = "yes"
	arity            = "dynamic"
	flags            = [ "cfopcode" ]
	knownBlock       = True
	block            = "get_irg_end_block(irg)"
	singleton        = True
	customSerializer = True

class Eor(Binop):
	"""returns the result of a bitwise exclusive or operation of its operands"""
	flags    = [ "commutative" ]

class Free(Op):
	"""Frees a block of memory previously allocated by an Alloc node"""
	ins    = [
		("mem",   "memory dependency" ),
		("ptr",   "pointer to the object to free"),
		("size",  "number of objects to allocate" ),
	]
	mode   = "mode_M"
	flags  = [ "uses_memory" ]
	pinned = "yes"
	attrs  = [
		dict(
			name    = "type",
			type    = "ir_type*",
			comment = "type of the allocated variable",
		),
		dict(
			name    = "where",
			type    = "ir_where_alloc",
			comment = "whether allocation was on the stack or heap",
		)
	]
	attr_struct = "free_attr"

class Id(Op):
	"""Returns its operand unchanged."""
	ins    = [
	   ("pred", "the value which is returned unchanged")
	]
	pinned = "no"
	flags  = []

class IJmp(Op):
	"""Jumps to the code in its argument. The code has to be in the same
	function and the the destination must be one of the blocks reachable
	by the tuple results"""
	mode     = "mode_X"
	pinned   = "yes"
	ins      = [
	   ("target", "target address of the jump"),
	]
	flags    = [ "cfopcode", "forking", "keep" ]

class InstOf(Op):
	"""Tests whether an object is an instance of a class-type"""
	ins   = [
	   ("store", "memory dependency"),
	   ("obj",   "pointer to object being queried")
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "checked object pointer",                "pn_Generic_other"),
	]
	flags = [ "highlevel" ]
	attrs = [
		dict(
			name    = "type",
			type    = "ir_type*",
			comment = "type to check ptr for",
		)
	]
	attr_struct = "io_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_floats"

class Jmp(Op):
	"""Jumps to the block connected through the out-value"""
	mode     = "mode_X"
	pinned   = "yes"
	ins      = []
	flags    = [ "cfopcode" ]

class Load(Op):
	"""Loads a value from memory (heap or stack)."""
	ins   = [
		("mem", "memory dependency"),
		("ptr",  "address to load from"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of load operation",              "pn_Generic_other"),
	]
	flags    = [ "fragile", "uses_memory" ]
	pinned   = "exception"
	attrs    = [
		dict(
			type      = "ir_mode*",
			name      = "mode",
			comment   = "mode of the value to be loaded",
		),
		dict(
			type      = "ir_volatility",
			name      = "volatility",
			comment   = "volatile loads are a visible side-effect and may not be optimized",
			init      = "flags & cons_volatile ? volatility_is_volatile : volatility_non_volatile",
		),
		dict(
			type      = "ir_align",
			name      = "unaligned",
			comment   = "pointers to unaligned loads don't need to respect the load-mode/type alignments",
			init      = "flags & cons_unaligned ? align_non_aligned : align_is_aligned",
		),
	]
	attr_struct = "load_attr"
	constructor_args = [
		dict(
			type    = "ir_cons_flags",
			name    = "flags",
			comment = "specifies alignment, volatility and pin state",
		),
	]
	pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"

class Minus(Unop):
	"""returns the difference between its operands"""
	flags = []

class Mod(Op):
	"""returns the remainder of its operands from an implied division.

	Examples:
	* mod(5,3)   produces 2
	* mod(5,-3)  produces 2
	* mod(-5,3)  produces -2
	* mod(-5,-3) produces -2
	"""
	ins   = [
		("mem",   "memory dependency"),
		("left",  "first operand"),
		("right", "second operand"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of computation",                 "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "mod"
	attrs = [
		dict(
			type    = "ir_mode*",
			name    = "resmode",
			comment = "mode of the result",
		),
	]
	attr_struct = "mod_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"

class Mul(Binop):
	"""returns the product of its operands"""
	flags = [ "commutative" ]

class Mulh(Binop):
	"""returns the upper word of the product of its operands (the part which
	would not fit into the result mode of a normal Mul anymore)"""
	flags = [ "commutative" ]

class Mux(Op):
	"""returns the false or true operand depending on the value of the sel
	operand"""
	ins    = [
	   ("sel",   "value making the output selection"),
	   ("false", "selected if sel input is false"),
	   ("true",  "selected if sel input is true"),
	]
	flags  = []
	pinned = "no"

class NoMem(Op):
	"""Placeholder node for cases where you don't need any memory input"""
	mode          = "mode_M"
	flags         = [ "dump_noblock", "dump_noinput" ]
	pinned        = "yes"
	knownBlock    = True
	block         = "get_irg_start_block(irg)"
	singleton     = True

class Not(Unop):
	"""returns the logical complement of a value. Works for integer values too.
	If the input is false/zero then true/one is returned, otherwise false/zero
	is returned."""
	flags = []

class Or(Binop):
	"""returns the result of a bitwise or operation of its operands"""
	flags = [ "commutative" ]

class Phi(Op):
	"""Choose a value based on control flow. A phi node has 1 input for each
	predecessor of its block. If a block is entered from its nth predecessor
	all phi nodes produce their nth input as result."""
	pinned        = "yes"
	arity         = "variable"
	flags         = []
	attr_struct   = "phi_attr"
	init          = '''
	res->attr.phi.u.backedge = new_backedge_arr(irg->obst, arity);'''
	init_after_opt = '''
	/* Memory Phis in endless loops must be kept alive.
	   As we can't distinguish these easily we keep all of them alive. */
	if (is_Phi(res) && mode == mode_M)
		add_End_keepalive(get_irg_end(irg), res);'''

class Pin(Op):
	"""Pin the value of the node node in the current block. No users of the Pin
	node can float above the Block of the Pin. The node cannot float behind
	this block. Often used to Pin the NoMem node."""
	ins      = [
		("op", "value which is pinned"),
	]
	mode     = "get_irn_mode(irn_op)"
	flags    = [ "highlevel" ]
	pinned   = "yes"

class Proj(Op):
	"""returns an entry of a tuple value"""
	ins              = [
		("pred", "the tuple value from which a part is extracted"),
	]
	flags            = []
	pinned           = "no"
	knownBlock       = True
	knownGraph       = True
	block            = "get_nodes_block(irn_pred)"
	graph            = "get_irn_irg(irn_pred)"
	customSerializer = True
	attrs      = [
		dict(
			type    = "long",
			name    = "proj",
			comment = "number of tuple component to be extracted",
		),
	]
	attr_struct = "proj_attr"

class Raise(Op):
	"""Raises an exception. Unconditional change of control flow. Writes an
	explicit Except variable to memory to pass it to the exception handler.
	Must be lowered to a Call to a runtime check function."""
	ins    = [
		("mem",     "memory dependency"),
		("exo_ptr", "pointer to exception object to be thrown"),
	]
	outs  = [
		("M", "memory result",                     "pn_Generic_M"),
		("X", "control flow to exception handler", "pn_Generic_X_regular"),
	]
	flags  = [ "highlevel", "cfopcode" ]
	pinned = "yes"

class Return(Op):
	"""Returns from the current function. Takes memory and return values as
	operands."""
	ins      = [
		("mem", "memory dependency"),
	]
	arity    = "variable"
	mode     = "mode_X"
	flags    = [ "cfopcode" ]
	pinned   = "yes"

class Rotl(Binop):
	"""Returns its first operand bits rotated left by the amount in the 2nd
	operand"""
	flags    = []

class Sel(Op):
	"""Computes the address of a entity of a compound type given the base
	address of an instance of the compound type."""
	ins    = [
		("mem", "memory dependency"),
		("ptr", "pointer to object to select from"),
	]
	arity  = "variable"
	flags  = []
	mode   = "is_Method_type(get_entity_type(entity)) ? mode_P_code : mode_P_data"
	pinned = "no"
	attrs  = [
		dict(
			type    = "ir_entity*",
			name    = "entity",
			comment = "entity which is selected",
		)
	]
	attr_struct = "sel_attr"

class Shl(Binop):
	"""Returns its first operands bits shifted left by the amount of the 2nd
	operand"""
	flags = []

class Shr(Binop):
	"""Returns its first operands bits shifted right by the amount of the 2nd
	operand. No special handling for the sign bit (zero extension)"""
	flags = []

class Shrs(Binop):
	"""Returns its first operands bits shifted right by the amount of the 2nd
	operand. The leftmost bit (usually the sign bit) stays the same
	(sign extension)"""
	flags = []

class Start(Op):
	"""The first node of a graph. Execution starts with this node."""
	outs       = [
		("X_initial_exec", "control flow"),
		("M",              "initial memory"),
		("P_frame_base",   "frame base pointer"),
		("P_tls",          "pointer to thread local storage segment"),
		("T_args",         "function arguments")
	]
	mode             = "mode_T"
	pinned           = "yes"
	flags            = [ "cfopcode" ]
	singleton        = True
	knownBlock       = True
	customSerializer = True
	block            = "get_irg_start_block(irg)"

class Store(Op):
	"""Stores a value into memory (heap or stack)."""
	ins   = [
	   ("mem",   "memory dependency"),
	   ("ptr",   "address to store to"),
	   ("value", "value to store"),
	]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
	]
	flags    = [ "fragile", "uses_memory" ]
	pinned   = "exception"
	attr_struct = "store_attr"
	pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
	attrs = [
		dict(
			type      = "ir_volatility",
			name      = "volatility",
			comment   = "volatile stores are a visible side-effect and may not be optimized",
			init      = "flags & cons_volatile ? volatility_is_volatile : volatility_non_volatile",
		),
		dict(
			type      = "ir_align",
			name      = "unaligned",
			comment   = "pointers to unaligned stores don't need to respect the load-mode/type alignments",
			init      = "flags & cons_unaligned ? align_non_aligned : align_is_aligned",
		),
	]
	constructor_args = [
		dict(
			type    = "ir_cons_flags",
			name    = "flags",
			comment = "specifies alignment, volatility and pin state",
		),
	]

class Sub(Binop):
	"""returns the difference of its operands"""
	flags = []

class SymConst(Op):
	"""A symbolic constant.

	 - symconst_type_tag   The symbolic constant represents a type tag.  The
	                       type the tag stands for is given explicitly.
	 - symconst_type_size  The symbolic constant represents the size of a type.
	                       The type of which the constant represents the size
	                       is given explicitly.
	 - symconst_type_align The symbolic constant represents the alignment of a
	                       type.  The type of which the constant represents the
	                       size is given explicitly.
	 - symconst_addr_ent   The symbolic constant represents the address of an
	                       entity (variable or method).  The variable is given
	                       explicitly by a firm entity.
	 - symconst_ofs_ent    The symbolic constant represents the offset of an
	                       entity in its owner type.
	 - symconst_enum_const The symbolic constant is a enumeration constant of
	                       an enumeration type."""
	mode       = "mode_P"
	flags      = [ "constlike", "start_block" ]
	knownBlock = True
	pinned     = "no"
	attrs      = [
		dict(
			type    = "ir_entity*",
			name    = "entity",
			noprop  = True,
			comment = "entity whose address is returned",
		)
	]
	attr_struct = "symconst_attr"
	customSerializer = True
	# constructor is written manually at the moment, because of the strange
	# union argument
	noconstructor = True

class Sync(Op):
	"""The Sync operation unifies several partial memory blocks. These blocks
	have to be pairwise disjunct or the values in common locations have to
	be identical.  This operation allows to specify all operations that
	eventually need several partial memory blocks as input with a single
	entrance by unifying the memories with a preceding Sync operation."""
	mode     = "mode_M"
	flags    = []
	pinned   = "no"
	arity    = "dynamic"

class Tuple(Op):
	"""Builds a Tuple from single values.

	This is needed to implement optimizations that remove a node that produced
	a tuple.  The node can be replaced by the Tuple operation so that the
	following Proj nodes have not to be changed. (They are hard to find due to
	the implementation with pointers in only one direction.) The Tuple node is
	smaller than any other node, so that a node can be changed into a Tuple by
	just changing its opcode and giving it a new in array."""
	arity  = "variable"
	mode   = "mode_T"
	pinned = "no"
	flags  = [ "labeled" ]

class Unknown(Op):
	"""Returns an unknown (at compile- and runtime) value. It is a valid
	optimisation to replace an Unknown by any other constant value."""
	knownBlock = True
	pinned     = "yes"
	block      = "get_irg_start_block(irg)"
	flags      = [ "cfopcode", "start_block", "constlike", "dump_noblock" ]

# Prepare node list

def getOpList(namespace):
	nodes = []
	for t in namespace.values():
		if type(t) != type:
			continue

		if issubclass(t, Op):
			setnodedefaults(t)
			nodes.append(t)
	return nodes

nodes = getOpList(globals())
nodes = sorted(nodes, lambda x,y: cmp(x.name, y.name))
