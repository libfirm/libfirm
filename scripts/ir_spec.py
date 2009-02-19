nodes = dict(
Start = dict(
	mode       = "mode_T",
	op_flags   = "cfopcode",
	state      = "pinned",
	knownBlock = True,
	noconstr   = True,
),

End = dict(
	mode       = "mode_X",
	op_flags   = "cfopcode",
	state      = "pinned",
	arity      = "dynamic",
	knownBlock = True,
	noconstr   = True,
),

Phi = dict(
	noconstr = True,
	state    = "pinned",
	arity    = "variable",
),

Jmp = dict(
	mode     = "mode_X",
	op_flags = "cfopcode",
	state    = "pinned",
	ins      = [],
),

IJmp = dict(
	mode     = "mode_X",
	op_flags = "cfopcode",
	state    = "pinned",
	ins      = [ "target" ],
),

Const = dict(
	mode       = "",
	knownBlock = True,
	attrs      = [
		dict(
			type = "tarval*",
			name = "tarval",
		)
	],
),

Block = dict(
	mode   = "mode_BB",
	knownBlock = True,
	noconstr   = True,
	arity      = "variable",
	java_add   = '''
	public void addPred(Node node) {
		binding_cons.add_immBlock_pred(ptr, node.ptr);
	}

	public void mature() {
		binding_cons.mature_immBlock(ptr);
	}

	@Override
	public Block getBlock() {
		return null;
	}

	public boolean blockVisited() {
		return 0 != binding.Block_block_visited(ptr);
	}

	public void markBlockVisited() {
		binding.mark_Block_block_visited(ptr);
	}''',
),

SymConst = dict(
	mode       = "mode_P",
	knownBlock = True,
	noconstr   = True,
	attrs      = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	],
),

# SymConst

Call = dict(
	ins      = [ "mem", "ptr" ],
	arity    = "variable",
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ],
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	]
),

Builtin = dict(
	ins      = [ "mem" ],
	arity    = "variable",
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ],
	attrs    = [
		dict(
			type = "ir_builtin_kind",
			name = "kind"
		),
		dict(
			type = "ir_type*",
			name = "type"
		)
	]
),

binop = dict(
	abstract = True,
	ins      = [ "left", "right" ]
),

Add = dict(
	is_a     = "binop"
),

Carry = dict(
	is_a     = "binop"
),

Sub = dict(
	is_a     = "binop"
),

Borrow = dict(
	is_a     = "binop"
),

Mul = dict(
	is_a     = "binop"
),

Mulh = dict(
	is_a     = "binop"
),

Abs = dict(
	is_a     = "unop"
),

And = dict(
	is_a     = "binop"
),

Or = dict(
	is_a     = "binop"
),

Eor = dict(
	is_a     = "binop"
),

Not = dict(
	is_a     = "unop"
),

Shl = dict(
	is_a     = "binop"
),

Shr = dict(
	is_a     = "binop"
),

Shrs = dict(
	is_a     = "binop"
),

Rotl = dict(
	is_a     = "binop"
),

Quot = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state"
		)
	]
),

Div = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state"
		)
	]
),

DivMod = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res_div", "res_mod" ],
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state"
		)
	]
),

Mod = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state"
		)
	]
),

Load = dict(
	ins      = [ "mem", "ptr" ],
	outs     = [ "M", "X_regular", "X_except", "res" ],
	attrs    = [
		dict(
			type = "ir_mode*",
			name = "mode",
			java_name = "load_mode"
		),
	],
	constructor_args = [
		dict(
			type = "cons_flags",
			name = "flags",
		),
	],
),

Store = dict(
	ins      = [ "mem", "ptr", "value" ],
	outs     = [ "M", "X_regular", "X_except" ],
	constructor_args = [
		dict(
			type = "cons_flags",
			name = "flags",
		),
	],
),

Anchor = dict(
	mode       = "mode_ANY",
	ins        = [ "end_block", "start_block", "end", "start",
	               "end_reg", "end_except", "initial_exec",
				   "frame", "tls", "initial_mem", "args",
				   "bad", "no_mem" ],
	knownBlock = True,
	noconstr   = True
),

NoMem = dict(
	mode       = "mode_M",
	knownBlock = True,
),

Bad = dict(
	mode       = "mode_Bad",
	knownBlock = True,
),

Pin = dict(
	ins      = [ "op" ],
	mode     = "get_irn_mode(op);"
),

Proj = dict(
	ins      = [ "pred" ],
	attrs    = [
		dict(
			type = "long",
			name = "proj"
		)
	]
),

Sel = dict(
	ins    = [ "mem", "ptr" ],
	arity  = "variable",
	mode   = "mode_P",
	attrs    = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	]
),

Sync = dict(
	mode     = "mode_M",
	arity    = "dynamic"
),

Tuple = dict(
	arity    = "variable",
	mode     = "mode_T",
),

Unknown = dict(
	knownBlock = True
),

Confirm = dict(
	ins      = [ "value", "bound" ],
	block    = "get_nodes_block(value)",
	mode     = "get_irn_mode(value)",
	attrs    = [
		dict(
			name = "cmp",
			type = "pn_Cmp"
		),
	],
),

Return = dict(
	ins      = [ "mem" ],
	arity    = "variable",
	mode     = "mode_X"
),

unop = dict(
	abstract = True,
	ins      = [ "op" ]
),

Minus = dict(
	is_a     = "unop"
),

Mux = dict(
	ins      = [ "sel", "false", "true" ]
),

Cond = dict(
	ins      = [ "selector" ],
	outs     = [ "false", "true" ],
),

Cmp = dict(
	is_a     = "binop",
	outs     = [ "False", "Eq", "Lt", "Le", "Gt", "Ge", "Lg", "Leg", "Uo", "Ue", "Ul", "Ule", "Ug", "Uge", "Ne", "True" ],
),

Conv = dict(
	is_a     = "unop",
	attrs = [
		dict(
			name = "strict",
			type = "int"
		)
	]
),

Alloc = dict(
	ins   = [ "mem", "size" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		),
		dict(
			name = "where",
			type = "ir_where_alloc"
		)
	]
),

Free = dict(
	ins   = [ "mem", "ptr", "size" ],
	mode  = "mode_M",
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		),
		dict(
			name = "where",
			type = "ir_where_alloc"
		)
	]
),

CopyB = dict(
	ins   = [ "mem", "dst", "src" ],
	outs  = [ "M", "X_regular", "X_except" ],
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	]
),
)
