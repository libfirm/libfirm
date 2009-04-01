nodes = dict(
Start = dict(
	mode       = "mode_T",
	op_flags   = "cfopcode",
	state      = "pinned",
	noconstr   = True,
	optimize   = False
),

End = dict(
	mode       = "mode_X",
	op_flags   = "cfopcode",
	state      = "pinned",
	arity      = "dynamic",
	noconstr   = True,
	optimize   = False
),

Id = dict(
	ins = [ "pred" ]
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
	attrs_name = "con",
	attrs      = [
		dict(
			type = "tarval*",
			name = "tarval",
		)
	],
),

Block = dict(
	mode       = "mode_BB",
	knownBlock = True,
	block      = "NULL",
	noconstr   = True,
	optimize   = False,
	arity      = "variable",

	init = '''
	/* macroblock header */
	res->in[0] = res;

	res->attr.block.is_dead     = 0;
	res->attr.block.is_mb_head  = 1;
	res->attr.block.has_label   = 0;
	res->attr.block.irg         = irg;
	res->attr.block.backedge    = new_backedge_arr(irg->obst, arity);
	res->attr.block.in_cg       = NULL;
	res->attr.block.cg_backedge = NULL;
	res->attr.block.extblk      = NULL;
	res->attr.block.mb_depth    = 0;
	res->attr.block.label       = 0;

	set_Block_matured(res, 1);
	set_Block_block_visited(res, 0);
	''',

	d_pre = '''
	int i;
	int has_unknown = 0;
	''',

	d_post = '''
	/* Create and initialize array for Phi-node construction. */
	if (get_irg_phase_state(current_ir_graph) == phase_building) {
		res->attr.block.graph_arr = NEW_ARR_D(ir_node *, current_ir_graph->obst,
		                                      current_ir_graph->n_loc);
		memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);
	}

	for (i = arity - 1; i >= 0; i--)
		if (is_Unknown(in[i])) {
			has_unknown = 1;
			break;
		}

	if (!has_unknown) res = optimize_node(res);

	current_ir_graph->current_block = res;

	IRN_VRFY_IRG(res, current_ir_graph);
	''',

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
		),
		dict(
			name = "no_remainder",
			type = "int",
			init = "0",
			special = dict(
				suffix = "RL",
				init = "1"
			)
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
			type = "ir_cons_flags",
			name = "flags",
		),
	],
	d_post = '''
#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Load, &res->attr.load.exc.frag_arr);
#endif
	'''
),

Store = dict(
	ins      = [ "mem", "ptr", "value" ],
	outs     = [ "M", "X_regular", "X_except" ],
	constructor_args = [
		dict(
			type = "ir_cons_flags",
			name = "flags",
		),
	],
	d_post = '''
#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Store, &res->attr.store.exc.frag_arr);
#endif
	'''
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
	mode     = "get_irn_mode(irn_op)"
),

Proj = dict(
	ins      = [ "pred" ],
	attrs    = [
		dict(
			type = "long",
			name = "proj",
			initname = ""
		)
	]
),

Sel = dict(
	ins    = [ "mem", "ptr" ],
	arity  = "variable",
	mode   = "is_Method_type(get_entity_type(entity)) ? mode_P_code : mode_P_data",
	attrs    = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	]
),

Sync = dict(
	mode     = "mode_M",
	optimize = False,
	arity    = "dynamic"
),

Tuple = dict(
	arity    = "variable",
	mode     = "mode_T",
),

Unknown = dict(
	knownBlock = True,
	block      = "get_irg_start_block(irg)",
	nodbginfo  = True
),

Confirm = dict(
	ins      = [ "value", "bound" ],
	mode     = "get_irn_mode(irn_value)",
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
	attrs    = [
		dict(
			name = "kind",
			type = "cond_kind",
			init = "dense"
		),
		dict(
			name = "default_proj",
			type = "long",
			init = "0"
		),
		dict(
			name = "jmp_pred",
			type = "cond_jmp_predicate",
			init = "COND_JMP_PRED_NONE"
		)
	]
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
			type = "int",
			init = "0",
			special = dict(
				prefix = "strict",
				init = "1"
			)
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
