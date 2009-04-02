nodes = dict(

#
# Abstract node types
#
unop = dict(
	abstract = True,
	ins      = [ "op" ]
),

binop = dict(
	abstract = True,
	ins      = [ "left", "right" ]
),

#
# Real node types
#
Abs = dict(
	is_a     = "unop"
),

Add = dict(
	is_a     = "binop"
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

Anchor = dict(
	mode       = "mode_ANY",
	ins        = [ "end_block", "start_block", "end", "start",
	               "end_reg", "end_except", "initial_exec",
				   "frame", "tls", "initial_mem", "args",
				   "bad", "no_mem" ],
	knownBlock = True,
	noconstr   = True
),

And = dict(
	is_a     = "binop"
),

Bad = dict(
	mode       = "mode_Bad",
	knownBlock = True,
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

Borrow = dict(
	is_a     = "binop"
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

Carry = dict(
	is_a     = "binop"
),

Cast = dict(
	ins      = [ "op" ],
	mode     = "get_irn_mode(irn_op)",
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	],
	init     = "assert(is_atomic_type(type));"
),

Cmp = dict(
	is_a     = "binop",
	outs     = [ "False", "Eq", "Lt", "Le", "Gt", "Ge", "Lg", "Leg", "Uo", "Ue", "Ul", "Ule", "Ug", "Uge", "Ne", "True" ],
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

Div = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state",
			initname = ".exc.pin_state"
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
	],
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Div, &res->attr.except.frag_arr);
	#endif
	'''
),

DivMod = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res_div", "res_mod" ],
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state",
			initname = ".exc.pin_state"
		)
	],
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_DivMod, &res->attr.except.frag_arr);
	#endif
	'''
),

End = dict(
	mode       = "mode_X",
	op_flags   = "cfopcode",
	state      = "pinned",
	arity      = "dynamic",
	noconstr   = True,
	optimize   = False
),

Eor = dict(
	is_a     = "binop"
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

Id = dict(
	ins = [ "pred" ]
),

IJmp = dict(
	mode     = "mode_X",
	op_flags = "cfopcode",
	state    = "pinned",
	ins      = [ "target" ],
),

Jmp = dict(
	mode     = "mode_X",
	op_flags = "cfopcode",
	state    = "pinned",
	ins      = [],
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

Minus = dict(
	is_a     = "unop"
),

Mod = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state",
			initname = ".exc.pin_state"
		)
	],
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Mod, &res->attr.except.frag_arr);
	#endif
	'''
),

Mul = dict(
	is_a     = "binop"
),

Mulh = dict(
	is_a     = "binop"
),

Mux = dict(
	ins      = [ "sel", "false", "true" ]
),

NoMem = dict(
	mode       = "mode_M",
	knownBlock = True,
),

Not = dict(
	is_a     = "unop"
),

Or = dict(
	is_a     = "binop"
),

Phi = dict(
	noconstr = True,
	state    = "pinned",
	arity    = "variable",
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

Quot = dict(
	ins   = [ "mem", "dividend", "divisor" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
		dict(
			name = "state",
			type = "op_pin_state",
			initname = ".exc.pin_state"
		)
	],
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Quot, &res->attr.except.frag_arr);
	#endif
	'''
),

Return = dict(
	ins      = [ "mem" ],
	arity    = "variable",
	mode     = "mode_X"
),

Rotl = dict(
	is_a     = "binop"
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

Shl = dict(
	is_a     = "binop"
),

Shr = dict(
	is_a     = "binop"
),

Shrs = dict(
	is_a     = "binop"
),

Start = dict(
	mode       = "mode_T",
	op_flags   = "cfopcode",
	state      = "pinned",
	noconstr   = True,
	optimize   = False
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

Sub = dict(
	is_a     = "binop"
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
)
