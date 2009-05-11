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
	is_a     = "unop",
	flags    = "none",
),

Add = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Alloc = dict(
	ins   = [ "mem", "size" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	flags = "fragile, uses_memory",
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		),
		dict(
			name = "where",
			type = "ir_where_alloc"
		)
	],
	pinned      = "yes",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Alloc, &res->attr.alloc.exc.frag_arr);
	#endif
	'''
),

Anchor = dict(
	mode        = "mode_ANY",
	arity       = "variable",
	flags       = "dump_noblock",
	attr_struct = "block_attr",
	knownBlock  = True,
	singleton   = True,
),

And = dict(
	is_a     = "binop",
	flags    = "commutative",
),

ASM = dict(
	mode          = "mode_T",
	arity         = "variable",
	flags         = "keep, uses_memory",
	attr_struct   = "asm_attr",
	pinned        = "memory",
	pinned_init   = "pinned",
	attrs = [
		dict(
			name = "input_constraints",
			type = "ir_asm_constraint*",
		),
		dict(
			name = "n_output_constraints",
			type = "int",
		),
		dict(
			name = "output_constraints",
			type = "ir_asm_constraint*",
		),
		dict(
			name = "n_clobbers",
			type = "int",
		),
		dict(
			name = "clobbers",
			type = "ident**",
		),
		dict(
			name = "text",
			type = "ident*",
		),
	],
	java_noconstr = True,
),

Bad = dict(
	mode       = "mode_Bad",
	flags      = "cfopcode, fragile, start_block, dump_noblock",
	knownBlock = True,
	singleton  = True,
),

Block = dict(
	mode       = "mode_BB",
	knownBlock = True,
	block      = "NULL",
	optimize   = False,
	arity      = "variable",
	flags      = "labeled",
	java_noconstr = True,

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
	}

	public boolean isBad() {
		return binding.is_Bad(ptr) != 0;
	}
	''',
),

Borrow = dict(
	is_a     = "binop",
	flags    = "none",
),

Bound = dict(
	ins    = [ "mem", "index", "lower", "upper" ],
 	outs   = [ "M", "X_regular", "X_except", "res" ],
 	flags  = "fragile, highlevel",
	pinned = "exception",
	pinned_init = "pinned",
	attr_struct = "bound_attr",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Bound, &res->attr.bound.exc.frag_arr);
	#endif
	'''
),

Break = dict(
	mode  = "mode_X",
	flags = "cfopcode",
),

Builtin = dict(
	ins      = [ "mem" ],
	arity    = "variable",
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ],
	flags    = "uses_memory",
	attrs    = [
		dict(
			type = "ir_builtin_kind",
			name = "kind"
		),
		dict(
			type = "ir_type*",
			name = "type"
		)
	],
	pinned      = "memory",
	pinned_init = "pinned",
	init   = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''

	# TODO: No firm_alloc_frag_arr??
),

Call = dict(
	ins      = [ "mem", "ptr" ],
	arity    = "variable",
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ],
	flags    = "fragile, uses_memory",
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	],
	attr_struct = "call_attr",
	pinned      = "memory",
	pinned_init = "pinned",
	init = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	''',
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Call, &res->attr.call.exc.frag_arr);
	#endif
	'''
),

CallBegin = dict(
	ins   = [ "ptr" ],
	outs  = [ "" ], # TODO
	flags         = "cfopcode, ip_cfopcode",
	# TODO: attribute with call...
	attr_struct   = "callbegin_attr",
	java_noconstr = True,
	init = '''
	res->attr.callbegin.call = call;
	''',
),

Carry = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Cast = dict(
	ins      = [ "op" ],
	mode     = "get_irn_mode(irn_op)",
	flags    = "highlevel",
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	],
	attr_struct = "cast_attr",
	init     = "assert(is_atomic_type(type));"
),

Cmp = dict(
	is_a     = "binop",
	outs     = [ "False", "Eq", "Lt", "Le", "Gt", "Ge", "Lg", "Leg", "Uo", "Ue", "Ul", "Ule", "Ug", "Uge", "Ne", "True" ],
	flags    = "none",
),

Cond = dict(
	ins      = [ "selector" ],
	outs     = [ "false", "true" ],
	flags    = "cfopcode, forking",
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
	],
	attr_struct = "cond_attr"
),

Confirm = dict(
	ins      = [ "value", "bound" ],
	mode     = "get_irn_mode(irn_value)",
	flags    = "highlevel",
	attrs    = [
		dict(
			name = "cmp",
			type = "pn_Cmp"
		),
	],
	attr_struct = "confirm_attr",
),

Const = dict(
	mode       = "",
	flags      = "constlike, start_block",
	knownBlock = True,
	attrs_name = "con",
	attrs      = [
		dict(
			type = "tarval*",
			name = "tarval",
		)
	],
	attr_struct = "const_attr",
),

Conv = dict(
	is_a     = "unop",
	flags    = "none",
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
	],
	attr_struct = "conv_attr",
),

CopyB = dict(
	ins   = [ "mem", "dst", "src" ],
	outs  = [ "M", "X_regular", "X_except" ],
	flags = "fragile, highlevel, uses_memory",
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	],
	attr_struct = "copyb_attr",
	pinned      = "memory",
	pinned_init = "pinned",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_CopyB, &res->attr.copyb.exc.frag_arr);
	#endif
	'''
),

Div = dict(
	ins   = [ "mem", "left", "right" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	flags = "fragile, uses_memory",
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
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
	attr_struct = "divmod_attr",
	pinned = "exception",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Div, &res->attr.except.frag_arr);
	#endif
	'''
),

DivMod = dict(
	ins   = [ "mem", "left", "right" ],
	outs  = [ "M", "X_regular", "X_except", "res_div", "res_mod" ],
	flags = "fragile, uses_memory",
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	],
	attr_struct = "divmod_attr",
	pinned = "exception",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_DivMod, &res->attr.except.frag_arr);
	#endif
	'''
),

Dummy = dict(
	ins   = [],
	flags = "cfopcode, fragile, start_block, constlike, dump_noblock",
	knownBlock = True,
	block      = "get_irg_start_block(irg)",
),

End = dict(
	mode       = "mode_X",
	pinned     = "yes",
	arity      = "dynamic",
	flags      = "cfopcode",
	singleton  = True,
),

EndExcept = dict(
	mode      = "mode_X",
	pinned    = "yes",
	arity     = "dynamic",
	flags     = "cfopcode, ip_cfopcode",
	singleton = True
),

EndReg = dict(
	mode      = "mode_X",
	pinned    = "yes",
	arity     = "dynamic",
	flags     = "cfopcode, ip_cfopcode",
	singleton = True
),

Eor = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Filter = dict(
	ins   = [ "pred" ],
	flags = "none",
	attrs = [
		dict(
			name = "proj",
			type = "long"
		)
	],
	attr_struct = "filter_attr",
	java_noconstr = True

	# TODO: Broken asserts in original:
	# assert(get_Proj_pred(res));
	# assert(get_nodes_block(get_Proj_pred(res)));
),

Free = dict(
	ins   = [ "mem", "ptr", "size" ],
	mode  = "mode_M",
	flags = "uses_memory",
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		),
		dict(
			name = "where",
			type = "ir_where_alloc"
		)
	],
	attr_struct = "free_attr",
),

Id = dict(
	ins   = [ "pred" ],
	flags = "none",
),

IJmp = dict(
	mode     = "mode_X",
	pinned   = "yes",
	ins      = [ "target" ],
	flags    = "cfopcode, forking, keep",
),

InstOf = dict(
	ins   = [ "store", "obj" ],
	outs  = [ "M", "X_regular", "X_except", "res", "M_except" ],
	flags = "highlevel",
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	],
	attr_struct = "io_attr",
	pinned      = "memory",
	pinned_init = "floats",
),

Jmp = dict(
	mode     = "mode_X",
	pinned   = "yes",
	ins      = [],
	flags    = "cfopcode",
),

Load = dict(
	ins      = [ "mem", "ptr" ],
	outs     = [ "M", "X_regular", "X_except", "res" ],
	flags    = "fragile, uses_memory",
	attrs    = [
		dict(
			type = "ir_mode*",
			name = "mode",
			java_name = "load_mode"
		),
	],
	attr_struct = "load_attr",
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
	is_a     = "unop",
	flags    = "none",
),

Mod = dict(
	ins   = [ "mem", "left", "right" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	flags = "fragile, uses_memory",
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	],
	attr_struct = "divmod_attr",
	pinned = "exception",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Mod, &res->attr.except.frag_arr);
	#endif
	'''
),

Mul = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Mulh = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Mux = dict(
	ins   = [ "sel", "false", "true" ],
	flags = "none",
),

NoMem = dict(
	mode       = "mode_M",
	flags      = "dump_noblock, dump_noinput",
	knownBlock = True,
	singleton  = True,
),

Not = dict(
	is_a     = "unop",
	flags    = "none",
),

Or = dict(
	is_a     = "binop",
	flags    = "commutative",
),

Phi = dict(
	pinned      = "yes",
	arity       = "variable",
	flags       = "none",
	attr_struct = "phi_attr",
	custom_is   = True,
	java_noconstr = True,
),

Pin = dict(
	ins      = [ "op" ],
	mode     = "get_irn_mode(irn_op)",
	flags    = "highlevel",
),

Proj = dict(
	ins      = [ "pred" ],
	flags    = "none",
	attrs    = [
		dict(
			type = "long",
			name = "proj",
			initname = ""
		)
	],
	attr_struct = "long",
	custom_is   = True,
),

Quot = dict(
	ins   = [ "mem", "left", "right" ],
	outs  = [ "M", "X_regular", "X_except", "res" ],
	flags = "fragile, uses_memory",
	attrs_name = "divmod",
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	],
	attr_struct = "divmod_attr",
	pinned = "exception",
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Quot, &res->attr.except.frag_arr);
	#endif
	'''
),

Raise = dict(
	ins   = [ "mem", "exo_ptr" ],
	outs  = [ "M", "X" ],
	flags = "highlevel, cfopcode",
),

Return = dict(
	ins      = [ "mem" ],
	arity    = "variable",
	mode     = "mode_X",
	flags    = "cfopcode",
),

Rotl = dict(
	is_a     = "binop",
	flags    = "none",
),

Sel = dict(
	ins    = [ "mem", "ptr" ],
	arity  = "variable",
	flags  = "none",
	mode   = "is_Method_type(get_entity_type(entity)) ? mode_P_code : mode_P_data",
	attrs    = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	],
	attr_struct = "sel_attr",
),

Shl = dict(
	is_a     = "binop",
	flags    = "none",
),

Shr = dict(
	is_a     = "binop",
	flags    = "none",
),

Shrs = dict(
	is_a     = "binop",
	flags    = "none",
),

Start = dict(
	mode       = "mode_T",
	pinned     = "yes",
	flags      = "cfopcode",
	singleton  = True,
),

Store = dict(
	ins      = [ "mem", "ptr", "value" ],
	outs     = [ "M", "X_regular", "X_except" ],
	flags    = "fragile, uses_memory",
	attr_struct = "store_attr",
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
	is_a     = "binop",
	flags    = "none",
),

SymConst = dict(
	mode       = "mode_P",
	flags      = "constlike, start_block",
	knownBlock = True,
	attrs      = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	],
	attr_struct = "symconst_attr",
	java_noconstr = True,
),

Sync = dict(
	mode     = "mode_M",
	flags    = "none",
	optimize = False,
	arity    = "dynamic"
),

Tuple = dict(
	arity = "variable",
	mode  = "mode_T",
	flags = "labeled",
	java_noconstr = True
),

Unknown = dict(
	knownBlock = True,
	block      = "get_irg_start_block(irg)",
	flags      = "cfopcode, fragile, start_block, constlike, dump_noblock",
),
)
