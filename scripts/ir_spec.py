from spec_util import abstract, setnodedefaults

class Op(object):
	"Base class for firm nodes"
abstract(Op)

class Unop(Op):
	"Unary nodes have exactly 1 input"
	name     = "unop"
	ins      = [ "op" ]
	op_index = 0
	pinned   = "no"
abstract(Unop)

class Binop(Op):
	"Binary nodes have exactly 2 inputs"
	name     = "binop"
	ins      = [ "left", "right" ]
	op_index = 0
	pinned   = "no"
abstract(Binop)

class Abs(Unop):
	flags = []

class Add(Binop):
	flags = ["commutative"]

class Alloc(Op):
	ins   = [ "mem", "count" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "pointer to newly allocated memory",     "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
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
	pinned      = "yes"
	attr_struct = "alloc_attr"
	d_post = '''
	firm_alloc_frag_arr(res, op_Alloc, &res->attr.alloc.exc.frag_arr);
	'''

class Anchor(Op):
	mode        = "mode_ANY"
	arity       = "variable"
	flags       = [ "dump_noblock" ]
	pinned      = "yes"
	knownBlock  = True
	singleton   = True

class And(Binop):
	flags    = [ "commutative" ]

class ASM(Op):
	mode          = "mode_T"
	arity         = "variable"
	flags         = [ "keep", "uses_memory" ]
	pinned        = "memory"
	pinned_init   = "op_pin_state_pinned"
	attr_struct   = "asm_attr"
	attrs_name    = "assem"
	attrs = [
		dict(
			name = "input_constraints",
			type = "ir_asm_constraint*",
		),
		dict(
			name = "n_output_constraints",
			type = "int",
			noprop = True,
		),
		dict(
			name = "output_constraints",
			type = "ir_asm_constraint*",
		),
		dict(
			name = "n_clobbers",
			type = "int",
			noprop = True,
		),
		dict(
			name = "clobbers",
			type = "ident**",
		),
		dict(
			name = "text",
			type = "ident*",
		),
	]
	java_noconstr = True

class Bad(Op):
	mode        = "mode_Bad"
	flags       = [ "cfopcode", "fragile", "start_block", "dump_noblock" ]
	pinned      = "yes"
	knownBlock  = True
	singleton   = True
	attr_struct = "irg_attr"
	init = '''
	res->attr.irg.irg = irg;
	'''

class Block(Op):
	mode        = "mode_BB"
	knownBlock  = True
	block       = "NULL"
	pinned      = "yes"
	optimize    = False
	arity       = "variable"
	flags       = [ "labeled" ]
	attr_struct = "block_attr"
	java_noconstr = True

	init = '''
	/* macroblock header */
	res->in[0] = res;

	res->attr.block.is_dead     = 0;
	res->attr.block.is_mb_head  = 1;
	res->attr.block.irg.irg     = irg;
	res->attr.block.backedge    = new_backedge_arr(irg->obst, arity);
	res->attr.block.in_cg       = NULL;
	res->attr.block.cg_backedge = NULL;
	res->attr.block.extblk      = NULL;
	res->attr.block.mb_depth    = 0;
	res->attr.block.entity      = NULL;

	set_Block_matured(res, 1);
	set_Block_block_visited(res, 0);

	/* Create and initialize array for Phi-node construction. */
	if (get_irg_phase_state(irg) == phase_building) {
		res->attr.block.graph_arr = NEW_ARR_D(ir_node *, irg->obst, irg->n_loc);
		memset(res->attr.block.graph_arr, 0, irg->n_loc * sizeof(ir_node*));
	}
	'''

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
	'''

class Borrow(Binop):
	flags = []

class Bound(Op):
	ins    = [ "mem", "index", "lower", "upper" ]
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
	d_post = '''
	firm_alloc_frag_arr(res, op_Bound, &res->attr.bound.exc.frag_arr);
	'''

class Builtin(Op):
	ins      = [ "mem" ]
	arity    = "variable"
	outs     = [
		("M",        "memory result", "pn_Generic_M"),
		("1_result", "first result",  "pn_Generic_other"),
	]
	flags    = [ "uses_memory" ]
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
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	attr_struct = "builtin_attr"
	init   = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''

class Call(Op):
	ins      = [ "mem", "ptr" ]
	arity    = "variable"
	outs     = [
		("M",                "memory result",                         "pn_Generic_M"),
		("X_regular",        "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",         "control flow when exception occured",   "pn_Generic_X_except"),
		("T_result",         "tuple containing all results",          "pn_Generic_other"),
		("P_value_res_base", "pointer to memory register containing copied results passed by value"),
	]
	flags    = [ "fragile", "uses_memory" ]
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		),
		dict(
			type = "unsigned",
			name = "tail_call",
			# the tail call attribute can only be set by analysis
			init = "0"
		)
	]
	attr_struct = "call_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	init = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''
	d_post = '''
	firm_alloc_frag_arr(res, op_Call, &res->attr.call.exc.frag_arr);
	'''

class Carry(Binop):
	flags = [ "commutative" ]

class Cast(Unop):
	mode     = "get_irn_mode(irn_op)"
	flags    = [ "highlevel" ]
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	]
	attr_struct = "cast_attr"
	init     = "assert(is_atomic_type(type));"

class Cmp(Binop):
	outs  = [
		("False", "always false",                            "0"),
		("Eq",    "equal",                                   "1"),
		("Lt",    "less",                                    "2"),
		("Le",    "less or equal",                           "pn_Cmp_Eq|pn_Cmp_Lt"),
		("Gt",    "greater",                                 "4"),
		("Ge",    "greater or equal",                        "pn_Cmp_Eq|pn_Cmp_Gt"),
		("Lg",    "less or greater ('not equal' for integer numbers)", "pn_Cmp_Lt|pn_Cmp_Gt"),
		("Leg",   "less, equal or greater ('not unordered')", "pn_Cmp_Lt|pn_Cmp_Eq|pn_Cmp_Gt"),
		("Uo",    "unordered",                               "8"),
		("Ue",    "unordered or equal",                      "pn_Cmp_Uo|pn_Cmp_Eq"),
		("Ul",    "unordered or less",                       "pn_Cmp_Uo|pn_Cmp_Lt"),
		("Ule",   "unordered, less or equal",                "pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Eq"),
		("Ug",    "unordered or greater",                    "pn_Cmp_Uo|pn_Cmp_Gt"),
		("Uge",   "onordered, greater or equal",             "pn_Cmp_Uo|pn_Cmp_Gt|pn_Cmp_Eq"),
		("Ne",    "unordered, less or greater ('not equal' for floatingpoint numbers)", "pn_Cmp_Uo|pn_Cmp_Lt|pn_Cmp_Gt"),
		("True",  "always true",                             "15"),
	]
	flags = []

class Cond(Op):
	ins      = [ "selector" ]
	outs     = [
		("false", "control flow if operand is \"false\""),
		("true",  "control flow if operand is \"true\""),
	]
	flags    = [ "cfopcode", "forking" ]
	pinned   = "yes"
	attrs    = [
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
	attr_struct = "cond_attr"

class Confirm(Op):
	ins      = [ "value", "bound" ]
	mode     = "get_irn_mode(irn_value)"
	flags    = [ "highlevel" ]
	pinned   = "yes"
	attrs    = [
		dict(
			name = "cmp",
			type = "pn_Cmp"
		),
	]
	attr_struct = "confirm_attr"
	attrs_name  = "confirm"

class Const(Op):
	mode       = ""
	flags      = [ "constlike", "start_block" ]
	knownBlock = True
	pinned     = "no"
	attrs_name = "con"
	attrs      = [
		dict(
			type = "tarval*",
			name = "tarval",
		)
	]
	attr_struct = "const_attr"

class Conv(Unop):
	flags = []
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
	attr_struct = "conv_attr"
	attrs_name  = "conv"

class CopyB(Op):
	ins   = [ "mem", "dst", "src" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
	]
	flags = [ "fragile", "highlevel", "uses_memory" ]
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	]
	attr_struct = "copyb_attr"
	attrs_name  = "copyb"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	d_post = '''
	firm_alloc_frag_arr(res, op_CopyB, &res->attr.copyb.exc.frag_arr);
	'''

class Div(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of computation",                 "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "divmod"
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
	]
	attr_struct = "divmod_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"
	d_post = '''
	firm_alloc_frag_arr(res, op_Div, &res->attr.except.frag_arr);
	'''

class DivMod(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res_div",   "result of computation a/b",             "pn_Generic_other"),
		("res_mod",   "result of computation a%b"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "divmod"
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	]
	attr_struct = "divmod_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"
	d_post = '''
	firm_alloc_frag_arr(res, op_DivMod, &res->attr.except.frag_arr);
	'''

class Dummy(Op):
	ins   = []
	flags = [ "cfopcode", "fragile", "start_block", "constlike",
	          "dump_noblock" ]
	knownBlock = True
	pinned     = "yes"
	block      = "get_irg_start_block(irg)"

class End(Op):
	mode       = "mode_X"
	pinned     = "yes"
	arity      = "dynamic"
	flags      = [ "cfopcode" ]
	singleton  = True

class Eor(Binop):
	flags    = [ "commutative" ]

class Free(Op):
	ins    = [ "mem", "ptr", "size" ]
	mode   = "mode_M"
	flags  = [ "uses_memory" ]
	pinned = "yes"
	attrs  = [
		dict(
			name = "type",
			type = "ir_type*"
		),
		dict(
			name = "where",
			type = "ir_where_alloc"
		)
	]
	attr_struct = "free_attr"

class Id(Op):
	ins    = [ "pred" ]
	pinned = "no"
	flags  = []

class IJmp(Op):
	mode     = "mode_X"
	pinned   = "yes"
	ins      = [ "target" ]
	flags    = [ "cfopcode", "forking", "keep" ]

class InstOf(Op):
	ins   = [ "store", "obj" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "checked object pointer",                "pn_Generic_other"),
	]
	flags = [ "highlevel" ]
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	]
	attr_struct = "io_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_floats"

class Jmp(Op):
	mode     = "mode_X"
	pinned   = "yes"
	ins      = []
	flags    = [ "cfopcode" ]

class Load(Op):
	ins      = [ "mem", "ptr" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of load operation",              "pn_Generic_other"),
	]
	flags    = [ "fragile", "uses_memory" ]
	pinned   = "exception"
	pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
	attrs    = [
		dict(
			type = "ir_mode*",
			name = "mode",
			java_name = "load_mode"
		),
	]
	attr_struct = "load_attr"
	constructor_args = [
		dict(
			type = "ir_cons_flags",
			name = "flags",
		),
	]
	d_post = '''
	firm_alloc_frag_arr(res, op_Load, &res->attr.load.exc.frag_arr);
	'''

class Minus(Unop):
	flags = []

class Mod(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of computation",                 "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "divmod"
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	]
	attr_struct = "divmod_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"
	d_post = '''
	firm_alloc_frag_arr(res, op_Mod, &res->attr.except.frag_arr);
	'''

class Mul(Binop):
	flags = [ "commutative" ]

class Mulh(Binop):
	flags = [ "commutative" ]

class Mux(Op):
	ins    = [ "sel", "false", "true" ]
	flags  = []
	pinned = "no"

class NoMem(Op):
	mode       = "mode_M"
	flags      = [ "dump_noblock", "dump_noinput" ]
	pinned     = "yes"
	knownBlock = True
	singleton  = True

class Not(Unop):
	flags = []

class Or(Binop):
	flags = [ "commutative" ]

class Phi(Op):
	pinned        = "yes"
	arity         = "variable"
	flags         = []
	attr_struct   = "phi_attr"
	java_noconstr = True
	init = '''
	/* Memory Phis in endless loops must be kept alive.
	   As we can't distinguish these easily we keep all of them alive. */
   	if (is_Phi(res) && mode == mode_M)
		add_End_keepalive(get_irg_end(irg), res);
	'''

class Pin(Op):
	ins      = [ "op" ]
	mode     = "get_irn_mode(irn_op)"
	flags    = [ "highlevel" ]
	pinned   = "yes"

class Proj(Op):
	ins        = [ "pred" ]
	flags      = []
	pinned     = "no"
	knownBlock = True
	knownGraph = True
	block      = "get_nodes_block(irn_pred)"
	graph      = "get_irn_irg(irn_pred)"
	attrs      = [
		dict(
			type = "long",
			name = "proj",
			initname = "",
			noprop = False,
		)
	]
	attr_struct = "long"

class Quot(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
		("res",       "result of computation",                 "pn_Generic_other"),
	]
	flags = [ "fragile", "uses_memory" ]
	attrs_name = "divmod"
	attrs = [
		dict(
			type = "ir_mode*",
			name = "resmode"
		),
	]
	attr_struct = "divmod_attr"
	pinned      = "exception"
	op_index    = 1
	arity_override = "oparity_binary"
	d_post = '''
	firm_alloc_frag_arr(res, op_Quot, &res->attr.except.frag_arr);
	'''

class Raise(Op):
	ins    = [ "mem", "exo_ptr" ]
	outs  = [
		("M", "memory result",                     "pn_Generic_M"),
		("X", "control flow to exception handler", "pn_Generic_X_regular"),
	]
	flags  = [ "highlevel", "cfopcode" ]
	pinned = "yes"

class Return(Op):
	ins      = [ "mem" ]
	arity    = "variable"
	mode     = "mode_X"
	flags    = [ "cfopcode" ]
	pinned   = "yes"

class Rotl(Binop):
	flags    = []

class Sel(Op):
	ins    = [ "mem", "ptr" ]
	arity  = "variable"
	flags  = []
	mode   = "is_Method_type(get_entity_type(entity)) ? mode_P_code : mode_P_data"
	pinned = "no"
	attrs  = [
		dict(
			type = "ir_entity*",
			name = "entity"
		)
	]
	attr_struct = "sel_attr"

class Shl(Binop):
	flags = []

class Shr(Binop):
	flags = []

class Shrs(Binop):
	flags = []

class Start(Op):
	outs       = [
		("X_initial_exec", "control flow"),
		("M",              "initial memory"),
		("P_frame_base",   "frame base pointer"),
		("P_tls",          "pointer to thread local storage segment"),
		("T_args",         "function arguments")
	]
	mode       = "mode_T"
	pinned     = "yes"
	flags      = [ "cfopcode" ]
	singleton  = True

class Store(Op):
	ins      = [ "mem", "ptr", "value" ]
	outs  = [
		("M",         "memory result",                         "pn_Generic_M"),
		("X_regular", "control flow when no exception occurs", "pn_Generic_X_regular"),
		("X_except",  "control flow when exception occured",   "pn_Generic_X_except"),
	]
	flags    = [ "fragile", "uses_memory" ]
	pinned   = "exception"
	attr_struct = "store_attr"
	pinned_init = "flags & cons_floats ? op_pin_state_floats : op_pin_state_pinned"
	constructor_args = [
		dict(
			type = "ir_cons_flags",
			name = "flags",
		),
	]
	d_post = '''
	firm_alloc_frag_arr(res, op_Store, &res->attr.store.exc.frag_arr);
	'''

class Sub(Binop):
	flags = []

class SymConst(Op):
	mode       = "mode_P"
	flags      = [ "constlike", "start_block" ]
	knownBlock = True
	pinned     = "no"
	attrs      = [
		dict(
			type = "ir_entity*",
			name = "entity",
			noprop = True
		)
	]
	attr_struct = "symconst_attr"
	java_noconstr = True

class Sync(Op):
	mode     = "mode_M"
	flags    = []
	pinned   = "no"
	optimize = False
	arity    = "dynamic"

class Tuple(Op):
	arity  = "variable"
	mode   = "mode_T"
	pinned = "no"
	flags  = [ "labeled" ]
	java_noconstr = True

class Unknown(Op):
	knownBlock = True
	pinned     = "yes"
	block      = "get_irg_start_block(irg)"
	flags      = [ "cfopcode", "fragile", "start_block", "constlike",
	               "dump_noblock" ]

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
