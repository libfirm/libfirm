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
	ins   = [ "mem", "size" ]
	outs  = [ "M", "X_regular", "X_except", "res" ]
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
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Alloc, &res->attr.alloc.exc.frag_arr);
	#endif
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
	attr_struct   = "asm_attr"
	pinned        = "memory"
	pinned_init   = "op_pin_state_pinned"
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
	]
	java_noconstr = True

class Bad(Op):
	mode       = "mode_Bad"
	flags      = [ "cfopcode", "fragile", "start_block", "dump_noblock" ]
	pinned     = "yes"
	knownBlock = True
	singleton  = True
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
	'''

	d_pre = '''
	int i;
	int has_unknown = 0;
	'''

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
 	outs   = [ "M", "X_regular", "X_except", "res" ]
 	flags  = [ "fragile", "highlevel" ]
	pinned = "exception"
	pinned_init = "op_pin_state_pinned"
	attr_struct = "bound_attr"
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Bound, &res->attr.bound.exc.frag_arr);
	#endif
	'''

class Break(Op):
	mode   = "mode_X"
	flags  = [ "cfopcode" ]
	pinned = "yes"

class Builtin(Op):
	ins      = [ "mem" ]
	arity    = "variable"
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ]
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
	outs     = [ "M_regular", "X_regular", "X_except", "T_result", "M_except", "P_value_res_base" ]
	flags    = [ "fragile", "uses_memory" ]
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	]
	attr_struct = "call_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	init = '''
	assert((get_unknown_type() == type) || is_Method_type(type));
	'''
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Call, &res->attr.call.exc.frag_arr);
	#endif
	'''

class CallBegin(Op):
	ins   = [ "ptr" ]
	outs  = [ "" ] # TODO
	flags         = [ "cfopcode", "ip_cfopcode" ]
	pinned        = "yes"
	# TODO: attribute with call...
	attr_struct   = "callbegin_attr"
	attrs         = [
		dict(
			type = "ir_node*",
			name = "call"
		)
	]
	java_noconstr = True

class Carry(Binop):
	flags = [ "commutative" ]

class Cast(Op):
	ins      = [ "op" ]
	mode     = "get_irn_mode(irn_op)"
	flags    = [ "highlevel" ]
	pinned   = "no"
	attrs    = [
		dict(
			type = "ir_type*",
			name = "type"
		)
	]
	attr_struct = "cast_attr"
	init     = "assert(is_atomic_type(type));"

class Cmp(Binop):
	outs  = [ "False", "Eq", "Lt", "Le", "Gt", "Ge", "Lg", "Leg", "Uo", "Ue", "Ul", "Ule", "Ug", "Uge", "Ne", "True" ]
	flags = []

class Cond(Op):
	ins      = [ "selector" ]
	outs     = [ "false", "true" ]
	flags    = [ "cfopcode", "forking" ]
	pinned   = "yes"
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

class CopyB(Op):
	ins   = [ "mem", "dst", "src" ]
	outs  = [ "M", "X_regular", "X_except" ]
	flags = [ "fragile", "highlevel", "uses_memory" ]
	attrs = [
		dict(
			name = "type",
			type = "ir_type*"
		)
	]
	attr_struct = "copyb_attr"
	pinned      = "memory"
	pinned_init = "op_pin_state_pinned"
	d_post = '''
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_CopyB, &res->attr.copyb.exc.frag_arr);
	#endif
	'''

class Div(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [ "M", "X_regular", "X_except", "res" ]
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
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Div, &res->attr.except.frag_arr);
	#endif
	'''

class DivMod(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [ "M", "X_regular", "X_except", "res_div", "res_mod" ]
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
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_DivMod, &res->attr.except.frag_arr);
	#endif
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

class EndExcept(Op):
	mode      = "mode_X"
	pinned    = "yes"
	arity     = "dynamic"
	flags     = [ "cfopcode", "ip_cfopcode" ]
	singleton = True

class EndReg(Op):
	mode      = "mode_X"
	pinned    = "yes"
	arity     = "dynamic"
	flags     = [ "cfopcode", "ip_cfopcode" ]
	singleton = True

class Eor(Binop):
	flags    = [ "commutative" ]

class Filter(Op):
	ins   = [ "pred" ]
	flags = []
	attrs = [
		dict(
			name = "proj",
			type = "long"
		)
	]
	pinned      = "yes"
	attr_struct = "filter_attr"
	java_noconstr = True

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
	outs  = [ "M", "X_regular", "X_except", "res", "M_except" ]
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
	outs     = [ "M", "X_regular", "X_except", "res" ]
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
#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Load, &res->attr.load.exc.frag_arr);
#endif
	'''

class Minus(Unop):
	flags = []

class Mod(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [ "M", "X_regular", "X_except", "res" ]
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
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Mod, &res->attr.except.frag_arr);
	#endif
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
	pinned      = "yes"
	arity       = "variable"
	flags       = []
	attr_struct = "phi_attr"
	custom_is   = True
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
	ins      = [ "pred" ]
	flags    = []
	pinned   = "no"
	attrs    = [
		dict(
			type = "long",
			name = "proj",
			initname = ""
		)
	]
	attr_struct = "long"
	custom_is   = True

class Quot(Op):
	ins   = [ "mem", "left", "right" ]
	outs  = [ "M", "X_regular", "X_except", "res" ]
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
	#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Quot, &res->attr.except.frag_arr);
	#endif
	'''

class Raise(Op):
	ins    = [ "mem", "exo_ptr" ]
	outs   = [ "M", "X" ]
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
	mode       = "mode_T"
	pinned     = "yes"
	flags      = [ "cfopcode" ]
	singleton  = True

class Store(Op):
	ins      = [ "mem", "ptr", "value" ]
	outs     = [ "M", "X_regular", "X_except" ]
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
#if PRECISE_EXC_CONTEXT
	firm_alloc_frag_arr(res, op_Store, &res->attr.store.exc.frag_arr);
#endif
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
			name = "entity"
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
