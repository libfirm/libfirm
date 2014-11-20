# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)
$arch = "TEMPLATE";

#
# Modes
#
$mode_gp  = "mode_Iu"; # mode used by general purpose registers
$mode_fp  = "mode_F";  # mode used by floatingpoint registers

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   arity     => "0|1|2|3 ... |variable|dynamic|any",   # optional
#   state     => "floats|pinned|mem_pinned|exc_pinned", # optional
#   args      => [
#                    { type => "type 1", name => "name 1" },
#                    { type => "type 2", name => "name 2" },
#                    ...
#                  ],
#   comment   => "any comment for constructor",  # optional
#   reg_req   => { in => [ "reg_class|register" ], out => [ "reg_class|register|in_rX" ] },
#   outs      => { "out1", "out2" },# optional, creates pn_op_out1, ... consts
#   ins       => { "in1", "in2" },  # optional, creates n_op_in1, ... consts
#   mode      => "mode_Iu",         # optional, predefines the mode
#   emit      => "emit code with templates",   # optional for virtual nodes
#   attr      => "additional attribute arguments for constructor", # optional
#   init_attr => "emit attribute initialization template",         # optional
#   rd_constructor => "c source code which constructs an ir_node", # optional
#   hash_func => "name of the hash function for this operation",   # optional, get the default hash function else
#   latency   => "latency of this operation (can be float)"        # optional
#   attr_type => "name of the attribute struct",                   # optional
# },
#
# ... # (all nodes you need to describe)
#
# ); # close the %nodes initializer

# state: state of the operation, OPTIONAL (default is "floats")
#
# arity: arity of the operation, MUST NOT BE OMITTED
#
# args:  the OPTIONAL arguments of the node constructor (debug, irg and block
#        are always the first 3 arguments and are always autmatically
#        created)
#        If this key is missing the following arguments will be created:
#        for i = 1 .. arity: ir_node *op_i
#        ir_mode *mode
#
# outs:  if a node defines more than one output, the names of the projections
#        nodes having outs having automatically the mode mode_T
#
# comment: OPTIONAL comment for the node constructor
%reg_classes = (
	gp => [
		{ name => "r0" },
		{ name => "r1" },
		{ name => "r2" },
		{ name => "r3" },
		{ name => "r4" },
		{ name => "r5" },
		{ name => "r6" },
		{ name => "r7" },
		{ name => "r8" },
		{ name => "r9" },
		{ name => "r10" },
		{ name => "r11" },
		{ name => "r12" },
		{ name => "r13" },
		{ name => "sp"  }, # stackpointer
		{ name => "bp"  }, # basepointer
		{ mode => $mode_gp }
	],
	fp => [
		{ name => "f0" },
		{ name => "f1" },
		{ name => "f2" },
		{ name => "f3" },
		{ name => "f4" },
		{ name => "f5" },
		{ name => "f6" },
		{ name => "f7" },
		{ name => "f8" },
		{ name => "f9" },
		{ name => "f10" },
		{ name => "f11" },
		{ name => "f12" },
		{ name => "f13" },
		{ name => "f14" },
		{ name => "f15" },
		{ mode => $mode_fp }
	]
);

$default_attr_type = "TEMPLATE_attr_t";
$default_copy_attr = "TEMPLATE_copy_attr";

%nodes = (

# Integer nodes

Add => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = add %S0, %S1',
	mode      => $mode_gp,
},

Mul => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = mul %S0, %S1',
	mode      => $mode_gp,
},

And => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = and %S0, %S1',
	mode      => $mode_gp,
},

Or => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = or %S0, %S1',
	mode      => $mode_gp,
},

Xor => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = xor %S0, %S1',
	mode      => $mode_gp,
},

Sub => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = sub %S0, %S1',
	mode      => $mode_gp,
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '%D0 = shl %S0, %S1',
	mode      => $mode_gp,
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1" ] },
	emit      => '%D0 = shr %S1',
	mode      => $mode_gp,
},

Minus => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	emit      => '%D0 = neg %S0',
	mode      => $mode_gp,
},

Not => {
	arity   => 1,
	remat   => 1,
	reg_req => { in => [ "gp" ], out => [ "gp" ] },
	emit    => '%D0 = not %S0',
	mode    => $mode_gp,
},

Const => {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	attr       => "ir_tarval *value",
	custominit => "set_TEMPLATE_value(res, value);",
	reg_req    => { out => [ "gp" ] },
	emit       => '%D0 = const %I',
	mode       => $mode_gp,
},

Address => {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	attr       => "ir_entity *entity",
	custominit => "set_TEMPLATE_entity(res, entity);",
	reg_req    => { out => [ "gp" ] },
	emit       => '%D0 = address of %E',
	mode       => $mode_gp,
},

# Control Flow

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	reg_req   => { in => [], out => [ "sp:I|S", "r0", "r1", "r2", "r3", "none" ] },
	outs      => [ "stack", "arg0", "arg1", "arg2", "arg3", "M" ],
	ins       => [],
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	# This is fixed to 1 result in a gp register
	ins      => [ "result", "stack", "mem" ],
	outs     => [ "X" ],
	reg_req  => { in => [ "gp", "sp", "none", ], out => [ "none" ] },
	mode     => "mode_X",
},

# Load / Store

Load => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	outs      => [ "res", "M" ],
	emit      => '%D0 = load (%S0)',
},

Store => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	outs      => [ "M" ],
	mode      => "mode_M",
	emit      => 'store %S0 -> (%S1)',
},

# Floating Point operations

fAdd => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '%D0 = fadd %S0, %S1',
	mode      => $mode_fp,
},

fMul => {
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '%D0 = fmul %S0, %S1',
	mode      => $mode_fp,
},

fSub => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '%D0 = fsub %S0, %S1',
	mode      => $mode_fp,
},

fDiv => {
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '%D0 = fdiv %S0, %S1',
	mode      => $mode_fp,
},

fMinus => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	emit      => '%D0 = fneg %S0',
	mode      => $mode_fp,
},

fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	emit      => '%D0 = fconst %I',
	mode      => $mode_fp,
},

# Load / Store

fLoad => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "fp", "none" ] },
	outs      => [ "res", "M" ],
	emit      => '%D0 = fload (%S0)',
},

fStore => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "fp", "none" ], out => [ "none" ] },
	outs      => [ "M" ],
	mode      => "mode_M",
	emit      => 'fstore %S0 -> (%S1)',
},

);
