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
#   state     => "floats|pinned|mem_pinned|exc_pinned", # optional
#   comment   => "any comment for constructor",  # optional
#   in_reqs   => [ "reg_class|register" ] | "...",
#   out_reqs  => [ "reg_class|register|in_rX" ] | "...",
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

my $binop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp" ],
	mode      => $mode_gp,
};

my $constop = {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	out_reqs   => [ "gp" ],
	mode       => $mode_gp,
};

my $fbinop = {
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "fp" ],
	mode      => $mode_fp,
};

my $unop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	mode      => $mode_gp,
};

%nodes = (

# Integer nodes

Add => {
	template => $binop,
	emit     => '%D0 = add %S0, %S1',
},

Mul => {
	template => $binop,
	emit     => '%D0 = mul %S0, %S1',
},

And => {
	template => $binop,
	emit     => '%D0 = and %S0, %S1',
},

Or => {
	template => $binop,
	emit     => '%D0 = or %S0, %S1',
},

Xor => {
	template => $binop,
	emit     => '%D0 = xor %S0, %S1',
},

Sub => {
	template => $binop,
	emit     => '%D0 = sub %S0, %S1',
},

Shl => {
	template => $binop,
	emit     => '%D0 = shl %S0, %S1',
},

Shr => {
	template => $binop,
	emit     => '%D0 = shr %S0, %S1',
},

Minus => {
	template => $unop,
	emit     => '%D0 = neg %S0',
},

Not => {
	template => $unop,
	emit     => '%D0 = not %S0',
},

Const => {
	template   => $constop,
	attr       => "ir_tarval *value",
	custominit => "set_TEMPLATE_value(res, value);",
	emit       => '%D0 = const %I',
},

Address => {
	template   => $constop,
	attr       => "ir_entity *entity",
	custominit => "set_TEMPLATE_entity(res, entity);",
	emit       => '%D0 = address of %E',
},

# Control Flow

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	out_reqs  => [ "none" ],
	mode      => "mode_X",
},

Start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	out_reqs  => [ "sp:I", "r0", "r1", "r2", "r3", "none" ],
	outs      => [ "stack", "arg0", "arg1", "arg2", "arg3", "M" ],
	ins       => [],
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	out_reqs => [ "none" ],
	ins      => [ "mem", "stack", "first_result" ],
	outs     => [ "X" ],
	mode     => "mode_X",
},

# Load / Store

Load => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "none", "gp" ],
	out_reqs  => [ "gp", "none" ],
	ins       => [ "mem", "ptr" ],
	outs      => [ "res", "M" ],
	emit      => '%D0 = load (%S1)',
},

Store => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "none", "gp", "gp" ],
	out_reqs  => [ "none" ],
	ins       => [ "mem", "ptr", "val" ],
	outs      => [ "M" ],
	mode      => "mode_M",
	emit      => '(%S1) = store %S2',
},

# Floating Point operations

fAdd => {
	template  => $fbinop,
	irn_flags => [ "rematerializable" ],
	emit      => '%D0 = fadd %S0, %S1',
},

fMul => {
	template => $fbinop,
	emit     => '%D0 = fmul %S0, %S1',
},

fSub => {
	template  => $fbinop,
	irn_flags => [ "rematerializable" ],
	emit      => '%D0 = fsub %S0, %S1',
},

fDiv => {
	template => $fbinop,
	emit     => '%D0 = fdiv %S0, %S1',
},

fMinus => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp" ],
	out_reqs  => [ "fp" ],
	emit      => '%D0 = fneg %S0',
	mode      => $mode_fp,
},

fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "fp" ],
	emit      => '%D0 = fconst %I',
	mode      => $mode_fp,
},

# Load / Store

fLoad => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "none", "gp" ],
	out_reqs  => [ "fp", "none" ],
	ins       => [ "mem", "ptr" ],
	outs      => [ "res", "M" ],
	emit      => '%D0 = fload (%S1)',
},

fStore => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "none", "gp", "fp" ],
	out_reqs  => [ "none" ],
	ins       => [ "mem", "ptr", "val" ],
	outs      => [ "M" ],
	mode      => "mode_M",
	emit      => '(%S1) = fstore %S2',
},

);
