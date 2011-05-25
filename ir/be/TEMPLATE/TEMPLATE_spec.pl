# Creation: 2006/02/13
# $Id$

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "TEMPLATE";

#
# Modes
#
$mode_gp  = "mode_Iu"; # mode used by general purpose registers
$mode_fp  = "mode_E";  # mode used by floatingpoint registers

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
#   cmp_attr  => "c source code for comparing node attributes", # optional
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
#
# register types:
#   0 - no special type
#   1 - caller save (register must be saved by the caller of a function)
#   2 - callee save (register must be saved by the called function)
#   4 - ignore (do not assign this register)
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "r0", type => 1 },
		{ name => "r1", type => 1 },
		{ name => "r2", type => 1 },
		{ name => "r3", type => 1 },
		{ name => "r4", type => 1 },
		{ name => "r5", type => 1 },
		{ name => "r6", type => 1 },
		{ name => "r7", type => 2 },
		{ name => "r8", type => 2 },
		{ name => "r9", type => 2 },
		{ name => "r10", type => 2 },
		{ name => "r11", type => 2 },
		{ name => "r12", type => 2 },
		{ name => "r13", type => 2 },
		{ name => "sp", realname => "r14", type => 4 },  # stackpointer
		{ name => "bp", realname => "r15", type => 4 },  # basepointer
		{ mode => $mode_gp }
	],
	fp => [
		{ name => "f0", type => 1 },
		{ name => "f1", type => 1 },
		{ name => "f2", type => 1 },
		{ name => "f3", type => 1 },
		{ name => "f4", type => 1 },
		{ name => "f5", type => 1 },
		{ name => "f6", type => 1 },
		{ name => "f7", type => 1 },
		{ name => "f8", type => 1 },
		{ name => "f9", type => 1 },
		{ name => "f10", type => 1 },
		{ name => "f11", type => 1 },
		{ name => "f12", type => 1 },
		{ name => "f13", type => 1 },
		{ name => "f14", type => 1 },
		{ name => "f15", type => 1 },
		{ mode => $mode_fp }
	]
);

%emit_templates = (
	S1 => "${arch}_emit_source_register(node, 0);",
	S2 => "${arch}_emit_source_register(node, 1);",
	S3 => "${arch}_emit_source_register(node, 2);",
	S4 => "${arch}_emit_source_register(node, 3);",
	S5 => "${arch}_emit_source_register(node, 4);",
	S6 => "${arch}_emit_source_register(node, 5);",
	D1 => "${arch}_emit_dest_register(node, 0);",
	D2 => "${arch}_emit_dest_register(node, 1);",
	D3 => "${arch}_emit_dest_register(node, 2);",
	D4 => "${arch}_emit_dest_register(node, 3);",
	D5 => "${arch}_emit_dest_register(node, 4);",
	D6 => "${arch}_emit_dest_register(node, 5);",
	C  => "${arch}_emit_immediate(node);"
);

$default_attr_type = "TEMPLATE_attr_t";
$default_copy_attr = "TEMPLATE_copy_attr";

%nodes = (

# Integer nodes

Add => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. add %S1, %S2, %D1',
	mode      => $mode_gp,
},

Mul => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      =>'. mul %S1, %S2, %D1',
	mode      => $mode_gp,
},

And => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. and %S1, %S2, %D1',
	mode      => $mode_gp,
},

Or => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. or %S1, %S2, %D1',
	mode      => $mode_gp,
},

Xor => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. xor %S1, %S2, %D1',
	mode      => $mode_gp,
},

Sub => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. sub %S1, %S2, %D1',
	mode      => $mode_gp,
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. shl %S1, %S2, %D1',
	mode      => $mode_gp,
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1" ] },
	emit      => '. shr %S2, %D1',
	mode      => $mode_gp,
},

Minus => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	emit      => '. neg %S1, %D1',
	mode      => $mode_gp,
},

Not => {
	arity   => 1,
	remat   => 1,
	reg_req => { in => [ "gp" ], out => [ "gp" ] },
	emit    => '. not %S1, %D1',
	mode    => $mode_gp,
},

Const => {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	attr       => "ir_tarval *value",
	custominit => "set_TEMPLATE_value(res, value);",
	reg_req    => { out => [ "gp" ] },
	emit       => '. mov %C, %D1',
	cmp_attr   =>
'
	/* TODO: compare Const attributes */
    return 1;
',
	mode    => $mode_gp,
},

# Control Flow

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

# Load / Store

Load => {
	op_flags  => [ "labeled" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp" ] },
	emit      => '. mov (%S1), %D1',
},

Store => {
	op_flags  => [ "labeled" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ] },
	emit      => '. movl %S2, (%S1)',
},

# Floating Point operations

fAdd => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '. fadd %S1, %S2, %D1',
	mode    => $mode_fp,
},

fMul => {
	op_flags  => [ "commutative" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      =>'. fmul %S1, %S2, %D1',
	mode      => $mode_fp,
},

fSub => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '. fsub %S1, %S2, %D1',
	mode      => $mode_fp,
},

fDiv => {
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '. fdiv %S1, %S2, %D1',
	mode      => $mode_fp,
},

fMinus => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	emit      => '. fneg %S1, %D1',
	mode      => $mode_fp,
},

fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	emit      => '. fmov %C, %D1',
	cmp_attr  =>
'
	/* TODO: compare fConst attributes */
	return 1;
',
	mode      => $mode_fp,
},

# Load / Store

fLoad => {
	op_flags  => [ "labeled" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "fp" ] },
	emit      => '. fmov (%S1), %D1',
},

fStore => {
	op_flags  => [ "labeled" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "fp", "none" ] },
	emit      => '. fmov %S2, (%S1)',
},

);
