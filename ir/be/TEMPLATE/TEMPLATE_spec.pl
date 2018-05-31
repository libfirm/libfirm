# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)
$arch = "TEMPLATE";

# Modes
$mode_gp = "mode_Iu"; # mode used by general purpose registers
$mode_fp = "mode_F";  # mode used by floatingpoint registers

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   state     => "floats|pinned|mem_pinned|exc_pinned", # optional, default floats
#   comment   => "any comment for constructor",  # optional
#   in_reqs   => [ "reg_class|register" ] | "...",
#   out_reqs  => [ "reg_class|register|in_rX" ] | "...",
#   ins       => { "in1", "in2" },  # optional, creates n_op_in1, ... consts
#   outs      => { "out1", "out2" },# optional, creates pn_op_out1, ... consts
#   mode      => "first" | "<mode>" # optional, determines the mode, auto-detected by default
#   emit      => "emit code with templates",   # optional for virtual nodes
#   attr      => "additional attribute arguments for constructor", # optional
#   init      => "emit attribute initialization template",         # optional
#   hash_func => "name of the hash function for this operation",   # optional, get the default hash function else
#   attr_type => "name of the attribute struct",                   # optional
# },
#
# ... # (all nodes you need to describe)
#
# );

%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
			{ name => "r0"  },
			{ name => "r1"  },
			{ name => "r2"  },
			{ name => "r3"  },
			{ name => "r4"  },
			{ name => "r5"  },
			{ name => "r6"  },
			{ name => "r7"  },
			{ name => "r8"  },
			{ name => "r9"  },
			{ name => "r10" },
			{ name => "r11" },
			{ name => "r12" },
			{ name => "r13" },
			{ name => "sp"  }, # stackpointer
			{ name => "bp"  }, # basepointer
		]
	},
	fp => {
		mode => $mode_fp,
		registers => [
			{ name => "f0"  },
			{ name => "f1"  },
			{ name => "f2"  },
			{ name => "f3"  },
			{ name => "f4"  },
			{ name => "f5"  },
			{ name => "f6"  },
			{ name => "f7"  },
			{ name => "f8"  },
			{ name => "f9"  },
			{ name => "f10" },
			{ name => "f11" },
			{ name => "f12" },
			{ name => "f13" },
			{ name => "f14" },
			{ name => "f15" },
		]
	}
);

%init_attr = (
	TEMPLATE_attr_t => ""
);

my $binop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp" ],
	emit      => '%D0 = {name} %S0, %S1',
};

my $constop = {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	out_reqs   => [ "gp" ],
};

my $fbinop = {
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "fp" ],
	emit      => '%D0 = {name} %S0, %S1',
};

my $unop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	emit      => '%D0 = {name} %S0',
};

%nodes = (

# Integer nodes

Add => { template => $binop },

Mul => { template => $binop },

And => { template => $binop },

Or => { template => $binop },

Xor => { template => $binop },

Sub => { template => $binop },

Shl => { template => $binop },

Shr => { template => $binop },

Minus => { template => $unop },

Not => { template => $unop },

Const => {
	template => $constop,
	attr     => "ir_entity *entity, ir_tarval *value",
	init     => "set_TEMPLATE_value(res, entity, value);",
	emit     => '%D0 = const %I',
},

# Control Flow

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump", "fallthrough" ],
	out_reqs  => [ "exec" ],
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	out_reqs => [ "exec" ],
	ins      => [ "mem", "stack", "first_result" ],
	outs     => [ "X" ],
},

# Load / Store

Load => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "mem", "gp" ],
	out_reqs  => [ "gp", "mem" ],
	ins       => [ "mem", "ptr" ],
	outs      => [ "res", "M" ],
	emit      => '%D0 = load (%S1)',
},

Store => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "mem", "gp", "gp" ],
	out_reqs  => [ "mem" ],
	ins       => [ "mem", "ptr", "val" ],
	outs      => [ "M" ],
	emit      => '(%S1) = store %S2',
},

# Floating Point operations

fAdd => {
	template  => $fbinop,
	irn_flags => [ "rematerializable" ],
},

fMul => { template => $fbinop },

fSub => {
	template  => $fbinop,
	irn_flags => [ "rematerializable" ],
},

fDiv => { template => $fbinop },

fMinus => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp" ],
	out_reqs  => [ "fp" ],
	emit      => '%D0 = fneg %S0',
},

fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "fp" ],
	emit      => '%D0 = fconst %I',
},

# Load / Store

fLoad => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "mem", "gp" ],
	out_reqs  => [ "fp", "mem" ],
	ins       => [ "mem", "ptr" ],
	outs      => [ "res", "M" ],
	emit      => '%D0 = fload (%S1)',
},

fStore => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "mem", "gp", "fp" ],
	out_reqs  => [ "mem" ],
	ins       => [ "mem", "ptr", "val" ],
	outs      => [ "M" ],
	emit      => '(%S1) = fstore %S2',
},

);
