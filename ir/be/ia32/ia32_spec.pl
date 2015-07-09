# This is the specification for the ia32 assembler Firm-operations

# Note on Proj numbers:
# If possible, Nodes should follow these rules for assigning proj numbers:
# "Normal" instructions:
#  0 => result
#  1 => flags
#  2 => memory
# instructions with destination address mode:
#  0 => unused
#  1 => flags
#  2 => memory

$arch = "ia32";

$mode_xmm           = "ia32_mode_float64";
$mode_fp87          = "ia32_mode_E";
$mode_gp            = "ia32_mode_gp";
$mode_flags         = "ia32_mode_flags";
$mode_fpcw          = "ia32_mode_fpcw";

%reg_classes = (
	gp => [
		{ name => "edx", encoding => 2, dwarf => 2 },
		{ name => "ecx", encoding => 1, dwarf => 1 },
		{ name => "eax", encoding => 0, dwarf => 0 },
		{ name => "ebx", encoding => 3, dwarf => 3 },
		{ name => "esi", encoding => 6, dwarf => 6 },
		{ name => "edi", encoding => 7, dwarf => 7 },
		{ name => "ebp", encoding => 5, dwarf => 5 },
		{ name => "esp", encoding => 4, dwarf => 4 },
		{ name => "gp_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		{ mode => $mode_gp }
	],
	xmm => [
		{ name => "xmm0", dwarf => 21 },
		{ name => "xmm1", dwarf => 22 },
		{ name => "xmm2", dwarf => 23 },
		{ name => "xmm3", dwarf => 24 },
		{ name => "xmm4", dwarf => 25 },
		{ name => "xmm5", dwarf => 26 },
		{ name => "xmm6", dwarf => 27 },
		{ name => "xmm7", dwarf => 28 },
		{ name => "xmm_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		{ mode => $mode_xmm }
	],
	fp => [
		{ name => "st0", realname => "st",    encoding => 0, dwarf => 11 },
		{ name => "st1", realname => "st(1)", encoding => 1, dwarf => 12 },
		{ name => "st2", realname => "st(2)", encoding => 2, dwarf => 13 },
		{ name => "st3", realname => "st(3)", encoding => 3, dwarf => 14 },
		{ name => "st4", realname => "st(4)", encoding => 4, dwarf => 15 },
		{ name => "st5", realname => "st(5)", encoding => 5, dwarf => 16 },
		{ name => "st6", realname => "st(6)", encoding => 6, dwarf => 17 },
		{ name => "st7", realname => "st(7)", encoding => 7, dwarf => 18 },
		{ name => "fp_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		{ mode => $mode_fp87 }
	],
	fp_cw => [	# the floating point control word
		{ name => "fpcw", dwarf => 37 },
		{ mode => $mode_fpcw, flags => "manual_ra" }
	],
	flags => [
		{ name => "eflags", dwarf => 9 },
		{ mode => "ia32_mode_flags", flags => "manual_ra" }
	],
); # %reg_classes

$default_attr_type = "ia32_attr_t";
$default_copy_attr = "ia32_copy_attr";

sub ia32_custom_init_attr {
	my $constr = shift;
	my $node   = shift;
	my $name   = shift;
	my $res    = "";

	if(defined($node->{am})) {
		my $am = $node->{am};
		if($am eq "source,unary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_unary);";
		} elsif($am eq "source,binary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_binary);";
		} elsif($am eq "none") {
			# nothing to do
		} else {
			die("Invalid address mode '$am' specified on op $name");
		}
		if($am ne "none") {
			if($node->{state} ne "exc_pinned"
					and $node->{state} ne "pinned") {
				die("AM nodes must have pinned or AM pinned state ($name)");
			}
		}
	}
	return $res;
}
$custom_init_attr_func = \&ia32_custom_init_attr;

%init_attr = (
	ia32_attr_t     =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);",
	ia32_call_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_call_attributes(res, pop, call_tp);",
	ia32_condcode_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_condcode_attributes(res, condition_code);",
	ia32_switch_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_switch_attributes(res, switch_table);",
	ia32_copyb_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_copyb_attributes(res, size);",
	ia32_immediate_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_immediate_attributes(res, entity, no_pic_adjust, offset);",
	ia32_x87_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_x87_attributes(res);",
	ia32_climbframe_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_climbframe_attributes(res, count);",
	ia32_return_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_return_attributes(res, pop);",
);

my $x87sim = "ia32_request_x87_sim(irg);";

my $binop_commutative = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp", "gp" ],
	out_reqs  => [ "in_r4 in_r5", "flags", "none" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	mode      => $mode_gp,
};

my $binop_flags = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "none", "gp", "gp" ], },
		"8bit" => { in_reqs => [ "gp", "gp", "none", "eax ebx ecx edx", "eax ebx ecx edx" ], }
	},
	out_reqs  => [ "flags", "none", "none" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	mode      => $mode_flags,
};

my $binop_mem = {
	irn_flags    => [ "modify_flags", "rematerializable" ],
	state        => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "none", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "none", "eax ebx ecx edx" ] },
	},
	out_reqs     => [ "none", "flags", "none" ],
	ins          => [ "base", "index", "mem", "val" ],
	outs         => [ "unused", "flags", "M" ],
};

my $shiftop = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "ecx" ],
	out_reqs  => [ "in_r1 !in_r2", "flags" ],
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	mode      => $mode_gp,
};

my $shiftop_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "ecx" ],
	out_reqs  => [ "none", "flags", "none" ],
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
};

my $shiftop_double = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp", "ecx" ],
	constructors => {
		""    => { out_reqs  => [ "in_r1 !in_r2 !in_r3", "flags" ] },
		# With an immediate shift amount we can swap between ShlD/ShrD and negate
		# the shift amount, if the output gets the same register as the second
		# input.
		"imm" => { out_reqs  => [ "in_r1 in_r2",         "flags" ] },
	},
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	mode      => $mode_gp,
};

my $divop = {
	op_flags  => [ "fragile", "uses_memory" ],
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp", "eax", "edx" ],
	out_reqs  => [ "eax", "flags", "none", "edx", "none", "none" ],
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
};

my $mulop = {
	# we should not rematerialize these nodes. They produce 2 results and have
	# very strict constraints
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "eax", "gp" ],
	out_reqs  => [ "eax", "flags", "none", "edx" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
};

my $unop = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "in_r1", "flags" ],
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	mode      => $mode_gp,
};

my $unop_no_flags = {
	# no flags modified
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "in_r1" ],
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
};

my $unop_from_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp" ],
	out_reqs  => [ "gp", "flags", "none" ],
	ins       => [ "base", "index", "mem", "operand" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,unary",
	mode      => $mode_gp,
};

my $unop_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "none", "flags", "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused", "flags", "M" ],
};

my $memop = {
	state    => "pinned",
	in_reqs  => [ "none" ],
	out_reqs => [ "none" ],
	ins      => [ "mem" ],
	mode     => "mode_M",
};

my $prefetchop = {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
};

my $fbinop = {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "fp", "fp", "fpcw" ],
	out_reqs  => [ "fp", "none", "none" ],
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	mode      => $mode_fp87,
	attr_type => "ia32_x87_attr_t",
};

my $funop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp" ],
	out_reqs  => [ "fp" ],
	ins       => [ "value" ],
	init_attr => "attr->ls_mode = ia32_mode_E;",
	mode      => $mode_fp87,
};

my $fconstop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "fp" ],
	outs      => [ "res" ],
	init_attr => "attr->ls_mode = ia32_mode_E;",
	fixed     => $x87sim,
	mode      => $mode_fp87,
};

my $valueop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "gp" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
};

my $fpopop = {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	attr_type   => "ia32_x87_attr_t",
	mode        => "mode_ANY",
};

my $xbinop = {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm", "xmm" ],
	out_reqs  => [ "in_r4 !in_r5", "flags", "none" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	mode      => $mode_xmm,
};

my $xbinop_commutative = {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm", "xmm" ],
	out_reqs  => [ "in_r4 in_r5", "flags", "none" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	mode      => $mode_xmm,
};

my $xconv_i2f = {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "none", "gp" ],
	out_reqs => [ "xmm" ],
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	mode     => $mode_xmm
};

my $xshiftop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "xmm", "xmm" ],
	out_reqs  => [ "in_r1 !in_r2" ],
	mode      => $mode_xmm,
};

my $xvalueop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "xmm" ],
	outs      => [ "res" ],
	mode      => $mode_xmm,
};

my $carry_user_op = {
	irn_flags => [ "modify_flags" ],
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
};

%nodes = (

Immediate => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "gp_NOREG:I" ],
	attr      => "ir_entity *entity, bool no_pic_adjust, int32_t offset",
	attr_type => "ia32_immediate_attr_t",
	hash_func => "ia32_hash_Immediate",
	latency   => 0,
	mode      => $mode_gp,
},

Add => {
	template => $binop_commutative,
	emit     => "add%M %B",
	latency  => 1,
},

AddMem => {
	template => $binop_mem,
	emit     => "add%M %#S3, %AM",
	latency  => 1,
},

Adc => {
	template => $carry_user_op,
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	out_reqs => [ "in_r4 in_r5", "flags", "none" ],
	ins      => [ "base", "index", "mem", "left", "right", "eflags" ],
	outs     => [ "res", "flags", "M" ],
	emit     => "adc%M %B",
	am       => "source,binary",
	latency  => 1,
	mode     => $mode_gp,
},

l_Add => {
	ins       => [ "left", "right" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

l_Adc => {
	ins       => [ "left", "right", "eflags" ],
	attr_type => "",
	dump_func => "NULL",
},

Mul => {
	template => $mulop,
	emit     => "mul%M %AS4",
	latency  => 10,
},

l_Mul => {
	ins       => [ "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

IMul => {
	template => $binop_commutative,
	emit     => "imul%M %B",
	latency  => 5,
},

IMulImm => {
	template => $binop_commutative,
	out_reqs  => [ "gp", "flags", "none" ],
	emit      => "imul%M %#S4, %#AS3, %#D0",
	latency   => 5,
},

IMul1OP => {
	template => $mulop,
	emit     => "imul%M %AS4",
	latency  => 5,
},

l_IMul => {
	ins       => [ "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

And => {
	template => $binop_commutative,
	emit     => "and%M %B",
	latency  => 1,
},

AndMem => {
	template => $binop_mem,
	emit     => "and%M %#S3, %AM",
	latency  => 1,
},

Or => {
	template => $binop_commutative,
	emit     => "or%M %B",
	latency  => 1,
},

OrMem => {
	template => $binop_mem,
	emit     => "or%M %#S3, %AM",
	latency  => 1,
},

Xor => {
	template => $binop_commutative,
	emit     => "xor%M %B",
	latency  => 1,
},

Xor0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "modify_flags", "rematerializable" ],
	out_reqs  => [ "gp", "flags" ],
	outs      => [ "res", "flags" ],
	emit      => "xor%M %D0, %D0",
	latency   => 1,
	mode      => $mode_gp,
},

XorMem => {
	template => $binop_mem,
	emit     => "xor%M %#S3, %AM",
	latency  => 1,
},

Sub => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp", "gp" ],
	out_reqs  => [ "in_r4", "flags", "none" ],
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "sub%M %B",
	latency   => 1,
	mode      => $mode_gp,
},

SubMem => {
	template => $binop_mem,
	emit     => "sub%M %#S3, %AM",
	latency  => 1,
},

Sbb => {
	template => $carry_user_op,
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	out_reqs => [ "in_r4", "flags", "none" ],
	ins      => [ "base", "index", "mem", "minuend", "subtrahend", "eflags" ],
	outs     => [ "res", "flags", "M" ],
	am       => "source,binary",
	emit     => "sbb%M %B",
	latency  => 1,
	mode     => $mode_gp,
},

Sbb0 => {
	template => $carry_user_op,
	# Spiller currently fails when rematerializing flag consumers
	# irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs  => [ "flags" ],
	out_reqs => [ "gp", "flags" ],
	outs     => [ "res", "flags" ],
	emit     => "sbb%M %D0, %D0",
	latency  => 1,
	mode     => $mode_gp,
},

l_Sub => {
	ins       => [ "minuend", "subtrahend" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

l_Sbb => {
	ins       => [ "minuend", "subtrahend", "eflags" ],
	attr_type => "",
	dump_func => "NULL",
},

IDiv => {
	template => $divop,
	emit     => "idiv%M %AS3",
	latency  => 25,
},

Div => {
	template => $divop,
	emit     => "div%M %AS3",
	latency  => 25,
},

Shl => {
	template => $shiftop,
	emit     => "shl%M %<,S1 %D0",
	latency  => 1,
},

ShlMem => {
	template => $shiftop_mem,
	emit     => "shl%M %<,S3 %AM",
	latency  => 1,
},

ShlD => {
	template => $shiftop_double,
	emit     => "shld%M %<S2, %S1, %D0",
	latency  => 6,
},

Shr => {
	template => $shiftop,
	emit     => "shr%M %<,S1 %D0",
	latency  => 1,
},

ShrMem => {
	template => $shiftop_mem,
	emit     => "shr%M %<,S3 %AM",
	latency  => 1,
},

ShrD => {
	template => $shiftop_double,
	emit     => "shrd%M %<S2, %S1, %D0",
	latency  => 6,
},

Sar => {
	template => $shiftop,
	emit     => "sar%M %<,S1 %D0",
	latency  => 1,
},

SarMem => {
	template => $shiftop_mem,
	emit     => "sar%M %<,S3 %AM",
	latency  => 1,
},

Ror => {
	template => $shiftop,
	emit     => "ror%M %<,S1 %D0",
	latency  => 1,
},

RorMem => {
	template => $shiftop_mem,
	emit     => "ror%M %<,S3 %AM",
	latency  => 1,
},

Rol => {
	template => $shiftop,
	emit     => "rol%M %<,S1 %#D0",
	latency  => 1,
},

RolMem => {
	template => $shiftop_mem,
	emit     => "rol%M %<,S3 %AM",
	latency  => 1,
},

Neg => {
	template => $unop,
	emit     => "neg%M %D0",
	latency  => 1,
},

NegMem => {
	template => $unop_mem,
	emit     => "neg%M %AM",
	latency  => 1,
},

Minus64 => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "in_r1", "in_r2" ],
	ins       => [ "low", "high" ],
	outs      => [ "res_low", "res_high" ],
	latency   => 3,
},

l_Minus64 => {
	ins       => [ "low", "high" ],
	outs      => [ "res_low", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

Inc => {
	template => $unop,
	emit     => "inc%M %D0",
	latency  => 1,
},

IncMem => {
	template => $unop_mem,
	emit     => "inc%M %AM",
	latency  => 1,
},

Dec => {
	template => $unop,
	emit     => "dec%M %D0",
	latency  => 1,
},

DecMem => {
	template => $unop_mem,
	emit     => "dec%M %AM",
	latency  => 1,
},

Not => {
	template => $unop_no_flags,
	emit     => "not%M %D0",
	latency  => 1,
},

NotMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "none", "none", "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused0", "unused1", "M" ],
	emit      => "not%M %AM",
	latency   => 1,
	# no flags modified
},

Cmc => {
	template => $carry_user_op,
	in_reqs  => [ "flags" ],
	out_reqs => [ "flags" ],
	emit     => "cmc",
	latency  => 1,
	mode     => $mode_flags,
},

Stc => {
	irn_flags => [ "modify_flags" ],
	out_reqs  => [ "flags" ],
	emit      => "stc",
	latency   => 1,
	mode      => $mode_flags,
},

Cmp => {
	template => $binop_flags,
	emit     => "cmp%M %B",
	latency  => 1,
},

XorHighLow => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "eax ebx ecx edx" ],
	out_reqs  => [ "in_r1", "flags" ],
	emit      => "xorb %>D0, %<D0",
	ins       => [ "value" ],
	outs      => [ "res", "flags" ],
	latency   => 1,
},

Test => {
	template => $binop_flags,
	emit     => "test%M %B",
	latency  => 1,
},

Setcc => {
	#irn_flags => [ "rematerializable" ],
	in_reqs   => [ "eflags" ],
	out_reqs  => [ "eax ebx ecx edx" ],
	ins       => [ "eflags" ],
	outs      => [ "res" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	# The way we handle Setcc with float nodes (potentially) destroys the flags
	# (when we emit the setX; setp; orb and the setX;setnp;andb sequences)
	init_attr => "set_ia32_ls_mode(res, mode_Bu);\n"
		. "\tif (condition_code & x86_cc_additional_float_cases) {\n"
		. "\t\tarch_add_irn_flags(res, arch_irn_flag_modify_flags);\n"
		. "\t\t/* attr->latency = 3; */\n"
		. "\t}\n",
	latency   => 1,
	mode      => $mode_gp,
},

SetccMem => {
	#irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "eflags" ],
	out_reqs  => [ "none" ],
	ins       => [ "base", "index", "mem","eflags" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	init_attr => "set_ia32_ls_mode(res, mode_Bu);\n",
	emit      => "set%P3 %AM",
	latency   => 1,
	mode      => "mode_M",
},

CMovcc => {
	#irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	# (note: leave the false,true order intact to make it compatible with other
	#  ia32_binary ops)
	in_reqs   => [ "gp", "gp", "none", "gp", "gp", "eflags" ],
	out_reqs  => [ "in_r4 in_r5", "none", "none" ],
	ins       => [ "base", "index", "mem", "val_false", "val_true", "eflags" ],
	outs      => [ "res", "unused", "M" ],
	am        => "source,binary",
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	emit      => "cmov%P5 %#AS4, %#D0",
	latency   => 1,
	mode      => $mode_gp,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	in_reqs   => [ "eflags" ],
	out_reqs  => [ "none", "none" ],
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	latency   => 2,
},

SwitchJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	in_reqs   => [ "gp", "gp" ],
	ins       => [ "base", "index" ],
	out_reqs  => "...",
	attr_type => "ia32_switch_attr_t",
	attr      => "const ir_switch_table *switch_table",
	latency   => 2,
},

Jmp => {
	state     => "pinned",
	irn_flags => [ "simple_jump" ],
	op_flags  => [ "cfopcode" ],
	out_reqs  => [ "none" ],
	latency   => 1,
	mode      => "mode_X",
},

IJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "unknown_jump" ],
	in_reqs   => [ "gp", "gp", "none", "gp" ],
	out_reqs  => [ "none", "none", "none" ],
	ins       => [ "base", "index", "mem", "target" ],
	outs      => [ "jmp", "none", "M" ],
	am        => "source,unary",
	emit      => "jmp %*AS3",
	latency   => 1,
	mode      => "mode_X",
},

Const => {
	template  => $valueop,
	emit      => "movl %I, %D0",
	attr      => "ir_entity *entity, bool no_pic_adjust, int32_t offset",
	attr_type => "ia32_immediate_attr_t",
	latency   => 1,
},

Unknown => {
	template  => $valueop,
	latency   => 0,
	emit      => "",
},

GetEIP => {
	template  => $valueop,
	# not rematerializable, value depends on location in code
	irn_flags => [],
	latency   => 5,
},

NoReg_GP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "gp_NOREG:I" ],
	latency   => 0,
	mode      => $mode_gp
},

NoReg_FP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "fp_NOREG:I" ],
	fixed     => $x87sim,
	mode      => $mode_fp87,
	latency   => 0,
},

NoReg_XMM => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "xmm_NOREG:I" ],
	latency   => 0,
	mode      => $mode_xmm,
},

ChangeCW => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "fpcw" ],
	mode      => $mode_fpcw,
	latency   => 3,
},

FldCW => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "fpcw" ],
	ins       => [ "base", "index", "mem" ],
	latency   => 5,
	emit      => "fldcw %AM",
	mode      => $mode_fpcw,
},

FnstCW => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "none", "fp_cw" ],
	out_reqs  => [ "none" ],
	ins       => [ "base", "index", "mem", "fpcw" ],
	latency   => 5,
	emit      => "fnstcw %AM",
	mode      => "mode_M",
},

FnstCWNOP => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	in_reqs   => [ "fp_cw" ],
	out_reqs  => [ "none" ],
	ins       => [ "fpcw" ],
	latency   => 0,
	emit      => "",
	mode      => "mode_M",
},

Cltd => {
	# we should not rematerialize this node. It has very strict constraints.
	in_reqs   => [ "eax", "edx" ],
	out_reqs  => [ "edx" ],
	ins       => [ "val", "clobbered" ],
	emit      => "cltd",
	latency   => 1,
	mode      => $mode_gp,
},

# Load / Store
#
# Note that we add additional latency values depending on address mode, so a
# lateny of 0 for load is correct

Load => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "gp", "none", "none", "none", "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	latency   => 0,
	emit      => "mov%#Ml %AM, %D0",
},

Store => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "none", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "none", "eax ebx ecx edx" ] }
	},
	out_reqs  => [ "none", "none", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => "mov%M %#S3, %AM",
	latency   => 2,
},

Lea => {
	# Lea doesn't modify the flags, but setting this seems advantageous since it
	# increases chances that the Lea is transformed back to an Add
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "base", "index" ],
	outs      => [ "res" ],
	emit      => "leal %AM, %D0",
	latency   => 2,
	mode      => $mode_gp,
},

Push => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp", "esp" ],
	out_reqs  => [ "none", "esp:I" ],
	ins       => [ "base", "index", "mem", "val", "stack" ],
	emit      => "push%M %AS3",
	outs      => [ "M", "stack" ],
	am        => "source,unary",
	latency   => 2,
	attr      => "ir_mode *store_mode",
	init_attr => "attr->ls_mode = store_mode;",
},

PushEax => {
	state    => "exc_pinned",
	in_reqs  => [ "esp" ],
	out_reqs => [ "esp:I" ],
	ins      => [ "stack" ],
	outs     => [ "stack" ],
	emit     => "pushl %%eax",
	latency  => 2,
	mode     => $mode_gp,
},

Pop => {
	state     => "exc_pinned",
	constructors => {
		"" => {
			out_reqs => [ "gp", "none", "none", "esp:I" ],
		},
		"ebp" => {
			out_reqs => [ "ebp:I", "none", "none", "esp:I" ],
		}
	},
	in_reqs   => [ "none", "esp" ],
	ins       => [ "mem", "stack" ],
	outs      => [ "res", "unused", "M", "stack" ],
	emit      => "pop%M %D0",
	latency   => 3, # Pop is more expensive than Push on Athlon
	init_attr => "attr->ls_mode = ia32_mode_gp;",
},

CopyEbpEsp => {
	state    => "exc_pinned",
	in_reqs  => [ "ebp" ],
	out_reqs => [ "esp:I" ],
	ins      => [ "ebp" ],
	outs     => [ "esp" ],
	emit     => "movl %S0, %D0",
	latency  => 1,
	mode     => $mode_gp,
},

PopMem => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "esp" ],
	out_reqs  => [ "none", "none", "none", "esp:I" ],
	ins       => [ "base", "index", "mem", "stack" ],
	outs      => [ "unused0", "unused1", "M", "stack" ],
	emit      => "pop%M %AM",
	latency   => 3, # Pop is more expensive than Push on Athlon
},

Enter => {
	in_reqs   => [ "esp" ],
	out_reqs  => [ "ebp", "esp:I", "none" ],
	emit      => "enter",
	outs      => [ "frame", "stack", "M" ],
	latency   => 15,
},

Leave => {
	in_reqs  => [ "none", "ebp" ],
	out_reqs => [ "ebp:I", "none", "esp:I" ],
	emit     => "leave",
	outs     => [ "frame", "M", "stack" ],
	latency  => 3,
	state    => "exc_pinned",
},

AddSP => {
	irn_flags => [ "modify_flags" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "none", "esp", "gp" ],
	out_reqs  => [ "esp:I", "none" ],
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => "addl %B",
	latency   => 1,
	outs      => [ "stack", "M" ],
},

SubSP => {
	irn_flags => [ "modify_flags" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "none", "esp", "gp" ],
	out_reqs  => [ "esp:I", "gp", "none" ],
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => "subl %B\n".
	             "movl %%esp, %D1",
	latency   => 2,
	outs      => [ "stack", "addr", "M" ],
},

LdTls => {
	template  => $valueop,
	emit      => "movl %%gs:0, %D0",
	latency   => 1,
},

#
# BT supports source address mode, but this is unused yet
#
Bt => {
	# only CF is set, but the other flags are undefined
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "flags" ],
	ins       => [ "left", "right" ],
	emit      => "bt%M %S1, %S0",
	latency   => 1,
	mode      => $mode_flags,
},

Bsf => {
	template => $unop_from_mem,
	emit     => "bsf%M %AS3, %D0",
	latency  => 1,
},

Bsr => {
	template => $unop_from_mem,
	emit     => "bsr%M %AS3, %D0",
	latency  => 1,
},

#
# SSE4.2 or SSE4a popcnt instruction
#
Popcnt => {
	template => $unop_from_mem,
	emit     => "popcnt%M %AS3, %D0",
	latency  => 1,
},

Start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	out_reqs  => "...",
	ins       => [],
	latency   => 0,
	emit      => "",
},

Return => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	in_reqs   => "...",
	out_reqs  => [ "none" ],
	ins       => [ "mem", "stack", "first_result" ],
	mode      => "mode_X",
	attr_type => "ia32_return_attr_t",
	attr      => "uint16_t pop",
	latency   => 0,
},

Call => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => "...",
	ins       => [ "base", "index", "mem", "callee", "stack", "fpcw", "first_argument" ],
	outs      => [ "mem", "stack", "fpcw", "first_result" ],
	emit      => "call %*AS3",
	attr_type => "ia32_call_attr_t",
	attr      => "unsigned pop, ir_type *call_tp",
	am        => "source,unary",
	latency   => 4, # random number
},

#
# a Helper node for frame-climbing, needed for __builtin_(frame|return)_address
#
# PS: try gcc __builtin_frame_address(100000) :-)
#
ClimbFrame => {
	irn_flags => [ "modify_flags" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "in_r1", "!in_r1" ],
	ins       => [ "frame" ],
	outs      => [ "res", "cnt" ],
	latency   => 4, # random number
	attr_type => "ia32_climbframe_attr_t",
	attr      => "unsigned count",
},

#
# bswap
#
Bswap => {
	template => $unop_no_flags,
	emit     => "bswap%M %D0",
	latency  => 1,
},

#
# bswap16, use xchg here
#
Bswap16 => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "eax ebx ecx edx" ],
	out_reqs  => [ "in_r1" ],
	emit      => "xchg %<D0, %>D0",
	ins       => [ "val" ],
	latency   => 1,
	mode      => $mode_gp,
},

CmpXChgMem => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "eax", "gp" ],
	out_reqs  => [  "eax", "flags", "none" ],
	ins       => [ "base", "index", "mem", "old", "new" ],
	outs      => [ "res", "flags", "M" ],
	emit      => "lock cmpxchg%M %#S4, %AM",
	latency   => 2,
},

#
# BreakPoint
#
Breakpoint => {
	template => $memop,
	latency  => 0,
	emit     => "int3",
},

#
# Undefined Instruction on ALL x86 CPU's
#
UD2 => {
	template => $memop,
	latency  => 0,
	emit     => "ud2",
},

#
# outport
#
Outport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	in_reqs   => [ "edx", "eax", "none" ],
	out_reqs  => [ "none" ],
	ins       => [ "port", "value", "mem" ],
	emit      => "out%M %#S1, %^S0",
	latency   => 1,
	mode      => "mode_M",
},

#
# inport
#
Inport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	in_reqs   => [ "edx", "none" ],
	out_reqs  => [ "eax", "none" ],
	ins       => [ "port", "mem" ],
	outs      => [ "res", "M" ],
	emit      => "in%M %^S0, %#D0",
	latency   => 1,
},

#
# Intel style prefetching
#
Prefetch0 => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetcht0 %AM",
},

Prefetch1 => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetcht1 %AM",
},

Prefetch2 => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetcht2 %AM",
},

PrefetchNTA => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetchnta %AM",
},

#
# 3DNow! prefetch instructions
#
Prefetch => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetch %AM",
},

PrefetchW => {
	template => $prefetchop,
	latency  => 0,
	emit     => "prefetchw %AM",
},

# produces a 0/+0.0
xZero => {
	template => $xvalueop,
	emit     => "xorp%FX %D0, %D0",
	latency  => 3,
},

xUnknown => {
	template => $xvalueop,
	emit     => "",
	latency  => 0,
},

xPzero => {
	template => $xvalueop,
	emit     => "pxor %D0, %D0",
	latency  => 3,
},

# produces all 1 bits
xAllOnes => {
	template => $xvalueop,
	emit     => "pcmpeqb %D0, %D0",
	latency  => 3,
},

# integer shift left, dword
xPslld => {
	template => $xshiftop,
	emit     => "pslld %#S1, %D0",
	latency  => 3,
},

# integer shift left, qword
xPsllq => {
	template => $xshiftop,
	emit     => "psllq %#S1, %D0",
	latency  => 3,
},

# integer shift right, dword
xPsrld => {
	template => $xshiftop,
	emit     => "psrld %#S1, %D0",
	latency  => 1,
},

# mov from integer to SSE register
xMovd  => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "xmm" ],
	emit      => "movd %S0, %D0",
	latency   => 1,
	mode      => $mode_xmm
},

xAdd => {
	template => $xbinop_commutative,
	emit     => "adds%FX %B",
	latency  => 4,
},

xMul => {
	template => $xbinop_commutative,
	emit     => "muls%FX %B",
	latency  => 4,
},

xMax => {
	template => $xbinop_commutative,
	emit     => "maxs%FX %B",
	latency  => 2,
},

xMin => {
	template => $xbinop_commutative,
	emit     => "mins%FX %B",
	latency  => 2,
},

xAnd => {
	template => $xbinop_commutative,
	emit     => "andp%FX %B",
	latency  => 3,
},

xOr => {
	template => $xbinop_commutative,
	emit     => "orp%FX %B",
	latency  => 3,
},

xXor => {
	template => $xbinop_commutative,
	emit     => "xorp%FX %B",
	latency  => 3,
},

xAndNot => {
	template => $xbinop,
	emit     => "andnp%FX %B",
	latency  => 3,
},

xSub => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm", "xmm" ],
	out_reqs  => [ "in_r4", "flags", "none" ],
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "subs%FX %B",
	latency   => 4,
	mode      => $mode_xmm
},

xDiv => {
	template => $xbinop,
	am       => "source,binary",
	emit     => "divs%FX %B",
	latency  => 16,
	mode     => "mode_T"
},

Ucomi => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm", "xmm" ],
	out_reqs  => [ "eflags" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "flags" ],
	am        => "source,binary",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	emit      => "ucomis%FX %B",
	latency   => 3,
	mode      => $mode_flags,
},

xLoad => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "xmm", "none", "none", "none", "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => "movs%FX %AM, %D0",
	attr      => "ir_mode *load_mode",
	init_attr => "attr->ls_mode = load_mode;",
	latency   => 0,
},

xStore => {
	op_flags => [ "uses_memory", "fragile" ],
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "none", "xmm" ],
	out_reqs => [ "none", "none", "none" ],
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => "movs%FX %S3, %AM",
	latency  => 0,
},

CvtSI2SS => {
	template => $xconv_i2f,
	emit     => "cvtsi2ss %AS3, %D0",
	latency  => 2,
},

CvtSI2SD => {
	template => $xconv_i2f,
	emit     => "cvtsi2sd %AS3, %D0",
	latency  => 2,
},


l_LLtoFloat => {
	ins      => [ "val_high", "val_low" ],
	attr_type => "",
	dump_func => "NULL",
},

l_FloattoLL => {
	ins      => [ "val" ],
	outs     => [ "res_high", "res_low" ],
	attr_type => "",
	dump_func => "NULL",
},

CopyB => {
	op_flags  => [ "uses_memory" ],
	in_reqs   => [ "edi", "esi", "ecx", "none" ],
	out_reqs  => [ "edi", "esi", "ecx", "none" ],
	ins       => [ "dest", "source", "count", "mem" ],
	outs      => [ "dest", "source", "count", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 250,
},

CopyB_i => {
	op_flags  => [ "uses_memory" ],
	in_reqs   => [ "edi", "esi", "none" ],
	out_reqs  => [ "edi", "esi", "none" ],
	ins       => [ "dest", "source", "mem" ],
	outs      => [ "dest", "source", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 3,
},

Cwtl => {
	state     => "exc_pinned",
	in_reqs   => [ "eax" ],
	out_reqs  => [ "eax" ],
	ins       => [ "val" ],
	outs      => [ "res" ],
	emit      => "cwtl",
	latency   => 1,
	mode      => $mode_gp,
},

Conv_I2I => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "none", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "none", "eax ebx ecx edx" ] }
	},
	out_reqs  => [ "gp", "none", "none", "none", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => "mov%#Ml %#AS3, %D0",
	am        => "source,unary",
	latency   => 1,
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	mode      => $mode_gp,
},

Conv_I2FP => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "gp" ],
	out_reqs  => [ "xmm", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 10,
	attr      => "ir_mode *tgt_mode",
	init_attr => "attr->ls_mode = tgt_mode;",
	mode      => $mode_xmm,
},

Conv_FP2I => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm" ],
	out_reqs  => [ "gp", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 10,
	attr      => "ir_mode *src_mode",
	init_attr => "attr->ls_mode = src_mode;",
	mode      => $mode_gp,
},

Conv_FP2FP => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "xmm" ],
	out_reqs  => [ "xmm", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 8,
	attr      => "ir_mode *tgt_mode",
	init_attr => "attr->ls_mode = tgt_mode;",
	mode      => $mode_xmm,
},

# rematerialisation disabled for all float nodes for now, because the fpcw
# handler runs before spilling and we might end up with wrong fpcw then

fadd => {
	template => $fbinop,
	emit     => "fadd%FP%FM %AF",
	latency  => 4,
},

fmul => {
	template => $fbinop,
	emit     => "fmul%FP%FM %AF",
	latency  => 4,
},

fsub => {
	template => $fbinop,
	emit     => "fsub%FR%FP%FM %AF",
	latency  => 4,
},

fdiv => {
	template => $fbinop,
	emit     => "fdiv%FR%FP%FM %AF",
	latency  => 20,
	mode     => "mode_T"
},

fabs => {
	template => $funop,
	emit     => "fabs",
	latency  => 2,
},

fchs => {
	template => $funop,
	emit     => "fchs",
	latency  => 2,
},

fld => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "fp", "none", "none", "none", "none" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => "fld%FM %AM",
	attr      => "ir_mode *load_mode",
	init_attr => "attr->ls_mode = load_mode;",
	fixed     => $x87sim,
	latency   => 2,
},

fst => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "fp" ],
	out_reqs  => [ "none", "none", "none" ],
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => "fst%FP%FM %AM",
	attr      => "ir_mode *store_mode",
	init_attr => "attr->attr.ls_mode = store_mode;",
	latency   => 2,
	attr_type => "ia32_x87_attr_t",
},

fild => {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "fp", "none", "none" ],
	outs      => [ "res", "unused", "M" ],
	ins       => [ "base", "index", "mem" ],
	emit      => "fild%FM %AM",
	fixed     => $x87sim,
	latency   => 4,
},

fist => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "fp", "fpcw" ],
	out_reqs  => [ "none", "none", "none" ],
	ins       => [ "base", "index", "mem", "val", "fpcw" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => "fist%FP%FM %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

# SSE3 fisttp instruction
fisttp => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none", "fp" ],
	out_reqs  => [ "none", "none", "none", "in_r4" ],
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except", "res" ],
	emit      => "fisttp%FM %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

fldz => {
	template => $fconstop,
	emit     => "fldz",
	latency  => 4,
},

fld1 => {
	template => $fconstop,
	emit     => "fld1",
	latency  => 4,
},

fldpi => {
	template => $fconstop,
	emit     => "fldpi",
	latency  => 4,
},

fldln2 => {
	template => $fconstop,
	emit     => "fldln2",
	latency  => 4,
},

fldlg2 => {
	template => $fconstop,
	emit     => "fldlg2",
	latency  => 4,
},

fldl2t => {
	template => $fconstop,
	emit     => "fldll2t",
	latency  => 4,
},

fldl2e => {
	template => $fconstop,
	emit     => "fldl2e",
	latency  => 4,
},

FucomFnstsw => {
# we can't allow to rematerialize this node so we don't
#  accidently produce Phi(Fucom, Fucom(ins_permuted))
#	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "eax" ],
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	emit      => "fucom%FP %F0\n".
	             "fnstsw %%ax",
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.ins_permuted = ins_permuted;",
	latency   => 3,
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

FucomppFnstsw => {
# we can't allow to rematerialize this node so we don't
#  accidently produce Phi(Fucom, Fucom(ins_permuted))
#	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "eax" ],
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	emit      => "fucompp\n".
	             "fnstsw %%ax",
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.ins_permuted = ins_permuted;",
	latency   => 3,
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

Fucomi => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "eflags" ],
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	emit      => "fucom%FPi %F0",
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.ins_permuted = ins_permuted;",
	latency   => 3,
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_flags,
},

FtstFnstsw => {
#	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp" ],
	out_reqs  => [ "eax" ],
	ins       => [ "left" ],
	outs      => [ "flags" ],
	emit      => "ftst\n".
	             "fnstsw %%ax",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	fixed     => $x87sim,
	latency   => 3,
	mode      => $mode_gp
},

Sahf => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "eax" ],
	out_reqs  => [ "eflags" ],
	ins       => [ "val" ],
	outs      => [ "flags" ],
	emit      => "sahf",
	latency   => 1,
	mode      => $mode_flags,
},

# fxch, fdup, fpop
# Note that it is NEVER allowed to do CSE on these nodes
# Moreover, note the virtual register requierements!

fxch => {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	emit        => "fxch %F0",
	attr_type   => "ia32_x87_attr_t",
	mode        => "mode_ANY",
	latency     => 1,
},

fdup => {
	in_reqs     => [ "fp" ],
	out_reqs    => [ "fp" ],
	ins         => [ "val" ],
	attrs_equal => "attrs_equal_false",
	emit        => "fld %F0",
	attr_type   => "ia32_x87_attr_t",
	mode        => "get_irn_mode(val)",
	latency     => 1,
},

fpop => {
	template => $fpopop,
	emit     => "fstp %F0",
	latency  => 1,
},

ffreep => {
	template => $fpopop,
	emit     => "ffreep %F0",
	latency  => 1,
},

emms => {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	emit        => "emms",
	fixed       => $x87sim,
	mode        => "mode_ANY",
	latency     => 3,
},

femms => {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	emit        => "femms",
	fixed       => $x87sim,
	mode        => "mode_ANY",
	latency     => 3,
},

# Spilling and reloading of SSE registers, hardcoded, not generated #

xxLoad => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "xmm", "none", "none", "none" ],
	emit      => "movdqu %D0, %AM",
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "M", "X_regular", "X_except" ],
	latency   => 1,
},

xxStore => {
	op_flags => [ "uses_memory", "fragile" ],
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "none", "xmm" ],
	out_reqs => [ "none", "none", "none" ],
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => "movdqu %S3, %AM",
	latency  => 1,
},

); # end of %nodes

# Transform some attributes
foreach my $op (keys(%nodes)) {
	my $node         = $nodes{$op};
	my $op_attr_init = $node->{op_attr_init};

	if(defined($op_attr_init)) {
		$op_attr_init .= "\n\t";
	} else {
		$op_attr_init = "";
	}

	if(!defined($node->{latency})) {
		if($op =~ m/^l_/) {
			$node->{latency} = 0;
		} else {
			die("Latency missing for op $op");
		}
	}
	$op_attr_init .= "ia32_init_op(op, ".$node->{latency} . ");";

	$node->{op_attr_init} = $op_attr_init;
}

print "";
