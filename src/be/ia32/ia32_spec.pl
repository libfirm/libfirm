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

$mode_xmm   = "ia32_mode_float64";
$mode_fp87  = "x86_mode_E";
$mode_gp    = "ia32_mode_gp";
$mode_flags = "ia32_mode_flags";
$mode_fpcw  = "ia32_mode_fpcw";

%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
			{ name => "edx", encoding => 2, dwarf => 2 },
			{ name => "ecx", encoding => 1, dwarf => 1 },
			{ name => "eax", encoding => 0, dwarf => 0 },
			{ name => "ebx", encoding => 3, dwarf => 3 },
			{ name => "esi", encoding => 6, dwarf => 6 },
			{ name => "edi", encoding => 7, dwarf => 7 },
			{ name => "ebp", encoding => 5, dwarf => 5 },
			{ name => "esp", encoding => 4, dwarf => 4 },
			{ name => "gp_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		]
	},
	xmm => {
		mode => $mode_xmm,
		registers => [
			{ name => "xmm0", dwarf => 21 },
			{ name => "xmm1", dwarf => 22 },
			{ name => "xmm2", dwarf => 23 },
			{ name => "xmm3", dwarf => 24 },
			{ name => "xmm4", dwarf => 25 },
			{ name => "xmm5", dwarf => 26 },
			{ name => "xmm6", dwarf => 27 },
			{ name => "xmm7", dwarf => 28 },
			{ name => "xmm_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		]
	},
	fp => {
		flags => "allow_clobber_input",
		mode => $mode_fp87,
		registers => [
			{ name => "st0", realname => "st",    encoding => 0, dwarf => 11 },
			{ name => "st1", realname => "st(1)", encoding => 1, dwarf => 12 },
			{ name => "st2", realname => "st(2)", encoding => 2, dwarf => 13 },
			{ name => "st3", realname => "st(3)", encoding => 3, dwarf => 14 },
			{ name => "st4", realname => "st(4)", encoding => 4, dwarf => 15 },
			{ name => "st5", realname => "st(5)", encoding => 5, dwarf => 16 },
			{ name => "st6", realname => "st(6)", encoding => 6, dwarf => 17 },
			{ name => "st7", realname => "st(7)", encoding => 7, dwarf => 18 },
			{ name => "fp_NOREG", type => "virtual" }, # we need a dummy register for NoReg nodes
		]
	},
	fpcw => { # the floating point control word
		flags => "manual_ra",
		mode => $mode_fpcw,
		registers => [ { name => "fpcw", dwarf => 37 }, ]
	},
	flags => {
		flags => "manual_ra",
		mode => "ia32_mode_flags",
		registers => [ { name => "eflags", dwarf => 9 }, ]
	},
);

sub ia32_custom_init_attr {
	my ($constr, $node, $name) = @_;

	my $res = "";
	my $am  = $node->{am};
	if (defined($am)) {
		if ($am eq "source,unary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_unary);";
		} elsif ($am eq "source,binary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_binary);";
		} elsif ($am eq "none") {
			# nothing to do
		} else {
			die("Invalid address mode '$am' specified on op $name");
		}
		if ($am ne "none" && $node->{state} ne "exc_pinned" && $node->{state} ne "pinned") {
			die("AM nodes must have pinned or AM pinned state ($name)");
		}
	}
	return $res;
}
$custom_init_attr_func = \&ia32_custom_init_attr;

%init_attr = (
	ia32_attr_t =>
		"init_ia32_attributes(res, size);",
	ia32_call_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_call_attributes(res, pop, n_reg_results);",
	ia32_condcode_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_condcode_attributes(res, condition_code);",
	ia32_switch_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_switch_attributes(res, switch_table, table_entity);",
	ia32_copyb_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_copyb_attributes(res, size);",
	ia32_immediate_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_immediate_attributes(res, imm);",
	ia32_x87_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_x87_attributes(res);",
	ia32_return_attr_t =>
		"init_ia32_attributes(res, size);\n".
		"\tinit_ia32_return_attributes(res, pop);",
);

my $x87sim = "ia32_request_x87_sim(irg);";

my $binop_commutative = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	constructors => {
		""     => {
			in_reqs  => [ "gp", "gp", "mem", "gp", "gp" ],
			out_reqs => [ "in_r3 in_r4", "flags", "mem" ],
		},
		"8bit" => {
			in_reqs  => [ "gp", "gp", "mem", "eax ebx ecx edx", "eax ebx ecx edx" ],
			out_reqs => [ "eax ebx ecx edx in_r3 in_r4", "flags", "mem" ],
		},
	},
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	attr      => "x86_insn_size_t size",
	am        => "source,binary",
	mode      => "first",
	emit      => "{name}%M %B",
};

my $binop_flags = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "mem", "gp", "gp" ], },
		"8bit" => { in_reqs => [ "gp", "gp", "mem", "eax ebx ecx edx", "eax ebx ecx edx" ], }
	},
	out_reqs  => [ "flags", "none", "mem" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	attr      => "x86_insn_size_t size, bool ins_permuted",
	init      => "attr->ins_permuted = ins_permuted;",
	mode      => "first",
	emit      => "{name}%M %B",
};

my $binop_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "mem", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "mem", "eax ebx ecx edx" ] },
	},
	out_reqs  => [ "none", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "unused", "flags", "M" ],
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %S3, %AM",
};

my $shiftop = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	constructors => {
		""     => { in_reqs => [ "gp",              "ecx" ] },
		"8bit" => { in_reqs => [ "eax ebx ecx edx", "ecx" ] },
	},
	out_reqs  => [ "in_r0 !in_r1", "flags" ],
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	mode      => "first",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %<,S1 %D0",
};

my $shiftop_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "ecx" ],
	out_reqs  => [ "none", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %<,S3 %AM",
};

my $shiftop_double = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp", "ecx" ],
	constructors => {
		""  => { out_reqs  => [ "in_r0 !in_r1 !in_r2", "flags" ] },
		# With an immediate shift amount we can swap between ShlD/ShrD and negate
		# the shift amount, if the output gets the same register as the second
		# input.
		imm => { out_reqs  => [ "in_r0 in_r1",         "flags" ] },
	},
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	mode      => "first",
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "{name}%M %<S2, %S1, %D0",
};

my $divop = {
	op_flags  => [ "fragile" ],
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "gp", "eax", "edx" ],
	out_reqs  => [ "eax", "flags", "mem", "edx", "exec", "exec" ],
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %AS3",
};

my $mulop = {
	# we should not rematerialize these nodes. They produce 2 results and have
	# very strict constraints
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "eax", "gp" ],
	out_reqs  => [ "eax", "flags", "mem", "edx" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %AS4",
};

my $unop = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "in_r0", "flags" ],
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	attr      => "x86_insn_size_t size",
	mode      => "first",
	emit      => "{name}%M %D0",
};

my $unop_no_flags = {
	# no flags modified
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "in_r0" ],
	ins       => [ "val" ],
	outs      => [ "res" ],
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %D0",
};

my $unop_from_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "gp" ],
	out_reqs  => [ "gp", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "operand" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,unary",
	mode      => "first",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %AS3, %D0",
};

my $unop_mem = {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem" ],
	out_reqs  => [ "none", "flags", "mem" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused", "flags", "M" ],
	attr      => "x86_insn_size_t size",
	emit      => "{name}%M %AM",
};

my $memop = {
	state    => "pinned",
	in_reqs  => [ "mem" ],
	out_reqs => [ "mem" ],
	ins      => [ "mem" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
};

my $prefetchop = {
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem" ],
	out_reqs  => [ "mem" ],
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_8;",
	emit      => "{name} %AM",
};

my $fbinop = {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "fp", "fp", "cls-fpcw" ],
	out_reqs  => [ "fp", "none", "mem" ],
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	mode      => "first",
	attr_type => "ia32_x87_attr_t",
	attr      => "x86_insn_size_t size",
};

my $funop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp" ],
	out_reqs  => [ "fp" ],
	ins       => [ "value" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_80;",
	emit      => "{name}",
};

my $fconstop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "fp" ],
	outs      => [ "res" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_80;\n"
	            ."\t".$x87sim,
	emit      => "{name}",
};

my $valueop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "gp" ],
	outs      => [ "res" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
};

my $fpopop = {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	attr_type   => "ia32_x87_attr_t",
	attr        => "const arch_register_t *reg",
	init        => "attr->x87.reg = reg;",
	fixed       => "x86_insn_size_t const size = X86_SIZE_80;",
	emit        => "{name} %F0",
};

my $xbinop = {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "xmm", "xmm" ],
	out_reqs  => [ "in_r3 !in_r4", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	mode      => "first",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%FX %B",
};

my $xbinop_commutative = {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "xmm", "xmm" ],
	out_reqs  => [ "in_r3 in_r4", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	mode      => "first",
	attr      => "x86_insn_size_t size",
	emit      => "{name}%FX %B",
};

my $xconv_i2f = {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "gp" ],
	out_reqs => [ "xmm" ],
	ins      => [ "base", "index", "mem", "val" ],
	attr     => "x86_insn_size_t size",
	am       => "source,unary",
	emit     => "{name} %AS3, %D0",
};

my $xshiftop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "xmm", "xmm" ],
	out_reqs  => [ "in_r0 !in_r1" ],
	attr      => "x86_insn_size_t size",
	emit      => "{name} %S1, %D0",
};

my $xvalueop = {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "xmm" ],
	outs      => [ "res" ],
	attr      => "x86_insn_size_t size",
};

my $carry_user_op = {
	irn_flags => [ "modify_flags" ],
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
	attr      => "x86_insn_size_t size",
};

my $noregop = {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
};

my $fpcwop = {
	state    => "pinned",
	fixed    => "x86_insn_size_t const size = X86_SIZE_16;",
	emit     => "{name} %AM",
};

my $emmsop = {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	emit        => "{name}",
	fixed       => "x86_insn_size_t const size = X86_SIZE_32;"
	              ."\t".$x87sim,
};

my $loadop = {
	op_flags => [ "fragile" ],
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem" ],
	ins      => [ "base", "index", "mem" ],
};

my $storeop = {
	op_flags => [ "fragile" ],
	state    => "exc_pinned",
	out_reqs => [ "mem", "exec", "exec" ],
	outs     => [ "M", "X_regular", "X_except" ],
	attr     => "x86_insn_size_t size",
};

my $fucomop = {
# we can't allow to rematerialize this node so we don't
# accidently produce Phi(Fucom, Fucom(ins_permuted))
#	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "eax" ],
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	attr_type => "ia32_x87_attr_t",
	attr      => "bool ins_permuted",
	fixed     => "x86_insn_size_t const size = X86_SIZE_16;",
	init      => "attr->attr.ins_permuted = ins_permuted;",
};

%nodes = (

Immediate => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	out_reqs  => [ "gp_NOREG:I" ],
	attr      => "const x86_imm32_t *imm",
	attr_type => "ia32_immediate_attr_t",
	hash_func => "ia32_hash_Immediate",
	latency   => 0,
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
},

Add => {
	template => $binop_commutative,
	encode   => "ia32_enc_binop(node, 0)",
	latency  => 1,
},

AddMem => {
	template => $binop_mem,
	name     => "add",
	encode   => "ia32_enc_binop_mem(node, 0)",
	latency  => 1,
},

Adc => {
	template => $carry_user_op,
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "gp", "gp", "flags" ],
	out_reqs => [ "in_r3 in_r4", "flags", "mem" ],
	ins      => [ "base", "index", "mem", "left", "right", "eflags" ],
	outs     => [ "res", "flags", "M" ],
	emit     => "adc%M %B",
	encode   => "ia32_enc_binop(node, 2)",
	am       => "source,binary",
	latency  => 1,
	mode     => "first",
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
	encode   => "ia32_enc_unop(node, 0xF7, 4, n_ia32_Mul_right)",
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
	encode   => "ia32_enc_0f_unop_reg(node, 0xAF, n_ia32_IMul_right)",
	latency  => 5,
},

IMulImm => {
	template => $binop_commutative,
	constructors => {
		"" => {
			in_reqs  => [ "gp", "gp", "mem", "gp", "gp" ],
			out_reqs => [ "gp", "flags", "mem" ],
		}
	},
	emit     => "imul%M %S4, %AS3, %D0",
	latency  => 5,
},

IMul1OP => {
	template => $mulop,
	name     => "imul",
	encode   => "ia32_enc_unop(node, 0xF7, 5, n_ia32_IMul1OP_right)",
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
	encode   => "ia32_enc_binop(node, 4)",
	latency  => 1,
},

AndMem => {
	template => $binop_mem,
	name     => "and",
	encode   => "ia32_enc_binop_mem(node, 4)",
	latency  => 1,
},

Or => {
	template => $binop_commutative,
	encode   => "ia32_enc_binop(node, 1)",
	latency  => 1,
},

OrMem => {
	template => $binop_mem,
	name     => "or",
	encode   => "ia32_enc_binop_mem(node, 1)",
	latency  => 1,
},

Xor => {
	template => $binop_commutative,
	encode   => "ia32_enc_binop(node, 6)",
	latency  => 1,
},

Xor0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "modify_flags", "rematerializable" ],
	out_reqs  => [ "gp", "flags" ],
	outs      => [ "res", "flags" ],
	emit      => "xor%M %D0, %D0",
	attr      => "x86_insn_size_t size",
	latency   => 1,
	mode      => "first",
},

XorMem => {
	template => $binop_mem,
	name     => "xor",
	encode   => "ia32_enc_binop_mem(node, 6)",
	latency  => 1,
},

Sub => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	constructors => {
		""     => {
			in_reqs  => [ "gp", "gp", "mem", "gp", "gp" ],
			out_reqs => [ "in_r3", "flags", "mem" ],
		},
		"8bit" => {
			in_reqs  => [ "gp", "gp", "mem", "eax ebx ecx edx", "eax ebx ecx edx" ],
			out_reqs => [ "eax ebx ecx edx in_r3", "flags", "mem" ],
		},
	},
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	attr      => "x86_insn_size_t size",
	emit      => "sub%M %B",
	encode    => "ia32_enc_binop(node, 5)",
	latency   => 1,
	mode      => "first",
},

SubMem => {
	template => $binop_mem,
	name     => "sub",
	encode   => "ia32_enc_binop_mem(node, 5)",
	latency  => 1,
},

Sbb => {
	template => $carry_user_op,
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "gp", "gp", "flags" ],
	out_reqs => [ "in_r3", "flags", "mem" ],
	ins      => [ "base", "index", "mem", "minuend", "subtrahend", "eflags" ],
	outs     => [ "res", "flags", "M" ],
	am       => "source,binary",
	emit     => "sbb%M %B",
	encode   => "ia32_enc_binop(node, 3)",
	latency  => 1,
	mode     => "first",
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
	mode     => "first",
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
	encode   => "ia32_enc_unop(node, 0xF7, 7, n_ia32_IDiv_divisor)",
	latency  => 25,
},

Div => {
	template => $divop,
	encode   => "ia32_enc_unop(node, 0xF7, 6, n_ia32_Div_divisor)",
	latency  => 25,
},

Shl => {
	template => $shiftop,
	encode   => "ia32_enc_shiftop(node, 4)",
	latency  => 1,
},

ShlMem => {
	template => $shiftop_mem,
	name     => "shl",
	encode   => "ia32_enc_shiftop_mem(node, 4)",
	latency  => 1,
},

ShlD => {
	template => $shiftop_double,
	latency  => 6,
},

Shr => {
	template => $shiftop,
	encode   => "ia32_enc_shiftop(node, 5)",
	latency  => 1,
},

ShrMem => {
	template => $shiftop_mem,
	name     => "shr",
	encode   => "ia32_enc_shiftop_mem(node, 5)",
	latency  => 1,
},

ShrD => {
	template => $shiftop_double,
	latency  => 6,
},

Sar => {
	template => $shiftop,
	encode   => "ia32_enc_shiftop(node, 7)",
	latency  => 1,
},

SarMem => {
	template => $shiftop_mem,
	name     => "sar",
	encode   => "ia32_enc_shiftop_mem(node, 7)",
	latency  => 1,
},

Ror => {
	template => $shiftop,
	encode   => "ia32_enc_shiftop(node, 1)",
	latency  => 1,
},

RorMem => {
	template => $shiftop_mem,
	name     => "ror",
	encode   => "ia32_enc_shiftop_mem(node, 1)",
	latency  => 1,
},

Rol => {
	template => $shiftop,
	encode   => "ia32_enc_shiftop(node, 0)",
	latency  => 1,
},

RolMem => {
	template => $shiftop_mem,
	name     => "rol",
	encode   => "ia32_enc_shiftop_mem(node, 0)",
	latency  => 1,
},

Neg => {
	template => $unop,
	encode   => "ia32_enc_unop(node, 0xF7, 3, n_ia32_Neg_val)",
	latency  => 1,
},

NegMem => {
	template => $unop_mem,
	name     => "neg",
	encode   => "ia32_enc_unop_mem(node, 0xF6, 3)",
	latency  => 1,
},

Minus64 => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "in_r0", "in_r1" ],
	ins       => [ "low", "high" ],
	outs      => [ "res_low", "res_high" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
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
	latency  => 1,
},

IncMem => {
	template => $unop_mem,
	name     => "inc",
	encode   => "ia32_enc_unop_mem(node, 0xFE, 0)",
	latency  => 1,
},

Dec => {
	template => $unop,
	latency  => 1,
},

DecMem => {
	template => $unop_mem,
	name     => "dec",
	encode   => "ia32_enc_unop_mem(node, 0xFE, 1)",
	latency  => 1,
},

Not => {
	template => $unop_no_flags,
	constructors => {
		""     => { in_reqs => [ "gp" ] },
		"8bit" => { in_reqs => [ "eax ebx ecx edx" ] },
	},
	encode   => "ia32_enc_unop(node, 0xF7, 2, n_ia32_Not_val)",
	latency  => 1,
},

NotMem => {
	template  => $unop_mem,
	name      => "not",
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "none", "none", "mem" ],
	outs      => [ "unused0", "unused1", "M" ],
	encode    => "ia32_enc_unop_mem(node, 0xF6, 2)",
	latency   => 1,
	# no flags modified
},

Cmc => {
	template => $carry_user_op,
	in_reqs  => [ "flags" ],
	out_reqs => [ "flags" ],
	fixed    => "x86_condition_code_t condition_code = x86_cc_carry;\n"
	           ."\tx86_insn_size_t const size = X86_SIZE_32;",
	attr     => undef,
	emit     => "cmc",
	encode   => "ia32_enc_simple(0xF5)",
	latency  => 1,
},

Stc => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	out_reqs  => [ "flags" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "stc",
	encode    => "ia32_enc_simple(0xF9)",
	latency   => 1,
},

Cmp => {
	template => $binop_flags,
	encode   => "ia32_enc_binop(node, 7)",
	latency  => 1,
},

XorHighLow => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "eax ebx ecx edx" ],
	out_reqs  => [ "eax ebx ecx edx in_r0", "flags" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_8;",
	emit      => "xorb %>D0, %<D0",
	ins       => [ "value" ],
	outs      => [ "res", "flags" ],
	latency   => 1,
},

Test => {
	template => $binop_flags,
	latency  => 1,
},

Setcc => {
	#irn_flags => [ "rematerializable" ],
	in_reqs   => [ "flags" ],
	out_reqs  => [ "eax ebx ecx edx" ],
	ins       => [ "eflags" ],
	outs      => [ "res" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	# The way we handle Setcc with float nodes (potentially) destroys the flags
	# (when we emit the setX; setp; orb and the setX;setnp;andb sequences)
	fixed     => "x86_insn_size_t const size = X86_SIZE_8;",
	init      => "if (condition_code & x86_cc_additional_float_cases) {\n".
	             "\t\tarch_add_irn_flags(res, arch_irn_flag_modify_flags);\n".
	             "\t\t/* attr->latency = 3; */\n".
	             "\t}\n",
	latency   => 1,
},

SetccMem => {
	#irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "flags" ],
	out_reqs  => [ "mem" ],
	ins       => [ "base", "index", "mem","eflags" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	fixed     => "x86_insn_size_t const size = X86_SIZE_8;",
	emit      => "set%P3 %AM",
	latency   => 1,
},

CMovcc => {
	#irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	# (note: leave the false,true order intact to make it compatible with other
	#  ia32_binary ops)
	in_reqs   => [ "gp", "gp", "mem", "gp", "gp", "flags" ],
	out_reqs  => [ "in_r3 in_r4", "none", "mem" ],
	ins       => [ "base", "index", "mem", "val_false", "val_true", "eflags" ],
	outs      => [ "res", "unused", "M" ],
	am        => "source,binary",
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_insn_size_t size, x86_condition_code_t condition_code",
	emit      => "cmov%P5 %B",
	latency   => 1,
	mode      => "first",
},

Jcc => {
	state     => "pinned",
	irn_flags => [ "fallthrough" ],
	op_flags  => [ "cfopcode", "forking" ],
	in_reqs   => [ "flags" ],
	out_reqs  => [ "exec", "exec" ],
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
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
	attr      => "const ir_switch_table *switch_table, const ir_entity *table_entity",
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	latency   => 2,
},

Jmp => {
	state     => "pinned",
	irn_flags => [ "simple_jump", "fallthrough" ],
	op_flags  => [ "cfopcode" ],
	out_reqs  => [ "exec" ],
	latency   => 1,
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
},

IJmp => {
	state    => "pinned",
	op_flags => [ "cfopcode", "unknown_jump" ],
	in_reqs  => [ "gp", "gp", "mem", "gp" ],
	out_reqs => [ "exec", "none", "mem" ],
	ins      => [ "base", "index", "mem", "target" ],
	outs     => [ "jmp", "none", "M" ],
	am       => "source,unary",
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "jmp %*AS3",
	# TOOD: No AM when using ia32_enc_unop
	encode   => "ia32_enc_unop(node, 0xFF, 4, n_ia32_IJmp_target)",
	latency  => 1,
	mode     => "first",
},

Const => {
	template  => $valueop,
	emit      => "movl %I, %D0",
	fixed     => undef,
	attr      => "const x86_imm32_t *imm",
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	attr_type => "ia32_immediate_attr_t",
	latency   => 1,
},

GetEIP => {
	template  => $valueop,
	# not rematerializable, value depends on location in code
	irn_flags => [],
	latency   => 5,
},

NoReg_GP => {
	template => $noregop,
	out_reqs => [ "gp_NOREG:I" ],
	latency  => 0,
},

NoReg_FP => {
	template => $noregop,
	out_reqs => [ "fp_NOREG:I" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;\n"
	           ."\t".$x87sim,
	latency  => 0,
},

NoReg_XMM => {
	template => $noregop,
	out_reqs => [ "xmm_NOREG:I" ],
	latency  => 0,
},

ChangeCW => {
	template => $noregop,
	out_reqs => [ "cls-fpcw" ],
	latency  => 3,
},

FldCW => {
	template => $fpcwop,
	in_reqs  => [ "gp", "gp", "mem" ],
	out_reqs => [ "cls-fpcw" ],
	ins      => [ "base", "index", "mem" ],
	latency  => 5,
},

FnstCW => {
	template => $fpcwop,
	in_reqs  => [ "gp", "gp", "mem", "cls-fpcw" ],
	out_reqs => [ "mem" ],
	ins      => [ "base", "index", "mem", "fpcw" ],
	latency  => 5,
},

FnstCWNOP => {
	template => $fpcwop,
	in_reqs  => [ "cls-fpcw" ],
	out_reqs => [ "mem" ],
	ins      => [ "fpcw" ],
	latency  => 0,
	emit     => "",
},

Cltd => {
	# we should not rematerialize this node. It has very strict constraints.
	in_reqs  => [ "eax" ],
	out_reqs => [ "edx" ],
	ins      => [ "val" ],
	emit     => "cltd",
	encode   => "ia32_enc_simple(0x99)",
	latency  => 1,
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	init     => "arch_set_additional_pressure(res, &ia32_reg_classes[CLASS_ia32_gp], 1);",
},

# Load / Store
#
# Note that we add additional latency values depending on address mode, so a
# lateny of 0 for load is correct

Load => {
	template => $loadop,
	out_reqs => [ "gp", "none", "mem", "exec", "exec" ],
	outs     => [ "res", "unused", "M", "X_regular", "X_except" ],
	latency  => 0,
	attr     => "x86_insn_size_t size, bool sign_extend",
	init     => "attr->sign_extend = sign_extend;",
	emit     => "mov%#Ml %AM, %#D0",
},

Store => {
	template => $storeop,
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "mem", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "mem", "eax ebx ecx edx" ] }
	},
	ins      => [ "base", "index", "mem", "val" ],
	emit     => "mov%M %S3, %AM",
	latency  => 2,
},

Lea => {
	# Lea doesn't modify the flags, but setting this seems advantageous since it
	# increases chances that the Lea is transformed back to an Add
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "base", "index" ],
	outs      => [ "res" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "leal %AM, %D0",
	latency   => 2,
},

Push => {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "gp", "esp" ],
	out_reqs => [ "mem", "esp:I" ],
	ins      => [ "base", "index", "mem", "val", "stack" ],
	emit     => "push%M %AS3",
	outs     => [ "M", "stack" ],
	am       => "source,unary",
	latency  => 2,
	attr     => "x86_insn_size_t size",
},

PushEax => {
	in_reqs  => [ "esp" ],
	out_reqs => [ "esp:I" ],
	ins      => [ "stack" ],
	outs     => [ "stack" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "pushl %%eax",
	latency  => 2,
},

Pop => {
	state   => "exc_pinned",
	constructors => {
		""  => { out_reqs => [ "gp",    "none", "mem", "esp:I" ] },
		ebp => { out_reqs => [ "ebp:I", "none", "mem", "esp:I" ] }
	},
	in_reqs => [ "mem", "esp" ],
	ins     => [ "mem", "stack" ],
	outs    => [ "res", "unused", "M", "stack" ],
	emit    => "pop%M %D0",
	attr    => "x86_insn_size_t size",
	latency => 3, # Pop is more expensive than Push on Athlon
},

CopyEbpEsp => {
	in_reqs  => [ "ebp" ],
	out_reqs => [ "esp:I" ],
	ins      => [ "ebp" ],
	outs     => [ "esp" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "movl %S0, %D0",
	latency  => 1,
},

PopMem => {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "esp" ],
	out_reqs => [ "none", "none", "mem", "esp:I" ],
	ins      => [ "base", "index", "mem", "stack" ],
	outs     => [ "unused0", "unused1", "M", "stack" ],
	attr     => "x86_insn_size_t size",
	emit     => "pop%M %AM",
	latency  => 3, # Pop is more expensive than Push on Athlon
},

Enter => {
	in_reqs  => [ "esp" ],
	out_reqs => [ "ebp", "esp:I", "mem" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "enter",
	outs     => [ "frame", "stack", "M" ],
	latency  => 15,
},

Leave => {
	in_reqs  => [ "mem", "ebp" ],
	out_reqs => [ "ebp:I", "mem", "esp:I" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "leave",
	encode   => "ia32_enc_simple(0xC9)",
	outs     => [ "frame", "M", "stack" ],
	latency  => 3,
},

AddSP => {
	irn_flags => [ "modify_flags" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "mem", "esp", "gp" ],
	out_reqs  => [ "esp:I", "mem" ],
	ins       => [ "base", "index", "mem", "stack", "amount" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	am        => "source,binary",
	emit      => "addl %B",
	latency   => 1,
	outs      => [ "stack", "M" ],
},

SubSP => {
	irn_flags => [ "modify_flags" ],
	state     => "pinned",
	in_reqs   => [ "gp", "gp", "mem", "esp", "gp" ],
	out_reqs  => [ "esp:I", "gp", "mem" ],
	ins       => [ "base", "index", "mem", "stack", "amount" ],
	am        => "source,binary",
	fixed     => "x86_insn_size_t size = X86_SIZE_32;",
	emit      => "subl %B\n".
	             "movl %%esp, %D1",
	latency   => 2,
	outs      => [ "stack", "addr", "M" ],
},

LdTls => {
	template => $valueop,
	emit     => "movl %%gs:0, %D0",
	latency  => 1,
},

# BT supports source address mode, but this is unused yet
Bt => {
	# only CF is set, but the other flags are undefined
	irn_flags => [ "modify_flags", "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "flags" ],
	ins       => [ "left", "right" ],
	attr      => "x86_insn_size_t size",
	emit      => "bt%M %S1, %S0",
	latency   => 1,
},

Bsf => {
	template => $unop_from_mem,
	encode   => "ia32_enc_0f_unop_reg(node, 0xBC, n_ia32_Bsf_operand)",
	latency  => 1,
},

Bsr => {
	template => $unop_from_mem,
	encode   => "ia32_enc_0f_unop_reg(node, 0xBD, n_ia32_Bsr_operand)",
	latency  => 1,
},

# SSE4.2 or SSE4a popcnt instruction
Popcnt => {
	template => $unop_from_mem,
	latency  => 1,
},

Ret => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	in_reqs   => "...",
	out_reqs  => [ "exec" ],
	ins       => [ "mem", "stack", "first_result" ],
	attr_type => "ia32_return_attr_t",
	attr      => "uint16_t pop",
	latency   => 0,
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
},

Call => {
	op_flags  => [ "uses_memory" ],
	irn_flags => [ "modify_flags" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => "...",
	ins       => [ "base", "index", "mem", "callee", "stack", "first_argument" ],
	outs      => [ "mem", "stack", "first_result" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "call %*AS3",
	attr_type => "ia32_call_attr_t",
	attr      => "uint8_t pop, uint8_t n_reg_results",
	am        => "source,unary",
	latency   => 4, # random number
},

Bswap => {
	template => $unop_no_flags,
	latency  => 1,
},

Bswap16 => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "eax ebx ecx edx" ],
	out_reqs  => [ "eax ebx ecx edx in_r0" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_8;",
	emit      => "xchg %<D0, %>D0",
	ins       => [ "val" ],
	outs      => [ "res" ],
	latency   => 1,
},

CmpXChgMem => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "eax", "gp" ],
	out_reqs  => [ "eax", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "old", "new" ],
	outs      => [ "res", "flags", "M" ],
	attr      => "x86_insn_size_t size",
	emit      => "lock cmpxchg%M %S4, %AM",
	latency   => 2,
},

Breakpoint => {
	template => $memop,
	latency  => 0,
	emit     => "int3",
	encode   => "ia32_enc_simple(0xCC)",
},

# Undefined Instruction on ALL x86 CPUs
UD2 => {
	template => $memop,
	latency  => 0,
	emit     => "ud2",
},

Outport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	in_reqs   => [ "edx", "eax", "mem" ],
	out_reqs  => [ "mem" ],
	ins       => [ "port", "value", "mem" ],
	attr      => "x86_insn_size_t size",
	emit      => "out%M %S1, %^S0",
	latency   => 1,
},

Inport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	in_reqs   => [ "edx", "mem" ],
	out_reqs  => [ "eax", "mem" ],
	ins       => [ "port", "mem" ],
	outs      => [ "res", "M" ],
	attr      => "x86_insn_size_t size",
	emit      => "in%M %^S0, %D0",
	latency   => 1,
},

# Intel style prefetching
PrefetchT0 => {
	template => $prefetchop,
	latency  => 0,
},

PrefetchT1 => {
	template => $prefetchop,
	latency  => 0,
},

PrefetchT2 => {
	template => $prefetchop,
	latency  => 0,
},

PrefetchNTA => {
	template => $prefetchop,
	latency  => 0,
},

# 3DNow! prefetch instructions
Prefetch => {
	template => $prefetchop,
	latency  => 0,
},

PrefetchW => {
	template => $prefetchop,
	latency  => 0,
},

# produces a 0/+0.0
xZero => {
	template => $xvalueop,
	emit     => "xorp%FX %D0, %D0",
	latency  => 3,
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
Pslld => {
	template => $xshiftop,
	latency  => 3,
},

# integer shift left, qword
Psllq => {
	template => $xshiftop,
	latency  => 3,
},

# integer shift right, dword
Psrld => {
	template => $xshiftop,
	latency  => 1,
},

# mov from integer to SSE register
Movd  => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "xmm" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "movd %S0, %D0",
	latency   => 1,
},

Adds => {
	template => $xbinop_commutative,
	latency  => 4,
},

Muls => {
	template => $xbinop_commutative,
	latency  => 4,
},

Maxs => {
	template => $xbinop_commutative,
	latency  => 2,
},

Mins => {
	template => $xbinop_commutative,
	latency  => 2,
},

Andp => {
	template => $xbinop_commutative,
	latency  => 3,
},

Orp => {
	template => $xbinop_commutative,
	latency  => 3,
},

Xorp => {
	template => $xbinop_commutative,
	latency  => 3,
},

Andnp => {
	template => $xbinop,
	latency  => 3,
},

Subs => {
	template  => $xbinop,
	out_reqs  => [ "in_r3", "flags", "mem" ],
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	latency   => 4,
},

Divs => {
	template => $xbinop,
	am       => "source,binary",
	latency  => 16,
	mode     => "mode_T"
},

Ucomis => {
	irn_flags => [ "modify_flags", "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => [ "gp", "gp", "mem", "xmm", "xmm" ],
	out_reqs  => [ "flags" ],
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "flags" ],
	am        => "source,binary",
	attr      => "bool ins_permuted",
	init      => "attr->ins_permuted = ins_permuted;",
	fixed     => "x86_insn_size_t const size = X86_SIZE_32;",
	emit      => "ucomis%FX %B",
	latency   => 3,
},

xLoad => {
	template => $loadop,
	out_reqs => [ "xmm", "none", "mem", "exec", "exec" ],
	outs     => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit     => "movs%FX %AM, %D0",
	attr     => "x86_insn_size_t size",
	latency  => 0,
},

xStore => {
	template => $storeop,
	in_reqs  => [ "gp", "gp", "mem", "xmm" ],
	ins      => [ "base", "index", "mem", "val" ],
	emit     => "movs%FX %S3, %AM",
	latency  => 0,
},

CvtSI2SS => {
	template => $xconv_i2f,
	latency  => 2,
},

CvtSI2SD => {
	template => $xconv_i2f,
	latency  => 2,
},

l_LLtoFloat => {
	ins       => [ "val_high", "val_low" ],
	attr_type => "",
	dump_func => "NULL",
},

l_FloattoLL => {
	ins       => [ "val" ],
	outs      => [ "res_high", "res_low" ],
	attr_type => "",
	dump_func => "NULL",
},

CopyB => {
	in_reqs   => [ "edi", "esi", "ecx", "mem" ],
	out_reqs  => [ "edi", "esi", "ecx", "mem" ],
	ins       => [ "dest", "source", "count", "mem" ],
	outs      => [ "dest", "source", "count", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 250,
},

CopyB_i => {
	in_reqs   => [ "edi", "esi", "mem" ],
	out_reqs  => [ "edi", "esi", "mem" ],
	ins       => [ "dest", "source", "mem" ],
	outs      => [ "dest", "source", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 3,
},

Cwtl => {
	in_reqs  => [ "eax" ],
	out_reqs => [ "eax" ],
	ins      => [ "val" ],
	outs     => [ "res" ],
	fixed    => "x86_insn_size_t const size = X86_SIZE_32;",
	emit     => "cwtl",
	encode   => "ia32_enc_simple(0x98)",
	latency  => 1,
},

Conv_I2I => {
	op_flags => [ "fragile" ],
	state    => "exc_pinned",
	constructors => {
		""     => { in_reqs => [ "gp", "gp", "mem", "gp" ] },
		"8bit" => { in_reqs => [ "gp", "gp", "mem", "eax ebx ecx edx" ] }
	},
	out_reqs => [ "gp", "none", "mem", "exec", "exec" ],
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit     => "mov%#Ml %AS3, %#D0",
	am       => "source,unary",
	latency  => 1,
	attr     => "x86_insn_size_t size, bool sign_extend",
	init     => "attr->sign_extend = sign_extend;",
	mode     => "first",
},

Conv_I2FP => {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "gp" ],
	out_reqs => [ "xmm", "mem" ],
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	latency  => 10,
	attr     => "x86_insn_size_t size",
	mode     => "first",
},

Conv_FP2I => {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "xmm" ],
	out_reqs => [ "gp", "mem" ],
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	latency  => 10,
	attr     => "x86_insn_size_t size",
	mode     => "first",
},

Conv_FP2FP => {
	state    => "exc_pinned",
	in_reqs  => [ "gp", "gp", "mem", "xmm" ],
	out_reqs => [ "xmm", "mem" ],
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	latency  => 8,
	attr     => "x86_insn_size_t size",
	mode     => "first",
},

# rematerialisation disabled for all float nodes for now, because the fpcw
# handler runs before spilling and we might end up with wrong fpcw then

fadd => {
	template => $fbinop,
	emit     => "fadd%FP%FM %AF",
	encode   => "ia32_enc_fbinop(node, 0, 0)",
	latency  => 4,
},

fmul => {
	template => $fbinop,
	emit     => "fmul%FP%FM %AF",
	encode   => "ia32_enc_fbinop(node, 1, 1)",
	latency  => 4,
},

fsub => {
	template => $fbinop,
	emit     => "fsub%FR%FP%FM %AF",
	encode   => "ia32_enc_fbinop(node, 4, 5)",
	latency  => 4,
},

fdiv => {
	template => $fbinop,
	emit     => "fdiv%FR%FP%FM %AF",
	encode   => "ia32_enc_fbinop(node, 6, 7)",
	latency  => 20,
	mode     => "mode_T"
},

fabs => {
	template => $funop,
	encode   => "ia32_enc_fsimple(0xE1)",
	latency  => 2,
},

fchs => {
	template => $funop,
	encode   => "ia32_enc_fsimple(0xE0)",
	latency  => 2,
},

fld => {
	template  => $loadop,
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "fp", "none", "mem", "exec", "exec" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => "fld%FM %AM",
	attr      => "x86_insn_size_t size",
	fixed     => $x87sim,
	latency   => 2,
},

fst => {
	template  => $storeop,
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "mem", "fp" ],
	ins       => [ "base", "index", "mem", "val" ],
	emit      => "fst%FP%FM %AM",
	latency   => 2,
	attr_type => "ia32_x87_attr_t",
},

fstp => {
	template  => $storeop,
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "mem", "fp:K" ],
	ins       => [ "base", "index", "mem", "val" ],
	emit      => "fstp%FM %AM",
	latency   => 2,
	attr_type => "ia32_x87_attr_t",
},

fild => {
	template  => $loadop,
	out_reqs  => [ "fp", "none", "mem", "exec", "exec" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	attr      => "x86_insn_size_t size",
	emit      => "fild%FI %AM",
	fixed     => $x87sim,
	latency   => 4,
},

fist => {
	template  => $storeop,
	in_reqs   => [ "gp", "gp", "mem", "fp", "cls-fpcw" ],
	ins       => [ "base", "index", "mem", "val", "fpcw" ],
	emit      => "fist%FP%FI %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

fistp => {
	template  => $storeop,
	in_reqs   => [ "gp", "gp", "mem", "fp:K", "cls-fpcw" ],
	ins       => [ "base", "index", "mem", "val", "fpcw" ],
	emit      => "fistp%FI %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

# SSE3 fisttp instruction
fisttp => {
	template  => $storeop,
	in_reqs   => [ "gp", "gp", "mem", "fp:K" ],
	ins       => [ "base", "index", "mem", "val" ],
	emit      => "fisttp%FI %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

fldz => {
	template => $fconstop,
	encode   => "ia32_enc_fsimple(0xEE)",
	latency  => 4,
},

fld1 => {
	template => $fconstop,
	encode   => "ia32_enc_fsimple(0xE8)",
	latency  => 4,
},

fldpi => {
	template => $fconstop,
	latency  => 4,
},

fldln2 => {
	template => $fconstop,
	latency  => 4,
},

fldlg2 => {
	template => $fconstop,
	latency  => 4,
},

fldl2t => {
	template => $fconstop,
	latency  => 4,
},

fldl2e => {
	template => $fconstop,
	latency  => 4,
},

FucomFnstsw => {
	template => $fucomop,
	emit     => "fucom%FP %F0\n".
	            "fnstsw %%ax",
	latency  => 3,
},

FucomppFnstsw => {
	template => $fucomop,
	emit     => "fucompp\n".
	            "fnstsw %%ax",
	latency  => 3,
},

Fucomi => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fp", "fp" ],
	out_reqs  => [ "flags" ],
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_80;",
	emit      => "fucom%FPi %F0",
	attr      => "bool ins_permuted",
	init      => "attr->attr.ins_permuted = ins_permuted;",
	latency   => 3,
	attr_type => "ia32_x87_attr_t",
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
	init      => "attr->ins_permuted = ins_permuted;",
	fixed     => "x86_insn_size_t const size = X86_SIZE_16;"
	            ."\t".$x87sim,
	latency   => 3,
},

Sahf => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "eax" ],
	out_reqs  => [ "flags" ],
	ins       => [ "val" ],
	outs      => [ "flags" ],
	fixed     => "x86_insn_size_t const size = X86_SIZE_16;",
	emit      => "sahf",
	encode    => "ia32_enc_simple(0x9E)",
	latency   => 1,
},

# fxch, fdup, fpop
# Note that it is NEVER allowed to do CSE on these nodes

fxch => {
	op_flags    => [ "keep" ],
	out_reqs    => [ "none" ],
	attrs_equal => "attrs_equal_false",
	fixed       => "x86_insn_size_t const size = X86_SIZE_80;",
	emit        => "fxch %F0",
	encode      => "ia32_enc_fop_reg(node, 0xD9, 0xC8)",
	attr_type   => "ia32_x87_attr_t",
	attr        => "const arch_register_t *reg",
	init        => "attr->x87.reg = reg;",
	latency     => 1,
},

fdup => {
	in_reqs     => [ "fp" ],
	out_reqs    => [ "fp" ],
	ins         => [ "val" ],
	attrs_equal => "attrs_equal_false",
	fixed       => "x86_insn_size_t const size = X86_SIZE_80;",
	emit        => "fld %F0",
	encode      => "ia32_enc_fop_reg(node, 0xD9, 0xC0)",
	attr_type   => "ia32_x87_attr_t",
	attr        => "const arch_register_t *reg",
	init        => "attr->x87.reg = reg;",
	latency     => 1,
},

fpop => {
	template => $fpopop,
	name     => "fstp",
	encode   => "ia32_enc_fop_reg(node, 0xDD, 0xD8)",
	latency  => 1,
},

ffreep => {
	template => $fpopop,
	encode   => "ia32_enc_fop_reg(node, 0xDF, 0xC0)",
	latency  => 1,
},

emms => {
	template => $emmsop,
	latency  => 3,
},

femms => {
	template => $emmsop,
	latency  => 3,
},

# Spilling and reloading of SSE registers

xxLoad => {
	template => $loadop,
	out_reqs  => [ "xmm", "mem", "exec", "exec" ],
	attr      => "x86_insn_size_t size",
	emit      => "movdqu %D0, %AM",
	outs      => [ "res", "M", "X_regular", "X_except" ],
	latency   => 1,
},

xxStore => {
	template => $storeop,
	in_reqs  => [ "gp", "gp", "mem", "xmm" ],
	ins      => [ "base", "index", "mem", "val" ],
	emit     => "movdqu %S3, %AM",
	latency  => 1,
},

);

# Transform some attributes
foreach my $op (keys(%nodes)) {
	my $node         = $nodes{$op};
	my $op_attr_init = $node->{op_attr_init};

	if (defined($op_attr_init)) {
		$op_attr_init .= "\n\t";
	} else {
		$op_attr_init = "";
	}

	my $latency = $node->{latency};
	if (!defined($latency)) {
		if ($op =~ m/^l_/) {
			$latency = 0;
		} else {
			die("Latency missing for op $op");
		}
	}
	$op_attr_init .= "ia32_init_op(op, $latency);";

	$node->{op_attr_init} = $op_attr_init;
}

print "";
