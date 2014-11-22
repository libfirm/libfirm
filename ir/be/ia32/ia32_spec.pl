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

	if(defined($node->{modified_flags})) {
		$res .= "\tarch_add_irn_flags(res, arch_irn_flag_modify_flags);\n";
	}
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
	ia32_asm_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_ia32_asm_attributes(res);",
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

$status_flags       = [ "CF", "PF", "AF", "ZF", "SF", "OF" ];
$status_flags_wo_cf = [       "PF", "AF", "ZF", "SF", "OF" ];
$fpcw_flags         = [ "FP_IM", "FP_DM", "FP_ZM", "FP_OM", "FP_UM", "FP_PM",
                        "FP_PC0", "FP_PC1", "FP_RC0", "FP_RC1", "FP_X" ];

my %binop_flags_constructors = (
	"" => {
	  reg_req => { in => [ "gp", "gp", "none", "gp", "gp" ],
	              out => [ "flags", "none", "none" ] },
	},
	"8bit" => {
	  reg_req => { in => [ "gp", "gp", "none", "eax ebx ecx edx", "eax ebx ecx edx" ],
	              out => [ "flags", "none", "none" ] },
	}
);

my %binop_mem_constructors = (
	""     => { reg_req   => { in => [ "gp", "gp", "none", "gp" ],              out => [ "none", "flags", "none" ] } },
	"8bit" => { reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ], out => [ "none", "flags", "none" ] } },
);

%nodes = (

Immediate => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "gp_NOREG:I" ] },
	attr      => "ir_entity *entity, bool no_pic_adjust, int32_t offset",
	attr_type => "ia32_immediate_attr_t",
	hash_func => "ia32_hash_Immediate",
	latency   => 0,
	mode      => $mode_gp,
},

Asm => {
	arity     => "variable",
	out_arity => "variable",
	attr_type => "ia32_asm_attr_t",
	attr      => "ident *asm_text, const ia32_asm_reg_t *register_map",
	init_attr => "attr->asm_text = asm_text;\n".
	             "\tattr->register_map = register_map;\n",
	latency   => 10,
	modified_flags => $status_flags,
},

# "allocates" a free register
ProduceVal => {
	op_flags  => [ "constlike", "cse_neutral" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	emit      => "",
	latency   => 0,
	mode      => $mode_gp,
	attrs_equal => "attrs_equal_false",
},

Add => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in  => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	emit      => "add%M %B",
	am        => "source,binary",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AddMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_mem_constructors,
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "add%M %#S3, %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Adc => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "eflags" ],
	outs      => [ "res", "flags", "M" ],
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
	emit      => "adc%M %B",
	am        => "source,binary",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
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
	# we should not rematerialize this node. It produces 2 results and has
	# very strict constraints
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax", "gp" ],
	               out => [ "eax", "flags", "none", "edx" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	emit      => "mul%M %AS4",
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
	latency   => 10,
	modified_flags => $status_flags
},

l_Mul => {
	ins       => [ "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

IMul => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	# TODO: adjust out requirements for the 3 operand form
	# (no need for should_be_same then)
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ],
		           out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	latency   => 5,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

IMul1OP => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax", "gp" ],
	               out => [ "eax", "flags", "none", "edx" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	emit      => "imul%M %AS4",
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
	latency   => 5,
	modified_flags => $status_flags
},

l_IMul => {
	ins       => [ "left", "right" ],
	outs      => [ "res_low", "flags", "M", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

And => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ],
		           out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "and%M %B",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AndMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_mem_constructors,
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "and%M %#S3, %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Or => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "or%M %B",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

OrMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_mem_constructors,
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "or%M %#S3, %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Xor => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "xor%M %B",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp", "flags" ] },
	outs      => [ "res", "flags" ],
	emit      => "xor%M %D0, %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

XorMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_mem_constructors,
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "xor%M %#S3, %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Sub => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r4", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "sub%M %B",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SubMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_mem_constructors,
	ins       => [ "base", "index", "mem", "subtrahend" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "sub%M %#S3, %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Sbb => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	               out => [ "in_r4", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend", "eflags" ],
	outs      => [ "res", "flags", "M" ],
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
	am        => "source,binary",
	emit      => "sbb%M %B",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sbb0 => {
	# Spiller currently fails when rematerializing flag consumers
	# irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "flags" ], out => [ "gp", "flags" ] },
	outs      => [ "res", "flags" ],
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
	emit      => "sbb%M %D0, %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
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
	op_flags  => [ "fragile", "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "eax", "edx" ],
	               out => [ "eax", "flags", "none", "edx", "none", "none" ] },
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
	emit      => "idiv%M %AS3",
	latency   => 25,
	modified_flags => $status_flags
},

Div => {
	op_flags  => [ "fragile", "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "eax", "edx" ],
	               out => [ "eax", "flags", "none", "edx", "none", "none" ] },
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
	emit      => "div%M %AS3",
	latency   => 25,
	modified_flags => $status_flags
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "shl%M %<,S1 %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

ShlMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "shl%M %<,S3 %AM",
	latency   => 1,
	modified_flags => $status_flags
},

ShlD => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp", "ecx" ],
	               out => [ "in_r1 !in_r2 !in_r3", "flags" ] },
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "shld%M %<S2, %S1, %D0",
	latency   => 6,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "shr%M %<,S1 %D0",
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags
},

ShrMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "shr%M %<,S3 %AM",
	latency   => 1,
	modified_flags => $status_flags
},

ShrD => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp", "ecx" ],
	               out => [ "in_r1 !in_r2 !in_r3", "flags" ] },
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "shrd%M %<S2, %S1, %D0",
	latency   => 6,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sar => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "sar%M %<,S1 %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SarMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "sar%M %<,S3 %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Ror => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "ror%M %<,S1 %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RorMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "ror%M %<,S3 %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Rol => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => "rol%M %<,S1 %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RolMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "rol%M %<,S3 %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Neg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	emit      => "neg%M %D0",
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

NegMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "neg%M %AM",
	latency   => 1,
	modified_flags => $status_flags
},

Minus64 => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1", "in_r2" ] },
	ins       => [ "low", "high" ],
	outs      => [ "res_low", "res_high" ],
	latency   => 3,
	modified_flags => $status_flags
},

l_Minus64 => {
	ins       => [ "low", "high" ],
	outs      => [ "res_low", "res_high" ],
	attr_type => "",
	dump_func => "NULL",
},

Inc => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	emit      => "inc%M %D0",
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

IncMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "inc%M %AM",
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

Dec => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	emit      => "dec%M %D0",
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

DecMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none", "flags", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused", "flags", "M" ],
	emit      => "dec%M %AM",
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

Not => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	emit      => "not%M %D0",
	latency   => 1,
	mode      => $mode_gp,
	# no flags modified
},

NotMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "unused0", "unused1", "M" ],
	emit      => "not%M %AM",
	latency   => 1,
	# no flags modified
},

Cmc => {
	reg_req   => { in => [ "flags" ], out => [ "flags" ] },
	attr_type => "ia32_condcode_attr_t",
	fixed     => "x86_condition_code_t condition_code = x86_cc_carry;",
	emit      => "cmc",
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Stc => {
	reg_req   => { out => [ "flags" ] },
	emit      => "stc",
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Cmp => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_flags_constructors,
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => "cmp%M %B",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

XorHighLow => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "eax ebx ecx edx" ],
	               out => [ "in_r1", "flags" ] },
	emit      => "xorb %>D0, %<D0",
	ins       => [ "value" ],
	outs      => [ "res", "flags" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags,
},

Test => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	constructors => \%binop_flags_constructors,
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => "test%M %B",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Setcc => {
	#irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "eflags" ], out => [ "eax ebx ecx edx" ] },
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
	reg_req   => { in => [ "gp", "gp", "none", "eflags" ], out => [ "none" ] },
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
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp", "eflags" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "val_false", "val_true", "eflags" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	latency   => 1,
	mode      => $mode_gp,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "x86_condition_code_t condition_code",
	latency   => 2,
},

SwitchJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	reg_req   => { in => [ "gp", "gp" ] },
	ins       => [ "base", "index" ],
	out_arity => "variable",
	attr_type => "ia32_switch_attr_t",
	attr      => "const ir_switch_table *switch_table",
	latency   => 2,
},

Jmp => {
	state     => "pinned",
	irn_flags => [ "simple_jump" ],
	op_flags  => [ "cfopcode" ],
	reg_req   => { out => [ "none" ] },
	latency   => 1,
	mode      => "mode_X",
},

IJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "unknown_jump" ],
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "target" ],
	outs      => [ "jmp", "none", "M" ],
	am        => "source,unary",
	emit      => "jmp %*AS3",
	latency   => 1,
	mode      => "mode_X",
},

Const => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	outs      => [ "res" ],
	emit      => "movl %I, %D0",
	attr      => "ir_entity *entity, bool no_pic_adjust, int32_t offset",
	attr_type => "ia32_immediate_attr_t",
	latency   => 1,
	mode      => $mode_gp,
},

Unknown => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	latency   => 0,
	emit      => "",
	mode      => $mode_gp,
},

GetEIP => {
	op_flags => [ "constlike" ],
	reg_req  => { out => [ "gp" ] },
	latency  => 5,
	mode     => $mode_gp,
},

NoReg_GP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "gp_NOREG:I" ] },
	latency   => 0,
	mode      => $mode_gp
},

NoReg_FP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "fp_NOREG:I" ] },
	mode      => $mode_fp87,
	latency   => 0,
},

NoReg_XMM => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "xmm_NOREG:I" ] },
	latency   => 0,
	mode      => $mode_xmm,
},

ChangeCW => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "fpcw" ] },
	mode      => $mode_fpcw,
	latency   => 3,
	modified_flags => $fpcw_flags
},

FldCW => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "fpcw" ] },
	ins       => [ "base", "index", "mem" ],
	latency   => 5,
	emit      => "fldcw %AM",
	mode      => $mode_fpcw,
	modified_flags => $fpcw_flags
},

FnstCW => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp_cw" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "fpcw" ],
	latency   => 5,
	emit      => "fnstcw %AM",
	mode      => "mode_M",
},

FnstCWNOP => {
	op_flags  => [ "uses_memory" ],
	state     => "pinned",
	reg_req   => { in => [ "fp_cw" ], out => [ "none" ] },
	ins       => [ "fpcw" ],
	latency   => 0,
	emit      => "",
	mode      => "mode_M",
},

Cltd => {
	# we should not rematerialize this node. It has very strict constraints.
	reg_req   => { in => [ "eax", "edx" ], out => [ "edx" ] },
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
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "gp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	latency   => 0,
	emit      => "mov%#Ml %AM, %D0",
},

Store => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	constructors => {
		""     => {
			reg_req => { in => [ "gp", "gp", "none", "gp" ],
			            out => [ "none", "none", "none" ] }
		},
		"8bit" => {
			reg_req => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ],
			            out => [ "none", "none", "none" ] }
		}
	},
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => "mov%M %#S3, %AM",
	latency   => 2,
},

Lea => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "base", "index" ],
	outs      => [ "res" ],
	emit      => "leal %AM, %D0",
	latency   => 2,
	mode      => $mode_gp,
# lea doesn't modify the flags, but setting this seems advantageous since it
# increases chances that the Lea is transformed back to an Add
	modified_flags => 1,
},

Push => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "esp" ], out => [ "none", "esp:I|S" ] },
	ins       => [ "base", "index", "mem", "val", "stack" ],
	emit      => "push%M %AS3",
	outs      => [ "M", "stack" ],
	am        => "source,unary",
	latency   => 2,
	attr      => "ir_mode *store_mode",
	init_attr => "attr->ls_mode = store_mode;",
},

PushEax => {
	state   => "exc_pinned",
	reg_req => { in => [ "esp" ], out => [ "esp:I|S" ] },
	ins     => [ "stack" ],
	outs    => [ "stack" ],
	emit    => "pushl %%eax",
	latency => 2,
	mode    => $mode_gp,
},

Pop => {
	state     => "exc_pinned",
	reg_req   => { in => [ "none", "esp" ], out => [ "gp", "none", "none", "esp:I|S" ] },
	ins       => [ "mem", "stack" ],
	outs      => [ "res", "unused", "M", "stack" ],
	emit      => "pop%M %D0",
	latency   => 3, # Pop is more expensive than Push on Athlon
},

PopEbp => {
	state     => "exc_pinned",
	reg_req   => { in => [ "none", "esp" ], out => [ "ebp:I", "none", "none", "esp:I|S" ] },
	ins       => [ "mem", "stack" ],
	outs      => [ "res", "unused", "M", "stack" ],
	emit      => "pop%M %D0",
	init_attr => "attr->ls_mode = ia32_mode_gp;",
	latency   => 3, # Pop is more expensive than Push on Athlon
},

CopyEbpEsp => {
	state     => "exc_pinned",
	reg_req   => { in => [ "ebp" ], out => [ "esp:I|S" ] },
	ins       => [ "ebp" ],
	outs      => [ "esp" ],
	emit      => "movl %S0, %D0",
	latency   => 1,
	mode      => $mode_gp,
},

PopMem => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp" ], out => [ "none", "none", "none", "esp:I|S" ] },
	ins       => [ "base", "index", "mem", "stack" ],
	outs      => [ "unused0", "unused1", "M", "stack" ],
	emit      => "pop%M %AM",
	latency   => 3, # Pop is more expensive than Push on Athlon
},

Enter => {
	reg_req   => { in => [ "esp" ], out => [ "ebp", "esp:I|S", "none" ] },
	emit      => "enter",
	outs      => [ "frame", "stack", "M" ],
	latency   => 15,
},

Leave => {
	reg_req   => { in => [ "none", "ebp" ], out => [ "ebp:I", "none", "esp:I|S" ] },
	emit      => "leave",
	outs      => [ "frame", "M", "stack" ],
	latency   => 3,
	state     => "exc_pinned",
},

AddSP => {
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp", "gp" ], out => [ "esp:I|S", "none" ] },
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => "addl %B",
	latency   => 1,
	outs      => [ "stack", "M" ],
	modified_flags => $status_flags
},

SubSP => {
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp", "gp" ], out => [ "esp:I|S", "gp", "none" ] },
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => "subl %B\n".
	             "movl %%esp, %D1",
	latency   => 2,
	outs      => [ "stack", "addr", "M" ],
	modified_flags => $status_flags
},

LdTls => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	outs      => [ "res" ],
	emit      => "movl %%gs:0, %D0",
	mode      => $mode_gp,
	latency   => 1,
},

#
# BT supports source address mode, but this is unused yet
#
Bt => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp" ], out => [ "flags" ] },
	ins       => [ "left", "right" ],
	emit      => "bt%M %S1, %S0",
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags  # only CF is set, but the other flags are undefined
},

Bsf => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "gp", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "operand" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,unary",
	emit      => "bsf%M %AS3, %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Bsr => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "gp", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "operand" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,unary",
	emit      => "bsr%M %AS3, %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

#
# SSE4.2 or SSE4a popcnt instruction
#
Popcnt => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "gp", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "operand" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,unary",
	emit      => "popcnt%M %AS3, %D0",
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	out_arity => "variable",
	ins       => [],
	latency   => 0,
	emit      => "",
},

Return => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	arity     => "variable",
	reg_req   => { out => [ "none" ] },
	ins       => [ "mem", "stack", "first_result" ],
	mode      => "mode_X",
	attr_type => "ia32_return_attr_t",
	attr      => "uint16_t pop",
	latency   => 0,
},

Call => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	ins       => [ "base", "index", "mem", "callee", "stack", "fpcw", "first_argument" ],
	outs      => [ "mem", "stack", "fpcw", "first_result" ],
	emit      => "call %*AS3",
	attr_type => "ia32_call_attr_t",
	attr      => "unsigned pop, ir_type *call_tp",
	am        => "source,unary",
	latency   => 4, # random number
	modified_flags => $status_flags
},

#
# a Helper node for frame-climbing, needed for __builtin_(frame|return)_address
#
# PS: try gcc __builtin_frame_address(100000) :-)
#
ClimbFrame => {
	reg_req   => { in => [ "gp" ], out => [ "in_r1", "!in_r1" ] },
	ins       => [ "frame" ],
	outs      => [ "res", "cnt" ],
	latency   => 4, # random number
	attr_type => "ia32_climbframe_attr_t",
	attr      => "unsigned count",
	modified_flags => $status_flags
},

#
# bswap
#
Bswap => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	outs      => [ "res" ],
	emit      => "bswap%M %D0",
	ins       => [ "val" ],
	latency   => 1,
	mode      => $mode_gp,
},

#
# bswap16, use xchg here
#
Bswap16 => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "eax ebx ecx edx" ],
	               out => [ "in_r1" ] },
	emit      => "xchg %<D0, %>D0",
	ins       => [ "val" ],
	latency   => 1,
	mode      => $mode_gp,
},

CmpXChgMem => {
	irn_flags      => [ "rematerializable" ],
	state          => "exc_pinned",
	reg_req        => { in  => [ "gp", "gp", "none", "eax", "gp" ],
	                    out => [  "eax", "flags", "none" ] },
	ins            => [ "base", "index", "mem", "old", "new" ],
	outs           => [ "res", "flags", "M" ],
	emit           => "lock cmpxchg%M %#S4, %AM",
	latency        => 2,
	modified_flags => $status_flags
},

#
# BreakPoint
#
Breakpoint => {
	state     => "pinned",
	reg_req   => { in => [ "none" ], out => [ "none" ] },
	ins       => [ "mem" ],
	latency   => 0,
	emit      => "int3",
	mode      => "mode_M",
},

#
# Undefined Instruction on ALL x86 CPU's
#
UD2 => {
	state     => "pinned",
	reg_req   => { in => [ "none" ], out => [ "none" ] },
	ins       => [ "mem" ],
	latency   => 0,
	emit      => "ud2",
	mode      => "mode_M",
},

#
# outport
#
Outport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	reg_req   => { in => [ "edx", "eax", "none" ], out => [ "none" ] },
	ins       => [ "port", "value", "mem" ],
	emit      => "out%M %#S1, %^S0",
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

#
# inport
#
Inport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	reg_req   => { in => [ "edx", "none" ], out => [ "eax", "none" ] },
	ins       => [ "port", "mem" ],
	outs      => [ "res", "M" ],
	emit      => "in%M %^S0, %#D0",
	latency   => 1,
	modified_flags => $status_flags
},

#
# Intel style prefetching
#
Prefetch0 => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetcht0 %AM",
},

Prefetch1 => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetcht1 %AM",
},

Prefetch2 => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetcht2 %AM",
},

PrefetchNTA => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetchnta %AM",
},

#
# 3DNow! prefetch instructions
#
Prefetch => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetch %AM",
},

PrefetchW => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => "prefetchw %AM",
},

# produces a 0/+0.0
xZero => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => "xorp%FX %D0, %D0",
	latency   => 3,
	mode      => $mode_xmm
},

xUnknown => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => "",
	latency   => 0,
	mode      => $mode_xmm
},

xPzero => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => "pxor %D0, %D0",
	latency   => 3,
	mode      => $mode_xmm
},

# produces all 1 bits
xAllOnes => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => "pcmpeqb %D0, %D0",
	latency   => 3,
	mode      => $mode_xmm
},

# integer shift left, dword
xPslld => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => "pslld %#S1, %D0",
	latency   => 3,
	mode      => $mode_xmm
},

# integer shift left, qword
xPsllq => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => "psllq %#S1, %D0",
	latency   => 3,
	mode      => $mode_xmm
},

# integer shift right, dword
xPsrld => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => "psrld %#S1, %D0",
	latency   => 1,
	mode      => $mode_xmm
},

# mov from integer to SSE register
xMovd  => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "xmm" ] },
	emit      => "movd %S0, %D0",
	latency   => 1,
	mode      => $mode_xmm
},

xAdd => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "adds%FX %B",
	latency   => 4,
	mode      => $mode_xmm
},

xMul => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "muls%FX %B",
	latency   => 4,
	mode      => $mode_xmm
},

xMax => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "maxs%FX %B",
	latency   => 2,
	mode      => $mode_xmm
},

xMin => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "mins%FX %B",
	latency   => 2,
	mode      => $mode_xmm
},

xAnd => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "andp%FX %B",
	latency   => 3,
	mode      => $mode_xmm
},

xOr => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "orp%FX %B",
	latency   => 3,
	mode      => $mode_xmm
},

xXor => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "xorp%FX %B",
	latency   => 3,
	mode      => $mode_xmm
},

xAndNot => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 !in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "andnp%FX %B",
	latency   => 3,
	mode      => $mode_xmm
},

xSub => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "subs%FX %B",
	latency   => 4,
	mode      => $mode_xmm
},

xDiv => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "in_r4 !in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "dividend", "divisor" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => "divs%FX %B",
	latency   => 16,
},

Ucomi => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm", "xmm" ],
	               out => [ "eflags" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "flags" ],
	am        => "source,binary",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	emit      => "ucomis%FX %B",
	latency   => 3,
	mode      => $mode_flags,
	modified_flags => 1,
},

xLoad => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "xmm", "none", "none", "none", "none" ] },
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
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit     => "movs%FX %S3, %AM",
	latency  => 0,
},

xStoreSimple => {
	op_flags => [ "uses_memory", "fragile" ],
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => "movs%FX %S3, %AM",
	latency  => 0,
},

CvtSI2SS => {
	state     => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "gp" ], out => [ "xmm" ] },
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	emit     => "cvtsi2ss %AS3, %D0",
	latency  => 2,
	mode     => $mode_xmm
},

CvtSI2SD => {
	state     => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "gp" ], out => [ "xmm" ] },
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	emit     => "cvtsi2sd %AS3, %D0",
	latency  => 2,
	mode     => $mode_xmm
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
	reg_req   => { in => [ "edi", "esi", "ecx", "none" ],
	               out => [ "edi", "esi", "ecx", "none" ] },
	ins       => [ "dest", "source", "count", "mem" ],
	outs      => [ "dest", "source", "count", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 250,
# we don't care about this flag, so no need to mark this node
#	modified_flags => [ "DF" ]
},

CopyB_i => {
	op_flags  => [ "uses_memory" ],
	reg_req   => { in => [ "edi", "esi", "none" ],
	               out => [  "edi", "esi", "none" ] },
	ins       => [ "dest", "source", "mem" ],
	outs      => [ "dest", "source", "M" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	latency   => 3,
# we don't care about this flag, so no need to mark this node
#	modified_flags => [ "DF" ]
},

Cwtl => {
	state     => "exc_pinned",
	reg_req   => { in => [ "eax" ], out => [ "eax" ] },
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
		"" => {
			reg_req => { in => [ "gp", "gp", "none", "gp" ],
			            out => [ "gp", "none", "none", "none", "none" ] }
		},
		"8bit" => {
			reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ],
			              out => [ "gp", "none", "none", "none", "none" ] }
		}
	},
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "flags", "M", "X_regular", "X_except" ],
	emit      => "mov%#Ml %#AS3, %D0",
	am        => "source,unary",
	latency   => 1,
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	mode      => $mode_gp,
},

Conv_I2FP => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "xmm", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 10,
	attr      => "ir_mode *tgt_mode",
	init_attr => "attr->ls_mode = tgt_mode;",
	mode      => $mode_xmm,
},

Conv_FP2I => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm" ], out => [ "gp", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 10,
	attr      => "ir_mode *src_mode",
	init_attr => "attr->ls_mode = src_mode;",
	mode      => $mode_gp,
},

Conv_FP2FP => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm" ], out => [ "xmm", "none" ] },
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
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp", "fp", "fpcw" ],
	               out => [ "fp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	emit      => "fadd%FP%FM %AF",
	am        => "source,binary",
	latency   => 4,
	mode      => $mode_fp87,
	attr_type => "ia32_x87_attr_t",
},

fmul => {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp", "fp", "fpcw" ],
	               out => [ "fp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	emit      => "fmul%FP%FM %AF",
	am        => "source,binary",
	latency   => 4,
	mode      => $mode_fp87,
	attr_type => "ia32_x87_attr_t",
},

fsub => {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp", "fp", "fpcw" ],
	               out => [ "fp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	emit      => "fsub%FR%FP%FM %AF",
	am        => "source,binary",
	latency   => 4,
	mode      => $mode_fp87,
	attr_type => "ia32_x87_attr_t",
},

fdiv => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp", "fp", "fpcw" ],
	               out => [ "fp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "dividend", "divisor", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	emit      => "fdiv%FR%FP%FM %AF",
	am        => "source,binary",
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
},

fabs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	ins       => [ "value" ],
	emit      => "fabs",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 2,
	mode      => $mode_fp87,
},

fchs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	ins       => [ "value" ],
	emit      => "fchs",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 2,
	mode      => $mode_fp87,
},

fld => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "fp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => "fld%FM %AM",
	attr      => "ir_mode *load_mode",
	init_attr => "attr->ls_mode = load_mode;",
	latency   => 2,
},

fst => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp" ],
	               out => [ "none", "none", "none" ] },
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
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "fp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	ins       => [ "base", "index", "mem" ],
	emit      => "fild%FM %AM",
	latency   => 4,
},

fist => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp", "fpcw" ],
	               out => [ "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val", "fpcw" ],
	outs      => [ "dummy", "M", "X_regular", "X_except" ],
	emit      => "fist%FP%FM %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

# SSE3 fisttp instruction
fisttp => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp" ],
	               out => [ "in_r4", "none", "none", "none" ]},
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "M", "X_regular", "X_except" ],
	emit      => "fisttp%FM %AM",
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
},

fldz => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	outs      => [ "res" ],
	emit      => "fldz",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fld1 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	outs      => [ "res" ],
	emit      => "fld1",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fldpi => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	outs      => [ "res" ],
	emit      => "fldpi",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fldln2 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	outs      => [ "res" ],
	emit      => "fldln2",
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fldlg2 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	emit      => "fldlg2",
	outs      => [ "res" ],
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fldl2t => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	emit      => "fldll2t",
	outs      => [ "res" ],
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

fldl2e => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "fp" ] },
	emit      => "fldl2e",
	outs      => [ "res" ],
	init_attr => "attr->ls_mode = ia32_mode_E;",
	latency   => 4,
	mode      => $mode_fp87,
},

FucomFnstsw => {
# we can't allow to rematerialize this node so we don't
#  accidently produce Phi(Fucom, Fucom(ins_permuted))
#	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "eax" ] },
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
	reg_req   => { in => [ "fp", "fp" ], out => [ "eax" ] },
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
	reg_req   => { in => [ "fp", "fp" ], out => [ "eflags" ] },
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	emit      => "fucom%FPi %F0",
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.ins_permuted = ins_permuted;",
	latency   => 3,
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

FtstFnstsw => {
#	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "eax" ] },
	ins       => [ "left" ],
	outs      => [ "flags" ],
	emit      => "ftst\n".
	             "fnstsw %%ax",
	attr      => "bool ins_permuted",
	init_attr => "attr->ins_permuted = ins_permuted;",
	latency   => 3,
	mode      => $mode_gp
},

Sahf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "eax" ], out => [ "eflags" ] },
	ins       => [ "val" ],
	outs      => [ "flags" ],
	emit      => "sahf",
	latency   => 1,
	mode      => $mode_flags,
},

# fxch, fpush, fpop
# Note that it is NEVER allowed to do CSE on these nodes
# Moreover, note the virtual register requierements!

fxch => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "fxch %F0",
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

fpush => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "fld %F0",
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

fpushCopy => {
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "fld %F0",
	attr_type => "ia32_x87_attr_t",
	latency   => 1,
},

fpop => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "fstp %F0",
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

ffreep => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "ffreep %F0",
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

emms => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "emms",
	mode      => "mode_ANY",
	latency   => 3,
},

femms => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	attrs_equal => "attrs_equal_false",
	emit      => "femms",
	mode      => "mode_ANY",
	latency   => 3,
},

# Spilling and reloading of SSE registers, hardcoded, not generated #

xxLoad => {
	op_flags  => [ "uses_memory", "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "xmm", "none", "none", "none" ] },
	emit      => "movdqu %D0, %AM",
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "M", "X_regular", "X_except" ],
	latency   => 1,
},

xxStore => {
	op_flags => [ "uses_memory", "fragile" ],
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => "movdqu %B",
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
