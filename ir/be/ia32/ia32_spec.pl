# Creation: 2005/10/19
# $Id$
# This is the specification for the ia32 assembler Firm-operations

$arch = "ia32";

# register types:
$normal      =  0; # no special type
$ignore      =  1; # ignore (do not assign this register)
$arbitrary   =  2; # emitter can choose an arbitrary register of this class
$virtual     =  4; # the register is a virtual one
$state       =  8; # register represents a state
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "edx" },
		{ name => "ecx" },
		{ name => "eax" },
		{ name => "ebx" },
		{ name => "esi" },
		{ name => "edi" },
		{ name => "ebp" },
		{ name => "esp", type => $ignore },
		{ name => "gp_NOREG", type => $ignore | $arbitrary | $virtual }, # we need a dummy register for NoReg nodes
		{ mode => "mode_Iu" }
	],
	mmx => [
		{ name => "mm0", type => $ignore },
		{ name => "mm1", type => $ignore },
		{ name => "mm2", type => $ignore },
		{ name => "mm3", type => $ignore },
		{ name => "mm4", type => $ignore },
		{ name => "mm5", type => $ignore },
		{ name => "mm6", type => $ignore },
		{ name => "mm7", type => $ignore },
		{ mode => "mode_E", flags => "manual_ra" }
	],
	xmm => [
		{ name => "xmm0" },
		{ name => "xmm1" },
		{ name => "xmm2" },
		{ name => "xmm3" },
		{ name => "xmm4" },
		{ name => "xmm5" },
		{ name => "xmm6" },
		{ name => "xmm7" },
		{ name => "xmm_NOREG", type => $ignore | $virtual },     # we need a dummy register for NoReg nodes
		{ mode => "mode_E" }
	],
	vfp => [
		{ name => "vf0" },
		{ name => "vf1" },
		{ name => "vf2" },
		{ name => "vf3" },
		{ name => "vf4" },
		{ name => "vf5" },
		{ name => "vf6" },
		{ name => "vf7" },
		{ name => "vfp_NOREG", type => $ignore | $arbitrary | $virtual }, # we need a dummy register for NoReg nodes
		{ mode => "mode_E" }
	],
	st => [
		{ name => "st0", realname => "st",    type => $ignore },
		{ name => "st1", realname => "st(1)", type => $ignore },
		{ name => "st2", realname => "st(2)", type => $ignore },
		{ name => "st3", realname => "st(3)", type => $ignore },
		{ name => "st4", realname => "st(4)", type => $ignore },
		{ name => "st5", realname => "st(5)", type => $ignore },
		{ name => "st6", realname => "st(6)", type => $ignore },
		{ name => "st7", realname => "st(7)", type => $ignore },
		{ mode => "mode_E", flags => "manual_ra" }
	],
	fp_cw => [	# the floating point control word
		{ name => "fpcw", type => $ignore | $state },
		{ mode => "ia32_mode_fpcw", flags => "manual_ra|state" }
	],
	flags => [
		{ name => "eflags", type => 0 },
		{ mode => "mode_Iu", flags => "manual_ra" }
	],
); # %reg_classes

%cpu = (
	GP     => [ 1, "GP_EAX", "GP_EBX", "GP_ECX", "GP_EDX", "GP_ESI", "GP_EDI", "GP_EBP" ],
	SSE    => [ 1, "SSE_XMM0", "SSE_XMM1", "SSE_XMM2", "SSE_XMM3", "SSE_XMM4", "SSE_XMM5", "SSE_XMM6", "SSE_XMM7" ],
	VFP    => [ 1, "VFP_VF0", "VFP_VF1", "VFP_VF2", "VFP_VF3", "VFP_VF4", "VFP_VF5", "VFP_VF6", "VFP_VF7" ],
	BRANCH => [ 1, "BRANCH1", "BRANCH2" ],
); # %cpu

%vliw = (
	bundle_size       => 1,
	bundels_per_cycle => 1
); # vliw

%emit_templates = (
	S0 => "${arch}_emit_source_register(node, 0);",
	S1 => "${arch}_emit_source_register(node, 1);",
	S2 => "${arch}_emit_source_register(node, 2);",
	S3 => "${arch}_emit_source_register(node, 3);",
	SB0 => "${arch}_emit_8bit_source_register_or_immediate(node, 0);",
	SB1 => "${arch}_emit_8bit_source_register_or_immediate(node, 1);",
	SB2 => "${arch}_emit_8bit_source_register_or_immediate(node, 2);",
	SB3 => "${arch}_emit_8bit_source_register_or_immediate(node, 3);",
	SH0 => "${arch}_emit_8bit_high_source_register(node, 0);",
	SS0 => "${arch}_emit_16bit_source_register_or_immediate(node, 0);",
	SI0 => "${arch}_emit_source_register_or_immediate(node, 0);",
	SI1 => "${arch}_emit_source_register_or_immediate(node, 1);",
	SI3 => "${arch}_emit_source_register_or_immediate(node, 3);",
	D0 => "${arch}_emit_dest_register(node, 0);",
	D1 => "${arch}_emit_dest_register(node, 1);",
	DS0 => "${arch}_emit_dest_register_size(node, 0);",
	DB0 => "${arch}_emit_8bit_dest_register(node, 0);",
	X0 => "${arch}_emit_x87_register(node, 0);",
	X1 => "${arch}_emit_x87_register(node, 1);",
	EX => "${arch}_emit_extend_suffix(node);",
	M  => "${arch}_emit_mode_suffix(node);",
	XM => "${arch}_emit_x87_mode_suffix(node);",
	XXM => "${arch}_emit_xmm_mode_suffix(node);",
	XSD => "${arch}_emit_xmm_mode_suffix_s(node);",
	AM => "${arch}_emit_am(node);",
	unop3 => "${arch}_emit_unop(node, n_ia32_unary_op);",
	unop4 => "${arch}_emit_unop(node, n_ia32_binary_right);",
	binop => "${arch}_emit_binop(node);",
	x87_binop => "${arch}_emit_x87_binop(node);",
	CMP3  => "${arch}_emit_cmp_suffix_node(node, 3);",
);




$default_op_attr_type = "ia32_op_attr_t";
$default_attr_type    = "ia32_attr_t";
$default_copy_attr    = "ia32_copy_attr";

sub ia32_custom_init_attr {
	my $constr = shift;
	my $node   = shift;
	my $name   = shift;
	my $res    = "";

	if(defined($node->{modified_flags})) {
		$res .= "\tarch_add_irn_flags(res, arch_irn_flags_modify_flags);\n";
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
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_x87_attributes(res);".
		"\tinit_ia32_asm_attributes(res);",
	ia32_attr_t     =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);",
	ia32_call_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_call_attributes(res, pop, call_tp);",
	ia32_condcode_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_condcode_attributes(res, condition_code);",
	ia32_switch_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_switch_attributes(res, default_pn);",
	ia32_copyb_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_copyb_attributes(res, size);",
	ia32_immediate_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_immediate_attributes(res, symconst, symconst_sign, no_pic_adjust, offset);",
	ia32_x87_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_x87_attributes(res);",
	ia32_climbframe_attr_t =>
		"\tinit_ia32_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
		"\tinit_ia32_climbframe_attributes(res, count);",
);

%compare_attr = (
	ia32_asm_attr_t        => "ia32_compare_asm_attr",
	ia32_attr_t            => "ia32_compare_nodes_attr",
	ia32_call_attr_t       => "ia32_compare_call_attr",
	ia32_condcode_attr_t   => "ia32_compare_condcode_attr",
	ia32_switch_attr_t     => "ia32_compare_switch_attr",
	ia32_copyb_attr_t      => "ia32_compare_copyb_attr",
	ia32_immediate_attr_t  => "ia32_compare_immediate_attr",
	ia32_x87_attr_t        => "ia32_compare_x87_attr",
	ia32_climbframe_attr_t => "ia32_compare_climbframe_attr",
);

%operands = (
);

$mode_xmm           = "mode_E";
$mode_gp            = "mode_Iu";
$mode_flags         = "mode_Iu";
$mode_fpcw          = "ia32_mode_fpcw";
$status_flags       = [ "CF", "PF", "AF", "ZF", "SF", "OF" ];
$status_flags_wo_cf = [       "PF", "AF", "ZF", "SF", "OF" ];
$fpcw_flags         = [ "FP_IM", "FP_DM", "FP_ZM", "FP_OM", "FP_UM", "FP_PM",
                        "FP_PC0", "FP_PC1", "FP_RC0", "FP_RC1", "FP_X" ];

%nodes = (

Immediate => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "gp_NOREG:I" ] },
	attr      => "ir_entity *symconst, int symconst_sign, int no_pic_adjust, long offset",
	attr_type => "ia32_immediate_attr_t",
	hash_func => "ia32_hash_Immediate",
	latency   => 0,
	mode      => $mode_gp,
},

Asm => {
	mode      => "mode_T",
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
	units     => [ ],
	latency   => 0,
	mode      => $mode_gp,
	cmp_attr  => "return 1;",
},

Add => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in  => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "res", "flags", "M" ],
	emit      => '. add%M %binop',
	am        => "source,binary",
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AddMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => ". add%M %SI3, %AM",
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

AddMem8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => ". add%M %SB3, %AM",
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

Adc => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	               out => [ "in_r4 in_r5", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "eflags" ],
	outs      => [ "res", "flags", "M" ],
	emit      => '. adc%M %binop',
	am        => "source,binary",
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Add => {
	ins       => [ "left", "right" ],
	attr_type => "",
	dump_func => "NULL",
},

l_Adc => {
	ins       => [ "left", "right", "eflags" ],
	attr_type => "",
	dump_func => "NULL",
},

Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constraints
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax", "gp" ],
	               out => [ "eax", "flags", "none", "edx" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	emit      => '. mul%M %unop4',
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
	latency   => 10,
	units     => [ "GP" ],
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
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

IMul1OP => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax", "gp" ],
	               out => [ "eax", "flags", "none", "edx" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	emit      => '. imul%M %unop4',
	outs      => [ "res_low", "flags", "M", "res_high" ],
	am        => "source,binary",
	latency   => 5,
	units     => [ "GP" ],
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
	emit      => '. and%M %binop',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AndMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. and%M %SI3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

AndMem8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none",  "eax ebx ecx edx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. and%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
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
	emit      => '. or%M %binop',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

OrMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. or%M %SI3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

OrMem8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. or%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
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
	emit      => '. xor%M %binop',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp", "flags" ] },
	outs      => [ "res", "flags" ],
	emit      => ". xor%M %D0, %D0",
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

XorMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. xor%M %SI3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

XorMem8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	emit      => '. xor%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
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
	emit      => '. sub%M %binop',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SubMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "subtrahend" ],
	emit      => '. sub%M %SI3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => 'mode_M',
	modified_flags => $status_flags
},

SubMem8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "subtrahend" ],
	emit      => '. sub%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => 'mode_M',
	modified_flags => $status_flags
},

Sbb => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp", "flags" ],
	               out => [ "in_r4", "flags", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend", "eflags" ],
	outs      => [ "res", "flags", "M" ],
	am        => "source,binary",
	emit      => '. sbb%M %binop',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sbb0 => {
	# Spiller currently fails when rematerializing flag consumers
	# irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "flags" ], out => [ "gp", "flags" ] },
	outs      => [ "res", "flags" ],
	emit      => ". sbb%M %D0, %D0",
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Sub => {
	ins       => [ "minuend", "subtrahend" ],
	attr_type => "",
	dump_func => "NULL",
},

l_Sbb => {
	ins       => [ "minuend", "subtrahend", "eflags" ],
	attr_type => "",
	dump_func => "NULL",
},

IDiv => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "eax", "edx" ],
	               out => [ "eax", "flags", "none", "edx", "none", "none" ] },
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
	emit      => ". idiv%M %unop3",
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Div => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "eax", "edx" ],
	               out => [ "eax", "flags", "none", "edx", "none", "none" ] },
	ins       => [ "base", "index", "mem", "divisor", "dividend_low", "dividend_high" ],
	outs      => [ "div_res", "flags", "M", "mod_res", "X_regular", "X_except" ],
	am        => "source,unary",
	emit      => ". div%M %unop3",
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => '. shl%M %SB1, %S0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

ShlMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	emit      => '. shl%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

ShlD => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp", "ecx" ],
	               out => [ "in_r1 !in_r2 !in_r3", "flags" ] },
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	emit      => ". shld%M %SB2, %S1, %D0",
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => '. shr%M %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags
},

ShrMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	emit      => '. shr%M %SB3, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	latency   => 1,
	modified_flags => $status_flags
},

ShrD => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp", "ecx" ],
	               out => [ "in_r1 !in_r2 !in_r3", "flags" ] },
	ins       => [ "val_high", "val_low", "count" ],
	outs      => [ "res", "flags" ],
	emit      => ". shrd%M %SB2, %S1, %D0",
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sar => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => '. sar%M %SB1, %S0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SarMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	emit      => '. sar%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

Ror => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => '. ror%M %SB1, %S0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RorMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	emit      => '. ror%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

Rol => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "ecx" ],
	               out => [ "in_r1 !in_r2", "flags" ] },
	ins       => [ "val", "count" ],
	outs      => [ "res", "flags" ],
	emit      => '. rol%M %SB1, %S0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RolMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "ecx" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "count" ],
	emit      => '. rol%M %SB3, %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

Neg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	emit      => '. neg%M %S0',
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

NegMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. neg%M %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	modified_flags => $status_flags
},

Minus64Bit => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1", "in_r2" ] },
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	latency   => 3,
	modified_flags => $status_flags
},


Inc => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	emit      => '. inc%M %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

IncMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. inc%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

Dec => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	emit      => '. dec%M %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

DecMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. dec%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	latency   => 1,
	modified_flags => $status_flags_wo_cf
},

Not => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	emit      => '. not%M %S0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	# no flags modified
},

NotMem => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. not%M %AM',
	units     => [ "GP" ],
	latency   => 1,
	mode      => "mode_M",
	# no flags modified
},

Cmc => {
	reg_req   => { in => [ "flags" ], out => [ "flags" ] },
	emit      => '.cmc',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Stc => {
	reg_req   => { out => [ "flags" ] },
	emit      => '.stc',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Cmp => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in  => [ "gp", "gp", "none", "gp", "gp" ],
	               out => [ "flags", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => '. cmp%M %binop',
	attr      => "bool ins_permuted",
	init_attr => "attr->data.ins_permuted   = ins_permuted;",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Cmp8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx", "eax ebx ecx edx" ] ,
	               out => [ "flags", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => '. cmpb %binop',
	attr      => "bool ins_permuted",
	init_attr => "attr->data.ins_permuted   = ins_permuted;",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_flags,
	modified_flags => $status_flags
},

XorHighLow => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "eax ebx ecx edx" ],
	               out => [ "in_r1", "flags" ] },
	emit      => '. xorb %SH0, %SB0',
	ins       => [ "value" ],
	outs      => [ "res", "flags" ],
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags,
},

Test => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "gp" ] ,
	               out => [ "flags", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => '. test%M %binop',
	attr      => "bool ins_permuted",
	init_attr => "attr->data.ins_permuted = ins_permuted;",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Test8Bit => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx", "eax ebx ecx edx" ] ,
	               out => [ "flags", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right" ],
	outs      => [ "eflags", "unused", "M" ],
	am        => "source,binary",
	emit      => '. testb %binop',
	attr      => "bool ins_permuted",
	init_attr => "attr->data.ins_permuted = ins_permuted;",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_flags,
	modified_flags => $status_flags
},

Setcc => {
	#irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "eflags" ], out => [ "eax ebx ecx edx" ] },
	ins       => [ "eflags" ],
	outs      => [ "res" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "ia32_condition_code_t condition_code",
	# The way we handle Setcc with float nodes (potentially) destroys the flags
	# (when we emit the setX; setp; orb and the setX;setnp;andb sequences)
	init_attr => "set_ia32_ls_mode(res, mode_Bu);\n"
		. "\tif (condition_code & ia32_cc_additional_float_cases) {\n"
		. "\t\tarch_add_irn_flags(res, arch_irn_flags_modify_flags);\n"
		. "\t\t/* attr->latency = 3; */\n"
		. "\t}\n",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

SetccMem => {
	#irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eflags" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem","eflags" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "ia32_condition_code_t condition_code",
	init_attr => "set_ia32_ls_mode(res, mode_Bu);\n",
	emit      => '. set%CMP3 %AM',
	latency   => 1,
	units     => [ "GP" ],
	mode      => 'mode_M',
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
	attr      => "ia32_condition_code_t condition_code",
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr_type => "ia32_condcode_attr_t",
	attr      => "ia32_condition_code_t condition_code",
	latency   => 2,
	units     => [ "BRANCH" ],
},

SwitchJmp => {
	state     => "pinned",
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	reg_req   => { in => [ "gp", "gp" ] },
	ins       => [ "base", "index" ],
	mode      => "mode_T",
	attr_type => "ia32_switch_attr_t",
	attr      => "long default_pn",
	latency   => 2,
	units     => [ "BRANCH" ],
	init_attr => "info->out_infos = NULL;", # XXX ugly hack for out requirements
},

Jmp => {
	state     => "pinned",
	irn_flags => [ "simple_jump" ],
	op_flags  => [ "cfopcode" ],
	reg_req   => { out => [ "none" ] },
	latency   => 1,
	units     => [ "BRANCH" ],
	mode      => "mode_X",
},

IJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "unknown_jump" ],
	reg_req   => { in => [ "gp", "gp", "none", "gp" ] },
	ins       => [ "base", "index", "mem", "target" ],
	am        => "source,unary",
	emit      => '. jmp *%unop3',
	latency   => 1,
	units     => [ "BRANCH" ],
	mode      => "mode_X",
	init_attr => "info->out_infos = NULL;", # XXX ugly hack for out requirements
},

Const => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
	attr      => "ir_entity *symconst, int symconst_sign, int no_pic_adjust, long offset",
	attr_type => "ia32_immediate_attr_t",
	latency   => 1,
	mode      => $mode_gp,
},

Unknown => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	latency   => 0,
	emit      => '',
	mode      => $mode_gp,
},

GetEIP => {
	op_flags => [ "constlike" ],
	reg_req  => { out => [ "gp" ] },
	units    => [ "GP" ],
	latency  => 5,
	mode     => $mode_gp,
	modified_flags => $status_flags,
},

NoReg_GP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock", "dump_noinput" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "gp_NOREG:I" ] },
	units     => [],
	emit      => "",
	latency   => 0,
	mode      => $mode_gp
},

NoReg_VFP => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock", "dump_noinput" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "vfp_NOREG:I" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E",
	latency   => 0,
	attr_type => "ia32_x87_attr_t",
},

NoReg_XMM => {
	state     => "pinned",
	op_flags  => [ "constlike", "dump_noblock", "dump_noinput" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "xmm_NOREG:I" ] },
	units     => [],
	emit      => "",
	latency   => 0,
	mode      => "mode_E"
},

ChangeCW => {
	state     => "pinned",
	op_flags  => [ "constlike" ],
	irn_flags => [ "not_scheduled" ],
	reg_req   => { out => [ "fpcw:I" ] },
	mode      => $mode_fpcw,
	latency   => 3,
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FldCW => {
	op_flags  => [ "labeled" ],
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "fpcw:I" ] },
	ins       => [ "base", "index", "mem" ],
	latency   => 5,
	emit      => ". fldcw %AM",
	mode      => $mode_fpcw,
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FnstCW => {
	op_flags  => [ "labeled" ],
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "fp_cw" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem", "fpcw" ],
	latency   => 5,
	emit      => ". fnstcw %AM",
	mode      => "mode_M",
	units     => [ "GP" ],
},

FnstCWNOP => {
	op_flags  => [ "labeled" ],
	state     => "pinned",
	reg_req   => { in => [ "fp_cw" ], out => [ "none" ] },
	ins       => [ "fpcw" ],
	latency   => 0,
	emit      => "",
	mode      => "mode_M",
},

Cltd => {
	# we should not rematrialize this node. It has very strict constraints.
	reg_req   => { in => [ "eax", "edx" ], out => [ "edx" ] },
	ins       => [ "val", "clobbered" ],
	emit      => '. cltd',
	latency   => 1,
	mode      => $mode_gp,
	units     => [ "GP" ],
},

# Load / Store
#
# Note that we add additional latency values depending on address mode, so a
# lateny of 0 for load is correct

Load => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "gp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	latency   => 0,
	emit      => ". mov%EX%.l %AM, %D0",
	units     => [ "GP" ],
},

Store => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => '. mov%M %SI3, %AM',
	latency   => 2,
	units     => [ "GP" ],
},

Store8Bit => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ],
	               out => ["none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit      => '. mov%M %SB3, %AM',
	latency   => 2,
	units     => [ "GP" ],
},

Lea => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "base", "index" ],
	emit      => '. leal %AM, %D0',
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
# lea doesn't modify the flags, but setting this seems advantageous since it
# increases chances that the Lea is transformed back to an Add
	modified_flags => 1,
},

Push => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "esp" ], out => [ "esp:I|S", "none" ] },
	ins       => [ "base", "index", "mem", "val", "stack" ],
	emit      => '. push%M %unop3',
	outs      => [ "stack", "M" ],
	am        => "source,unary",
	latency   => 2,
	units     => [ "GP" ],
},

PushEax => {
	state   => "exc_pinned",
	reg_req => { in => [ "esp" ], out => [ "esp:I|S" ] },
	ins     => [ "stack" ],
	outs    => [ "stack" ],
	emit    => '. pushl %%eax',
	latency => 2,
	units   => [ "GP" ],
	mode    => $mode_gp,
},

Pop => {
	state     => "exc_pinned",
	reg_req   => { in => [ "none", "esp" ], out => [ "gp", "none", "none", "esp:I|S" ] },
	ins       => [ "mem", "stack" ],
	outs      => [ "res", "M", "unused", "stack" ],
	emit      => '. pop%M %D0',
	latency   => 3, # Pop is more expensive than Push on Athlon
	units     => [ "GP" ],
},

PopEbp => {
	state     => "exc_pinned",
	reg_req   => { in => [ "none", "esp" ], out => [ "ebp:I", "none", "none", "esp:I|S" ] },
	ins       => [ "mem", "stack" ],
	outs      => [ "res", "M", "unused", "stack" ],
	emit      => '. pop%M %D0',
	latency   => 3, # Pop is more expensive than Push on Athlon
	units     => [ "GP" ],
},

CopyEbpEsp => {
	state     => "exc_pinned",
	reg_req   => { in => [ "ebp" ], out => [ "esp:I|S" ] },
	ins       => [ "ebp" ],
	outs      => [ "esp" ],
	emit      => '. movl %S0, %D0',
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

PopMem => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp" ], out => [ "none", "none", "none", "esp:I|S" ] },
	ins       => [ "base", "index", "mem", "stack" ],
	outs      => [ "unused0", "M", "unused1", "stack" ],
	emit      => '. pop%M %AM',
	latency   => 3, # Pop is more expensive than Push on Athlon
	units     => [ "GP" ],
},

Enter => {
	reg_req   => { in => [ "esp" ], out => [ "ebp", "esp:I|S", "none" ] },
	emit      => '. enter',
	outs      => [ "frame", "stack", "M" ],
	latency   => 15,
	units     => [ "GP" ],
},

Leave => {
	reg_req   => { in => [ "ebp" ], out => [ "ebp:I", "esp:I|S" ] },
	emit      => '. leave',
	outs      => [ "frame", "stack" ],
	latency   => 3,
	units     => [ "GP" ],
	state     => "exc_pinned",
},

AddSP => {
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp", "gp" ], out => [ "esp:I|S", "none" ] },
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => '. addl %binop',
	latency   => 1,
	outs      => [ "stack", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

SubSP => {
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none", "esp", "gp" ], out => [ "esp:I|S", "gp", "none" ] },
	ins       => [ "base", "index", "mem", "stack", "size" ],
	am        => "source,binary",
	emit      => ". subl %binop\n".
	             ". movl %%esp, %D1",
	latency   => 2,
	outs      => [ "stack", "addr", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

RepPrefix => {
	op_flags  => [ "keep" ],
	state     => "pinned",
	mode      => "mode_M",
	emit      => ".	rep",
	latency   => 0,
},

LdTls => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
	emit      => ". movl %%gs:0, %D0",
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
	emit      => '. bt%M %S1, %S0',
	units     => [ "GP" ],
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
	am        => "source,binary",
	emit      => '. bsf%M %unop3, %D0',
	units     => [ "GP" ],
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
	am        => "source,binary",
	emit      => '. bsr%M %unop3, %D0',
	units     => [ "GP" ],
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
	am        => "source,binary",
	emit      => '. popcnt%M %unop3, %D0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Call => {
	op_flags  => [ "fragile" ],
	state     => "exc_pinned",
	reg_req   => {
		in  => [ "gp", "gp", "none", "gp", "esp", "fpcw", "eax", "ecx", "edx" ],
		out => [ "esp:I|S", "fpcw:I", "none", "eax", "ecx", "edx", "vf0", "vf1", "vf2", "vf3", "vf4", "vf5", "vf6", "vf7", "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "none", "none" ]
	},
	ins       => [ "base", "index", "mem", "addr", "stack", "fpcw", "eax", "ecx", "edx" ],
	outs      => [ "stack", "fpcw", "M", "eax", "ecx", "edx", "vf0", "vf1", "vf2", "vf3", "vf4", "vf5", "vf6", "vf7", "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "X_regular", "X_except" ],
	attr_type => "ia32_call_attr_t",
	attr      => "unsigned pop, ir_type *call_tp",
	am        => "source,unary",
	units     => [ "BRANCH" ],
	latency   => 4, # random number
	modified_flags => $status_flags
},

#
# a Helper node for frame-climbing, needed for __builtin_(frame|return)_address
#
# PS: try gcc __builtin_frame_address(100000) :-)
#
ClimbFrame => {
	reg_req   => { in => [ "gp", "gp", "gp"], out => [ "in_r3" ] },
	ins       => [ "frame", "cnt", "tmp" ],
	outs      => [ "res" ],
	latency   => 4, # random number
	attr_type => "ia32_climbframe_attr_t",
	attr      => "unsigned count",
	units     => [ "GP" ],
	mode      => $mode_gp
},

#
# bswap
#
Bswap => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	emit      => '. bswap%M %S0',
	ins       => [ "val" ],
	units     => [ "GP" ],
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
	emit      => '. xchg %SB0, %SH0',
	ins       => [ "val" ],
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
},

#
# BreakPoint
#
Breakpoint => {
	state     => "pinned",
	reg_req   => { in => [ "none" ], out => [ "none" ] },
	ins       => [ "mem" ],
	latency   => 0,
	emit      => ". int3",
	units     => [ "GP" ],
	mode      => mode_M,
},

#
# Undefined Instruction on ALL x86 CPU's
#
UD2 => {
	state     => "pinned",
	reg_req   => { in => [ "none" ], out => [ "none" ] },
	ins       => [ "mem" ],
	latency   => 0,
	emit      => ". .value  0x0b0f",
	units     => [ "GP" ],
	mode      => mode_M,
},

#
# outport
#
Outport => {
	irn_flags => [ "rematerializable" ],
	state     => "pinned",
	reg_req   => { in => [ "edx", "eax", "none" ], out => [ "none" ] },
	ins       => [ "port", "value", "mem" ],
	emit      => '. out%M %SS0, %SI1',
	units     => [ "GP" ],
	latency   => 1,
	mode      => mode_M,
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
	emit      => '. in%M %DS0, %SS0',
	units     => [ "GP" ],
	latency   => 1,
	mode      => mode_T,
	modified_flags => $status_flags
},

#
# Intel style prefetching
#
Prefetch0 => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetcht0 %AM",
	units     => [ "GP" ],
},

Prefetch1 => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetcht1 %AM",
	units     => [ "GP" ],
},

Prefetch2 => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetcht2 %AM",
	units     => [ "GP" ],
},

PrefetchNTA => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetchnta %AM",
	units     => [ "GP" ],
},

#
# 3DNow! prefetch instructions
#
Prefetch => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetch %AM",
	units     => [ "GP" ],
},

PrefetchW => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "M" ],
	latency   => 0,
	emit      => ". prefetchw %AM",
	units     => [ "GP" ],
},

# produces a 0/+0.0
xZero => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => '. xorp%XSD %D0, %D0',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

xUnknown => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => '',
	latency   => 0,
	mode      => $mode_xmm
},

xPzero => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => '. pxor %D0, %D0',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

# produces all 1 bits
xAllOnes => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "xmm" ] },
	emit      => '. pcmpeqb %D0, %D0',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

# integer shift left, dword
xPslld => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => '. pslld %SI1, %D0',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

# integer shift left, qword
xPsllq => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => '. psllq %SI1, %D0',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

# integer shift right, dword
xPsrld => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "xmm", "xmm" ], out => [ "in_r1 !in_r2" ] },
	emit      => '. psrld %SI1, %D0',
	latency   => 1,
	units     => [ "SSE" ],
	mode      => $mode_xmm
},

# mov from integer to SSE register
xMovd  => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "xmm" ] },
	emit      => '. movd %S0, %D0',
	latency   => 1,
	units     => [ "SSE" ],
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
	emit      => '. add%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
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
	emit      => '. mul%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
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
	emit      => '. max%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
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
	emit      => '. min%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
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
	emit      => '. andp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
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
	emit      => '. orp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
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
	emit      => '. xorp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
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
	emit      => '. andnp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
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
	emit      => '. sub%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
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
	emit      => '. div%XXM %binop',
	latency   => 16,
	units     => [ "SSE" ],
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
	init_attr => "attr->data.ins_permuted = ins_permuted;",
	emit      => ' .ucomi%XXM %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => $mode_flags,
	modified_flags => 1,
},

xLoad => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "xmm", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	emit      => '. mov%XXM %AM, %D0',
	attr      => "ir_mode *load_mode",
	init_attr => "attr->ls_mode = load_mode;",
	latency   => 0,
	units     => [ "SSE" ],
},

xStore => {
	op_flags => [ "fragile", "labeled" ],
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	emit     => '. mov%XXM %S3, %AM',
	latency  => 0,
	units    => [ "SSE" ],
},

xStoreSimple => {
	op_flags => [ "fragile", "labeled" ],
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => '. mov%XXM %S3, %AM',
	latency  => 0,
	units    => [ "SSE" ],
},

CvtSI2SS => {
	op_flags => [ "labeled" ],
	state     => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "gp" ], out => [ "xmm" ] },
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	emit     => '. cvtsi2ss %unop3, %D0',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => $mode_xmm
},

CvtSI2SD => {
	op_flags => [ "labeled" ],
	state     => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "gp" ], out => [ "xmm" ] },
	ins      => [ "base", "index", "mem", "val" ],
	am       => "source,unary",
	emit     => '. cvtsi2sd %unop3, %D0',
	latency  => 2,
	units    => [ "SSE" ],
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
	op_flags  => [ "fragile" ],
	state     => "pinned",
	reg_req   => { in => [ "edi", "esi", "ecx", "none" ],
	               out => [ "edi", "esi", "ecx", "none", "none", "none" ] },
	ins       => [ "dest", "source", "count", "mem" ],
	outs      => [ "dest", "source", "count", "M", "X_regular", "X_except" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	units     => [ "GP" ],
	latency   => 3,
# we don't care about this flag, so no need to mark this node
#	modified_flags => [ "DF" ]
},

CopyB_i => {
	op_flags  => [ "fragile" ],
	state     => "pinned",
	reg_req   => { in => [ "edi", "esi", "none" ],
	               out => [  "edi", "esi", "none", "none", "none" ] },
	ins       => [ "dest", "source", "mem" ],
	outs      => [ "dest", "source", "M", "X_regular", "X_except" ],
	attr_type => "ia32_copyb_attr_t",
	attr      => "unsigned size",
	units     => [ "GP" ],
	latency   => 3,
# we don't care about this flag, so no need to mark this node
#	modified_flags => [ "DF" ]
},

Cwtl => {
	state     => "exc_pinned",
	reg_req   => { in => [ "eax" ], out => [ "eax" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	emit      => '. cwtl',
	units     => [ "GP" ],
	latency   => 1,
	mode      => $mode_gp,
},

Conv_I2I => {
	op_flags  => [ "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp" ],
	               out => [ "gp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "flags", "M", "X_regular", "X_except" ],
	am        => "source,unary",
	units     => [ "GP" ],
	latency   => 1,
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	mode      => $mode_gp,
},

Conv_I2I8Bit => {
	op_flags  => [ "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "eax ebx ecx edx" ],
	               out => [ "gp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "flags", "M", "X_regular", "X_except" ],
	am        => "source,unary",
	units     => [ "GP" ],
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
	units     => [ "SSE" ],
	mode      => $mode_xmm,
},

Conv_FP2I => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm" ], out => [ "gp", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 10,
	units     => [ "SSE" ],
	mode      => $mode_gp,
},

Conv_FP2FP => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "xmm" ], out => [ "xmm", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	am        => "source,unary",
	latency   => 8,
	units     => [ "SSE" ],
	mode      => $mode_xmm,
},

# rematerialisation disabled for all float nodes for now, because the fpcw
# handler runs before spilling and we might end up with wrong fpcw then

vfadd => {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp", "vfp", "fpcw" ],
	               out => [ "vfp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfmul => {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp", "vfp", "fpcw" ],
	               out => [ "vfp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "left", "right", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfsub => {
#	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp", "vfp", "fpcw" ],
	               out => [ "vfp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "minuend", "subtrahend", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfdiv => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp", "vfp", "fpcw" ],
	               out => [ "vfp", "none", "none" ] },
	ins       => [ "base", "index", "mem", "dividend", "divisor", "fpcw" ],
	outs      => [ "res", "dummy", "M" ],
	am        => "source,binary",
	latency   => 20,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfprem => {
	reg_req   => { in => [ "vfp", "vfp", "fpcw" ], out => [ "vfp" ] },
	ins       => [ "left", "right", "fpcw" ],
	latency   => 20,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfabs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	ins       => [ "value" ],
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfchs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	ins       => [ "value" ],
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfld => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "vfp", "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "unused", "M", "X_regular", "X_except" ],
	attr      => "ir_mode *load_mode",
	init_attr => "attr->attr.ls_mode = load_mode;",
	latency   => 2,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfst => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp" ],
	               out => [ "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "M", "X_regular", "X_except" ],
	attr      => "ir_mode *store_mode",
	init_attr => "attr->attr.ls_mode = store_mode;",
	latency   => 2,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfild => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "vfp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	ins       => [ "base", "index", "mem" ],
	latency   => 4,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfist => {
	op_flags  => [ "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp", "fpcw" ],
	               out => [ "none", "none", "none", "none" ] },
	ins       => [ "base", "index", "mem", "val", "fpcw" ],
	outs      => [ "dummy", "M", "X_regular", "X_except" ],
	latency   => 4,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

# SSE3 fisttp instruction
vfisttp => {
	op_flags  => [ "fragile" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "vfp" ],
	               out => [ "in_r4", "none", "none", "none" ]},
	ins       => [ "base", "index", "mem", "val" ],
	outs      => [ "res", "M", "X_regular", "X_except" ],
	latency   => 4,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfldz => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfld1 => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldpi => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldln2 => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldlg2 => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldl2t => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldl2e => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	outs      => [ "res" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vFucomFnstsw => {
# we can't allow to rematerialize this node so we don't
#  accidently produce Phi(Fucom, Fucom(ins_permuted))
#	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "vfp", "vfp" ], out => [ "eax" ] },
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.data.ins_permuted = ins_permuted;",
	latency   => 3,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

vFucomi => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "vfp", "vfp" ], out => [ "eflags" ] },
	ins       => [ "left", "right" ],
	outs      => [ "flags" ],
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.data.ins_permuted = ins_permuted;",
	latency   => 3,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

vFtstFnstsw => {
#	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "vfp" ], out => [ "eax" ] },
	ins       => [ "left" ],
	outs      => [ "flags" ],
	attr      => "bool ins_permuted",
	init_attr => "attr->attr.data.ins_permuted = ins_permuted;",
	latency   => 3,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
	mode      => $mode_gp
},

Sahf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "eax" ], out => [ "eflags" ] },
	ins       => [ "val" ],
	outs      => [ "flags" ],
	emit      => '. sahf',
	latency   => 1,
	units     => [ "GP" ],
	mode      => $mode_flags,
},

fadd => {
	state     => "exc_pinned",
	emit      => '. fadd%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

faddp => {
	state     => "exc_pinned",
	emit      => '. faddp%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fmul => {
	state     => "exc_pinned",
	emit      => '. fmul%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fmulp => {
	state     => "exc_pinned",
	emit      => '. fmulp%XM %x87_binop',,
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fsub => {
	state     => "exc_pinned",
	emit      => '. fsub%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

# Note: gas is strangely buggy: fdivrp and fdivp as well as fsubrp and fsubp
#       are swapped, we work this around in the emitter...

fsubp => {
	state     => "exc_pinned",
# see note about gas bugs
	emit      => '. fsubrp%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fsubr => {
	state     => "exc_pinned",
	irn_flags => [ "rematerializable" ],
	emit      => '. fsubr%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fsubrp => {
	state     => "exc_pinned",
	irn_flags => [ "rematerializable" ],
# see note about gas bugs before fsubp
	emit      => '. fsubp%XM %x87_binop',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fprem => {
	emit      => '. fprem1',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

# this node is just here, to keep the simulator running
# we can omit this when a fprem simulation function exists
fpremp => {
	emit      => '. fprem1\n'.
	             '. fstp %X0',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fdiv => {
	state     => "exc_pinned",
	emit      => '. fdiv%XM %x87_binop',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fdivp => {
	state     => "exc_pinned",
# see note about gas bugs before fsubp
	emit      => '. fdivrp%XM %x87_binop',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fdivr => {
	state     => "exc_pinned",
	emit      => '. fdivr%XM %x87_binop',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fdivrp => {
	state     => "exc_pinned",
# see note about gas bugs before fsubp
	emit      => '. fdivp%XM %x87_binop',
	latency   => 20,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fabs => {
	emit      => '. fabs',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fchs => {
	op_flags  => [ "keep" ],
	irn_flags => [ "rematerializable" ],
	emit      => '. fchs',
	latency   => 4,
	attr_type => "ia32_x87_attr_t",
	constructors => {},
},

fld => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	emit      => '. fld%XM %AM',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fst => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	emit      => '. fst%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fstp => {
	irn_flags => [ "rematerializable" ],
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	emit      => '. fstp%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fild => {
	state     => "exc_pinned",
	emit      => '. fild%XM %AM',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fist => {
	state     => "exc_pinned",
	emit      => '. fist%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fistp => {
	state     => "exc_pinned",
	emit      => '. fistp%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

# SSE3 fisttp instruction
fisttp => {
	state     => "exc_pinned",
	emit      => '. fisttp%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
	constructors => {},
},

fldz => {
	op_flags  =>  [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldz',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fld1 => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fld1',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fldpi => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldpi',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fldln2 => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldln2',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fldlg2 => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldlg2',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fldl2t => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldll2t',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

fldl2e => {
	op_flags  => [ "constlike", "keep" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "vfp" ] },
	emit      => '. fldl2e',
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

# fxch, fpush, fpop
# Note that it is NEVER allowed to do CSE on these nodes
# Moreover, note the virtual register requierements!

fxch => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. fxch %X0',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

fpush => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. fld %X0',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

fpushCopy => {
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	cmp_attr  => "return 1;",
	emit      => '. fld %X0',
	attr_type => "ia32_x87_attr_t",
	latency   => 1,
},

fpop => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. fstp %X0',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

ffreep => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. ffreep %X0',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 1,
},

emms => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. emms',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 3,
},

femms => {
	op_flags  => [ "keep" ],
	reg_req   => { out => [ "none" ] },
	cmp_attr  => "return 1;",
	emit      => '. femms',
	attr_type => "ia32_x87_attr_t",
	mode      => "mode_ANY",
	latency   => 3,
},

FucomFnstsw => {
	reg_req   => { },
	emit      => ". fucom %X1\n".
	             ". fnstsw %%ax",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

FucompFnstsw => {
	reg_req   => { },
	emit      => ". fucomp %X1\n".
	             ". fnstsw %%ax",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

FucomppFnstsw => {
	reg_req   => { },
	emit      => ". fucompp\n".
	             ". fnstsw %%ax",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

Fucomi => {
	reg_req   => { },
	emit      => '. fucomi %X1',
	attr_type => "ia32_x87_attr_t",
	latency   => 1,
},

Fucompi => {
	reg_req   => { },
	emit      => '. fucompi %X1',
	attr_type => "ia32_x87_attr_t",
	latency   => 1,
},

FtstFnstsw => {
	reg_req   => { },
	emit      => ". ftst\n".
	             ". fnstsw %%ax",
	attr_type => "ia32_x87_attr_t",
	latency   => 2,
},

# Spilling and reloading of SSE registers, hardcoded, not generated #

xxLoad => {
	op_flags  => [ "fragile", "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ],
	               out => [ "xmm", "none", "none", "none" ] },
	emit      => '. movdqu %D0, %AM',
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "M", "X_regular", "X_except" ],
	units     => [ "SSE" ],
	latency   => 1,
},

xxStore => {
	op_flags => [ "fragile", "labeled" ],
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "none", "xmm" ],
	              out => [ "none", "none", "none" ] },
	ins      => [ "base", "index", "mem", "val" ],
	outs     => [ "M", "X_regular", "X_except" ],
	emit     => '. movdqu %binop',
	units    => [ "SSE" ],
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
	$op_attr_init .= "attr->latency = ".$node->{latency} . ";";

	$node->{op_attr_init} = $op_attr_init;
}

print "";
