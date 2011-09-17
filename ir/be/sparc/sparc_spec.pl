# Creation: 2006/02/13
# $Id$

$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fpflags = "mode_Bu";
$mode_fp      = "mode_F";
$mode_fp2     = "mode_D";
$mode_fp4     = "mode_E"; # not correct, we need to register a new mode

# available SPARC registers: 8 globals, 24 window regs (8 ins, 8 outs, 8 locals)
%reg_classes = (
	gp => [
		{ name => "g0" },
		{ name => "g1" },
		{ name => "g2" },
		{ name => "g3" },
		{ name => "g4" },
		{ name => "g5" },
		{ name => "g6" },
		{ name => "g7" },

		{ name => "o0" },
		{ name => "o1" },
		{ name => "o2" },
		{ name => "o3" },
		{ name => "o4" },
		{ name => "o5" },
		{ name => "sp" },
		{ name => "o7" },

		{ name => "l0" },
		{ name => "l1" },
		{ name => "l2" },
		{ name => "l3" },
		{ name => "l4" },
		{ name => "l5" },
		{ name => "l6" },
		{ name => "l7" },

		{ name => "i0" },
		{ name => "i1" },
		{ name => "i2" },
		{ name => "i3" },
		{ name => "i4" },
		{ name => "i5" },
		{ name => "frame_pointer", realname => "fp" },
		{ name => "i7" },
		{ mode => $mode_gp }
	],
	fpflags_class => [
		{ name => "fpflags" },
		{ mode => $mode_fpflags, flags => "manual_ra" }
	],
	flags_class => [
		{ name => "flags" },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
	mul_div_high_res => [
		{ name => "y" },
		{ mode => $mode_gp, flags => "manual_ra" }
	],
	# fp registers can be accessed any time
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
		{ name => "f16" },
		{ name => "f17" },
		{ name => "f18" },
		{ name => "f19" },
		{ name => "f20" },
		{ name => "f21" },
		{ name => "f22" },
		{ name => "f23" },
		{ name => "f24" },
		{ name => "f25" },
		{ name => "f26" },
		{ name => "f27" },
		{ name => "f28" },
		{ name => "f29" },
		{ name => "f30" },
		{ name => "f31" },
		{ mode => $mode_fp }
	]
); # %reg_classes

%emit_templates = (
# emit source reg or imm dep. on node's arity
	RI  => "${arch}_emit_reg_or_imm(node, -1);",
	R1I => "${arch}_emit_reg_or_imm(node, 1);",
	S0  => "${arch}_emit_source_register(node, 0);",
	S1  => "${arch}_emit_source_register(node, 1);",
	D0  => "${arch}_emit_dest_register(node, 0);",
	HIM => "${arch}_emit_high_immediate(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	FLSM => "${arch}_emit_float_load_store_mode(node);",
	FPM  => "${arch}_emit_fp_mode_suffix(node);",
	FCONVS => "${arch}_emit_fp_conv_source(node);",
	FCONVD => "${arch}_emit_fp_conv_destination(node);",
	O1     => "${arch}_emit_offset(node, 1);",
	O2     => "${arch}_emit_offset(node, 2);",
);

$default_attr_type = "sparc_attr_t";
$default_copy_attr = "sparc_copy_attr";


%init_attr = (
	sparc_attr_t             => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);",
	sparc_load_store_attr_t  => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);",
	sparc_jmp_cond_attr_t    => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);",
	sparc_switch_jmp_attr_t  => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
	                            "\tinit_sparc_switch_jmp_attributes(res, default_pn, jump_table);\n",
	sparc_fp_attr_t          => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);\n".
	                            "\tinit_sparc_fp_attributes(res, fp_mode);\n",
	sparc_fp_conv_attr_t     => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, exec_units, n_res);".
	                            "\tinit_sparc_fp_conv_attributes(res, src_mode, dest_mode);\n",
);

%compare_attr = (
	sparc_attr_t            => "cmp_attr_sparc",
	sparc_load_store_attr_t => "cmp_attr_sparc_load_store",
	sparc_jmp_cond_attr_t   => "cmp_attr_sparc_jmp_cond",
	sparc_switch_jmp_attr_t	=> "cmp_attr_sparc_switch_jmp",
	sparc_fp_attr_t         => "cmp_attr_sparc_fp",
	sparc_fp_conv_attr_t    => "cmp_attr_sparc_fp_conv",
);

%custom_irn_flags = (
	modifies_flags    => "(arch_irn_flags_t)sparc_arch_irn_flag_modifies_flags",
	modifies_fp_flags => "(arch_irn_flags_t)sparc_arch_irn_flag_modifies_fp_flags",
);

my %cmp_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "flags" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
);

my %binop_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right" ],
	},
);

my %binopcc_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "gp", "flags" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp", "flags" ] },
		ins        => [ "left", "right" ],
	},
);

my %binopx_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp", "flags" ], out => [ "gp" ] },
		ins        => [ "left", "carry" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp", "flags" ], out => [ "gp" ] },
		ins        => [ "left", "right", "carry" ],
	},
);


my %binopcczero_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "flags" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
);

my %div_operand_constructors = (
	imm => {
		attr       => "ir_entity *immediate_entity, int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
	},
	reg => {
		reg_req    => { in => [ "gp", "gp", "gp" ], out => [ "gp" ] },
	},
);

my %float_binop_constructors = (
	s => {
		reg_req => { in => [ "fp", "fp" ], out => [ "fp" ] },
		mode    => $mode_fp,
	},
	d => {
		reg_req => { in => [ "fp:a|2", "fp:a|2" ], out => [ "fp:a|2" ] },
		mode    => $mode_fp2,
	},
	q => {
		reg_req => { in => [ "fp:a|4", "fp:a|4" ], out => [ "fp:a|4" ] },
		mode    => $mode_fp4,
	}
);

my %float_unop_constructors = (
	s => {
		reg_req => { in => [ "fp" ], out => [ "fp" ] },
		mode    => $mode_fp,
	},
	d => {
		reg_req => { in => [ "fp:a|2" ], out => [ "fp:a|2" ] },
		mode    => $mode_fp2,
	},
	q => {
		reg_req => { in => [ "fp:a|4" ], out => [ "fp:a|4" ] },
		mode    => $mode_fp4,
	}
);

%nodes = (

Add => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. add %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

AddCC => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. addcc %S0, %R1I, %D0',
	outs         => [ "res", "flags" ],
	constructors => \%binopcc_operand_constructors,
},

AddX => {
	# At the moment not rematerializable because of assert in beflags.c/
	# (it claims that spiller can't rematerialize flag stuff correctly)
	#irn_flags    => [ "rematerializable" ],
	emit         => '. addx %S0, %R1I, %D0',
	constructors => \%binopx_operand_constructors,
	mode         => $mode_gp,
},

AddCC_t => {
	ins       => [ "left", "right" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

AddX_t => {
	ins       => [ "left", "right", "flags_input" ],
	attr_type => "",
	dump_func => "NULL",
},

Sub => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sub %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

SubCC => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. subcc %S0, %R1I, %D0',
	outs         => [ "res", "flags" ],
	constructors => \%binopcc_operand_constructors,
},

SubX => {
	# Not rematerializable (see AddX)
	emit         => '. subx %S0, %R1I, %D0',
	constructors => \%binopx_operand_constructors,
	mode         => $mode_gp,
},

SubCC_t => {
	ins       => [ "left", "right" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

SubX_t => {
	ins       => [ "left", "right", "flags_input" ],
	attr_type => "",
	dump_func => "NULL",
},

# Load / Store
Ld => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	constructors => {
		imm => {
			reg_req    => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
			ins        => [ "ptr", "mem" ],
			attr       => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
			custominit => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
		},
		reg => {
			reg_req    => { in => [ "gp", "gp", "none" ], out => [ "gp", "none" ] },
			ins        => [ "ptr", "ptr2", "mem" ],
			attr       => "ir_mode *ls_mode",
			custominit => "init_sparc_load_store_attributes(res, ls_mode, NULL, 0, false, true);",
		},
	},
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	attr_type => "sparc_load_store_attr_t",
	emit      => '. ld%LM [%S0%O1], %D0'
},

SetHi => {
	irn_flags  => [ "rematerializable" ],
	outs       => [ "res" ],
	mode       => $mode_gp,
	reg_req    => { in => [], out => [ "gp" ] },
	attr       => "ir_entity *entity, int32_t immediate_value",
	custominit => "sparc_set_attr_imm(res, entity, immediate_value);",
	emit       => '. sethi %HIM, %D0'
},

St => {
	op_flags  => [ "labeled" ],
	mode      => "mode_M",
	state     => "exc_pinned",
	constructors => {
		imm => {
			reg_req    => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
			ins        => [ "val", "ptr", "mem" ],
			attr       => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
			custominit => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
		},
		reg => {
			reg_req    => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
			ins        => [ "val", "ptr", "ptr2", "mem" ],
			attr       => "ir_mode *ls_mode",
			custominit => "init_sparc_load_store_attributes(res, ls_mode, NULL, 0, false, true);",
		},
	},
	ins       => [ "val", "ptr", "mem" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	emit      => '. st%SM %S0, [%S1%O2]'
},

Save => {
	emit      => '. save %S0, %R1I, %D0',
	outs      => [ "stack" ],
	ins       => [ "stack" ],
	constructors => {
		imm => {
			attr       => "ir_entity *immediate_entity, int32_t immediate_value",
			custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			reg_req    => { in => [ "sp" ], out => [ "sp:I|S" ] },
			ins        => [ "stack" ],
		},
		reg => {
			reg_req    => { in => [ "sp", "gp" ], out => [ "sp:I|S" ] },
			ins        => [ "stack", "increment" ],
		}
	},
	mode => $mode_gp,
},

Restore => {
	emit => '. restore %S0, %R1I, %D0',
	outs => [ "stack" ],
	ins  => [ "stack" ],
	constructors => {
		imm => {
			attr       => "ir_entity *immediate_entity, int32_t immediate_value",
			custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			reg_req    => { in => [ "sp" ], out => [ "sp:I|S" ] },
			ins        => [ "stack" ],
		},
		reg => {
			reg_req    => { in => [ "sp", "gp" ], out => [ "sp:I|S" ] },
			ins        => [ "stack", "increment" ],
		}
	},
	mode => $mode_gp,
},

RestoreZero => {
	reg_req => { in => [ "frame_pointer" ], out => [ "sp:I|S" ] },
	ins     => [ "frame_pointer" ],
	outs    => [ "stack" ],
	emit    => '. restore',
	mode    => $mode_gp,
},

SubSP => {
	reg_req => { in => [ "sp", "gp" ], out => [ "sp:I|S" ] },
	ins     => [ "stack", "size" ],
	outs    => [ "stack" ],
	emit    => ". sub %S0, %S1, %D0\n",
	mode    => $mode_gp,
},

AddSP => {
	reg_req => { in => [ "sp", "gp" ], out => [ "sp:I|S" ] },
	ins     => [ "stack", "size" ],
	outs    => [ "stack" ],
	emit    => ". add %S0, %S1, %D0\n",
	mode    => $mode_gp,
},

FrameAddr => {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	attr       => "ir_entity *entity, int32_t offset",
	reg_req    => { in => [ "gp" ], out => [ "gp" ] },
	ins        => [ "base" ],
	attr_type  => "sparc_attr_t",
	custominit => "sparc_set_attr_imm(res, entity, offset);",
	mode       => $mode_gp,
},

Bicc => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr_type => "sparc_jmp_cond_attr_t",
	attr      => "ir_relation relation, bool is_unsigned",
	init_attr => "\tinit_sparc_jmp_cond_attr(res, relation, is_unsigned);",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
	ins       => [ "flags" ],
	outs      => [ "false", "true" ],
},

fbfcc => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr_type => "sparc_jmp_cond_attr_t",
	attr      => "ir_relation relation",
	init_attr => "\tinit_sparc_jmp_cond_attr(res, relation, false);",
	reg_req   => { in => [ "fpflags" ], out => [ "none", "none" ] },
	ins       => [ "flags" ],
	outs      => [ "false", "true" ],
},

Ba => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Start => {
	state     => "pinned",
	out_arity => "variable",
	ins       => [],
},

# This is a JumpLink instruction, but with the addition that you can add custom
# register constraints to model your calling conventions
Return => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	arity     => "variable",
	mode      => "mode_X",
	constructors => {
		imm => {
			attr       => "ir_entity *entity, int32_t offset",
			custominit => "\tsparc_set_attr_imm(res, entity, offset);",
			arity     => "variable",
			reg_req   => { out => [ "none" ] },
		},
		reg => {
			arity     => "variable",
			reg_req   => { out => [ "none" ] },
		}
	},
},

Call => {
	irn_flags => [ "modifies_flags", "modifies_fp_flags" ],
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	constructors => {
		imm => {
			attr       => "ir_entity *entity, int32_t offset, bool aggregate_return",
			custominit => "\tsparc_set_attr_imm(res, entity, offset);".
			              "\tif (aggregate_return) arch_add_irn_flags(res, sparc_arch_irn_flag_aggregate_return);",
			arity     => "variable",
			out_arity => "variable",
		},
		reg => {
			attr       => "bool aggregate_return",
			arity      => "variable",
			out_arity  => "variable",
			custominit => "\tif (aggregate_return) arch_add_irn_flags(res, sparc_arch_irn_flag_aggregate_return);",
		}
	},
},

Cmp => {  # aka SubccZero
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. cmp %S0, %R1I',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

SwitchJmp => {
	op_flags     => [ "labeled", "cfopcode", "forking" ],
	state        => "pinned",
	mode         => "mode_T",
	reg_req      => { in => [ "gp" ], out => [ ] },
	attr_type    => "sparc_switch_jmp_attr_t",
	attr         => "long default_pn, ir_entity *jump_table",
	init_attr => "info->out_infos = NULL;", # XXX ugly hack for out requirements
},

Sll => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sll %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

Srl => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. srl %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

Sra => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sra %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

And => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. and %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

AndCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. andcc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

AndN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. andn %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

AndNCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. andncc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Or => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. or %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

OrCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. orcc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

OrN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. orn %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

OrNCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. orncc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Xor => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. xor %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

XorCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. xorcc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

XNor => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. xnor %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

XNorCCZero => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. xnorcc %S0, %R1I, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Mul => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. smul %S0, %R1I, %D0',
	constructors => \%binop_operand_constructors,
},

SMulh => {
	irn_flags    => [ "rematerializable" ],
	outs         => [ "low", "high" ],
	constructors => \%binop_operand_constructors,
},

UMulh => {
	irn_flags    => [ "rematerializable" ],
	outs         => [ "low", "high" ],
	constructors => \%binop_operand_constructors,
},

SDiv => {
	irn_flags    => [ "rematerializable" ],
	state        => "exc_pinned",
	ins          => [ "dividend_high", "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%div_operand_constructors,
},

UDiv => {
	irn_flags    => [ "rematerializable" ],
	state        => "exc_pinned",
	ins          => [ "dividend_high", "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%div_operand_constructors,
},

fcmp => {
	irn_flags => [ "rematerializable", "modifies_fp_flags" ],
	emit      => '. fcmp%FPM %S0, %S1',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	mode      => $mode_fpflags,
	constructors => {
		s => {
			reg_req => { in => [ "fp", "fp" ], out => [ "fpflags" ] },
		},
		d => {
			reg_req => { in => [ "fp:a|2", "fp:a|2" ], out => [ "fpflags" ] },
		},
		q => {
			reg_req => { in => [ "fp:a|4", "fp:a|4" ], out => [ "fpflags" ] },
		},
	},
},

fadd => {
	op_flags     => [ "commutative" ],
	irn_flags    => [ "rematerializable" ],
	emit         => '. fadd%FPM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fsub => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. fsub%FPM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fmul => {
	irn_flags    => [ "rematerializable" ],
	op_flags     => [ "commutative" ],
	emit         =>'. fmul%FPM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fdiv => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. fdiv%FPM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	outs         => [ "res", "M" ],
	constructors => {
		s => {
			reg_req => { in => [ "fp", "fp" ], out => [ "fp", "none" ] },
		},
		d => {
			reg_req => { in => [ "fp:a|2", "fp:a|2" ], out => [ "fp:a|2", "none" ] },
		},
		q => {
			reg_req => { in => [ "fp:a|4", "fp:a|4" ], out => [ "fp:a|4", "none" ] },
		}
	},
},

fneg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	# note that we only need the first register even for wide-values
	emit      => '. fnegs %S0, %D0',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	ins          => [ "val" ],
	constructors => \%float_unop_constructors,
},

"fabs" => {
	irn_flags    => [ "rematerializable" ],
	# note that we only need the first register even for wide-values
	emit         => '. fabs %S0, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "val" ],
	constructors => \%float_unop_constructors,
},

fftof => {
	irn_flags => [ "rematerializable" ],
	emit      => '. f%FCONVS%.to%FCONVD %S0, %D0',
	attr_type => "sparc_fp_conv_attr_t",
	attr      => "ir_mode *src_mode, ir_mode *dest_mode",
	constructors => {
		s_d => {
			reg_req => { in => [ "fp" ], out => [ "fp:a|2" ] },
			mode    => $mode_fp2,
		},
		s_q => {
			reg_req => { in => [ "fp" ], out => [ "fp:a|2" ] },
			mode    => $mode_fp4,
		},
		d_s => {
			reg_req => { in => [ "fp:a|2" ], out => [ "fp" ] },
			mode    => $mode_fp,
		},
		d_q => {
			reg_req => { in => [ "fp:a|2" ], out => [ "fp:a|4" ] },
			mode    => $mode_fp4,
		},
		q_s => {
			reg_req => { in => [ "fp:a|4" ], out => [ "fp" ] },
			mode    => $mode_fp,
		},
		q_d => {
			reg_req => { in => [ "fp:a|4" ], out => [ "fp:a|2" ] },
			mode    => $mode_fp2,
		},
	},
},

fitof => {
	irn_flags => [ "rematerializable" ],
	emit      => '. fito%FPM %S0, %D0',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	constructors => {
		s => {
			reg_req => { in => [ "fp" ], out => [ "fp" ] },
			mode    => $mode_fp,
		},
		d => {
			reg_req => { in => [ "fp" ], out => [ "fp:a|2" ] },
			mode    => $mode_fp2,
		},
		q => {
			reg_req => { in => [ "fp" ], out => [ "fp:a|4" ] },
			mode    => $mode_fp4,
		},
	},
},

fftoi => {
	irn_flags => [ "rematerializable" ],
	emit      => '. f%FPM%.toi %S0, %D0',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	mode      => $mode_gp,
	constructors => {
		s => {
			reg_req => { in => [ "fp" ], out => [ "fp" ] },
		},
		d => {
			reg_req => { in => [ "fp:a|2" ], out => [ "fp" ] },
		},
		q => {
			reg_req => { in => [ "fp:a|4" ], out => [ "fp" ] },
		},
	},
},

Ldf => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	constructors => {
		s => {
			reg_req => { in => [ "gp", "none" ], out => [ "fp", "none" ] },
		},
		d => {
			reg_req => { in => [ "gp", "none" ], out => [ "fp:a|2", "none" ] },
		},
		q => {
			reg_req => { in => [ "gp", "none" ], out => [ "fp:a|4", "none" ] },
		},
	},
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
	custominit => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
	emit      => '. ld%FLSM [%S0%O1], %D0'
},

Stf => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	constructors => {
		s => {
			reg_req => { in => [ "fp",     "gp", "none" ], out => [ "none" ] },
		},
		d => {
			reg_req => { in => [ "fp:a|2", "gp", "none" ], out => [ "none" ] },
		},
		q => {
			reg_req => { in => [ "fp:a|4", "gp", "none" ], out => [ "none" ] },
		},
	},
	ins       => [ "val", "ptr", "mem" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
	custominit => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
	emit      => '. st%FLSM %S0, [%S1%O2]',
	mode      => 'mode_M',
},

); # end of %nodes
