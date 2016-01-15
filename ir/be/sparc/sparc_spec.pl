# Creation: 2006/02/13

$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fpflags = "mode_Bu";
$mode_fp      = "mode_F";
$mode_fp2     = "mode_D";
$mode_fp4     = "mode_Q";

# available SPARC registers: 8 globals, 24 window regs (8 ins, 8 outs, 8 locals)
%reg_classes = (
	gp => [
		# Note: locals come first here since they're usually constrained last
		# (by calls and others)
		{ name => "l0", dwarf => 16 },
		{ name => "l3", dwarf => 19 },
		{ name => "l4", dwarf => 20 },
		{ name => "l5", dwarf => 21 },
		{ name => "l6", dwarf => 22 },
		{ name => "l7", dwarf => 23 },
		{ name => "l1", dwarf => 17 },
		{ name => "l2", dwarf => 18 },
		{ name => "l8", dwarf => 64 },
		{ name => "l9", dwarf => 65 },
		{ name => "l10", dwarf => 66 },
		{ name => "l11", dwarf => 67 },
		{ name => "l12", dwarf => 68 },
		{ name => "l13", dwarf => 69 },
		{ name => "l14", dwarf => 70 },
		{ name => "l15", dwarf => 71 },

		{ name => "g0", dwarf => 0 },
		{ name => "g1", dwarf => 1 },
		{ name => "g2", dwarf => 2 },
		{ name => "g3", dwarf => 3 },
		{ name => "g4", dwarf => 4 },
		{ name => "g5", dwarf => 5 },
		{ name => "g6", dwarf => 6 },
		{ name => "g7", dwarf => 7 },
		{ name => "g8", dwarf => 72 },
		{ name => "g9", dwarf => 73 },
		{ name => "g10", dwarf => 74 },
		{ name => "g11", dwarf => 75 },
		{ name => "g12", dwarf => 76 },
		{ name => "g13", dwarf => 77 },
		{ name => "g14", dwarf => 78 },
		{ name => "g15", dwarf => 79 },

		{ name => "o0", dwarf => 8 },
		{ name => "o1", dwarf => 9 },
		{ name => "o2", dwarf => 10 },
		{ name => "o3", dwarf => 11 },
		{ name => "o4", dwarf => 12 },
		{ name => "o5", dwarf => 13 },
		{ name => "sp", dwarf => 14 },
		{ name => "o7", dwarf => 15 },
		{ name => "o8", dwarf => 80 },
		{ name => "o9", dwarf => 81 },
		{ name => "o10", dwarf => 82 },
		{ name => "o11", dwarf => 83 },
		{ name => "o12", dwarf => 84 },
		{ name => "o13", dwarf => 85 },
		{ name => "o14", dwarf => 86 },
		{ name => "o15", dwarf => 87 },

		{ name => "i0", dwarf => 24 },
		{ name => "i1", dwarf => 25 },
		{ name => "i2", dwarf => 26 },
		{ name => "i3", dwarf => 27 },
		{ name => "i4", dwarf => 28 },
		{ name => "i5", dwarf => 29 },
		{ name => "frame_pointer", dwarf => 30, realname => "fp" },
		{ name => "i7", dwarf => 30 },
		{ name => "i8", dwarf => 88 },
		{ name => "i9", dwarf => 89 },
		{ name => "i10", dwarf => 90 },
		{ name => "i11", dwarf => 91 },
		{ name => "i12", dwarf => 92 },
		{ name => "i13", dwarf => 93 },
		{ name => "i14", dwarf => 95 },
		{ name => "i15", dwarf => 96 },
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
		{ name => "f0",  dwarf => 32 },
		{ name => "f1",  dwarf => 33 },
		{ name => "f2",  dwarf => 34 },
		{ name => "f3",  dwarf => 35 },
		{ name => "f4",  dwarf => 36 },
		{ name => "f5",  dwarf => 37 },
		{ name => "f6",  dwarf => 38 },
		{ name => "f7",  dwarf => 39 },
		{ name => "f8",  dwarf => 40 },
		{ name => "f9",  dwarf => 41 },
		{ name => "f10", dwarf => 42 },
		{ name => "f11", dwarf => 43 },
		{ name => "f12", dwarf => 44 },
		{ name => "f13", dwarf => 45 },
		{ name => "f14", dwarf => 46 },
		{ name => "f15", dwarf => 47 },
		{ name => "f16", dwarf => 48 },
		{ name => "f17", dwarf => 49 },
		{ name => "f18", dwarf => 50 },
		{ name => "f19", dwarf => 51 },
		{ name => "f20", dwarf => 52 },
		{ name => "f21", dwarf => 53 },
		{ name => "f22", dwarf => 54 },
		{ name => "f23", dwarf => 55 },
		{ name => "f24", dwarf => 56 },
		{ name => "f25", dwarf => 57 },
		{ name => "f26", dwarf => 58 },
		{ name => "f27", dwarf => 59 },
		{ name => "f28", dwarf => 60 },
		{ name => "f29", dwarf => 61 },
		{ name => "f30", dwarf => 62 },
		{ name => "f31", dwarf => 63 },
		{ mode => $mode_fp }
	]
); # %reg_classes

$default_attr_type = "sparc_attr_t";
$default_copy_attr = "sparc_copy_attr";

%init_attr = (
	sparc_attr_t             => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);",
	sparc_load_store_attr_t  => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);",
	sparc_jmp_cond_attr_t    => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);",
	sparc_switch_jmp_attr_t  => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);\n".
	                            "\tinit_sparc_switch_jmp_attributes(res, table, jump_table);\n",
	sparc_fp_attr_t          => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);\n".
	                            "\tinit_sparc_fp_attributes(res, fp_mode);\n",
	sparc_fp_conv_attr_t     => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);".
	                            "\tinit_sparc_fp_conv_attributes(res, src_mode, dest_mode);\n",
	sparc_permi_attr_t       => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);\n".
	                            "\tinit_sparc_permi_attributes(res, false);\n",
	sparc_permi23_attr_t     => "\tinit_sparc_attributes(res, irn_flags_, in_reqs, n_res);\n".
	                            "\tinit_sparc_permi23_attributes(res, false, false);\n",
);

%compare_attr = (
	sparc_attr_t            => "cmp_attr_sparc",
	sparc_fp_attr_t         => "cmp_attr_sparc_fp",
	sparc_fp_conv_attr_t    => "cmp_attr_sparc_fp_conv",
	sparc_jmp_cond_attr_t   => "cmp_attr_sparc_jmp_cond",
	sparc_load_store_attr_t => "cmp_attr_sparc_load_store",
	sparc_switch_jmp_attr_t => "cmp_attr_sparc",
	sparc_permi_attr_t      => "cmp_attr_sparc_permi",
	sparc_permi23_attr_t    => "cmp_attr_sparc_permi23",
);

%custom_irn_flags = (
	has_delay_slot    => "(arch_irn_flags_t)sparc_arch_irn_flag_has_delay_slot",
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
	emit         => 'add %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

AddCC => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'addcc %S0, %SI1, %D0',
	outs         => [ "res", "flags" ],
	constructors => \%binopcc_operand_constructors,
},

AddCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'addcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

AddX => {
	# At the moment not rematerializable because of assert in beflags.c/
	# (it claims that spiller can't rematerialize flag stuff correctly)
	#irn_flags    => [ "rematerializable" ],
	emit         => 'addx %S0, %SI1, %D0',
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
	emit         => 'sub %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

SubCC => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'subcc %S0, %SI1, %D0',
	outs         => [ "res", "flags" ],
	constructors => \%binopcc_operand_constructors,
},

SubCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'subcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

SubX => {
	# Not rematerializable (see AddX)
	emit         => 'subx %S0, %SI1, %D0',
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
	op_flags  => [ "uses_memory" ],
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
	emit      => 'ld%ML [%S0%O1], %D0'
},

SetHi => {
	irn_flags  => [ "rematerializable" ],
	outs       => [ "res" ],
	mode       => $mode_gp,
	reg_req    => { in => [], out => [ "gp" ] },
	attr       => "ir_entity *entity, int32_t immediate_value",
	custominit => "sparc_set_attr_imm(res, entity, immediate_value);",
	emit       => 'sethi %H, %D0'
},

St => {
	op_flags  => [ "uses_memory" ],
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
	emit      => 'st%MS %S0, [%S1%O2]'
},

Save => {
	emit      => 'save %S0, %SI1, %D0',
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
	outs => [ "stack", "res" ],
	constructors => {
		imm => {
			attr       => "ir_entity *immediate_entity, int32_t immediate_value",
			custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			reg_req    => { in => [ "sp", "frame_pointer", "gp" ], out => [ "sp:I|S", "gp" ] },
			ins        => [ "stack", "frame_pointer", "left" ],
		},
		reg => {
			reg_req    => { in => [ "sp", "frame_pointer", "gp", "gp" ], out => [ "sp:I|S", "gp" ] },
			ins        => [ "stack", "frame_pointer", "left", "right" ],
		}
	},
},

RestoreZero => {
	reg_req => { in => [ "sp", "frame_pointer" ], out => [ "sp:I|S" ] },
	ins     => [ "stack", "frame_pointer" ],
	outs    => [ "stack" ],
	emit    => 'restore',
	mode    => $mode_gp,
},

SubSP => {
	constructors => {
		imm => {
			attr       => "ir_entity *immediate_entity, int32_t immediate_value",
			custominit => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			reg_req    => { in => [ "sp", "none" ], out => [ "sp:I|S", "gp", "none" ] },
			ins        => [ "stack", "mem" ],
		},
		reg => {
			reg_req    => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "gp", "none" ] },
			ins        => [ "stack", "size", "mem" ],
		}
	},
	outs    => [ "stack", "addr", "M" ],
},

AddSP => {
	reg_req => { in => [ "sp", "gp" ], out => [ "sp:I|S" ] },
	ins     => [ "stack", "size" ],
	outs    => [ "stack" ],
	emit    => "add %S0, %S1, %D0\n",
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
	op_flags  => [ "cfopcode", "forking" ],
	irn_flags => [ "has_delay_slot" ],
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
	op_flags  => [ "cfopcode", "forking" ],
	irn_flags => [ "has_delay_slot" ],
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
	# Note: has_delay_slot depends on wether it is a fallthrough or not, so we
	# have special code for this in sparc_emitter
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

# This is a Jump instruction, but with the addition that you can add custom
# register constraints to model your calling conventions
Return => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "has_delay_slot" ],
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

# This is a JumpLink instruction, but with the addition that you can add custom
# register constraints to model your calling conventions
Call => {
	irn_flags => [ "has_delay_slot" ],
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	constructors => {
		imm => {
			attr       => "ir_entity *entity, int32_t offset, bool aggregate_return",
			custominit => "\tsparc_set_attr_imm(res, entity, offset);".
			              "\tif (aggregate_return) arch_add_irn_flags(res, (arch_irn_flags_t)sparc_arch_irn_flag_aggregate_return);",
			arity     => "variable",
			out_arity => "variable",
		},
		reg => {
			attr       => "bool aggregate_return",
			arity      => "variable",
			out_arity  => "variable",
			custominit => "\tif (aggregate_return) arch_add_irn_flags(res, (arch_irn_flags_t)sparc_arch_irn_flag_aggregate_return);",
		}
	},
},

Cmp => {  # aka SubccZero
	irn_flags    => [ "rematerializable" ],
	emit         => 'cmp %S0, %SI1',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

SwitchJmp => {
	op_flags     => [ "cfopcode", "forking" ],
	irn_flags    => [ "has_delay_slot" ],
	state        => "pinned",
	mode         => "mode_T",
	reg_req      => { in => [ "gp" ], out => [ ] },
	out_arity    => "variable",
	attr_type    => "sparc_switch_jmp_attr_t",
	attr         => "const ir_switch_table *table, ir_entity *jump_table",
},

Sll => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'sll %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

Srl => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'srl %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

Sra => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'sra %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

And => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'and %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

AndCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'andcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

AndN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => 'andn %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

AndNCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'andncc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Or => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'or %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

OrCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'orcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

OrN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => 'orn %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

OrNCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'orncc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Xor => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'xor %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

XorCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'xorcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

XNor => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => 'xnor %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

XNorCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'xnorcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

Mul => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => 'smul %S0, %SI1, %D0',
	constructors => \%binop_operand_constructors,
},

MulCCZero => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'smulcc %S0, %SI1, %%g0',
	mode         => $mode_flags,
	constructors => \%binopcczero_operand_constructors,
},

SMulh => {
	irn_flags    => [ "rematerializable" ],
	outs         => [ "low", "high" ],
	emit         => 'smul %S0, %SI1, %D0\n'.
	                'mov %%y, %D0',
	constructors => \%binop_operand_constructors,
},

UMulh => {
	irn_flags    => [ "rematerializable" ],
	outs         => [ "low", "high" ],
	emit         => 'umul %S0, %SI1, %D0\n'.
	                'mov %%y, %D0',
	constructors => \%binop_operand_constructors,
},

SDiv => {
	irn_flags    => [ "rematerializable", "has_delay_slot" ],
	state        => "exc_pinned",
	ins          => [ "dividend_high", "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%div_operand_constructors,
},

UDiv => {
	irn_flags    => [ "rematerializable", "has_delay_slot" ],
	state        => "exc_pinned",
	ins          => [ "dividend_high", "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%div_operand_constructors,
},

Permi => {
	irn_flags => [ "rematerializable" ],
	attr_type => "sparc_permi_attr_t",
	constructors => {
		cycle => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi_attributes(res, true);",
		},
		chain => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi_attributes(res, false);",
		},
	},
},

Permi23 => {
	irn_flags => [ "rematerializable" ],
	attr_type => "sparc_permi23_attr_t",
	constructors => {
		cycle_cycle => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi23_attributes(res, true, true);",
		},
		cycle_chain => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi23_attributes(res, true, false);",
		},
		chain_cycle => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi23_attributes(res, false, true);",
		},
		chain_chain => {
			arity     => "variable",
			out_arity => "variable",
			custominit => "init_sparc_permi23_attributes(res, false, false);",
		},
	},
},

fcmp => {
	irn_flags => [ "rematerializable" ],
	emit      => 'fcmp%FM %S0, %S1',
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
	emit         => 'fadd%FM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fsub => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'fsub%FM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fmul => {
	irn_flags    => [ "rematerializable" ],
	op_flags     => [ "commutative" ],
	emit         =>'fmul%FM %S0, %S1, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	constructors => \%float_binop_constructors,
},

fdiv => {
	irn_flags    => [ "rematerializable" ],
	emit         => 'fdiv%FM %S0, %S1, %D0',
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
	emit      => 'fnegs %S0, %D0',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	ins          => [ "val" ],
	constructors => \%float_unop_constructors,
},

"fabs" => {
	irn_flags    => [ "rematerializable" ],
	# note that we only need the first register even for wide-values
	emit         => 'fabs %S0, %D0',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "val" ],
	constructors => \%float_unop_constructors,
},

fftof => {
	irn_flags => [ "rematerializable" ],
	emit      => 'f%FSto%FD %S0, %D0',
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
	emit      => 'fito%FM %S0, %D0',
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
	emit      => 'f%FMtoi %S0, %D0',
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
	op_flags  => [ "uses_memory" ],
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
	emit      => 'ld%ML [%S0%O1], %D0'
},

Stf => {
	op_flags  => [ "uses_memory" ],
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
	emit      => 'st%MS %S0, [%S1%O2]',
	mode      => 'mode_M',
},

); # end of %nodes
