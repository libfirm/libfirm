$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fpflags = "mode_Bu";
$mode_fp      = "mode_F";
$mode_fp2     = "mode_D";
$mode_fp4     = "sparc_mode_Q";

# available SPARC registers: 8 globals, 24 window regs (8 ins, 8 outs, 8 locals)
%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
			# Note: locals come first here since they're usually constrained last
			# (by calls and others)
			{ name => "l0", encoding => 16, dwarf => 16 },
			{ name => "l1", encoding => 17, dwarf => 17 },
			{ name => "l2", encoding => 18, dwarf => 18 },
			{ name => "l3", encoding => 19, dwarf => 19 },
			{ name => "l4", encoding => 20, dwarf => 20 },
			{ name => "l5", encoding => 21, dwarf => 21 },
			{ name => "l6", encoding => 22, dwarf => 22 },
			{ name => "l7", encoding => 23, dwarf => 23 },

			{ name => "g0", encoding =>  0, dwarf => 0 },
			{ name => "g1", encoding =>  1, dwarf => 1 },
			{ name => "g2", encoding =>  2, dwarf => 2 },
			{ name => "g3", encoding =>  3, dwarf => 3 },
			{ name => "g4", encoding =>  4, dwarf => 4 },
			{ name => "g5", encoding =>  5, dwarf => 5 },
			{ name => "g6", encoding =>  6, dwarf => 6 },
			{ name => "g7", encoding =>  7, dwarf => 7 },

			{ name => "o0", encoding =>  8, dwarf => 8 },
			{ name => "o1", encoding =>  9, dwarf => 9 },
			{ name => "o2", encoding => 10, dwarf => 10 },
			{ name => "o3", encoding => 11, dwarf => 11 },
			{ name => "o4", encoding => 12, dwarf => 12 },
			{ name => "o5", encoding => 13, dwarf => 13 },
			{ name => "sp", encoding => 14, dwarf => 14 },
			{ name => "o7", encoding => 15, dwarf => 15 },

			{ name => "i0", encoding => 24, dwarf => 24 },
			{ name => "i1", encoding => 25, dwarf => 25 },
			{ name => "i2", encoding => 26, dwarf => 26 },
			{ name => "i3", encoding => 27, dwarf => 27 },
			{ name => "i4", encoding => 28, dwarf => 28 },
			{ name => "i5", encoding => 29, dwarf => 29 },
			{ name => "fp", encoding => 30, dwarf => 30 },
			{ name => "i7", encoding => 31, dwarf => 31 },
		]
	},
	fpflags => {
		flags => "manual_ra",
		mode => $mode_fpflags,
		registers => [ { name => "fsr" }, ]
	},
	flags => {
		flags => "manual_ra",
		mode => $mode_flags,
		registers => [ { name => "psr" }, ]
	},
	mul_div_high_res => {
		flags => "manual_ra",
		mode => $mode_gp,
		registers => [ { name => "y" }, ]
	},
	fp => {
		mode => $mode_fp,
		registers => [
			{ name => "f0",  encoding =>  0, dwarf => 32 },
			{ name => "f1",  encoding =>  1, dwarf => 33 },
			{ name => "f2",  encoding =>  2, dwarf => 34 },
			{ name => "f3",  encoding =>  3, dwarf => 35 },
			{ name => "f4",  encoding =>  4, dwarf => 36 },
			{ name => "f5",  encoding =>  5, dwarf => 37 },
			{ name => "f6",  encoding =>  6, dwarf => 38 },
			{ name => "f7",  encoding =>  7, dwarf => 39 },
			{ name => "f8",  encoding =>  8, dwarf => 40 },
			{ name => "f9",  encoding =>  9, dwarf => 41 },
			{ name => "f10", encoding => 10, dwarf => 42 },
			{ name => "f11", encoding => 11, dwarf => 43 },
			{ name => "f12", encoding => 12, dwarf => 44 },
			{ name => "f13", encoding => 13, dwarf => 45 },
			{ name => "f14", encoding => 14, dwarf => 46 },
			{ name => "f15", encoding => 15, dwarf => 47 },
			{ name => "f16", encoding => 16, dwarf => 48 },
			{ name => "f17", encoding => 17, dwarf => 49 },
			{ name => "f18", encoding => 18, dwarf => 50 },
			{ name => "f19", encoding => 19, dwarf => 51 },
			{ name => "f20", encoding => 20, dwarf => 52 },
			{ name => "f21", encoding => 21, dwarf => 53 },
			{ name => "f22", encoding => 22, dwarf => 54 },
			{ name => "f23", encoding => 23, dwarf => 55 },
			{ name => "f24", encoding => 24, dwarf => 56 },
			{ name => "f25", encoding => 25, dwarf => 57 },
			{ name => "f26", encoding => 26, dwarf => 58 },
			{ name => "f27", encoding => 27, dwarf => 59 },
			{ name => "f28", encoding => 28, dwarf => 60 },
			{ name => "f29", encoding => 29, dwarf => 61 },
			{ name => "f30", encoding => 30, dwarf => 62 },
			{ name => "f31", encoding => 31, dwarf => 63 },
		]
	}
);

%init_attr = (
	sparc_attr_t            => "",
	sparc_load_store_attr_t => "",
	sparc_jmp_cond_attr_t   => "init_sparc_jmp_cond_attr(res, relation, is_unsigned);",
	sparc_switch_jmp_attr_t => "be_switch_attr_init(res, &attr->swtch, table, jump_table);",
	sparc_fp_attr_t         => "init_sparc_fp_attributes(res, fp_mode);",
	sparc_fp_conv_attr_t    => "init_sparc_fp_conv_attributes(res, src_mode, dest_mode);",
);

%custom_irn_flags = (
	has_delay_slot => "(arch_irn_flags_t)sparc_arch_irn_flag_has_delay_slot",
);

my $binop_operand = {
	irn_flags    => [ "rematerializable" ],
	out_reqs     => [ "gp" ],
	emit         => "{name} %S0, %SI1, %D0",
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $binopcc_operand = {
	irn_flags    => [ "rematerializable" ],
	out_reqs     => [ "gp", "flags" ],
	outs         => [ "res", "flags" ],
	emit         => "{name} %S0, %SI1, %D0",
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $binopx_operand = {
	# At the moment not rematerializable because of assert in beflags.c/
	# (it claims that spiller can't rematerialize flag stuff correctly)
	#irn_flags    => [ "rematerializable" ],
	out_reqs     => [ "gp" ],
	emit         => "{name} %S0, %SI1, %D0",
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "gp", "flags" ],
			ins     => [ "left", "carry" ],
		},
		reg => {
			in_reqs => [ "gp", "gp", "flags" ],
			ins     => [ "left", "right", "carry" ],
		},
	},
};

my $binopcczero_operand = {
	irn_flags    => [ "rematerializable" ],
	out_reqs     => [ "flags" ],
	emit         => "{name} %S0, %SI1, %%g0",
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $div_operand = {
	irn_flags    => [ "rematerializable", "has_delay_slot" ],
	state        => "exc_pinned",
	ins          => [ "mem", "dividend_high", "dividend_low", "divisor" ],
	out_reqs     => [ "gp", "mem" ],
	outs         => [ "res", "M" ],
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "mem", "gp", "gp" ],
		},
		reg => {
			in_reqs    => [ "mem", "gp", "gp", "gp" ],
		},
	},
};

my $float_binop = {
	irn_flags    => [ "rematerializable" ],
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	emit         => "{name}%FM %S0, %S1, %D0",
	constructors => {
		s => {
			in_reqs  => [ "cls-fp", "cls-fp" ],
			out_reqs => [ "cls-fp" ],
		},
		d => {
			in_reqs  => [ "cls-fp:2", "cls-fp:2" ],
			out_reqs => [ "cls-fp:2" ],
			mode     => $mode_fp2,
		},
		q => {
			in_reqs  => [ "cls-fp:4", "cls-fp:4" ],
			out_reqs => [ "cls-fp:4" ],
			mode     => $mode_fp4,
		}
	},
};

my $float_unop = {
	irn_flags    => [ "rematerializable" ],
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "val" ],
	# note that we only need the first register even for wide-values
	emit         => "{name}s %S0, %D0",
	constructors => {
		s => {
			in_reqs  => [ "cls-fp" ],
			out_reqs => [ "cls-fp" ],
		},
		d => {
			in_reqs  => [ "cls-fp:2" ],
			out_reqs => [ "cls-fp:2" ],
			mode     => $mode_fp2,
		},
		q => {
			in_reqs  => [ "cls-fp:4" ],
			out_reqs => [ "cls-fp:4" ],
			mode     => $mode_fp4,
		}
	},
};

my $branchcc = {
	op_flags  => [ "cfopcode", "forking" ],
	irn_flags => [ "fallthrough", "has_delay_slot" ],
	state     => "pinned",
	attr_type => "sparc_jmp_cond_attr_t",
	ins       => [ "flags" ],
	out_reqs  => [ "exec", "exec" ],
	outs      => [ "false", "true" ],
};

my $xop = {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	out_reqs => [ "exec" ],
};

%nodes = (

Add => { template => $binop_operand },

AddCC => { template => $binopcc_operand },

AddX => { template => $binopx_operand },

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

Sub => { template => $binop_operand },

SubCC => { template => $binopcc_operand },

SubX => { template => $binopx_operand },

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
	state     => "exc_pinned",
	constructors => {
		imm => {
			in_reqs => [ "mem", "gp" ],
			ins     => [ "mem", "ptr" ],
			attr    => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
			init    => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
		},
		reg => {
			in_reqs => [ "mem", "gp", "gp" ],
			ins     => [ "mem", "ptr", "ptr2" ],
			attr    => "ir_mode *ls_mode",
			init    => "init_sparc_load_store_attributes(res, ls_mode, NULL, 0, false, true);",
		},
	},
	ins       => [ "mem", "ptr" ],
	out_reqs  => [ "gp", "mem" ],
	outs      => [ "res", "M" ],
	attr_type => "sparc_load_store_attr_t",
	emit      => "ld%ML %O1, %D0"
},

SetHi => {
	irn_flags => [ "rematerializable" ],
	outs      => [ "res" ],
	out_reqs  => [ "gp" ],
	attr      => "ir_entity *entity, int32_t immediate_value",
	init      => "sparc_set_attr_imm(res, entity, immediate_value);",
	emit      => "sethi %H, %D0"
},

St => {
	state     => "exc_pinned",
	constructors => {
		imm => {
			in_reqs => [ "mem", "gp", "gp" ],
			ins     => [ "mem", "val", "ptr" ],
			attr    => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
			init    => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
		},
		reg => {
			in_reqs => [ "mem", "gp", "gp", "gp" ],
			ins     => [ "mem", "val", "ptr", "ptr2" ],
			attr    => "ir_mode *ls_mode",
			init    => "init_sparc_load_store_attributes(res, ls_mode, NULL, 0, false, true);",
		},
	},
	out_reqs  => [ "mem" ],
	ins       => [ "mem", "val", "ptr" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	emit      => "st%MS %S1, %O2"
},

Save => {
	irn_flags => [ "schedule_first" ],
	emit      => "save %S0, %SI1, %D0",
	out_reqs  => [ "sp:I" ],
	outs      => [ "stack" ],
	ins       => [ "stack" ],
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "sp" ],
			ins     => [ "stack" ],
		},
		reg => {
			in_reqs    => [ "sp", "gp" ],
			ins        => [ "stack", "increment" ],
		}
	},
},

Restore => {
	out_reqs => [ "sp:I", "gp" ],
	outs     => [ "stack", "res" ],
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "sp", "reg-fp", "gp" ],
			ins     => [ "stack", "frame_pointer", "left" ],
		},
		reg => {
			in_reqs => [ "sp", "reg-fp", "gp", "gp" ],
			ins     => [ "stack", "frame_pointer", "left", "right" ],
		}
	},
},

RestoreZero => {
	in_reqs  => [ "sp", "reg-fp" ],
	out_reqs => [ "sp:I" ],
	ins      => [ "stack", "frame_pointer" ],
	outs     => [ "stack" ],
	emit     => "restore",
},

SubSP => {
	constructors => {
		imm => {
			attr    => "ir_entity *immediate_entity, int32_t immediate_value",
			init    => "sparc_set_attr_imm(res, immediate_entity, immediate_value);",
			in_reqs => [ "mem", "sp" ],
			ins     => [ "mem", "stack" ],
		},
		reg => {
			in_reqs => [ "mem", "sp", "gp" ],
			ins     => [ "mem", "stack", "size" ],
		}
	},
	ins      => [ "mem", "stack" ],
	out_reqs => [ "sp:I", "gp", "mem" ],
	outs     => [ "stack", "addr", "M" ],
},

AddSP => {
	in_reqs  => [ "sp", "gp" ],
	out_reqs => [ "sp:I" ],
	ins      => [ "stack", "size" ],
	outs     => [ "stack" ],
	emit     => "add %S0, %S1, %D0\n",
},

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int32_t offset",
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "base" ],
	init      => "sparc_set_attr_imm(res, entity, offset);",
},

Bicc => {
	template => $branchcc,
	attr     => "ir_relation relation, bool is_unsigned",
	in_reqs  => [ "flags" ],
},

fbfcc => {
	template => $branchcc,
	attr     => "ir_relation relation",
	fixed    => "bool const is_unsigned = false;",
	in_reqs  => [ "fpflags" ],
},

Ba => {
	# Note: has_delay_slot depends on whether it is a fallthrough or not, so we
	# have special code for this in sparc_emitter
	template  => $xop,
	irn_flags => [ "simple_jump", "fallthrough" ],
},

# This is a Jump instruction, but with the addition that you can add custom
# register constraints to model your calling conventions
Return => {
	template  => $xop,
	irn_flags => [ "has_delay_slot" ],
	in_reqs   => "...",
	ins       => [ "mem", "sp", "first_result" ],
	constructors => {
		imm => {
			attr => "ir_entity *entity, int32_t offset",
			init => "\tsparc_set_attr_imm(res, entity, offset);",
		},
		reg => {}
	},
},

# This is a JumpLink instruction, but with the addition that you can add custom
# register constraints to model your calling conventions
Call => {
	irn_flags => [ "has_delay_slot" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => "...",
	outs      => [ "M", "stack", "first_result" ],
	fixed     => "if (aggregate_return) arch_add_irn_flags(res, (arch_irn_flags_t)sparc_arch_irn_flag_aggregate_return);",
	constructors => {
		imm => {
			attr => "ir_entity *entity, int32_t offset, bool aggregate_return",
			init => "\tsparc_set_attr_imm(res, entity, offset);",
		},
		reg => {
			attr => "bool aggregate_return",
		}
	},
},

Cmp => { # aka SubccZero
	template => $binopcczero_operand,
	emit     => "cmp %S0, %SI1",
},

IJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	irn_flags => [ "has_delay_slot" ],
	state     => "pinned",
	constructors => {
		imm => {
			in_reqs => [ "gp" ],
			ins     => [ "ptr" ],
			attr    => "ir_entity *entity, int32_t offset, bool is_frame_entity",
			init    => "init_sparc_load_store_attributes(res, NULL, entity, offset, is_frame_entity, false);",
		},
		reg => {
			in_reqs => [ "gp", "gp" ],
			ins     => [ "ptr", "ptr2" ],
			init    => "init_sparc_load_store_attributes(res, NULL, NULL, 0, false, true);",
		},
	},
	out_reqs  => [ "exec" ],
	outs      => [ "jmp" ],
	attr_type => "sparc_load_store_attr_t",
},

SwitchJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	irn_flags => [ "has_delay_slot" ],
	state     => "pinned",
	in_reqs   => [ "gp" ],
	out_reqs  => "...",
	attr_type => "sparc_switch_jmp_attr_t",
	attr      => "const ir_switch_table *table, ir_entity *jump_table",
},

Sll => { template => $binop_operand },

Srl => { template => $binop_operand },

Sra => { template => $binop_operand },

And => { template => $binop_operand },

AndCCZero => {
	template => $binopcczero_operand,
	name     => "andcc",
},

AndN => { template => $binop_operand },

AndNCCZero => {
	template => $binopcczero_operand,
	name     => "andncc",
},

Or => { template => $binop_operand },

OrCCZero => {
	template => $binopcczero_operand,
	name     => "orcc",
},

OrN => { template => $binop_operand },

OrNCCZero => {
	template => $binopcczero_operand,
	name     => "orncc",
},

Xor => { template => $binop_operand },

XorCCZero => {
	template => $binopcczero_operand,
	name     => "xorcc",
},

XNor => { template => $binop_operand },

XNorCCZero => {
	template => $binopcczero_operand,
	name     => "xnorcc",
},

SMul => {
	template => $binop_operand,
},

SMulCCZero => {
	template => $binopcczero_operand,
	name     => "smulcc",
},

SMulh => {
	template => $binop_operand,
	emit     => "smul %S0, %SI1, %D0\n".
	            "mov %%y, %D0",
},

UMulh => {
	template => $binop_operand,
	emit     => "umul %S0, %SI1, %D0\n".
	            "mov %%y, %D0",
},

SDiv => { template => $div_operand },

UDiv => { template => $div_operand },

Stbar => {
	state    => "exc_pinned",
	ins      => [ "mem" ],
	outs     => [ "M" ],
	in_reqs  => [ "mem" ],
	out_reqs => [ "mem" ],
	emit     => "stbar",
},

Cas => {
	state    => "exc_pinned",
	ins      => [ "ptr", "old", "new", "mem" ],
	outs     => [ "res", "M" ],
	in_reqs  => [ "gp", "gp", "gp", "mem" ],
	out_reqs => [ "in_r2 !in_r0 !in_r1", "mem" ],
	# TODO: we need a must-be-same constraint for the CAS
},

fcmp => {
	irn_flags => [ "rematerializable" ],
	emit      => "fcmp%FM %S0, %S1",
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	out_reqs  => [ "fpflags" ],
	constructors => {
		s => { in_reqs => [ "cls-fp",   "cls-fp"   ] },
		d => { in_reqs => [ "cls-fp:2", "cls-fp:2" ] },
		q => { in_reqs => [ "cls-fp:4", "cls-fp:4" ] },
	},
},

fadd => { template => $float_binop },

fsub => { template => $float_binop },

fmul => { template => $float_binop },

fdiv => {
	irn_flags    => [ "rematerializable" ],
	emit         => "fdiv%FM %S0, %S1, %D0",
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	ins          => [ "left", "right" ],
	outs         => [ "res", "M" ],
	constructors => {
		s => { in_reqs => [ "cls-fp",   "cls-fp"   ], out_reqs => [ "cls-fp",   "mem" ] },
		d => { in_reqs => [ "cls-fp:2", "cls-fp:2" ], out_reqs => [ "cls-fp:2", "mem" ] },
		q => { in_reqs => [ "cls-fp:4", "cls-fp:4" ], out_reqs => [ "cls-fp:4", "mem" ] }
	},
},

fneg => { template => $float_unop },

fabs => { template => $float_unop },

fftof => {
	irn_flags => [ "rematerializable" ],
	emit      => "f%FSto%FD %S0, %D0",
	attr_type => "sparc_fp_conv_attr_t",
	attr      => "ir_mode *src_mode, ir_mode *dest_mode",
	constructors => {
		s_d => { in_reqs => [ "cls-fp"   ], out_reqs => [ "cls-fp:2" ], mode => $mode_fp2, },
		s_q => { in_reqs => [ "cls-fp"   ], out_reqs => [ "cls-fp:2" ], mode => $mode_fp4, },
		d_s => { in_reqs => [ "cls-fp:2" ], out_reqs => [ "cls-fp"   ] },
		d_q => { in_reqs => [ "cls-fp:2" ], out_reqs => [ "cls-fp:4" ], mode => $mode_fp4, },
		q_s => { in_reqs => [ "cls-fp:4" ], out_reqs => [ "cls-fp"   ] },
		q_d => { in_reqs => [ "cls-fp:4" ], out_reqs => [ "cls-fp:2" ], mode => $mode_fp2, },
	},
},

fitof => {
	irn_flags => [ "rematerializable" ],
	emit      => "fito%FM %S0, %D0",
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	in_reqs   => [ "cls-fp" ],
	constructors => {
		s => { out_reqs => [ "cls-fp"   ] },
		d => { out_reqs => [ "cls-fp:2" ], mode => $mode_fp2, },
		q => { out_reqs => [ "cls-fp:4" ], mode => $mode_fp4, },
	},
},

fftoi => {
	irn_flags => [ "rematerializable" ],
	emit      => "f%FMtoi %S0, %D0",
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	out_reqs  => [ "cls-fp" ],
	constructors => {
		s => { in_reqs => [ "cls-fp"   ] },
		d => { in_reqs => [ "cls-fp:2" ] },
		q => { in_reqs => [ "cls-fp:4" ] },
	},
},

Ldf => {
	state     => "exc_pinned",
	constructors => {
		s => { out_reqs => [ "cls-fp",   "mem" ] },
		d => { out_reqs => [ "cls-fp:2", "mem" ] },
		q => { out_reqs => [ "cls-fp:4", "mem" ] },
	},
	in_reqs   => [ "gp", "mem" ],
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
	init      => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
	emit      => "ld%ML %O0, %D0"
},

Stf => {
	state     => "exc_pinned",
	constructors => {
		s => { in_reqs => [ "cls-fp",   "gp", "mem" ] },
		d => { in_reqs => [ "cls-fp:2", "gp", "mem" ] },
		q => { in_reqs => [ "cls-fp:4", "gp", "mem" ] },
	},
	out_reqs  => [ "mem" ],
	ins       => [ "val", "ptr", "mem" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int32_t offset, bool is_frame_entity",
	init      => "init_sparc_load_store_attributes(res, ls_mode, entity, offset, is_frame_entity, false);",
	emit      => "st%MS %S0, %O1",
},

);
