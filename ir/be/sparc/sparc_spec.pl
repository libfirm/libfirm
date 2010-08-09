# Creation: 2006/02/13
# $Id$

$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fpflags = "mode_Bu";
$mode_fp      = "mode_F";
$mode_fp2     = "mode_D";
$mode_fp4     = "mode_E"; # not correct, we need to register a new mode

$normal      =  0; # no special type
$caller_save =  1; # caller save (register must be saved by the caller of a function)
$callee_save =  2; # callee save (register must be saved by the called function)
$ignore      =  4; # ignore (do not assign this register)
$arbitrary   =  8; # emitter can choose an arbitrary register of this class
$virtual     = 16; # the register is a virtual one
$state       = 32; # register represents a state

# available SPARC registers: 8 globals, 24 window regs (8 ins, 8 outs, 8 locals)
%reg_classes = (
	gp => [
		{ name => "g0", type => $ignore }, # hardwired 0, behaves like /dev/null
		{ name => "g1", type => $caller_save }, # temp. value
		{ name => "g2", type => $caller_save },
		{ name => "g3", type => $caller_save },
		{ name => "g4", type => $caller_save },
		{ name => "g5", type => $ignore }, # reserved by SPARC ABI
		{ name => "g6", type => $ignore }, # reserved by SPARC ABI
		{ name => "g7", type => $ignore }, # reserved by SPARC ABI

		# window's out registers
		{ name => "o0", type => $caller_save }, # param 1 / return value from callee
		{ name => "o1", type => $caller_save }, # param 2
		{ name => "o2", type => $caller_save }, # param 3
		{ name => "o3", type => $caller_save }, # param 4
		{ name => "o4", type => $caller_save }, # param 5
		{ name => "o5", type => $caller_save }, # param 6
		{ name => "sp", type => $ignore }, # our stackpointer
		{ name => "o7", type => $ignore }, # temp. value / address of CALL instr.

		# window's local registers
		{ name => "l0", type => 0 },
		{ name => "l1", type => 0 },
		{ name => "l2", type => 0 },
		{ name => "l3", type => 0 },
		{ name => "l4", type => 0 },
		{ name => "l5", type => 0 },
		{ name => "l6", type => 0 },
		{ name => "l7", type => 0 },

		# window's in registers
		{ name => "i0", type => 0 }, # incoming param1 / return value to caller
		{ name => "i1", type => 0 }, # param 2
		{ name => "i2", type => 0 }, # param 3
		{ name => "i3", type => 0 }, # param 4
		{ name => "i4", type => 0 }, # param 5
		{ name => "i5", type => 0 }, # param 6
		{ name => "frame_pointer", realname => "fp", type => $ignore }, # our framepointer
		{ name => "i7", type => $ignore }, # return address - 8
		{ mode => $mode_gp }
	],
	fpflags_class => [
		{ name => "fpflags", type => $ignore },
		{ mode => $mode_fpflags, flags => "manual_ra" }
	],
	flags_class => [
		{ name => "flags", type => $ignore },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
	mul_div_high_res => [
		{ name => "y", type => $ignore },
		{ mode => $mode_gp, flags => "manual_ra" }
	],
	# fp registers can be accessed any time
	fp => [
		{ name => "f0",  type => $caller_save },
		{ name => "f1",  type => $caller_save },
		{ name => "f2",  type => $caller_save },
		{ name => "f3",  type => $caller_save },
		{ name => "f4",  type => $caller_save },
		{ name => "f5",  type => $caller_save },
		{ name => "f6",  type => $caller_save },
		{ name => "f7",  type => $caller_save },
		{ name => "f8",  type => $caller_save },
		{ name => "f9",  type => $caller_save },
		{ name => "f10", type => $caller_save },
		{ name => "f11", type => $caller_save },
		{ name => "f12", type => $caller_save },
		{ name => "f13", type => $caller_save },
		{ name => "f14", type => $caller_save },
		{ name => "f15", type => $caller_save },
		{ name => "f16", type => $caller_save },
		{ name => "f17", type => $caller_save },
		{ name => "f18", type => $caller_save },
		{ name => "f19", type => $caller_save },
		{ name => "f20", type => $caller_save },
		{ name => "f21", type => $caller_save },
		{ name => "f22", type => $caller_save },
		{ name => "f23", type => $caller_save },
		{ name => "f24", type => $caller_save },
		{ name => "f25", type => $caller_save },
		{ name => "f26", type => $caller_save },
		{ name => "f27", type => $caller_save },
		{ name => "f28", type => $caller_save },
		{ name => "f29", type => $caller_save },
		{ name => "f30", type => $caller_save },
		{ name => "f31", type => $caller_save },
		{ mode => $mode_fp }
	]
); # %reg_classes

%emit_templates = (
# emit source reg or imm dep. on node's arity
	RI  => "${arch}_emit_reg_or_imm(node, -1);",
	R1I => "${arch}_emit_reg_or_imm(node, 0);",
	R2I => "${arch}_emit_reg_or_imm(node, 1);",
	R3I => "${arch}_emit_reg_or_imm(node, 2);",
	S1  => "${arch}_emit_source_register(node, 0);",
	S2  => "${arch}_emit_source_register(node, 1);",
	S3  => "${arch}_emit_source_register(node, 2);",
	S4  => "${arch}_emit_source_register(node, 3);",
	S5  => "${arch}_emit_source_register(node, 4);",
	S6  => "${arch}_emit_source_register(node, 5);",
	D1  => "${arch}_emit_dest_register(node, 0);",
	D2  => "${arch}_emit_dest_register(node, 1);",
	D3  => "${arch}_emit_dest_register(node, 2);",
	D4  => "${arch}_emit_dest_register(node, 3);",
	D5  => "${arch}_emit_dest_register(node, 4);",
	D6  => "${arch}_emit_dest_register(node, 5);",
	IM  => "${arch}_emit_immediate(node);",
	HIM => "${arch}_emit_high_immediate(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	FLSM => "${arch}_emit_float_load_store_mode(node);",
	FPM  => "${arch}_emit_fp_mode_suffix(node);",
	FCONVS => "${arch}_emit_fp_conv_source(node);",
	FCONVD => "${arch}_emit_fp_conv_destination(node);",
	O      => "${arch}_emit_offset(node);",
);

$default_attr_type = "sparc_attr_t";
$default_copy_attr = "sparc_copy_attr";


%init_attr = (
	sparc_attr_t             => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
	sparc_load_store_attr_t  => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n".
	                            "\tinit_sparc_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
	sparc_symconst_attr_t    => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n".
	                            "\tinit_sparc_symconst_attributes(res, entity);",
	sparc_jmp_cond_attr_t    => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
	sparc_jmp_switch_attr_t  => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
	sparc_save_attr_t        => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
	sparc_fp_attr_t          => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n".
	                            "\tinit_sparc_fp_attributes(res, fp_mode);\n",
	sparc_fp_conv_attr_t     => "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);".
	                            "\tinit_sparc_fp_conv_attributes(res, src_mode, dest_mode);\n",
);

%compare_attr = (
	sparc_attr_t            => "cmp_attr_sparc",
	sparc_load_store_attr_t => "cmp_attr_sparc_load_store",
	sparc_symconst_attr_t   => "cmp_attr_sparc_symconst",
	sparc_jmp_cond_attr_t   => "cmp_attr_sparc_jmp_cond",
	sparc_jmp_switch_attr_t	=> "cmp_attr_sparc_jmp_switch",
	sparc_save_attr_t       => "cmp_attr_sparc_save",
	sparc_fp_attr_t         => "cmp_attr_sparc_fp",
	sparc_fp_conv_attr_t    => "cmp_attr_sparc_fp_conv",
);

%custom_irn_flags = (
	modifies_flags    => "sparc_arch_irn_flag_modifies_flags",
	modifies_fp_flags => "sparc_arch_irn_flag_modifies_fp_flags",
);

# addressing modes: imm, reg, reg +/- imm, reg + reg
# max. imm = 13 bits signed (-4096 ... 4096)

my %cmp_operand_constructors = (
	imm => {
		attr       => "int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "flags" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
);

my %unop_operand_constructors = (
	imm => {
		attr       => "int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_value);",
		reg_req    => { in => [], out => [ "gp" ] },
	},
	reg => {
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
	},
);

my %binop_operand_constructors = (
	imm => {
		attr       => "int32_t immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right" ],
	},
);

my %float_binop_constructors = (
	s => {
		reg_req => { in => [ "fp", "fp" ], out => [ "fp" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp,
	},
	d => {
		reg_req => { in => [ "fp:a|2", "fp:a|2" ], out => [ "fp:a|2" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp2,
	},
	q => {
		reg_req => { in => [ "fp:a|4", "fp:a|4" ], out => [ "fp:a|4" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp4,
	}
);

my %float_unop_constructors = (
	s => {
		reg_req => { in => [ "fp" ], out => [ "fp" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp,
	},
	d => {
		reg_req => { in => [ "fp:a|2" ], out => [ "fp:a|2" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp2,
	},
	q => {
		reg_req => { in => [ "fp:a|4" ], out => [ "fp:a|4" ] },
		ins     => [ "left", "right" ],
		mode    => $mode_fp4,
	}
);

%nodes = (

Add => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. add %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Sub => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sub %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},


# Load / Store
Ld => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	constructors => {
		"" => {
			reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
		},
		d => {
			reg_req => { in => [ "gp", "none" ], out => [ "gp:a|2", "none" ] },
		},
	},
	outs      => [ "res", "M" ],
	ins       => [ "ptr", "mem" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. ld%LM [%S1%O], %D1'
},

SetHi => {
	irn_flags  => [ "rematerializable" ],
	outs       => [ "res" ],
	mode       => $mode_gp,
	reg_req    => { in => [], out => [ "gp" ] },
	attr       => "int32_t immediate_value",
	custominit => "sparc_set_attr_imm(res, immediate_value);",
	emit       => '. sethi %HIM, %D1'
},

St => {
	op_flags  => [ "labeled", "fragile" ],
	mode      => "mode_M",
	state     => "exc_pinned",
	constructors => {
		"" => {
			reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
		},
		d => {
			reg_req   => { in => [ "gp", "gp:a|2", "none" ], out => [ "none" ] },
		},
	},
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. st%SM %S2, [%S1%O]'
},

Save => {
	reg_req   => {
		in => [ "sp", "none"],
		out => [ "sp:I|S", "frame_pointer:I", "none" ]
	},
	ins       => [ "stack", "mem" ],
	outs      => [ "stack", "frame", "mem" ],
	attr      => "int initial_stacksize",
	attr_type => "sparc_save_attr_t",
	init_attr => "\tinit_sparc_save_attributes(res, initial_stacksize);",
},

SubSP => {
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "gp", "none" ] },
	ins       => [ "stack", "size", "mem" ],
	outs      => [ "stack", "addr", "M" ],
	emit      => ". sub %S1, %S2, %D1\n",
},

AddSP => {
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "none" ] },
	ins       => [ "stack", "size", "mem" ],
	outs      => [ "stack", "M" ],
	emit      => ". add %S1, %S2, %D1\n",
},

SymConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity",
	reg_req   => { out => [ "gp" ] },
	attr_type => "sparc_symconst_attr_t",
	mode      => $mode_gp,
},

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "base" ],
	attr_type => "sparc_symconst_attr_t",
	mode      => $mode_gp,
},

Bicc => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr_type => "sparc_jmp_cond_attr_t",
	attr      => "pn_Cmp pnc, bool is_unsigned",
	init_attr => "\tinit_sparc_jmp_cond_attr(res, pnc, is_unsigned);",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
},

fbfcc => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr_type => "sparc_jmp_cond_attr_t",
	attr      => "pn_Cmp pnc",
	init_attr => "\tinit_sparc_jmp_cond_attr(res, pnc, false);",
	reg_req   => { in => [ "fpflags" ], out => [ "none", "none" ] },
},

Ba => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Call => {
	irn_flags => [ "modifies_flags", "modifies_fp_flags" ],
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	constructors => {
		imm => {
			attr       => "ir_entity *entity, long offset",
			custominit => "get_sparc_attr(res)->immediate_value_entity = entity;",
			arity     => "variable",
			out_arity => "variable",
		},
		reg => {
			arity     => "variable",
			out_arity => "variable",
		}
	},
},

Cmp => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. cmp %S1, %R2I',
	ins          => [ "left", "right" ],
	mode         => $mode_flags,
	constructors => \%cmp_operand_constructors,
},

Tst => {
	irn_flags    => [ "rematerializable", "modifies_flags" ],
	emit         => '. tst %S1',
	mode         => $mode_flags,
	reg_req      => { in => [ "gp" ], out => [ "flags" ] },
	ins          => [ "val" ],
},

SwitchJmp => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	irn_flags => [ "modifies_flags" ],
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int n_projs, long def_proj_num",
	init_attr => "\tset_sparc_jmp_switch_n_projs(res, n_projs);\n".
					"\tset_sparc_jmp_switch_default_proj_num(res, def_proj_num);",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	attr_type => "sparc_jmp_switch_attr_t",
},

Sll => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sll %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Slr => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. srl %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Sra => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. sra %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

And => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. and %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

AndN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. andn %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Or => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. or %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

OrN => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. orn %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Xor => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. xor %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

XNor => {
	irn_flags => [ "rematerializable" ],
	mode      => $mode_gp,
	emit      => '. xnor %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Mul => {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	emit         => '. smul %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Mulh => {
	irn_flags    => [ "rematerializable" ],
	outs         => [ "low", "high" ],
	constructors => \%binop_operand_constructors,
},

# The div instructions are kinda hacky. Things to improve:
# * Make high-value input explicitely. Either as a gp at first or ideally
#   as an explicit y-register

SDiv => {
	irn_flags    => [ "rematerializable" ],
	state        => "exc_pinned",
	ins          => [ "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%binop_operand_constructors,
},

UDiv => {
	irn_flags    => [ "rematerializable" ],
	state        => "exc_pinned",
	ins          => [ "dividend_low", "divisor" ],
	outs         => [ "res", "M" ],
	constructors => \%binop_operand_constructors,
},

fcmp => {
	irn_flags => [ "rematerializable", "modifies_fp_flags" ],
	emit      => '. fcmp%FPM %S1, %S2',
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
	emit         => '. fadd%FPM %S1, %S2, %D1',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	constructors => \%float_binop_constructors,
},

fsub => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. fsub%FPM %S1, %S2, %D1',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	constructors => \%float_binop_constructors,
},

fmul => {
	irn_flags    => [ "rematerializable" ],
	op_flags     => [ "commutative" ],
	emit         =>'. fmul%FPM %S1, %S2, %D1',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	constructors => \%float_binop_constructors,
},

fdiv => {
	irn_flags    => [ "rematerializable" ],
	emit         => '. fdiv%FPM %S1, %S2, %D1',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	outs         => [ "res", "M" ],
	constructors => \%float_binop_constructors,
},

fneg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	# note that we only need the first register even for wide-values
	emit      => '. fneg %S1, %D1',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	constructors => \%float_unop_constructors,
},

"fabs" => {
	irn_flags    => [ "rematerializable" ],
	# note that we only need the first register even for wide-values
	emit         => '. fabs %S1, %D1',
	attr_type    => "sparc_fp_attr_t",
	attr         => "ir_mode *fp_mode",
	constructors => \%float_unop_constructors,
},

fftof => {
	irn_flags => [ "rematerializable" ],
	emit      => '. f%FCONVS%.to%FCONVD %S1, %D1',
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
	reg_req   => { in => [ "gp" ], out => [ "fp" ] },
	emit      => '. fito%FPM %S1, %D1',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	constructors => {
		s => {
			reg_req => { in => [ "gp" ], out => [ "fp" ] },
			mode    => $mode_fp,
		},
		d => {
			reg_req => { in => [ "gp" ], out => [ "fp:a|2" ] },
			mode    => $mode_fp2,
		},
		q => {
			reg_req => { in => [ "gp" ], out => [ "fp:a|4" ] },
			mode    => $mode_fp4,
		},
	},
},

fftoi => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "gp" ] },
	emit      => '. f%FPM.toi %S1, %D1',
	attr_type => "sparc_fp_attr_t",
	attr      => "ir_mode *fp_mode",
	mode      => $mode_gp,
	constructors => {
		s => {
			reg_req => { in => [ "gp" ], out => [ "gp" ] },
		},
		d => {
			reg_req => { in => [ "fp:a|2" ], out => [ "gp" ] },
		},
		q => {
			reg_req => { in => [ "fp:a|4" ], out => [ "gp" ] },
		},
	},
},

Ldf => {
	op_flags  => [ "labeled", "fragile" ],
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
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. ld%FLSM [%S1%O], %D1'
},

Stf => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	constructors => {
		s => {
			reg_req => { in => [ "gp", "fp", "none" ], out => [ "none" ] },
		},
		d => {
			reg_req => { in => [ "gp", "fp:a|2", "none" ], out => [ "none" ] },
		},
		q => {
			reg_req => { in => [ "gp", "fp:a|4", "none" ], out => [ "none" ] },
		},
	},
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. st%FLSM %S2, [%S1%O]',
	mode      => 'mode_M',
},

); # end of %nodes
