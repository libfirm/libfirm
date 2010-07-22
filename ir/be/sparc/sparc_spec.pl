# Creation: 2006/02/13
# $Id$

$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fp      = "mode_D";

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
		{ name => "g0", realname => "g0", type => $ignore }, # hardwired 0, behaves like /dev/null
		{ name => "g1", realname => "g1", type => $caller_save }, # temp. value
		{ name => "g2", realname => "g2", type => $caller_save },
		{ name => "g3", realname => "g3", type => $caller_save },
		{ name => "g4", realname => "g4", type => $caller_save },
		{ name => "g5", realname => "g5", type => $ignore }, # reserved by SPARC ABI
		{ name => "g6", realname => "g6", type => $ignore }, # reserved by SPARC ABI
		{ name => "g7", realname => "g7", type => $ignore }, # reserved by SPARC ABI

		# window's out registers
		{ name => "o0", realname => "o0", type => $caller_save }, # param 1 / return value from callee
		{ name => "o1", realname => "o1", type => $caller_save }, # param 2
		{ name => "o2", realname => "o2", type => $caller_save }, # param 3
		{ name => "o3", realname => "o3", type => $caller_save }, # param 4
		{ name => "o4", realname => "o4", type => $caller_save }, # param 5
		{ name => "o5", realname => "o5", type => $caller_save }, # param 6
		{ name => "sp", realname => "sp", type => $ignore }, # our stackpointer
		{ name => "o7", realname => "o6", type => $ignore }, # temp. value / address of CALL instr.

		# window's local registers
		{ name => "l0", realname => "l0", type => 0 },
		{ name => "l1", realname => "l1", type => 0 },
		{ name => "l2", realname => "l2", type => 0 },
		{ name => "l3", realname => "l3", type => 0 },
		{ name => "l4", realname => "l4", type => 0 },
		{ name => "l5", realname => "l5", type => 0 },
		{ name => "l6", realname => "l6", type => 0 },
		{ name => "l7", realname => "l7", type => 0 },

		# window's in registers
		{ name => "i0", realname => "i0", type => 0 }, # incoming param1 / return value to caller
		{ name => "i1", realname => "i1", type => 0 }, # param 2
		{ name => "i2", realname => "i2", type => 0 }, # param 3
		{ name => "i3", realname => "i3", type => 0 }, # param 4
		{ name => "i4", realname => "i4", type => 0 }, # param 5
		{ name => "i5", realname => "i5", type => 0 }, # param 6
		{ name => "fp", realname => "fp", type => $ignore }, # our framepointer
		{ name => "i7", realname => "i7", type => $ignore }, # return address - 8
		{ mode => $mode_gp }
	],
	flags => [
		{ name => "y", realname => "y", type => $ignore },  # the multiply/divide state register
		{ mode => $mode_flags, flags => "manual_ra" }
	],
	# fp registers can be accessed any time
	fp  => [
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
	RI => "${arch}_emit_reg_or_imm(node, -1);",
	R1I => "${arch}_emit_reg_or_imm(node, 0);",
	R2I => "${arch}_emit_reg_or_imm(node, 1);",
	R3I => "${arch}_emit_reg_or_imm(node, 2);",
# simple reg emitters
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
# more custom emitters
	C  => "${arch}_emit_immediate(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	EXTPREF  => "${arch}_emit_mode_sign_prefix(node);",
	FPM  => "${arch}_emit_fp_mode_suffix(node);",
	FPLM  => "${arch}_emit_fp_load_mode(node);",
	FPSM  => "${arch}_emit_fp_store_mode(node);",
	O  => "${arch}_emit_offset(node);",
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

);

%compare_attr = (
	sparc_attr_t            => "cmp_attr_sparc",
	sparc_load_store_attr_t => "cmp_attr_sparc_load_store",
	sparc_symconst_attr_t   => "cmp_attr_sparc_symconst",
	sparc_jmp_cond_attr_t   => "cmp_attr_sparc_jmp_cond",
	sparc_jmp_switch_attr_t	=> "cmp_attr_sparc_jmp_switch",
	sparc_save_attr_t       => "cmp_attr_sparc_save",
);

# addressing modes: imm, reg, reg +/- imm, reg + reg
# max. imm = 13 bits signed (-4096 ... 4096)

my %cmp_operand_constructors = (
	imm => {
		attr       => "int immediate_value",
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
		attr       => "int immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_value);",
		reg_req    => { in => [], out => [ "gp" ] },
	},
	reg => {
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
	},
);

my %binop_operand_constructors = (
	imm => {
		attr       => "int immediate_value",
		custominit => "sparc_set_attr_imm(res, immediate_value);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
		ins        => [ "left" ],
	},
	reg => {
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right" ],
	},
);

%nodes = (

Add => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	emit      => '. add %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Sub => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. sub %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},


# Load / Store
Ld => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. ld%LM [%S1%O], %D1'
},

HiImm => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	outs      => [ "res" ],
	mode      => $mode_gp,
	reg_req   => { in => [], out => [ "gp" ] },
	attr       => "int immediate_value",
	custominit => "sparc_set_attr_imm(res, immediate_value);",
},

LoImm => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	ins       => [ "hireg" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	attr       => "int immediate_value",
	custominit => "sparc_set_attr_imm(res, immediate_value);",
},

St => {
	op_flags  => [ "labeled", "fragile" ],
	mode 		=> "mode_M",
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "mem" ],
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. st%SM %S2, [%S1%O]'
},

Mov => {
	irn_flags => [ "rematerializable" ],
	arity     => "variable",
	emit      => '. mov %R1I, %D1',
	mode      => $mode_gp,
	constructors => \%unop_operand_constructors,
},

Save => {
	reg_req   => {
		in => [ "sp", "none"],
		out => [ "sp:I|S","none" ]
	},
	ins       => [ "stack", "mem" ],
	outs      => [ "stack", "mem" ],
	attr      => "int initial_stacksize",
	attr_type => "sparc_save_attr_t",
	init_attr => "\tinit_sparc_save_attr(res, initial_stacksize);",
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

BXX => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
	attr      => "int proj_num, bool is_unsigned",
	attr_type => "sparc_jmp_cond_attr_t",
	init_attr => "\tinit_sparc_jmp_cond_attr(res, proj_num, is_unsigned);",
},

Ba => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Cmp => {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => '. cmp %S1, %R2I',
	mode         => $mode_flags,
	constructors => \%cmp_operand_constructors,
},

Tst => {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => '. tst %S1',
	mode         => $mode_flags,
	reg_req      => { in => [ "gp" ], out => [ "flags" ] },
	ins          => [ "val" ],
},

SwitchJmp => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int n_projs, long def_proj_num",
	init_attr => "\tset_sparc_jmp_switch_n_projs(res, n_projs);\n".
					"\tset_sparc_jmp_switch_default_proj_num(res, def_proj_num);",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	attr_type => "sparc_jmp_switch_attr_t",
},

Sll => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. sll %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Slr => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. srl %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Sra => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. sra %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

And => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. and %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Or => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. or %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Xor => {
	irn_flags => [ "rematerializable" ],
	mode		=> $mode_gp,
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. xor %S1, %R2I, %D1',
	constructors => \%binop_operand_constructors,
},

Mul => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "flags" ] },
	outs      => [ "low", "high" ],
	constructors => \%binop_operand_constructors,
	#emit      =>'. mul %S1, %R2I, %D1'
},

Mulh => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "gp" ] },
	outs      => [ "low", "high" ],
	constructors => \%binop_operand_constructors,
},

Div => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	outs      => [ "res" ],
	constructors => \%binop_operand_constructors,
	#mode      => $mode_gp,
	#emit      =>'. div %S1, %R2I, %D1'
},

Minus => {
	irn_flags => [ "rematerializable" ],
	mode	    => $mode_gp,
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	emit      => ". sub %%g0, %S1, %D1"
},

Not => {
	irn_flags   => [ "rematerializable" ],
	mode	      => $mode_gp,
	reg_req     => { in => [ "gp" ], out => [ "gp" ] },
	emit        => '. xnor %S1, %%g0, %D1'
},

Nop => {
	op_flags => [ "keep" ],
	reg_req	 => { in => [], out => [ "none" ] },
	emit     => '. nop',
},

fAdd => {
	op_flags  => [ "commutative" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      => '. fadd%FPM %S1, %S2, %D1'
},

fMul => {
	op_flags  => [ "commutative" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      =>'. fmul%FPM %S1, %S2, %D1'
},

fsMuld => {
	op_flags  => [ "commutative" ],
	reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
	emit      =>'. fsmuld %S1, %S2, %D1'
},

FsTOd => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	emit      =>'. FsTOd %S1, %D1'
},

FdTOs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "fp" ] },
	emit      =>'. FdTOs %S1, %D1'
},

FiTOs => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "fp" ] },
	emit      =>'. FiTOs %S1, %D1'
},

FiTOd => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "fp" ] },
	emit      =>'. FiTOd %S1, %D1'
},

FsTOi => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "gp" ] },
	emit      =>'. FsTOi %S1, %D1'
},

FdTOi => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fp" ], out => [ "gp" ] },
	emit      =>'. FdTOi %S1, %D1'
},

Ldf => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	reg_req   => { in => [ "gp", "none" ], out => [ "fp", "none" ] },
	attr_type => "sparc_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => '. ld [%S1%O], %D1'
},

); # end of %nodes
