# Creation: 2006/02/13
# Arm Architecure Specification
# Author: Matthias Braun, Michael Beck, Oliver Richter, Tobias Gneist
# $Id$

$arch = "arm";

#
# Modes
#
$mode_gp    = "mode_Iu";
$mode_flags = "mode_Bu";
$mode_fp    = "mode_E";

# register types:
$normal      =  0; # no special type
$caller_save =  1; # caller save (register must be saved by the caller of a function)
$callee_save =  2; # callee save (register must be saved by the called function)
$ignore      =  4; # ignore (do not assign this register)
$arbitrary   =  8; # emitter can choose an arbitrary register of this class
$virtual     = 16; # the register is a virtual one
$state       = 32; # register represents a state
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "r0",  type => $caller_save },
		{ name => "r1",  type => $caller_save },
		{ name => "r2",  type => $caller_save },
		{ name => "r3",  type => $caller_save },
		{ name => "r4",  type => $callee_save },
		{ name => "r5",  type => $callee_save },
		{ name => "r6",  type => $callee_save },
		{ name => "r7",  type => $callee_save },
		{ name => "r8",  type => $callee_save },
		{ name => "r9",  type => $callee_save },
		{ name => "r10", type => $callee_save },
		{ name => "r11", type => $callee_save },
		{ name => "r12", type => $ignore }, # reserved for linker/immediate fixups
		{ name => "sp",  type => $ignore }, # this is our stack pointer
		{ name => "lr",  type => $callee_save | $caller_save }, # this is our return address
		{ name => "pc",  type => $ignore }, # this is our program counter
		{ mode => $mode_gp }
	],
	fpa => [
		{ name => "f0", type => $caller_save },
		{ name => "f1", type => $caller_save },
		{ name => "f2", type => $caller_save },
		{ name => "f3", type => $caller_save },
		{ name => "f4", type => $caller_save },
		{ name => "f5", type => $caller_save },
		{ name => "f6", type => $caller_save },
		{ name => "f7", type => $caller_save },
		{ mode => $mode_fp }
	],
	flags => [
		{ name => "fl", type => 0 },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
);

%emit_templates = (
	FM  => "${arch}_emit_float_load_store_mode(node);",
	AM  => "${arch}_emit_float_arithmetic_mode(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	SO  => "${arch}_emit_shifter_operand(node);",
	S0  => "${arch}_emit_source_register(node, 0);",
	SC  => "${arch}_emit_symconst(node);",
	S1  => "${arch}_emit_source_register(node, 1);",
	S2  => "${arch}_emit_source_register(node, 2);",
	S3  => "${arch}_emit_source_register(node, 3);",
	S4  => "${arch}_emit_source_register(node, 4);",
	D0  => "${arch}_emit_dest_register(node, 0);",
	D1  => "${arch}_emit_dest_register(node, 1);",
	D2  => "${arch}_emit_dest_register(node, 2);",
	O   => "${arch}_emit_offset(node);",
);

$default_attr_type = "arm_attr_t";
$default_copy_attr = "arm_copy_attr";

%init_attr = (
	arm_attr_t           => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_SymConst_attr_t  =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_SymConst_attributes(res, entity, symconst_offset);",
	arm_CondJmp_attr_t   => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_SwitchJmp_attr_t => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_fConst_attr_t    => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_load_store_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
	arm_shifter_operand_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n",
	arm_cmp_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n",
	arm_farith_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_farith_attributes(res, op_mode);",
	arm_CopyB_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_CopyB_attributes(res, size);",
);

%compare_attr = (
	arm_attr_t            => "cmp_attr_arm",
	arm_SymConst_attr_t   => "cmp_attr_arm_SymConst",
	arm_CondJmp_attr_t    => "cmp_attr_arm_CondJmp",
	arm_SwitchJmp_attr_t  => "cmp_attr_arm_SwitchJmp",
	arm_fConst_attr_t     => "cmp_attr_arm_fConst",
	arm_load_store_attr_t => "cmp_attr_arm_load_store",
	arm_shifter_operand_t => "cmp_attr_arm_shifter_operand",
	arm_CopyB_attr_t      => "cmp_attr_arm_CopyB",
	arm_cmp_attr_t        => "cmp_attr_arm_cmp",
	arm_farith_attr_t     => "cmp_attr_arm_farith",
);

my %unop_shifter_operand_constructors = (
	imm => {
		attr       => "unsigned char immediate_value, unsigned char immediate_rot",
		custominit => "init_arm_shifter_operand(res, immediate_value, ARM_SHF_IMM, immediate_rot);",
		reg_req    => { in => [], out => [ "gp" ] },
	},
	reg => {
		custominit => "init_arm_shifter_operand(res, 0, ARM_SHF_REG, 0);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
	},
	reg_shift_reg => {
		attr       => "arm_shift_modifier_t shift_modifier",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, 0);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, shift_immediate);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
	},
);

my %binop_shifter_operand_constructors = (
	imm => {
		attr       => "unsigned char immediate_value, unsigned char immediate_rot",
		custominit => "init_arm_shifter_operand(res, immediate_value, ARM_SHF_IMM, immediate_rot);",
		reg_req    => { in => [ "gp" ], out => [ "gp" ] },
		ins        => [ "left" ],
	},
	reg => {
		custominit => "init_arm_shifter_operand(res, 0, ARM_SHF_REG, 0);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right" ],
	},
	reg_shift_reg => {
		attr       => "arm_shift_modifier_t shift_modifier",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, 0);",
		reg_req    => { in => [ "gp", "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right", "shift" ],
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, shift_immediate);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right" ],
	},
);

my %cmp_shifter_operand_constructors = (
	imm => {
		attr       => "unsigned char immediate_value, unsigned char immediate_rot, bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, immediate_value, ARM_SHF_IMM, immediate_rot);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp" ], out => [ "flags" ] },
		ins        => [ "left" ],
	},
	reg => {
		attr       => "bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, 0, ARM_SHF_REG, 0);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
	reg_shift_reg => {
		attr       => "arm_shift_modifier_t shift_modifier, bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, 0, shift_modifier, 0);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp", "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right", "shift" ],
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate, bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, 0, shift_modifier, shift_immediate);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
);


%nodes = (

Add => {
	irn_flags => [ "rematerializable" ],
	emit      => '. add %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Mul => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "!in_r1" ] },
	emit      =>'. mul %D0, %S0, %S1',
	mode      => $mode_gp,
},

Smull => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "gp" ] },
	emit      =>'. smull %D0, %D1, %S0, %S1',
	outs      => [ "low", "high" ],
},

Umull => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "gp" ] },
	emit      =>'. umull %D0, %D1, %S0, %S1',
	outs      => [ "low", "high" ],
	mode      => $mode_gp,
},

Mla => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "gp", "gp" ], out => [ "!in_r1" ] },
	emit      =>'. mla %D0, %S0, %S1, %S2',
	mode      => $mode_gp,
},

And => {
	irn_flags => [ "rematerializable" ],
	emit      => '. and %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Or => {
	irn_flags => [ "rematerializable" ],
	emit      => '. orr %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Eor => {
	irn_flags => [ "rematerializable" ],
	emit      => '. eor %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Bic => {
	irn_flags => [ "rematerializable" ],
	emit      => '. bic %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Sub => {
	irn_flags => [ "rematerializable" ],
	emit      => '. sub %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Rsb => {
	irn_flags => [ "rematerializable" ],
	emit      => '. rsb %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Mov => {
	irn_flags => [ "rematerializable" ],
	arity     => "variable",
	emit      => '. mov %D0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%unop_shifter_operand_constructors,
},

Mvn => {
	irn_flags => [ "rematerializable" ],
	attr_type => "arm_shifter_operand_t",
	arity     => "variable",
	emit      => '. mvn %D0, %SO',
	mode      => $mode_gp,
	constructors => \%unop_shifter_operand_constructors,
},

# mov lr, pc\n mov pc, XXX -- This combination is used for calls to function
# pointers
LinkMovPC => {
	state        => "exc_pinned",
	arity        => "variable",
	out_arity    => "variable",
	attr_type    => "arm_shifter_operand_t",
	attr         => "arm_shift_modifier_t shift_modifier, unsigned char immediate_value, unsigned char immediate_rot",
	custominit   => "init_arm_shifter_operand(res, immediate_value, shift_modifier, immediate_rot);\n".
	                "\tarch_irn_add_flags(res, arch_irn_flags_modify_flags);",
	emit         => ". mov lr, pc\n".
	                ". mov pc, %SO",
	mode         => "mode_T",
},

# mov lr, pc\n ldr pc, XXX -- This combination is used for calls to function
# pointers
LinkLdrPC => {
	state        => "exc_pinned",
	arity        => "variable",
	out_arity    => "variable",
	attr_type    => "arm_load_store_attr_t",
	attr         => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	custominit   => "arch_irn_add_flags(res, arch_irn_flags_modify_flags);",
	emit         => ". mov lr, pc\n".
	                ". ldr pc, %SO",
	mode         => "mode_T",
},

Bl => {
	state      => "exc_pinned",
	arity      => "variable",
	out_arity  => "variable",
	attr_type  => "arm_SymConst_attr_t",
	attr       => "ir_entity *entity, int symconst_offset",
	custominit => "arch_irn_add_flags(res, arch_irn_flags_modify_flags);",
	emit       => '. bl %SC',
	mode       => "mode_T",
},

# this node produces ALWAYS an empty (tempary) gp reg and cannot be CSE'd
EmptyReg => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	emit      => '. /* %D0 now available for calculations */',
	cmp_attr  => 'return 1;',
	mode      => $mode_gp,
},

CopyB => {
	op_flags  => [ "fragile" ],
	state     => "pinned",
	attr      => "unsigned size",
	attr_type => "arm_CopyB_attr_t",
	reg_req   => { in => [ "!sp", "!sp", "gp", "gp", "gp", "none" ], out => [ "none" ] },
	outs      => [ "M" ],
},

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int symconst_offset",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "base" ],
	attr_type => "arm_SymConst_attr_t",
	mode      => $mode_gp,
},

SymConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int symconst_offset",
	reg_req   => { out => [ "gp" ] },
	attr_type => "arm_SymConst_attr_t",
	mode      => $mode_gp,
},

Cmp => {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => '. cmp %S0, %SO',
	mode         => $mode_flags,
	attr_type    => "arm_cmp_attr_t",
	constructors => \%cmp_shifter_operand_constructors,
},

Tst => {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => '. tst %S0, %SO',
	mode         => $mode_flags,
	attr_type    => "arm_cmp_attr_t",
	constructors => \%cmp_shifter_operand_constructors,
},

B => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
	attr      => "pn_Cmp pnc",
	attr_type => "arm_CondJmp_attr_t",
	init_attr => "\tset_arm_CondJmp_pnc(res, pnc);",
},

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

SwitchJmp => {
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int n_projs, long def_proj_num",
	init_attr => "\tset_arm_SwitchJmp_n_projs(res, n_projs);\n".
	             "\tset_arm_SwitchJmp_default_proj_num(res, def_proj_num);",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	attr_type => "arm_SwitchJmp_attr_t",
},

Ldr => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	emit      => '. ldr%LM %D0, [%S0, #%O]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Str => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "mem" ],
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	emit      => '. str%SM %S1, [%S0, #%O]',
	mode      => "mode_M",
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

StoreStackM4Inc => {
	op_flags  => [ "labeled", "fragile" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "sp", "gp", "gp", "gp", "gp", "none" ], out => [ "sp:I|S", "none" ] },
	emit      => '. stmfd %S0!, {%S1, %S2, %S3, %S4}',
	outs      => [ "ptr", "M" ],
},

LoadStackM3Epilogue => {
	op_flags  => [ "labeled", "fragile" ],
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "sp", "none" ], out => [ "r11:I", "sp:I|S", "pc:I", "none" ] },
	emit      => '. ldmfd %S0, {%D0, %D1, %D2}',
	outs      => [ "res0", "res1", "res2", "M" ],
},



Adf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      => '. adf%AM %D0, %S0, %S1',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Muf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      =>'. muf%AM %D0, %S0, %S1',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Suf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      => '. suf%AM %D0, %S0, %S1',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Dvf => {
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa", "none" ] },
	emit      =>'. dvf%AM %D0, %S0, %S1',
	outs      => [ "res", "M" ],
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Mvf => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "fpa" ], out => [ "fpa" ] },
	emit      => '. mvf%AM %S0, %D0',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

FltX => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "fpa" ] },
	emit      => '. flt%AM %D0, %S0',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Cmfe => {
	irn_flags => [ "rematerializable", "modify_flags" ],
	mode      => $mode_flags,
	attr_type => "arm_cmp_attr_t",
	attr      => "bool ins_permuted",
	init_attr => "init_arm_cmp_attr(res, ins_permuted, false);",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "flags" ] },
	emit      => '. cmfe %S0, %S1',
},

Ldf => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	reg_req   => { in => [ "gp", "none" ], out => [ "fpa", "none" ] },
	emit      => '. ldf%FM %D0, [%S0, #%O]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Stf => {
	op_flags  => [ "labeled", "fragile" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	mode      => "mode_M",
	reg_req   => { in => [ "gp", "fpa", "none" ], out => [ "none" ] },
	emit      => '. stf%FM %S1, [%S0, #%O]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

#
# floating point constants
#
fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "tarval *tv",
	init_attr => "attr->tv = tv;",
	mode      => "get_tarval_mode(tv)",
	reg_req   => { out => [ "fpa" ] },
	attr_type => "arm_fConst_attr_t",
}

); # end of %nodes
