# Creation: 2006/02/13
# $Id$

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "arm";

#
# Modes
#
$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fpa     = "mode_E";

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
	fpa  => [
		{ name => "f0", type => $caller_save },
		{ name => "f1", type => $caller_save },
		{ name => "f2", type => $caller_save },
		{ name => "f3", type => $caller_save },
		{ name => "f4", type => $caller_save },
		{ name => "f5", type => $caller_save },
		{ name => "f6", type => $caller_save },
		{ name => "f7", type => $caller_save },
		{ mode => $mode_fpa }
	],
	flags => [
		{ name => "fl", type => 0 },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
);

%emit_templates = (
	M   => "${arch}_emit_mode(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	SO  => "${arch}_emit_shifter_operand(node);",
	S0  => "${arch}_emit_source_register(node, 0);",
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
		"\tinit_arm_SymConst_attributes(res, entity);",
	arm_CondJmp_attr_t   => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_SwitchJmp_attr_t => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_fpaConst_attr_t  => "\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);",
	arm_load_store_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
	arm_shifter_operand_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n",
	arm_cmp_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n",
	arm_CopyB_attr_t =>
		"\tinit_arm_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		"\tinit_arm_CopyB_attributes(res, size);",
);

%compare_attr = (
	arm_attr_t            => "cmp_attr_arm",
	arm_SymConst_attr_t   => "cmp_attr_arm_SymConst",
	arm_CondJmp_attr_t    => "cmp_attr_arm_CondJmp",
	arm_SwitchJmp_attr_t  => "cmp_attr_arm_SwitchJmp",
	arm_fpaConst_attr_t   => "cmp_attr_arm_fpaConst",
	arm_load_store_attr_t => "cmp_attr_arm_load_store",
	arm_shifter_operand_t => "cmp_attr_arm_shifter_operand",
	arm_CopyB_attr_t      => "cmp_attr_arm_CopyB",
	arm_cmp_attr_t        => "cmp_attr_arm_cmp",
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
		attr       => "arm_shift_modifier shift_modifier",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, 0);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier shift_modifier, unsigned shift_immediate",
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
		attr       => "arm_shift_modifier shift_modifier",
		custominit => "init_arm_shifter_operand(res, 0, shift_modifier, 0);",
		reg_req    => { in => [ "gp", "gp", "gp" ], out => [ "gp" ] },
		ins        => [ "left", "right", "shift" ],
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier shift_modifier, unsigned shift_immediate",
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
		attr       => "arm_shift_modifier shift_modifier, bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, 0, shift_modifier, 0);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp", "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right", "shift" ],
	},
	reg_shift_imm => {
		attr       => "arm_shift_modifier shift_modifier, unsigned shift_immediate, bool ins_permuted, bool is_unsigned",
		custominit =>
			"init_arm_shifter_operand(res, 0, shift_modifier, shift_immediate);\n".
			"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
		reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
		ins        => [ "left", "right" ],
	},
);


%nodes = (

Add => {
	irn_flags => "R",
	emit      => '. add %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Mul => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "!in_r1" ] },
	emit      =>'. mul %D0, %S0, %S1',
	mode      => $mode_gp,
},

Smull => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "gp" ] },
	emit      =>'. smull %D0, %D1, %S0, %S1',
	outs      => [ "low", "high" ],
},

Umull => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "gp" ] },
	emit      =>'. umull %D0, %D1, %S0, %S1',
	outs      => [ "low", "high" ],
	mode      => $mode_gp,
},

Mla => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp" ], out => [ "!in_r1" ] },
	emit      =>'. mla %D0, %S0, %S1, %S2',
	mode      => $mode_gp,
},

And => {
	irn_flags => "R",
	emit      => '. and %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Or => {
	irn_flags => "R",
	emit      => '. orr %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Eor => {
	irn_flags => "R",
	emit      => '. eor %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Bic => {
	irn_flags => "R",
	emit      => '. bic %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Sub => {
	irn_flags => "R",
	emit      => '. sub %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Rsb => {
	irn_flags => "R",
	emit      => '. rsb %D0, %S0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%binop_shifter_operand_constructors,
},

Mov => {
	irn_flags => "R",
	arity     => "variable",
	emit      => '. mov %D0, %SO',
	mode      => $mode_gp,
	attr_type => "arm_shifter_operand_t",
	constructors => \%unop_shifter_operand_constructors,
},

Mvn => {
	irn_flags => "R",
	attr_type => "arm_shifter_operand_t",
	arity     => "variable",
	emit      => '. mvn %D0, %SO',
	mode      => $mode_gp,
	constructors => \%unop_shifter_operand_constructors,
},

# Deprecated - we should construct the movs and rsbmi directly...
Abs => {
	irn_flags => "R",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	emit      =>
'. movs %S0, %S0, #0
. rsbmi %D0, %S0, #0',
	mode      => $mode_gp,
},

# this node produces ALWAYS an empty (tempary) gp reg and cannot be CSE'd
EmptyReg => {
	op_flags  => "c",
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
	emit      => '. /* %D0 now available for calculations */',
	cmp_attr  => 'return 1;',
	mode      => $mode_gp,
},

CopyB => {
	op_flags  => "F|H",
	state     => "pinned",
	attr      => "unsigned size",
	attr_type => "arm_CopyB_attr_t",
	reg_req   => { in => [ "!sp", "!sp", "gp", "gp", "gp", "none" ], out => [ "none" ] },
	outs      => [ "M" ],
},

FrameAddr => {
	op_flags  => "c",
	irn_flags => "R",
	attr      => "ir_entity *entity",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "base" ],
	attr_type => "arm_SymConst_attr_t",
	mode      => $mode_gp,
},

SymConst => {
	op_flags  => "c",
	irn_flags => "R",
	attr      => "ir_entity *entity",
	reg_req   => { out => [ "gp" ] },
	attr_type => "arm_SymConst_attr_t",
	mode      => $mode_gp,
},

Cmp => {
	irn_flags    => "R|F",
	emit         => '. cmp %S0, %SO',
	mode         => $mode_flags,
	attr_type    => "arm_cmp_attr_t",
	constructors => \%cmp_shifter_operand_constructors,
},

Tst => {
	irn_flags    => "R|F",
	emit         => '. tst %S0, %SO',
	mode         => $mode_flags,
	attr_type    => "arm_cmp_attr_t",
	constructors => \%cmp_shifter_operand_constructors,
},

B => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
	attr      => "int proj_num",
	attr_type => "arm_CondJmp_attr_t",
	init_attr => "\tset_arm_CondJmp_proj_num(res, proj_num);",
},

Jmp => {
	state     => "pinned",
	op_flags  => "X",
	irn_flags => "J",
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

SwitchJmp => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int n_projs, long def_proj_num",
	init_attr => "\tset_arm_SwitchJmp_n_projs(res, n_projs);\n".
	             "\tset_arm_SwitchJmp_default_proj_num(res, def_proj_num);",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	attr_type => "arm_SwitchJmp_attr_t",
},

Ldr => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	emit      => '. ldr%LM %D0, [%S0, #%O]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Str => {
	op_flags  => "L|F",
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
	op_flags  => "L|F",
	irn_flags => "R",
	state     => "exc_pinned",
	reg_req   => { in => [ "sp", "gp", "gp", "gp", "gp", "none" ], out => [ "sp:I|S", "none" ] },
	emit      => '. stmfd %S0!, {%S1, %S2, %S3, %S4}',
	outs      => [ "ptr", "M" ],
},

LoadStackM3Epilogue => {
	op_flags  => "L|F",
	irn_flags => "R",
	state     => "exc_pinned",
	reg_req   => { in => [ "sp", "none" ], out => [ "r11:I", "sp:I|S", "pc:I", "none" ] },
	emit      => '. ldmfd %S0, {%D0, %D1, %D2}',
	outs      => [ "res0", "res1", "res2", "M" ],
},



fpaAdf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      => '. adf%M %D0, %S0, %S1',
},

fpaMuf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      =>'. muf%M %D0, %S0, %S1',
},

fpaFml => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      =>'. fml%M %D0, %S0, %S1',
},

fpaMax => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      =>'. fmax %S0, %S1, %D0',
},

fpaMin => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      =>'. fmin %S0, %S1, %D0',
},

fpaSuf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      => '. suf%M %D0, %S0, %S1'
},

fpaRsf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa" ] },
	emit      => '. rsf%M %D0, %S0, %S1'
},

fpaDvf => {
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa", "none" ] },
	emit      =>'. dvf%M %D0, %S0, %S1',
	outs      => [ "res", "M" ],
},

fpaRdf => {
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa", "none" ] },
	emit      =>'. rdf%M %D0, %S0, %S1',
	outs      => [ "res", "M" ],
},

fpaFdv => {
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa", "none" ] },
	emit      =>'. fdv%M %D0, %S0, %S1',
	outs      => [ "res", "M" ],
},

fpaFrd => {
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "fpa", "none" ] },
	emit      =>'. frd%M %D0, %S0, %S1',
	outs      => [ "res", "M" ],
},

fpaMvf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa" ], out => [ "fpa" ] },
	emit      => '. mvf%M %S0, %D0',
},

fpaMnf => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa" ], out => [ "fpa" ] },
	emit      => '. mnf%M %S0, %D0',
},

fpaAbs => {
	irn_flags => "R",
	reg_req   => { in => [ "fpa" ], out => [ "fpa" ] },
	emit      => '. abs%M %D0, %S0',
},

fpaFlt => {
	irn_flags => "R",
	reg_req   => { in => ["gp"], out => [ "fpa" ] },
	emit      => '. flt%M %D0, %S0',
},

fpaFix => {
	irn_flags => "R",
	reg_req   => { in => ["fpa"], out => [ "gp" ] },
	emit      => '. fix %D0, %S0',
},

fpaCmfBra => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int proj_num",
	init_attr => "\tset_arm_CondJmp_proj_num(res, proj_num);",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "none", "none"] },
	attr_type => "arm_CondJmp_attr_t",
},

fpaCnfBra => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int proj_num",
	init_attr => "\tset_arm_CondJmp_proj_num(res, proj_num);",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "none", "none"] },
	attr_type => "arm_CondJmp_attr_t",
},

fpaCmfeBra => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int proj_num",
	init_attr => "\tset_arm_CondJmp_proj_num(res, proj_num);",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "none", "none"] },
	attr_type => "arm_CondJmp_attr_t",
},

fpaCnfeBra => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int proj_num",
	init_attr => "\tset_arm_CondJmp_proj_num(res, proj_num);",
	reg_req   => { in => [ "fpa", "fpa" ], out => [ "none", "none"] },
	attr_type => "arm_CondJmp_attr_t",
},

fpaLdf => {
	op_flags  => "L|F",
	irn_flags => "R",
	state     => "exc_pinned",
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "gp", "none" ], out => [ "fpa", "none" ] },
	emit      => '. ldf%M %D0, [%S0]',
	outs      => [ "res", "M" ],
},

fpaStf => {
	op_flags  => "L|F",
	irn_flags => "R",
	state     => "exc_pinned",
	attr      => "ir_mode *op_mode",
	init_attr => "attr->op_mode = op_mode;",
	reg_req   => { in => [ "gp", "fpa", "none" ], out => [ "none" ] },
	emit      => '. stf%M %S1, [%S0]',
	mode      => "mode_M",
},

fpaDbl2GP => {
	op_flags  => "L|F",
	irn_flags => "R",
	reg_req   => { in => [ "fpa", "none" ], out => [ "gp", "gp", "none" ] },
	outs      => [ "low", "high", "M" ],
},

AddSP => {
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "none" ] },
	emit      => '. add %D0, %S0, %S1',
	outs      => [ "stack", "M" ],
},

SubSPandCopy => {
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "gp", "none" ] },
	ins       => [ "stack", "size", "mem" ],
	emit      => ". sub %D0, %S0, %S1\n".
	             ". mov sp, %D1",
	outs      => [ "stack", "addr", "M" ],
},

LdTls => {
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
},


#
# floating point constants
#
fpaConst => {
	op_flags  => "c",
	irn_flags => "R",
	attr      => "tarval *tv",
	init_attr => "attr->tv = tv;",
	mode      => "get_tarval_mode(tv)",
	reg_req   => { out => [ "fpa" ] },
	attr_type => "arm_fpaConst_attr_t",
}

); # end of %nodes
