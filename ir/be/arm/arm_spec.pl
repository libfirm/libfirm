# Arm Architecure Specification
# Author: Matthias Braun, Michael Beck, Oliver Richter, Tobias Gneist

$arch = "arm";

#
# Modes
#
$mode_gp    = "arm_mode_gp";
$mode_flags = "arm_mode_flags";
$mode_fp    = "mode_F";

# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "r0",  dwarf => 0 },
		{ name => "r1",  dwarf => 1 },
		{ name => "r2",  dwarf => 2 },
		{ name => "r3",  dwarf => 3 },
		{ name => "r4",  dwarf => 4 },
		{ name => "r5",  dwarf => 5 },
		{ name => "r6",  dwarf => 6 },
		{ name => "r7",  dwarf => 7 },
		{ name => "r8",  dwarf => 8 },
		{ name => "r9",  dwarf => 9 },
		{ name => "r10", dwarf => 10 },
		{ name => "r11", dwarf => 11 },
		{ name => "r12", dwarf => 12 },
		{ name => "sp",  dwarf => 13 },
		{ name => "lr",  dwarf => 14 },
		{ name => "pc",  dwarf => 15 },
		{ mode => $mode_gp }
	],
	fpa => [
		{ name => "f0", dwarf => 96 },
		{ name => "f1", dwarf => 97 },
		{ name => "f2", dwarf => 98 },
		{ name => "f3", dwarf => 99 },
		{ name => "f4", dwarf => 100 },
		{ name => "f5", dwarf => 101 },
		{ name => "f6", dwarf => 102 },
		{ name => "f7", dwarf => 103 },
		{ mode => $mode_fp }
	],
	flags => [
		{ name => "fl" },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
);

$default_attr_type = "arm_attr_t";
$default_copy_attr = "arm_copy_attr";

%init_attr = (
	arm_attr_t           => "\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);",
	arm_Address_attr_t  =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_arm_Address_attributes(res, entity, offset);",
	arm_CondJmp_attr_t   => "\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);",
	arm_SwitchJmp_attr_t => "\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);",
	arm_fConst_attr_t    => "\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);",
	arm_load_store_attr_t =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_arm_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
	arm_shifter_operand_t =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n",
	arm_cmp_attr_t =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n",
	arm_farith_attr_t =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_arm_farith_attributes(res, op_mode);",
	arm_CopyB_attr_t =>
		"\tinit_arm_attributes(res, irn_flags_, in_reqs, n_res);\n".
		"\tinit_arm_CopyB_attributes(res, size);",
);

my $unop_shifter_operand = {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp" ],
	constructors => {
		imm => {
			attr       => "unsigned char immediate_value, unsigned char immediate_rot",
			custominit => "init_arm_shifter_operand(res, 0, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs    => [],
		},
		reg => {
			custominit => "init_arm_shifter_operand(res, 0, 0, ARM_SHF_REG, 0);",
			in_reqs    => [ "gp" ],
			ins        => [ "Rm" ],
		},
		reg_shift_reg => {
			attr       => "arm_shift_modifier_t shift_modifier",
			custominit => "init_arm_shifter_operand(res, 0, 0, shift_modifier, 0);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "Rm", "Rs" ],
		},
		reg_shift_imm => {
			attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			custominit => "init_arm_shifter_operand(res, 0, 0, shift_modifier, shift_immediate);",
			in_reqs    => [ "gp" ],
			ins        => [ "Rm" ],
		},
	},
},

my $binop_shifter_operand = {
	irn_flags    => [ "rematerializable" ],
	mode         => $mode_gp,
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp" ],
	constructors => {
		imm => {
			attr       => "unsigned char immediate_value, unsigned char immediate_rot",
			custominit => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs    => [ "gp" ],
			ins        => [ "left" ],
		},
		reg => {
			custominit => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr       => "arm_shift_modifier_t shift_modifier",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs    => [ "gp", "gp", "gp" ],
			ins        => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $binop_shifter_operand_setflags = {
	irn_flags    => [ "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp", "flags" ],
	outs         => [ "res", "flags" ],
	constructors => {
		imm => {
			attr       => "unsigned char immediate_value, unsigned char immediate_rot",
			custominit => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs    => [ "gp" ],
			ins        => [ "left" ],
		},
		reg => {
			custominit => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr       => "arm_shift_modifier_t shift_modifier",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs    => [ "gp", "gp", "gp" ],
			ins        => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $binop_shifter_operand_flags = {
	#irn_flags    => [ "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	mode         => $mode_gp,
	out_reqs     => [ "gp" ],
	constructors => {
		imm => {
			attr       => "unsigned char immediate_value, unsigned char immediate_rot",
			custominit => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs    => [ "gp", "flags" ],
			ins        => [ "left", "flags" ],
		},
		reg => {
			custominit => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs    => [ "gp", "gp", "flags" ],
			ins        => [ "left", "right", "flags" ],
		},
		reg_shift_reg => {
			attr       => "arm_shift_modifier_t shift_modifier",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs    => [ "gp", "gp", "gp", "flags" ],
			ins        => [ "left", "right", "shift", "flags" ],
		},
		reg_shift_imm => {
			attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			custominit => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs    => [ "gp", "gp", "flags" ],
			ins        => [ "left", "right", "flags" ],
		},
	},
};

my $cmp_shifter_operand = {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => 'cmp %S0, %O',
	mode         => $mode_flags,
	attr_type    => "arm_cmp_attr_t",
	out_reqs     => [ "flags" ],
	constructors => {
		imm => {
			attr       => "unsigned char immediate_value, unsigned char immediate_rot, bool ins_permuted, bool is_unsigned",
			custominit =>
				"init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);\n".
				"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
			in_reqs    => [ "gp" ],
			ins        => [ "left" ],
		},
		reg => {
			attr       => "bool ins_permuted, bool is_unsigned",
			custominit =>
				"init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);\n".
				"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr       => "arm_shift_modifier_t shift_modifier, bool ins_permuted, bool is_unsigned",
			custominit =>
				"init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);\n".
				"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
			in_reqs    => [ "gp", "gp", "gp" ],
			ins        => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr       => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate, bool ins_permuted, bool is_unsigned",
			custominit =>
				"init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);\n".
				"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
			in_reqs    => [ "gp", "gp" ],
			ins        => [ "left", "right" ],
		},
	},
};

my $mullop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp", "gp" ],
	outs      => [ "low", "high" ],
};

my $binopf = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fpa", "fpa" ],
	out_reqs  => [ "fpa" ],
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
};


%nodes = (

Add => {
	template => $binop_shifter_operand,
	emit     => 'add %D0, %S0, %O',
},

AddS => {
	template => $binop_shifter_operand_setflags,
	emit     => 'adds %D0, %S0, %O',
},

AdC => {
	template => $binop_shifter_operand_flags,
	emit     => 'adc %D0, %S0, %O',
},

Mul => {
	irn_flags    => [ "rematerializable" ],
	in_reqs      => [ "gp", "gp" ],
	emit         => 'mul %D0, %S0, %S1',
	mode         => $mode_gp,
	constructors => {
		""   => { out_reqs => [ "gp" ]     },
		# TODO: !in_r1 for out constrains the register allocator more than
		# necessary, as usually you can fix the problem by swapping the inputs. But
		# for this scheme we would need a special if both inputs are the same value.
		"v5" => { out_reqs => [ "!in_r1" ] },
	},
},

SMulL => {
	template => $mullop,
	emit     => 'smull %D0, %D1, %S0, %S1',
},

UMulL => {
	template => $mullop,
	emit     => 'umull %D0, %D1, %S0, %S1',
},

Mla => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "gp" ],
	ins       => [ "left", "right", "add" ],
	emit      => 'mla %D0, %S0, %S1, %S2',
	mode      => $mode_gp,
	constructors => {
		""   => { out_reqs => [ "gp" ]     },
		# See comments for Mul_v5 out register constraint
		"v5" => { out_reqs => [ "!in_r1" ] },
	}
},

Mls => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "left", "right", "sub" ],
	emit      => 'mls %D0, %S0, %S1, %S2',
	mode      => $mode_gp,
},

And => {
	template => $binop_shifter_operand,
	emit     => 'and %D0, %S0, %O',
},

Or => {
	template => $binop_shifter_operand,
	emit     => 'orr %D0, %S0, %O',
},

OrPl => {
	#irn_flags => [ "rematerializable" ],
	emit       => 'orrpl %D0, %S2, %O',
	mode       => $mode_gp,
	attr_type  => "arm_shifter_operand_t",
	in_reqs    => [ "gp", "flags", "gp", "gp" ],
	out_reqs   => [ "in_r3" ],
	ins        => [ "falseval", "flags", "left", "right" ],
	custominit => "init_arm_shifter_operand(res, 3, 0, ARM_SHF_REG, 0);",
},

Eor => {
	template => $binop_shifter_operand,
	emit     => 'eor %D0, %S0, %O',
},

Bic => {
	template => $binop_shifter_operand,
	emit     => 'bic %D0, %S0, %O',
},

Sub => {
	template => $binop_shifter_operand,
	emit     => 'sub %D0, %S0, %O',
},

SubS => {
	template => $binop_shifter_operand_setflags,
	emit     => 'subs %D0, %S0, %O',
},

SbC => {
	template => $binop_shifter_operand_flags,
	emit     => 'sbc %D0, %S0, %O',
},

RsC => {
	template => $binop_shifter_operand_flags,
	emit     => 'rsc %D0, %S0, %O',
},

Rsb => {
	template => $binop_shifter_operand,
	emit     => 'rsb %D0, %S0, %O',
},

RsbS => {
	template => $binop_shifter_operand_setflags,
	emit     => 'rsbs %D0, %S0, %O',
},

Mov => {
	template  => $unop_shifter_operand,
	emit      => 'mov %D0, %O',
	ins       => [ "Rm", "Rs" ],
},

Mvn => {
	template  => $unop_shifter_operand,
	emit      => 'mvn %D0, %O',
},

Pkhbt => {
	template => $binop_shifter_operand,
	emit     => 'pkhbt %D0, %S0, %O',
},

Pkhtb => {
	template => $binop_shifter_operand,
	emit     => 'pkhtb %D0, %S0, %O',
},

Clz => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	emit      => 'clz %D0, %S0',
	mode      => $mode_gp,
},

# mov lr, pc\n mov pc, XXX -- This combination is used for calls to function
# pointers
LinkMovPC => {
	state      => "exc_pinned",
	irn_flags  => [ "modify_flags" ],
	in_reqs    => "...",
	out_reqs   => "...",
	attr_type  => "arm_shifter_operand_t",
	attr       => "unsigned shiftop_input, arm_shift_modifier_t shift_modifier, unsigned char immediate_value, unsigned char immediate_rot",
	custominit => "init_arm_shifter_operand(res, shiftop_input, immediate_value, shift_modifier, immediate_rot);\n",
	emit       => "mov lr, pc\n".
	              "mov pc, %O",
},

# mov lr, pc\n ldr pc, XXX -- This combination is used for calls to function
# pointers
LinkLdrPC => {
	state      => "exc_pinned",
	irn_flags  => [ "modify_flags" ],
	in_reqs    => "...",
	out_reqs   => "...",
	attr_type  => "arm_load_store_attr_t",
	attr       => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit       => "mov lr, pc\n".
	              "ldr pc, %O",
},

Bl => {
	state     => "exc_pinned",
	irn_flags => [ "modify_flags" ],
	in_reqs   => "...",
	out_reqs  => "...",
	outs      => [ "M", "stack", "first_result" ],
	attr_type => "arm_Address_attr_t",
	attr      => "ir_entity *entity, int offset",
	emit      => 'bl %I',
},

CopyB => {
	state     => "pinned",
	attr      => "unsigned size",
	attr_type => "arm_CopyB_attr_t",
	in_reqs   => [ "!sp", "!sp", "gp", "gp", "gp", "none" ],
	out_reqs  => [ "none" ],
	outs      => [ "M" ],
	mode      => "mode_M",
},

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int offset",
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "base" ],
	attr_type => "arm_Address_attr_t",
	mode      => $mode_gp,
},

Address => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int offset",
	out_reqs  => [ "gp" ],
	attr_type => "arm_Address_attr_t",
	mode      => $mode_gp,
},

Cmn => {
	template => $cmp_shifter_operand,
	emit     => 'cmn %S0, %O',
},

Cmp => {
	template => $cmp_shifter_operand,
	emit     => 'cmp %S0, %O',
},

Tst => {
	template => $cmp_shifter_operand,
	emit     => 'tst %S0, %O',
},

B => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	in_reqs   => [ "flags" ],
	out_reqs  => [ "none", "none" ],
	ins       => [ "flags" ],
	attr      => "ir_relation relation",
	attr_type => "arm_CondJmp_attr_t",
	init_attr => "\tset_arm_CondJmp_relation(res, relation);",
},

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump" ],
	out_reqs  => [ "none" ],
	mode      => "mode_X",
},

SwitchJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	attr      => "const ir_switch_table *table",
	init_attr => "init_arm_SwitchJmp_attributes(res, table);",
	in_reqs   => [ "gp" ],
	out_reqs  => "...",
	attr_type => "arm_SwitchJmp_attr_t",
},

Ldr => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	in_reqs   => [ "gp", "none" ],
	out_reqs  => [ "gp", "none" ],
	emit      => 'ldr%ML %D0, [%S0, #%o]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Str => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	in_reqs   => [ "gp", "gp", "none" ],
	out_reqs  => [ "none" ],
	emit      => 'str%MS %S1, [%S0, #%o]',
	mode      => "mode_M",
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},


Adf => {
	template => $binopf,
	emit     => 'adf%MA %D0, %S0, %S1',
},

Muf => {
	template => $binopf,
	emit     => 'muf%MA %D0, %S0, %S1',
},

Suf => {
	template => $binopf,
	emit     => 'suf%MA %D0, %S0, %S1',
},

Dvf => {
	in_reqs   => [ "fpa", "fpa" ],
	out_reqs  => [ "fpa", "none" ],
	emit      => 'dvf%MA %D0, %S0, %S1',
	outs      => [ "res", "M" ],
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

Mvf => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fpa" ],
	out_reqs  => [ "fpa" ],
	emit      => 'mvf%MA %S0, %D0',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	mode      => $mode_fp,
},

FltX => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "fpa" ],
	emit      => 'flt%MA %D0, %S0',
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
	in_reqs   => [ "fpa", "fpa" ],
	out_reqs  => [ "flags" ],
	emit      => 'cmfe %S0, %S1',
},

Ldf => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	in_reqs   => [ "gp", "none" ],
	out_reqs  => [ "fpa", "none" ],
	emit      => 'ldf%MF %D0, [%S0, #%o]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Stf => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	mode      => "mode_M",
	in_reqs   => [ "gp", "fpa", "none" ],
	out_reqs  => [ "none" ],
	emit      => 'stf%MF %S1, [%S0, #%o]',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

#
# floating point constants
#
fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_tarval *tv",
	init_attr => "attr->tv = tv;",
	mode      => "get_tarval_mode(tv)",
	out_reqs  => [ "fpa" ],
	attr_type => "arm_fConst_attr_t",
},

Start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	out_reqs  => "...",
	ins       => [],
	emit      => "",
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	ins      => [ "mem", "sp", "first_result" ],
	mode     => "mode_X",
	out_reqs => [ "none" ],
	emit     => "bx lr",
},

AddS_t => {
	ins       => [ "left", "right" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

AdC_t => {
	ins       => [ "left", "right", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

SubS_t => {
	ins       => [ "left", "right" ],
	outs      => [ "res", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

SbC_t => {
	ins       => [ "left", "right", "flags" ],
	attr_type => "",
	dump_func => "NULL",
},

UMulL_t => {
	ins       => [ "left", "right" ],
	outs      => [ "low", "high" ],
	attr_type => "",
	dump_func => "NULL",
},

SMulL_t => {
	ins       => [ "left", "right" ],
	outs      => [ "low", "high" ],
	attr_type => "",
	dump_func => "NULL",
},

OrPl_t => {
	ins       => [ "falseval", "flags", "left", "right" ],
	attr_type => "",
	dump_func => "NULL",
},

); # end of %nodes
