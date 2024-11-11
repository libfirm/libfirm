# Arm Architecure Specification
# Author: Matthias Braun, Michael Beck, Oliver Richter, Tobias Gneist

$arch = "arm";

$mode_gp    = "arm_mode_gp";
$mode_flags = "arm_mode_flags";
$mode_fp    = "mode_F";

%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
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
		]
	},
	fpa => {
		mode => $mode_fp,
		registers => [
			{ name => "f0", dwarf => 96 },
			{ name => "f1", dwarf => 97 },
			{ name => "f2", dwarf => 98 },
			{ name => "f3", dwarf => 99 },
			{ name => "f4", dwarf => 100 },
			{ name => "f5", dwarf => 101 },
			{ name => "f6", dwarf => 102 },
			{ name => "f7", dwarf => 103 },
		]
	},
	flags => {
		flags => "manual_ra",
		mode => $mode_flags,
		registers => [ { name => "fl" }, ]
	},
);

%init_attr = (
	arm_attr_t =>
		"init_arm_attributes(res);",
	arm_Address_attr_t =>
		"init_arm_attributes(res);\n".
		"\tinit_arm_Address_attributes(res, entity, offset);",
	arm_CondJmp_attr_t =>
		"init_arm_attributes(res);",
	arm_SwitchJmp_attr_t =>
		"init_arm_attributes(res);",
	arm_fConst_attr_t =>
		"init_arm_attributes(res);",
	arm_load_store_attr_t =>
		"init_arm_attributes(res);\n".
		"\tinit_arm_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
	arm_shifter_operand_t =>
		"init_arm_attributes(res);",
	arm_cmp_attr_t =>
		"init_arm_attributes(res);\n".
		"\tinit_arm_cmp_attr(res, ins_permuted, is_unsigned);",
	arm_farith_attr_t =>
		"init_arm_attributes(res);\n".
		"\tinit_arm_farith_attributes(res, op_mode);",
);

my $unop_shifter_operand = {
	irn_flags    => [ "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp" ],
	emit         => "{name} %D0, %O",
	constructors => {
		imm => {
			attr    => "unsigned char immediate_value, unsigned char immediate_rot",
			init    => "init_arm_shifter_operand(res, 0, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs => [],
		},
		reg => {
			init    => "init_arm_shifter_operand(res, 0, 0, ARM_SHF_REG, 0);",
			in_reqs => [ "gp" ],
			ins     => [ "Rm" ],
		},
		reg_shift_reg => {
			attr    => "arm_shift_modifier_t shift_modifier",
			init    => "init_arm_shifter_operand(res, 0, 0, shift_modifier, 0);",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "Rm", "Rs" ],
		},
		reg_shift_imm => {
			attr    => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			init    => "init_arm_shifter_operand(res, 0, 0, shift_modifier, shift_immediate);",
			in_reqs => [ "gp" ],
			ins     => [ "Rm" ],
		},
	},
},

my $binop_shifter_operand = {
	irn_flags    => [ "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp" ],
	emit         => "{name} %D0, %S0, %O",
	constructors => {
		imm => {
			attr    => "unsigned char immediate_value, unsigned char immediate_rot",
			init    => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			init    => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr    => "arm_shift_modifier_t shift_modifier",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs => [ "gp", "gp", "gp" ],
			ins     => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr    => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
	},
};

my $binop_shifter_operand_setflags = {
	irn_flags    => [ "modify_flags", "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp", "flags" ],
	outs         => [ "res", "flags" ],
	emit         => "{name} %D0, %S0, %O",
	constructors => {
		imm => {
			attr    => "unsigned char immediate_value, unsigned char immediate_rot",
			init    => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			init    => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr    => "arm_shift_modifier_t shift_modifier",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs => [ "gp", "gp", "gp" ],
			ins     => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr    => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
	},
};

my $binop_shifter_operand_flags = {
	#irn_flags    => [ "rematerializable" ],
	attr_type    => "arm_shifter_operand_t",
	out_reqs     => [ "gp" ],
	emit         => "{name} %D0, %S0, %O",
	constructors => {
		imm => {
			attr    => "unsigned char immediate_value, unsigned char immediate_rot",
			init    => "init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);",
			in_reqs => [ "gp", "flags" ],
			ins     => [ "left", "flags" ],
		},
		reg => {
			init    => "init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);",
			in_reqs => [ "gp", "gp", "flags" ],
			ins     => [ "left", "right", "flags" ],
		},
		reg_shift_reg => {
			attr    => "arm_shift_modifier_t shift_modifier",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);",
			in_reqs => [ "gp", "gp", "gp", "flags" ],
			ins     => [ "left", "right", "shift", "flags" ],
		},
		reg_shift_imm => {
			attr    => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate",
			init    => "init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);",
			in_reqs => [ "gp", "gp", "flags" ],
			ins     => [ "left", "right", "flags" ],
		},
	},
};

my $cmp_shifter_operand = {
	irn_flags    => [ "rematerializable", "modify_flags" ],
	emit         => 'cmp %S0, %O',
	attr_type    => "arm_cmp_attr_t",
	out_reqs     => [ "flags" ],
	emit         => "{name} %S0, %O",
	constructors => {
		imm => {
			attr    => "unsigned char immediate_value, unsigned char immediate_rot, bool ins_permuted, bool is_unsigned",
			init    =>
				"init_arm_shifter_operand(res, 1, immediate_value, ARM_SHF_IMM, immediate_rot);\n",
			in_reqs => [ "gp" ],
			ins     => [ "left" ],
		},
		reg => {
			attr    => "bool ins_permuted, bool is_unsigned",
			init    =>
				"init_arm_shifter_operand(res, 1, 0, ARM_SHF_REG, 0);\n",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
		reg_shift_reg => {
			attr    => "arm_shift_modifier_t shift_modifier, bool ins_permuted, bool is_unsigned",
			init    =>
				"init_arm_shifter_operand(res, 1, 0, shift_modifier, 0);\n",
			in_reqs => [ "gp", "gp", "gp" ],
			ins     => [ "left", "right", "shift" ],
		},
		reg_shift_imm => {
			attr    => "arm_shift_modifier_t shift_modifier, unsigned shift_immediate, bool ins_permuted, bool is_unsigned",
			init    =>
				"init_arm_shifter_operand(res, 1, 0, shift_modifier, shift_immediate);\n",
			in_reqs => [ "gp", "gp" ],
			ins     => [ "left", "right" ],
		},
	},
};

my $mullop = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp" ],
	out_reqs  => [ "gp", "gp" ],
	outs      => [ "low", "high" ],
	emit      => "{name} %D0, %D1, %S0, %S1",
};

my $binopf = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fpa", "fpa" ],
	out_reqs  => [ "fpa" ],
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
	emit      => '{name}%MA %D0, %S0, %S1',
};


%nodes = (

Add => { template => $binop_shifter_operand },

AddS => { template => $binop_shifter_operand_setflags },

AdC => { template => $binop_shifter_operand_flags },

Mul => {
	irn_flags    => [ "rematerializable" ],
	in_reqs      => [ "gp", "gp" ],
	emit         => 'mul %D0, %S0, %S1',
	constructors => {
		"" => { out_reqs => [ "gp" ]     },
		# TODO: !in_r0 for out constraints the register allocator more than
		# necessary, as usually you can fix the problem by swapping the inputs. But
		# for this scheme we would need a special if both inputs are the same value.
		v5 => { out_reqs => [ "!in_r0" ] },
	},
},

SMulL => { template => $mullop },

UMulL => { template => $mullop },

Mla => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "gp" ],
	ins       => [ "left", "right", "add" ],
	emit      => 'mla %D0, %S0, %S1, %S2',
	constructors => {
		"" => { out_reqs => [ "gp" ]     },
		# See comments for Mul_v5 out register constraint
		v5 => { out_reqs => [ "!in_r0" ] },
	}
},

Mls => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp", "gp", "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "left", "right", "sub" ],
	emit      => 'mls %D0, %S0, %S1, %S2',
},

And => { template => $binop_shifter_operand },

Orr => { template => $binop_shifter_operand },

OrrPl => {
	#irn_flags => [ "rematerializable" ],
	emit      => 'orrpl %D0, %S2, %O',
	attr_type => "arm_shifter_operand_t",
	in_reqs   => [ "gp", "flags", "gp", "gp" ],
	out_reqs  => [ "in_r2 !in_r0 !in_r3" ],
	ins       => [ "falseval", "flags", "left", "right" ],
	init      => "init_arm_shifter_operand(res, 3, 0, ARM_SHF_REG, 0);",
},

Eor => { template => $binop_shifter_operand },

Bic => { template => $binop_shifter_operand },

Sub => { template => $binop_shifter_operand },

SubS => { template => $binop_shifter_operand_setflags },

SbC => { template => $binop_shifter_operand_flags },

RsC => { template => $binop_shifter_operand_flags },

Rsb => { template => $binop_shifter_operand },

RsbS => { template => $binop_shifter_operand_setflags },

Mov => {
	template => $unop_shifter_operand,
	ins      => [ "Rm", "Rs" ],
},

Mvn => { template => $unop_shifter_operand },

Pkhbt => { template => $binop_shifter_operand },

Pkhtb => { template => $binop_shifter_operand },

Clz => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	emit      => 'clz %D0, %S0',
},

# mov lr, pc\n mov pc, XXX -- This combination is used for calls to function
# pointers
LinkMovPC => {
	state     => "exc_pinned",
	irn_flags => [ "modify_flags" ],
	in_reqs   => "...",
	out_reqs  => "...",
	attr_type => "arm_shifter_operand_t",
	attr      => "unsigned shiftop_input, arm_shift_modifier_t shift_modifier, unsigned char immediate_value, unsigned char immediate_rot",
	init      => "init_arm_shifter_operand(res, shiftop_input, immediate_value, shift_modifier, immediate_rot);\n",
	emit      => "mov lr, pc\n".
	             "mov pc, %O",
},

# mov lr, pc\n ldr pc, XXX -- This combination is used for calls to function
# pointers
LinkLdrPC => {
	state     => "exc_pinned",
	irn_flags => [ "modify_flags" ],
	in_reqs   => "...",
	out_reqs  => "...",
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
	emit      => "mov lr, pc\n".
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

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int offset",
	in_reqs   => [ "gp" ],
	out_reqs  => [ "gp" ],
	ins       => [ "base" ],
	attr_type => "arm_Address_attr_t",
},

Address => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity, int offset",
	out_reqs  => [ "gp" ],
	attr_type => "arm_Address_attr_t",
},

Cmn => { template => $cmp_shifter_operand },

Cmp => { template => $cmp_shifter_operand },

Tst => { template => $cmp_shifter_operand },

Bcc => {
	irn_flags => [ "fallthrough" ],
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	in_reqs   => [ "flags" ],
	out_reqs  => [ "exec", "exec" ],
	ins       => [ "flags" ],
	attr      => "ir_relation relation",
	attr_type => "arm_CondJmp_attr_t",
	init      => "\tset_arm_CondJmp_relation(res, relation);",
},

B => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	irn_flags => [ "simple_jump", "fallthrough" ],
	out_reqs  => [ "exec" ],
},

IJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	in_reqs   => [ "gp" ],
	ins       => [ "target" ],
	out_reqs  => [ "exec" ],
	outs      => [ "jmp" ],
	emit      => "mov pc, %S0",
},

SwitchJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	attr      => "const ir_switch_table *table",
	init      => "be_switch_attr_init(res, &attr->swtch, table, NULL);",
	in_reqs   => [ "gp" ],
	out_reqs  => "...",
	attr_type => "arm_SwitchJmp_attr_t",
},

Ldr => {
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	in_reqs   => [ "gp", "mem" ],
	out_reqs  => [ "gp", "mem" ],
	emit      => 'ldr%ML %D0, %A',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Str => {
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	in_reqs   => [ "gp", "gp", "mem" ],
	out_reqs  => [ "mem" ],
	emit      => 'str%MS %S1, %A',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},


Adf => { template => $binopf },

Muf => { template => $binopf },

Suf => { template => $binopf },

Dvf => {
	template  => $binopf,
	irn_flags => [],
	out_reqs  => [ "fpa", "mem" ],
	outs      => [ "res", "M" ],
	mode      => "first",
},

Mvf => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "fpa" ],
	out_reqs  => [ "fpa" ],
	emit      => 'mvf%MA %S0, %D0',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
},

Flt => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "gp" ],
	out_reqs  => [ "fpa" ],
	emit      => 'flt%MA %D0, %S0',
	attr_type => "arm_farith_attr_t",
	attr      => "ir_mode *op_mode",
},

Cmfe => {
	irn_flags => [ "rematerializable", "modify_flags" ],
	attr_type => "arm_cmp_attr_t",
	attr      => "bool ins_permuted",
	fixed     => "bool const is_unsigned = false;",
	in_reqs   => [ "fpa", "fpa" ],
	out_reqs  => [ "flags" ],
	emit      => 'cmfe %S0, %S1',
},

Ldf => {
	state     => "exc_pinned",
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	in_reqs   => [ "gp", "mem" ],
	out_reqs  => [ "fpa", "mem" ],
	emit      => 'ldf%MF %D0, %A',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

Stf => {
	state     => "exc_pinned",
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	in_reqs   => [ "gp", "fpa", "mem" ],
	out_reqs  => [ "mem" ],
	emit      => 'stf%MF %S1, %A',
	attr_type => "arm_load_store_attr_t",
	attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
},

# floating point constants
fConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_tarval *tv",
	init      => "attr->tv = tv;",
	out_reqs  => [ "fpa" ],
	attr_type => "arm_fConst_attr_t",
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	ins      => [ "mem", "sp", "first_result" ],
	out_reqs => [ "exec" ],
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

);
