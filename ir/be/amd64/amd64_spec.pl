$arch = "amd64";

%reg_classes = (
	gp => [
		{ name => "rax", dwarf => 0 },
		{ name => "rcx", dwarf => 2 },
		{ name => "rdx", dwarf => 1 },
		{ name => "rsi", dwarf => 4 },
		{ name => "rdi", dwarf => 5 },
		{ name => "rbx", dwarf => 3 },
		{ name => "rbp", dwarf => 6 },
		{ name => "rsp", dwarf => 7 },
		{ name => "r8",  dwarf => 8 },
		{ name => "r9",  dwarf => 9 },
		{ name => "r10", dwarf => 10 },
		{ name => "r11", dwarf => 11 },
		{ name => "r12", dwarf => 12 },
		{ name => "r13", dwarf => 13 },
		{ name => "r14", dwarf => 14 },
		{ name => "r15", dwarf => 15 },
		{ mode => "mode_Lu" }
	],
	flags => [
		{ name => "eflags", dwarf => 49 },
		{ mode => "mode_Iu", flags => "manual_ra" }
	],
);

$mode_gp      = "mode_Lu";
$mode_flags   = "mode_Iu";
$status_flags = "all"; # TODO
$all_flags    = "all";

sub amd64_custom_init_attr {
	my $constr = shift;
	my $node   = shift;
	my $name   = shift;
	my $res    = "";

	if(defined($node->{modified_flags})) {
		$res .= "\tarch_add_irn_flags(res, arch_irn_flag_modify_flags);\n";
	}
	return $res;
}
$custom_init_attr_func = \&amd64_custom_init_attr;

$default_copy_attr = "amd64_copy_attr";

%init_attr = (
	amd64_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);",
	amd64_addr_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);\n"
		."\tattr->insn_mode = insn_mode;\n"
		."\tattr->addr = addr;",
	amd64_binop_addr_attr_t =>
		"init_be_info(res, irn_flags_, in_reqs, n_res);\n"
		."\t*attr = *attr_init;",
	amd64_switch_jmp_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);\n"
		."\tinit_amd64_switch_attributes(res, table, table_entity);",
	amd64_cc_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);\n"
		."\tinit_amd64_cc_attributes(res, cc);",
	amd64_movimm_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);\n"
		."\tinit_amd64_movimm_attributes(res, insn_mode, entity, offset);",
	amd64_shift_attr_t =>
		"init_be_info(res, irn_flags_, in_reqs, n_res);\n"
		."\t*attr = *attr_init;\n",
);

%compare_attr = (
	amd64_addr_attr_t       => "cmp_amd64_addr_attr",
	amd64_attr_t            => "cmp_amd64_attr",
	amd64_binop_addr_attr_t => "cmp_amd64_binop_addr_attr",
	amd64_cc_attr_t         => "cmp_amd64_cc_attr",
	amd64_movimm_attr_t     => "cmp_amd64_movimm_attr",
	amd64_shift_attr_t      => "cmp_amd64_shift_attr",
	amd64_switch_jmp_attr_t => "cmp_amd64_switch_jmp_attr",
);

%nodes = (
PushAM => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "rsp:I|S", "none" ] },
	arity     => "variable",
	outs      => [ "stack", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "push%M %A",
},

PopAM => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "rsp:I|S", "none" ] },
	arity     => "variable",
	outs      => [ "stack", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "pop%M %A",
},

Add => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "add%M %AM",
	modified_flags => $status_flags,
},

And => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "and%M %AM",
	modified_flags => $status_flags,

},

# Convert doubleword
CDQ => {
	reg_req   => { in => [ "rax" ], out => [ "rdx" ] },
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;",
	emit      => "cdq",
	mode      => $mode_gp,
},

# Convert quadword
CQO => {
	reg_req   => { in => [ "rax" ], out => [ "rdx" ] },
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;",
	emit      => "cqo",
	mode      => $mode_gp,
},

Div => {
	state     => "exc_pinned",
	reg_req   => { out => [ "rax", "flags", "none", "rdx" ] },
	arity     => "variable",
	outs      => [ "res_div", "flags", "M", "res_mod" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "div%M %AM",
	modified_flags => $status_flags,
},

IDiv => {
	state     => "exc_pinned",
	reg_req   => { out => [ "rax", "flags", "none", "rdx" ] },
	arity     => "variable",
	outs      => [ "res_div", "flags", "M", "res_mod" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "idiv%M %AM",
	modified_flags => $status_flags,
},

IMul => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	outs      => [ "res", "flags", "M" ],
	arity     => "variable",
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "imul%M %AM",
	modified_flags => $status_flags,
},

IMul1Op => {
	# Do not rematerialize this node
	# It produces 2 results and has strict constraints
	state     => "exc_pinned",
	reg_req   => { out => [ "rax", "flags", "none", "rdx" ] },
	outs      => [ "res_low", "flags", "M", "res_high" ],
	arity     => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "imul%M %AM",
	modified_flags => $status_flags,
},

Mul => {
	# Do not rematerialize this node
	# It produces 2 results and has strict constraints
	state     => "exc_pinned",
	reg_req   => { out => [ "rax", "flags", "none", "rdx" ] },
	outs      => [ "res_low", "flags", "M", "res_high" ],
	arity     => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "mul%M %AM",
	modified_flags => $status_flags,
},

Or => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	outs      => [ "res", "flags", "M" ],
	arity     => "variable",
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "or%M %AM",
	modified_flags => $status_flags,
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr_type => "amd64_shift_attr_t",
	attr      => "const amd64_shift_attr_t *attr_init",
	emit      => "shl%MS %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr_type => "amd64_shift_attr_t",
	attr      => "const amd64_shift_attr_t *attr_init",
	emit      => "shr%MS %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sar => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr_type => "amd64_shift_attr_t",
	attr      => "const amd64_shift_attr_t *attr_init",
	emit      => "sar%MS %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sub => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	outs      => [ "res", "flags", "M" ],
	arity     => "variable",
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "sub%M %S1, %D0",
	modified_flags => $status_flags,
},

Neg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_UNOP_REG;\n"
	            ."amd64_addr_t addr = { { NULL, 0 }, NO_INPUT, NO_INPUT, NO_INPUT, 0, AMD64_SEGMENT_DEFAULT };",
	emit      => "neg%M %AM",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Not => {
	irn_flags => [ "rematerializable" ],
	attr      => "amd64_insn_mode_t insn_mode",
	init_attr => "attr->insn_mode = insn_mode;",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	attr_type => "amd64_addr_attr_t",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_UNOP_REG;\n"
	            ."amd64_addr_t addr = { { NULL, 0 }, NO_INPUT, NO_INPUT, NO_INPUT, 0, AMD64_SEGMENT_DEFAULT };",
	emit      => "not%M %AM",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "xor%M %AM",
	modified_flags => $status_flags,
},

Xor0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	outs      => [ "res" ],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_REG_REG;",
	emit      => "xorl %3D0, %3D0",
	mode      => $mode_gp,
	modified_flags => $status_flags,
},

MovImm => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	attr_type => "amd64_movimm_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, int64_t offset, ir_entity *entity",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_IMM64;",
	emit      => 'mov%MM $%C, %D0',
	mode      => $mode_gp,
},

Movs => {
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	arity     => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "movs%Mq %AM, %^D0",
},

Mov => {
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	arity     => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
},

IJmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "unknown_jump" ],
	reg_req   => { in => [ "gp" ], out => [ "none", "none", "none" ] },
	outs      => [ "X", "unused", "M" ],
	arity     => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "jmp %AM",
},

Jmp => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	reg_req  => { out => [ "none" ] },
	fixed    => "amd64_op_mode_t op_mode = AMD64_OP_IMM32;",
	mode     => "mode_X",
},

Cmp => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "none", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "dummy", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "cmp%M %AM",
	modified_flags => $status_flags,
},

Lea => {
	irn_flags => [ "rematerializable" ],
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	reg_req   => { out => [ "gp", "flags", "none" ] },
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "lea%M %A, %D0",
	mode      => $mode_gp,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr_type => "amd64_cc_attr_t",
	attr      => "x86_condition_code_t cc",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	mode      => "mode_T",
},

Store => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "none" ] },
	arity     => "variable",
	outs      => [ "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	mode      => "mode_M",
	emit      => "mov%M %S0, %A",
	mode      => "mode_M",
},

SwitchJmp => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	mode      => "mode_T",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	out_arity => "variable",
	attr_type => "amd64_switch_jmp_attr_t",
	attr      => "const ir_switch_table *table, ir_entity *table_entity",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
},

Call => {
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_op_mode_t op_mode, amd64_addr_t addr",
	fixed     => "amd64_insn_mode_t insn_mode = INSN_MODE_64;\n",
	emit      => "call %AM",
	modified_flags => $all_flags,
},

Start => {
	state     => "pinned",
	out_arity => "variable",
	ins       => [],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
},

Return => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	arity    => "variable",
	reg_req  => { out => [ "none" ] },
	fixed    => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	mode     => "mode_X",
},

);
