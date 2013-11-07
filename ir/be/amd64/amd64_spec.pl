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

$mode_gp        = "mode_Lu";
$mode_flags     = "mode_Iu";

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
	amd64_attr_t           =>
		 "\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);",
	amd64_switch_jmp_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);"
		. "\tinit_amd64_switch_attributes(res, table, table_entity);",
	amd64_cc_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);"
		. "\tinit_amd64_cc_attributes(res, cc);",
	amd64_movimm_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);"
		. "\tinit_amd64_movimm_attributes(res, entity, offset);",
);

%compare_attr = (
	amd64_attr_t            => "cmp_amd64_attr",
	amd64_switch_jmp_attr_t => "cmp_amd64_switch_jmp_attr",
	amd64_movimm_attr_t     => "cmp_amd64_movimm_attr",
	amd64_cc_attr_t         => "cmp_amd64_cc_attr",
);

%nodes = (
PushAM => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "rsp:I|S", "none" ] },
	arity     => "variable",
	outs      => [ "stack", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "push%M %A",
},

PopAM => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "rsp:I|S", "none" ] },
	arity     => "variable",
	outs      => [ "stack", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "pop%M %A",
},

Add => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "add%M %AM",
	modified_flags => $status_flags,
},

And => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "and%M %AM",
	modified_flags => $status_flags,

},

Div => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	attr       => "amd64_insn_mode_t insn_mode",
	init_attr  => "attr->data.insn_mode = insn_mode;",
	reg_req    => { in => [ "rdx", "rax", "gp", "none" ], out => [ "rax", "rdx", "none" ] },
	ins        => [ "left_high", "left_low", "right", "mem" ],
	outs       => [ "res_div", "res_mod", "M" ],
	emit       => "div%M %S2",
	modified_flags => 1,
},

IDiv => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	attr       => "amd64_insn_mode_t insn_mode",
	init_attr  => "attr->data.insn_mode = insn_mode;",
	reg_req    => { in => [ "rdx", "rax", "gp", "none" ], out => [ "rax", "rdx", "none" ] },
	ins        => [ "left_high", "left_low", "right", "mem" ],
	outs       => [ "res_div", "res_mod", "M" ],
	emit       => "idiv%M %S2",
	modified_flags => 1,
},

IMul => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	outs      => [ "res", "flags", "M" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "imul%M %AM",
	modified_flags => $status_flags,
},

Or => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	outs      => [ "res", "flags", "M" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "or%M %AM",
	modified_flags => $status_flags,
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, uint8_t immediate",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am.offset      = immediate;\n",
	emit      => "shl%M %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, uint8_t immediate",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am.offset      = immediate;\n",
	emit      => "shr%M %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sar => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { out => [ "gp" ] },
	out       => [ "res" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, uint8_t immediate",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am.offset      = immediate;\n",
	emit      => "sar%M %SO",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sub => {
	irn_flags  => [ "rematerializable" ],
	attr       => "amd64_insn_mode_t insn_mode",
	init_attr  => "attr->data.insn_mode = insn_mode;",
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => "sub%M %S1, %D0",
	mode       => $mode_gp,
	modified_flags => 1,
},

Neg => {
	irn_flags => [ "rematerializable" ],
	attr      => "amd64_insn_mode_t insn_mode",
	init_attr => "attr->data.insn_mode = insn_mode;",
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	emit      => "neg%M %S0",
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Not => {
	irn_flags => [ "rematerializable" ],
	attr      => "amd64_insn_mode_t insn_mode",
	init_attr => "attr->data.insn_mode = insn_mode;",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	emit      => "not%M %S0",
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "xor%M %AM",
	modified_flags => $status_flags,
},

Xor0 => {
	op_flags   => [ "constlike" ],
	irn_flags  => [ "rematerializable" ],
	init_attr  => "attr->data.insn_mode = INSN_MODE_32;",
	reg_req    => { out => [ "gp" ] },
	outs       => [ "res" ],
	emit       => "xorl %D0, %D0",
	mode       => $mode_gp,
	modified_flags => 1,
},

Const => {
	op_flags  => [ "constlike" ],
	attr      => "amd64_insn_mode_t insn_mode, int64_t offset, ir_entity *entity",
	init_attr => "attr->base.data.insn_mode = insn_mode;\n",
	attr_type => "amd64_movimm_attr_t",
	reg_req   => { out => [ "gp" ] },
	emit      => 'mov%M $%C, %D0',
	mode      => $mode_gp,
},

Movs => {
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "movs%Mq %AM, %^D0",
},

Movz => {
	state     => "exc_pinned",
	reg_req   => { out => [ "gp", "none", "none" ] },
	outs      => [ "res", "unused", "M" ],
	arity     => "variable",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
},

Jmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode" ],
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Cmp => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "none", "flags", "none" ] },
	arity     => "variable",
	outs      => [ "dummy", "flags", "M" ],
	attr       => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->data.op_mode   = op_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "cmp%M %AM",
	modified_flags => 1,
},

Lea => {
	irn_flags => [ "rematerializable" ],
	arity     => "variable",
	outs      => [ "res", "flags", "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_am_info_t am",
	reg_req   => { out => [ "gp", "flags", "none" ] },
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->am             = am;\n",
	emit      => "lea%M %A, %D0",
	mode      => $mode_gp,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr      => "x86_condition_code_t cc",
	attr_type => "amd64_cc_attr_t",
	mode      => "mode_T",
},

Store => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { out => [ "none" ] },
	arity     => "variable",
	outs      => [ "M" ],
	attr      => "amd64_insn_mode_t insn_mode, amd64_am_info_t am",
	init_attr => "attr->data.insn_mode = insn_mode;\n"
	            ."\tattr->am             = am;\n",
	mode      => "mode_M",
	emit      => "mov%M %S0, %A",
	mode      => "mode_M",
},

SwitchJmp => {
	op_flags     => [ "cfopcode", "forking" ],
	state        => "pinned",
	mode         => "mode_T",
	reg_req      => { in => [ "gp" ], out => [ "none" ] },
	out_arity    => "variable",
	attr_type    => "amd64_switch_jmp_attr_t",
	attr         => "const ir_switch_table *table, ir_entity *table_entity",
},

Call => {
	state     => "exc_pinned",
	arity     => "variable",
	out_arity => "variable",
	attr      => "amd64_am_info_t am",
	init_attr => "attr->am = am;\n",
	emit      => "call %A",
},

Start => {
	state     => "pinned",
	out_arity => "variable",
	ins       => [],
},

Return => {
	state  => "pinned",
	op_flags => [ "cfopcode" ],
	arity    => "variable",
	reg_req  => { out => [ "none" ] },
	mode     => "mode_X",
},

);
