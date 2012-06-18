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
		{ name => "rsp", dwarf => 7, type => "ignore" }, # stackpointer
		{ name => "r8",  dwarf => 8 },
		{ name => "r9",  dwarf => 9 },
		{ name => "r10", dwarf => 10 },
		{ name => "r11", dwarf => 11 },
		{ name => "r12", dwarf => 12 },
		{ name => "r13", dwarf => 13 },
		{ name => "r14", dwarf => 14 },
		{ name => "r15", dwarf => 15 },
#		{ name => "gp_NOREG", type => "ignore" }, # we need a dummy register for NoReg nodes
		{ mode => "mode_Lu" }
	],
#	fp => [
#		{ name => "xmm0",  dwarf => 17 },
#		{ name => "xmm1",  dwarf => 18 },
#		{ name => "xmm2",  dwarf => 19 },
#		{ name => "xmm3",  dwarf => 20 },
#		{ name => "xmm4",  dwarf => 21 },
#		{ name => "xmm5",  dwarf => 22 },
#		{ name => "xmm6",  dwarf => 23 },
#		{ name => "xmm7",  dwarf => 24 },
#		{ name => "xmm8",  dwarf => 25 },
#		{ name => "xmm9",  dwarf => 26 },
#		{ name => "xmm10", dwarf => 27 },
#		{ name => "xmm11", dwarf => 28 },
#		{ name => "xmm12", dwarf => 29 },
#		{ name => "xmm13", dwarf => 30 },
#		{ name => "xmm14", dwarf => 31 },
#		{ name => "xmm15", dwarf => 32 },
#		{ mode => "mode_D" }
#	]
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
		$res .= "\tarch_add_irn_flags(res, arch_irn_flags_modify_flags);\n";
	}
	return $res;
}
$custom_init_attr_func = \&amd64_custom_init_attr;

$default_copy_attr = "amd64_copy_attr";

%init_attr = (
	amd64_attr_t           =>
		 "\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);",
	amd64_SymConst_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);"
		. "\tinit_amd64_SymConst_attributes(res, entity);",
	amd64_condcode_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, n_res);"
		. "\tinit_amd64_condcode_attributes(res, pnc);",
);

%compare_attr = (
	amd64_attr_t           => "cmp_amd64_attr",
	amd64_SymConst_attr_t  => "cmp_amd64_attr_SymConst",
	amd64_condcode_attr_t  => "cmp_amd64_attr_condcode",
);

%nodes = (
Push => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "rsp" ], out => [ "rsp:I|S", "none" ] },
	ins       => [ "base", "index", "mem", "val", "stack" ],
	emit      => 'push %S0',
	outs      => [ "stack", "M" ],
	am        => "source,unary",
},

Add => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => 'add %S1, %D0',
	mode       => $mode_gp,
	modified_flags => 1,
},

And => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => 'and %S1, %D0',
	mode       => $mode_gp,
	modified_flags => 1,
},

IMul => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	reg_req   => { in  => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
	emit      => 'imul %S1, %D0',
	mode      => $mode_gp,
	am        => "source,binary",
	modified_flags => $status_flags
},

Or => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => 'or %S1, %D0',
	mode       => $mode_gp,
	modified_flags => 1,
},

Shl => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "rcx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	out       => [ "res" ],
	emit      => 'shl %%cl, %D0',
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Shr => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "rcx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	out       => [ "res" ],
	emit      => 'shr %%cl, %D0',
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sar => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp", "rcx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	out       => [ "res" ],
	emit      => 'sar %%cl, %D0',
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sub => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => 'sub %S1, %D0',
	mode       => $mode_gp,
	modified_flags => 1,
},

Neg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1" ] },
	emit      => 'neg %S0',
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Not => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	emit      => 'not %S0',
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ], out => [ "in_r1 !in_r2" ] },
	ins        => [ "left", "right" ],
	outs       => [ "res" ],
	emit       => 'xor %S1, %D0',
	mode       => $mode_gp,
	modified_flags => 1,
},

Immediate => {
	op_flags  => [ "constlike" ],
	attr      => "unsigned imm_value",
	init_attr => "attr->ext.imm_value = imm_value;",
	reg_req   => { out => [ "gp" ] },
	emit      => 'mov %C, %D0',
	mode      => $mode_gp,
},

SymConst => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	reg_req   => { out => [ "gp" ] },
	outs      => [ "res" ],
	mode      => $mode_gp,
},

Conv => {
	state     => "exc_pinned",
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "val" ],
	outs      => [ "res" ],
	mode      => $mode_gp,
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
	reg_req   => { in  => [ "gp", "gp" ],
	               out => [ "flags" ] },
	ins       => [ "left", "right" ],
	outs      => [ "eflags" ],
	emit      => 'cmp %S0, %S1',
	attr      => "int ins_permuted, int cmp_unsigned",
	init_attr => "attr->data.ins_permuted   = ins_permuted;\n".
	             "\tattr->data.cmp_unsigned = cmp_unsigned;\n",
	mode      => $mode_flags,
	modified_flags => 1,
},

Jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr      => "ir_relation relation",
	init_attr => "attr->ext.relation = relation;",
	mode      => "mode_T",
},

Load => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ],
	               out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res",  "M" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	emit      => "mov %O(%S0), %D0"
},

FrameAddr => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "base" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	mode      => $mode_gp,
},

Store => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	mode      => "mode_M",
	emit      => "mov %S1, %O(%S0)"
},
);
