$arch = "amd64";

$mode_gp      = "mode_Lu";
$mode_flags   = "mode_Iu";
$mode_xmm     = "amd64_mode_xmm";
$status_flags = "all"; # TODO
$all_flags    = "all";

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
		{ mode => $mode_gp }
	],
	flags => [
		{ name => "eflags", dwarf => 49 },
		{ mode => $mode_flags, flags => "manual_ra" }
	],
	xmm => [
		{ name => "xmm0",  dwarf => 17 },
		{ name => "xmm1",  dwarf => 18 },
		{ name => "xmm2",  dwarf => 19 },
		{ name => "xmm3",  dwarf => 20 },
		{ name => "xmm4",  dwarf => 21 },
		{ name => "xmm5",  dwarf => 22 },
		{ name => "xmm6",  dwarf => 23 },
		{ name => "xmm7",  dwarf => 24 },
		{ name => "xmm8",  dwarf => 25 },
		{ name => "xmm9",  dwarf => 26 },
		{ name => "xmm10", dwarf => 27 },
		{ name => "xmm11", dwarf => 28 },
		{ name => "xmm12", dwarf => 29 },
		{ name => "xmm13", dwarf => 30 },
		{ name => "xmm14", dwarf => 31 },
		{ name => "xmm15", dwarf => 32 },
		{ mode => $mode_xmm }
	]
);

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

%custom_irn_flags = (
	commutative => "(arch_irn_flags_t)amd64_arch_irn_flag_commutative_binop",
);

$default_copy_attr = "amd64_copy_attr";

%init_attr = (
	amd64_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);",
	amd64_addr_attr_t =>
		"init_amd64_attributes(res, irn_flags_, in_reqs, n_res, op_mode);\n"
		."\tattr->insn_mode = insn_mode;\n"
		."\tattr->addr = addr;",
	amd64_binop_addr_attr_t =>
		"be_info_init_irn(res, irn_flags_, in_reqs, n_res);\n"
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
		"be_info_init_irn(res, irn_flags_, in_reqs, n_res);\n"
		."\t*attr = *attr_init;\n",
);

my $binop = {
	state          => "exc_pinned",
	in_reqs        => "...",
	out_reqs       => [ "gp", "flags", "none" ],
	outs           => [ "res", "flags", "M" ],
	attr_type      => "amd64_binop_addr_attr_t",
	attr           => "const amd64_binop_addr_attr_t *attr_init",
	modified_flags => $status_flags,
};

my $binop_commutative = {
	irn_flags      => [ "rematerializable", "commutative" ],
	state          => "exc_pinned",
	in_reqs        => "...",
	out_reqs       => [ "gp", "flags", "none" ],
	outs           => [ "res", "flags", "M" ],
	attr_type      => "amd64_binop_addr_attr_t",
	attr           => "const amd64_binop_addr_attr_t *attr_init",
	modified_flags => $status_flags,
};

my $divop = {
	state          => "pinned",
	in_reqs        => "...",
	out_reqs       => [ "rax", "flags", "none", "rdx" ],
	outs           => [ "res_div", "flags", "M", "res_mod" ],
	attr_type      => "amd64_addr_attr_t",
	fixed          => "amd64_addr_t addr = { { NULL, 0 }, NO_INPUT, NO_INPUT, NO_INPUT, 0, AMD64_SEGMENT_DEFAULT };\n"
	                 ."amd64_op_mode_t op_mode = AMD64_OP_RAX_REG;\n",
	attr           => "amd64_insn_mode_t insn_mode",
	modified_flags => $status_flags,
};

my $mulop = {
	# Do not rematerialize this node
	# TODO: should mark this commutative as soon as the backend code
	#       can handle this special case
	# It produces 2 results and has strict constraints
	state          => "exc_pinned",
	in_reqs        => "...",
	out_reqs       => [ "rax", "flags", "none", "rdx" ],
	outs           => [ "res_low", "flags", "M", "res_high" ],
	attr_type      => "amd64_addr_attr_t",
	attr           => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	modified_flags => $status_flags,
};

my $shiftop = {
	irn_flags      => [ "rematerializable" ],
	in_reqs        => "...",
	out_reqs       => [ "gp", "flags" ],
	outs           => [ "res", "flags" ],
	attr_type      => "amd64_shift_attr_t",
	attr           => "const amd64_shift_attr_t *attr_init",
	modified_flags => $status_flags
};

my $unop = {
	irn_flags      => [ "rematerializable" ],
	in_reqs        => [ "gp" ],
	out_reqs       => [ "in_r1", "flags" ],
	ins            => [ "val" ],
	outs           => [ "res", "flags" ],
	attr_type      => "amd64_addr_attr_t",
	attr           => "amd64_insn_mode_t insn_mode",
	fixed          => "amd64_op_mode_t op_mode = AMD64_OP_UNOP_REG;\n"
	                 ."amd64_addr_t addr = { { NULL, 0 }, NO_INPUT, NO_INPUT, NO_INPUT, 0, AMD64_SEGMENT_DEFAULT };",
	modified_flags => $status_flags
};

my $binopx = {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "xmm", "none", "none" ],
	outs      => [ "res", "none", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
};

my $binopx_commutative = {
	irn_flags => [ "rematerializable", "commutative" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "xmm", "none", "none" ],
	outs      => [ "res", "none", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
};

my $cvtop2x = {
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "xmm", "none", "none" ],
	outs      => [ "res", "none", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
};

my $cvtopx2i = {
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "gp", "none", "none" ],
	outs      => [ "res", "none", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
};

my $movopx = {
	state     => "exc_pinned",
	in_reqs   => "...",
	outs      => [ "res", "none", "M" ],
	out_reqs  => [ "xmm", "none", "none" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_op_mode_t op_mode, amd64_addr_t addr",
};

%nodes = (
push_am => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "rsp:I|S", "none" ],
	outs      => [ "stack", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "push%M %A",
},

push_reg => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => [ "rsp", "gp" ],
	out_reqs  => [ "rsp:I|S" ],
	ins       => [ "stack", "val" ],
	outs      => [ "stack" ],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	emit      => "pushq %^S1",
},

pop_am => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "rsp:I|S", "none" ],
	outs      => [ "stack", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "pop%M %A",
},

sub_sp => {
	state     => "pinned",
	in_reqs   => "...",
	out_reqs  => [ "rsp:I|S", "gp", "none" ],
	outs      => [ "stack", "addr", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "subq %AM\n".
	             "movq %%rsp, %D1",
	modified_flags => $status_flags,
},

leave => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => [ "rbp" ],
	out_reqs  => [ "rbp:I", "rsp:I|S" ],
	outs      => [ "frame", "stack" ],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	emit      => "leave",
},

add => {
	template => $binop_commutative,
	emit     => "add%M %AM",
},

and => {
	template => $binop_commutative,
	emit     => "and%M %AM",
},

div => {
	template => $divop,
	emit     => "div%M %AM",
},

idiv => {
	template => $divop,
	emit     => "idiv%M %AM",
},

imul => {
	template => $binop_commutative,
	emit     => "imul%M %AM",
},

imul_1op => {
	template => $mulop,
	emit     => "imul%M %AM",
},

mul => {
	template => $mulop,
	emit     => "mul%M %AM",
},

or => {
	template => $binop_commutative,
	emit     => "or%M %AM",
},

shl => {
	template => $shiftop,
	emit     => "shl%MS %SO",
},

shr => {
	template => $shiftop,
	emit     => "shr%MS %SO",
},

sar => {
	template => $shiftop,
	emit     => "sar%MS %SO",
},

sub => {
	template  => $binop,
	irn_flags => [ "rematerializable" ],
	emit      => "sub%M %AM",
},

sbb => {
	template => $binop,
	emit     => "sbb%M %AM",
},

neg => {
	template => $unop,
	emit     => "neg%M %AM",
},

not => {
	template => $unop,
	emit     => "not%M %AM",
},

xor => {
	template => $binop_commutative,
	emit     => "xor%M %AM",
},

xor_0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "gp", "flags" ],
	outs      => [ "res", "flags" ],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_REG_REG;",
	emit      => "xorl %3D0, %3D0",
	modified_flags => $status_flags,
},

mov_imm => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "gp" ],
	attr_type => "amd64_movimm_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, int64_t offset, ir_entity *entity",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_IMM64;",
	emit      => 'mov%MM $%C, %D0',
	mode      => $mode_gp,
},

movs => {
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "gp", "none", "none" ],
	outs      => [ "res", "unused", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "movs%Mq %AM, %^D0",
},

mov_gp => {
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "gp", "none", "none" ],
	outs      => [ "res", "unused", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
},

ijmp => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "unknown_jump" ],
	in_reqs   => "...",
	out_reqs  => [ "none", "none", "none" ],
	outs      => [ "X", "unused", "M" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit      => "jmp %*AM",
},

jmp => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	out_reqs => [ "none" ],
	fixed    => "amd64_op_mode_t op_mode = AMD64_OP_IMM32;",
	mode     => "mode_X",
},

cmp => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "none", "flags", "none" ],
	outs      => [ "dummy", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "cmp%M %AM",
	modified_flags => $status_flags,
},

lea => {
	irn_flags => [ "rematerializable" ],
	in_reqs   => "...",
	outs      => [ "res" ],
	out_reqs  => [ "gp" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_insn_mode_t insn_mode, amd64_addr_t addr",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_ADDR;\n",
	emit      => "lea%M %A, %D0",
	mode      => $mode_gp,
},

jcc => {
	state     => "pinned",
	op_flags  => [ "cfopcode", "forking" ],
	in_reqs   => [ "eflags" ],
	out_reqs  => [ "none", "none" ],
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr_type => "amd64_cc_attr_t",
	attr      => "x86_condition_code_t cc",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
},

mov_store => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "none" ],
	outs      => [ "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	mode      => "mode_M",
	emit      => "mov%M %S0, %A",
},

jmp_switch => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	in_reqs   => [ "gp" ],
	out_reqs  => "...",
	attr_type => "amd64_switch_jmp_attr_t",
	attr      => "const ir_switch_table *table, ir_entity *table_entity",
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
},

call => {
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => "...",
	outs      => [ "M", "stack", "flags", "first_result" ],
	attr_type => "amd64_addr_attr_t",
	attr      => "amd64_op_mode_t op_mode, amd64_addr_t addr",
	fixed     => "amd64_insn_mode_t insn_mode = INSN_MODE_64;\n",
	emit      => "call %*AM",
	modified_flags => $all_flags,
},

start => {
	irn_flags => [ "schedule_first" ],
	state     => "pinned",
	out_reqs  => "...",
	ins       => [],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	emit      => "",
},

ret => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	out_reqs => [ "none" ],
	ins      => [ "mem", "stack", "first_result" ],
	fixed    => "amd64_op_mode_t op_mode = AMD64_OP_NONE;\n",
	mode     => "mode_X",
	emit     => "ret",
},

# SSE

adds => {
	template => $binopx_commutative,
	emit     => "adds%MX %AM",
},

divs => {
	template => $binopx,
	emit     => "divs%MX %AM",
},

movs_xmm => {
	template => $movopx,
	attr     => "amd64_insn_mode_t insn_mode, amd64_op_mode_t op_mode, amd64_addr_t addr",
	emit     => "movs%MX %AM, %D0",
},

muls => {
	template => $binopx_commutative,
	emit     => "muls%MX %AM",
},

movs_store_xmm => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "none" ],
	outs      => [ "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	mode      => "mode_M",
	emit      => "movs%MX %^S0, %A",
},

subs => {
	template => $binopx,
	emit     => "subs%MX %AM",
},

ucomis => {
	irn_flags => [ "rematerializable" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "none", "flags", "none" ],
	outs      => [ "dummy", "flags", "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	emit      => "ucomis%MX %AM",
	modified_flags => $status_flags,
},

xorpd_0 => {
	op_flags  => [ "constlike" ],
	irn_flags => [ "rematerializable" ],
	out_reqs  => [ "xmm" ],
	outs      => [ "res" ],
	fixed     => "amd64_op_mode_t op_mode = AMD64_OP_REG_REG;",
	emit      => "xorpd %^D0, %^D0",
	mode      => $mode_xmm,
},

xorp  => {
	template => $binopx_commutative,
	emit     => "xorp%MX %AM",
},

# Conversion operations

cvtss2sd => {
	template => $cvtop2x,
	emit     => "cvtss2sd %AM, %^D0",
},

cvtsd2ss => {
	template => $cvtop2x,
	attr     => "amd64_op_mode_t op_mode, amd64_addr_t addr",
	fixed    => "amd64_insn_mode_t insn_mode = INSN_MODE_64;\n",
	emit     => "cvtsd2ss %AM, %^D0",
},

cvttsd2si => {
	template => $cvtopx2i,
	emit     => "cvttsd2si %AM, %D0",
},

cvttss2si => {
	template => $cvtopx2i,
	emit     => "cvttss2si %AM, %D0",
},

cvtsi2ss => {
	template => $cvtop2x,
	emit     => "cvtsi2ss %AM, %^D0",
},

cvtsi2sd => {
	template => $cvtop2x,
	emit     => "cvtsi2sd %AM, %^D0",
},

movq => {
	template => $movopx,
	fixed    => "amd64_insn_mode_t insn_mode = INSN_MODE_64;\n",
	emit     => "movq %AM, %D0",
},

movdqa => {
	template => $movopx,
	fixed    => "amd64_insn_mode_t insn_mode = INSN_MODE_128;\n",
	emit     => "movdqa %AM, %D0",
},

movdqu => {
	template => $movopx,
	fixed    => "amd64_insn_mode_t insn_mode = INSN_MODE_128;\n",
	emit     => "movdqu %AM, %D0",
},

movdqu_store => {
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => "...",
	out_reqs  => [ "none" ],
	outs      => [ "M" ],
	attr_type => "amd64_binop_addr_attr_t",
	attr      => "const amd64_binop_addr_attr_t *attr_init",
	mode      => "mode_M",
	emit      => "movdqu %^S0, %A",
},

l_punpckldq => {
	ins       => [ "arg0", "arg1" ],
	outs      => [ "res" ],
	attr_type => "",
	dump_func => "NULL",
	mode      => $mode_xmm,
},

l_subpd => {
	ins       => [ "arg0", "arg1" ],
	outs      => [ "res" ],
	attr_type => "",
	dump_func => "NULL",
	mode      => $mode_xmm,
},

l_haddpd => {
	ins       => [ "arg0", "arg1" ],
	outs      => [ "res" ],
	attr_type => "",
	dump_func => "NULL",
	mode      => $mode_xmm,
},

punpckldq => {
	template => $binopx,
	emit     => "punpckldq %AM",
},

subpd => {
	template => $binopx,
	emit     => "subpd %AM",
},

haddpd => {
	template => $binopx,
	emit     => "haddpd %AM",
},

);
