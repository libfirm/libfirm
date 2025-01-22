# This file is part of libFirm.
# Copyright (C) 2017 University of Karlsruhe.

$arch = "mips";

my $mode_gp = "mode_Iu"; # TODO

%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
			{ name => "zero", encoding =>  0 },
			{ name => "at",   encoding =>  1 },
			{ name => "v0",   encoding =>  2 },
			{ name => "v1",   encoding =>  3 },
			{ name => "a0",   encoding =>  4 },
			{ name => "a1",   encoding =>  5 },
			{ name => "a2",   encoding =>  6 },
			{ name => "a3",   encoding =>  7 },
			{ name => "t0",   encoding =>  8 },
			{ name => "t1",   encoding =>  9 },
			{ name => "t2",   encoding => 10 },
			{ name => "t3",   encoding => 11 },
			{ name => "t4",   encoding => 12 },
			{ name => "t5",   encoding => 13 },
			{ name => "t6",   encoding => 14 },
			{ name => "t7",   encoding => 15 },
			{ name => "s0",   encoding => 16 },
			{ name => "s1",   encoding => 17 },
			{ name => "s2",   encoding => 18 },
			{ name => "s3",   encoding => 19 },
			{ name => "s4",   encoding => 20 },
			{ name => "s5",   encoding => 21 },
			{ name => "s6",   encoding => 22 },
			{ name => "s7",   encoding => 23 },
			{ name => "t8",   encoding => 24 },
			{ name => "t9",   encoding => 25 },
			{ name => "k0",   encoding => 26 },
			{ name => "k1",   encoding => 27 },
			{ name => "gp",   encoding => 28 },
			{ name => "sp",   encoding => 29 },
			{ name => "s8",   encoding => 30 },
			{ name => "ra",   encoding => 31 },
		]
	},
);

%init_attr = (
	mips_attr_t => "",
	mips_cond_attr_t =>
		"attr->cond = cond;",
	mips_immediate_attr_t =>
		"attr->ent = ent;\n".
		"\tattr->val = val;",
	mips_switch_attr_t =>
		"be_switch_attr_init(res, &attr->swtch, table, table_entity);",
);

my $binOp = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "cls-gp", "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
	emit      => "{name}\t%D0, %S0, %S1",
};

my $callOp = {
  state     => "exc_pinned",
  in_reqs   => "...",
  out_reqs  => "...",
  ins       => [ "mem", "stack", "first_argument" ],
  outs      => [ "M",   "stack", "first_result" ],
};

my $divOp = {
	in_reqs   => [ "cls-gp", "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
	emit      => "{name}\t%S0, %S1\n".
	             "mflo\t%D0",
};

my $immediateOp = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left" ],
	outs      => [ "res" ],
	attr_type => "mips_immediate_attr_t",
	attr      => "ir_entity *const ent, int32_t const val",
	emit      => "{name}\t%D0, %S0, %I",
};

my $loadOp = {
	state     => "exc_pinned",
	in_reqs   => [ "mem", "cls-gp" ],
	out_reqs  => [ "mem", "cls-gp" ],
	ins       => [ "mem", "base" ],
	outs      => [ "M", "res" ],
	attr_type => "mips_immediate_attr_t",
	attr      => "ir_entity *const ent, int32_t const val",
	emit      => "{name}\t%D1, %A",
};

my $modOp = {
	in_reqs   => [ "cls-gp", "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
	emit      => "{name}\t%S0, %S1\n".
	             "mfhi\t%D0",
};

my $storeOp = {
	state     => "exc_pinned",
	in_reqs   => [ "mem", "cls-gp", "cls-gp" ],
	out_reqs  => [ "mem" ],
	ins       => [ "mem", "base", "value" ],
	outs      => [ "M" ],
	attr_type => "mips_immediate_attr_t",
	attr      => "ir_entity *const ent, int32_t const val",
	emit      => "{name}\t%S2, %A",
};

%nodes = (

addu => { template => $binOp },

addiu => { template => $immediateOp },

and => { template => $binOp },

andi => { template => $immediateOp },

b => {
	state     => "pinned",
	irn_flags => [ "simple_jump", "fallthrough" ],
	op_flags  => [ "cfopcode" ],
	out_reqs  => [ "exec" ],
},

bcc => {
	state        => "pinned",
	irn_flags    => [ "fallthrough" ],
	op_flags     => [ "cfopcode", "forking" ],
	constructors => {
		""  => { in_reqs => [ "cls-gp", "cls-gp" ], ins => [ "left", "right" ] },
		"z" => { in_reqs => [ "cls-gp" ],           ins => [ "left" ]          },
	},
	out_reqs     => [ "exec", "exec" ],
	outs         => [ "false", "true" ],
	attr_type    => "mips_cond_attr_t",
	attr         => "mips_cond_t const cond",
},

div_lo => {
	template => $divOp,
	name     => "div",
},

divu_lo => {
	template => $divOp,
	name     => "divu",
},

ijmp => {
	state    => "pinned",
	op_flags => [ "cfopcode", "unknown_jump" ],
	in_reqs  => [ "cls-gp" ],
	out_reqs => [ "exec" ],
	emit     => "jr\t%S0\n".
	            "nop",
},

jal => {
	template  => $callOp,
	attr_type => "mips_immediate_attr_t",
	attr      => "ir_entity *const ent, int32_t const val",
	emit      => "jal\t%J\n".
	             "nop",
},

jalr => {
	template => $callOp,
	emit     => "jalr\t%S2\n".
	            "nop",
},

lb => { template => $loadOp },

lbu => { template => $loadOp },

lh => { template => $loadOp },

lhu => { template => $loadOp },

lui => {
	template  => $immediateOp,
	in_reqs   => [],
	ins       => [],
	emit      => "lui\t%D0, %H",
},

lw => { template => $loadOp },

div_hi => {
	template => $modOp,
	name     => "div",
},

divu_hi => {
	template => $modOp,
	name     => "divu",
},

mult_hi => {
	template => $binOp,
	emit     => "mult\t%S0, %S1\n".
	            "mfhi\t%D0",
},

mult_lo => {
	template => $binOp,
	emit     => "mult\t%S0, %S1\n".
	            "mflo\t%D0",
},

multu_hi => {
	template => $binOp,
	emit     => "multu\t%S0, %S1\n".
	            "mfhi\t%D0",
},

nor => { template => $binOp },

or => { template => $binOp },

ori => { template => $immediateOp },

ret => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	out_reqs => [ "exec" ],
	ins      => [ "mem", "stack", "addr", "first_result" ],
	emit     => "jr\t%S2\nnop",
},

sb => { template => $storeOp },

sh => { template => $storeOp },

sll => { template => $immediateOp },

sllv => { template => $binOp },

slt => { template => $binOp },

sltiu => { template => $immediateOp },

sltu => { template => $binOp },

sra => { template => $immediateOp },

srav => { template => $binOp },

srl => { template => $immediateOp },

srlv => { template => $binOp },

subu => { template => $binOp },

sw => { template => $storeOp },

switch => {
	op_flags  => [ "cfopcode", "forking" ],
	state     => "pinned",
	in_reqs   => [ "cls-gp" ],
	out_reqs  => "...",
	attr_type => "mips_switch_attr_t",
	attr      => "const ir_switch_table *table, ir_entity *table_entity",
},

xor => { template => $binOp },

xori => { template => $immediateOp },

);
