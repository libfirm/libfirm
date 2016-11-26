# This file is part of libFirm.
# Copyright (C) 2016 University of Karlsruhe.

$arch = "mips";

my $mode_gp = "mode_Iu"; # TODO

%reg_classes = (
	gp => [
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
		{ mode => $mode_gp }
	],
);

%init_attr = (
	mips_attr_t =>
		"be_info_init_irn(res, irn_flags, in_reqs, n_res);",
	mips_cond_attr_t =>
		"be_info_init_irn(res, irn_flags, in_reqs, n_res);\n".
		"\tattr->cond = cond;",
	mips_immediate_attr_t =>
		"be_info_init_irn(res, irn_flags, in_reqs, n_res);\n".
		"\tattr->ent = ent;\n".
		"\tattr->val = val;",
);

my $binOp = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "cls-gp", "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
	emit      => "{name}\t%D0, %S0, %S1",
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
	op_flags  => [ "uses_memory" ],
	state     => "exc_pinned",
	in_reqs   => [ "mem", "cls-gp" ],
	out_reqs  => [ "mem", "cls-gp" ],
	ins       => [ "mem", "base" ],
	outs      => [ "M", "res" ],
	attr_type => "mips_immediate_attr_t",
	attr      => "ir_entity *const ent, int32_t const val",
	emit      => "{name}\t%D1, %A",
};

my $storeOp = {
	op_flags  => [ "uses_memory" ],
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
	irn_flags => [ "simple_jump" ],
	op_flags  => [ "cfopcode" ],
	out_reqs  => [ "exec" ],
},

bcc => {
	state        => "pinned",
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

jr => {
	state    => "pinned",
	op_flags => [ "cfopcode" ],
	in_reqs  => "...",
	out_reqs => [ "exec" ],
	ins      => [ "mem", "stack", "addr", "first_result" ],
	emit     => "jr\t%S2\nnop",
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

mult_lo => {
	template => $binOp,
	emit     => "mult\t%S0, %S1\n".
	            "mflo\t%D0",
},

nor => { template => $binOp },

or => { template => $binOp },

ori => { template => $immediateOp },

sb => { template => $storeOp },

sh => { template => $storeOp },

sll => { template => $immediateOp },

sllv => { template => $binOp },

slt => { template => $binOp },

sltu => { template => $binOp },

sra => { template => $immediateOp },

srav => { template => $binOp },

srl => { template => $immediateOp },

srlv => { template => $binOp },

subu => { template => $binOp },

sw => { template => $storeOp },

xor => { template => $binOp },

xori => { template => $immediateOp },

);
