# This file is part of libFirm.
# Copyright (C) 2019 University of Karlsruhe.

$arch = "vhdl";

my $mode_gp = "mode_Iu"; # TODO

%reg_classes = (
	gp => {
		mode => $mode_gp,
		registers => [
			{ name => "zero", encoding =>  0 }, # dummy register
		]
	},
);

%init_attr = (
	vhdl_attr_t => "",
	vhdl_cmp_attr_t => "attr->rel = rel;",
	vhdl_immediate_attr_t =>
		"attr->ent = ent;\n".
		"\tattr->val = val;",
	vhdl_varsig_attr_t => "attr->name = name;",
);

my $binOp = {
	in_reqs   => "..." ,
	out_reqs  => "..." ,
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
};

my $unOp = {
	in_reqs   => "..." ,
	out_reqs  => "..." ,
	ins       => [ "val" ],
	outs      => [ "res" ],
};
my $assignOp = {
	ins       => [ "val" ],
	in_reqs   => "...",
	outs      => [ "res" ],
	out_reqs  => "...",
	attr_type => "vhdl_varsig_attr_t",
	attr      => "char *name"
};

%nodes = (
	Add         => { template => $binOp },
	Sub         => { template => $binOp },
	And         => { template => $binOp },
	Or          => { template => $binOp },
	Eor         => { template => $binOp },
	Mul         => { template => $binOp },

	Shl_Const   => { template => $binOp },
	Shl_Barrel  => { template => $binOp },
	Shr_Const   => { template => $binOp },
	Shr_Barrel  => { template => $binOp },
	Shrs_Const  => { template => $binOp },
	Shrs_Barrel => { template => $binOp },

	Not         => { template => $unOp },
	Minus       => { template => $unOp },
	Conv        => { template => $unOp },

	Jmp         => {
		state     => "pinned",
		irn_flags => [ "simple_jump", "fallthrough" ],
		op_flags  => [ "cfopcode" ],
		out_reqs  => [ "exec" ],
	},

	Cond        => {
		state     => "pinned",
		irn_flags => [ "fallthrough" ],
		op_flags  => [ "cfopcode", "forking" ],
		in_reqs   => "...",
		ins       => [ "left", "right" ],
		out_reqs  => [ "exec", "exec" ],
		outs      => [ "false", "true" ],
	},

	Cmp         => {
		template  => $binOp,
		attr_type => "vhdl_cmp_attr_t",
		attr      => "ir_relation rel"
	},

	Mux         => {
		ins      => [ "sel", "true", "false" ],
		in_reqs  => "...",
		outs     => [ "res" ],
		out_reqs => "...",
	},

	Const       => {
		state     => "pinned",
		outs      => [ "res" ],
		out_reqs  => "...",
		attr_type => "vhdl_immediate_attr_t",
		attr      => "ir_entity *const ent, int32_t const val",
	},

	AssignVar   => { template => $assignOp},
	AssignSig => { template => $assignOp}


);
