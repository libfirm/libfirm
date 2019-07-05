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
);

my $binOp = {
	irn_flags => [ "rematerializable" ],
	in_reqs   => [ "cls-gp", "cls-gp" ],
	out_reqs  => [ "cls-gp" ],
	ins       => [ "left", "right" ],
	outs      => [ "res" ],
};

%nodes = (
	Add => { template => $binOp },
);
