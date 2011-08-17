# Creation: 2006/02/13
# $Id: amd64_spec.pl 26673 2009-10-01 16:43:13Z matze $

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "amd64";

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   arity     => "0|1|2|3 ... |variable|dynamic|any",   # optional
#   state     => "floats|pinned|mem_pinned|exc_pinned", # optional
#   args      => [
#                    { type => "type 1", name => "name 1" },
#                    { type => "type 2", name => "name 2" },
#                    ...
#                  ],
#   comment   => "any comment for constructor",  # optional
#   reg_req   => { in => [ "reg_class|register" ], out => [ "reg_class|register|in_rX" ] },
#   cmp_attr  => "c source code for comparing node attributes", # optional
#   outs      => { "out1", "out2" },# optional, creates pn_op_out1, ... consts
#   ins       => { "in1", "in2" },  # optional, creates n_op_in1, ... consts
#   mode      => "mode_Iu",         # optional, predefines the mode
#   emit      => "emit code with templates",   # optional for virtual nodes
#   attr      => "additional attribute arguments for constructor", # optional
#   init_attr => "emit attribute initialization template",         # optional
#   rd_constructor => "c source code which constructs an ir_node", # optional
#   hash_func => "name of the hash function for this operation",   # optional, get the default hash function else
#   latency   => "latency of this operation (can be float)"        # optional
#   attr_type => "name of the attribute struct",                   # optional
# },
#
# ... # (all nodes you need to describe)
#
# ); # close the %nodes initializer

# state: state of the operation, OPTIONAL (default is "floats")
#
# arity: arity of the operation, MUST NOT BE OMITTED
#
# args:  the OPTIONAL arguments of the node constructor (debug, irg and block
#        are always the first 3 arguments and are always autmatically
#        created)
#        If this key is missing the following arguments will be created:
#        for i = 1 .. arity: ir_node *op_i
#        ir_mode *mode
#
# outs:  if a node defines more than one output, the names of the projections
#        nodes having outs having automatically the mode mode_T
#
# comment: OPTIONAL comment for the node constructor
#
# register types:
$normal      =  0; # no special type
$ignore      =  1; # ignore (do not assign this register)
$arbitrary   =  2; # emitter can choose an arbitrary register of this class
$virtual     =  4; # the register is a virtual one
$state       =  8; # register represents a state
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "rax" },
		{ name => "rcx" },
		{ name => "rdx" },
		{ name => "rsi" },
		{ name => "rdi" },
		{ name => "rbx" },
		{ name => "rbp" },
		{ name => "rsp", type => $ignore }, # stackpointer?
		{ name => "r8" },
		{ name => "r9" },
		{ name => "r10" },
		{ name => "r11" },
		{ name => "r12" },
		{ name => "r13" },
		{ name => "r14" },
		{ name => "r15" },
#		{ name => "gp_NOREG", type => $ignore }, # we need a dummy register for NoReg nodes
		{ mode => "mode_Lu" }
	],
#	fp => [
#		{ name => "xmm0" },
#		{ name => "xmm1" },
#		{ name => "xmm2" },
#		{ name => "xmm3" },
#		{ name => "xmm4" },
#		{ name => "xmm5" },
#		{ name => "xmm6" },
#		{ name => "xmm7" },
#		{ name => "xmm8" },
#		{ name => "xmm9" },
#		{ name => "xmm10" },
#		{ name => "xmm11" },
#		{ name => "xmm12" },
#		{ name => "xmm13" },
#		{ name => "xmm14" },
#		{ name => "xmm15" },
#		{ mode => "mode_D" }
#	]
	flags => [
		{ name => "eflags" },
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

%emit_templates = (
	S1 => "${arch}_emit_source_register(node, 0);",
	S2 => "${arch}_emit_source_register(node, 1);",
	S3 => "${arch}_emit_source_register(node, 2);",
	S4 => "${arch}_emit_source_register(node, 3);",
	S5 => "${arch}_emit_source_register(node, 4);",
	S6 => "${arch}_emit_source_register(node, 5);",
	D1 => "${arch}_emit_dest_register(node, 0);",
	D2 => "${arch}_emit_dest_register(node, 1);",
	D3 => "${arch}_emit_dest_register(node, 2);",
	D4 => "${arch}_emit_dest_register(node, 3);",
	D5 => "${arch}_emit_dest_register(node, 4);",
	D6 => "${arch}_emit_dest_register(node, 5);",
	C  => "${arch}_emit_immediate(node);",
	O  => "${arch}_emit_fp_offset(node);",
);

%init_attr = (
	amd64_attr_t           =>
		 "\tinit_amd64_attributes(res, irn_flags_, in_reqs, exec_units, n_res);",
	amd64_SymConst_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, exec_units, n_res);"
		. "\tinit_amd64_SymConst_attributes(res, entity);",
	amd64_condcode_attr_t =>
		"\tinit_amd64_attributes(res, irn_flags_, in_reqs, exec_units, n_res);"
		. "\tinit_amd64_condcode_attributes(res, pnc);",
);

%compare_attr = (
	amd64_attr_t           => "cmp_amd64_attr",
	amd64_SymConst_attr_t  => "cmp_amd64_attr_SymConst",
	amd64_condcode_attr_t  => "cmp_amd64_attr_condcode",
);

%nodes = (
Push => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none", "gp", "rsp" ], out => [ "rsp:I|S", "none" ] },
	ins       => [ "base", "index", "mem", "val", "stack" ],
	emit      => '. push %S1',
	outs      => [ "stack", "M" ],
	am        => "source,unary",
	latency   => 2,
#	units     => [ "GP" ],
},
Add => {
	op_flags   => [ "commutative" ],
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ],
	                out => [ "gp" ] },
	in         => [ "left", "right" ],
	outs       => [ "res" ],
	mode       => $mode_gp,
	modified_flags => 1,
},
Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constraints
	state     => "exc_pinned",
	reg_req   => { in  => [ "rax", "gp" ],
	               out => [ "rax rdx" ] },
	ins       => [ "left", "right" ],
	emit      => '. mul %S2',
	outs      => [ "res" ],
	mode      => $mode_gp,
	am        => "source,binary",
	modified_flags => $status_flags
},
Sub => {
	irn_flags  => [ "rematerializable" ],
	state      => "exc_pinned",
	reg_req    => { in => [ "gp", "gp" ],
	                out => [ "gp" ] },
	in         => [ "left", "right" ],
	outs       => [ "res" ],
	mode       => $mode_gp,
	modified_flags => 1,
},
Neg => {
	irn_flags => [ "rematerializable" ],
	reg_req   => { in => [ "gp" ],
	               out => [ "in_r1", "flags" ] },
	emit      => '. neg %S1',
	ins       => [ "val" ],
	outs      => [ "res", "flags" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},
Immediate => {
	op_flags  => [ "constlike" ],
	attr      => "unsigned imm_value",
	init_attr => "attr->ext.imm_value = imm_value;",
	reg_req   => { out => [ "gp" ] },
	emit      => '. mov %C, %D1',
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
	emit      => '. cmp %S1, %S2',
	attr      => "int ins_permuted, int cmp_unsigned",
	init_attr => "attr->data.ins_permuted   = ins_permuted;\n".
	             "\tattr->data.cmp_unsigned = cmp_unsigned;\n",
	mode      => $mode_flags,
	modified_flags => 1,
},
Jcc => {
	state     => "pinned",
	op_flags  => [ "labeled", "cfopcode", "forking" ],
	reg_req   => { in  => [ "eflags" ], out => [ "none", "none" ] },
	ins       => [ "eflags" ],
	outs      => [ "false", "true" ],
	attr      => "ir_relation relation",
	init_attr => "attr->ext.relation = relation;",
	mode      => "mode_T",
},
Load => {
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ],
	               out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res",  "M" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	emit      => ". mov %O(%S1), %D1"
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
	op_flags  => [ "labeled" ],
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "ptr", "val", "mem" ],
	outs      => [ "M" ],
	attr      => "ir_entity *entity",
	attr_type => "amd64_SymConst_attr_t",
	mode      => "mode_M",
	emit      => ". mov %S2, %O(%S1)"
},

#NoReg_GP => {
#	state     => "pinned",
#	op_flags  => [ "constlike", "dump_noblcok", "dump_noinput" ],
#	reg_req   => { out => [ "gp_NOREG:I" ] },
#	units     => [],
#	emit      => "",
#	latency   => 0,
#	mode      => $mode_gp,
#},
);
