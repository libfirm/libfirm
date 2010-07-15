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
#   op_flags  => "N|L|C|X|I|F|Y|H|c|K",                 # optional
#   irn_flags => "R|N|I"                                # optional
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

# op_flags: flags for the operation, OPTIONAL (default is "N")
# the op_flags correspond to the firm irop_flags:
#   N   irop_flag_none
#   L   irop_flag_labeled
#   C   irop_flag_commutative
#   X   irop_flag_cfopcode
#   I   irop_flag_ip_cfopcode
#   F   irop_flag_fragile
#   Y   irop_flag_forking
#   H   irop_flag_highlevel
#   c   irop_flag_constlike
#   K   irop_flag_keep
#
# irn_flags: special node flags, OPTIONAL (default is 0)
# following irn_flags are supported:
#   R   rematerializeable
#   N   not spillable
#   I   ignore for register allocation
#
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
# rd_constructor: for every operation there will be a
#      new_rd_<arch>_<op-name> function with the arguments from above
#      which creates the ir_node corresponding to the defined operation
#      you can either put the complete source code of this function here
#
#      This key is OPTIONAL. If omitted, the following constructor will
#      be created:
#      if (!op_<arch>_<op-name>) assert(0);
#      for i = 1 to arity
#         set in[i] = op_i
#      done
#      res = new_ir_node(db, irg, block, op_<arch>_<op-name>, mode, arity, in)
#      return res
#
# NOTE: rd_constructor and args are only optional if and only if arity is 0,1,2 or 3

# register types:
#   0 - no special type
#   1 - caller save (register must be saved by the caller of a function)
#   2 - callee save (register must be saved by the called function)
#   4 - ignore (do not assign this register)
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "rax", type => 1 },
		{ name => "rcx", type => 1 },
		{ name => "rdx", type => 1 },
		{ name => "rsi", type => 1 },
		{ name => "rdi", type => 1 },
		{ name => "rbx", type => 2 },
		{ name => "rbp", type => 2 },
		{ name => "rsp", type => 4 }, # stackpointer?
		{ name => "r8",  type => 1 },
		{ name => "r9",  type => 1 },
		{ name => "r10", type => 1 },
		{ name => "r11", type => 1 },
		{ name => "r12", type => 2 },
		{ name => "r13", type => 2 },
		{ name => "r14", type => 2 },
		{ name => "r15", type => 2 },
#		{ name => "gp_NOREG", type => 4 }, # we need a dummy register for NoReg nodes
		{ mode => "mode_Lu" }
	],
#	fp => [
#		{ name => "xmm0", type => 1 },
#		{ name => "xmm1", type => 1 },
#		{ name => "xmm2", type => 1 },
#		{ name => "xmm3", type => 1 },
#		{ name => "xmm4", type => 1 },
#		{ name => "xmm5", type => 1 },
#		{ name => "xmm6", type => 1 },
#		{ name => "xmm7", type => 1 },
#		{ name => "xmm8", type => 1 },
#		{ name => "xmm9", type => 1 },
#		{ name => "xmm10", type => 1 },
#		{ name => "xmm11", type => 1 },
#		{ name => "xmm12", type => 1 },
#		{ name => "xmm13", type => 1 },
#		{ name => "xmm14", type => 1 },
#		{ name => "xmm15", type => 1 },
#		{ mode => "mode_D" }
#	]
	flags => [
		{ name => "eflags", type => 0 },
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
		$res .= "\tarch_irn_add_flags(res, arch_irn_flags_modify_flags);\n";
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
		 "\tinit_amd64_attributes(res, flags, in_reqs, exec_units, n_res);",
	amd64_SymConst_attr_t =>
		"\tinit_amd64_attributes(res, flags, in_reqs, exec_units, n_res);"
		. "\tinit_amd64_SymConst_attributes(res, entity);",
	amd64_condcode_attr_t =>
		"\tinit_amd64_attributes(res, flags, in_reqs, exec_units, n_res);"
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
	attr      => "pn_Cmp pnc",
	init_attr => "attr->ext.pnc = pnc;",
	mode      => "mode_T",
},
Load => {
	op_flags  => [ "labeled", "fragile" ],
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
	op_flags  => [ "labeled", "fragile" ],
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
