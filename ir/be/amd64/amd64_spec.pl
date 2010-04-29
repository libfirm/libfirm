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
		{ name => "rbx", type => 2 },
		{ name => "rsi", type => 2 },
		{ name => "rdi", type => 2 },
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
		{ mode => "mode_Iu" }
	],
	fp => [
		{ name => "xmm0", type => 1 },
		{ name => "xmm1", type => 1 },
		{ name => "xmm2", type => 1 },
		{ name => "xmm3", type => 1 },
		{ name => "xmm4", type => 1 },
		{ name => "xmm5", type => 1 },
		{ name => "xmm6", type => 1 },
		{ name => "xmm7", type => 1 },
		{ name => "xmm8", type => 1 },
		{ name => "xmm9", type => 1 },
		{ name => "xmm10", type => 1 },
		{ name => "xmm11", type => 1 },
		{ name => "xmm12", type => 1 },
		{ name => "xmm13", type => 1 },
		{ name => "xmm14", type => 1 },
		{ name => "xmm15", type => 1 },
		{ mode => "mode_D" }
	]
);

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
	C  => "${arch}_emit_immediate(node);"
);

%init_attr = (
	amd64_attr_t           =>
		 "\tinit_amd64_attributes(res, flags, in_reqs, exec_units, n_res);",
	amd64_immediate_attr_t =>
		"\tinit_amd64_attributes(res, flags, in_reqs, exec_units, n_res);"
		. "\tinit_amd64_immediate_attributes(res, imm_value);",
);

%compare_attr = (
	amd64_attr_t           => "cmp_amd64_attr",
	amd64_immediate_attr_t => "cmp_amd64_attr_immediate",
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
Immediate => {
	op_flags  => "c",
	attr      => "unsigned imm_value",
	attr_type => "amd64_immediate_attr_t",
	reg_req   => { out => [ "gp" ] },
	emit      => '. movq %C, %D1',
	mode      => "mode_Iu",
},
);
