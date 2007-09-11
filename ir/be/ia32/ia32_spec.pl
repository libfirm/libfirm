# Creation: 2005/10/19
# $Id$
# This is the specification for the ia32 assembler Firm-operations

use File::Basename;

$new_emit_syntax = 1;
my $myname = $0;

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)
$arch = "ia32";

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   op_flags  => "N|L|C|X|I|F|Y|H|c|K",
#   irn_flags => "R|N|I|S"
#   arity     => "0|1|2|3 ... |variable|dynamic|any",
#   state     => "floats|pinned|mem_pinned|exc_pinned",
#   args      => [
#                    { type => "type 1", name => "name 1" },
#                    { type => "type 2", name => "name 2" },
#                    ...
#                  ],
#   comment   => "any comment for constructor",
#   reg_req   => { in => [ "reg_class|register" ], out => [ "reg_class|register|in_rX" ] },
#   cmp_attr  => "c source code for comparing node attributes",
#   emit      => "emit code with templates",
#   attr      => "attitional attribute arguments for constructor"
#   init_attr => "emit attribute initialization template"
#   rd_constructor => "c source code which constructs an ir_node"
#   latency   => "latency of this operation (can be float)"
#   attr_type => "name of the attribute struct",
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
#   S   modifies stack pointer
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
#        One can also annotate some flags for each out, additional to irn_flags.
#        They are separated from name with a colon ':', and concatenated by pipe '|'
#        Only I and S are available at the moment (same meaning as in irn_flags).
#        example: [ "frame:I", "stack:I|S", "M" ]
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
#
# latency: the latency of the operation, default is 1
#

# register types:
#   0 - no special type
#   1 - caller save (register must be saved by the caller of a function)
#   2 - callee save (register must be saved by the called function)
#   4 - ignore (do not assign this register)
#   8 - emitter can choose an arbitrary register of this class
#  16 - the register is a virtual one
#  32 - register represents a state
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold
%reg_classes = (
	gp => [
		{ name => "eax", type => 1 },
		{ name => "edx", type => 1 },
		{ name => "ebx", type => 2 },
		{ name => "ecx", type => 1 },
		{ name => "esi", type => 2 },
		{ name => "edi", type => 2 },
		{ name => "ebp", type => 2 },
		{ name => "esp", type => 4 },
		{ name => "gp_NOREG", type => 4 | 8 | 16 }, # we need a dummy register for NoReg nodes
		{ name => "gp_UKNWN", type => 4 | 8 | 16 },  # we need a dummy register for Unknown nodes
		{ mode => "mode_Iu" }
	],
	mmx => [
		{ name => "mm0", type => 4 },
		{ name => "mm1", type => 4 },
		{ name => "mm2", type => 4 },
		{ name => "mm3", type => 4 },
		{ name => "mm4", type => 4 },
		{ name => "mm5", type => 4 },
		{ name => "mm6", type => 4 },
		{ name => "mm7", type => 4 },
		{ mode => "mode_E" }
	],
	xmm => [
		{ name => "xmm0", type => 1 },
		{ name => "xmm1", type => 1 },
		{ name => "xmm2", type => 1 },
		{ name => "xmm3", type => 1 },
		{ name => "xmm4", type => 1 },
		{ name => "xmm5", type => 1 },
		{ name => "xmm6", type => 1 },
		{ name => "xmm7", type => 1 },
		{ name => "xmm_NOREG", type => 4 | 16 },     # we need a dummy register for NoReg nodes
		{ name => "xmm_UKNWN", type => 4 | 8 | 16},  # we need a dummy register for Unknown nodes
		{ mode => "mode_E" }
	],
	vfp => [
		{ name => "vf0", type => 1 | 16 },
		{ name => "vf1", type => 1 | 16 },
		{ name => "vf2", type => 1 | 16 },
		{ name => "vf3", type => 1 | 16 },
		{ name => "vf4", type => 1 | 16 },
		{ name => "vf5", type => 1 | 16 },
		{ name => "vf6", type => 1 | 16 },
		{ name => "vf7", type => 1 | 16 },
		{ name => "vfp_NOREG", type => 4 | 8 | 16 }, # we need a dummy register for NoReg nodes
		{ name => "vfp_UKNWN", type => 4 | 8 | 16 },  # we need a dummy register for Unknown nodes
		{ mode => "mode_E" }
	],
	st => [
		{ name => "st0", realname => "st",    type => 4 },
		{ name => "st1", realname => "st(1)", type => 4 },
		{ name => "st2", realname => "st(2)", type => 4 },
		{ name => "st3", realname => "st(3)", type => 4 },
		{ name => "st4", realname => "st(4)", type => 4 },
		{ name => "st5", realname => "st(5)", type => 4 },
		{ name => "st6", realname => "st(6)", type => 4 },
		{ name => "st7", realname => "st(7)", type => 4 },
		{ mode => "mode_E" }
	],
	fp_cw => [	# the floating point control word
		{ name => "fpcw", type => 4 | 32},
		{ mode => "mode_fpcw" }
	],
	flags => [
		{ name => "eflags", type => 4 },
		{ mode => "mode_Iu" }
	],
	fp_sw => [
		{ name => "fpsw", type => 4 },
		{ mode => "mode_Hu" }
	],
); # %reg_classes

%flags = (
	CF  => { reg => "eflags", bit => 0 },
	PF  => { reg => "eflags", bit => 2 },
	AF  => { reg => "eflags", bit => 4 },
	ZF  => { reg => "eflags", bit => 6 },
	SF  => { reg => "eflags", bit => 7 },
	TF  => { reg => "eflags", bit => 8 },
	IF  => { reg => "eflags", bit => 9 },
	DF  => { reg => "eflags", bit => 10 },
	OF  => { reg => "eflags", bit => 11 },
	IOPL0 => { reg => "eflags", bit => 12 },
	IOPL1 => { reg => "eflags", bit => 13 },
	NT  => { reg => "eflags", bit => 14 },
	RF  => { reg => "eflags", bit => 16 },
	VM  => { reg => "eflags", bit => 17 },
	AC  => { reg => "eflags", bit => 18 },
	VIF => { reg => "eflags", bit => 19 },
	VIP => { reg => "eflags", bit => 20 },
	ID  => { reg => "eflags", bit => 21 },

	FP_IE => { reg => "fpsw", bit => 0 },
	FP_DE => { reg => "fpsw", bit => 1 },
	FP_ZE => { reg => "fpsw", bit => 2 },
	FP_OE => { reg => "fpsw", bit => 3 },
	FP_UE => { reg => "fpsw", bit => 4 },
	FP_PE => { reg => "fpsw", bit => 5 },
	FP_SF => { reg => "fpsw", bit => 6 },
	FP_ES => { reg => "fpsw", bit => 7 },
	FP_C0 => { reg => "fpsw", bit => 8 },
	FP_C1 => { reg => "fpsw", bit => 9 },
	FP_C2 => { reg => "fpsw", bit => 10 },
	FP_TOP0 => { reg => "fpsw", bit => 11 },
	FP_TOP1 => { reg => "fpsw", bit => 12 },
	FP_TOP2 => { reg => "fpsw", bit => 13 },
	FP_C3 => { reg => "fpsw", bit => 14 },
	FP_B  => { reg => "fpsw", bit => 15 },

	FP_IM => { reg => "fpcw", bit => 0 },
	FP_DM => { reg => "fpcw", bit => 1 },
	FP_ZM => { reg => "fpcw", bit => 2 },
	FP_OM => { reg => "fpcw", bit => 3 },
	FP_UM => { reg => "fpcw", bit => 4 },
	FP_PM => { reg => "fpcw", bit => 5 },
	FP_PC0 => { reg => "fpcw", bit => 8 },
	FP_PC1 => { reg => "fpcw", bit => 9 },
	FP_RC0 => { reg => "fpcw", bit => 10 },
	FP_RC1 => { reg => "fpcw", bit => 11 },
	FP_X  => { reg => "fpcw", bit => 12 }
); # %flags

%cpu = (
	GP     => [ 1, "GP_EAX", "GP_EBX", "GP_ECX", "GP_EDX", "GP_ESI", "GP_EDI", "GP_EBP" ],
	SSE    => [ 1, "SSE_XMM0", "SSE_XMM1", "SSE_XMM2", "SSE_XMM3", "SSE_XMM4", "SSE_XMM5", "SSE_XMM6", "SSE_XMM7" ],
	VFP    => [ 1, "VFP_VF0", "VFP_VF1", "VFP_VF2", "VFP_VF3", "VFP_VF4", "VFP_VF5", "VFP_VF6", "VFP_VF7" ],
	BRANCH => [ 1, "BRANCH1", "BRANCH2" ],
); # %cpu

%vliw = (
	bundle_size       => 1,
	bundels_per_cycle => 1
); # vliw

%emit_templates = (
	S0 => "${arch}_emit_source_register(env, node, 0);",
	S1 => "${arch}_emit_source_register(env, node, 1);",
	S2 => "${arch}_emit_source_register(env, node, 2);",
	S3 => "${arch}_emit_source_register(env, node, 3);",
	S4 => "${arch}_emit_source_register(env, node, 4);",
	S5 => "${arch}_emit_source_register(env, node, 5);",
	SB1 => "${arch}_emit_8bit_source_register(env, node, 1);",
	SB2 => "${arch}_emit_8bit_source_register(env, node, 2);",
	SW0 => "${arch}_emit_16bit_source_register(env, node, 0);",
	SI0 => "${arch}_emit_source_register_or_immediate(env, node, 0);",
	SI1 => "${arch}_emit_source_register_or_immediate(env, node, 1);",
	SI2 => "${arch}_emit_source_register_or_immediate(env, node, 2);",
	SI3 => "${arch}_emit_source_register_or_immediate(env, node, 3);",
	D0 => "${arch}_emit_dest_register(env, node, 0);",
	D1 => "${arch}_emit_dest_register(env, node, 1);",
	D2 => "${arch}_emit_dest_register(env, node, 2);",
	D3 => "${arch}_emit_dest_register(env, node, 3);",
	D4 => "${arch}_emit_dest_register(env, node, 4);",
	D5 => "${arch}_emit_dest_register(env, node, 5);",
	X0 => "${arch}_emit_x87_name(env, node, 0);",
	X1 => "${arch}_emit_x87_name(env, node, 1);",
	X2 => "${arch}_emit_x87_name(env, node, 2);",
	SE => "${arch}_emit_extend_suffix(env, get_ia32_ls_mode(node));",
	ME => "if(get_mode_size_bits(get_ia32_ls_mode(node)) != 32)\n
	           ia32_emit_mode_suffix(env, node);",
	M  => "${arch}_emit_mode_suffix(env, node);",
	XM => "${arch}_emit_x87_mode_suffix(env, node);",
	XXM => "${arch}_emit_xmm_mode_suffix(env, node);",
	XSD => "${arch}_emit_xmm_mode_suffix_s(env, node);",
	AM => "${arch}_emit_am(env, node);",
	unop0 => "${arch}_emit_unop(env, node, 0);",
	unop1 => "${arch}_emit_unop(env, node, 1);",
	unop2 => "${arch}_emit_unop(env, node, 2);",
	unop3 => "${arch}_emit_unop(env, node, 3);",
	unop4 => "${arch}_emit_unop(env, node, 4);",
	DAM0  => "${arch}_emit_am_or_dest_register(env, node, 0);",
	DAM1  => "${arch}_emit_am_or_dest_register(env, node, 0);",
	binop => "${arch}_emit_binop(env, node);",
	x87_binop => "${arch}_emit_x87_binop(env, node);",
);

#--------------------------------------------------#
#                        _                         #
#                       (_)                        #
#  _ __   _____      __  _ _ __    ___  _ __  ___  #
# | '_ \ / _ \ \ /\ / / | | '__|  / _ \| '_ \/ __| #
# | | | |  __/\ V  V /  | | |    | (_) | |_) \__ \ #
# |_| |_|\___| \_/\_/   |_|_|     \___/| .__/|___/ #
#                                      | |         #
#                                      |_|         #
#--------------------------------------------------#

$default_attr_type = "ia32_attr_t";
$default_copy_attr = "ia32_copy_attr";

sub ia32_custom_init_attr {
	my $node = shift;
	my $name = shift;
	my $res = "";
	if(defined($node->{modified_flags})) {
		$res .= "\t/*attr->data.flags |= arch_irn_flags_modify_flags;*/\n";
	}
	if(defined($node->{am})) {
		my $am = $node->{am};
		if($am eq "full,binary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Full, ia32_am_binary);";
		} elsif($am eq "full,unary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Full, ia32_am_unary);";
		} elsif($am eq "source,binary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Source, ia32_am_binary);";
		} elsif($am eq "dest,unary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Dest, ia32_am_unary);";
		} elsif($am eq "dest,binary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Dest, ia32_am_binary);";
		} elsif($am eq "dest,ternary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Dest, ia32_am_ternary);";
		} elsif($am eq "source,ternary") {
			$res .= "\tset_ia32_am_support(res, ia32_am_Source, ia32_am_ternary);";
		} elsif($am eq "none") {
			# nothing to do
		} else {
			die("Invalid address mode '$am' specified on op $name");
		}
	}
	return $res;
}
$custom_init_attr_func = \&ia32_custom_init_attr;

%init_attr = (
	ia32_attr_t     => "\tinit_ia32_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res, latency);",
	ia32_x87_attr_t =>
		"\tinit_ia32_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res, latency);\n".
		"\tinit_ia32_x87_attributes(res);",
	ia32_asm_attr_t =>
		"\tinit_ia32_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res, latency);\n".
		"\tinit_ia32_x87_attributes(res);".
		"\tinit_ia32_asm_attributes(res);",
	ia32_immediate_attr_t =>
		"\tinit_ia32_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res, latency);\n".
		"\tinit_ia32_immediate_attributes(res, symconst, symconst_sign, offset);"
);

%compare_attr = (
	ia32_attr_t           => "ia32_compare_nodes_attr",
	ia32_x87_attr_t       => "ia32_compare_x87_attr",
	ia32_asm_attr_t       => "ia32_compare_asm_attr",
	ia32_immediate_attr_t => "ia32_compare_immediate_attr",
);

%operands = (
);

$mode_xmm     = "mode_E";
$mode_gp      = "mode_Iu";
$mode_fpcw    = "mode_fpcw";
$status_flags = [ "CF", "PF", "AF", "ZF", "SF", "OF" ];
$fpcw_flags   = [ "FP_IM", "FP_DM", "FP_ZM", "FP_OM", "FP_UM", "FP_PM",
                  "FP_PC0", "FP_PC1", "FP_RC0", "FP_RC1", "FP_X" ];

%nodes = (

Immediate => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "gp_NOREG" ] },
	attr      => "ir_entity *symconst, int symconst_sign, long offset",
	attr_type => "ia32_immediate_attr_t",
	latency   => 0,
	mode      => $mode_gp,
},

Asm => {
	mode      => "mode_T",
	arity     => "variable",
	out_arity => "variable",
	attr_type => "ia32_asm_attr_t",
	latency   => 100,
},

ProduceVal => {
	op_flags  => "c",
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
	emit      => "",
	units     => [ ],
	latency   => 0,
	mode      => $mode_gp,
	cmp_attr  => "return 1;",
},

#-----------------------------------------------------------------#
#  _       _                                         _            #
# (_)     | |                                       | |           #
#  _ _ __ | |_ ___  __ _  ___ _ __   _ __   ___   __| | ___  ___  #
# | | '_ \| __/ _ \/ _` |/ _ \ '__| | '_ \ / _ \ / _` |/ _ \/ __| #
# | | | | | ||  __/ (_| |  __/ |    | | | | (_) | (_| |  __/\__ \ #
# |_|_| |_|\__\___|\__, |\___|_|    |_| |_|\___/ \__,_|\___||___/ #
#                   __/ |                                         #
#                  |___/                                          #
#-----------------------------------------------------------------#

# commutative operations

# NOTE:
# All nodes supporting Addressmode have 5 INs:
# 1 - base    r1 == NoReg in case of no AM or no base
# 2 - index   r2 == NoReg in case of no AM or no index
# 3 - op1     r3 == always present
# 4 - op2     r4 == NoReg in case of immediate operation
# 5 - mem     NoMem in case of no AM otherwise it takes the mem from the Load

Add => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	emit      => '. add%M %binop',
	am        => "full,binary",
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AddMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	emit      => ". add%M %SI2, %AM",
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

Adc => {
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. adc%M %binop',
	am        => "full,binary",
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Add64Bit => {
	irn_flags => "R",
	arity     => 4,
	reg_req   => { in => [ "gp", "gp", "gp", "gp" ], out => [ "!in", "!in" ] },
	emit      => '
. movl %S0, %D0
. movl %S1, %D1
. addl %SI2, %D0
. adcl %SI3, %D1
',
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Add => {
	op_flags  => "C",
	irn_flags => "R",
	cmp_attr  => "return 1;",
	arity     => 2,
},

l_Adc => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	arity     => 2,
},

Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	reg_req   => { in => [ "gp", "gp", "eax", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	emit      => '. mul%M %unop3',
	outs      => [ "EAX", "EDX", "M" ],
	ins       => [ "base", "index", "val_high", "val_low", "mem" ],
	am        => "source,binary",
	latency   => 10,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	op_flags  => "C",
	cmp_attr  => "return 1;",
	outs      => [ "EAX", "EDX", "M" ],
	arity     => 2
},

IMul => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	emit      => '. imul%M %binop',
	am        => "source,binary",
	latency   => 5,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

IMul1OP => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "eax", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	emit      => '. imul%M %unop3',
	outs      => [ "EAX", "EDX", "M" ],
	ins       => [ "base", "index", "val_high", "val_low", "mem" ],
	am        => "source,binary",
	latency   => 5,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_IMul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	op_flags  => "C",
	cmp_attr  => "return 1;",
	outs      => [ "EAX", "EDX", "M" ],
	arity     => 2
},

And => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	am        => "full,binary",
	emit      => '. and%M %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

AndMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	emit      => '. and%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

Or => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	am        => "full,binary",
	emit      => '. or%M %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

OrMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	emit      => '. or%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

Xor => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	am        => "full,binary",
	emit      => '. xor%M %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

XorMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	emit      => '. xor%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

l_Xor => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	arity     => 2,
	modified_flags => $status_flags
},

# not commutative operations

Sub => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	am        => "full,binary",
	emit      => '. sub%M %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SubMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	emit      => '. sub%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => 'mode_M',
	modified_flags => $status_flags
},

Sbb => {
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3 !in_r4" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	am        => "full,binary",
	emit      => '. sbb%M %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sub64Bit => {
	irn_flags => "R",
	arity     => 4,
	reg_req   => { in => [ "gp", "gp", "gp", "gp" ], out => [ "!in", "!in" ] },
	emit      => '
. movl %S0, %D0
. movl %S1, %D1
. subl %SI2, %D0
. sbbl %SI3, %D1
',
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Sub => {
	irn_flags => "R",
	cmp_attr  => "return 1;",
	arity     => 2,
},

l_Sbb => {
	cmp_attr  => "return 1;",
	arity     => 2,
},

IDiv => {
	op_flags  => "F|L",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax", "edx", "gp", "none" ],
	               out => [ "eax", "edx", "none" ] },
	ins       => [ "base", "index", "left_low", "left_high", "right", "mem" ],
	outs      => [ "div_res", "mod_res", "M" ],
	attr      => "ia32_op_flavour_t dm_flav",
	am        => "source,ternary",
	init_attr => "attr->data.op_flav = dm_flav;",
	emit      => ". idiv%M %unop4",
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Div => {
	op_flags  => "F|L",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax", "edx", "gp", "none" ],
	               out => [ "eax", "edx", "none" ] },
	ins       => [ "base", "index", "left_low", "left_high", "right", "mem" ],
	outs      => [ "div_res", "mod_res", "M" ],
	attr      => "ia32_op_flavour_t dm_flav",
	am        => "source,ternary",
	init_attr => "attr->data.op_flav = dm_flav;",
	emit      => ". div%M %unop4",
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Shl => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "ecx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "left", "right" ],
	am        => "dest,binary",
	emit      => '. shl %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

ShlMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "count", "mem" ],
	emit      => '. shl%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

l_ShlDep => {
	cmp_attr  => "return 1;",
	# value, cnt, dependency
	arity     => 3
},

ShlD => {
	# FIXME: WHY? the right requirement is in_r3 !in_r5, especially this is the same as in Shl
	#
	# Out requirements is: different from all in
	# This is because, out must be different from LowPart and ShiftCount.
	# We could say "!ecx !in_r4" but it can occur, that all values live through
	# this Shift and the only value dying is the ShiftCount. Then there would be a
	# register missing, as result must not be ecx and all other registers are
	# occupied. What we should write is "!in_r4 !in_r5", but this is not supported
	# (and probably never will). So we create artificial interferences of the result
	# with all inputs, so the spiller can always assure a free register.
	# reg_req   => { in => [ "gp", "gp", "gp", "gp", "ecx", "none" ], out => [ "!in" ] },

	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx" ], out => [ "in_r1 !in_r3" ] },
	ins       => [ "left_high", "left_low", "right" ],
	am        => "dest,ternary",
	emit      => '. shld%M %SB2, %S1, %S0',
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_ShlD => {
	cmp_attr  => "return 1;",
	arity     => 3,
},

Shr => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "ecx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	am        => "dest,binary",
	emit      => '. shr %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

ShrMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "count", "mem" ],
	emit      => '. shr%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

l_ShrDep => {
	cmp_attr  => "return 1;",
	# value, cnt, dependency
	arity     => 3
},

ShrD => {
	# FIXME: WHY? the right requirement is in_r3 !in_r5, especially this is the same as in Shr
	#
	# Out requirements is: different from all in
	# This is because, out must be different from LowPart and ShiftCount.
	# We could say "!ecx !in_r4" but it can occur, that all values live through
	# this Shift and the only value dying is the ShiftCount. Then there would be a
	# register missing, as result must not be ecx and all other registers are
	# occupied. What we should write is "!in_r4 !in_r5", but this is not supported
	# (and probably never will). So we create artificial interferences of the result
	# with all inputs, so the spiller can always assure a free register.
	# reg_req   => { in => [ "gp", "gp", "gp", "gp", "ecx", "none" ], out => [ "!in" ] },

	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx" ], out => [ "in_r1 !in_r3" ] },
	ins       => [ "left_high", "left_low", "right" ],
	am        => "dest,ternary",
	emit      => '. shrd%M %SB2, %S1, %S0',
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_ShrD => {
	cmp_attr  => "return 1;",
	arity     => 3
},

Sar => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "ecx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	am        => "dest,binary",
	emit      => '. sar %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

SarMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "count", "mem" ],
	emit      => '. sar%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

l_Sar => {
	cmp_attr  => "return 1;",
	# value, cnt
	arity     => 2
},

l_SarDep => {
	cmp_attr  => "return 1;",
	# value, cnt, dependency
	arity     => 3
},

Ror => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "ecx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	am        => "dest,binary",
	emit      => '. ror %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RorMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "count", "mem" ],
	emit      => '. ror%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

Rol => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "ecx" ], out => [ "in_r1 !in_r2" ] },
	ins       => [ "val", "count" ],
	am        => "dest,binary",
	emit      => '. rol %SB1, %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

RolMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "ecx", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "count", "mem" ],
	emit      => '. rol%M %SI2, %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

# unary operations

Neg => {
	irn_flags => "R",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	emit      => '. neg %S0',
	ins       => [ "val" ],
	am        => "dest,unary",
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

NegMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. neg%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => $status_flags
},

Minus64Bit => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1", "gp" ] },
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},


l_Neg => {
	cmp_attr  => "return 1;",
	arity     => 1,
},

Inc => {
	irn_flags => "R",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	am        => "dest,unary",
	emit      => '. inc %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

IncMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. inc%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

Dec => {
	irn_flags => "R",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	am        => "dest,unary",
	emit      => '. dec %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

DecMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. dec%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

Not => {
	irn_flags => "R",
	reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
	ins       => [ "val" ],
	am        => "dest,unary",
	emit      => '. not %S0',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => []
},

NotMem => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "mem" ],
	emit      => '. not%M %AM',
	units     => [ "GP" ],
	mode      => "mode_M",
	modified_flags => [],
},

# other operations

CmpJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in  => [ "gp", "gp", "gp", "gp", "none" ],
	               out => [ "none", "none"] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	outs      => [ "false", "true" ],
	attr      => "long pnc",
	am        => "source,binary",
	init_attr => "attr->pn_code = pnc;",
	latency   => 3,
	units     => [ "BRANCH" ],
},

CmpJmp8Bit => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in  => [ "gp", "gp", "eax ebx ecx edx", "eax ebx ecx edx",
	                        "none" ],
	               out => [ "none", "none"] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	outs      => [ "false", "true" ],
	attr      => "long pnc",
	am        => "source,binary",
	init_attr => "attr->pn_code = pnc;",
	latency   => 3,
	units     => [ "BRANCH" ],
},

TestJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in  => [ "gp", "gp", "gp", "gp", "none" ],
	               out => [ "none", "none" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	outs      => [ "false", "true" ],
	attr      => "long pnc",
	am        => "source,binary",
	init_attr => "attr->pn_code = pnc;",
	latency   => 3,
	units     => [ "BRANCH" ],
},

TestJmp8Bit => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in  => [ "gp", "gp", "eax ebx ecx edx", "eax ebx ecx edx",
	                        "none" ],
	               out => [ "none", "none" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	outs      => [ "false", "true" ],
	attr      => "long pnc",
	am        => "source,binary",
	init_attr => "attr->pn_code = pnc;",
	latency   => 3,
	units     => [ "BRANCH" ],
},

SwitchJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	latency   => 3,
	units     => [ "BRANCH" ],
	mode      => "mode_T",
},

IJmp => {
	state     => "pinned",
	op_flags  => "X",
	reg_req   => { in => [ "gp" ] },
	emit      => '. jmp *%S0',
	units     => [ "BRANCH" ],
	mode      => "mode_X",
	modified_flags => []
},

Const => {
	op_flags  => "c",
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
	attr      => "ir_entity *symconst, int symconst_sign, long offset",
	attr_type => "ia32_immediate_attr_t",
	mode      => $mode_gp,
},

Unknown_GP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "gp_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => $mode_gp
},

Unknown_VFP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "vfp_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

Unknown_XMM => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "xmm_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

NoReg_GP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "gp_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => $mode_gp
},

NoReg_VFP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "vfp_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

NoReg_XMM => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "xmm_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

ChangeCW => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "fp_cw" ] },
	mode      => $mode_fpcw,
	latency   => 3,
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FldCW => {
	op_flags  => "L|F",
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "fp_cw" ] },
	ins       => [ "base", "index", "mem" ],
	latency   => 5,
	emit      => ". fldcw %AM",
	mode      => $mode_fpcw,
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FnstCW => {
	op_flags  => "L|F",
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "fp_cw", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "fpcw", "mem" ],
	latency   => 5,
	emit      => ". fnstcw %AM",
	mode      => "mode_M",
	units     => [ "GP" ],
},

Cltd => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	reg_req   => { in => [ "eax", "edx" ], out => [ "edx" ] },
	ins       => [ "val", "globbered" ],
	emit      => '. cltd',
	mode      => $mode_gp,
	units     => [ "GP" ],
},

# Load / Store
#
# Note that we add additional latency values depending on address mode, so a
# lateny of 0 for load is correct

Load => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "M" ],
	latency   => 0,
	emit      => ". mov%SE%ME%.l %AM, %D0",
	units     => [ "GP" ],
},

l_Load => {
	op_flags  => "L|F",
	cmp_attr  => "return 1;",
	outs      => [ "res", "M" ],
	arity     => 2,
},

l_Store => {
	op_flags  => "L|F",
	cmp_attr  => "return 1;",
	state     => "exc_pinned",
	arity     => 3,
	mode      => "mode_M",
},

Store => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	emit      => '. mov%M %SI2, %AM',
	latency   => 2,
	units     => [ "GP" ],
	mode      => "mode_M",
},

Store8Bit => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax ebx ecx edx", "none" ], out => ["none" ] },
	emit      => '. mov%M %SB2, %AM',
	latency   => 2,
	units     => [ "GP" ],
	mode      => "mode_M",
},

Lea => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "base", "index" ],
	emit      => '. leal %AM, %D0',
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [],
},

Push => {
	reg_req   => { in => [ "gp", "gp", "gp", "esp", "none" ], out => [ "esp", "none" ] },
	emit      => '. push%M %unop2',
	ins       => [ "base", "index", "val", "stack", "mem" ],
	outs      => [ "stack:I|S", "M" ],
	am        => "source,binary",
	latency   => 2,
	units     => [ "GP" ],
	modified_flags => [],
},

Pop => {
	reg_req   => { in => [ "gp", "gp", "esp", "none" ], out => [ "esp", "gp", "none" ] },
	emit      => '. pop%M %DAM1',
	outs      => [ "stack:I|S", "res", "M" ],
	ins       => [ "base", "index", "stack", "mem" ],
	am        => "dest,unary",
	latency   => 3, # Pop is more expensive than Push on Athlon
	units     => [ "GP" ],
	modified_flags => [],
},

Enter => {
	reg_req   => { in => [ "esp" ], out => [ "ebp", "esp", "none" ] },
	emit      => '. enter',
	outs      => [ "frame:I", "stack:I|S", "M" ],
	latency   => 15,
	units     => [ "GP" ],
},

Leave => {
	reg_req   => { in => [ "esp", "ebp" ], out => [ "ebp", "esp" ] },
	emit      => '. leave',
	outs      => [ "frame:I", "stack:I|S" ],
	latency   => 3,
	units     => [ "GP" ],
},

AddSP => {
	irn_flags => "I",
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "esp", "gp", "none" ], out => [ "in_r3", "none" ] },
	am        => "source,binary",
	emit      => '. addl %binop',
	outs      => [ "stack:S", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

SubSP => {
#irn_flags => "I",
	state     => "pinned",
	reg_req   => { in => [ "gp", "gp", "esp", "gp", "none" ], out => [ "in_r3", "gp", "none" ] },
	am        => "source,binary",
	emit      => ". subl %binop\n".
	             ". movl %%esp, %D1",
	outs      => [ "stack:I|S", "addr", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

LdTls => {
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
},

# the int instruction
int => {
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	mode      => "mode_M",
	emit      => '. int %SI0',
	units     => [ "GP" ],
	cmp_attr  => "return 1;",
},


#-----------------------------------------------------------------------------#
#   _____ _____ ______    __ _             _                     _            #
#  / ____/ ____|  ____|  / _| |           | |                   | |           #
# | (___| (___ | |__    | |_| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
#  \___ \\___ \|  __|   |  _| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  ____) |___) | |____  | | | | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
# |_____/_____/|______| |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#-----------------------------------------------------------------------------#

xZero => {
	irn_flags => "R",
	reg_req   => { out => [ "xmm" ] },
	emit      => '. xorp%XSD %D1, %D1',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

# commutative operations

xAdd => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. add%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMul => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. mul%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMax => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. max%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMin => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. min%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xAnd => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. andp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xOr => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. orp%XSD %binop',
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xXor => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. xorp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

# not commutative operations

xAndNot => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. andnp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xSub => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. sub%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xDiv => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4", "none" ] },
	outs      => [ "res", "M" ],
	emit      => '. div%XXM %binop',
	latency   => 16,
	units     => [ "SSE" ],
},

# other operations

xCmp => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4" ] },
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xCmpJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "none", "none" ] },
	ins       => [ "base", "index", "left", "right", "mem" ],
	outs      => [ "false", "true" ],
	attr      => "long pnc",
	init_attr => "attr->pn_code = pnc;",
	latency   => 5,
	units     => [ "SSE" ],
},

# Load / Store

xLoad => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	emit      => '. mov%XXM %AM, %D0',
	attr      => "ir_mode *load_mode",
	init_attr => "attr->ls_mode = load_mode;",
	outs      => [ "res", "M" ],
	latency   => 0,
	units     => [ "SSE" ],
},

xStore => {
	op_flags => "L|F",
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ] },
	emit     => '. mov%XXM %S2, %AM',
	latency  => 0,
	units    => [ "SSE" ],
	mode     => "mode_M",
},

xStoreSimple => {
	op_flags => "L|F",
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ] },
	ins      => [ "base", "index", "val", "mem" ],
	emit     => '. mov%XXM %S2, %AM',
	latency  => 0,
	units    => [ "SSE" ],
	mode     => "mode_M",
},

CvtSI2SS => {
	op_flags => "L|F",
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm" ] },
	emit     => '. cvtsi2ss %D0, %AM',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => $mode_xmm
},

CvtSI2SD => {
	op_flags => "L|F",
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm" ] },
	emit     => '. cvtsi2sd %unop2',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => $mode_xmm
},


l_X87toSSE => {
	op_flags => "L|F",
	cmp_attr => "return 1;",
	arity    => 3,
},

l_SSEtoX87 => {
	op_flags => "L|F",
	cmp_attr => "return 1;",
	arity    => 3,
},

# CopyB

CopyB => {
	op_flags => "F|H",
	state    => "pinned",
	reg_req  => { in => [ "edi", "esi", "ecx", "none" ], out => [ "edi", "esi", "ecx", "none" ] },
	outs     => [ "DST", "SRC", "CNT", "M" ],
	units    => [ "GP" ],
	modified_flags => [ "DF" ]
},

CopyB_i => {
	op_flags => "F|H",
	state    => "pinned",
	reg_req  => { in => [ "edi", "esi", "none" ], out => [  "edi", "esi", "none" ] },
	outs     => [ "DST", "SRC", "M" ],
	units    => [ "GP" ],
	modified_flags => [ "DF" ]
},

# Conversions

Conv_I2I => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3", "none" ] },
	units     => [ "GP" ],
	ins       => [ "base", "index", "val", "mem" ],
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Conv_I2I8Bit => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax ebx ecx edx", "none" ], out => [ "in_r3", "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	units     => [ "GP" ],
	attr      => "ir_mode *smaller_mode",
	init_attr => "attr->ls_mode = smaller_mode;",
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Conv_I2FP => {
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	latency  => 10,
	units    => [ "SSE" ],
	mode     => "mode_E",
},

Conv_FP2I => {
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ], out => [ "gp", "none" ] },
	latency  => 10,
	units    => [ "SSE" ],
	mode     => $mode_gp,
},

Conv_FP2FP => {
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ], out => [ "xmm", "none" ] },
	latency  => 8,
	units    => [ "SSE" ],
	mode     => "mode_E",
},

CmpCMov => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none", "gp", "gp" ], out => [ "in_r7" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem", "val_true", "val_false" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

CmpCMov8Bit => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none", "gp", "gp" ], out => [ "in_r7" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem", "val_true", "val_false" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

TestCMov => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r7" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem", "val_true",
	               "val_false" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

TestCMov8Bit => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none", "gp", "gp" ],
	               out => [ "in_r7" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem", "val_true",
	               "val_false" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

xCmpCMov => {
	irn_flags => "R",
	reg_req   => { in => [ "xmm", "xmm", "gp", "gp" ], out => [ "in_r4" ] },
	latency   => 5,
	units     => [ "SSE" ],
	mode      => $mode_gp,
},

vfCmpCMov => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "gp", "gp" ],
	               out => [ "in_r7" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem", "val_true",
	               "val_false" ],
	latency   => 10,
	units     => [ "VFP", "GP" ],
	mode      => $mode_gp,
	attr_type => "ia32_x87_attr_t",
},

CmpSet => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ],
	               out => [ "eax ebx ecx edx" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

CmpSet8Bit => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "eax ebx ecx edx", "eax ebx ecx edx",
	                       "none" ],
	               out => [ "eax ebx ecx edx" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

TestSet => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ],
	               out => [ "eax ebx ecx edx" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

TestSet8Bit => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "eax ebx ecx edx", "eax ebx ecx edx",
	                       "none" ],
	               out => [ "eax ebx ecx edx" ] },
	ins       => [ "base", "index", "cmp_left", "cmp_right", "mem" ],
	attr      => "pn_Cmp pn_code",
	init_attr => "attr->pn_code = pn_code;",
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

xCmpSet => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "eax ebx ecx edx" ] },
	latency   => 5,
	units     => [ "SSE" ],
	mode      => $mode_gp,
},

vfCmpSet => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "eax ebx ecx edx" ] },
	latency   => 10,
	units     => [ "VFP" ],
	mode      => $mode_gp,
	attr_type => "ia32_x87_attr_t",
},

vfCMov => {
	irn_flags => "R",
	reg_req   => { in => [ "vfp", "vfp", "vfp", "vfp" ], out => [ "vfp" ] },
	latency   => 10,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

#----------------------------------------------------------#
#        _      _               _    __ _             _    #
#       (_)    | |             | |  / _| |           | |   #
# __   ___ _ __| |_ _   _  __ _| | | |_| | ___   __ _| |_  #
# \ \ / / | '__| __| | | |/ _` | | |  _| |/ _ \ / _` | __| #
#  \ V /| | |  | |_| |_| | (_| | | | | | | (_) | (_| | |_  #
#   \_/ |_|_|   \__|\__,_|\__,_|_| |_| |_|\___/ \__,_|\__| #
#                 | |                                      #
#  _ __   ___   __| | ___  ___                             #
# | '_ \ / _ \ / _` |/ _ \/ __|                            #
# | | | | (_) | (_| |  __/\__ \                            #
# |_| |_|\___/ \__,_|\___||___/                            #
#----------------------------------------------------------#

vfadd => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "fpcw" ], out => [ "vfp" ] },
	ins       => [ "base", "index", "left", "right", "mem", "fpcw" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfmul => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "fpcw" ], out => [ "vfp" ] },
	ins       => [ "base", "index", "left", "right", "mem", "fpcw" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

l_vfmul => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	arity     => 2,
},

vfsub => {
	irn_flags => "R",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "fpcw" ], out => [ "vfp" ] },
	ins       => [ "base", "index", "left", "right", "mem", "fpcw" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

l_vfsub => {
	cmp_attr  => "return 1;",
	arity     => 2,
},

vfdiv => {
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "fpcw" ], out => [ "vfp", "none" ] },
	ins       => [ "base", "index", "left", "right", "mem", "fpcw" ],
	outs      => [ "res", "M" ],
	latency   => 20,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

l_vfdiv => {
	cmp_attr  => "return 1;",
	outs      => [ "res", "M" ],
	arity     => 2,
},

vfprem => {
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none", "fpcw" ], out => [ "vfp" ] },
	ins       => [ "base", "index", "left", "right", "mem", "fpcw" ],
	latency   => 20,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

l_vfprem => {
	cmp_attr  => "return 1;",
	arity     => 2,
},

vfabs => {
	irn_flags => "R",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	ins       => [ "value" ],
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfchs => {
	irn_flags => "R",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	ins       => [ "value" ],
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

# virtual Load and Store

vfld => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "vfp", "none" ] },
	ins       => [ "base", "index", "mem" ],
	outs      => [ "res", "M" ],
	attr      => "ir_mode *load_mode",
	init_attr => "attr->attr.ls_mode = load_mode;",
	latency   => 2,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

vfst => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "vfp", "none" ] },
	ins       => [ "base", "index", "val", "mem" ],
	attr      => "ir_mode *store_mode",
	init_attr => "attr->attr.ls_mode = store_mode;",
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

# Conversions

vfild => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "vfp", "none" ] },
	outs      => [ "res", "M" ],
	ins       => [ "base", "index", "mem" ],
	latency   => 4,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

l_vfild => {
	cmp_attr  => "return 1;",
	outs      => [ "res", "M" ],
	arity     => 2,
},

vfist => {
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "vfp", "fpcw", "none" ] },
	ins       => [ "base", "index", "val", "fpcw", "mem" ],
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

l_vfist => {
	cmp_attr  => "return 1;",
	state     => "exc_pinned",
	arity     => 3,
	mode      => "mode_M",
},


# constants

vfldz => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfld1 => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldpi => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldln2 => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldlg2 => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldl2t => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

vfldl2e => {
	irn_flags => "R",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
	attr_type => "ia32_x87_attr_t",
},

# other

vfCmpJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	reg_req   => { in => [ "vfp", "vfp" ], out => [ "none", "none", "eax" ] },
	ins       => [ "left", "right" ],
	outs      => [ "false", "true", "temp_reg_eax" ],
	attr      => "long pnc",
	init_attr => "attr->attr.pn_code = pnc;",
	latency   => 10,
	units     => [ "VFP" ],
	attr_type => "ia32_x87_attr_t",
},

#------------------------------------------------------------------------#
#       ___ _____    __ _             _                     _            #
# __  _( _ )___  |  / _| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
# \ \/ / _ \  / /  | |_| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  >  < (_) |/ /   |  _| | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
# /_/\_\___//_/    |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#------------------------------------------------------------------------#

# Note: gas is strangely buggy: fdivrp and fdivp as well as fsubrp and fsubp
#       are swapped, we work this around in the emitter...

fadd => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fadd%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

faddp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. faddp%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fmul => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fmul%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fmulp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fmulp%XM %x87_binop',,
	attr_type => "ia32_x87_attr_t",
},

fsub => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fsub%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fsubp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fsubrp%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fsubr => {
	op_flags  => "R",
	rd_constructor => "NONE",
	irn_flags => "R",
	reg_req   => { },
	emit      => '. fsubr%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fsubrp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	irn_flags => "R",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fsubp%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fprem => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fprem1',
	attr_type => "ia32_x87_attr_t",
},

# this node is just here, to keep the simulator running
# we can omit this when a fprem simulation function exists
fpremp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fprem1',
	attr_type => "ia32_x87_attr_t",
},

fdiv => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fdiv%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fdivp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fdivrp%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fdivr => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fdivr%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fdivrp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fdivp%XM %x87_binop',
	attr_type => "ia32_x87_attr_t",
},

fabs => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fabs',
	attr_type => "ia32_x87_attr_t",
},

fchs => {
	op_flags  => "R|K",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fchs',
	attr_type => "ia32_x87_attr_t",
},

# x87 Load and Store

fld => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	reg_req   => { },
	emit      => '. fld%XM %AM',
	attr_type => "ia32_x87_attr_t",
},

fst => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	reg_req   => { },
	emit      => '. fst%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

fstp => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	reg_req   => { },
	emit      => '. fstp%XM %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

# Conversions

fild => {
	op_flags  => "R",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fild%M %AM',
	attr_type => "ia32_x87_attr_t",
},

fist => {
	op_flags  => "R",
	state     => "exc_pinned",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fist%M %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

fistp => {
	op_flags  => "R",
	state     => "exc_pinned",
	rd_constructor => "NONE",
	reg_req   => { },
	emit      => '. fistp%M %AM',
	mode      => "mode_M",
	attr_type => "ia32_x87_attr_t",
},

# constants

fldz => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldz',
	attr_type => "ia32_x87_attr_t",
},

fld1 => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fld1',
	attr_type => "ia32_x87_attr_t",
},

fldpi => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldpi',
	attr_type => "ia32_x87_attr_t",
},

fldln2 => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldln2',
	attr_type => "ia32_x87_attr_t",
},

fldlg2 => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldlg2',
	attr_type => "ia32_x87_attr_t",
},

fldl2t => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldll2t',
	attr_type => "ia32_x87_attr_t",
},

fldl2e => {
	op_flags  => "R|c|K",
	irn_flags  => "R",
	reg_req   => { },
	emit      => '. fldl2e',
	attr_type => "ia32_x87_attr_t",
},

# fxch, fpush, fpop
# Note that it is NEVER allowed to do CSE on these nodes
# Moreover, note the virtual register requierements!

fxch => {
	op_flags  => "R|K",
	reg_req   => { },
	cmp_attr  => "return 1;",
	emit      => '. fxch %X0',
	attr_type => "ia32_x87_attr_t",
},

fpush => {
	op_flags  => "R|K",
	reg_req   => {},
	cmp_attr  => "return 1;",
	emit      => '. fld %X0',
	attr_type => "ia32_x87_attr_t",
},

fpushCopy => {
	op_flags  => "R",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	cmp_attr  => "return 1;",
	emit      => '. fld %X0',
	attr_type => "ia32_x87_attr_t",
},

fpop => {
	op_flags  => "R|K",
	reg_req   => { },
	cmp_attr  => "return 1;",
	emit      => '. fstp %X0',
	attr_type => "ia32_x87_attr_t",
},

# compare

fcomJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},

fcompJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},

fcomppJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},

fcomrJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},

fcomrpJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},

fcomrppJmp => {
	op_flags  => "L|X|Y",
	reg_req   => { },
	attr_type => "ia32_x87_attr_t",
},


# -------------------------------------------------------------------------------- #
#  ____ ____  _____                  _                               _             #
# / ___/ ___|| ____| __   _____  ___| |_ ___  _ __   _ __   ___   __| | ___  ___   #
# \___ \___ \|  _|   \ \ / / _ \/ __| __/ _ \| '__| | '_ \ / _ \ / _` |/ _ \/ __|  #
#  ___) |__) | |___   \ V /  __/ (__| || (_) | |    | | | | (_) | (_| |  __/\__ \  #
# |____/____/|_____|   \_/ \___|\___|\__\___/|_|    |_| |_|\___/ \__,_|\___||___/  #
#                                                                                  #
# -------------------------------------------------------------------------------- #


# Spilling and reloading of SSE registers, hardcoded, not generated #

xxLoad => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	emit      => '. movdqu %D0, %AM',
	outs      => [ "res", "M" ],
	units     => [ "SSE" ],
},

xxStore => {
	op_flags => "L|F",
	state    => "exc_pinned",
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ] },
	emit     => '. movdqu %binop',
	units    => [ "SSE" ],
	mode     => "mode_M",
},

); # end of %nodes

# Include the generated SIMD node specification written by the SIMD optimization
$my_script_name = dirname($myname) . "/../ia32/ia32_simd_spec.pl";
unless ($return = do $my_script_name) {
	warn "couldn't parse $my_script_name: $@" if $@;
	warn "couldn't do $my_script_name: $!"    unless defined $return;
	warn "couldn't run $my_script_name"       unless $return;
}
