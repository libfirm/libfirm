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
		{ mode => "mode_Hu" }
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
	S1 => "${arch}_emit_source_register(env, node, 0);",
	S2 => "${arch}_emit_source_register(env, node, 1);",
	S3 => "${arch}_emit_source_register(env, node, 2);",
	S4 => "${arch}_emit_source_register(env, node, 3);",
	S5 => "${arch}_emit_source_register(env, node, 4);",
	S6 => "${arch}_emit_source_register(env, node, 5);",
	D1 => "${arch}_emit_dest_register(env, node, 0);",
	D2 => "${arch}_emit_dest_register(env, node, 1);",
	D3 => "${arch}_emit_dest_register(env, node, 2);",
	D4 => "${arch}_emit_dest_register(env, node, 3);",
	D5 => "${arch}_emit_dest_register(env, node, 4);",
	D6 => "${arch}_emit_dest_register(env, node, 5);",
	A1 => "${arch}_emit_in_node_name(env, node, 0);",
	A2 => "${arch}_emit_in_node_name(env, node, 1);",
	A3 => "${arch}_emit_in_node_name(env, node, 2);",
	A4 => "${arch}_emit_in_node_name(env, node, 3);",
	A5 => "${arch}_emit_in_node_name(env, node, 4);",
	A6 => "${arch}_emit_in_node_name(env, node, 5);",
	X1 => "${arch}_emit_x87_name(env, node, 0);",
	X2 => "${arch}_emit_x87_name(env, node, 1);",
	X3 => "${arch}_emit_x87_name(env, node, 2);",
	C  => "${arch}_emit_immediate(env, node);",
	SE => "${arch}_emit_extend_suffix(env, get_ia32_ls_mode(node));",
	ME => "if(get_mode_size_bits(get_ia32_ls_mode(node)) != 32)\n
	           ia32_emit_mode_suffix(env, get_ia32_ls_mode(node));",
	M  => "${arch}_emit_mode_suffix(env, get_ia32_ls_mode(node));",
	XM => "${arch}_emit_x87_mode_suffix(env, node);",
	XXM => "${arch}_emit_xmm_mode_suffix(env, node);",
	XSD => "${arch}_emit_xmm_mode_suffix_s(env, node);",
	AM => "${arch}_emit_am(env, node);",
	unop => "${arch}_emit_unop(env, node);",
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

$default_cmp_attr = "return ia32_compare_attr(attr_a, attr_b);";

%operands = (
);

$mode_xmm     = "mode_LLu";
$mode_gp      = "mode_Iu";
$status_flags = [ "CF", "PF", "AF", "ZF", "SF", "OF" ];
$fpcw_flags   = [ "FP_IM", "FP_DM", "FP_ZM", "FP_OM", "FP_UM", "FP_PM",
                  "FP_PC0", "FP_PC1", "FP_RC0", "FP_RC1", "FP_X" ];

%nodes = (

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
	comment   => "construct Add: Add(a, b) = Add(b, a) = a + b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. addl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Adc => {
	comment   => "construct Add with Carry: Adc(a, b) = Add(b, a) = a + b + carry",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. adcl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Add64Bit => {
	irn_flags => "R",
	comment   => "construct 64Bit Add: Add(a_l, a_h, b_l, b_h) = a_l + b_l; a_h + b_h + carry",
	arity     => 4,
	reg_req   => { in => [ "gp", "gp", "gp", "gp" ], out => [ "!in", "!in" ] },
	emit      => '
. movl %S1, %D1
. movl %S2, %D2
. addl %S3, %D1
. adcl %S4, %D2
',
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Add => {
	op_flags  => "C",
	irn_flags => "R",
	cmp_attr  => "return 1;",
	comment   => "construct lowered Add: Add(a, b) = Add(b, a) = a + b",
	arity     => 2,
},

l_Adc => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	comment   => "construct lowered Add with Carry: Adc(a, b) = Adc(b, a) = a + b + carry",
	arity     => 2,
},

Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	comment   => "construct MulS: MulS(a, b) = MulS(b, a) = a * b",
	reg_req   => { in => [ "gp", "gp", "eax", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	emit      => '. mull %unop',
	outs      => [ "EAX", "EDX", "M" ],
	latency   => 10,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Mul => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	op_flags  => "C",
	cmp_attr  => "return 1;",
	comment   => "construct lowered MulS: Mul(a, b) = Mul(b, a) = a * b",
	outs      => [ "EAX", "EDX", "M" ],
	arity     => 2
},

IMul => {
	irn_flags => "R",
	comment   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. imull %binop',
	latency   => 5,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

IMul1OP => {
	irn_flags => "R",
	comment   => "construct Mul (1 operand format): Mul(a, b) = Mul(b, a) = a * b",
	reg_req   => { in => [ "gp", "gp", "eax", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	emit      => '. imull %unop',
	outs      => [ "EAX", "EDX", "M" ],
	latency   => 5,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_IMul => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	comment   => "construct lowered IMul: IMul(a, b) = IMul(b, a) = a * b",
	arity     => 2
},

And => {
	irn_flags => "R",
	comment   => "construct And: And(a, b) = And(b, a) = a AND b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. andl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Or => {
	irn_flags => "R",
	comment   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. orl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Xor => {
	irn_flags => "R",
	comment   => "construct Xor: Xor(a, b) = Xor(b, a) = a EOR b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. xorl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Xor => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	comment   => "construct lowered Xor: Xor(a, b) = Xor(b, a) = a XOR b",
	arity     => 2,
	modified_flags => $status_flags
},

# not commutative operations

Sub => {
	irn_flags => "R",
	comment   => "construct Sub: Sub(a, b) = a - b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. subl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sbb => {
	comment   => "construct Sub with Carry: SubC(a, b) = a - b - carry",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. sbbl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Sub64Bit => {
	irn_flags => "R",
	comment   => "construct 64Bit Sub: Sub(a_l, a_h, b_l, b_h) = a_l - b_l; a_h - b_h - borrow",
	arity     => 4,
	reg_req   => { in => [ "gp", "gp", "gp", "gp" ], out => [ "!in", "!in" ] },
	emit      => '
. movl %S1, %D1
. movl %S2, %D2
. subl %S3, %D1
. sbbl %S4, %D2
',
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

l_Sub => {
	irn_flags => "R",
	cmp_attr  => "return 1;",
	comment   => "construct lowered Sub: Sub(a, b) = a - b",
	arity     => 2,
},

l_Sbb => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered Sub with Carry: SubC(a, b) = a - b - carry",
	arity     => 2,
},

IDiv => {
	op_flags  => "F|L",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax", "edx", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	attr      => "ia32_op_flavour_t dm_flav",
	init_attr => "attr->data.op_flav = dm_flav;",
	emit      => ". idivl %unop",
	outs      => [ "div_res", "mod_res", "M" ],
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Div => {
	op_flags  => "F|L",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "eax", "edx", "gp", "none" ], out => [ "eax", "edx", "none" ] },
	attr      => "ia32_op_flavour_t dm_flav",
	init_attr => "attr->data.op_flav = dm_flav;",
	emit      => ". divl %unop",
	outs      => [ "div_res", "mod_res", "M" ],
	latency   => 25,
	units     => [ "GP" ],
	modified_flags => $status_flags
},

Shl => {
	irn_flags => "R",
	comment   => "construct Shl: Shl(a, b) = a << b",
	reg_req   => { in => [ "gp", "gp", "gp", "ecx", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. shll %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Shl => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered Shl: Shl(a, b) = a << b",
	arity     => 2
},

ShlD => {
	irn_flags => "R",
	comment   => "construct ShlD: ShlD(a, b, c) = a, b << count (shift left count bits from b into a)",
	# Out requirements is: different from all in
	# This is because, out must be different from LowPart and ShiftCount.
	# We could say "!ecx !in_r4" but it can occur, that all values live through
	# this Shift and the only value dying is the ShiftCount. Then there would be a
	# register missing, as result must not be ecx and all other registers are
	# occupied. What we should write is "!in_r4 !in_r5", but this is not supported
	# (and probably never will). So we create artificial interferences of the result
	# with all inputs, so the spiller can always assure a free register.
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "ecx", "none" ], out => [ "!in" ] },
	emit      =>
'
if (get_ia32_immop_type(node) == ia32_ImmNone) {
	if (get_ia32_op_type(node) == ia32_AddrModeD) {
		. shldl %%cl, %S4, %AM
	} else {
		. shldl %%cl, %S4, %S3
	}
} else {
	if (get_ia32_op_type(node) == ia32_AddrModeD) {
		. shldl $%C, %S4, %AM
	} else {
		. shldl $%C, %S4, %S3
	}
}
',
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_ShlD => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered ShlD: ShlD(a, b, c) = a, b << count (shift left count bits from b into a)",
	arity     => 3,
},

Shr => {
	irn_flags => "R",
	comment   => "construct Shr: Shr(a, b) = a >> b",
	reg_req   => { in => [ "gp", "gp", "gp", "ecx", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. shrl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Shr => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered Shr: Shr(a, b) = a << b",
	arity     => 2
},

ShrD => {
	irn_flags => "R",
	comment   => "construct ShrD: ShrD(a, b, c) = a, b >> count (shift rigth count bits from a into b)",
	# Out requirements is: different from all in
	# This is because, out must be different from LowPart and ShiftCount.
	# We could say "!ecx !in_r4" but it can occur, that all values live through
	# this Shift and the only value dying is the ShiftCount. Then there would be a
	# register missing, as result must not be ecx and all other registers are
	# occupied. What we should write is "!in_r4 !in_r5", but this is not supported
	# (and probably never will). So we create artificial interferences of the result
	# with all inputs, so the spiller can always assure a free register.
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "ecx", "none" ], out => [ "!in" ] },
	emit      => '
if (get_ia32_immop_type(node) == ia32_ImmNone) {
	if (get_ia32_op_type(node) == ia32_AddrModeD) {
		. shrdl %%cl, %S4, %AM
	} else {
		. shrdl %%cl, %S4, %S3
	}
} else {
	if (get_ia32_op_type(node) == ia32_AddrModeD) {
		. shrdl $%C, %S4, %AM
	} else {
		. shrdl $%C, %S4, %S3
	}
}
',
	latency   => 6,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_ShrD => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered ShrD: ShrD(a, b, c) = a, b >> count (shift rigth count bits from a into b)",
	arity     => 3
},

Sar => {
	irn_flags => "R",
	comment   => "construct Shrs: Shrs(a, b) = a >> b",
	reg_req   => { in => [ "gp", "gp", "gp", "ecx", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. sarl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

l_Sar => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered Sar: Sar(a, b) = a << b",
	arity     => 2
},

Ror => {
	irn_flags => "R",
	comment   => "construct Ror: Ror(a, b) = a ROR b",
	reg_req   => { in => [ "gp", "gp", "gp", "ecx", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. rorl %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Rol => {
	irn_flags => "R",
	comment   => "construct Rol: Rol(a, b) = a ROL b",
	reg_req   => { in => [ "gp", "gp", "gp", "ecx", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. roll %binop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

# unary operations

Neg => {
	irn_flags => "R",
	comment   => "construct Minus: Minus(a) = -a",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. negl %unop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => $status_flags
},

Minus64Bit => {
	irn_flags => "R",
	comment   => "construct 64Bit Minus: Minus(a_l, a_h, 0) = 0 - a_l; 0 - a_h - borrow",
	arity     => 4,
	reg_req   => { in => [ "gp", "gp", "gp" ], out => [ "!in", "!in" ] },
	emit      => '
. movl %S1, %D1
. movl %S1, %D2
. subl %S2, %D1
. sbbl %S3, %D2
',
	outs      => [ "low_res", "high_res" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},


l_Neg => {
	cmp_attr  => "return 1;",
	comment   => "construct lowered Minus: Minus(a) = -a",
	arity     => 1,
},

Inc => {
	irn_flags => "R",
	comment   => "construct Increment: Inc(a) = a++",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. incl %unop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

Dec => {
	irn_flags => "R",
	comment   => "construct Decrement: Dec(a) = a--",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. decl %unop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [ "OF", "SF", "ZF", "AF", "PF" ]
},

Not => {
	irn_flags => "R",
	comment   => "construct Not: Not(a) = !a",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3" ] },
	emit      => '. notl %unop',
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => []
},

# other operations

CondJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct conditional jump: CMP A, B && JMPxx LABEL",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ] },
	outs      => [ "false", "true" ],
	latency   => 3,
	units     => [ "BRANCH" ],
},

TestJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct conditional jump: TEST A, B && JMPxx LABEL",
	reg_req  => { in => [ "gp", "gp" ] },
	outs      => [ "false", "true" ],
	latency   => 3,
	units     => [ "BRANCH" ],
},

CJmpAM => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct conditional jump without CMP (replaces CondJmp): JMPxx LABEL",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "none", "none" ] },
	outs      => [ "false", "true" ],
	units     => [ "BRANCH" ],
},

CJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct conditional jump without CMP (replaces TestJmp): JMPxx LABEL",
	reg_req   => { in => [ "gp", "gp" ] },
	units     => [ "BRANCH" ],
},

SwitchJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct switch",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	latency   => 3,
	units     => [ "BRANCH" ],
},

Const => {
	op_flags  => "c",
	irn_flags => "R",
	comment   => "represents an integer constant",
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
	mode      => $mode_gp,
},

Unknown_GP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "unknown value",
	reg_req   => { out => [ "gp_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => $mode_gp
},

Unknown_VFP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "unknown value",
	reg_req   => { out => [ "vfp_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

Unknown_XMM => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "unknown value",
	reg_req   => { out => [ "xmm_UKNWN" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

NoReg_GP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "noreg GP value",
	reg_req   => { out => [ "gp_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => $mode_gp
},

NoReg_VFP => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "noreg VFP value",
	reg_req   => { out => [ "vfp_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

NoReg_XMM => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "noreg XMM value",
	reg_req   => { out => [ "xmm_NOREG" ] },
	units     => [],
	emit      => "",
	mode      => "mode_E"
},

ChangeCW => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	comment   => "change floating point control word",
	reg_req   => { out => [ "fp_cw" ] },
	mode      => "mode_Hu",
	latency   => 3,
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FldCW => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "load floating point control word FldCW(ptr, mem) = LD ptr -> reg",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "fp_cw" ] },
	latency   => 5,
	emit      => ". fldcw %AM",
	mode      => "mode_Hu",
	units     => [ "GP" ],
	modified_flags => $fpcw_flags
},

FnstCW => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "store floating point control word: FstCW(ptr, mem) = ST ptr -> reg",
	reg_req   => { in => [ "gp", "gp", "fp_cw", "none" ], out => [ "none" ] },
	latency   => 5,
	emit      => ". fnstcw %AM",
	mode      => "mode_M",
	units     => [ "GP" ],
},

Cltd => {
	# we should not rematrialize this node. It produces 2 results and has
	# very strict constrains
	comment   => "construct CDQ: sign extend EAX -> EDX:EAX",
	reg_req   => { in => [ "gp" ], out => [ "eax in_r1", "edx" ] },
	emit      => '. cltd',
	outs      => [ "EAX", "EDX" ],
	units     => [ "GP" ],
},

# Load / Store

Load => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "gp", "none" ] },
	latency   => 3,
	emit      => ". mov%SE%ME%.l %AM, %D1",
	outs      => [ "res", "M" ],
	units     => [ "GP" ],
},

l_Load => {
	op_flags  => "L|F",
	cmp_attr  => "return 1;",
	comment   => "construct lowered Load: Load(ptr, mem) = LD ptr -> reg",
	outs      => [ "res", "M" ],
	arity     => 2,
},

l_Store => {
	op_flags  => "L|F",
	cmp_attr  => "return 1;",
	state     => "exc_pinned",
	comment   => "construct lowered Store: Store(ptr, val, mem) = ST ptr,val",
	arity     => 3,
	mode      => "mode_M",
},

Store => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req   => { in => [ "gp", "gp", "gp", "none" ], out => [ "none" ] },
	emit      => '. mov%M %binop',
	latency   => 3,
	units     => [ "GP" ],
	mode      => "mode_M",
},

Store8Bit => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "construct 8Bit Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req   => { in => [ "gp", "gp", "eax ebx ecx edx", "none" ], out => ["none" ] },
	emit      => '. mov%M %binop',
	latency   => 3,
	units     => [ "GP" ],
	mode      => "mode_M",
},

Lea => {
	irn_flags => "R",
	comment   => "construct Lea: Lea(a,b) = lea [a+b*const+offs] | res = a + b * const + offs with const = 0,1,2,4,8",
	reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1" ] },
	emit      => '. leal %AM, %D1',
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
	modified_flags => [],
},

Push => {
	comment   => "push on the stack",
	reg_req   => { in => [ "gp", "gp", "gp", "esp", "none" ], out => [ "esp", "none" ] },
	emit      => '. pushl %unop',
	outs      => [ "stack:I|S", "M" ],
	latency   => 3,
	units     => [ "GP" ],
	modified_flags => [],
},

Pop => {
	comment   => "pop a gp register from the stack",
	reg_req   => { in => [ "gp", "gp", "esp", "none" ], out => [ "esp", "gp", "none" ] },
	emit      => '. popl %unop',
	outs      => [ "stack:I|S", "res", "M" ],
	latency   => 4,
	units     => [ "GP" ],
	modified_flags => [],
},

Enter => {
	comment   => "create stack frame",
	reg_req   => { in => [ "esp" ], out => [ "ebp", "esp" ] },
	emit      => '. enter',
	outs      => [ "frame:I", "stack:I|S", "M" ],
	latency   => 15,
	units     => [ "GP" ],
},

Leave => {
	comment   => "destroy stack frame",
	reg_req   => { in => [ "esp", "ebp" ], out => [ "ebp", "esp" ] },
	emit      => '. leave',
	outs      => [ "frame:I", "stack:I|S" ],
	latency   => 3,
	units     => [ "GP" ],
},

AddSP => {
	irn_flags => "I",
	comment   => "allocate space on stack",
	reg_req   => { in => [ "gp", "gp", "esp", "gp", "none" ], out => [ "in_r3", "none" ] },
	emit      => '. addl %binop',
	outs      => [ "stack:S", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

SubSP => {
	irn_flags => "I",
	comment   => "free space on stack",
	reg_req   => { in => [ "gp", "gp", "esp", "gp", "none" ], out => [ "in_r3", "none" ] },
	emit      => '. subl %binop',
	outs      => [ "stack:S", "M" ],
	units     => [ "GP" ],
	modified_flags => $status_flags
},

LdTls => {
	irn_flags => "R",
	comment   => "get the TLS base address",
	reg_req   => { out => [ "gp" ] },
	units     => [ "GP" ],
},



#-----------------------------------------------------------------------------#
#   _____ _____ ______    __ _             _                     _            #
#  / ____/ ____|  ____|  / _| |           | |                   | |           #
# | (___| (___ | |__    | |_| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
#  \___ \\___ \|  __|   |  _| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  ____) |___) | |____  | | | | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
# |_____/_____/|______| |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#-----------------------------------------------------------------------------#

# commutative operations

xAdd => {
	irn_flags => "R",
	comment   => "construct SSE Add: Add(a, b) = Add(b, a) = a + b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. add%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMul => {
	irn_flags => "R",
	comment   => "construct SSE Mul: Mul(a, b) = Mul(b, a) = a * b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. mul%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMax => {
	irn_flags => "R",
	comment   => "construct SSE Max: Max(a, b) = Max(b, a) = a > b ? a : b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. max%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xMin => {
	irn_flags => "R",
	comment   => "construct SSE Min: Min(a, b) = Min(b, a) = a < b ? a : b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. min%XXM %binop',
	latency   => 2,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xAnd => {
	irn_flags => "R",
	comment   => "construct SSE And: And(a, b) = a AND b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. andp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xOr => {
	irn_flags => "R",
	comment   => "construct SSE Or: Or(a, b) = a OR b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. orp%XSD %binop',
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xXor => {
	irn_flags => "R",
	comment   => "construct SSE Xor: Xor(a, b) = a XOR b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. xorp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

# not commutative operations

xAndNot => {
	irn_flags => "R",
	comment   => "construct SSE AndNot: AndNot(a, b) = a AND NOT b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4" ] },
	emit      => '. andnp%XSD %binop',
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xSub => {
	irn_flags => "R",
	comment   => "construct SSE Sub: Sub(a, b) = a - b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3" ] },
	emit      => '. sub%XXM %binop',
	latency   => 4,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xDiv => {
	irn_flags => "R",
	comment   => "construct SSE Div: Div(a, b) = a / b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4" ] },
	outs      => [ "res", "M" ],
	emit      => '. div%XXM %binop',
	latency   => 16,
	units     => [ "SSE" ],
},

# other operations

xCmp => {
	irn_flags => "R",
	comment   => "construct SSE Compare: Cmp(a, b) == a = a cmp b",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "in_r3 !in_r4" ] },
	latency   => 3,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

xCondJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "construct conditional jump: UCOMIS A, B && JMPxx LABEL",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "none", "none" ] },
	outs      => [ "false", "true" ],
	latency   => 5,
	units     => [ "SSE" ],
},

xConst => {
	op_flags  => "c",
	irn_flags => "R",
	comment   => "represents a SSE constant",
	reg_req   => { out => [ "xmm" ] },
	emit      => '. mov%XXM $%C, %D1',
	latency   => 2,
	units     => [ "SSE" ],
	mode      => "mode_E",
},

# Load / Store

xLoad => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "construct SSE Load: Load(ptr, mem) = LD ptr",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	emit      => '. mov%XXM %AM, %D1',
	outs      => [ "res", "M" ],
	latency   => 2,
	units     => [ "SSE" ],
},

xStore => {
	op_flags => "L|F",
	state    => "exc_pinned",
	comment  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ] },
	emit     => '. mov%XXM %binop',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => "mode_M",
},

xStoreSimple => {
	op_flags => "L|F",
	state    => "exc_pinned",
	comment  => "construct Store without index: Store(ptr, val, mem) = ST ptr,val",
	reg_req  => { in => [ "gp", "xmm", "none" ] },
	emit     => '. mov%XXM %S2, %AM',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => "mode_M",
},

CvtSI2SS => {
	op_flags => "L|F",
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm" ] },
	emit     => '. cvtsi2ss %D1, %AM',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => $mode_xmm
},

CvtSI2SD => {
	op_flags => "L|F",
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm" ] },
	emit     => '. cvtsi2sd %unop',
	latency  => 2,
	units    => [ "SSE" ],
	mode     => $mode_xmm
},


l_X87toSSE => {
	op_flags => "L|F",
	comment  => "construct: transfer a value from x87 FPU into a SSE register",
	cmp_attr => "return 1;",
	arity    => 3,
},

l_SSEtoX87 => {
	op_flags => "L|F",
	comment  => "construct: transfer a value from SSE register to x87 FPU",
	cmp_attr => "return 1;",
	arity    => 3,
},

GetST0 => {
	op_flags => "L|F",
	irn_flags => "I",
	state    => "exc_pinned",
	comment  => "store ST0 onto stack",
	reg_req  => { in => [ "gp", "gp", "none" ] },
	emit     => '. fstp%XM %AM',
	latency  => 4,
	units    => [ "SSE" ],
	mode     => "mode_M",
},

SetST0 => {
	op_flags => "L|F",
	irn_flags => "I",
	state    => "exc_pinned",
	comment  => "load ST0 from stack",
	reg_req  => { in => [ "gp", "none" ], out => [ "vf0", "none" ] },
	emit     => '. fld%M %AM',
	outs     => [ "res", "M" ],
	latency  => 2,
	units     => [ "SSE" ],
},

# CopyB

CopyB => {
	op_flags => "F|H",
	state    => "pinned",
	comment  => "implements a memcopy: CopyB(dst, src, size, mem) == memcpy(dst, src, size)",
	reg_req  => { in => [ "edi", "esi", "ecx", "none" ], out => [ "edi", "esi", "ecx", "none" ] },
	outs     => [ "DST", "SRC", "CNT", "M" ],
	units    => [ "GP" ],
	modified_flags => [ "DF" ]
},

CopyB_i => {
	op_flags => "F|H",
	state    => "pinned",
	comment  => "implements a memcopy: CopyB(dst, src, mem) == memcpy(dst, src, attr(size))",
	reg_req  => { in => [ "edi", "esi", "none" ], out => [  "edi", "esi", "none" ] },
	outs     => [ "DST", "SRC", "M" ],
	units    => [ "GP" ],
	modified_flags => [ "DF" ]
},

# Conversions

Conv_I2I => {
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "in_r3", "none" ] },
	comment  => "construct Conv Int -> Int",
	units    => [ "GP" ],
	mode     => $mode_gp,
	modified_flags => $status_flags
},

Conv_I2I8Bit => {
	reg_req  => { in => [ "gp", "gp", "eax ebx ecx edx", "none" ], out => [ "in_r3", "none" ] },
	comment  => "construct Conv Int -> Int",
	units    => [ "GP" ],
	mode     => $mode_gp,
	modified_flags => $status_flags
},

Conv_I2FP => {
	reg_req  => { in => [ "gp", "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	comment  => "construct Conv Int -> Floating Point",
	latency  => 10,
	units    => [ "SSE" ],
	mode     => "mode_E",
},

Conv_FP2I => {
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ], out => [ "gp", "none" ] },
	comment  => "construct Conv Floating Point -> Int",
	latency  => 10,
	units    => [ "SSE" ],
	mode     => $mode_gp,
},

Conv_FP2FP => {
	reg_req  => { in => [ "gp", "gp", "xmm", "none" ], out => [ "xmm", "none" ] },
	comment  => "construct Conv Floating Point -> Floating Point",
	latency  => 8,
	units    => [ "SSE" ],
	mode     => "mode_E",
},

CmpCMov => {
	irn_flags => "R",
	comment   => "construct Conditional Move: CMov(sel, a, b) == sel ? a : b",
	reg_req   => { in => [ "gp", "gp", "gp", "gp" ], out => [ "in_r4" ] },
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

PsiCondCMov => {
	irn_flags => "R",
	comment   => "check if Psi condition tree evaluates to true and move result accordingly",
	reg_req   => { in => [ "gp", "gp", "gp" ], out => [ "in_r3" ] },
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

xCmpCMov => {
	irn_flags => "R",
	comment   => "construct Conditional Move: SSE Compare + int CMov ",
	reg_req   => { in => [ "xmm", "xmm", "gp", "gp" ], out => [ "in_r4" ] },
	latency   => 5,
	units     => [ "SSE" ],
	mode      => $mode_gp,
},

vfCmpCMov => {
	irn_flags => "R",
	comment   => "construct Conditional Move: x87 Compare + int CMov",
	reg_req   => { in => [ "vfp", "vfp", "gp", "gp" ], out => [ "in_r4" ] },
	latency   => 10,
	units     => [ "VFP" ],
	mode      => $mode_gp,
},

CmpSet => {
	irn_flags => "R",
	comment   => "construct Set: Set(sel) == sel ? 1 : 0",
	reg_req   => { in => [ "gp", "gp", "gp", "gp", "none" ], out => [ "eax ebx ecx edx" ] },
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

PsiCondSet => {
	irn_flags => "R",
	comment   => "check if Psi condition tree evaluates to true and set result accordingly",
	reg_req   => { in => [ "gp" ], out => [ "eax ebx ecx edx" ] },
	latency   => 2,
	units     => [ "GP" ],
	mode      => $mode_gp,
},

xCmpSet => {
	irn_flags => "R",
	comment   => "construct Set: SSE Compare + int Set",
	reg_req   => { in => [ "gp", "gp", "xmm", "xmm", "none" ], out => [ "eax ebx ecx edx" ] },
	latency   => 5,
	units     => [ "SSE" ],
	mode      => $mode_gp,
},

vfCmpSet => {
	irn_flags => "R",
	comment   => "construct Set: x87 Compare + int Set",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "eax ebx ecx edx" ] },
	latency   => 10,
	units     => [ "VFP" ],
	mode      => $mode_gp,
},

vfCMov => {
	irn_flags => "R",
	comment   => "construct x87 Conditional Move: vfCMov(sel, a, b) = sel ? a : b",
	reg_req   => { in => [ "vfp", "vfp", "vfp", "vfp" ], out => [ "vfp" ] },
	latency   => 10,
	units     => [ "VFP" ],
	mode      => "mode_E",
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
	comment   => "virtual fp Add: Add(a, b) = Add(b, a) = a + b",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfmul => {
	irn_flags => "R",
	comment   => "virtual fp Mul: Mul(a, b) = Mul(b, a) = a * b",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

l_vfmul => {
	op_flags  => "C",
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp Mul: Mul(a, b) = Mul(b, a) = a * b",
	arity     => 2,
},

vfsub => {
	irn_flags => "R",
	comment   => "virtual fp Sub: Sub(a, b) = a - b",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

l_vfsub => {
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp Sub: Sub(a, b) = a - b",
	arity     => 2,
},

vfdiv => {
	comment   => "virtual fp Div: Div(a, b) = a / b",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "vfp" ] },
	outs      => [ "res", "M" ],
	latency   => 20,
	units     => [ "VFP" ],
},

l_vfdiv => {
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp Div: Div(a, b) = a / b",
	outs      => [ "res", "M" ],
	arity     => 2,
},

vfprem => {
	comment   => "virtual fp Rem: Rem(a, b) = a - Q * b (Q is integer)",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "vfp" ] },
	latency   => 20,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

l_vfprem => {
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp Rem: Rem(a, b) = a - Q * b (Q is integer)",
	arity     => 2,
},

vfabs => {
	irn_flags => "R",
	comment   => "virtual fp Abs: Abs(a) = |a|",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfchs => {
	irn_flags => "R",
	comment   => "virtual fp Chs: Chs(a) = -a",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfsin => {
	irn_flags => "R",
	comment   => "virtual fp Sin: Sin(a) = sin(a)",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	latency   => 150,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfcos => {
	irn_flags => "R",
	comment   => "virtual fp Cos: Cos(a) = cos(a)",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	latency   => 150,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfsqrt => {
	irn_flags => "R",
	comment   => "virtual fp Sqrt: Sqrt(a) = a ^ 0.5",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	latency   => 30,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

# virtual Load and Store

vfld => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "virtual fp Load: Load(ptr, mem) = LD ptr -> reg",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "vfp", "none" ] },
	outs      => [ "res", "M" ],
	latency   => 2,
	units     => [ "VFP" ],
},

vfst => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	comment   => "virtual fp Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req   => { in => [ "gp", "gp", "vfp", "none" ] },
	latency   => 2,
	units     => [ "VFP" ],
	mode      => "mode_M",
},

# Conversions

vfild => {
	comment   => "virtual fp integer Load: Load(ptr, mem) = iLD ptr -> reg",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "vfp", "none" ] },
	outs      => [ "res", "M" ],
	latency   => 4,
	units     => [ "VFP" ],
},

l_vfild => {
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp integer Load: Load(ptr, mem) = iLD ptr -> reg",
	outs      => [ "res", "M" ],
	arity     => 2,
},

vfist => {
	comment   => "virtual fp integer Store: Store(ptr, val, mem) = iST ptr,val",
	reg_req   => { in => [ "gp", "gp", "vfp", "fpcw", "none" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_M",
},

l_vfist => {
	cmp_attr  => "return 1;",
	comment   => "lowered virtual fp integer Store: Store(ptr, val, mem) = iST ptr,val",
	arity     => 3,
	mode      => "mode_M",
},


# constants

vfldz => {
	irn_flags => "R",
	comment   => "virtual fp Load 0.0: Ld 0.0 -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfld1 => {
	irn_flags => "R",
	comment   => "virtual fp Load 1.0: Ld 1.0 -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfldpi => {
	irn_flags => "R",
	comment   => "virtual fp Load pi: Ld pi -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfldln2 => {
	irn_flags => "R",
	comment   => "virtual fp Load ln 2: Ld ln 2 -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfldlg2 => {
	irn_flags => "R",
	comment   => "virtual fp Load lg 2: Ld lg 2 -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfldl2t => {
	irn_flags => "R",
	comment   => "virtual fp Load ld 10: Ld ld 10 -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfldl2e => {
	irn_flags => "R",
	comment   => "virtual fp Load ld e: Ld ld e -> reg",
	reg_req   => { out => [ "vfp" ] },
	latency   => 4,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

vfConst => {
	op_flags  => "c",
	irn_flags => "R",
#  init_attr => "  set_ia32_ls_mode(res, mode);",
	comment   => "represents a virtual floating point constant",
	reg_req   => { out => [ "vfp" ] },
	latency   => 3,
	units     => [ "VFP" ],
	mode      => "mode_E",
},

# other

vfCondJmp => {
	state     => "pinned",
	op_flags  => "L|X|Y",
	comment   => "represents a virtual floating point compare",
	reg_req   => { in => [ "gp", "gp", "vfp", "vfp", "none" ], out => [ "none", "none", "eax" ] },
	outs      => [ "false", "true", "temp_reg_eax" ],
	latency   => 10,
	units     => [ "VFP" ],
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
	comment   => "x87 Add: Add(a, b) = Add(b, a) = a + b",
	reg_req   => { },
	emit      => '. fadd%XM %x87_binop',
},

faddp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 Add: Add(a, b) = Add(b, a) = a + b",
	reg_req   => { },
	emit      => '. faddp %x87_binop',
},

fmul => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Mul: Mul(a, b) = Mul(b, a) = a + b",
	reg_req   => { },
	emit      => '. fmul%XM %x87_binop',
},

fmulp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Mul: Mul(a, b) = Mul(b, a) = a + b",
	reg_req   => { },
	emit      => '. fmulp %x87_binop',,
},

fsub => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Sub: Sub(a, b) = a - b",
	reg_req   => { },
	emit      => '. fsub%XM %x87_binop',
},

fsubp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Sub: Sub(a, b) = a - b",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fsubrp %x87_binop',
},

fsubr => {
	op_flags  => "R",
	rd_constructor => "NONE",
	irn_flags => "R",
	comment   => "x87 fp SubR: SubR(a, b) = b - a",
	reg_req   => { },
	emit      => '. fsubr%XM %x87_binop',
},

fsubrp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	irn_flags => "R",
	comment   => "x87 fp SubR: SubR(a, b) = b - a",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fsubp %x87_binop',
},

fprem => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Rem: Rem(a, b) = a - Q * b (Q is integer)",
	reg_req   => { },
	emit      => '. fprem1',
},

# this node is just here, to keep the simulator running
# we can omit this when a fprem simulation function exists
fpremp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Rem: Rem(a, b) = a - Q * b (Q is integer)",
	reg_req   => { },
	emit      => '. fprem1',
},

fdiv => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Div: Div(a, b) = a / b",
	reg_req   => { },
	emit      => '. fdiv%XM %x87_binop',
},

fdivp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Div: Div(a, b) = a / b",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fdivrp %x87_binop',
},

fdivr => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp DivR: DivR(a, b) = b / a",
	reg_req   => { },
	emit      => '. fdivr%XM %x87_binop',
},

fdivrp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp DivR: DivR(a, b) = b / a",
	reg_req   => { },
# see note about gas bugs
	emit      => '. fdivp %x87_binop',
},

fabs => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Abs: Abs(a) = |a|",
	reg_req   => { },
	emit      => '. fabs',
},

fchs => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Chs: Chs(a) = -a",
	reg_req   => { },
	emit      => '. fchs',
},

fsin => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Sin: Sin(a) = sin(a)",
	reg_req   => { },
	emit      => '. fsin',
},

fcos => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Cos: Cos(a) = cos(a)",
	reg_req   => { },
	emit      => '. fcos',
},

fsqrt => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp Sqrt: Sqrt(a) = a ^ 0.5",
	reg_req   => { },
	emit      => '. fsqrt $',
},

# x87 Load and Store

fld => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	comment   => "x87 fp Load: Load(ptr, mem) = LD ptr -> reg",
	reg_req   => { },
	emit      => '. fld%XM %AM',
},

fst => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	comment   => "x87 fp Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req   => { },
	emit      => '. fst%XM %AM',
	mode      => "mode_M",
},

fstp => {
	rd_constructor => "NONE",
	op_flags  => "R|L|F",
	state     => "exc_pinned",
	comment   => "x87 fp Store: Store(ptr, val, mem) = ST ptr,val",
	reg_req   => { },
	emit      => '. fstp%XM %AM',
	mode      => "mode_M",
},

# Conversions

fild => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp integer Load: Load(ptr, mem) = iLD ptr -> reg",
	reg_req   => { },
	emit      => '. fild%XM %AM',
},

fist => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp integer Store: Store(ptr, val, mem) = iST ptr,val",
	reg_req   => { },
	emit      => '. fist%M %AM',
	mode      => "mode_M",
},

fistp => {
	op_flags  => "R",
	rd_constructor => "NONE",
	comment   => "x87 fp integer Store: Store(ptr, val, mem) = iST ptr,val",
	reg_req   => { },
	emit      => '. fistp%M %AM',
	mode      => "mode_M",
},

# constants

fldz => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load 0.0: Ld 0.0 -> reg",
	reg_req   => { },
	emit      => '. fldz',
},

fld1 => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load 1.0: Ld 1.0 -> reg",
	reg_req   => { },
	emit      => '. fld1',
},

fldpi => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load pi: Ld pi -> reg",
	reg_req   => { },
	emit      => '. fldpi',
},

fldln2 => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load ln 2: Ld ln 2 -> reg",
	reg_req   => { },
	emit      => '. fldln2',
},

fldlg2 => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load lg 2: Ld lg 2 -> reg",
	reg_req   => { },
	emit      => '. fldlg2',
},

fldl2t => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load ld 10: Ld ld 10 -> reg",
	reg_req   => { },
	emit      => '. fldll2t',
},

fldl2e => {
	op_flags  => "R|c",
	irn_flags  => "R",
	comment   => "x87 fp Load ld e: Ld ld e -> reg",
	reg_req   => { },
	emit      => '. fldl2e',
},

# fxch, fpush, fpop
# Note that it is NEVER allowed to do CSE on these nodes
# Moreover, note the virtual register requierements!

fxch => {
	op_flags  => "R|K",
	comment   => "x87 stack exchange",
	reg_req   => { },
	cmp_attr  => "return 1;",
	emit      => '. fxch %X1',
},

fpush => {
	op_flags  => "R|K",
	comment   => "x87 stack push",
	reg_req   => {},
	cmp_attr  => "return 1;",
	emit      => '. fld %X1',
},

fpushCopy => {
	op_flags  => "R",
	comment   => "x87 stack push",
	reg_req   => { in => [ "vfp"], out => [ "vfp" ] },
	cmp_attr  => "return 1;",
	emit      => '. fld %X1',
},

fpop => {
	op_flags  => "R|K",
	comment   => "x87 stack pop",
	reg_req   => { },
	cmp_attr  => "return 1;",
	emit      => '. fstp %X1',
},

# compare

fcomJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare",
	reg_req   => { },
},

fcompJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare and pop",
	reg_req   => { },
},

fcomppJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare and pop twice",
	reg_req   => { },
},

fcomrJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare reverse",
	reg_req   => { },
},

fcomrpJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare reverse and pop",
	reg_req   => { },
},

fcomrppJmp => {
	op_flags  => "L|X|Y",
	comment   => "floating point compare reverse and pop twice",
	reg_req   => { },
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
	comment   => "construct SSE Load: Load(ptr, mem) = LD ptr",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "xmm", "none" ] },
	emit      => '. movdqu %D1, %AM',
	outs      => [ "res", "M" ],
	units     => [ "SSE" ],
},

xxStore => {
	op_flags => "L|F",
	state    => "exc_pinned",
	comment  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
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
