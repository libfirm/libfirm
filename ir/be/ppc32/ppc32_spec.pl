# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc32, ...)

$arch = "ppc32";
$new_emit_syntax = 1;

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   "op_flags"  => "N|L|C|X|I|F|Y|H|c|K",
#   "irn_flags" => "R|N|I"
#   "arity"     => "0|1|2|3 ... |variable|dynamic|any",
#   "state"     => "floats|pinned|mem_pinned|exc_pinned",
#   "args"      => [
#                    { "type" => "type 1", "name" => "name 1" },
#                    { "type" => "type 2", "name" => "name 2" },
#                    ...
#                  ],
#   "comment"   => "any comment for constructor",
#   "reg_req"   => { "in" => [ "reg_class|register" ], "out" => [ "reg_class|register|in_rX" ] },
#   "cmp_attr"  => "c source code for comparing node attributes",
#   "emit"      => "emit code with templates",
#   "rd_constructor" => "c source code which constructs an ir_node"
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
  "gp" => [
                         { "name" => "r0", "type" => 1 },
                         { "name" => "r2", "type" => 1 },
                         { "name" => "r3", "type" => 1 },
                         { "name" => "r4", "type" => 1 },
                         { "name" => "r5", "type" => 1 },
                         { "name" => "r6", "type" => 1 },
                         { "name" => "r7", "type" => 1 },
                         { "name" => "r8", "type" => 1 },
                         { "name" => "r9", "type" => 1 },
                         { "name" => "r10", "type" => 1 },
#                        { "name" => "r11", "type" => 1 },
#                        { "name" => "r12", "type" => 1 },
                         { "name" => "r13", "type" => 2 },
                         { "name" => "r14", "type" => 2 },
                         { "name" => "r15", "type" => 2 },
#                        { "name" => "r16", "type" => 2 },
#                        { "name" => "r17", "type" => 2 },
#                        { "name" => "r18", "type" => 2 },
#                        { "name" => "r19", "type" => 2 },
#                        { "name" => "r20", "type" => 2 },
#                        { "name" => "r21", "type" => 2 },
#                        { "name" => "r22", "type" => 2 },
#                        { "name" => "r23", "type" => 2 },
#                        { "name" => "r24", "type" => 2 },
#                        { "name" => "r25", "type" => 2 },
#                        { "name" => "r26", "type" => 2 },
#                        { "name" => "r27", "type" => 2 },
#                        { "name" => "r28", "type" => 2 },
#                        { "name" => "r29", "type" => 2 },
#                        { "name" => "r30", "type" => 2 },
                         { "name" => "r31", "type" => 2 },
                         { "name" => "r1", "type" => 6 }, # this is our stackpointer
                         { "mode" => "mode_P" }
                       ],
  "fp"  => [
#                        { "name" => "f0", "type" => 1 }, # => reserved for FP Perm
                         { "name" => "f1", "type" => 1 },
                         { "name" => "f2", "type" => 1 },
                         { "name" => "f3", "type" => 1 },
                         { "name" => "f4", "type" => 1 },
                         { "name" => "f5", "type" => 1 },
                         { "name" => "f6", "type" => 1 },
                         { "name" => "f7", "type" => 1 },
                         { "name" => "f8", "type" => 1 },
                         { "name" => "f9", "type" => 1 },
                         { "name" => "f10", "type" => 1 },
                         { "name" => "f11", "type" => 1 },
                         { "name" => "f12", "type" => 1 },
                         { "name" => "f13", "type" => 1 },
                         { "name" => "f14", "type" => 2 },
                         { "name" => "f15", "type" => 2 },
                         { "name" => "f16", "type" => 2 },
#                        { "name" => "f17", "type" => 2 },
#                        { "name" => "f18", "type" => 2 },
#                        { "name" => "f19", "type" => 2 },
#                        { "name" => "f20", "type" => 2 },
#                        { "name" => "f21", "type" => 2 },
#                        { "name" => "f22", "type" => 2 },
#                        { "name" => "f23", "type" => 2 },
#                        { "name" => "f24", "type" => 2 },
#                        { "name" => "f25", "type" => 2 },
#                        { "name" => "f26", "type" => 2 },
#                        { "name" => "f27", "type" => 2 },
#                        { "name" => "f28", "type" => 2 },
#                        { "name" => "f29", "type" => 2 },
#                        { "name" => "f30", "type" => 2 },
#                        { "name" => "f31", "type" => 2 },
                         { "mode" => "mode_D" }
                       ],
  "condition" => [
                         { "name" => "cr0", "type" => 1 },
                         { "name" => "cr1", "type" => 1 },
                         { "name" => "cr2", "type" => 2 },
                         { "name" => "cr3", "type" => 2 },
                         { "name" => "cr4", "type" => 2 },
                         { "name" => "cr5", "type" => 1 },
                         { "name" => "cr6", "type" => 1 },
#                        { "name" => "cr7", "type" => 1 }, # => reserved for Condition Perm
                         { "mode" => "mode_P" } # real mode is 4 bit, but doesn't matter ...
                       ],
  "link" => [
                         { "name" => "lr", "type" => 4 }, # 3
                         { "mode" => "mode_P" }
                       ],
  "count" => [
                         { "name" => "ctr", "type" => 1 },
                         { "mode" => "mode_P" }
                       ]
); # %reg_classes

%emit_templates = (
    S0 => "${arch}_emit_source_register(env, node, 0);",
    S1 => "${arch}_emit_source_register(env, node, 1);",
    S2 => "${arch}_emit_source_register(env, node, 2);",
    D0 => "${arch}_emit_dest_register(env, node, 0);",
    D1 => "${arch}_emit_dest_register(env, node, 1);",
    D2 => "${arch}_emit_dest_register(env, node, 2);",
	O  => "${arch}_emit_offset(env, node);",
	C  => "${arch}_emit_immediate(env, node);",
	RLWIMI => "${arch}_emit_rlwimi_helper(env, node);",
);

$default_cmp_attr = "NULL";

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

"Add" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. add     %D0, %S0, %S1',
},

"Addi" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "construct Add: Addi(a, const) = Addi(const, a) = a + const",
  "reg_req"   => { "in" => [ "!r0" ], "out" => [ "gp" ] },
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. addi    %D0, %S0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},


"Mullw" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mul: Mullw(a, b) = Mullw(b, a) = lo32(a * b)",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mullw   %D0, %S0, %S1',
},

"Mulhw" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mul: Mulhw(a, b) = Mulhw(b, a) = hi32(a * b)",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mulhw   %D0, %S0, %S1',
},

"Mulhwu" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mul: Mulhwu(a, b) = Mulhwu(b, a) = hi32(a * b)",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mulhwu  %D0, %S0, %S1',
},

#"Mul_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. mul %S0, %C, %D0',
#},

"And" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, b) = And(b, a) = a AND b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. and     %D0, %S0, %S1',
},

#"And_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct And: And(a, const) = And(const, a) = a AND const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. and %S0, %C, %D0',
#},

"Or" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. or      %D0, %S0, %S1',
},

#"Or_i" => {
#  "op_flags"  => "C",
#  "irn_flags" => "R",
#  "comment"   => "construct Or: Or(a, const) = Or(const, a) = a OR const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. or %S0, %C, %D0',
#},

"Xor" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Xor: Xor(a, b) = Xor(b, a) = a XOR b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. xor     %D0, %S0, %S1',
},

#"Xor_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Xor: Xor(a, const) = Xor(const, a) = a EOR const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. xor %S0, %C, %D0',
#},

# not commutative operations

"Sub" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. sub %D0, %S0, %S1',
},

#"Sub_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Sub: Sub(a, const) = a - const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. subl %S0, %C, %D0',
#},

"Slw" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, b) = a << b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. slw %D0, %S0, %S1',
},

#"Shl_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Shl: Shl(a, const) = a << const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. shl %S0, %C, %D0',
#},

"Srw" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Srw(a, b): c = a >> b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. srw     %D0, %S0, %S1',
},

#"Shr_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Shr: Shr(a, const) = a >> const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. shr %S0, %C, %D0',
#},

"Sraw" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Sraw(a, b): c = a >> b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. sraw %D0, %S0, %S1',
},

"Srawi" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Srawi(a, const): c = a >> const",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. sraw %D0, %S0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',

},

"Rlwnm" => {
  "irn_flags" => "R",
  "comment"   => "construct ???: Rlwnm(a, b): c = a ROTL b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. rlwnm %D0, %S0, %S1',
},

"Rlwinm" => {
  "irn_flags" => "R",
  "comment"   => "construct ???: Rlwinm(a, b_const, c_const, d_const): (m = MASK(c, d)) e = (a ROTL b) & m",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. rlwinm %D0, %S0, %RLWIMI',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},


"Neg" => {
  "irn_flags" => "R",
  "comment"   => "construct Minus: Neg(a) = -a",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. neg %D0, %S0',
},

"Not" => {
  "irn_flags" => "R",
  "comment"   => "construct Not: Not(a) = !a",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. nor %D0, %S0, %S0',
},

"Extsb" => {
  "irn_flags" => "R",
  "comment"   => "construct Sign extension of byte: Extsb(char a) = (int) a",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. extsb %D0, %S0',
},

"Extsh" => {
  "irn_flags" => "R",
  "comment"   => "construct Sign extension of halfword: Extsh(char a) = (short) a",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. extsh %D0, %S0',
},

"Divw" => {
  "irn_flags" => "R",
  "comment"   => "construct Div (signed): Div(a, b) = a div b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. divw %D0, %S0, %S1',
},

"Divwu" => {
  "irn_flags" => "R",
  "comment"   => "construct Div (unsigned): Div(a, b) = a div b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. divwu %D0, %S0, %S1',
},

"Mtctr" => {
  "irn_flags" => "R",
  "comment"   => "construct Mtctr: Ctr = a",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "count" ] },
  "emit"      => '. mtctr %S0',
},


# other operations

"Const" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "Const (high-level node)",
  "reg_req"   => { "out" => [ "gp" ] },
  "cmp_attr"  =>
'
	return attr_a->data.constant_tarval != attr_b->data.constant_tarval;
',
},

"fConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "float Const (high-level node)",
  "reg_req"   => { "out" => [ "fp" ] },
  "cmp_attr"  =>
'
	return attr_a->data.constant_tarval != attr_b->data.constant_tarval;
',
},

"SymConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "SymConst (high-level node)",
  "reg_req"   => { "out" => [ "gp" ] },
  "cmp_attr"  =>
'
	return attr_a->data.constant_tarval != attr_b->data.constant_tarval;
',
},

"Unknown" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "construct unknown register",
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. \t\t /* use %D0 as uninitialized value */',
  "cmp_attr"  =>
'
	return 1;
',
},

"fUnknown" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "construct unknown float register",
  "reg_req"   => { "out" => [ "fp" ] },
  "emit"      => '. \t\t /* use %D0 as uninitialized value */',
  "cmp_attr"  =>
'
	return 1;
',
},

"cUnknown" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "construct unknown condition register",
  "reg_req"   => { "out" => [ "condition" ] },
  "emit"      => '. \t\t /* use %D0 as uninitialized value */',
  "cmp_attr"  =>
'
	return 1;
',
},

"Addi_zero" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "load constant (16bit with sign extension)",
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. addi    %D0, 0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"Branch" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "branch somewhere",
  "reg_req"   => { "in" => [ "condition" ], "out" => [ "none", "none" ] },
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"LoopCopy" => {
  "irn_flags" => "R",
  "comment"   => "construct LoopCopy(src, dest, count, mem): Copy count words from src to dest",
  "reg_req"   => { "in" => [ "gp", "gp", "count", "none" ], "out" => [ "none", "in_r1", "in_r2", "in_r3", "gp" ] },
},

"Switch" => {
  "op_flags" => "L|X|Y",
  "comment"   => "construct Switch(selector): Jump to whatever",
  "reg_req"   => { "in" => [ "gp", "gp", "condition" ], "out" => [ "none" ] },
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"Addis_zero" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "load the constant to higher 16 bit of register",
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. addis %D0, 0, %C',
  "attr"      => "ppc32_attr_offset_mode om, tarval *tv, ident *id",
  "init_attr" =>
'
	attr->offset_mode = om;
	if (tv) {
		attr->content_type = ppc32_ac_Const;
		attr->data.constant_tarval = tv;
	}
	else if (id) {
		attr->content_type = ppc32_ac_SymConst;
		attr->data.symconst_ident = id;
	}
',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"Ori" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "ors constant with register",
  "reg_req"   => { "in" => [ "gp"], "out" => [ "gp" ] },
  "emit"      => '. ori %D0, %S0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"Andi_dot" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "ands constant with register with cr0 update",
  "reg_req"   => { "in" => [ "gp"], "out" => [ "gp", "cr0" ] },
  "emit"      => '. andi. %D0, %S0,%C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},

"Cmp" => {
  "irn_flags" => "R",
  "comment"   => "construct Cmp: Cmp(a, b) = Flags in crX",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "condition" ] },
  "emit"      => '. cmp %D0, 0, %S0, %S1',
},

"Cmpi" => {
  "irn_flags" => "R",
  "comment"   => "construct Cmp immediate: Cmpi(a, const) = Flags in crX",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "condition" ] },
  "emit"      => '. cmpi %D0, 0, %S0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},


"Cmpl" => {
  "irn_flags" => "R",
  "comment"   => "construct Cmp logical: Cmpl(a, b) = Flags in crX",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "condition" ] },
  "emit"      => '. cmpl %D0, 0, %S0, %S1',
},

"Cmpli" => {
  "irn_flags" => "R",
  "comment"   => "construct Cmp logical immediate: Cmpli(a, const) = Flags in crX",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "condition" ] },
  "emit"      => '. cmpli %D0, 0, %S0, %C',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
},


# Load / Store

"Lbz" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load (byte unsigned): Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. lbz %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Lhz" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load (halfword unsigned): Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. lhz %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Lha" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load (halfword signed): Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. lha %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Lwz" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load (word): Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. lwz %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Lwzu" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load with update (word): Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "gp", "in_r1", "none"] },
  "emit"      => '. lwzu %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "ptr", "M" ],
},

"Stb" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store (byte) (ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "!r0", "gp", "none" ], "out" => [ "none" ] },
  "emit"      => '. stb %S1, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "M" ],
},

"Sth" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store (halfword) (ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "!r0", "gp", "none" ], "out" => [ "none" ] },
  "emit"      => '. sth %S1, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "M" ],
},

"Stw" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store (word) (ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "!r0", "gp", "none" ], "out" => [ "none" ] },
  "emit"      => '. stw %S1, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "M" ],
},

#--------------------------------------------------------#
#    __ _             _                     _            #
#   / _| |           | |                   | |           #
#  | |_| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
#  |  _| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  | | | | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
#  |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#--------------------------------------------------------#

# commutative operations

"fAdd" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fadd %D0, %S0, %S1',
},

"fAdds" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Add (single): Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fadds %D0, %S0, %S1',
},

"fMul" => {
  "op_flags"  => "C",
  "comment"   => "construct FP Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fmul %D0, %S0, %S1',
},

"fMuls" => {
  "op_flags"  => "C",
  "comment"   => "construct FP Mul (single): Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fmuls %D0, %S0, %S1',
},

"fNeg" => {
  "comment"   => "construct FP Negation: fNeg(a) = -a",
  "reg_req"   => { "in" => [ "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fneg %D0, %S0',
},


"fMax" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fmax %S0, %S1, %D0',
},

"fMin" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fmin %S0, %S1, %D0',
},

# not commutative operations

"fSub" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fsub %D0, %S0, %S1',
},

"fSubs" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Sub (single): Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fsubs %D0, %S0, %S1',
},

"fDiv" => {
  "comment"   => "construct FP Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fdiv %D0, %S0, %S1',
},

"fDivs" => {
  "comment"   => "construct FP Div (single): Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fdivs %D0, %S0, %S1',
},

"fMinus" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Minus: fMinus(a) = -a",
  "reg_req"   => { "in" => [ "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fneg %D0, %S0',
},

"fCtiw" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Convert to integer word: fCtiw(a) = (int) a",
  "reg_req"   => { "in" => [ "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fctiw %D0, %S0',
},

"fRsp" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Round to single: fRsp(a) = (float) a",
  "reg_req"   => { "in" => [ "fp" ], "out" => [ "fp" ] },
  "emit"      => '. frsp %D0, %S0',
},

"fAbs" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Abs: fAbs(a) = |a|",
  "reg_req"   => { "in" => [ "fp" ], "out" => [ "fp" ] },
  "emit"      => '. fabs %D0, %S0',
},

"fCmpu" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Cmp unordered: fCmpu(a, b) = a ? b",
  "reg_req"   => { "in" => [ "fp", "fp" ], "out" => [ "condition" ] },
  "emit"      => '. fcmpu %D0, %S0, %S1',
},

# other operations

#"fConst" => {
#  "op_flags"  => "c",
#  "irn_flags" => "R",
#  "comment"   => "represents a FP constant",
#  "reg_req"   => { "out" => [ "fp" ] },
#  "emit"      => '. fmov %C, %D0',
#  "cmp_attr"  =>
#'
#	/* TODO: compare fConst attributes */
#	return 1;
#',
#},

"fUnknown" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "construct unknown floating point register",
  "reg_req"   => { "out" => [ "fp" ] },
  "emit"      => '. \t\t /* use %D0 as uninitialized value */',
  "cmp_attr"  =>
'
	return 1;
',
},

# Load / Store

"Lfd" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct FP Load (double): Load(ptr, mem) = LD ptr",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "fp", "none" ] },
  "emit"      => '. lfd %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Lfs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct FP Load (single): Load(ptr, mem) = LD ptr",
  "reg_req"   => { "in" => [ "!r0", "none" ], "out" => [ "fp","none" ] },
  "emit"      => '. lfs %D0, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "res", "M" ],
},

"Stfd" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store (double): Store(ptr, val, mem)  = ST ptr,val",
  "reg_req"   => { "in" => [ "!r0", "fp", "none" ], "out" => [ "none" ] },
  "emit"      => '. stfd %S1, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "M" ],
},

"Stfs" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store (single): Store(ptr, val, mem)  = ST ptr,val",
  "reg_req"   => { "in" => [ "!r0", "fp", "none" ], "out" => [ "none" ] },
  "emit"      => '. stfs %S1, %O(%S0)',
  "cmp_attr"  =>
'
	return (attr_a->data.constant_tarval != attr_b->data.constant_tarval);
',
  "outs"      => [ "M" ],
},

); # end of %nodes
