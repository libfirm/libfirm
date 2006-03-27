# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "arm";

# $comment_string = 'WIRHABENKEINEKOMMENTARE';
$comment_string = '/*';

# the number of additional opcodes you want to register
$additional_opcodes = 0;

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
  "general_purpose" => [
                         { "name" => "r0", "type" => 1 },
                         { "name" => "r1", "type" => 1 },
                         { "name" => "r2", "type" => 1 },
                         { "name" => "r3", "type" => 1 },
                         { "name" => "r4", "type" => 2 },
                         { "name" => "r5", "type" => 2 },
                         { "name" => "r6", "type" => 2 },
                         { "name" => "r7", "type" => 2 },
                         { "name" => "r8", "type" => 2 },
                         { "name" => "r9", "type" => 2 },
                         { "name" => "r10", "type" => 2 },
                         { "name" => "r11", "type" => 2 },
                         { "name" => "r12", "type" => 6 }, # reserved for linker
                         { "name" => "r13", "type" => 6 }, # this is our stack pointer
                         { "name" => "r14", "type" => 3 }, # this is our return address
                         { "name" => "r15", "type" => 6 }, # this is our program counter
						 { "name" => "rxx", "type" => 6 }, # dummy register for no_mem
						 { "name" => "MURX", "type" => 6 }, # this is our workaround-register
                         { "mode" => "mode_P" }
                       ],
  "floating_point"  => [
                         { "name" => "f0", "type" => 1 },
                         { "name" => "f1", "type" => 1 },
                         { "name" => "f2", "type" => 1 },
                         { "name" => "f3", "type" => 1 },
                         { "name" => "f4", "type" => 2 },
                         { "name" => "f5", "type" => 2 },
                         { "name" => "f6", "type" => 2 },
                         { "name" => "f7", "type" => 2 },
						 { "name" => "fxx", "type" => 6 }, # dummy register for no_mem
                         { "mode" => "mode_D" }
                       ]
); # %reg_classes

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
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. ADD %D1, %S1, %S2\t\t\t/* Add(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Add_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. ADD %D1, %S1, %C\t\t\t/* Add(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Mul" => {
  #"op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "!in_r1" ] },
  "emit"      =>'. MUL %D1, %S1, %S2\t\t\t/* Mul(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"And" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, b) = And(b, a) = a AND b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. AND %D1, %S1, %S2\t\t/* And(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"And_i" => {
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, const) = And(const, a) = a AND const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. AND %D1, %S1, %C\t\t\t/* And(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Or" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. ORR %D1, %S1, %S2\t\t/* Or(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Or_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. ORR %D1, %S1, %C\t\t\t/* Or(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Or_Shl_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Or with immediate shifter operand",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. ORR %D1, %S1, %S2 LSL %C\t\t\t/* Or_Shl_i(%S1, %S2 << %C) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Eor" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. EOR %D1, %S1, %S2\t\t\t/* Xor(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Eor_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. EOR %D1, %S1, %C\t\t\t/* Xor(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

# not commutative operations

"Sub" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. SUB %D1, %S1, %S2\t\t\t/* Sub(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Sub_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, const) = a - const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. SUB %D1, %S1, %C\t\t\t/* Sub(%S1, %C) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Shl" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, b) = a << b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. MOV %D1, %S1, LSL %S2\t/* Shl(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shl_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, const) = a << const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. MOV %D1, %S1, LSL %C\t\t\t/* Shl(%S1, %C) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Shr" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Shr(a, b) = a >> b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"      => '. MOV %D1, %S1, LSR %S2\t\t\t/* Shr(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shr_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Shr(a, const) = a >> const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. MOV %D1, %S1, LSR %C\t\t\t/* Shr(%S1, %C) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Shrs" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Shrs(a, b) = a >> b",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"      => '. MOV %D1, %S1, ASR %S2\t\t\t\t\t/* Shrs(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shrs_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Shrs(a, const) = a >> const",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. MOV %D1, %S1, ASR %C\t\t\t/* Shrs(%S1, %C) -> %D1, (%A1, const) */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

#"Div" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Div: Div(a, b) = a / b",
#  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      =>'. div %S1, %S2, %D1\t\t\t/* div(%S1, %S2) -> %D1, (%A1, %A2) */'
#},
#
#"Div_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Div: Div(a, const) = a / const",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. div %S1, %C, %D1\t\t\t/* signed div(%C, %S1) -> %D1, (%A1, const) */'
#},
#
#"Mod" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Mod: Mod(a, b) = a % b",
#  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      =>'. mod %S1, %S2, %D1\t\t\t/* mod(%S1, %S2) -> %D1, (%A1, %A2) */'
#},
#
#"Mod_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct mod: Mod(a, const) = a % const",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. mod %S1, %C, %D1\t\t\t/* mod(%C, %S1) -> %D1, (%A1, const) */'
#},
#
#"DivMod" => {
#  "irn_flags" => "R",
#  "comment"   => "construct DivMod: DivMod(a, b) = a / b",
#  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      =>'. div %S1, %S2, %D1\t\t\t/* div(%S1, %S2) -> %D1, (%A1, %A2) */'
#},
#
#"Div_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct DivMod: DivMod(a, const) = a / const",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. div %S1, %C, %D1\t\t\t/* signed div(%C, %S1) -> %D1, (%A1, const) */'
#},

#"RotR" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotR: RotR(a, b) = a ROTR b",
#  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. MOV %D1, %S1, ROR %S2\t\t\t/* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
##  "emit"      => '. ror %S1, %S2, %D1\t\t\t/* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
#},

#"RotL" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotL: RotL(a, b) = a ROTL b",
#  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. rol %S1, %S2, %D1\t\t\t/* RotL(%S1, %S2) -> %D1, (%A1, %A2) */'
#},

#"RotL_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotL: RotL(a, const) = a ROTL const",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. rol %S1, %C, %D1\t\t\t/* RotL(%S1, %C) -> %D1, (%A1, const) */'
#},

"Minus" => {
  "irn_flags" => "R",
  "comment"   => "construct Minus: Minus(a) = -a",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      => '. RSB %D1, %S1, #0\t\t\t/* Neg(%S1) -> %D1, (%A1) */'
},

#"Inc" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Increment: Inc(a) = a++",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. inc %S1, %D1\t\t\t/* Inc(%S1) -> %D1, (%A1) */'
#},
#
#"Dec" => {
#  "irn_flags" => "R",
#  "comment"   => "construct Decrement: Dec(a) = a--",
#  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
#  "emit"      => '. dec %S1, %D1\t\t\t/* Dec(%S1) -> %D1, (%A1) */'
#},

"Not" => {
  "arity"       => 1,
#  "remat"       => 1,
  "irn_flags"   => "R",
  "comment"     => "construct Not: Not(a) = !a",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. MVN %D1, %S1\t\t\t/* Not(%S1) -> %D1, (%A1) */'
},

"Abs" => {
  "irn_flags" => "R",
  "comment"   => "construct Abs: Abs(a) = |a|",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"      =>
'. MOVS %S1, %S1, #0\t\t\t/* set condition flag */\n
. RSBMI %D1, %S1, #0\t\t\t/* Neg(%S1) -> %D1, (%A1) */'
},

# other operations

"EmptyReg" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"  => "just to get an emptz register for calculations",
  "reg_req"  => { "out" => [ "general_purpose" ] },
  "emit"      => '. /* %D1 now available for calculations */',
  "cmp_attr"  => 'return 1;'
},

"Copy" => {
  "comment"  => "implements a register copy",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
},

"CopyB" => {
  "op_flags" => "F|H",
  "state"    => "pinned",
  "comment"  => "implements a memcopy: CopyB(dst, src, size, mem) == memcpy(dst, src, size)",
  "reg_req"  => { "in" => [ "!r13", "!r13", "general_purpose", "general_purpose", "general_purpose", "none" ], "out" => [ "none" ] },
},

"Const" => {
  "irn_flags" => "R",
  "comment"   => "represents an integer constant",
  "reg_req"   => { "out" => [ "general_purpose" ] },
  "emit"      => '. MOV %D1, %C  \t\t\t/* Mov Const into register */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"Const_Neg" => {
  "irn_flags" => "R",
  "comment"   => "represents a negated integer constant",
  "reg_req"   => { "out" => [ "general_purpose" ] },
  "emit"      => '. MVN %D1, %C  \t\t\t/* Mov negated Const into register */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"SymConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a symbolic constant",
  "reg_req"   => { "out" => [ "general_purpose" ] },
#  "emit"      => '. LDR %D1, %C\t\t\t/* Mov Const into register */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

    return get_tarval_long(attr_a->value) != get_tarval_long(attr_b->value);
'
},

"CondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "cmp_attr"  => "  return arm_comp_condJmp(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "none", "none"] },
},

"SwitchJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct switch",
  "reg_req"   => { "in" => [ "general_purpose" ], "out" => [ "none" ] },
  "cmp_attr"  => "  return 0;\n",
},

# Load / Store

"Load" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"      => '. LDR %D1, [%S1, #0]\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
#  "emit"      => '. LDR %D1, %S1, %O\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Loadb" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"      => '. LDRB %D1, [%S1, #0]\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
#  "emit"      => '. LDRB %D1, %S1, %O\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Loadbs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"      => '. LDRSB %D1, [%S1, #0]\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
#  "emit"      => '. LDRSB %D1, %S1, %O\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Loadh" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"      => '. LDRH %D1, [%S1, #0]\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
#  "emit"      => '. LDRH %D1, %S1, %O\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Loadhs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"      => '. LDRSH %D1, [%S1, #0]\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
#  "emit"      => '. LDRSH %D1, %S1, %O\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Storeb" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. STRB %S2, [%S1, #0]\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
#  "emit"      => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"Storebs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. STRSB %S2, [%S1, #0]\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
#  "emit"      => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"Storeh" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. STRH %S2, [%S1, #0]\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
#  "emit"      => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"Storehs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. STRSH%S2, [%S1, #0]\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
#  "emit"      => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"Store" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. STR %S2, [%S1, #0]\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
#  "emit"      => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"StoreStackM4Inc" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "r13", "general_purpose", "general_purpose", "general_purpose", "general_purpose", "none" ], "out" => [ "general_purpose", "none" ] },
  "emit"      => '. STMFD %S1!, {%S2, %S3, %S4, %S5}\t\t\t/* Store multiple on Stack*/'
},

"LoadStackM3" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "r13", "none" ], "out" => [ "general_purpose", "general_purpose", "general_purpose", "none" ] },
  "emit"      => '. LDMFD %S1, {%D1, %D2, %D3}\t\t\t/* Load multiple from Stack */'
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

"fAddd" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct DP FP Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FADDD %D1, %S1, %S2\t\t\t/* FP Add(%S1, %S2) -> %D1 */'
},

"fAdds" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct SP FP Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FADDS %D1, %S1, %S2\t\t\t/* FP Add(%S1, %S2) -> %D1 */'
},

"fMuls" => {
  "op_flags"  => "C",
  "comment"   => "construct FP Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. FMULS %D1, %S1, %S2\t\t\t/* FP Mul(%S1, %S2) -> %D1 */'
},

"fMuld" => {
  "op_flags"  => "C",
  "comment"   => "construct FP Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. FMULD %D1, %S1, %S2\t\t\t/* FP Mul(%S1, %S2) -> %D1 */'
},

"fDivs" => {
  "comment"   => "construct FP Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. FDIVS %D1, %S1, %S2\t\t\t/* FP Div(%S1, %S2) -> %D1 */'
},

"fDivd" => {
  "comment"   => "construct FP Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. FDIVD %D1, %S1, %S2\t\t\t/* FP Div(%S1, %S2) -> %D1 */'
},

"fDiv" => {
  "comment"   => "construct FP Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. FDIV %D1, %S1, %S2\t\t\t/* FP Div(%S1, %S2) -> %D1 */'
},

"fMax" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. fmax %S1, %S2, %D1\t\t\t/* FP Max(%S1, %S2) -> %D1 */'
},

"fMin" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FP Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      =>'. fmin %S1, %S2, %D1\t\t\t/* FP Min(%S1, %S2) -> %D1 */'
},

# not commutative operations

"fSubs" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FSUBS %D1, %S1, %S2\t\t\t/* FP Sub(%S1, %S2) -> %D1 */'
},

"fSubd" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FSUBD %D1, %S1, %S2\t\t\t/* FP Sub(%S1, %S2) -> %D1 */'
},

"fMinus" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Minus: Minus(a) = -a",
  "reg_req"   => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. fneg %S1, %D1\t\t\t/* FP Minus(%S1) -> %D1 */'
},

"fAbss" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Absolute value: fAbss(a) = |a|",
  "reg_req"   => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FABSS %D1, %S1\t\t\t/* FP Abss(%S1) -> %D1 */'
},

"fAbsd" => {
  "irn_flags" => "R",
  "comment"   => "construct FP Absolute value: fAbsd(a) = |a|",
  "reg_req"   => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FABSD %D1, %S1\t\t\t/* FP Absd(%S1) -> %D1 */'
},

# other operations

"fConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a FP constant",
  "reg_req"   => { "out" => [ "floating_point" ] },
  "emit"      => '. FMOV %D1, %C\t\t\t/* Mov fConst into register */',
  "cmp_attr"  =>
'    if (attr_a->value == NULL || attr_b->value == NULL)
	    return 1;

	return get_tarval_double(attr_a->value) != get_tarval_double(attr_b->value);'
},

"fConvD2S" => {
  "irn_flags" => "R",
  "comment"   => "convert double to single",
  "reg_req"   => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FCVTSD %D1, %S1\t\t\t/* Convert double to single */',
},

"fConvS2D" => {
  "irn_flags" => "R",
  "comment"   => "convert single to double",
  "reg_req"   => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"      => '. FCVTDS %D1, %S1\t\t\t/* Convert single to double */',
},


# Load / Store

"fLoads" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct FP Load: Load(ptr, mem) = LD ptr",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "floating_point" ] },
  "emit"      => '. FLDS %D1, %S1\t\t\t/* Load((%S1)) -> %D1 */'
#  "emit"      => '. fmov %O(%S1), %D1\t\t\t/* Load((%S1)) -> %D1 */'
},

"fLoadd" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct FP Load: Load(ptr, mem) = LD ptr",
  "reg_req"   => { "in" => [ "general_purpose", "none" ], "out" => [ "floating_point" ] },
  "emit"      => '. FLDD %D1, %S1\t\t\t/* Load((%S1)) -> %D1 */'
#  "emit"      => '. fmov %O(%S1), %D1\t\t\t/* Load((%S1)) -> %D1 */'
},

"fStores" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "floating_point", "none" ] },
  "emit"      => '. FSTS %S2, %S1\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"fStored" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "general_purpose", "floating_point", "none" ] },
  "emit"      => '. FSTD %S2, %S1\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

); # end of %nodes
