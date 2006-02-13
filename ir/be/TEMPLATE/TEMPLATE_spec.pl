# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "TEMPLATE";

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   "op_flags" => "N|L|C|X|I|F|Y|H|c|K",
#   "arity"    => "0|1|2|3|variable|dynamic|all",
#   "state"    => "floats|pinned",
#   "args"     => [
#                   { "type" => "type 1", "name" => "name 1" },
#                   { "type" => "type 2", "name" => "name 2" },
#                   ...
#                 ],
#   "comment"  => "any comment for constructor",
#   "rd_constructor" => "c source code which constructs an ir_node"
# },
#
# ... # (all nodes you need to describe)
#
# ); # close the %nodes initializer

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
# op_flags: flags for the operation, OPTIONAL (default is "N")
#
# state: state of the operation, OPTIONAL (default is "pinned")
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
#   1 - write invariant (writes to this register doesn't change it's content)
#   2 - caller save (register must be saved by the caller of a function)
#   3 - callee save (register must be saved by the called function)
#   4 - ignore (do not assign this register)
# NOTE: Make sure to list the registers returning the call-result before all other
#       caller save registers and in the correct order, otherwise it will break
#       the magic!

%reg_classes = (
  "general_purpose" => [
                         { "name" => "r0", "type" => 2 },
                         { "name" => "r1", "type" => 2 },
                         { "name" => "r2", "type" => 3 },
                         { "name" => "r3", "type" => 2 },
                         { "name" => "r4", "type" => 3 },
                         { "name" => "r5", "type" => 3 },
                         { "name" => "r6", "type" => 4 },  # this is our stackpointer
                         { "name" => "r7", "type" => 3 },
                         { "name" => "r8", "type" => 3 },
                         { "name" => "r9", "type" => 3 },
                         { "name" => "r10", "type" => 3 },
                         { "name" => "r11", "type" => 3 },
                         { "name" => "r12", "type" => 3 },
                         { "name" => "r13", "type" => 3 },
                         { "name" => "r14", "type" => 3 },
                         { "name" => "r15", "type" => 3 }
                       ],
  "floating_point"  => [
                         { "name" => "f0", "type" => 2 },
                         { "name" => "f1", "type" => 2 },
                         { "name" => "f2", "type" => 2 },
                         { "name" => "f3", "type" => 2 },
                         { "name" => "f4", "type" => 2 },
                         { "name" => "f5", "type" => 2 },
                         { "name" => "f6", "type" => 2 },
                         { "name" => "f7", "type" => 2 },
                         { "name" => "f8", "type" => 2 },
                         { "name" => "f9", "type" => 2 },
                         { "name" => "f10", "type" => 2 },
                         { "name" => "f11", "type" => 2 },
                         { "name" => "f12", "type" => 2 },
                         { "name" => "f13", "type" => 2 },
                         { "name" => "f14", "type" => 2 },
                         { "name" => "f15", "type" => 2 }
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
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. add %S1, %S2, %D1\t\t\t/* Add(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Add_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. add %S1, %C, %D1\t\t\t/* Add(%C, %S1) -> %D1, (%A1, const) */'
},

"Mul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "comment"     => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        =>'. mul %S1, %S2, %D1\t\t\t/* Mul(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Mul_i" => {
  "state"       => "pinned",
  "arity"       => 1,
  "comment"     => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. mul %S1, %C, %D1\t\t\t/* signed Mul(%C, %S1) -> %D1, (%A1, const) */'
},

"And" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct And: And(a, b) = And(b, a) = a AND b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. and %S1, %S2, %D1\t\t\t/* And(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"And_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct And: And(a, const) = And(const, a) = a AND const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. and %S1, %C, %D1\t\t\t/* And(%C, %S1) -> %D1, (%A1, const) */'
},

"Or" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. or %S1, %S2, %D1\t\t\t/* Or(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Or_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. or %S1, %C, %D1\t\t\t/* Or(%C, %S1) -> %D1, (%A1, const) */'
},

"Eor" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. xor %S1, %S2, %D1\t\t\t/* Xor(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Eor_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. xor %S1, %C, %D1\t\t\t/* Xor(%C, %S1) -> %D1, (%A1, const) */'
},

# not commutative operations

"Sub" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, b) = a - b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. sub %S1, %S2, %D1\t\t\t/* Sub(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Sub_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, const) = a - const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. subl %S1, %C, %D1\t\t\t/* Sub(%S1, %C) -> %D1, (%A1, const) */'
},

"Shl" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, b) = a << b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. shl %S1, %S2, %D1\t\t\t/* Shl(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shl_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, const) = a << const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. shl %S1, %C, %D1\t\t\t/* Shl(%S1, %C) -> %D1, (%A1, const) */'
},

"Shr" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, b) = a >> b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. shr %S2, %D1\t\t\t/* Shr(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shr_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, const) = a >> const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. shr %S1, %C, %D1\t\t\t/* Shr(%S1, %C) -> %D1, (%A1, const) */'
},

"RotR" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotR: RotR(a, b) = a ROTR b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. ror %S1, %S2, %D1\t\t\t/* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"RotL" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, b) = a ROTL b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. rol %S1, %S2, %D1\t\t\t/* RotL(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"RotL_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, const) = a ROTL const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. rol %S1, %C, %D1\t\t\t/* RotL(%S1, %C) -> %D1, (%A1, const) */'
},

"Minus" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Minus: Minus(a) = -a",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. neg %S1, %D1\t\t\t/* Neg(%S1) -> %D1, (%A1) */'
},

"Inc" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Increment: Inc(a) = a++",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. inc %S1, %D1\t\t\t/* Inc(%S1) -> %D1, (%A1) */'
},

"Dec" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Decrement: Dec(a) = a--",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. dec %S1, %D1\t\t\t/* Dec(%S1) -> %D1, (%A1) */'
},

"Not" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Not: Not(a) = !a",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"        => '. not %S1, %D1\t\t\t/* Not(%S1) -> %D1, (%A1) */'
},

# other operations

"Const" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents an integer constant",
  "reg_req"  => { "out" => [ "general_purpose" ] },
  "emit"     => '. mov %C, %D1\t\t\t/* Mov Const into register */',
  "cmp_attr" =>
'
  if (attr_a->tp == attr_b->tp) {
    if (attr_a->tp == asmop_SymConst) {
      if (attr_a->old_ir == NULL || attr_b->old_ir == NULL)
        return 1;
      else
        return strcmp(get_sc_name(attr_a->old_ir), get_sc_name(attr_b->old_ir));
    }
    else {
      if (attr_a->old_ir == NULL || attr_b->old_ir == NULL)
        return 1;

      if (tarval_cmp(attr_a->tv, attr_b->tv) == pn_Cmp_Eq)
        return 0;
      else
        return 1;
    }
  }
  else
    return 1;
'
},

# Load / Store

"Load" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 2,
  "remat"    => 1,
  "comment"  => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"  => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"     => '. mov %O(%S1), %D1\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Store" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"     => '. movl %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
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
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct FP Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        => '. fadd %S1, %S2, %D1\t\t\t/* FP Add(%S1, %S2) -> %D1 */'
},

"fMul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "comment"     => "construct FP Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        =>'. fmul %S1, %S2, %D1\t\t\t/* FP Mul(%S1, %S2) -> %D1 */'
},

"fMax" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct FP Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        =>'. fmax %S1, %S2, %D1\t\t\t/* FP Max(%S1, %S2) -> %D1 */'
},

"fMin" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct FP Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        =>'. fmin %S1, %S2, %D1\t\t\t/* FP Min(%S1, %S2) -> %D1 */'
},

# not commutative operations

"fSub" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct FP Sub: Sub(a, b) = a - b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        => '. fsub %S1, %S2, %D1\t\t\t/* FP Sub(%S1, %S2) -> %D1 */'
},

"fDiv" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct FP Div: Div(a, b) = a / b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        => '. fdiv %S1, %S2, %D1\t\t\t/* FP Div(%S1, %S2) -> %D1 */'
},

"fMinus" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct FP Minus: Minus(a) = -a",
  "reg_req"     => { "in" => [ "floating_point" ], "out" => [ "floating_point" ] },
  "emit"        => '. fneg %S1, %D1\t\t\t/* FP Minus(%S1) -> %D1 */'
},

# other operations

"fConst" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents a FP constant",
  "reg_req"  => { "out" => [ "floating_point" ] },
  "emit"     => '. fmov %C, %D1\t\t\t/* Mov fConst into register */',
  "cmp_attr" =>
'
  if (attr_a->tp == attr_b->tp) {
    if (attr_a->tp == asmop_SymConst) {
      if (attr_a->old_ir == NULL || attr_b->old_ir == NULL)
        return 1;
      else
        return strcmp(get_sc_name(attr_a->old_ir), get_sc_name(attr_b->old_ir));
    }
    else {
      if (attr_a->old_ir == NULL || attr_b->old_ir == NULL)
        return 1;

      if (tarval_cmp(attr_a->tv, attr_b->tv) == pn_Cmp_Eq)
        return 0;
      else
        return 1;
    }
  }
  else
    return 1;
'
},

# Load / Store

"fLoad" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 2,
  "remat"    => 1,
  "comment"  => "construct FP Load: Load(ptr, mem) = LD ptr",
  "reg_req"  => { "in" => [ "general_purpose", "none" ], "out" => [ "floating_point" ] },
  "emit"     => '. fmov %O(%S1), %D1\t\t\t/* Load((%S1)) -> %D1 */'
},

"fStore" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "floating_point", "none" ] },
  "emit"     => '. fmov %S2, %O(%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

# Call

"Call" => {
  "op_flags" => "L|F",
  "state"    => "mem_pinned",
  "arity"    => "variable",
  "comment"  => "construct Call: Call(...)",
  "args"     => [
                  { "type" => "int",        "name" => "n" },
                  { "type" => "ir_node **", "name" => "in" }
                ],
  "rd_constructor" =>
"  if (!op_ia32_Call) assert(0);
  return new_ir_node(db, irg, block, op_ia32_Call, mode_T, n, in);
"
},

# M/Alloc

"Alloca" => {
  "op_flags" => "L|F",
  "state"    => "pinned",
  "arity"    => "2",
  "comment"  => "construct Alloca: allocate memory on Stack",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] }
},

"Alloca_i" => {
  "op_flags" => "L|F",
  "state"    => "pinned",
  "arity"    => "1",
  "comment"  => "construct Alloca: allocate memory on Stack",
  "reg_req"  => { "out" => [ "general_purpose" ] }
}

); # end of %nodes
