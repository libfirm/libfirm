# Creation: 2005/10/19
# $Id$
# This is the specification for the ia32 assembler Firm-operations

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "ia32";

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
                         { "name" => "eax", "type" => 2 },
                         { "name" => "edx", "type" => 2 },
                         { "name" => "ebx", "type" => 3 },
                         { "name" => "ecx", "type" => 2 },
                         { "name" => "esi", "type" => 3 },
                         { "name" => "edi", "type" => 3 },
                         { "name" => "ebp", "type" => 3 },
                         { "name" => "esp", "type" => 4 }  # we don't want esp to be assigned
                       ],
  "floating_point"  => [
                         { "name" => "xmm0", "type" => 2 },
                         { "name" => "xmm1", "type" => 2 },
                         { "name" => "xmm2", "type" => 2 },
                         { "name" => "xmm3", "type" => 2 },
                         { "name" => "xmm4", "type" => 2 },
                         { "name" => "xmm5", "type" => 2 },
                         { "name" => "xmm6", "type" => 2 },
                         { "name" => "xmm7", "type" => 2 },
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
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. addl %S2, %D1\t\t\t/* Add(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Add_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. addl %C, %D1\t\t\t/* Add(%C, %S1) -> %D1, (%A1, const) */'
},

"Mul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "comment"     => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "eax in_r1", "edx in_r2" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %S2\t\t\t/* signed Mul(%S1, %S2) -> %D1, (%A1, %A2) */
  }
  else {
4. mull %S2\t\t\t/* unsigned Mul(%S1, %S2) -> %D1, (%A1, %A2) */
  }
'
},

"Mul_i" => {
  "state"       => "pinned",
  "arity"       => 1,
  "comment"     => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "eax in_r1", "edx" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %C\t\t\t/* signed Mul(%C, %S1) -> %D1, (%A1, const) */
  }
  else {
4. mull %C\t\t\t/* unsigned Mul(%C, %S1) -> %D1, (%A1, const) */
  }
'
},

"And" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct And: And(a, b) = And(b, a) = a AND b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. andl %S2, %D1\t\t\t/* And(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"And_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct And: And(a, const) = And(const, a) = a AND const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. andl %C, %D1\t\t\t/* And(%C, %S1) -> %D1, (%A1, const) */'
},

"Or" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. orl %S2, %D1\t\t\t/* Or(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Or_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. orl %C, %D1\t\t\t/* Or(%C, %S1) -> %D1, (%A1, const) */'
},

"Eor" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. xorl %S2, %D1\t\t\t/* Xor(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Eor_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. xorl %C, %D1\t\t\t/* Xor(%C, %S1) -> %D1, (%A1, const) */'
},

"Max" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        =>
'2. cmpl %S2, %S1\t\t\t/* prepare Max (%S1 should be %D1), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
4.  cmovl %S2, %D1\t\t\t/* %S1 is less %S2 */
  }
  else {
4.  cmovb %S2, %D1\t\t\t/* %S1 is below %S2 */
  }
'
},

"Min" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        =>
'2. cmpl %S2, %S1\t\t\t/* prepare Min (%S1 should be %D1), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
2.  cmovg %S2, %D1\t\t\t/* %S1 is greater %S2 */
  }
  else {
2.  cmova %S2, %D1\t\t\t/* %S1 is above %S2 */
  }
'
},

# not commutative operations

"Sub" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, b) = a - b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. subl %S2, %D1\t\t\t/* Sub(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Sub_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, const) = a - const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. subl %C, %D1\t\t\t/* Sub(%S1, %C) -> %D1, (%A1, const) */'
},

"DivMod" => {
  "op_flags"    => "F|L",
  "state"       => "exc_pinned",
  "arity"       => 4,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose", "general_purpose", "none" ], "out" => [ "eax in_r1", "edx in_r3" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4.  idivl %S2\t\t\t/* signed DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
  else {
4.  divl %S2\t\t\t/* unsigned DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
'
},

"Shl" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, b) = a << b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. shll %S2, %D1\t\t\t/* Shl(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shl_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, const) = a << const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. shll %C, %D1\t\t\t/* Shl(%S1, %C) -> %D1, (%A1, const) */'
},

"Shr" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, b) = a >> b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. shrl %S2, %D1\t\t\t/* Shr(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shr_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, const) = a >> const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. shrl %C, %D1\t\t\t/* Shr(%S1, %C) -> %D1, (%A1, const) */'
},

"Shrs" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shrs: Shrs(a, b) = a >> b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. sarl %S2, %D1\t\t\t/* Shrs(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shrs_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shrs: Shrs(a, const) = a >> const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. sarl %C, %D1\t\t\t/* Shrs(%S1, %C) -> %D1, (%A1, const) */'
},

"RotR" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotR: RotR(a, b) = a ROTR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. rorl %S2, %D1\t\t\t/* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"RotL" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, b) = a ROTL b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. roll %S2, %D1\t\t\t/* RotL(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"RotL_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, const) = a ROTL const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. roll %C, %D1\t\t\t/* RotL(%S1, %C) -> %D1, (%A1, const) */'
},

"Minus" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Minus: Minus(a) = -a",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. negl %D1\t\t\t/* Neg(%S1) -> %D1, (%A1) */'
},

"Inc" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Increment: Inc(a) = a++",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. incl %D1\t\t\t/* Inc(%S1) -> %D1, (%A1) */'
},

"Dec" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Decrement: Dec(a) = a--",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. decl %D1\t\t\t/* Dec(%S1) -> %D1, (%A1) */'
},

"Not" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Not: Not(a) = !a",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "emit"        => '. notl %D1\t\t\t/* Not(%S1) -> %D1, (%A1) */'
},

# other operations

"Conv" => {
  "arity"    => 1,
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "in_r1" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"CondJmp" => {
  "op_flags" => "C|L|X|Y",
  "arity"    => 2,
  "comment"  => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "none", "none" ] },
},

"CondJmp_i" => {
  "op_flags" => "L|X|Y",
  "arity"    => 1,
  "comment"  => "construct conditional jump: CMP A, const && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "none", "none" ] },
},

"SwitchJmp" => {
  "op_flags" => "L|X|Y",
  "arity"    => 1,
  "comment"  => "construct switch",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "none" ] },
},

"Const" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents an integer constant",
  "reg_req"  => { "out" => [ "general_purpose" ] },
  "emit"     => '. movl %C, %D1\t\t\t/* Mov Const into register */',
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

"Cltd" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Cltd: sign extend EAX -> EDX:EAX",
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "eax in_r1", "edx" ] },
  "emit"        => '. cltd\t\t\t/* sign extend EAX -> EDX:EAX, (%A1) */'
},

# Load / Store

"Load" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 2,
  "remat"    => 1,
  "comment"  => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"  => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"     => '. movl (%S1), %D1\t\t\t/* Load((%S1)) -> %D1, (%A1) */'
},

"Store" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"     => '. movl %S2, (%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"Lea" => {
  "arity"    => 2,
  "comment"  => "construct Lea: Lea(a,b) = lea offs(a,b,const) | res = a + b * const + offs with const = 0,1,2,4,8",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"     => '. leal %O(%S1, %S2, %C), %D1\t\t/* %D1 = %S1 + %S2 << %C + %O, (%A1, %A2) */'
},

"Lea_i" => {
  "arity"    => 1,
  "comment"  => "construct Lea: Lea(a) = lea offs(a) | res = a + offs",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"     => '. leal %C(%S1), %D1\t\t\t/* %D1 = %S1 + %C, (%A1)*/'
},

"StackParam" => {
  "arity"    => 1,
  "remat"    => 1,
  "comment"  => "constructs a Stack Parameter to retrieve a parameter from Stack",
  "reg_req"  => { "in" => [ "none" ], "out" => [ "general_purpose" ] },
  "cmp_attr" =>
'
  return (attr_a->pn_code != attr_b->pn_code);
'
},

"StackArg" => {
  "arity"    => 2,
  "comment"  => "constructs a Stack Argument to pass an argument on Stack",
  "reg_req"  => { "in" => [ "none", "general_purpose" ], "out" => [ "none" ] },
  "cmp_attr" =>
'
  return (attr_a->pn_code != attr_b->pn_code);
'
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
  "check_inout" => 1,
  "comment"     => "construct SSE Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        => '. add%M %S2, %D1\t\t\t/* SSE Add(%S1, %S2) -> %D1 */'
},

"fMul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "check_inout" => 1,
  "comment"     => "construct SSE Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        =>'. muls%M %S2, %D1\t\t\t/* SSE Mul(%S1, %S2) -> %D1 */'
},

"fMax" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        =>'. maxs%M %S2, %D1\t\t\t/* SSE Max(%S1, %S2) -> %D1 */'
},

"fMin" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        =>'. mins%M %S2, %D1\t\t\t/* SSE Min(%S1, %S2) -> %D1 */'
},

# not commutative operations

"fSub" => {
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Sub: Sub(a, b) = a - b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        => '. subs%M %S2, %D1\t\t\t/* SSE Sub(%S1, %S2) -> %D1 */'
},

"fDiv" => {
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Div: Div(a, b) = a / b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        => '. divs%M %S2, %D1\t\t\t/* SSE Div(%S1, %S2) -> %D1 */'
},

"fMinus" => {
  "arity"       => 1,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Minus: Minus(a) = -a",
  "reg_req"     => { "in" => [ "floating_point" ], "out" => [ "in_r1" ] },
  "emit"        => '. xorp%M c %D1\t\t\t/* SSE Minus(%S1) -> %D1 */'
},

# other operations

"fConv" => {
  "arity"    => 1,
  "reg_req"  => { "in" => [ "floating_point" ], "out" => [ "general_purpose" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"fCondJmp" => {
  "op_flags" => "C|L|X|Y",
  "arity"    => 2,
  "comment"  => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "none", "none" ] },
},

"fConst" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents a SSE constant",
  "reg_req"  => { "out" => [ "floating_point" ] },
  "emit"     => '. mov%M %C, %D1\t\t\t/* Mov fConst into register */',
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
  "comment"  => "construct SSE Load: Load(ptr, mem) = LD ptr",
  "reg_req"  => { "in" => [ "general_purpose", "none" ], "out" => [ "floating_point" ] },
  "emit"     => '. movl (%S1), %D1\t\t\t/* Load((%S1)) -> %D1 */'
},

"fStore" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "floating_point", "none" ] },
  "emit"     => '. movl %S2, (%S1)\t\t\t/* Store(%S2) -> (%S1), (%A1, %A2) */'
},

"fStackParam" => {
  "arity"    => 1,
  "remat"    => 1,
  "comment"  => "constructs a Stack Parameter to retrieve a SSE parameter from Stack",
  "reg_req"  => { "in" => [ "none" ], "out" => [ "floating_point" ] },
  "cmp_attr" =>
'
  return (attr_a->pn_code != attr_b->pn_code);
'
},

"fStackArg" => {
  "arity"    => 2,
  "comment"  => "constructs a Stack Argument to pass an argument on Stack",
  "reg_req"  => { "in" => [ "none", "floating_point" ], "out" => [ "none" ] },
  "cmp_attr" =>
'
  return (attr_a->pn_code != attr_b->pn_code);
'
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

# Return

"Return" => {
  "op_flags" => "L|X",
  "state"    => "pinned",
  "arity"    => "variable",
  "comment"  => "construct Return: Return(...)",
  "args"     => [
                  { "type" => "int",        "name" => "n" },
                  { "type" => "ir_node **", "name" => "in" }
                ],
  "rd_constructor" =>
"  if (!op_ia32_Return) assert(0);
  return new_ir_node(db, irg, block, op_ia32_Return, mode_X, n, in);
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
