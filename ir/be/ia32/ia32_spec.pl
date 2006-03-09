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
  "gp" => [
            { "name" => "eax", "type" => 1 },
            { "name" => "edx", "type" => 1 },
            { "name" => "ebx", "type" => 2 },
            { "name" => "ecx", "type" => 1 },
            { "name" => "esi", "type" => 2 },
            { "name" => "edi", "type" => 2 },
            { "name" => "ebp", "type" => 2 },
            { "name" => "esp", "type" => 4 },
            { "name" => "xxx", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
			{ "mode" => "mode_P" }
          ],
  "fp" => [
            { "name" => "xmm0", "type" => 1 },
            { "name" => "xmm1", "type" => 1 },
            { "name" => "xmm2", "type" => 1 },
            { "name" => "xmm3", "type" => 1 },
            { "name" => "xmm4", "type" => 1 },
            { "name" => "xmm5", "type" => 1 },
            { "name" => "xmm6", "type" => 1 },
            { "name" => "xmm7", "type" => 1 },
            { "name" => "xxxx", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
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

# NOTE:
# All nodes supporting Addressmode have 5 INs:
# 1 - base    r1 == NoReg in case of no AM or no base
# 2 - index   r2 == NoReg in case of no AM or no index
# 3 - op1     r3 == always present
# 4 - op2     r4 == NoReg in case of immediate operation
# 5 - mem     NoMem in case of no AM otherwise it takes the mem from the Load

"Add" => {
  "irn_flags" => "R",
  "comment"   => "construct Add: Add(a, b) = Add(b, a) = a + b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. add %ia32_emit_binop\t\t\t/* Add(%A1, %A2) -> %D1 */'
},

"Mul" => {
  "irn_flags" => "A",
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. imul %ia32_emit_binop\t\t\t/* Mul(%A1, %A2) -> %D1 */'
},

# Mulh is an exception from the 4 INs with AM because the target is always EAX:EDX
"Mulh" => {
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "eax in_r3", "edx in_r4" ] },
  "emit"      => '. imul %ia32_emit_unop\t\t\t/* Mulh(%A1, %A2) -> %D1 */ '
},

"And" => {
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, b) = And(b, a) = a AND b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. and %ia32_emit_binop\t\t\t/* And(%A1, %A2) -> %D1 */'
},

"Or" => {
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. or %ia32_emit_binop\t\t\t/* Or(%A1, %A2) -> %D1 */'
},

"Eor" => {
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. xor %ia32_emit_binop\t\t\t/* Xor(%A1, %A2) -> %D1 */'
},

"Max" => {
  "irn_flags" => "R",
  "comment"   => "construct Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      =>
'2. cmp %S1, %S2\t\t\t/* prepare Max (%S1 - %S2), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
4.  cmovl %D1, %S2\t\t\t/* %S1 is less %S2 */
  }
  else {
4.  cmovb %D1, %S2\t\t\t/* %S1 is below %S2 */
  }
'
},

"Min" => {
  "irn_flags" => "R",
  "comment"   => "construct Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      =>
'2. cmp %S1, %S2\t\t\t/* prepare Min (%S1 - %S2), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
2.  cmovg %D1, %S2\t\t\t/* %S1 is greater %S2 */
  }
  else {
2.  cmova %D1, %S2, %D1\t\t\t/* %S1 is above %S2 */
  }
'
},

"CMov" => {
  "irn_flags" => "R",
  "comment"   => "construct Mux: Mux(sel, a, b) == sel ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp", "gp" ], "out" => [ "in_r2" ] },
  "emit"      =>
'. cmp %S1, 0\t\t\t/* compare Sel for CMov (%A2, %A3) */
. cmovne %D1, %S3\t\t\t/* sel == true -> return %S3 */
'
},

# not commutative operations

"Sub" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, b) = a - b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. sub %ia32_emit_binop\t\t\t/* Sub(%A1, %A2) -> %D1 */'
},

"DivMod" => {
  "op_flags" => "F|L",
  "state"    => "exc_pinned",
  "reg_req"  => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "eax in_r1", "edx in_r3" ] },
  "emit"     =>
'  if (mode_is_signed(get_irn_mode(n))) {
4.  idiv %S2\t\t\t/* signed DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
  else {
4.  div %S2\t\t\t/* unsigned DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
'
},

"Shl" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, b) = a << b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. shl %ia32_emit_binop\t\t\t/* Shl(%A1, %A2) -> %D1 */'
},

"Shr" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Shr(a, b) = a >> b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. shr %ia32_emit_binop\t\t\t/* Shr(%A1, %A2) -> %D1 */'
},

"Shrs" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Shrs(a, b) = a >> b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. sar %ia32_emit_binop\t\t\t/* Shrs(%A1, %A2) -> %D1 */'
},

"RotR" => {
  "irn_flags" => "R",
  "comment"     => "construct RotR: RotR(a, b) = a ROTR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"     => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3" ] },
  "emit"        => '. ror %ia32_emit_binop\t\t\t/* RotR(%A1, %A2) -> %D1 */'
},

"RotL" => {
  "irn_flags" => "R",
  "comment"   => "construct RotL: RotL(a, b) = a ROTL b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. rol %ia32_emit_binop\t\t\t/* RotL(%A1, %A2) -> %D1 */'
},

# unary operations

"Minus" => {
  "irn_flags" => "R",
  "comment"   => "construct Minus: Minus(a) = -a",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. neg %ia32_emit_unop\t\t\t/* Neg(%A1) -> %D1, (%A1) */'
},

"Inc" => {
  "irn_flags" => "R",
  "comment"   => "construct Increment: Inc(a) = a++",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. inc %ia32_emit_unop\t\t\t/* Inc(%S1) -> %D1, (%A1) */'
},

"Dec" => {
  "irn_flags" => "R",
  "comment"   => "construct Decrement: Dec(a) = a--",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. dec %ia32_emit_unop\t\t\t/* Dec(%S1) -> %D1, (%A1) */'
},

"Not" => {
  "irn_flags" => "R",
  "comment"   => "construct Not: Not(a) = !a",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. not %ia32_emit_unop\t\t\t/* Not(%S1) -> %D1, (%A1) */'
},

# other operations

"Conv" => {
  "reg_req"  => { "in" => [ "gp" ], "out" => [ "in_r1" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"CondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "none", "none" ] },
},

"SwitchJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct switch",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "none" ] },
},

"Const" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents an integer constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. mov %D1, %C\t\t\t/* Mov Const into register */',
},

"Cdq" => {
  "irn_flags" => "R",
  "comment"   => "construct CDQ: sign extend EAX -> EDX:EAX",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "eax in_r1", "edx" ] },
  "emit"      => '. cdq\t\t\t/* sign extend EAX -> EDX:EAX, (%A1) */'
},

# Load / Store

"Load" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "gp" ] },
  "emit"      => '. mov %D1, %ia32_emit_am\t\t\t/* Load((%A1)) -> %D1 */'
},

"Store" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ] },
  "emit"      => '. mov %ia32_emit_binop\t\t\t/* Store(%A3) -> (%A1) */'
},

"Lea" => {
  "irn_flags" => "R",
  "comment"   => "construct Lea: Lea(a,b) = lea [a+b*const+offs] | res = a + b * const + offs with const = 0,1,2,4,8",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. lea %D1, %ia32_emit_am\t\t/* %D1 = %S1 + %S2 << scale + %O, (%A1, %A2) */'
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
  "irn_flags" => "R",
  "comment"   => "construct SSE Add: Add(a, b) = Add(b, a) = a + b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. adds%M %ia32_emit_binop\t\t\t/* SSE Add(%A1, %A2) -> %D1 */'
},

"fMul" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. muls%M %ia32_emit_binop\t\t\t/* SSE Mul(%A1, %A2) -> %D1 */'
},

"fMax" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. maxs%M %ia32_emit_binop\t\t\t/* SSE Max(%A1, %A2) -> %D1 */'
},

"fMin" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. mins%M %ia32_emit_binop\t\t\t/* SSE Min(%A1, %A2) -> %D1 */'
},

"fAnd" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE And: And(a, b) = a AND b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. andp%M %ia32_emit_binop\t\t\t/* SSE And(%A3, %A4) -> %D1 */'
},

"fOr" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Or: Or(a, b) = a OR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. orp%M %ia32_emit_binop\t\t\t/* SSE Or(%A3, %A4) -> %D1 */'
},

"fEor" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Eor: Eor(a, b) = a XOR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. xorp%M %ia32_emit_binop\t\t\t/* SSE Xor(%A3, %A4) -> %D1 */'
},

# not commutative operations

"fSub" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Sub: Sub(a, b) = a - b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. subs%M %ia32_emit_binop\t\t\t/* SSE Sub(%A1, %A2) -> %D1 */'
},

"fDiv" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Div: Div(a, b) = a / b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. divs%M %ia32_emit_binop\t\t\t/* SSE Div(%A1, %A2) -> %D1 */'
},

# other operations

"fConv" => {
  "reg_req"  => { "in" => [ "fp" ], "out" => [ "gp" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"fCondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: UCOMIS A, B && JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "fp", "fp", "none" ], "out" => [ "none", "none" ] },
},

"fConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a SSE constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "fp" ] },
  "emit"      => '. mov%M %D1, %C\t\t\t/* Load fConst into register */',
},

# Load / Store

"fLoad" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct SSE Load: Load(ptr, mem) = LD ptr",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "fp" ] },
  "emit"      => '. movs%M %D1, %ia32_emit_am\t\t\t/* Load((%A1)) -> %D1 */'
},

"fStore" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"  => { "in" => [ "gp", "gp", "fp", "none" ] },
  "emit"     => '. movs%M %ia32_emit_am, %S3\t\t\t/* Store(%S3) -> (%A1) */'
},

# CopyB

"CopyB" => {
  "op_flags" => "F|H",
  "state"    => "pinned",
  "comment"  => "implements a memcopy: CopyB(dst, src, size, mem) == memcpy(dst, src, size)",
  "reg_req"  => { "in" => [ "edi", "esi", "ecx", "none" ], "out" => [ "none" ] },
},

"CopyB_i" => {
  "op_flags" => "F|H",
  "state"    => "pinned",
  "comment"  => "implements a memcopy: CopyB(dst, src, mem) == memcpy(dst, src, attr(size))",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"  => { "in" => [ "edi", "esi", "none" ], "out" => [ "none" ] },
},

); # end of %nodes
