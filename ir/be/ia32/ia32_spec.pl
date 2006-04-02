# Creation: 2005/10/19
# $Id$
# This is the specification for the ia32 assembler Firm-operations

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)
$arch = "ia32";

# this string marks the beginning of a comment in emit
$comment_string = "/*";

# the number of additional opcodes you want to register
#$additional_opcodes = 0;

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
#   "attr"      => "attitional attribute arguments for constructor"
#   "init_attr" => "emit attribute initialization template"
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
            { "name" => "gp_NOREG", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
			{ "mode" => "mode_P" }
          ],
  "xmm" => [
            { "name" => "xmm0", "type" => 1 },
            { "name" => "xmm1", "type" => 1 },
            { "name" => "xmm2", "type" => 1 },
            { "name" => "xmm3", "type" => 1 },
            { "name" => "xmm4", "type" => 1 },
            { "name" => "xmm5", "type" => 1 },
            { "name" => "xmm6", "type" => 1 },
            { "name" => "xmm7", "type" => 1 },
            { "name" => "xmm_NOREG", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
			{ "mode" => "mode_D" }
          ],
  "vfp" => [
            { "name" => "vf0", "type" => 1 },
            { "name" => "vf1", "type" => 1 },
            { "name" => "vf2", "type" => 1 },
            { "name" => "vf3", "type" => 1 },
            { "name" => "vf4", "type" => 1 },
            { "name" => "vf5", "type" => 1 },
            { "name" => "vf6", "type" => 1 },
            { "name" => "vf7", "type" => 1 },
            { "name" => "vfp_NOREG", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
			{ "mode" => "mode_E" }
          ],
  "st" => [
            { "name" => "st0", "type" => 1 },
            { "name" => "st1", "type" => 1 },
            { "name" => "st2", "type" => 1 },
            { "name" => "st3", "type" => 1 },
            { "name" => "st4", "type" => 1 },
            { "name" => "st5", "type" => 1 },
            { "name" => "st6", "type" => 1 },
            { "name" => "st7", "type" => 1 },
            { "name" => "st_NOREG", "type" => 6 },  # we need a dummy register for NoReg and Unknown nodes
			{ "mode" => "mode_E" }
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
  "emit"      => '. add %ia32_emit_binop /* Add(%A1, %A2) -> %D1 */',
},

"Mul" => {
  "irn_flags" => "A",
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. imul %ia32_emit_binop /* Mul(%A1, %A2) -> %D1 */'
},

# Mulh is an exception from the 4 INs with AM because the target is always EAX:EDX
"Mulh" => {
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "eax in_r3", "edx in_r4" ] },
  "emit"      => '. imul %ia32_emit_binop /* Mulh(%A1, %A2) -> %D1 */'
},

"And" => {
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, b) = And(b, a) = a AND b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. and %ia32_emit_binop /* And(%A1, %A2) -> %D1 */'
},

"Or" => {
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. or %ia32_emit_binop /* Or(%A1, %A2) -> %D1 */'
},

"Eor" => {
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. xor %ia32_emit_binop /* Xor(%A1, %A2) -> %D1 */'
},

"Max" => {
  "irn_flags" => "R",
  "comment"   => "construct Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      =>
'2. cmp %S1, %S2 /* prepare Max (%S1 - %S2), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
4.  cmovl %D1, %S2 /* %S1 is less %S2 */
  }
  else {
4.  cmovb %D1, %S2 /* %S1 is below %S2 */
  }
'
},

"Min" => {
  "irn_flags" => "R",
  "comment"   => "construct Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      =>
'2. cmp %S1, %S2 /* prepare Min (%S1 - %S2), (%A1, %A2) */
  if (mode_is_signed(get_irn_mode(n))) {
2.  cmovg %D1, %S2 /* %S1 is greater %S2 */
  }
  else {
2.  cmova %D1, %S2, %D1 /* %S1 is above %S2 */
  }
'
},

"CMov" => {
  "irn_flags" => "R",
  "comment"   => "construct Mux: Mux(sel, a, b) == sel ? a : b",
  "reg_req"   => { "in" => [ "gp", "gp", "gp" ], "out" => [ "in_r2" ] },
  "emit"      =>
'. cmp %S1, 0 /* compare Sel for CMov (%A2, %A3) */
. cmovne %D1, %S3 /* sel == true -> return %S3 */
'
},

# not commutative operations

"Sub" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, b) = a - b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. sub %ia32_emit_binop /* Sub(%A1, %A2) -> %D1 */'
},

"DivMod" => {
  "op_flags" => "F|L",
  "state"    => "exc_pinned",
  "reg_req"  => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "eax in_r1", "edx in_r3" ] },
  "emit"     =>
'  if (mode_is_signed(get_irn_mode(n))) {
4.  idiv %S2 /* signed DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
  else {
4.  div %S2 /* unsigned DivMod(%S1, %S2) -> %D1, (%A1, %A2, %A3) */
  }
'
},

"Shl" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, b) = a << b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"      => '. shl %ia32_emit_binop /* Shl(%A1, %A2) -> %D1 */'
},

"Shr" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Shr(a, b) = a >> b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"      => '. shr %ia32_emit_binop /* Shr(%A1, %A2) -> %D1 */'
},

"Shrs" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Shrs(a, b) = a >> b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"      => '. sar %ia32_emit_binop /* Shrs(%A1, %A2) -> %D1 */'
},

"RotR" => {
  "irn_flags" => "R",
  "comment"     => "construct RotR: RotR(a, b) = a ROTR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"     => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"        => '. ror %ia32_emit_binop /* RotR(%A1, %A2) -> %D1 */'
},

"RotL" => {
  "irn_flags" => "R",
  "comment"   => "construct RotL: RotL(a, b) = a ROTL b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "ecx", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"      => '. rol %ia32_emit_binop /* RotL(%A1, %A2) -> %D1 */'
},

# unary operations

"Minus" => {
  "irn_flags" => "R",
  "comment"   => "construct Minus: Minus(a) = -a",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. neg %ia32_emit_unop /* Neg(%A1) -> %D1, (%A1) */'
},

"Inc" => {
  "irn_flags" => "R",
  "comment"   => "construct Increment: Inc(a) = a++",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. inc %ia32_emit_unop /* Inc(%S1) -> %D1, (%A1) */'
},

"Dec" => {
  "irn_flags" => "R",
  "comment"   => "construct Decrement: Dec(a) = a--",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. dec %ia32_emit_unop /* Dec(%S1) -> %D1, (%A1) */'
},

"Not" => {
  "irn_flags" => "R",
  "comment"   => "construct Not: Not(a) = !a",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. not %ia32_emit_unop /* Not(%S1) -> %D1, (%A1) */'
},

# other operations

"CondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ] },
},

"TestJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: TEST A, B && JMPxx LABEL",
  "reg_req"  => { "in" => [ "gp", "gp" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
},

"CJmpAM" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump without CMP (replaces CondJmp): JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "gp", "none" ], "out" => [ "none", "none" ] },
},

"CJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump without CMP (replaces TestJmp): JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp" ] },
},

"SwitchJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct switch",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "none" ] },
},

"Const" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents an integer constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      =>
'  if (get_ia32_Immop_tarval(n) == get_tarval_null(get_irn_mode(n))) {
4.   sub %D1, %D1 /* optimized mov 0 to register */
  }
  else {
    if (get_ia32_op_type(n) == ia32_SymConst) {
6.    mov %D1, OFFSET FLAT:%C /* Move address of SymConst into register */
    }
	else {
6.    mov %D1, %C /* Mov Const into register */
	}
  }
',
},

"Cdq" => {
  "irn_flags" => "R",
  "comment"   => "construct CDQ: sign extend EAX -> EDX:EAX",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "eax in_r1", "edx" ] },
  "emit"      => '. cdq /* sign extend EAX -> EDX:EAX, (%A1) */'
},

# Load / Store

"Load" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "gp" ] },
  "emit"      =>
'  if (get_mode_size_bits(get_ia32_ls_mode(n)) < 32) {
4.   mov%Mx %D1, %ia32_emit_am /* Load((%A1)) -> %D1 */
  }
  else {
4.   mov %D1, %ia32_emit_am /* Load((%A1)) -> %D1 */
  }
'
},

"Store" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "gp", "none" ] },
  "emit"      => '. mov %ia32_emit_binop /* Store(%A3) -> (%A1) */'
},

"Store8Bit" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "construct 8Bit Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "eax ebx ecx edx", "none" ] },
  "emit"      => '. mov %ia32_emit_binop /* Store(%A3) -> (%A1) */'
},

"Lea" => {
  "irn_flags" => "R",
  "comment"   => "construct Lea: Lea(a,b) = lea [a+b*const+offs] | res = a + b * const + offs with const = 0,1,2,4,8",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. lea %D1, %ia32_emit_am /* LEA(%A1, %A2) */'
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

"fAdd" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Add: Add(a, b) = Add(b, a) = a + b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. adds%M %ia32_emit_binop /* SSE Add(%A3, %A4) -> %D1 */'
},

"fMul" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Mul: Mul(a, b) = Mul(b, a) = a * b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. muls%M %ia32_emit_binop /* SSE Mul(%A3, %A4) -> %D1 */'
},

"fMax" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. maxs%M %ia32_emit_binop /* SSE Max(%A3, %A4) -> %D1 */'
},

"fMin" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. mins%M %ia32_emit_binop /* SSE Min(%A3, %A4) -> %D1 */'
},

"fAnd" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE And: And(a, b) = a AND b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. andp%M %ia32_emit_binop /* SSE And(%A3, %A4) -> %D1 */'
},

"fOr" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Or: Or(a, b) = a OR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. orp%M %ia32_emit_binop /* SSE Or(%A3, %A4) -> %D1 */'
},

"fEor" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Eor: Eor(a, b) = a XOR b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. xorp%M %ia32_emit_binop /* SSE Xor(%A3, %A4) -> %D1 */'
},

# not commutative operations

"fSub" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Sub: Sub(a, b) = a - b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3" ] },
  "emit"      => '. subs%M %ia32_emit_binop /* SSE Sub(%A1, %A2) -> %D1 */'
},

"fDiv" => {
  "irn_flags" => "R",
  "comment"   => "construct SSE Div: Div(a, b) = a / b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "in_r3 !in_r4" ] },
  "emit"      => '. divs%M %ia32_emit_binop /* SSE Div(%A1, %A2) -> %D1 */'
},

# other operations

"fCondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: UCOMIS A, B && JMPxx LABEL",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "xmm", "xmm", "none" ], "out" => [ "none", "none" ] },
},

"fConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a SSE constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "xmm" ] },
  "emit"      => '. mov%M %D1, %C /* Load fConst into register */',
},

# Load / Store

"fLoad" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct SSE Load: Load(ptr, mem) = LD ptr",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "xmm" ] },
  "emit"      => '. movs%M %D1, %ia32_emit_am /* Load((%A1)) -> %D1 */'
},

"fStore" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"  => { "in" => [ "gp", "gp", "xmm", "none" ] },
  "emit"     => '. movs%M %ia32_emit_binop /* Store(%S3) -> (%A1) */'
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

# Conversions

"Conv_I2I" => {
  "reg_req"  => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "in_r3", "none" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "comment"  => "construct Conv Int -> Int"
},

"Conv_I2I8Bit" => {
  "reg_req"  => { "in" => [ "gp", "gp", "eax ebx ecx edx", "none" ], "out" => [ "in_r3", "none" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "comment"  => "construct Conv Int -> Int"
},

"Conv_I2FP" => {
  "reg_req"  => { "in" => [ "gp", "gp", "gp", "none" ], "out" => [ "xmm", "none" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "comment"  => "construct Conv Int -> Floating Point"
},

"Conv_FP2I" => {
  "reg_req"  => { "in" => [ "gp", "gp", "xmm", "none" ], "out" => [ "gp", "none" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "comment"  => "construct Conv Floating Point -> Int"
},

"Conv_FP2FP" => {
  "reg_req"  => { "in" => [ "gp", "gp", "xmm", "none" ], "out" => [ "xmm", "none" ] },
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "comment"  => "construct Conv Floating Point -> Floating Point",
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

"vfadd" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Add: Add(a, b) = Add(b, a) = a + b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "vfp", "none" ], "out" => [ "vfp" ] },
},

"vfmul" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Mul: Mul(a, b) = Mul(b, a) = a + b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "vfp", "none" ], "out" => [ "vfp" ] },
},

"vfsub" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Sub: Sub(a, b) = a - b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "vfp", "none" ], "out" => [ "vfp" ] },
},

"vfdiv" => {
  "comment"   => "virtual fp Div: Div(a, b) = a / b",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "vfp", "none" ], "out" => [ "vfp" ] },
},

"vfabs" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Abs: Abs(a) = |a|",
  "reg_req"   => { "in" => [ "vfp"], "out" => [ "vfp" ] },
},

"vfchs" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Chs: Chs(a) = -a",
  "reg_req"   => { "in" => [ "vfp"], "out" => [ "vfp" ] },
},

"vfsin" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Sin: Sin(a) = sin(a)",
  "reg_req"   => { "in" => [ "vfp"], "out" => [ "vfp" ] },
},

"vfcos" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Cos: Cos(a) = cos(a)",
  "reg_req"   => { "in" => [ "vfp"], "out" => [ "vfp" ] },
},

"vfsqrt" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Sqrt: Sqrt(a) = a ^ 0.5",
  "reg_req"   => { "in" => [ "vfp"], "out" => [ "vfp" ] },
},

# virtual Load and Store

"vfld" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "virtual fp Load: Load(ptr, mem) = LD ptr -> reg",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "vfp" ] },
},

"vfst" => {
  "op_flags"  => "L|F",
  "state"     => "exc_pinned",
  "comment"   => "virtual fp Store: Store(ptr, val, mem) = ST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "none" ] },
},

# Conversions

"vfild" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp integer Load: Load(ptr, mem) = iLD ptr -> reg",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ], "out" => [ "vfp" ] },
},

"vfist" => {
  "comment"   => "virtual fp integer Store: Store(ptr, val, mem) = iST ptr,val",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp", "vfp", "none" ] },
},

# constants

"vfldz" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load 0.0: Ld 0.0 -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfld1" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load 1.0: Ld 1.0 -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfldpi" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load pi: Ld pi -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfldln2" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load ln 2: Ld ln 2 -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfldlg2" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load lg 2: Ld lg 2 -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfldl2t" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load ld 10: Ld ld 10 -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfldl2e" => {
  "irn_flags" => "R",
  "comment"   => "virtual fp Load ld e: Ld ld e -> reg",
  "reg_req"   => { "out" => [ "vfp" ] },
},

"vfConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a virtual floating point constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "vfp" ] },
},

#------------------------------------------------------------------------#
#       ___ _____    __ _             _                     _            #
# __  _( _ )___  |  / _| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
# \ \/ / _ \  / /  | |_| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  >  < (_) |/ /   |  _| | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
# /_/\_\___//_/    |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#------------------------------------------------------------------------#

"fadd" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { },
  "emit"      => '. fadd %ia32_emit_x87_binop /* x87 fadd(%A1, %A2) -> %D1 */'
},

"faddp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { },
  "emit"      => '. faddp %ia32_emit_x87_binop /* x87 fadd(%A1, %A2) -> %D1 */'
},

"fmul" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Mul: Mul(a, b) = Mul(b, a) = a + b",
  "reg_req"   => { },
  "emit"      => '. fmul %ia32_emit_x87_binop /* x87 fmul(%A1, %A2) -> %D1 */'
},

"fmulp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Mul: Mul(a, b) = Mul(b, a) = a + b",
  "reg_req"   => { },
  "emit"      => '. fmulp %ia32_emit_x87_binop /* x87 fmul(%A1, %A2) -> %D1 */'
},

"fsub" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Sub: Sub(a, b) = a - b",
  "reg_req"   => { },
  "emit"      => '. fsub %ia32_emit_x87_binop /* x87 fsub(%A1, %A2) -> %D1 */'
},

"fsubp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Sub: Sub(a, b) = a - b",
  "reg_req"   => { },
  "emit"      => '. fsubp %ia32_emit_x87_binop /* x87 fsub(%A1, %A2) -> %D1 */'
},

"fsubr" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "irn_flags" => "R",
  "comment"   => "x87 fp SubR: SubR(a, b) = b - a",
  "reg_req"   => { },
  "emit"      => '. fsubr %ia32_emit_x87_binop /* x87 fsubr(%A1, %A2) -> %D1 */'
},

"fsubrp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "irn_flags" => "R",
  "comment"   => "x87 fp SubR: SubR(a, b) = b - a",
  "reg_req"   => { },
  "emit"      => '. fsubrp %ia32_emit_x87_binop /* x87 fsubr(%A1, %A2) -> %D1 */'
},

"fdiv" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Div: Div(a, b) = a / b",
  "reg_req"   => { },
  "emit"      => '. fdiv %ia32_emit_x87_binop /* x87 fdiv(%A1, %A2) -> %D1 */'
},

"fdivp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Div: Div(a, b) = a / b",
  "reg_req"   => { },
  "emit"      => '. fdivp %ia32_emit_x87_binop /* x87 fdiv(%A1, %A2) -> %D1 */'
},

"fdivr" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp DivR: DivR(a, b) = b / a",
  "reg_req"   => { },
  "emit"      => '. fdivr %ia32_emit_x87_binop /* x87 fdivr(%A1, %A2) -> %D1 */'
},

"fdivrp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp DivR: DivR(a, b) = b / a",
  "reg_req"   => { },
  "emit"      => '. fdivrp %ia32_emit_x87_binop /* x87 fdivr(%A1, %A2) -> %D1 */'
},

"fabs" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Abs: Abs(a) = |a|",
  "reg_req"   => { },
  "emit"      => '. fabs /* x87 fabs(%S1) -> %D1 */'
},

"fchs" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Chs: Chs(a) = -a",
  "reg_req"   => { },
  "emit"      => '. fchs /* x87 fchs(%S1) -> %D1 */'
},

"fsin" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Sin: Sin(a) = sin(a)",
  "reg_req"   => { },
  "emit"      => '. fsin /* x87 sin(%S1) -> %D1 */'
},

"fcos" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Cos: Cos(a) = cos(a)",
  "reg_req"   => { },
  "emit"      => '. fcos /* x87 cos(%S1) -> %D1 */'
},

"fsqrt" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Sqrt: Sqrt(a) = a ^ 0.5",
  "reg_req"   => { },
  "emit"      => '. fsqrt $ /* x87 sqrt(%S1) -> %D1 */'
},

# x87 Load and Store

"fld" => {
  "rd_constructor" => "NONE",
  "op_flags"  => "R|L|F",
  "state"     => "exc_pinned",
  "comment"   => "x87 fp Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { },
  "emit"      => '. fld %ia32_emit_am /* Load((%A1)) -> %D1 */'
},

"fst" => {
  "rd_constructor" => "NONE",
  "op_flags"  => "R|L|F",
  "state"     => "exc_pinned",
  "comment"   => "x87 fp Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { },
  "emit"      => '. fst %ia32_emit_am /* Store(%A3) -> (%A1) */'
},

"fstp" => {
  "rd_constructor" => "NONE",
  "op_flags"  => "R|L|F",
  "state"     => "exc_pinned",
  "comment"   => "x87 fp Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { },
  "emit"      => '. fstp %ia32_emit_am /* Store(%A3) -> (%A1) and pop */'
},

# Conversions

"fild" => {
  "op_flags"  => "R",
  "irn_flags" => "R",
  "comment"   => "x87 fp integer Load: Load(ptr, mem) = iLD ptr -> reg",
  "reg_req"   => { },
  "emit"      => '. fild %ia32_emit_am /* integer Load((%A1)) -> %D1 */'
},

"fist" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp integer Store: Store(ptr, val, mem) = iST ptr,val",
  "reg_req"   => { },
  "emit"      => '. fist %ia32_emit_binop /* integer Store(%A3) -> (%A1) */'
},

"fistp" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp integer Store: Store(ptr, val, mem) = iST ptr,val",
  "reg_req"   => { },
  "emit"      => '. fistp %ia32_emit_binop /* integer Store(%A3) -> (%A1) and pop */'
},

# constants

"fldz" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load 0.0: Ld 0.0 -> reg",
  "reg_req"   => { },
  "emit"      => '. fldz /* x87 0.0 -> %D1 */'
},

"fld1" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load 1.0: Ld 1.0 -> reg",
  "reg_req"   => { },
  "emit"      => '. fld1 /* x87 1.0 -> %D1 */'
},

"fldpi" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load pi: Ld pi -> reg",
  "reg_req"   => { },
  "emit"      => '. fldpi /* x87 pi -> %D1 */'
},

"fldln2" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load ln 2: Ld ln 2 -> reg",
  "reg_req"   => { },
  "emit"      => '. fldln2 /* x87 ln(2) -> %D1 */'
},

"fldlg2" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load lg 2: Ld lg 2 -> reg",
  "reg_req"   => { },
  "emit"      => '. fldlg2 /* x87 log(2) -> %D1 */'
},

"fldl2t" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load ld 10: Ld ld 10 -> reg",
  "reg_req"   => { },
  "emit"      => '. fldll2t /* x87 ld(10) -> %D1 */'
},

"fldl2e" => {
  "op_flags"  => "R",
  "rd_constructor" => "NONE",
  "comment"   => "x87 fp Load ld e: Ld ld e -> reg",
  "reg_req"   => { },
  "emit"      => '. fldl2e /* x87 ld(e) -> %D1 */'
},

"fldConst" => {
  "op_flags"  => "R",
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a x87 constant",
  "cmp_attr"  => "  return ia32_compare_immop_attr(attr_a, attr_b);\n",
  "reg_req"   => { "out" => [ "st" ] },
  "emit"      => '. fld%M %C /* Load fConst into register -> %D1 */',
},

# fxch, fpush
# Note that it is NEVER allowed to do CSE on these nodes

"fxch" => {
  "op_flags"  => "R|K",
  "comment"   => "x87 stack exchange",
  "reg_req"   => { "in" => [ "st"], "out" => [ "st" ] },
  "cmp_attr"  => "  return 1;\n",
  "emit"      => '. fxch %X1 /* x87 swap %X1, %X3 */',
},

"fpush" => {
  "op_flags"  => "R",
  "comment"   => "x87 stack push",
  "reg_req"   => { "in" => [ "st"], "out" => [ "st" ] },
  "cmp_attr"  => "  return 1;\n",
  "emit"      => '. fld %X1 /* x87 push %X1 */',
},

); # end of %nodes
