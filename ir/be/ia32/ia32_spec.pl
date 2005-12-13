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
#   "op_flags" => "N|L|C|X|I|F|Y|H|c",
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

%reg_classes = (
  "general_purpose" => [
                         { "name" => "eax", "type" => 0 },
                         { "name" => "ebx", "type" => 0 },
                         { "name" => "ecx", "type" => 0 },
                         { "name" => "edx", "type" => 0 },
                         { "name" => "edi", "type" => 0 },
                         { "name" => "esi", "type" => 0 },
                         { "name" => "ebp", "type" => 0 }
                       ],
  "floating_point"  => [
                         { "name" => "xmm0", "type" => 0 },
                         { "name" => "xmm1", "type" => 0 },
                         { "name" => "xmm2", "type" => 0 },
                         { "name" => "xmm3", "type" => 0 },
                         { "name" => "xmm4", "type" => 0 },
                         { "name" => "xmm5", "type" => 0 },
                         { "name" => "xmm6", "type" => 0 },
                         { "name" => "xmm7", "type" => 0 },
                       ],
  "flag_register"   => [
                         { "name" => "eflags", "type" => 0 }
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
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. addl %s2, %d1\t\t\t/* Add(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Add_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. addl %c, %d1\t\t\t/* Add(%c, %s1) -> %d1, (%a1, const) */'
},

"Mul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "comment"     => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "eax", "general_purpose" ], "out" => [ "eax" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %s2\t\t\t/* signed Mul(%s1, %s2) -> %d1, (%a1, %a2) */
  }
  else {
4. mull %s2\t\t\t/* unsigned Mul(%s1, %s2) -> %d1, (%a1, %a2) */
  }
'
},

"Mul_i" => {
  "state"       => "pinned",
  "arity"       => 1,
  "comment"     => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
  "reg_req"     => { "in" => [ "eax" ], "out" => [ "eax" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %c\t\t\t/* signed Mul(%c, %s1) -> %d1, (%a1, const) */
  }
  else {
4. mull %c\t\t\t/* unsigned Mul(%c, %s1) -> %d1, (%a1, const) */
  }
'
},

"Mulh" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "comment"     => "construct Mulh: Mulh(a, b) = Mulh(b, a) = get_32_highest_bits(a * b)",
  "reg_req"     => { "in" => [ "eax", "general_purpose" ], "out" => [ "edx" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %s2\t\t\t/* signed Mulh(%s1, %s2) -> %d1, (%a1, %a2) */
  }
  else {
4. mull %s2\t\t\t/* unsigned Mulh(%s1, %s2) -> %d1, (%a1, %a2) */
  }
'
},

"Mulh_i" => {
  "state"       => "pinned",
  "arity"       => 1,
  "comment"     => "construct Mulh: Mulh(a, const) = Mulh(const, a) = get_32_highest_bits(a * const)",
  "reg_req"     => { "in" => [ "eax" ], "out" => [ "edx" ] },
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4. imull %c\t\t\t/* signed Mulh(%c, %s1) -> %d1, (%a1, const) */
  }
  else {
4. mull %c\t\t\t/* unsigned Mulh(%c, %s1) -> %d1, (%a1, const) */
  }
'
},

"And" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct And: And(a, b) = And(b, a) = a AND b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. andl %s2, %d1\t\t\t/* And(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"And_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct And: And(a, const) = And(const, a) = a AND const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. andl %c, %d1\t\t\t/* And(%c, %s1) -> %d1, (%a1, const) */'
},

"Or" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. orl %s2, %d1\t\t\t/* Or(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Or_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. orl %c, %d1\t\t\t/* Or(%c, %s1) -> %d1, (%a1, const) */'
},

"Eor" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. xorl %s2, %d1\t\t\t/* Xor(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Eor_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. xorl %c, %d1\t\t\t/* Xor(%c, %s1) -> %d1, (%a1, const) */'
},

"Max" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        =>
'2. cmpl %s2, %s1\t\t\t/* prepare Max (%s1 should be %d1), (%a1, %a2) */
  if (mode_is_signed(get_irn_mode(n))) {
4.  cmovl %s2, %d1\t\t\t/* %s1 is less %s2 */
  }
  else {
4.  cmovb %s2, %d1\t\t\t/* %s1 is below %s2 */
  }
'
},

"Min" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        =>
'2. cmpl %s2, %s1\t\t\t/* prepare Min (%s1 should be %d1), (%a1, %a2) */
  if (mode_is_signed(get_irn_mode(n))) {
2.  cmovg %s2, %d1\t\t\t/* %s1 is greater %s2 */
  }
  else {
2.  cmova %s2, %d1\t\t\t/* %s1 is above %s2 */
  }
'
},

# not commutative operations

"Sub" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, b) = a - b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. subl %s2, %d1\t\t\t/* Sub(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Sub_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Sub: Sub(a, const) = a - const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. subl %c, %d1\t\t\t/* Sub(%s1, %c) -> %d1, (%a1, const) */'
},

"DivMod" => {
  "op_flags"    => "F|L",
  "state"       => "exc_pinned",
  "arity"       => 4,
  "comment"     => "construct DivMod: DivMod(a,b) = (a / b, a % b)",
  "emit"        =>
'  if (mode_is_signed(get_irn_mode(n))) {
4.  idivl %s2\t\t\t/* signed Mod(%s1, %s2) -> %d1, (%a2, %a3, %4) */
  }
  else {
4.  divl %s2\t\t\t/* unsigned Mod(%s1, %s2) -> %d1, (%a2, %a3, %a4) */
  }
',
  "args"     => [
                  { "type" => "ir_node *",        "name" => "dividend" },
                  { "type" => "ir_node *",        "name" => "divisor" },
                  { "type" => "ir_node *",        "name" => "mem" },
                  { "type" => "divmod_flavour_t", "name" => "dm_flav" },   # flavours (flavour_Div, flavour_Mod, flavour_DivMod)
                  { "type" => "ir_mode *",        "name" => "mode" },
                ],
  "rd_constructor" =>
"  ir_node *res;
  ir_node *in[4];
  asmop_attr *attr;

  if (!op_ia32_DivMod) assert(0);

  in[1] = divisor;
  in[3] = mem;

  if (mode_is_signed(mode)) {
    ir_node *cltd;
    /* in signed mode , we need to sign extend the dividend */
    cltd  = new_rd_ia32_Cltd(db, current_ir_graph, block, divisor, mode_T);
    in[0] = new_rd_Proj(db, current_ir_graph, block, cltd, mode_Is, pn_EAX);
    in[2] = new_rd_Proj(db, current_ir_graph, block, cltd, mode_Is, pn_EDX);
  }
  else {
    in[0] = dividend;
    in[2] = new_rd_ia32_Const(db, current_ir_graph, block, mode_Iu);
    set_ia32_Const_type(in[2], asmop_Const);
    set_ia32_Immop_tarval(in[2], get_tarval_null(mode_Iu));
  }

  res = new_ir_node(db, irg, block, op_ia32_DivMod, mode, 4, in);

  set_ia32_DivMod_flavour(res, dm_flav);
  set_ia32_n_res(res, 2);

  attr = get_ia32_attr(res);

  attr->in_req    = calloc(4, sizeof(arch_register_req_t *));
  attr->in_req[0] = &ia32_default_req_ia32_general_purpose_eax;
  attr->in_req[1] = &ia32_default_req_ia32_general_purpose;
  attr->in_req[2] = &ia32_default_req_ia32_general_purpose_edx;
  attr->in_req[3] = &ia32_default_req_none;

  attr->out_req    = calloc(2, sizeof(arch_register_req_t *));
  attr->out_req[0] = &ia32_default_req_ia32_general_purpose_eax;
  attr->out_req[1] = &ia32_default_req_ia32_general_purpose_edx;

  attr->slots = calloc(2, sizeof(arch_register_t *));

  return res;
"
},

"Shl" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, b) = a << b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. shll %s2, %d1\t\t\t/* Shl(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Shl_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shl: Shl(a, const) = a << const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. shll %c, %d1\t\t\t/* Shl(%s1, %c) -> %d1, (%a1, const) */'
},

"Shr" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, b) = a >> b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. shrl %s2, %d1\t\t\t/* Shr(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Shr_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shr: Shr(a, const) = a >> const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. shrl %c, %d1\t\t\t/* Shr(%s1, %c) -> %d1, (%a1, const) */'
},

"Shrs" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct Shrs: Shrs(a, b) = a >> b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. sarl %s2, %d1\t\t\t/* Shrs(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"Shrs_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Shrs: Shrs(a, const) = a >> const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. sarl %c, %d1\t\t\t/* Shrs(%s1, %c) -> %d1, (%a1, const) */'
},

"RotR" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotR: RotR(a, b) = a ROTR b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. rorl %s2, %d1\t\t\t/* RotR(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"RotL" => {
  "arity"       => 2,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, b) = a ROTL b",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. roll %s2, %d1\t\t\t/* RotL(%s1, %s2) -> %d1, (%a1, %a2) */'
},

"RotL_i" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct RotL: RotL(a, const) = a ROTL const",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. roll %c, %d1\t\t\t/* RotL(%s1, %c) -> %d1, (%a1, const) */'
},

"Minus" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Minus: Minus(a) = -a",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. negl %d1\t\t\t/* Neg(%s1) -> %d1, (%a1) */'
},

"Inc" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Increment: Inc(a) = a++",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. incl %d1\t\t\t/* Inc(%s1) -> %d1, (%a1) */'
},

"Dec" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Decrement: Dec(a) = a--",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. decl %d1\t\t\t/* Dec(%s1) -> %d1, (%a1) */'
},

"Not" => {
  "arity"       => 1,
  "remat"       => 1,
  "comment"     => "construct Not: Not(a) = !a",
  "check_inout" => 1,
  "reg_req"     => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "emit"        => '. notl %d1\t\t\t/* Not(%s1) -> %d1, (%a1) */'
},

# other operations

"Conv" => {
  "arity"    => 1,
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"CondJmp" => {
  "op_flags" => "C|L|X|Y",
  "arity"    => 2,
  "comment"  => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "none", "flag_register" ] },
},

"CondJmp_i" => {
  "op_flags" => "L|X|Y",
  "arity"    => 1,
  "comment"  => "construct conditional jump: CMP A, const && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "none", "flag_register" ] },
},

"SwitchJmp" => {
  "op_flags" => "L|X|Y",
  "arity"    => 1,
  "comment"  => "construct switch",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "flag_register" ] },
},

"Const" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents an integer constant",
  "reg_req"  => { "out" => [ "general_purpose" ] },
  "emit"     => '. movl %c, %d1\t\t\t/* Mov Const into register */',
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
  "reg_req"     => { "in" => [ "eax" ], "out" => [ "eax", "edx" ] },
  "emit"        => '. cltd\t\t\t/* sign extend EAX -> EDX:EAX, (%a1) */'
},

# Load / Store

"Load" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 2,
  "remat"    => 1,
  "comment"  => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"  => { "in" => [ "general_purpose", "none" ], "out" => [ "general_purpose" ] },
  "emit"     => '. movl (%s1), %d1\t\t\t/* Load((%s1)) -> %d1, (%a1) */'
},

"Store" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose", "none" ] },
  "emit"     => '. movl %s2, (%s1)\t\t\t/* Store(%s1) -> (%s2), (%a1, %a2) */'
},

"Lea" => {
  "arity"    => 2,
  "comment"  => "construct Lea: Lea(a,b) = lea offs(a,b,const) | res = a + b * const + offs with const = 0,1,2,4,8",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"     => '. leal %o(%s1, %s2, %c), %d1\t\t/* %d1 = %s1 + %s2 << %c + %o, (%a1, %a2) */'
},

"Lea_i" => {
  "arity"    => 1,
  "comment"  => "construct Lea: Lea(a) = lea offs(a) | res = a + offs",
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "general_purpose" ] },
  "emit"     => '. leal %c(%s1), %d1\t\t\t/* %d1 = %s1 + %c, (%a1)*/'
},

"RegParam" => {
  "arity"    => 1,
  "comment"  => "constructs a Register Parameter to cover parameters passed in register",
  "reg_req"  => { "in" => [ "none" ], "out" => [ "none" ] },
  "cmp_attr" =>
'
  return (attr_a->pn_code != attr_b->pn_code);
'
},

"StackParam" => {
  "arity"    => 1,
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
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        => '. add%m %s2, %d1\t\t\t/* SSE Add(%s1, %s2) -> %d1 */'
},

"fMul" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "check_inout" => 1,
  "comment"     => "construct SSE Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        =>'. muls%m %s2, %d1\t\t\t/* SSE Mul(%s1, %s2) -> %d1 */'
},

"fMax" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        =>'. maxs%m %s2, %d1\t\t\t/* SSE Max(%s1, %s2) -> %d1 */'
},

"fMin" => {
  "op_flags"    => "C",
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        =>'. mins%m %s2, %d1\t\t\t/* SSE Min(%s1, %s2) -> %d1 */'
},

# not commutative operations

"fSub" => {
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Sub: Sub(a, b) = a - b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        => '. subs%m %s2, %d1\t\t\t/* SSE Sub(%s1, %s2) -> %d1 */'
},

"fDiv" => {
  "arity"       => 2,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Div: Div(a, b) = a / b",
  "reg_req"     => { "in" => [ "floating_point", "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        => '. divs%m %s2, %d1\t\t\t/* SSE Div(%s1, %s2) -> %d1 */'
},

"fMinus" => {
  "arity"       => 1,
  "remat"       => 1,
  "check_inout" => 1,
  "comment"     => "construct SSE Minus: Minus(a) = -a",
  "reg_req"     => { "in" => [ "floating_point" ], "out" => [ "in_s1" ] },
  "emit"        => '. xorp%m c %d1\t\t\t/* SSE Minus(%s1) -> %d1 */'
},

# other operations

"fConv" => {
  "arity"    => 1,
  "reg_req"  => { "in" => [ "general_purpose" ], "out" => [ "in_s1" ] },
  "comment"  => "construct Conv: Conv(a) = (conv)a"
},

"fCondJmp" => {
  "op_flags" => "C|L|X|Y",
  "arity"    => 2,
  "comment"  => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "reg_req"  => { "in" => [ "general_purpose", "general_purpose" ], "out" => [ "none", "flag_register" ] },
},

"fConst" => {
  "op_flags" => "c",
  "arity"    => "0",
  "remat"    => 1,
  "comment"  => "represents a SSE constant",
  "reg_req"  => { "out" => [ "floating_point" ] },
  "emit"     => '. mov%m %c, %d1\t\t\t/* Mov fConst into register */',
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
  "emit"     => '. movl (%s1), %d1\t\t\t/* Load((%s1)) -> %d1 */'
},

"fStore" => {
  "op_flags" => "L|F",
  "state"    => "exc_pinned",
  "arity"    => 3,
  "remat"    => 1,
  "comment"  => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"  => { "in" => [ "general_purpose", "floating_point", "none" ] },
  "emit"     => '. movl %s2, (%s1)\t\t\t/* Store(%s2) -> (%s1), (%a1, %a2) */'
},

"fStackParam" => {
  "arity"    => 1,
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
  "spill"    => 0,
  "comment"  => "construct Call: Call(...)",
  "args"     => [
                  { "type" => "int",        "name" => "n" },
                  { "type" => "ir_node **", "name" => "in" }
                ],
  "rd_constructor" =>
"  if (!op_ia32_Call) assert(0);
  return new_ir_node(db, irg, block, op_ia32_Call, mode_T, n, in);
"
}

); # end of %nodes
