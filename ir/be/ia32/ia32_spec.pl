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
#   "args"     => "DEFAULT"
#                 or
#                 [
#                   { "type" => "type 1", "name" => "name 1" },
#                   { "type" => "type 2", "name" => "name 2" },
#                   ...
#                 ],
#   "comment"  => "any comment for constructor",
#   "rd_constructor" => "DEFAULT"
#                       or
#                       "c source code which constructs an ir_node"
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
# arity: arity of the operation
# args:  the arguments of the node constructor (debug, irg and block
#        are always the first 3 arguments and are always autmatically
#        created)
#        or DEFAULT
#        DEFAULT will create the following arguments:
#        for i = 1 .. arity: ir_node *op_i
#        ir_mode *mode
# comment: optional comment for the node constructor
# rd_constructor: for every operation there will be a
#      new_rd_<arch>_<op-name> function with the arguments from above
#      which creates the ir_node corresponding to the defined operation
#      you can either put the complete source code of this function here
#      or use DEFAULT
#      DEFAULT creates the following constructor:
#
#      if (!op_<arch>_<op-name>) assert(0);
#      for i = 1 to arity
#         set in[i] = op_i
#      done
#      res = new_ir_node(dbg, irg, block, op_<arch>_<op-name>, mode, in)
#      res = optimize_node(res)
#      IRN_VRFY_IRG(res, irg)
#      return res
#
# NOTE: You can use DEFAULT if and only if arity is 0,1,2 or 3

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

# arithmetic operations

# commutative operations

"Add" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Add: Add(a, b) = Add(b, a) = a + b",
  "rd_constructor" => "DEFAULT"
},

"Add_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "rd_constructor" => "DEFAULT"
},

"Mul" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "rd_constructor" => "DEFAULT"
},

"Mul_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
  "rd_constructor" => "DEFAULT"
},

"And" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct And: And(a, b) = And(b, a) = a AND b",
  "rd_constructor" => "DEFAULT"
},

"And_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct And: And(a, const) = And(const, a) = a AND const",
  "rd_constructor" => "DEFAULT"
},

"Or" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "rd_constructor" => "DEFAULT"
},

"Or_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "rd_constructor" => "DEFAULT"
},

"Eor" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "rd_constructor" => "DEFAULT"
},

"Eor_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "rd_constructor" => "DEFAULT"
},

# not commutative operations

"Sub" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Sub: Sub(a, b) = a - b",
  "rd_constructor" => "DEFAULT"
},

"Sub_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Sub: Sub(a, const) = a - const",
  "rd_constructor" => "DEFAULT"
},

"Mod" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Mod: Mod(a, b) = a % b",
  "rd_constructor" => "DEFAULT"
},

"Mod_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Mod: Mod(a, const) = a % const",
  "rd_constructor" => "DEFAULT"
},

"Shl" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Shl: Shl(a, b) = a << b",
  "rd_constructor" => "DEFAULT"
},

"Shl_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Shl: Shl(a, const) = a << const",
  "rd_constructor" => "DEFAULT"
},

"Shr" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Shr: Shr(a, b) = a >> b",
  "rd_constructor" => "DEFAULT"
},

"Shr_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Shr: Shr(a, const) = a >> const",
  "rd_constructor" => "DEFAULT"
},

"Shrs" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Shrs: Shrs(a, b) = a >> b",
  "rd_constructor" => "DEFAULT"
},

"Shrs_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Shrs: Shrs(a, const) = a >> const",
  "rd_constructor" => "DEFAULT"
},

"Rot" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Rot: Rot(a, b) = a ROT b",
  "rd_constructor" => "DEFAULT"
},

"Rot_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Rot: Rot(a, const) = a ROT const",
  "rd_constructor" => "DEFAULT"
},

"Minus" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Minus: Minus(a) = -a",
  "rd_constructor" => "DEFAULT"
},

# other operations

"Conv" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Conv: Conv(a) = (conv)a",
  "rd_constructor" => "DEFAULT"
},

"Cmp" => {
  "op_flags" => "C",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Cmp: Cmp(a, b) = a CMP b",
  "rd_constructor" => "DEFAULT"
},

"Cmp_i" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 1,
  "args"     => "DEFAULT",
  "comment"  => "construct Cmp: Cmp(a, const) = Cmp(const, a) = a CMP const",
  "rd_constructor" => "DEFAULT"
},

# Load / Store

"Load" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 2,
  "args"     => "DEFAULT",
  "comment"  => "construct Load: Load(mem-edge, ptr) = LD ptr",
  "rd_constructor" => "DEFAULT"
},

"Store" => {
  "op_flags" => "N",
  "state"    => "pinned",
  "arity"    => 3,
  "args"     => "DEFAULT",
  "comment"  => "construct Store: Store(mem-edge, ptr, val) = ST ptr,val",
  "rd_constructor" => "DEFAULT"
}

); # end of %nodes
