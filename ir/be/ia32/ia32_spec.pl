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
#      res = new_ir_node(dbg, irg, block, op_<arch>_<op-name>, mode, in)
#      res = optimize_node(res)
#      IRN_VRFY_IRG(res, irg)
#      return res
#
# NOTE: rd_constructor and args are only optional if and only if arity is 0,1,2 or 3

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
  "arity"    => 2,
  "comment"  => "construct Add: Add(a, b) = Add(b, a) = a + b",
},

"Add_i" => {
  "arity"    => 1,
  "comment"  => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "rd_constructor" => "DEFAULT"
},

"Mul" => {
  "op_flags" => "C",
  "arity"    => 2,
  "comment"  => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
},

"Mul_i" => {
  "state"    => "pinned",
  "arity"    => 1,
  "comment"  => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
},

"And" => {
  "op_flags" => "C",
  "arity"    => 2,
  "comment"  => "construct And: And(a, b) = And(b, a) = a AND b",
},

"And_i" => {
  "arity"    => 1,
  "comment"  => "construct And: And(a, const) = And(const, a) = a AND const",
},

"Or" => {
  "op_flags" => "C",
  "arity"    => 2,
  "comment"  => "construct Or: Or(a, b) = Or(b, a) = a OR b",
},

"Or_i" => {
  "arity"    => 1,
  "comment"  => "construct Or: Or(a, const) = Or(const, a) = a OR const",
},

"Eor" => {
  "op_flags" => "C",
  "arity"    => 2,
  "comment"  => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
},

"Eor_i" => {
  "arity"    => 1,
  "comment"  => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
},

# not commutative operations

"Sub" => {
  "arity"    => 2,
  "comment"  => "construct Sub: Sub(a, b) = a - b",
},

"Sub_i" => {
  "arity"    => 1,
  "comment"  => "construct Sub: Sub(a, const) = a - const",
},

"Mod" => {
  "arity"    => 2,
  "comment"  => "construct Mod: Mod(a, b) = a % b",
},

"Mod_i" => {
  "arity"    => 1,
  "comment"  => "construct Mod: Mod(a, const) = a % const",
},

"Shl" => {
  "arity"    => 2,
  "comment"  => "construct Shl: Shl(a, b) = a << b",
},

"Shl_i" => {
  "arity"    => 1,
  "comment"  => "construct Shl: Shl(a, const) = a << const",
},

"Shr" => {
  "arity"    => 2,
  "comment"  => "construct Shr: Shr(a, b) = a >> b",
},

"Shr_i" => {
  "arity"    => 1,
  "comment"  => "construct Shr: Shr(a, const) = a >> const",
},

"Shrs" => {
  "arity"    => 2,
  "comment"  => "construct Shrs: Shrs(a, b) = a >> b",
},

"Shrs_i" => {
  "arity"    => 1,
  "comment"  => "construct Shrs: Shrs(a, const) = a >> const",
},

"Rot" => {
  "arity"    => 2,
  "comment"  => "construct Rot: Rot(a, b) = a ROT b",
},

"Rot_i" => {
  "arity"    => 1,
  "comment"  => "construct Rot: Rot(a, const) = a ROT const",
},

"Minus" => {
  "arity"    => 1,
  "comment"  => "construct Minus: Minus(a) = -a",
},

"Inc" => {
  "arity"    => 1,
  "comment"  => "construct Increment: Inc(a) = a++",
},

"Dec" => {
  "arity"    => 1,
  "comment"  => "construct Decrement: Dec(a) = a--",
},

# other operations

"Conv" => {
  "arity"    => 1,
  "comment"  => "construct Conv: Conv(a) = (conv)a",
},

"Cmp" => {
  "op_flags" => "C",
  "arity"    => 2,
  "comment"  => "construct Cmp: Cmp(a, b) = a CMP b",
},

"Cmp_i" => {
  "arity"    => 1,
  "comment"  => "construct Cmp: Cmp(a, const) = Cmp(const, a) = a CMP const",
},

# Load / Store

"Load" => {
  "arity"    => 2,
  "comment"  => "construct Load: Load(mem-edge, ptr) = LD ptr",
},

"Store" => {
  "arity"    => 3,
  "comment"  => "construct Store: Store(mem-edge, ptr, val) = ST ptr,val",
},

"Lea" => {
  "arity"    => 2,
  "comment"  => "construct Lea: Lea(a,b) = lea offs(a,b,const) | res = a + b * const + offs with const = 0,1,2,4,8",
},

"Lea_i" => {
  "arity"    => 1,
  "comment"  => "construct Lea: Lea(a) = lea offs(a) | res = a + offs",
}

); # end of %nodes
