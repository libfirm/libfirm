# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "arm";

# this strings mark the beginning and the end of a comment in emit
$comment_string     = "/*";
$comment_string_end = "*/";

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
                         { "name" => "sp", "realname" => "r13", "type" => 6 }, # this is our stack pointer
                         { "name" => "lr", "realname" => "r14", "type" => 3 }, # this is our return address
                         { "name" => "pc", "realname" => "r15", "type" => 6 }, # this is our program counter
                         { "mode" => "mode_Iu" }
                       ],
  "fpa"  => [
                         { "name" => "f0", "type" => 1 },
                         { "name" => "f1", "type" => 1 },
                         { "name" => "f2", "type" => 1 },
                         { "name" => "f3", "type" => 1 },
                         { "name" => "f4", "type" => 2 },
                         { "name" => "f5", "type" => 2 },
                         { "name" => "f6", "type" => 2 },
                         { "name" => "f7", "type" => 2 },
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

"Add" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Add: Add(a, b) = Add(b, a) = a + b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. add %D1, %S1, %S2%X0 /* Add(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Add_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Add: Add(a, const) = Add(const, a) = a + const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. add %D1, %S1, %C /* Add(%C, %S1) -> %D1, (%A1, const) */'
},

"Mul" => {
  #"op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "!in_r1" ] },
  "emit"      =>'. mul %D1, %S1, %S2 /* Mul(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Smull" => {
  #"op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct signed 64bit Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp", "gp" ] },
  "emit"      =>'. smull %D1, %D2, %S1, %S2 /* SMull(%S1, %S2) -> %D1, %D2, (%A1, %A2) */',
  "outs"      => [ "low", "high" ],
},

"Umull" => {
  #"op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct unsigned 64bit Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp", "gp" ] },
  "emit"      =>'. umull %D1, %D2, %S1, %S2 /* UMull(%S1, %S2) -> %D1, %D2, (%A1, %A2) */',
  "outs"      => [ "low", "high" ],
},

"Mla" => {
  #"op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Mla: Mla(a, b, c) = a * b + c",
  "reg_req"   => { "in" => [ "gp", "gp", "gp" ], "out" => [ "!in_r1" ] },
  "emit"      =>'. mla %D1, %S1, %S2, %S3 /* Mla(%S1, %S2, %S3) -> %D1, (%A1, %A2, %A3) */'
},

"And" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, b) = And(b, a) = a AND b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. and %D1, %S1, %S2%X0 /* And(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"And_i" => {
  "irn_flags" => "R",
  "comment"   => "construct And: And(a, const) = And(const, a) = a AND const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. and %D1, %S1, %C /* And(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;'
},

"Or" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. orr %D1, %S1, %S2%X0 /* Or(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Or_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Or: Or(a, const) = Or(const, a) = a OR const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
  "emit"      => '. orr %D1, %S1, %C /* Or(%C, %S1) -> %D1, (%A1, const) */'
},

"Eor" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. eor %D1, %S1, %S2%X0 /* Xor(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Eor_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
  "emit"      => '. eor %D1, %S1, %C /* Xor(%C, %S1) -> %D1, (%A1, const) */'
},

# not commutative operations

"Bic" => {
  "irn_flags" => "R",
  "comment"   => "construct Bic: Bic(a, b) = a AND ~b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. bic %D1, %S1, %S2%X0 /* AndNot(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Bic_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Bic: Bic(a, const) = a AND ~const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. bic %D1, %S1, %C /* AndNot(%C, %S1) -> %D1, (%A1, const) */',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;'
},

"Sub" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, b) = a - b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. sub %D1, %S1, %S2%X0 /* Sub(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Sub_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Sub: Sub(a, const) = a - const",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. sub %D1, %S1, %C /* Sub(%S1, %C) -> %D1, (%A1, const) */',
},

"Rsb" => {
  "irn_flags" => "R",
  "comment"   => "construct Rsb: Rsb(a, b) = b - a",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. rsb %D1, %S1, %S2%X0 /* Rsb(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Rsb_i" => {
  "irn_flags" => "R",
  "comment"   => "construct Rsb: Rsb(a, const) = const - a",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. rsb %D1, %S1, %C /* Rsb(%S1, %C) -> %D1, (%A1, const) */',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;'
},

"Shl" => {
  "irn_flags" => "R",
  "comment"   => "construct Shl: Shl(a, b) = a << b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mov %D1, %S1, lsl %S2 /* Shl(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shr" => {
  "irn_flags" => "R",
  "comment"   => "construct Shr: Shr(a, b) = a >> b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      => '. mov %D1, %S1, lsr %S2 /* Shr(%S1, %S2) -> %D1, (%A1, %A2) */'
},

"Shrs" => {
  "irn_flags" => "R",
  "comment"   => "construct Shrs: Shrs(a, b) = a >> b",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "in_r1" ] },
  "emit"      => '. mov %D1, %S1, asr %S2 /* Shrs(%S1, %S2) -> %D1, (%A1, %A2) */'
},

#"RotR" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotR: RotR(a, b) = a ROTR b",
#  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. mov %D1, %S1, ror %S2 /* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
##  "emit"      => '. ror %S1, %S2, %D1 /* RotR(%S1, %S2) -> %D1, (%A1, %A2) */'
#},

#"RotL" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotL: RotL(a, b) = a ROTL b",
#  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. rol %S1, %S2, %D1 /* RotL(%S1, %S2) -> %D1, (%A1, %A2) */'
#},

#"RotL_i" => {
#  "irn_flags" => "R",
#  "comment"   => "construct RotL: RotL(a, const) = a ROTL const",
#  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
#  "emit"      => '. rol %S1, %C, %D1 /* RotL(%S1, %C) -> %D1, (%A1, const) */'
#},

"Mov" => {
  "irn_flags" => "R",
  "comment"   => "construct Mov: a = b",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mov %D1, %S1%X0 /* Mov(%S1%X0) -> %D1, (%A1) */'
},

"Mov_i" => {
  "irn_flags" => "R",
  "comment"   => "represents an integer constant",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. mov %D1, %C /* Mov Const into register */',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;'
},

"Mvn" => {
  "irn_flags" => "R",
  "comment"   => "construct Not: Not(a) = !a",
  "attr"      => "arm_shift_modifier mod, tarval *shf",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, mod); attr->value = shf;',
  "cmp_attr"  => 'return (attr_a->instr_fl != attr_b->instr_fl) || (attr_a->value != attr_b->value);',
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      => '. mvn %D1, %S1%X0 /* ~(%S1%X0) -> %D1, (%A1) */'
},

"Mvn_i" => {
  "irn_flags" => "R",
  "comment"   => "represents a negated integer constant",
  "attr"      => "tarval *tv",
  "init_attr" => 'ARM_SET_SHF_MOD(attr, ARM_SHF_IMM); attr->value = tv;',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
  "reg_req"   => { "out" => [ "gp" ] },
  "emit"      => '. mvn %D1, %C /* Mov ~Const into register */',
},

"Abs" => {
  "irn_flags" => "R",
  "comment"   => "construct Abs: Abs(a) = |a|",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "gp" ] },
  "emit"      =>
'. movs %S1, %S1, #0 /* set condition flag */\n
. rsbmi %D1, %S1, #0 /* Neg(%S1) -> %D1, (%A1) */'
},

# other operations

"EmptyReg" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"  => "allocate an empty register for calculations",
  "reg_req"  => { "out" => [ "gp" ] },
  "emit"      => '. /* %D1 now available for calculations */',
  "cmp_attr"  => 'return 1;'
},

"Copy" => {
  "comment"  => "implements a register copy",
  "reg_req"  => { "in" => [ "gp" ], "out" => [ "gp" ] },
},

"CopyB" => {
  "op_flags" => "F|H",
  "state"    => "pinned",
  "comment"  => "implements a memcopy: CopyB(dst, src, size, mem) == memcpy(dst, src, size)",
  "reg_req"  => { "in" => [ "!sp", "!sp", "gp", "gp", "gp", "none" ], "out" => [ "none" ] },
},

"SymConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a symbolic constant",
  "attr"      => "const char *label",
  "init_attr" => '  attr->symconst_label = label;',
  "reg_req"   => { "out" => [ "gp" ] },
#  "emit"      => '. ldr %D1, %C /* Mov Const into register */',
  "cmp_attr"  =>
'  /* should be identical but ...*/
   return strcmp(attr_a->symconst_label, attr_b->symconst_label);'
},

"CondJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct conditional jump: CMP A, B && JMPxx LABEL",
  "cmp_attr"  => "  return arm_comp_condJmp(attr_a, attr_b);\n",
  "reg_req"   => { "in" => [ "gp", "gp" ], "out" => [ "none", "none"] },
},

"SwitchJmp" => {
  "op_flags"  => "L|X|Y",
  "comment"   => "construct switch",
  "reg_req"   => { "in" => [ "gp" ], "out" => [ "none" ] },
  "cmp_attr"  => "  return 0;\n",
},

# Load / Store

"Load" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. ldr %D1, [%S1, #0] /* Load((%S1)) -> %D1, (%A1) */',
  "outs"      => [ "res", "M" ],
},

"Loadb" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. ldrb %D1, [%S1, #0] /* Load((%S1)) -> %D1, (%A1) */',
  "outs"      => [ "res", "M" ],
},

"Loadbs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. ldrsb %D1, [%S1, #0] /* Load((%S1)) -> %D1, (%A1) */',
  "outs"      => [ "res", "M" ],
},

"Loadh" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. ldrh %D1, [%S1, #0] /* Load((%S1)) -> %D1, (%A1) */',
  "outs"      => [ "res", "M" ],
},

"Loadhs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. ldrsh %D1, [%S1, #0] /* Load((%S1)) -> %D1, (%A1) */',
  "outs"      => [ "res", "M" ],
},

"Storeb" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ] },
  "emit"      => '. strb %S2, [%S1, #0] /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"Storebs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ] },
  "emit"      => '. strsb %S2, [%S1, #0] /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"Storeh" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ] },
  "emit"      => '. strh %S2, [%S1, #0] /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"Storehs" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ] },
  "emit"      => '. strhs %S2, [%S1, #0] /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"Store" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "gp", "gp", "none" ] },
  "emit"      => '. str %S2, [%S1, #0] /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"StoreStackM4Inc" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "reg_req"   => { "in" => [ "sp", "gp", "gp", "gp", "gp", "none" ], "out" => [ "gp", "none" ] },
  "emit"      => '. stmfd %S1!, {%S2, %S3, %S4, %S5} /* Store multiple on Stack*/',
  "outs"      => [ "ptr", "M" ],
},

"LoadStackM3" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  "reg_req"   => { "in" => [ "sp", "none" ], "out" => [ "gp", "gp", "gp", "none" ] },
  "emit"      => '. ldmfd %S1, {%D1, %D2, %D3} /* Load multiple from Stack */',
  "outs"      => [ "res0", "res1", "res2", "M" ],
},


#---------------------------------------------------#
#    __                               _             #
#   / _|                             | |            #
#  | |_ _ __   __ _   _ __   ___   __| | ___  ___   #
#  |  _| '_ \ / _` | | '_ \ / _ \ / _` |/ _ \/ __|  #
#  | | | |_) | (_| | | | | | (_) | (_| |  __/\__ \  #
#  |_| | .__/ \__,_| |_| |_|\___/ \__,_|\___||___/  #
#      | |                                          #
#      |_|                                          #
#---------------------------------------------------#

# commutative operations

"fpaAdd" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FPA Add: Add(a, b) = Add(b, a) = a + b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. adf%M %D1, %S1, %S2 /* FPA Add(%S1, %S2) -> %D1 */',
},

"fpaMul" => {
  "op_flags"  => "C",
  "comment"   => "construct FPA Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. muf%M %D1, %S1, %S2 /* FPA Mul(%S1, %S2) -> %D1 */',
},

"fpaFMul" => {
  "op_flags"  => "C",
  "comment"   => "construct FPA Fast Mul: Mul(a, b) = Mul(b, a) = a * b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. fml%M %D1, %S1, %S2 /* FPA Fast Mul(%S1, %S2) -> %D1 */',
},

"fpaMax" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FPA Max: Max(a, b) = Max(b, a) = a > b ? a : b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. fmax %S1, %S2, %D1 /* FPA Max(%S1, %S2) -> %D1 */',
},

"fpaMin" => {
  "op_flags"  => "C",
  "irn_flags" => "R",
  "comment"   => "construct FPA Min: Min(a, b) = Min(b, a) = a < b ? a : b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. fmin %S1, %S2, %D1 /* FPA Min(%S1, %S2) -> %D1 */',
},

# not commutative operations

"fpaSub" => {
  "irn_flags" => "R",
  "comment"   => "construct FPA Sub: Sub(a, b) = a - b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. suf%M %D1, %S1, %S2 /* FPA Sub(%S1, %S2) -> %D1 */'
},

"fpaRsb" => {
  "irn_flags" => "R",
  "comment"   => "construct FPA reverse Sub: Sub(a, b) = b - a",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. rsf%M %D1, %S1, %S2 /* FPA reverse Sub(%S1, %S2) -> %D1 */'
},

"fpaDiv" => {
  "comment"   => "construct FPA Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. dvf%M %D1, %S1, %S2 /* FPA Div(%S1, %S2) -> %D1 */',
},

"fpaRdv" => {
  "comment"   => "construct FPA reverse Div: Div(a, b) = b / a",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. rdf%M %D1, %S1, %S2 /* FPA reverse Div(%S1, %S2) -> %D1 */',
},

"fpaFDiv" => {
  "comment"   => "construct FPA Fast Div: Div(a, b) = a / b",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. fdv%M %D1, %S1, %S2 /* FPA Fast Div(%S1, %S2) -> %D1 */',
},

"fpaFRdv" => {
  "comment"   => "construct FPA Fast reverse Div: Div(a, b) = b / a",
  "reg_req"   => { "in" => [ "fpa", "fpa" ], "out" => [ "fpa" ] },
  "emit"      =>'. frd%M %D1, %S1, %S2 /* FPA Fast reverse Div(%S1, %S2) -> %D1 */',
},

"fpaMov" => {
  "irn_flags" => "R",
  "comment"   => "construct FPA Move: b = a",
  "reg_req"   => { "in" => [ "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. mvf%M %S1, %D1 /* FPA Mov %S1 -> %D1 */',
},

"fpaMnv" => {
  "irn_flags" => "R",
  "comment"   => "construct FPA Move Negated: b = -a",
  "reg_req"   => { "in" => [ "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. mnf%M %S1, %D1 /* FPA Neg(%S1) -> %D1 */',
},

"fpaAbs" => {
  "irn_flags" => "R",
  "comment"   => "construct FPA Absolute value: fAbsd(a) = |a|",
  "reg_req"   => { "in" => [ "fpa" ], "out" => [ "fpa" ] },
  "emit"      => '. abs%M %D1, %S1 /* FPA Abs(%S1) -> %D1 */',
},

# other operations

"fpaConst" => {
  "op_flags"  => "c",
  "irn_flags" => "R",
  "comment"   => "represents a FPA constant",
  "attr"      => "tarval *val",
  "init_attr" => 'attr->value = val;',
  "reg_req"   => { "out" => [ "fpa" ] },
  "emit"      => '. fmov %D1, %C /* Mov fConst into register */',
  "cmp_attr"  => 'return attr_a->value != attr_b->value;',
},

"fpaFlt" => {
  "irn_flags" => "R",
  "comment"   => "construct a FPA integer->float conversion",
  "reg_req"   => { "in" => ["gp"], "out" => [ "fpa" ] },
  "emit"      => '. flt%M %D1, %S1 /* convert int to fp */',
},

"fpaFix" => {
  "irn_flags" => "R",
  "comment"   => "construct a FPA float->integer conversion",
  "reg_req"   => { "in" => ["fpa"], "out" => [ "gp" ] },
  "emit"      => '. fix %D1, %S1 /* convert fp to int */',
},

# Load / Store

"fpaLdf" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct FPA Load: Load(ptr, mem) = LD ptr",
  "attr"      => "ir_mode *op_mode",
  "init_attr" => "attr->op_mode = op_mode;",
  "reg_req"   => { "in" => [ "gp", "none" ], "out" => [ "fpa", "none" ] },
  "emit"      => '. ldf%M %D1, [%S1, #0] /* Load((%S1)) -> %D1 */',
  "outs"      => [ "res", "M" ],
},

"fpaStf" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  "attr"      => "ir_mode *op_mode",
  "init_attr" => "attr->op_mode = op_mode;",
  "reg_req"   => { "in" => [ "gp", "fpa", "none" ] },
  "emit"      => '. stf%M [%S2, #0], %S1 /* Store(%S2) -> (%S1), (%A1, %A2) */',
  "outs"      => [ "M" ],
},

"fpaDbl2GP" => {
  "op_flags"  => "L|F",
  "irn_flags" => "R",
  "state"     => "exc_pinned",
  "comment"   => "construct fp double to 2 gp register transfer",
  "reg_req"   => { "in" => [ "fpa", "none" ], "out" => [ "gp", "gp" ] },
  "outs"      => [ "low", "high", "M" ],
},


#---------------------------------------------------#
#          __                         _             #
#         / _|                       | |            #
#  __   _| |_ _ __    _ __   ___   __| | ___  ___   #
#  \ \ / /  _| '_ \  | '_ \ / _ \ / _` |/ _ \/ __|  #
#   \ V /| | | |_) | | | | | (_) | (_| |  __/\__ \  #
#    \_/ |_| | .__/  |_| |_|\___/ \__,_|\___||___/  #
#            | |                                    #
#            |_|                                    #
#---------------------------------------------------#

); # end of %nodes
