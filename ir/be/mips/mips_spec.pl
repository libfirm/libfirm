# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "mips";
$new_emit_syntax = 1;

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   op_flags  => "N|L|C|X|I|F|Y|H|c|K",
#   "arity"     => "0|1|2|3 ... |variable|dynamic|any",
#   "state"     => "floats|pinned|mem_pinned|exc_pinned",
#   "args"      => [
#                    { "type" => "type 1", "name" => "name 1" },
#                    { "type" => "type 2", "name" => "name 2" },
#                    ...
#                  ],
#   "comment"   => "any comment for constructor",
#   reg_req   => { in => [ "reg_class|register" ], out => [ "reg_class|register|in_rX" ] },
#   "cmp_attr"  => "c source code for comparing node attributes",
#   emit      => "emit code with templates",
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
# NOTE: Last entry of each class is the largest Firm-Mode a register can hold\
%reg_classes = (
	"gp" => [
		{ name => "zero", type => 4+2 },  # always zero
		{ name => "at", type => 4 }, # reserved for assembler
		{ name => "v0", realname => "2", type => 1 }, # first return value
		{ name => "v1", realname => "3", type => 1 }, # second return value
		{ name => "a0", realname => "4", type => 1 }, # first argument
		{ name => "a1", realname => "5", type => 1 }, # second argument
		{ name => "a2", realname => "6", type => 1 }, # third argument
		{ name => "a3", realname => "7", type => 1 }, # fourth argument
		{ name => "t0", realname => "8", type => 1 },
		{ name => "t1", realname => "9", type => 1 },
		{ name => "t2", realname => "10", type => 1 },
		{ name => "t3", realname => "11", type => 1 },
		{ name => "t4", realname => "12", type => 1 },
		{ name => "t5", realname => "13", type => 1 },
		{ name => "t6", realname => "14", type => 1 },
		{ name => "t7", realname => "15", type => 1 },
		{ name => "s0", realname => "16", type => 2 },
		{ name => "s1", realname => "17", type => 2 },
		{ name => "s2", realname => "18", type => 2 },
		{ name => "s3", realname => "19", type => 2 },
		{ name => "s4", realname => "20", type => 2 },
		{ name => "s5", realname => "21", type => 2 },
		{ name => "s6", realname => "22", type => 2 },
		{ name => "s7", realname => "23", type => 2 },
		{ name => "t8", realname => "24", type => 1 },
		{ name => "t9", realname => "25", type => 1 },
		{ name => "kt0", type => 4 }, # reserved for OS
		{ name => "kt1", type => 4 }, # reserved for OS
		{ name => "gp", type => 4 }, # general purpose
		{ name => "sp", type => 4 }, # stack pointer
		{ name => "fp", type => 4 }, # frame pointer
		{ name => "ra", type => 2+1 }, # return address
		{ name => "gp_NOREG", realname => "!NOREG_INVALID!", type => 4 | 8 | 16 }, #dummy register for immediate nodes
		{ mode => "mode_Iu" }
	],
); # %reg_classes

%emit_templates = (
    S0  => "${arch}_emit_source_register(node, 0);",
    S1  => "${arch}_emit_source_register(node, 1);",
    S2  => "${arch}_emit_source_register(node, 2);",
	SI1 => "${arch}_emit_source_register_or_immediate(node, 1);",
    D0  => "${arch}_emit_dest_register(node, 0);",
    D1  => "${arch}_emit_dest_register(node, 1);",
    D2  => "${arch}_emit_dest_register(node, 2);",
	A0  => "${arch}_emit_load_store_address(node, 0);",
	I   => "${arch}_emit_immediate_suffix(node, 1);",
	C   => "${arch}_emit_immediate(node);",
	JumpTarget => "${arch}_emit_jump_target(node);",
	JumpTarget1 => "${arch}_emit_jump_target_proj(node, 1);",
	JumpOrFallthrough => "${arch}_emit_jump_or_fallthrough(node, 0);",
);

$default_attr_type = "mips_attr_t";
$default_copy_attr = "mips_copy_attr";

$mode_gp = "mode_Iu";

%init_attr = (
	mips_attr_t            => "\tinit_mips_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res);",

	mips_immediate_attr_t  => "\tinit_mips_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res);\n".
	                         "\tinit_mips_immediate_attributes(res, imm_type, entity, val);",

	mips_load_store_attr_t => "\tinit_mips_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res);\n".
	                         "\tinit_mips_load_store_attributes(res, entity, offset);",
);

%compare_attr = (
	mips_attr_t            => "mips_compare_nodes_attr",
	mips_immediate_attr_t  => "mips_compare_immediate_attr",
	mips_load_store_attr_t => "mips_compare_load_store_attr",
);

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

Immediate => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "gp_NOREG" ] },
	attr      => "mips_immediate_type_t imm_type, ir_entity *entity, long val",
	attr_type => "mips_immediate_attr_t",
	mode      => $mode_gp,
},

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

addu => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. add%I%.u %D0, %S0, %SI1',
	mode      => $mode_gp,
},

and => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. and%I %D0, %S0, %SI1',
	mode      => $mode_gp,
},

div => {
	reg_req   => { in => [ "gp", "gp" ], out => [ "none", "none" ] },
	ins       => [ "left", "right" ],
	outs      => [ "lohi", "M" ],
	emit      => '. div %S0, %S1',
	mode      => "mode_M",
},

divu => {
	reg_req   => { in => [ "gp", "gp" ], out => [ "none", "none" ] },
	ins       => [ "left", "right" ],
	outs      => [ "lohi", "M" ],
	emit      => '. divu %S0, %S1',
	mode      => "mode_M",
},

mult => {
	reg_req   => { in => [ "gp", "gp" ], out => [ "none" ] },
	ins       => [ "left", "right" ],
	emit      => '. mult %S0, %S1',
	mode      => "mode_M"
},

multu => {
	reg_req   => { in => [ "gp", "gp" ], out => [ "none" ] },
	ins       => [ "left", "right" ],
	emit      => '. multu %S0, %S1',
	mode      => "mode_M",
},

nor => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit      => '. nor%I %D0, %S0, %SI1',
	mode      => $mode_gp
},

or => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. or%I %D0, %S0, %SI1',
	mode      => $mode_gp
},

sll => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. sll %D0, %S0, %SI1',
	mode      => $mode_gp,
},

sra => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. sra %D0, %S0, %SI1',
	mode      => $mode_gp
},

srl => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. srl %D0, %S0, %SI1',
	mode      => $mode_gp,
},

subu => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. subu %D0, %S0, %S1',
	mode      => $mode_gp
},

xor => {
	op_flags  => "R",
	reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
	ins       => [ "left", "right" ],
	emit      => '. xor%I %D0, %S0, %SI1',
	mode      => $mode_gp,
},

seb => {
	op_flags  => "R",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "val" ],
	emit      => '.seb %D0, %S0',
	mode      => $mode_gp,
},

seh => {
	op_flags  => "R",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "val" ],
	emit      => '.seh %D0, %S0',
	mode      => $mode_gp,
},

lui => {
	op_flags  => "c",
	irn_flags => "R",
	reg_req   => { out => [ "gp" ] },
	emit      => '.lui %D0, %C',
	attr_type => "mips_immediate_attr_t",
	attr      => "mips_immediate_type_t imm_type, ir_entity *entity, long val",
	mode      => $mode_gp,
},

mflo => {
	irn_flags => "R",
	reg_req   => { in => [ "none" ], out => [ "gp" ] },
	ins       => [ "lohi" ],
	emit      => '. mflo %D0',
	mode      => $mode_gp
},

mfhi => {
	irn_flags => "R",
	reg_req   => { in => [ "none" ], out => [ "gp" ] },
	ins       => [ "lohi" ],
	emit      => '. mfhi %D0',
	mode      => $mode_gp
},

zero => {
	state     => "pinned",
	op_flags  => "c",
	irn_flags => "I",
	reg_req   => { out => [ "zero" ] },
	emit      => '',
	mode      => $mode_gp
},

#
#  ____                       _        __  _
# | __ ) _ __ __ _ _ __   ___| |__    / / | |_   _ _ __ ___  _ __
# |  _ \| '__/ _` | '_ \ / __| '_ \  / /  | | | | | '_ ` _ \| '_ \
# | |_) | | | (_| | | | | (__| | | |/ / |_| | |_| | | | | | | |_) |
# |____/|_|  \__,_|_| |_|\___|_| |_/_/ \___/ \__,_|_| |_| |_| .__/
#                                                           |_|
#

slt => {
	op_flags => "R",
	reg_req => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit    => '. slt%I %D0, %S0, %SI1',
	mode    => $mode_gp,
},

sltu => {
	op_flags => "R",
	reg_req  => { in => [ "gp", "gp" ], out => [ "gp" ] },
	emit     => '. slt%I%.u %D0, %S0, %SI1',
	mode     => $mode_gp,
},

beq => {
	op_flags => "X|Y",
	reg_req  => { in => [ "gp", "gp" ], out => [ "none", "none" ] },
	outs     => [ "false", "true" ],
	emit     => '. beq %S0, %S1, %JumpTarget1
	             . %JumpOrFallthrough'
},

bne => {
	op_flags => "X|Y",
	reg_req  => { in => [ "gp", "gp" ], out => [ "none", "none" ] },
	outs     => [ "false", "true" ],
	emit     => '. bne %S0, %S1, %JumpTarget1
	             . %JumpOrFallthrough'
},

bgtz => {
	op_flags => "X|Y",
	reg_req  => { in => [ "gp" ], out => [ "none", "none" ] },
	outs     => [ "false", "true" ],
	emit     => '. bgtz %S0, %JumpTarget1
	             . %JumpOrFallthrough'
},

blez => {
	op_flags => "X|Y",
	reg_req  => { in => [ "gp" ], out => [ "none", "none" ] },
	outs     => [ "false", "true" ],
	emit     => '. blez %S0, %JumpTarget1
	             . %JumpOrFallthrough'
},

b => {
	op_flags => "X",
	reg_req  => { in => [ ], out => [ "none" ] },
	emit     => '. b %JumpTarget',
	mode     => 'mode_X'
},

jr => {
	op_flags => "X",
	reg_req  => { in => [ "gp" ], out => [ "none" ] },
	emit     => '. jr %S0',
	mode     => 'mode_X'
},

SwitchJump => {
	op_flags => "X",
	reg_req  => { in => [ "gp" ], out => [ "none" ] },
	emit     => '. jr %S0'
},

#  _                    _    ______  _
# | |    ___   __ _  __| |  / / ___|| |_ ___  _ __ ___
# | |   / _ \ / _` |/ _` | / /\___ \| __/ _ \| '__/ _ \
# | |__| (_) | (_| | (_| |/ /  ___) | || (_) | | |  __/
# |_____\___/ \__,_|\__,_/_/  |____/ \__\___/|_|  \___|
#

lw => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	emit      => '. lw %D0, %A0',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

lh => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	emit      => '. lh %D0, %A0',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

lhu => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	emit      => '. lhu %D0, %A0',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

lb => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	emit      => '. lb %D0, %A0',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

lbu => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
	ins       => [ "ptr", "mem" ],
	outs      => [ "res", "M" ],
	emit      => '. lbu %D0, %A0',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

sw => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "ptr", "val", "mem" ],
	emit      => '. sw %S1, %A0',
	mode      => 'mode_M',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

sh => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "ptr", "val", "mem" ],
	emit      => '. sh %S1, %A0',
	mode      => 'mode_M',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

sb => {
	op_flags  => "L|F",
	state     => "exc_pinned",
	reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
	ins       => [ "ptr", "val", "mem" ],
	emit      => '. sb %S1, %A0',
	mode      => 'mode_M',
	attr_type => "mips_load_store_attr_t",
	attr      => "ir_entity *entity, long offset",
},

#
# Miscelaneous
#

nop => {
	op_flags => "K",
	reg_req	 => { in => [], out => [ "none" ] },
	emit     => '. nop',
},

); # end of %nodes
