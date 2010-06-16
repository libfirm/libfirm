# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

$new_emit_syntax = 1;

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "sparc";

$mode_gp      = "mode_Iu";
$mode_flags   = "mode_Bu";
$mode_fp      = "mode_D";

# The node description is done as a perl hash initializer with the
# following structure:
#
# %nodes = (
#
# <op-name> => {
#   op_flags  => "N|L|C|X|I|F|Y|H|c|K",                 # optional
#   irn_flags => "R|N|I"                                # optional
#   arity     => "0|1|2|3 ... |variable|dynamic|any",   # optional
#   state     => "floats|pinned|mem_pinned|exc_pinned", # optional
#   args      => [
#                    { type => "type 1", name => "name 1" },
#                    { type => "type 2", name => "name 2" },
#                    ...
#                  ],
#   comment   => "any comment for constructor",  # optional
#   reg_req   => { in => [ "reg_class|register" ], out => [ "reg_class|register|in_rX" ] },
#   cmp_attr  => "c source code for comparing node attributes", # optional
#   outs      => { "out1", "out2" },# optional, creates pn_op_out1, ... consts
#   ins       => { "in1", "in2" },  # optional, creates n_op_in1, ... consts
#   mode      => "mode_Iu",         # optional, predefines the mode
#   emit      => "emit code with templates",   # optional for virtual nodes
#   attr      => "additional attribute arguments for constructor", # optional
#   init_attr => "emit attribute initialization template",         # optional
#   rd_constructor => "c source code which constructs an ir_node", # optional
#   hash_func => "name of the hash function for this operation",   # optional, get the default hash function else
#   latency   => "latency of this operation (can be float)"        # optional
#   attr_type => "name of the attribute struct",                   # optional
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

# available SPARC registers: 8 globals, 24 window regs (8 ins, 8 outs, 8 locals)
%reg_classes = (
	gp => [
		{ name => "g0", realname => "g0", type => 4 }, # hardwired 0, behaves like /dev/null
		{ name => "g1", realname => "g1", type => 1 }, # temp. value
		{ name => "g2", realname => "g2", type => 1 },
		{ name => "g3", realname => "g3", type => 1 },
		{ name => "g4", realname => "g4", type => 1 },
		{ name => "g5", realname => "g5", type => 4 }, # reserved by SPARC ABI
		{ name => "g6", realname => "g6", type => 4 }, # reserved by SPARC ABI
		{ name => "g7", realname => "g7", type => 4 }, # reserved by SPARC ABI

		# window's out registers
		{ name => "o0", realname => "o0", type => 1 }, # param 1 / return value from callee
		{ name => "o1", realname => "o1", type => 1 }, # param 2
		{ name => "o2", realname => "o2", type => 1 }, # param 3
		{ name => "o3", realname => "o3", type => 1 }, # param 4
		{ name => "o4", realname => "o4", type => 1 }, # param 5
		{ name => "o5", realname => "o5", type => 1 }, # param 6
		{ name => "sp", realname => "sp", type => 4 }, # our stackpointer
		{ name => "o7", realname => "o6", type => 4 }, # temp. value / address of CALL instr.

		# window's local registers
		{ name => "l0", realname => "l0", type => 0 },
		{ name => "l1", realname => "l1", type => 0 },
		{ name => "l2", realname => "l2", type => 0 },
		{ name => "l3", realname => "l3", type => 0 },
		{ name => "l4", realname => "l4", type => 0 },
		{ name => "l5", realname => "l5", type => 0 },
		{ name => "l6", realname => "l6", type => 0 },
		{ name => "l7", realname => "l7", type => 0 },

		# window's in registers
		{ name => "i0", realname => "i0", type => 0 }, # incoming param1 / return value to caller
		{ name => "i1", realname => "i1", type => 0 }, # param 2
		{ name => "i2", realname => "i2", type => 0 }, # param 3
		{ name => "i3", realname => "i3", type => 0 }, # param 4
		{ name => "i4", realname => "i4", type => 0 }, # param 5
		{ name => "i5", realname => "i5", type => 0 }, # param 6
		{ name => "fp", realname => "fp", type => 4 }, # our framepointer
		{ name => "i7", realname => "i7", type => 4 }, # return address - 8
		{ mode => $mode_gp }
	],
	flags => [
		{ name => "y", realname => "y", type => 4 },  # the multiply/divide state register
		{ mode => $mode_flags, flags => "manual_ra" }
	],
#	cpu => [
#		{ name => "psr", realname => "psr", type => 4 },  # the processor state register
#		{ name => "wim", realname => "wim", type => 4 },  # the window invalid mask register
#		{ name => "tbr", realname => "tbr", type => 4 },  # the trap base register
#		{ name => "pc", realname => "pc", type => 4 },  # the program counter register
#		{ name => "npc", realname => "npc", type => 4 },  # the next instruction addr. (PC + 1) register
#		{ mode => "mode_Iu", flags => "manual_ra" }
#	],

	# fp registers can be accessed any time
	fp  => [
		{ name => "f0", type => 1 },
		{ name => "f1", type => 1 },
		{ name => "f2", type => 1 },
		{ name => "f3", type => 1 },
		{ name => "f4", type => 1 },
		{ name => "f5", type => 1 },
		{ name => "f6", type => 1 },
		{ name => "f7", type => 1 },
		{ name => "f8", type => 1 },
		{ name => "f9", type => 1 },
		{ name => "f10", type => 1 },
		{ name => "f11", type => 1 },
		{ name => "f12", type => 1 },
		{ name => "f13", type => 1 },
		{ name => "f14", type => 1 },
		{ name => "f15", type => 1 },
		{ name => "f16", type => 1 },
		{ name => "f17", type => 1 },
		{ name => "f18", type => 1 },
		{ name => "f19", type => 1 },
		{ name => "f20", type => 1 },
		{ name => "f21", type => 1 },
		{ name => "f22", type => 1 },
		{ name => "f23", type => 1 },
		{ name => "f24", type => 1 },
		{ name => "f25", type => 1 },
		{ name => "f26", type => 1 },
		{ name => "f27", type => 1 },
		{ name => "f28", type => 1 },
		{ name => "f29", type => 1 },
		{ name => "f30", type => 1 },
		{ name => "f31", type => 1 },
		{ mode => $mode_fp }
	]
); # %reg_classes

%emit_templates = (
# emit source reg or imm dep. on node's arity
    RI => "${arch}_emit_reg_or_imm(node, -1);",
    R1I => "${arch}_emit_reg_or_imm(node, 0);",
    R2I => "${arch}_emit_reg_or_imm(node, 1);",
    R3I => "${arch}_emit_reg_or_imm(node, 2);",
# simple reg emitters
    S1 => "${arch}_emit_source_register(node, 0);",
    S2 => "${arch}_emit_source_register(node, 1);",
    S3 => "${arch}_emit_source_register(node, 2);",
    S4 => "${arch}_emit_source_register(node, 3);",
    S5 => "${arch}_emit_source_register(node, 4);",
    S6 => "${arch}_emit_source_register(node, 5);",
    D1 => "${arch}_emit_dest_register(node, 0);",
    D2 => "${arch}_emit_dest_register(node, 1);",
    D3 => "${arch}_emit_dest_register(node, 2);",
    D4 => "${arch}_emit_dest_register(node, 3);",
    D5 => "${arch}_emit_dest_register(node, 4);",
    D6 => "${arch}_emit_dest_register(node, 5);",
# more custom emitters
	C  => "${arch}_emit_immediate(node);",
	LM  => "${arch}_emit_load_mode(node);",
	SM  => "${arch}_emit_store_mode(node);",
	O  => "${arch}_emit_offset(node);",
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

$default_attr_type = "sparc_attr_t";
$default_copy_attr = "sparc_copy_attr";


%init_attr = (
		sparc_attr_t				=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
		sparc_load_store_attr_t			=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n".
		    									"\tinit_sparc_load_store_attributes(res, ls_mode, entity, entity_sign, offset, is_frame_entity);",
		sparc_symconst_attr_t			=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n".
								"\tinit_sparc_symconst_attributes(res, entity);",
		sparc_cmp_attr_t			=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);\n",
		sparc_jmp_cond_attr_t   		=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
		sparc_jmp_switch_attr_t 		=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",
		sparc_save_attr_t			=> "\tinit_sparc_attributes(res, flags, in_reqs, exec_units, n_res);",

);

%compare_attr = (
		sparc_attr_t            => "cmp_attr_sparc",
		sparc_load_store_attr_t => "cmp_attr_sparc_load_store",
		sparc_symconst_attr_t   => "cmp_attr_sparc_symconst",
		sparc_jmp_cond_attr_t	=> "cmp_attr_sparc_jmp_cond",
		sparc_jmp_switch_attr_t	=> "cmp_attr_sparc_jmp_switch",
		sparc_cmp_attr_t	=> "cmp_attr_sparc_cmp",
		sparc_save_attr_t	=> "cmp_attr_sparc_save",
);


# addressing modes: imm, reg, reg +/- imm, reg + reg
# max. imm = 13 bits signed (-4096 ... 4096)


my %cmp_operand_constructors = (
    imm => {
        attr       => "int immediate_value, bool ins_permuted, bool is_unsigned",
        custominit => "sparc_set_attr_imm(res, immediate_value);" .
						"\tinit_sparc_cmp_attr(res, ins_permuted, is_unsigned);",
        reg_req    => { in => [ "gp" ], out => [ "flags" ] },
	ins        => [ "left" ],
    },
    reg => {
	attr       => "bool ins_permuted, bool is_unsigned",
        custominit => "init_sparc_cmp_attr(res, ins_permuted, is_unsigned);",
        reg_req    => { in => [ "gp", "gp" ], out => [ "flags" ] },
        ins        => [ "left", "right" ],
    },
);

my %unop_operand_constructors = (
    imm => {
        attr       => "int immediate_value",
        custominit => "sparc_set_attr_imm(res, immediate_value);",
        reg_req    => { in => [], out => [ "gp" ] },
    },
    reg => {
        reg_req    => { in => [ "gp" ], out => [ "gp" ] },
    },
);

my %binop_operand_constructors = (
    imm => {
        attr       => "int immediate_value",
        custominit => "sparc_set_attr_imm(res, immediate_value);",
        reg_req    => { in => [ "gp" ], out => [ "gp" ] },
        ins        => [ "left" ],
    },
    reg => {
        reg_req    => { in => [ "gp", "gp" ], out => [ "gp" ] },
        ins        => [ "left", "right" ],
    },
);

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

Add => {
  irn_flags => "R",
  comment   => "construct Add: Add(a, b) = Add(b, a) = a + b",
  mode		=> $mode_gp,
  emit      => '. add %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

Sub => {
  irn_flags => "R",
  comment   => "construct Sub: Sub(a, b) = a - b",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. sub %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},


# Load / Store
Load => {
  op_flags  => "L|F",
  comment   => "construct Load: Load(ptr, mem) = LD ptr -> reg",
  state     => "exc_pinned",
  ins       => [ "ptr", "mem" ],
  outs      => [ "res", "M" ],
  reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
  attr_type => "sparc_load_store_attr_t",
  attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
  emit      => '. ld%LM [%S1%O], %D1'
},

LoadHi => {
  op_flags  => "L|F",
  comment   => "construct LoadHi: Load(ptr, mem) = sethi hi(ptr) -> reg",
  state     => "exc_pinned",
  ins       => [ "ptr", "mem" ],
  outs      => [ "res", "M" ],
  reg_req   => { in => [ "gp", "none" ], out => [ "gp", "none" ] },
  attr_type => "sparc_load_store_attr_t",
  attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
  emit      => '. sethi %%hi(%S1), %D1',
},

HiImm => {
  op_flags  => "R",
  comment   => "construct LoadHi: Load(imm, mem) = sethi hi(imm) -> reg",
  state     => "exc_pinned",
  outs      => [ "res" ],
  mode      => $mode_gp,
  reg_req   => { in => [], out => [ "gp" ] },
  #attr_type => "sparc_load_store_attr_t",
  attr       => "int immediate_value",
  custominit => "sparc_set_attr_imm(res, immediate_value);",

},

LoImm => {
  op_flags  => "R",
  comment   => "construct LoadHi: Load(imm, mem) = sethi hi(imm) -> reg",
  state     => "exc_pinned",
  ins       => [ "hireg" ],
  outs      => [ "res" ],
  mode      => $mode_gp,
  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
  #attr_type => "sparc_load_store_attr_t",
  attr       => "int immediate_value",
  custominit => "sparc_set_attr_imm(res, immediate_value);",

},

LoadLo => {
  op_flags  => "L|F",
  comment   => "construct LoadLo: Or(in, ptr, mem) = or in lo(ptr) -> reg",
  state     => "exc_pinned",
  ins       => [ "hireg", "ptr", "mem" ],
  outs      => [ "res", "M" ],
  reg_req   => { in => [ "gp", "gp", "none" ], out => [ "gp", "none" ] },
  attr_type => "sparc_load_store_attr_t",
  attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
  emit      => '. or %S1, %%lo(%S2), %D1'
},

Store => {
  op_flags  => "L|F",
  comment   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
  mode 		=> "mode_M",
  state     => "exc_pinned",
  ins       => [ "ptr", "val", "mem" ],
  outs      => [ "mem" ],
  reg_req   => { in => [ "gp", "gp", "none" ], out => [ "none" ] },
  attr_type => "sparc_load_store_attr_t",
  attr      => "ir_mode *ls_mode, ir_entity *entity, int entity_sign, long offset, bool is_frame_entity",
  emit      => '. st%SM %S2, [%S1%O]'
},

Mov => {
  irn_flags => "R",
  comment   => "construct Mov: Mov(src, dest) = MV src,dest",
  arity     => "variable",
  emit      => '. mov %R1I, %D1',
  mode      => $mode_gp,
  constructors => \%unop_operand_constructors,
},

Save => {
	comment => "function prolog instruction. autom. saves sp & shifts the register window. previous out regs become the new in regs",
	reg_req   => {
			in => [ "sp", "none"],
			out => [ "sp:I|S","none" ]
	},
	ins       => [ "stack", "mem" ],
	outs      => [ "stack", "mem" ],
	attr      => "int initial_stacksize",
	attr_type => "sparc_save_attr_t",
        init_attr => "\tinit_sparc_save_attr(res, initial_stacksize);",
},

AddSP => {
	comment => "alloc stack space",
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "gp", "none" ] },
	ins       => [ "stack", "size", "mem" ],
	outs      => [ "stack", "addr", "M" ],
	emit      => ". sub %S1, %S2, %D1\n",
},

SubSP => {
	comment => "free stack space",
	reg_req   => { in => [ "sp", "gp", "none" ], out => [ "sp:I|S", "none" ] },
	ins       => [ "stack", "size", "mem" ],
	outs      => [ "stack", "M" ],
	emit      => ". add %S1, %S2, %D1\n",
},

SymConst => {
	op_flags  => "c",
	irn_flags => "R",
	attr      => "ir_entity *entity",
	reg_req   => { out => [ "gp" ] },
	attr_type => "sparc_symconst_attr_t",
	mode      => $mode_gp,
},

FrameAddr => {
	op_flags  => "c",
	irn_flags => "R",
	attr      => "ir_entity *entity",
	reg_req   => { in => [ "gp" ], out => [ "gp" ] },
	ins       => [ "base" ],
	attr_type => "sparc_symconst_attr_t",
	mode      => $mode_gp,
},

Branch => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	reg_req   => { in => [ "flags" ], out => [ "none", "none" ] },
	attr      => "int proj_num",
	attr_type => "sparc_jmp_cond_attr_t",
	init_attr => "\tset_sparc_jmp_cond_proj_num(res, proj_num);",
},

Jmp => {
	state     => "pinned",
	op_flags  => "X",
	irn_flags => "J",
	reg_req   => { out => [ "none" ] },
	mode      => "mode_X",
},

Cmp => {
	irn_flags    => "R|F",
	emit         => '. cmp %S1, %R2I',
	mode         => $mode_flags,
	attr_type    => "sparc_cmp_attr_t",
	constructors => \%cmp_operand_constructors,
},

Tst => {
	irn_flags    => "R|F",
	emit         => '. tst %S1',
	mode         => $mode_flags,
	attr_type    => "sparc_cmp_attr_t",
	attr         => "bool ins_permuted, bool is_unsigned",
	custominit   => "init_sparc_cmp_attr(res, ins_permuted, is_unsigned);",
	reg_req      => { in => [ "gp" ], out => [ "flags" ] },
	ins          => [ "left" ],
},

SwitchJmp => {
	op_flags  => "L|X|Y",
	state     => "pinned",
	mode      => "mode_T",
	attr      => "int n_projs, long def_proj_num",
	init_attr => "\tset_sparc_jmp_switch_n_projs(res, n_projs);\n".
					"\tset_sparc_jmp_switch_default_proj_num(res, def_proj_num);",
	reg_req   => { in => [ "gp" ], out => [ "none" ] },
	attr_type => "sparc_jmp_switch_attr_t",
},

ShiftLL => {
  irn_flags => "R",
  comment   => "construct shift logical left",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. sll %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

ShiftLR => {
  irn_flags => "R",
  comment   => "construct shift logical right",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. srl %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

ShiftRA => {
  irn_flags => "R",
  comment   => "construct shift right arithmetical",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. sra %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

And => {
  irn_flags => "R",
  comment   => "construct logical and",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. and %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

Or => {
  irn_flags => "R",
  comment   => "construct logical or",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. or %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

Xor => {
  irn_flags => "R",
  comment   => "construct logical xor",
  mode		=> $mode_gp,
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  emit      => '. xor %S1, %R2I, %D1',
  constructors => \%binop_operand_constructors,
},

UMul => {
  state     => "exc_pinned",
  comment   => "construct Mul: Mul(a, b) = Mul(b, a) = a * b",
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp", "flags" ] },
  outs      => [ "low", "high" ],
  constructors => \%binop_operand_constructors,
  emit      =>'. umul %S1, %R2I, %D1'
},

UDiv => {
  irn_flags => "R",
  state     => "exc_pinned",
#  mode	    => $mode_gp,
  comment   => "construct Div: Div(a, b) = a / b",
  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
  outs      => [ "res" ],
  constructors => \%binop_operand_constructors,
  emit      =>'. udiv %S1, %R2I, %D1'
},

Minus => {
  irn_flags => "R",
  mode	    => $mode_gp,
  comment   => "construct Minus: Minus(a) = -a",
  #reg_req   => { in => [ "gp" ], out => [ "in_r1" ] },
  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
  emit      => ". sub %%g0, %S1, %D1"
},

Not => {
  irn_flags   => "R",
  mode	      => $mode_gp,
  comment     => "construct Not: Not(a) = !a",
  reg_req     => { in => [ "gp" ], out => [ "gp" ] },
  emit        => '. xnor %S1, %%g0, %D1'
},

#Mul_i => {
#  irn_flags => "R",
#  comment   => "construct Mul: Mul(a, const) = Mul(const, a) = a * const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. mul %S1, %C, %D1'
#},
#
#And => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct And: And(a, b) = And(b, a) = a AND b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. and %S1, %S2, %D1'
#},
#
#And_i => {
#  irn_flags => "R",
#  comment   => "construct And: And(a, const) = And(const, a) = a AND const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. and %S1, %C, %D1'
#},
#
#Or => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct Or: Or(a, b) = Or(b, a) = a OR b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. or %S1, %S2, %D1'
#},
#
#Or_i => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct Or: Or(a, const) = Or(const, a) = a OR const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. or %S1, %C, %D1'
#},
#
#Eor => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct Eor: Eor(a, b) = Eor(b, a) = a EOR b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. xor %S1, %S2, %D1'
#},
#
#Eor_i => {
#  irn_flags => "R",
#  comment   => "construct Eor: Eor(a, const) = Eor(const, a) = a EOR const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. xor %S1, %C, %D1'
#},

# not commutative operations
#Shl => {
#  irn_flags => "R",
#  comment   => "construct Shl: Shl(a, b) = a << b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. shl %S1, %S2, %D1'
#},
#
#Shl_i => {
#  irn_flags => "R",
#  comment   => "construct Shl: Shl(a, const) = a << const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. shl %S1, %C, %D1'
#},
#
#Shr => {
#  irn_flags => "R",
#  comment   => "construct Shr: Shr(a, b) = a >> b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "in_r1" ] },
#  emit      => '. shr %S2, %D1'
#},
#
#Shr_i => {
#  irn_flags => "R",
#  comment   => "construct Shr: Shr(a, const) = a >> const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. shr %S1, %C, %D1'
#},
#
#RotR => {
#  irn_flags => "R",
#  comment   => "construct RotR: RotR(a, b) = a ROTR b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. ror %S1, %S2, %D1'
#},
#
#RotL => {
#  irn_flags => "R",
#  comment   => "construct RotL: RotL(a, b) = a ROTL b",
#  reg_req   => { in => [ "gp", "gp" ], out => [ "gp" ] },
#  emit      => '. rol %S1, %S2, %D1'
#},
#
#RotL_i => {
#  irn_flags => "R",
#  comment   => "construct RotL: RotL(a, const) = a ROTL const",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. rol %S1, %C, %D1'
#},
#
#Minus => {
#  irn_flags => "R",
#  comment   => "construct Minus: Minus(a) = -a",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. neg %S1, %D1'
#},
#
#Inc => {
#  irn_flags => "R",
#  comment   => "construct Increment: Inc(a) = a++",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. inc %S1, %D1'
#},
#
#Dec => {
#  irn_flags => "R",
#  comment   => "construct Decrement: Dec(a) = a--",
#  reg_req   => { in => [ "gp" ], out => [ "gp" ] },
#  emit      => '. dec %S1, %D1'
#},
#
#Not => {
#  arity       => 1,
#  remat       => 1,
#  comment     => "construct Not: Not(a) = !a",
#  reg_req     => { in => [ "gp" ], out => [ "gp" ] },
#  emit        => '. not %S1, %D1'
#},


#--------------------------------------------------------#
#    __ _             _                     _            #
#   / _| |           | |                   | |           #
#  | |_| | ___   __ _| |_   _ __   ___   __| | ___  ___  #
#  |  _| |/ _ \ / _` | __| | '_ \ / _ \ / _` |/ _ \/ __| #
#  | | | | (_) | (_| | |_  | | | | (_) | (_| |  __/\__ \ #
#  |_| |_|\___/ \__,_|\__| |_| |_|\___/ \__,_|\___||___/ #
#--------------------------------------------------------#

# commutative operations

#fAdd => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct FP Add: Add(a, b) = Add(b, a) = a + b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      => '. fadd %S1, %S2, %D1'
#},
#
#fMul => {
#  op_flags  => "C",
#  comment   => "construct FP Mul: Mul(a, b) = Mul(b, a) = a * b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      =>'. fmul %S1, %S2, %D1'
#},
#
#fMax => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct FP Max: Max(a, b) = Max(b, a) = a > b ? a : b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      =>'. fmax %S1, %S2, %D1'
#},
#
#fMin => {
#  op_flags  => "C",
#  irn_flags => "R",
#  comment   => "construct FP Min: Min(a, b) = Min(b, a) = a < b ? a : b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      =>'. fmin %S1, %S2, %D1'
#},
#
## not commutative operations
#
#fSub => {
#  irn_flags => "R",
#  comment   => "construct FP Sub: Sub(a, b) = a - b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      => '. fsub %S1, %S2, %D1'
#},
#
#fDiv => {
#  comment   => "construct FP Div: Div(a, b) = a / b",
#  reg_req   => { in => [ "fp", "fp" ], out => [ "fp" ] },
#  emit      => '. fdiv %S1, %S2, %D1'
#},
#
#fMinus => {
#  irn_flags => "R",
#  comment   => "construct FP Minus: Minus(a) = -a",
#  reg_req   => { in => [ "fp" ], out => [ "fp" ] },
#  emit      => '. fneg %S1, %D1'
#},
#
## other operations
#
#fConst => {
#  op_flags  => "c",
#  irn_flags => "R",
#  comment   => "represents a FP constant",
#  reg_req   => { out => [ "fp" ] },
#  emit      => '. fmov %C, %D1',
#  cmp_attr  =>
#'
#	/* TODO: compare fConst attributes */
#	return 1;
#'
#},
#
## Load / Store
#
#fLoad => {
#  op_flags  => "L|F",
#  irn_flags => "R",
#  state     => "exc_pinned",
#  comment   => "construct FP Load: Load(ptr, mem) = LD ptr",
#  reg_req   => { in => [ "gp", "none" ], out => [ "fp" ] },
#  emit      => '. fmov (%S1), %D1'
#},
#
#fStore => {
#  op_flags  => "L|F",
#  irn_flags => "R",
#  state     => "exc_pinned",
#  comment   => "construct Store: Store(ptr, val, mem) = ST ptr,val",
#  reg_req   => { in => [ "gp", "fp", "none" ] },
#  emit      => '. fmov %S2, (%S1)'
#},

); # end of %nodes
