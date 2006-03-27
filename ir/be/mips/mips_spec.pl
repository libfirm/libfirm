# Creation: 2006/02/13
# $Id$
# This is a template specification for the Firm-Backend

# the cpu architecture (ia32, ia64, mips, sparc, ppc, ...)

$arch = "mips";

$comment_string = "#";

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
  "general_purpose" => [
                         { name => "zero", type => 4+2 },  # always zero
                         { name => "at", type => 4 }, # reserved for assembler
                         { name => "v0", type => 1 }, # first return value
                         { name => "v1", type => 1 }, # second return value
                         { name => "a0", type => 1 }, # first argument
                         { name => "a1", type => 1 }, # second argument
                         { name => "a2", type => 1 }, # third argument
                         { name => "a3", type => 1 }, # fourth argument
                         { name => "t0", type => 1 },
                         { name => "t1", type => 1 },
                         { name => "t2", type => 1 },
                         { name => "t3", type => 1 },
                         { name => "t4", type => 1 },
                         { name => "t5", type => 1 },
                         { name => "t6", type => 1 },
                         { name => "t7", type => 1 },
                         { name => "s0", type => 2 },
                         { name => "s1", type => 2 },
                         { name => "s2", type => 2 },
                         { name => "s3", type => 2 },
                         { name => "s4", type => 2 },
                         { name => "s5", type => 2 },
                         { name => "s6", type => 2 },
                         { name => "s7", type => 2 },
                         { name => "t8", type => 1 },
                         { name => "t9", type => 1 },
                         { name => "k0", type => 4 }, # reserved for OS
                         { name => "k1", type => 4 }, # reserved for OS
                         { name => "gp", type => 4 }, # general purpose
                         { name => "sp", type => 4+2 }, # stack pointer
                         { name => "fp", type => 4+2 }, # frame pointer
                         { name => "ra", type => 2+1 }, # return address. This is also caller save, because
															# the jla instruction that is used for calls modifies
															# the ra register. It is callee save too, because at the last
															# command of a function (the ja $ra) it needs to have it's
															# old value.
                         { mode => "mode_P" }
                       ],
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

add => {
  op_flags  => "C",
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		add %D1, %S1, %S2
	else
2.		addu %D1, %S1, %S2
'
},

addi => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		addi %D1, %S1, %C
	else
2.		addiu %D1, %S1, %C
',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

and => {
  op_flags  => "C",
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. and %D1, %S1, %S2',
},

andi => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. andi %D1, %S1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

div => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "none", "none", "none", "none" ] },
  emit      => '
	mips_attr_t *attr = get_mips_attr(n);
	if(attr->modes.original_mode->sign) {
2.		div %S1, %S2
	} else {
2.		divu %S1, %S2
	}
',
},

mult => {
  op_flags  => "C",
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "none" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		mult %S1, %S2
	else
2.		multu %S1, %S2
'
},

nor => {
  op_flags  => "C",
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. nor %D1, %S1, %S2'
},

not => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
  assert(get_mode_size_bits(get_irn_mode(n)) == 32);
. nor %D1, %S1, $zero
'
},

or => {
  op_flags  => "C",
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. or %D1, %S1, %S2'
},

ori => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. ori %D1, %S1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

sl => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		sal %D1, %S1, %S2
	else
2.		sll %D1, %S1, %S2',
},

sli => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		sal %D1, %S1, %C
	else
2.		sll %D1, %S1, %C',
},

sra => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. sra %D1, %S1, %S2',
},

srai => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. sra %D1, %S1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

sr => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		sra %D1, %S1, %S2
	else
2.		srl %D1, %S1, %S2
',
},

sri => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '
	if (mode_is_signed(get_irn_mode(n)))
2.		sra %D1, %S1, %C
	else
2.		srl %D1, %S1, %C
',
},

srlv => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. srlv %D1, %S1, %S2',
},

sllv => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. sllv %D1, %S1, %S2',
},

sub => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. sub %D1, %S1, %S2'
},

subu => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. subu %D1, %S1, %S2',
},

subuzero => {
  reg_req	=> { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit => '. subu %D1, $zero, %S1',
},

xor => {
  reg_req   => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. xor %D1, %S1, %S2'
},

xori => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. xori %D1, %S1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

#   ____                _              _
#  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___
# | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __|
# | |__| (_) | | | \__ \ || (_| | | | | |_\__ \
#  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/
#

# load upper imediate
lui => {
  op_flags	=> "c",
  reg_req   => { out => [ "general_purpose" ] },
  emit      => '. lui %D1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

# load lower immediate
lli => {
  op_flags	=> "c",
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. ori %D1, %S1, %C',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

la => {
  op_flags	=> "c",
  reg_req   => { out => [ "general_purpose" ] },
  emit      => '. la %D1, %C',
  cmp_attr => 'return attr_a->symconst_id != attr_b->symconst_id;',
},

mflo => {
  reg_req => { in => [ "none" ], out => [ "general_purpose" ] },
  emit	  => '. mflo %D1'
},

mfhi => {
  reg_req => { in => [ "none" ], out => [ "general_purpose" ] },
  emit	  => '. mfhi %D1'
},

zero => {
  reg_req => { out => [ "zero" ] },
  emit => '',
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
	reg_req => { in => [ "general_purpose", "general_purpose" ], out => [ "general_purpose" ] },
	emit => '
	if (mode_is_signed(get_irn_mode(n)))
2.		slt %D1, %S1, %S2
	else
2.		sltu %D1, %S1, %S2
',
},

slti => {
	reg_req => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
	emit => '
	if (mode_is_signed(get_irn_mode(n)))
2.		slti %D1, %S1, %C
	else
2.		sltiu %D1, %S1, %C
',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

beq => {
	op_flags  => "X|Y",
	# TxT -> TxX
	reg_req => { in => [ "general_purpose", "general_purpose" ], out => [ "in_r0", "none" ] },
	emit => '
	ir_node *jumpblock = mips_get_jump_block(n, 1);
	assert(jumpblock != NULL);

	lc_efprintf(arg_env, F, "\tbeq %1S, %2S, BLOCK_%d\n", n, n, get_irn_node_nr(jumpblock));
'
},

bne => {
	op_flags  => "X|Y",
	# TxT -> TxX
	reg_req => { in => [ "general_purpose", "general_purpose" ], out => [ "in_r0", "none" ] },
	emit => '
	ir_node *jumpblock = mips_get_jump_block(n, 1);
	assert(jumpblock != NULL);

	lc_efprintf(arg_env, F, "\tbne %1S, %2S, BLOCK_%d\n", n, n, get_irn_node_nr(jumpblock));
'
},

bgtz => {
	op_flags  => "X|Y",
	# TxT -> TxX
	reg_req => { in => [ "general_purpose" ], out => [ "in_r0", "none" ] },
	emit => '
	ir_node *jumpblock = mips_get_jump_block(n, 1);
	assert(jumpblock != NULL);

	lc_efprintf(arg_env, F, "\tbgtz %1S, BLOCK_%d\n", n, get_irn_node_nr(jumpblock));
'
},

blez => {
	op_flags  => "X|Y",
	# TxT -> TxX
	reg_req => { in => [ "general_purpose" ], out => [ "in_r0", "none" ] },
	emit => '
	ir_node *jumpblock = mips_get_jump_block(n, 1);
	assert(jumpblock != NULL);

	lc_efprintf(arg_env, F, "\tblez %1S, BLOCK_%d\n", n, get_irn_node_nr(jumpblock));
'
},

j => {
  op_flags => "X",
  reg_req => { in => [ "general_purpose" ] },
  emit => '. j %S1',
},

b => {
  op_flags => "X",
  # -> X
  reg_req => { in => [ ], out => [ "none" ] },
  emit => '
	ir_node *jumpblock = get_irn_link(n);
	assert(jumpblock != NULL);

  	lc_efprintf(arg_env, F, "\tb BLOCK_%d\t\t\t# mips_b\n", get_irn_node_nr(jumpblock));
'
},

fallthrough => {
  op_flags => "X",
  # -> X
  reg_req => { in => [ ], out => [ "none" ] },
  emit => '. # fallthrough'
},

SwitchJump => {
  op_flags => "X",
  # -> X,X,...
  reg_req => { in => [ "general_purpose" ], out => [ "none" ] },
  emit => '. j %S1'
},

#  _                    _
# | |    ___   __ _  __| |
# | |   / _ \ / _` |/ _` |
# | |__| (_) | (_| | (_| |
# |_____\___/ \__,_|\__,_|
#

load_r => {
  reg_req	=> { in => [ "none", "general_purpose" ], out => [ "none", "none", "general_purpose" ] },
  emit		=> '
	mips_attr_t* attr = get_mips_attr(n);
	ir_mode *mode;

	mode = attr->modes.load_store_mode;

	switch (get_mode_size_bits(mode)) {
	case 8:
		if (mode_is_signed(mode))
3.			lb %D3, %C(%S2)
		else
3.			lbu %D3, %C(%S2)
		break;
	case 16:
		if (mode_is_signed(mode))
3.			lh %D3, %C(%S2)
		else
3.			lhu %D3, %C(%S2)
		break;
	case 32:
2.		lw %D3, %C(%S2)
		break;
	default:
		assert(! "Only 8, 16 and 32 bit loads supported");
		break;
	}
',
  cmp_attr => 'return attr_a->tv != attr_b->tv || attr_a->stack_entity != attr_b->stack_entity;',
},


#  _                    _    ______  _
# | |    ___   __ _  __| |  / / ___|| |_ ___  _ __ ___
# | |   / _ \ / _` |/ _` | / /\___ \| __/ _ \| '__/ _ \
# | |__| (_) | (_| | (_| |/ /  ___) | || (_) | | |  __/
# |_____\___/ \__,_|\__,_/_/  |____/ \__\___/|_|  \___|
#

store_r => {
  reg_req	=> { in => [ "none", "general_purpose", "general_purpose" ], out => [ "none", "none" ] },
  emit		=> '
	mips_attr_t* attr = get_mips_attr(n);
	ir_mode* mode;

	mode = attr->modes.load_store_mode;

	switch (get_mode_size_bits(mode)) {
	case 8:
		if (mode_is_signed(mode))
2.		sb %S3, %C(%S2)
		break;
	case 16:
		if (mode_is_signed(mode))
2.		sh %S3, %C(%S2)
		break;
	case 32:
2.		sw %S3, %C(%S2)
		break;
	default:
		assert(! "Only 8, 16 and 32 bit stores supported");
		break;
	}
',
  cmp_attr => 'return attr_a->tv != attr_b->tv;',
},

store_i => {
  reg_req	=> { in => [ "none", "none", "general_purpose" ], out => [ "none", "none" ] },
  emit		=> '
	mips_attr_t* attr = get_mips_attr(n);
	ir_mode *mode;

	mode = attr->modes.load_store_mode;

	switch (get_mode_size_bits(mode)) {
	case 8:
2.		sb %S3, %C
		break;
	case 16:
2.		sh %S3, %C
		break;
	case 32:
2.		sw %S3, %C
		break;
	default:
		assert(! "Only 8, 16 and 32 bit stores supported");
		break;
	}
',
  cmp_attr => '
	return attr_a->stack_entity != attr_b->stack_entity;
',
},

move => {
  reg_req   => { in => [ "general_purpose" ], out => [ "general_purpose" ] },
  emit      => '. or %D1, $zero, %S1'
},

#
# Conversion
#

reinterpret_conv => {
  reg_req   => { in => [ "general_purpose" ], out => [ "in_r1" ] },
  emit      => '. # reinterpret %S1 -> %D1',
},

#
# Miscelaneous
#

nop => {
  op_flags  => "K",
  reg_req	=> { in => [], out => [ "none" ] },
  emit		=> '. nop  # nop',
},

); # end of %nodes
