#!/usr/bin/perl -w

#
# This file is part of libFirm.
# Copyright (C) 2014 University of Karlsruhe.
#

# This script generates the C code which creates the irop's and
# their corresponding node constructors for all operations in a given spec
# so they can be used as normal firm nodes.
# Creation: 2005/10/19

use strict;
use Data::Dumper;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %nodes;
our $default_attr_type;
our $default_copy_attr;
our %init_attr;
our $custom_init_attr_func;
our %copy_attr;
our %reg_classes;
our %custom_irn_flags;

# include spec file

my $return;

no strict "subs";
unless ($return = do $specfile) {
	die "Fatal error: couldn't parse $specfile: $@" if $@;
	die "Fatal error: couldn't do $specfile: $!"    unless defined $return;
	die "Fatal error: couldn't run $specfile"       unless $return;
}
use strict "subs";

my $target_c = $target_dir."/gen_".$arch."_new_nodes.c.inl";
my $target_h = $target_dir."/gen_".$arch."_new_nodes.h";

if(!defined($default_attr_type)) {
	$default_attr_type = "${arch}_attr_t";
}
if(! %init_attr) {
	%init_attr = (
		"$default_attr_type" => "\tinit_${arch}_attributes(res, irn_flags_, in_reqs, n_res);",
	);
}

# create c code file from specs

my $obst_limit_func  = ""; #
my $obst_reg_reqs    = ""; #
my $obst_opvar       = ""; # buffer for the "ir_op *op_<arch>_<op-name> = NULL;" statements
my $obst_get_opvar   = ""; # buffer for the get_op_<arch>_<op-name>() functions
my $obst_constructor = ""; # buffer for node constructor functions
my $obst_new_irop    = ""; # buffer for the new_ir_op calls
my $obst_free_irop   = ""; # buffer for free_ir_op calls
my $obst_enum_op     = ""; # buffer for creating the <arch>_opcode enum
my $obst_header      = ""; # buffer for function prototypes
my $obst_attrs_equal = ""; # buffer for the compare attribute functions
my $obst_proj        = ""; # buffer for the pn_ numbers
my $orig_op;
my $arity;
my $cmp_attr_func;
my $temp;
my $n_opcodes = 0;    # number of opcodes
my $ARITY_VARIABLE = -1;
my $ARITY_DYNAMIC  = -2;
my %requirements = ();
my %limit_bitsets = ();
my %reg2class = ();
my %regclass2len = ();

# build register->class hashes
foreach my $class_name (sort(keys(%reg_classes))) {
	my @class         = @{ $reg_classes{"$class_name"} };
	my $old_classname = $class_name;

	pop(@class);

	$class_name = $arch."_".$class_name;

	my $idx = 0;
	foreach (@class) {
		$reg2class{$_->{name}} = {
			"class" => $old_classname,
			"index" => $idx
		};
		$idx++;
	}

	$regclass2len{$old_classname} = $idx;
}


$obst_header .= "void ${arch}_create_opcodes(const arch_irn_ops_t *be_ops);\n";
$obst_header .= "void ${arch}_free_opcodes(void);\n";

sub create_constructor {
	my $op   = shift;
	my $name = shift;
	my $n    = shift;
	my $on   = shift;
	my $known_mode;

	my $suffix = "";
	if ($name ne "") {
		$suffix = "_${name}";
	}

	# determine mode
	if (exists($n->{mode})) {
		$known_mode = $n->{mode};
	}

	# determine arity
	my $arity = 0;
	if(exists($n->{"arity"})) {
		$arity = $n->{"arity"};
	} elsif (exists($n->{"reg_req"}) && exists($n->{"reg_req"}{"in"})) {
		$arity = scalar(@{ $n->{"reg_req"}{"in"} });
	} elsif (exists($n->{"ins"})) {
		$arity = scalar(@{ $n->{"ins"} });
	}
	if($arity eq "variable") {
		$arity = $ARITY_VARIABLE;
	} elsif($arity eq "dynamic") {
		$arity = $ARITY_DYNAMIC;
	}

	# determine out arity
	my $out_arity = 0;
	if(exists($n->{"out_arity"})) {
		$out_arity = $n->{"out_arity"};
	} elsif (exists($n->{"reg_req"}) && exists($n->{"reg_req"}{"out"})) {
		$out_arity = scalar(@{ $n->{"reg_req"}{"out"} });
	} elsif (exists($n->{"outs"})) {
		$out_arity = scalar(@{ $n->{"outs"} });
	}
	if($out_arity eq "variable") {
		$out_arity = $ARITY_VARIABLE;
	} elsif($out_arity eq "dynamic") {
		$out_arity = $ARITY_DYNAMIC;
	}
	if ($out_arity != 0 && $out_arity != 1 && !defined($known_mode)) {
		$known_mode = "mode_T";
	}

	my $comment = $n->{"comment"};
	if(!exists($n->{"comment"})) {
		$comment = "construct ${orig_op} node";
	}
	$comment =
		"/**\n".
		" * ${comment}\n".
		" */\n";

	$obst_constructor .= $comment;

	# create constructor head
	my $complete_args = "";
	$temp             = "";

	$temp = "ir_node *new_bd_${arch}_${op}${suffix}(dbg_info *dbgi, ir_node *block";
	if (!exists($n->{"args"})) { # default args
		if ($arity == $ARITY_VARIABLE) {
			$complete_args = ", int arity, ir_node *in[]";
		} elsif ($arity == $ARITY_DYNAMIC) {
			$complete_args = "";
		} else {
			for (my $i = 0; $i < $arity; $i++) {
				my $opname = "op${i}";
				if (exists($n->{"ins"})) {
					my @ins = @{ $n->{"ins"} };
					$opname = $ins[$i];
				}

				$complete_args .= ", ir_node *${opname}";
			}
		}
		if ($out_arity == $ARITY_VARIABLE) {
			$complete_args .= ", int n_res";
		}

		if (!defined($known_mode)) {
			$complete_args .= ", ir_mode *mode";
		}
	} else { # user defined args
		for my $href (@{ $n->{"args"} }) {
			$href->{"type"} .= " " if ($href->{"type"} !~ / [*]?$/); # put a space between name and type if there is none at the end
			$complete_args  .= ", ".$href->{"type"}.$href->{"name"};
		}
	}

	# we have additional attribute arguements
	if (exists($n->{"attr"})) {
		$complete_args .= ", ".$n->{"attr"};
	}

	$temp .= "$complete_args)";
	$obst_constructor .= "${temp}\n{\n";

	$obst_header .= $comment;
	$obst_header .= "${temp};\n";

	# emit constructor code
	$temp = <<EOF;
	arch_irn_flags_t irn_flags_ = arch_irn_flags_none;
EOF

	if($arity == $ARITY_DYNAMIC) {
		$temp .= <<EOF;
	int      const   arity      = -1;
	ir_node  const **in         = NULL;
EOF
	} elsif($arity == $ARITY_VARIABLE) {
	} else {
		$temp .= <<EOF;
	int      const   arity      = $arity;
EOF
		if($arity > 0) {
			$temp .= <<EOF;
	ir_node         *in[$arity];
EOF
		} else {
			$temp .= <<EOF;
	ir_node        **in         = NULL;
EOF
		}
	}
	if($out_arity == $ARITY_DYNAMIC) {
		$temp .= <<EOF;
	int      const   n_res      = -1;
EOF
	} elsif($out_arity == $ARITY_VARIABLE) {
	} else {
		$temp .= <<EOF;
	int      const   n_res      = ${out_arity};
EOF
	}

	undef my $in_req_var;
	undef my $out_req_var;

	my $set_out_reqs = "";

	# set up static variables for requirements and registers
	if (exists($n->{"reg_req"})) {
		my %req = %{ $n->{"reg_req"} };
		my $idx;

		undef my @in;
		@in = @{ $req{"in"} } if (exists($req{"in"}));
		undef my @out;
		@out = @{ $req{"out"} } if exists(($req{"out"}));

		for(my $idx = 0; $idx < $#in; $idx++) {
			my $req = $in[$idx];
			generate_requirements($req, $n, "${arch}_${op}", $idx, 1);
		}
		for(my $idx = 0; $idx < $#out; $idx++) {
			my $req = $out[$idx];
			generate_requirements($req, $n, "${arch}_${op}", $idx, 0);
		}

		if (@in) {
			if($arity >= 0 && scalar(@in) != $arity) {
				die "Fatal error: Arity and number of in requirements don't match for ${op}\n";
			}

			$temp .= "\tstatic const arch_register_req_t *in_reqs[] =\n";
			$temp .= "\t{\n";
			for ($idx = 0; $idx <= $#in; $idx++) {
				my $req = $in[$idx];
				my $reqstruct = generate_requirements($req, $n, "${arch}_${op}", $idx, 1);
				$temp .= "\t\t& ${reqstruct},\n";
			}
			$temp .= "\t};\n";
		} else {
			if($arity > 0) {
				die "Fatal error: need in requirements for ${op}\n";
			}
			$temp .= "\tstatic const arch_register_req_t **in_reqs = NULL;\n";
		}

		if (@out) {
			if($out_arity >= 0 && scalar(@out) != $out_arity) {
				die "Fatal error: Out-Arity and number of out requirements don't match for ${op}\n";
			}

			for ($idx = 0; $idx <= $#out; $idx++) {
				my $req = $out[$idx];
				my $reqstruct = generate_requirements($req, $n, "${arch}_${op}", $idx, 0);
				$set_out_reqs .= <<EOF;
	info->out_infos[${idx}].req = &${reqstruct};
EOF
			}
		} else {
			if($out_arity > 0) {
				die "Fatal error: need out requirements for ${op}\n";
			}
		}
	} else {
		$temp .= "\tstatic const arch_register_req_t **in_reqs = NULL;\n";
	}
	my $attr_type = $on->{attr_type};

	$temp .= "\n";

	if($arity > 0) {
		$temp .= "\t/* construct in array */\n";
		for (my $i = 0; $i < $arity; $i++) {
			my $opname = "op${i}";
			if (exists($n->{"ins"})) {
				my @ins = @{ $n->{"ins"} };
				$opname = $ins[$i];
			}

			$temp .= "\tin[${i}] = ${opname};\n";
		}
		$temp .= "\n";
	}

	# set flags
	if (exists($n->{"irn_flags"})) {
		$temp .= "\t/* flags */\n";
		my %known_irn_flags = (
			"none"             => "arch_irn_flag_none",
			"dont_spill"       => "arch_irn_flag_dont_spill",
			"rematerializable" => "arch_irn_flag_rematerializable",
			"modify_flags"     => "arch_irn_flag_modify_flags",
			"simple_jump"      => "arch_irn_flag_simple_jump",
			"schedule_first"   => "arch_irn_flag_schedule_first",
			"not_scheduled"    => "arch_irn_flag_not_scheduled",
		);
		if (%custom_irn_flags) {
			%known_irn_flags = (%known_irn_flags, %custom_irn_flags);
		}
		foreach my $flag (@{$n->{"irn_flags"}}) {
			if (not defined($known_irn_flags{$flag})) {
				print STDERR "WARNING: irn_flag '$flag' in opcode $op is unknown\n";
			} else {
				$temp .= "\tirn_flags_ |= " . $known_irn_flags{$flag} . ";\n";
			}
		}
		$temp .= "\n";
	}

	# lookup init function
	my $attr_init_code = "(void)in;(void)irn_flags_;(void)in_reqs;(void)n_res;";
	if ($attr_type ne "") {
		$attr_init_code = $init_attr{$attr_type};
		if(!defined($attr_init_code)) {
			die "Fatal error: Couldn't find attribute initialisation code for type '${attr_type}'";
		}
	}
	my $custominit = "";
	if(defined($custom_init_attr_func)) {
		$custominit .= &$custom_init_attr_func($n, $on, "${arch}_${op}");
	}
	if(defined($n->{custominit})) {
		$custominit .= $n->{custominit};
	}

	$temp .= <<EOF;
	/* create node */
	ir_graph *irg  = get_irn_irg(block);
	ir_op    *op   = op_${arch}_${op};
EOF
	if (defined($known_mode)) {
		$temp .= <<EOF;
	ir_mode  *mode = ${known_mode};
EOF
	}
	$temp .= <<EOF;
	ir_node  *res  = new_ir_node(dbgi, irg, block, op, mode, arity, in);

	/* init node attributes */
EOF
	if ($attr_type ne "") {
		$temp .= <<EOF;
	${attr_type} *attr = (${attr_type}*)get_irn_generic_attr(res);
	(void) attr; /* avoid potential warning */
EOF
	}
	my $fixed = $on->{fixed};
	if (defined($fixed)) {
		$temp .= <<EOF;
	${fixed}
EOF
	}
	$temp .= <<EOF;
	${attr_init_code}
EOF
	if ($custominit ne "") {
		$temp .= <<EOF;
	${custominit}
EOF
	}
	$temp .= <<EOF;
	backend_info_t *info = be_get_info(res);
	(void) info; /* avoid potential warning */
${set_out_reqs}
EOF

	if (exists($n->{"init_attr"})) {
		$temp .= "\t".$n->{"init_attr"}."\n";
	}

	$temp .= <<EOF;
	/* optimize node */
	res = optimize_node(res);
	verify_new_node(irg, res);

	return res;
EOF

	$obst_constructor .= $temp;

	# close constructor function
	$obst_constructor .= "}\n\n";
}

my @node_attrs = (
	"args",
	"arity",
	"attr",
	"comment",
	"custominit",
	"init_attr",
	"ins",
	"irn_flags",
	"mode",
	"out_arity",
	"outs",
	"reg_req",
);

$obst_enum_op .= "typedef enum ${arch}_opcodes {\n";
foreach my $op (sort(keys(%nodes))) {
	my %n        = %{ $nodes{"$op"} };
	my $known_mode;
	my $num_outs = 0;
	my $out_arity;
	my @out_flags;

	if (my $template = $n{"template"}) {
		foreach my $key (keys(%$template)) {
			if (!exists $n{$key}) {
				$n{$key} = $template->{$key};
			}
		}
	}

	# determine arity
	$arity = 0;
	if(exists($n{"arity"})) {
		$arity = $n{"arity"};
	} elsif (exists($n{"reg_req"}) && exists($n{"reg_req"}{"in"})) {
		$arity = scalar(@{ $n{"reg_req"}{"in"} });
	} elsif (exists($n{"ins"})) {
		$arity = scalar(@{ $n{"ins"} });
	}
	if($arity eq "variable") {
		$arity = $ARITY_VARIABLE;
	} elsif($arity eq "dynamic") {
		$arity = $ARITY_DYNAMIC;
	}

	# determine out arity
	$out_arity = 0;
	if(exists($n{"out_arity"})) {
		$out_arity = $n{"out_arity"};
	} elsif (exists($n{"reg_req"}) && exists($n{"reg_req"}{"out"})) {
		$out_arity = scalar(@{ $n{"reg_req"}{"out"} });
	} elsif (exists($n{"outs"})) {
		$out_arity = scalar(@{ $n{"outs"} });
	}
	if($out_arity eq "variable") {
		$out_arity = $ARITY_VARIABLE;
	} elsif($out_arity eq "dynamic") {
		$out_arity = $ARITY_DYNAMIC;
	}

	$orig_op = $op;
	$op      = $arch."_".$op;
	$temp    = "";

	# define proj numbers and in numbers
	if (exists($n{"outs"})) {
		undef my @outs;

		@outs = @{ $n{"outs"} };
		if($out_arity >= 0 && scalar(@outs) != $out_arity) {
			die "Fatal error: Op ${op} has different number of outs and out_arity\n";
		}

		$num_outs = $#outs + 1;

		if ($num_outs > 0) {
			$obst_proj .= "\ntypedef enum pn_$op {\n";

			for (my $idx = 0; $idx <= $#outs; $idx++) {
				# check, if we have additional flags annotated to out
				if ($outs[$idx] =~ /:((S|I)(\|(S|I))*)/) {
					push(@out_flags, $1);
					$outs[$idx] =~ s/:((S|I)(\|(S|I))*)//;
				}
				$obst_proj .= "\tpn_${op}_".$outs[$idx]." = ${idx},\n";
			}

			$obst_proj .= "} pn_${op};\n";
		}
		# outs have names, it must be a mode_T node
		if (!defined($n{mode})) {
			$n{mode} = "mode_T";
		}
	}
	if (exists($n{"ins"})) {
		undef my @ins;

		@ins = @{ $n{"ins"} };
		if($arity >= 0 && scalar(@ins) != $arity) {
			die "Fatal error: Op ${op} has different number of ins and arity\n";
		}

		if ($#ins >= 0) {
			$obst_proj .= "\ntypedef enum n_$op {\n";
			for (my $idx = 0; $idx <= $#ins; $idx++) {
				$obst_proj .= "\tn_${op}_".$ins[$idx]." = ${idx},\n";
			}
			$obst_proj .= "} n_$op;\n";
		}
	}

	# Create opcode
	$obst_opvar     .= "ir_op *op_$op = NULL;\n";
	$obst_get_opvar .= "bool is_$op(const ir_node *n)\n";
	$obst_get_opvar .= "{\n";
	$obst_get_opvar .= "\treturn get_irn_op(n) == op_$op;\n";
	$obst_get_opvar .= "}\n\n";

	$obst_header .= <<EOF;
extern ir_op *op_${op};
bool is_${op}(const ir_node *n);
EOF

	my $attr_type= $n{attr_type};
	if(!defined($attr_type)) {
		$attr_type = $default_attr_type;
		$n{attr_type} = $attr_type;
	}

	# determine hash function
	my $hash_func;
	if (exists($n{"hash_func"})) {
		$hash_func = $n{"hash_func"};
	}

	# determine compare function
	my $attrs_equal_func;
	if (exists($n{"attrs_equal"})) {
		$attrs_equal_func = $n{"attrs_equal"};
	} elsif ($attr_type eq "") {
		# do nothing
	} else {
		$attrs_equal_func = $attr_type;
		$attrs_equal_func =~ s/_t$//;
		$attrs_equal_func .= "s_equal";
	}

	my %constructors;
	if (exists($n{constructors})) {
		%constructors = %{ $n{constructors} };
	} else {
		# Create 1 default constructor
		my %constructor = ();
		foreach my $a (@node_attrs) {
			if (defined($n{$a})) {
				$constructor{$a} = $n{$a};
			}
		}
		%constructors = ( "" => \%constructor );
	}

	foreach my $constr (sort(keys(%constructors))) {
		my %cstr = %{ $constructors{$constr} };
		# Copy some values from outer node if they don't exists in the constr
		foreach my $a (@node_attrs) {
			if (!defined($cstr{$a}) && defined($n{$a})) {
				$cstr{$a} = $n{$a};
			}
		}
		create_constructor($orig_op, $constr, \%cstr, \%n);
	}

	# set default values for state and flags if not given
	$n{"state"}     = "floats" if (! exists($n{"state"}));
	$n{"op_flags"}  = ["none"] if (! exists($n{"op_flags"}));
	$n{"dump_func"} = "${arch}_dump_node" if (!exists($n{"dump_func"}));
	my $dump_func = $n{"dump_func"};

	my %known_flags = map { $_ => 1 } (
		"none", "commutative", "cfopcode", "unknown_jump", "fragile", "forking",
		"constlike", "keep", "start_block", "uses_memory", "dump_noblock",
		"cse_neutral"
	);
	my $is_fragile = 0;
	foreach my $flag (@{$n{"op_flags"}}) {
		if (not defined($known_flags{$flag})) {
			print STDERR "WARNING: Flag '$flag' in opcode $op is unknown\n";
		}
		if ($flag eq "fragile") {
			$is_fragile = 1;
		}
	}
	my @mapped = map { "irop_flag_$_" } @{$n{"op_flags"}};
	my $op_flags = join('|', @mapped);

	my $attr_size = "0";
	if ($attr_type ne "") {
		$attr_size = "sizeof(${attr_type})"
	}

	$n_opcodes++;
	$temp  = "\top = new_ir_op(cur_opcode + iro_$op, \"$op\", op_pin_state_".$n{"state"}.", $op_flags";
	$temp .= ", ".translate_arity($arity).", -1, ${attr_size});\n";
	$obst_new_irop .= $temp;
	$obst_new_irop .= "\top->ops.be_ops        = be_ops;\n";
	$obst_new_irop .= "\tset_op_dump(op, ${dump_func});\n";
	if (defined($attrs_equal_func)) {
		$obst_new_irop .= "\tset_op_attrs_equal(op, ${attrs_equal_func});\n";
	}
	my $copy_attr_func = $copy_attr{$attr_type};
	if (!defined($copy_attr_func)) {
		# don't set a copy_attr function if the node has no additional attributes.
		if ($attr_type ne "") {
			$copy_attr_func = $default_copy_attr;
		}
	}
	if (defined($copy_attr_func)) {
		$obst_new_irop .= "\tset_op_copy_attr(op, ${copy_attr_func});\n";
	}
	if (defined($hash_func)) {
		$obst_new_irop .= "\tset_op_hash(op, ${hash_func});\n";
	}

	if ($is_fragile) {
		$obst_new_irop .= "\tir_op_set_memory_index(op, n_${op}_mem);\n";
		$obst_new_irop .= "\tir_op_set_fragile_indices(op, pn_${op}_X_regular, pn_${op}_X_except);\n";
	}
	$obst_new_irop .= "\tset_op_tag(op, $arch\_op_tag);\n";
	if(defined($n{op_attr_init})) {
		$obst_new_irop .= "\t".$n{op_attr_init}."\n";
	}
	$obst_new_irop .= "\top_${op} = op;\n";

	$obst_free_irop .= "\tfree_ir_op(op_$op); op_$op = NULL;\n";

	$obst_enum_op .= "\tiro_$op,\n";

	$obst_header .= "\n";
}
$obst_enum_op .= "\tiro_$arch\_last\n";
$obst_enum_op .= "} $arch\_opcodes;\n\n";

# emit the code

open(OUT, ">$target_c") || die("Fatal error: Could not open $target_c, reason: $!\n");

print OUT<<EOF;
#include "gen_$arch\_regalloc_if.h"
#include "irverify_t.h"
#include "fourcc.h"
#include "irgopt.h"

$obst_attrs_equal
$obst_opvar
$obst_get_opvar

static int $arch\_opcode_start = -1;

EOF

# build the FOURCC arguments from $arch

my ($a, $b, $c, $d) = ('\0', '\0', '\0', '\0');

if (length($arch) >= 1) {
	$a = uc(substr($arch, 0, 1));
}

if (length($arch) >= 2) {
	$b = uc(substr($arch, 1, 1));
}

if (length($arch) >= 3) {
	$c = uc(substr($arch, 2, 1));
}

if (length($arch) >= 4) {
	$d = uc(substr($arch, 3, 1));
}

print OUT <<END;

/** A tag for the $arch opcodes. */
#define $arch\_op_tag FOURCC('$a', '$b', '$c', '$d')

/** Return 1 if the given opcode is a $arch machine op, 0 otherwise */
int is_$arch\_op(const ir_op *op)
{
	return get_op_tag(op) == $arch\_op_tag;
}

/** Return 1 if the given node is a $arch machine node, 0 otherwise */
int is_$arch\_irn(const ir_node *node)
{
	return is_$arch\_op(get_irn_op(node));
}

int get_$arch\_irn_opcode(const ir_node *node)
{
	assert(is_$arch\_irn(node));
	return get_irn_opcode(node) - $arch\_opcode_start;
}

#ifdef BIT
#undef BIT
#endif
#define BIT(x)  (1 << (x))

$obst_limit_func
$obst_reg_reqs
$obst_constructor

/**
 * Creates the $arch specific Firm machine operations
 * needed for the assembler irgs.
 */
void $arch\_create_opcodes(const arch_irn_ops_t *be_ops)
{
	ir_op *op;
	int    cur_opcode = get_next_ir_opcodes(iro_$arch\_last);

	$arch\_opcode_start = cur_opcode;
$obst_new_irop
}

void $arch\_free_opcodes(void)
{
$obst_free_irop
}
END

close(OUT);

open(OUT, ">$target_h") || die("Fatal error: Could not open $target_h, reason: $!\n");

my $creation_time = localtime(time());
my $tmp = uc($arch);

print OUT<<EOF;
/**
 * \@file
 * \@brief Function prototypes for the new opcode functions.
 * \@note  DO NOT EDIT THIS FILE, your changes will be lost.
 *        Edit $specfile instead.
 *        created by: $0 $specfile $target_dir
 * \@date  $creation_time
 */
#ifndef FIRM_BE_${tmp}_GEN_${tmp}_NEW_NODES_H
#define FIRM_BE_${tmp}_GEN_${tmp}_NEW_NODES_H

$obst_enum_op
int is_${arch}_irn(const ir_node *node);
int is_${arch}_op(const ir_op *op);

int get_${arch}_irn_opcode(const ir_node *node);
${obst_header}
${obst_proj}

#endif
EOF

close(OUT);

###
# Translates numeric arity into string constant.
###
sub translate_arity {
	my $arity = shift;

	if ($arity =~ /^\d+$/) {
		return "oparity_any";
	} elsif ($arity == $ARITY_VARIABLE) {
		return "oparity_variable";
	} elsif ($arity == $ARITY_DYNAMIC) {
		return "oparity_dynamic";
	} else {
		die "Fatal error: Unknown arity $arity";
	}
}

sub mangle_requirements {
	my $reqs  = shift;
	my $class = shift;
	my $flags = shift;

	my @alternatives = split(/ /, $reqs);
	for(my $idx = 0; $idx < scalar(@alternatives); $idx++) {
		$alternatives[$idx] =~ s/!/not_/g;
	}

	@alternatives = sort @alternatives;

	my $name = $class."_".join('_', @alternatives);
	if (defined($flags)) {
		$flags =~ s/\|/_/g;
		$name .= "_$flags";
	}

	return $name;
}

###
# Determines whether $name is a specified register class or not.
# @return 1 if name is register class, 0 otherwise
###
sub is_reg_class {
    my $name = shift;
    return 1 if exists($reg_classes{"$name"});
    return 0;
}

###
# Returns the register class for a given register.
# @return class or undef
###
sub get_reg_class {
    my $reg = shift;
    $reg = substr($reg, 1) if ($reg =~ /!.*/);
    return $reg2class{"$reg"}{"class"} if (exists($reg2class{"$reg"}));
    return undef;
}

###
# Returns the index of a given register within its register class.
# @return index or undef
###
sub get_reg_index {
    my $reg = shift;
    return $reg2class{"$reg"}{"index"} if (exists($reg2class{"$reg"}));
    return undef;
}

###
# Remember the register class for each index in the given requirements.
# We need this information for requirements like "in_sX" or "out_dX"
# @return array of classes corresponding to the requirement for each index
###
sub build_inout_idx_class {
	my $n     = shift;
	my $op    = shift;
	my $is_in = shift;
	my @idx_class;

	my $inout = ($is_in ? "in" : "out");

	if (exists($n->{"reg_req"}{"$inout"})) {
		my @reqs = @{ $n->{"reg_req"}{"$inout"} };

		for (my $idx = 0; $idx <= $#reqs; $idx++) {
			my $class = undef;
			my ($req,) = split(/:/, $reqs[$idx]);

			if ($req eq "none") {
				$class = "none";
			} elsif (is_reg_class($req)) {
				$class = $req;
			} else {
				my @regs = split(/ /, $req);
GET_CLASS:		foreach my $reg (@regs) {
					if ($reg =~ /!?(in|out)\_r\d+/ || $reg =~ /!in/) {
						$class = "UNKNOWN_CLASS";
					} else {
						$class = get_reg_class($reg);
						if (!defined $class) {
							die("Fatal error: Could not get ".uc($inout)." register class for '$op' pos $idx (reg $reg) ... exiting.\n");
						} else {
							last GET_CLASS;
						} # !defined class
					} # if (reg =~ ...
				} # foreach
			} # if

			push(@idx_class, $class);
		} # for
	} # if

	return @idx_class;
}

###
# Generates the function for a given $op and a given IN-index
# which returns a subset of possible register from a register class
# @return classname from which the subset is derived or undef and
#         pos which corresponds to in/out reference position or undef
###
sub build_subset_class_func {
	my $neg           = undef;
	my $class         = undef;
	my $has_limit     = 0;
	my $limit_name;
	my $same_pos      = 0;
	my $different_pos = 0;
	my $temp;
	my @obst_init;
	my @obst_limits;
	my @obst_ignore;
	my @limit_array;
	my $limit_reqs;   #used for name mangling

	# build function header
	my $node  = shift;
	my $op    = shift;
	my $idx   = shift;
	my $is_in = shift;
	my @regs  = split(/ /, shift);
	my $flags = shift;

	my @idx_class = build_inout_idx_class($node, $op, !$is_in);

	# set/unset registers
CHECK_REQS: foreach (@regs) {
		if (!$is_in && /(!)?in_r(\d+)/) {
			my $bit_pos = 1 << ($2 - 1);
			if ($different_pos & $bit_pos) {
				if ($1) {
					print STDERR "duplicate !in constraint\n";
				} else {
					print STDERR "conflicting !in and in constraints\n";
				}
				return (undef, undef, undef, undef);
			}

			if ($same_pos & $bit_pos) {
				if ($1) {
					print STDERR "conflicting !in and in constraints\n";
				} else {
					print STDERR "duplicate in constraint\n";
				}
				return (undef, undef, undef, undef);
			}

			if ($1) {
				$different_pos |= $bit_pos;
			} else {
				$same_pos      |= $bit_pos;
			}

			$class = $idx_class[$2 - 1];
			next CHECK_REQS;
		}

		# check for negate
		if (substr($_, 0, 1) eq "!") {
			if (defined($neg) && $neg == 0) {
				# we have seen a positiv constraint as first one but this one is negative
				# this doesn't make sense
				print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
				return (undef, undef, undef, undef);
			}

			if (!defined($neg)) {
				$has_limit = 1;
			}

			$_   = substr($_, 1); # skip '!'
			$neg = 1;
		} else {
			if (defined($neg) && $neg == 1) {
				# we have seen a negative constraint as first one but this one is positive
				# this doesn't make sense
				print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
				return (undef, undef, undef, undef);
			}

			$has_limit = 1;
			$neg = 0;
		}

		# check if register belongs to one of the given classes
		$temp = get_reg_class($_);
		if (!defined($temp)) {
			print STDERR "Unknown register '$_'!\n";
			return (undef, undef, undef, undef);
		}

		# set class
		if (!defined($class)) {
			$class = $temp;
		} elsif ($class ne $temp) {
			# all registers must belong to the same class
			print STDERR "Registerclass mismatch. '$_' is not member of class '$class'.\n";
			return (undef, undef, undef, undef);
		}

		# calculate position inside the initializer bitfield (only 32 bits per
		# element)
		my $regidx = get_reg_index($_);
		my $arrayp = $regidx / 32;
		push(@{$limit_array[$arrayp]}, $_);
		$limit_reqs .= "$_ ";
	}

	# don't allow ignore regs in negative constraints
	if($neg) {
		my @cur_class = @{ $reg_classes{"$class"} };
		for (my $idx = 0; $idx <= $#cur_class; $idx++) {
			if (defined($cur_class[$idx]{"type"}) && ($cur_class[$idx]{"type"} & 4)) {
				my $reg    = $cur_class[$idx]{"name"};
				my $regix  = get_reg_index($reg);
				my $arrayp = $regix / 32;
				push(@{$limit_array[$arrayp]}, $reg);
				$limit_reqs .= "$reg ";
			}
		}
	}

	if ($has_limit == 1) {
		$limit_name = "${arch}_limit_".mangle_requirements($limit_reqs, $class);

		if(defined($limit_bitsets{$limit_name})) {
			$limit_name = $limit_bitsets{$limit_name};
			return ($class, $limit_name, $same_pos, $different_pos);
		}

		$limit_bitsets{$limit_name} = $limit_name;

		$obst_limit_func .= "static const unsigned $limit_name\[] = { ";
		my $first = 1;
		my $limitbitsetlen = $regclass2len{$class};
		my $limitarraylen = ($limitbitsetlen+31) / 32;
		for(my $i = 0; $i < $limitarraylen; $i++) {

			my $limitarraypart = $limit_array[$i];
			if($first) {
				$first = 0;
			} else {
				$obst_limit_func .= ", ";
			}
			my $temp;
			if($neg) {
				$temp = "0xFFFFFFFF";
			}
			foreach my $reg (@{$limitarraypart}) {
				if($neg) {
					$temp .= " & ~";
				} elsif(defined($temp)) {
					$temp .= " | ";
				}
				my $firstreg = uc($reg_classes{$class}[0]->{"name"});
				my $classuc = uc($class);
				my $reguc = uc($reg);
				$temp .= "BIT(REG_${classuc}_${reguc})";
			}
			$obst_limit_func .= $temp || "0";
		}
		$obst_limit_func .= " };\n";
	}

	return ($class, $limit_name, $same_pos, $different_pos);
}

###
# Generate register requirements structure
###
sub generate_requirements {
	my ($reqs, $flags) = split(/:/, shift);
	my $node  = shift;
	my $op    = shift;
	my $idx   = shift;
	my $is_in = shift;
	my $width = 1;
	my $result;

	my @req_type_mask;
	if (defined($flags)) {
		foreach my $f (split(/|/, $flags)) {
			if ($f eq "I") {
				push(@req_type_mask, "arch_register_req_type_ignore");
			} elsif ($f eq "S") {
				push(@req_type_mask, "arch_register_req_type_produces_sp");
			} elsif ($f eq "a") {
				push(@req_type_mask, "arch_register_req_type_aligned");
			} elsif ($f eq "2" or $f eq "4" or $f eq "8") {
				$width = int($f);
			}
		}
	}

	my $class;
	if ($reqs eq "none") {
		return "arch_no_requirement";
	} elsif (is_reg_class($reqs)) {
		my $reqtype = join(" | ", @req_type_mask) || "arch_register_req_type_none";
		$class  = $reqs;
		$result = <<EOF;
{
	.cls             = &${arch}_reg_classes[CLASS_${arch}_${class}],
	.limited         = NULL,
	.type            = ${reqtype},
	.other_same      = 0,
	.other_different = 0,
	.width           = $width,
};

EOF

	} else {
		my ($regclass, $limit_bitset, $same_pos, $different_pos)
			= build_subset_class_func($node, $op, $idx, $is_in, $reqs, $flags);

		if (!defined($regclass)) {
			die("Fatal error: Could not build subset for requirements '$reqs' of '$op' pos $idx ... exiting.\n");
		}

		if (defined($limit_bitset) && $limit_bitset ne "NULL") {
			push(@req_type_mask, "arch_register_req_type_limited");
		}
		if ($same_pos != 0) {
			push(@req_type_mask, "arch_register_req_type_should_be_same");
		}
		if ($different_pos != 0) {
			push(@req_type_mask, "arch_register_req_type_must_be_different");
		}
		my $reqtype = join(" | ", @req_type_mask);

 		if(!defined($limit_bitset)) {
			$limit_bitset = "NULL";
		}

		$class  = $regclass;
		$result = <<EOF;
{
	.cls             = &${arch}_reg_classes[CLASS_${arch}_${class}],
	.limited         = ${limit_bitset},
	.type            = ${reqtype},
	.other_same      = ${same_pos},
	.other_different = ${different_pos},
	.width           = $width,
};

EOF
	}

	my $name = "${arch}_requirements_".mangle_requirements($reqs, $class, $flags);
	if(defined($requirements{$name})) {
		return $name;
	}
	$requirements{$name} = $name;
	$obst_reg_reqs .= "static const arch_register_req_t ${name} = ${result}\n";

	return $name;
}
