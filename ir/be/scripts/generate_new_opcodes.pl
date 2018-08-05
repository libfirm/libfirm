#! /usr/bin/env perl

#
# This file is part of libFirm.
# Copyright (C) 2014 University of Karlsruhe.
#

# This script generates the C code which creates the irop's and
# their corresponding node constructors for all operations in a given spec
# so they can be used as normal firm nodes.

use strict;
use warnings;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %nodes;
our $default_attr_type;
our %init_attr;
our $custom_init_attr_func;
our %reg_classes;
our %custom_irn_flags;

# include spec file
unless (my $return = do "${specfile}") {
	die "Fatal error: couldn't parse $specfile: $@" if $@;
	die "Fatal error: couldn't do $specfile: $!"    unless defined $return;
	die "Fatal error: couldn't run $specfile"       unless $return;
}

my $target_c = "$target_dir/gen_${arch}_new_nodes.c";
my $target_h = "$target_dir/gen_${arch}_new_nodes.h";

$default_attr_type //= "${arch}_attr_t";

# create c code file from specs

my $obst_limit_func  = ""; #
my $obst_reg_reqs    = ""; #
my $obst_opvar       = ""; # buffer for the "ir_op *op_<arch>_<op-name> = NULL;" statements
my $obst_constructor = ""; # buffer for node constructor functions
my $obst_new_irop    = ""; # buffer for the new_ir_op calls
my $obst_free_irop   = ""; # buffer for free_ir_op calls
my $obst_enum_op     = ""; # buffer for creating the <arch>_opcode enum
my $obst_header      = ""; # buffer for function prototypes
my $obst_proj        = ""; # buffer for the pn_ numbers
my $orig_op;
my $ARITY_VARIABLE = -1;
my %requirements = ();
my %limit_bitsets = ();
my %reg2class = ();
my %regclass2len = ();

# build register->class hashes
foreach my $class_name (sort(keys(%reg_classes))) {
	my $regs = $reg_classes{$class_name}{registers};

	my $idx = 0;
	foreach (@$regs) {
		$reg2class{$_->{name}} = {
			class => $class_name,
			index => $idx
		};
		$idx++;
	}

	$regclass2len{$class_name} = $idx;
}


$obst_header .= <<EOF;
void ${arch}_create_opcodes(void);
void ${arch}_free_opcodes(void);
EOF

sub get_requirement_mode
{
	my ($in_reqs, $req) = @_;

	if ($req eq "exec") {
		return "mode_X";
	} elsif ($req eq "mem") {
		return "mode_M";
	} elsif ($req eq "none") {
		return "mode_ANY";
	}

	$req =~ s/[ :].*//;
	if ($req =~ s/^!?in_r//) {
		$req = $in_reqs->[$req];
		$req =~ s/[ :].*//;
	}

	my $cls;
	if ($req =~ s/^cls-//) {
		$cls = $reg_classes{$req};
	} elsif ($req =~ s/^reg-//) {
		$cls = $reg_classes{$reg2class{$req}->{class}};
	}
	$cls //= $reg_classes{$req};
	$cls //= $reg_classes{$reg2class{$req}->{class}};
	if (!defined($cls)) {
		die "cannot determine mode for requirement '$req'";
	}
	return $cls->{mode};
}

sub create_constructor
{
	my ($op, $name, $n, $on) = @_;

	my $in_reqs = $n->{in_reqs};
	my $ins     = $n->{ins};
	# determine arity
	my $arity = 0;
	if ($in_reqs) {
		if ($in_reqs eq "...") {
			$arity   = $ARITY_VARIABLE;
			$in_reqs = undef;
		} else {
			$arity = scalar(@$in_reqs);
		}
	} elsif (defined($ins)) {
		$arity = scalar(@$ins);
	}

	my $out_reqs = $n->{out_reqs};
	# determine out arity
	my $out_arity = 0;
	if ($out_reqs) {
		if ($out_reqs eq "...") {
			$out_arity = $ARITY_VARIABLE;
			$out_reqs  = undef;
		} else {
			$out_arity = scalar(@$out_reqs);
		}
	} elsif (defined(my $outs = $n->{outs})) {
		$out_arity = scalar(@$outs);
	}

	# determine mode
	my $mode = $n->{mode};
	if (!defined($mode)) {
		if ($out_arity == 0) {
			$mode = undef;
		} elsif ($out_arity == 1) {
			$mode = get_requirement_mode($in_reqs, $out_reqs->[0]);
		} else {
			$mode = "mode_T";
		}
	} elsif ($mode eq "first") {
		if ($out_arity < 1) {
			die "cannot take first mode of '$op' with no out requirements\n";
		}
		$mode = get_requirement_mode($in_reqs, $out_reqs->[0]);
	}

	# create constructor head
	my $complete_args = "";
	if ($arity == $ARITY_VARIABLE) {
		$complete_args = ", int const arity, ir_node *const *const in, arch_register_req_t const **const in_reqs";
	} else {
		for (my $i = 0; $i < $arity; $i++) {
			my $opname = $ins ? $$ins[$i] : "op$i";
			$complete_args .= ", ir_node *$opname";
		}
	}
	if ($out_arity == $ARITY_VARIABLE) {
		$complete_args .= ", int n_res";
	}
	if (!defined($mode)) {
		$complete_args .= ", ir_mode *mode";
		$mode           = "mode";
	}
	# we have additional attribute arguments
	if (defined(my $attr = $n->{attr})) {
		$complete_args .= ", $attr";
	}

	my $suffix = $name ne "" ? "_$name" : "";
	my $temp   = "ir_node *new_bd_${arch}_$op$suffix(dbg_info *dbgi, ir_node *block$complete_args)";

	my $comment = $n->{comment} // "construct $orig_op node";

	$obst_header .= <<EOF;
/**
 * $comment
 */
$temp;
EOF

	# emit constructor code
	$temp = <<EOF;

$temp
{
EOF

	if ($in_reqs) {
		if ($arity >= 0 && scalar(@$in_reqs) != $arity) {
			die "Fatal error: Arity and number of in requirements don't match for $op\n";
		}

		$temp .= "\tstatic arch_register_req_t const *in_reqs[] = {\n";
		my $idx = 0;
		for my $req (@$in_reqs) {
			my $reqstruct = generate_requirements($req, $n, "${arch}_$op", $idx++, 1);
			$temp .= "\t\t&$reqstruct,\n";
		}
		$temp .= "\t};\n";
	} elsif ($arity == 0) {
		$temp .= "\tarch_register_req_t const **const in_reqs = NULL;\n";
	}

	my $set_out_reqs = "";
	if ($out_reqs) {
		if ($out_arity >= 0 && scalar(@$out_reqs) != $out_arity) {
			die "Fatal error: Out-Arity and number of out requirements don't match for $op\n";
		}

		my $idx = 0;
		for my $req (@$out_reqs) {
			if ($idx == 0) {
				$set_out_reqs .= "\treg_out_info_t *const out_infos = be_get_info(res)->out_infos;\n";
			}
			my $reqstruct = generate_requirements($req, $n, "${arch}_$op", $idx, 0);
			$set_out_reqs .= "\tout_infos[$idx].req = &$reqstruct;\n";
			++$idx;
		}
	}

	$temp .= "\n";

	if ($arity > 0) {
		$temp .= "\t/* construct in array */\n";
		$temp .= "\tir_node *const in[] = {\n";
		for (my $i = 0; $i < $arity; $i++) {
			my $opname = $ins ? $$ins[$i] : "op$i";
			$temp .= "\t\t$opname,\n";
		}
		$temp .= "\t};\n";
	}

	my $in     = $arity != 0               ? "in"   : "NULL";
	my $arity_ = $arity != $ARITY_VARIABLE ? $arity : "arity";
	$temp .= <<EOF;

	ir_graph *const irg = get_irn_irg(block);
	ir_node  *const res = new_ir_node(dbgi, irg, block, op_${arch}_$op, $mode, $arity_, $in);

	/* init node attributes */
EOF

	my $attr_init_code = "";
	if ((my $attr_type = $on->{attr_type}) ne "") {
		$temp .= <<EOF;

	/* flags */
	arch_irn_flags_t irn_flags = arch_irn_flags_none;
EOF
		if (my $irn_flags = $n->{irn_flags}) {
			my %known_irn_flags = (
				"dont_spill"       => "arch_irn_flag_dont_spill",
				"rematerializable" => "arch_irn_flag_rematerializable",
				"modify_flags"     => "arch_irn_flag_modify_flags",
				"simple_jump"      => "arch_irn_flag_simple_jump",
				"schedule_first"   => "arch_irn_flag_schedule_first",
				"not_scheduled"    => "arch_irn_flag_not_scheduled",
				"fallthrough"      => "arch_irn_flag_fallthrough",
			);
			if (%custom_irn_flags) {
				%known_irn_flags = (%known_irn_flags, %custom_irn_flags);
			}
			foreach my $flag (@$irn_flags) {
				if (defined(my $known_irn_flag = $known_irn_flags{$flag})) {
					$temp .= "\tirn_flags |= $known_irn_flag;\n";
				} else {
					print STDERR "WARNING: irn_flag '$flag' in opcode '$op' is unknown\n";
				}
			}
		}

		if ($out_arity != $ARITY_VARIABLE) {
			$temp .= "\tint const n_res = $out_arity;\n"
		}

		$temp .= "\tbe_info_init_irn(res, irn_flags, in_reqs, n_res);\n";
		if (!defined(my $init = $init_attr{$attr_type})) {
			die "Fatal error: Couldn't find attribute initialisation code for type '$attr_type'";
		} elsif ($init ne "") {
			$attr_init_code = "\t$init\n";
		}
		$temp .= <<EOF;
	$attr_type *const attr = ($attr_type*)get_irn_generic_attr(res);
	(void)attr; /* avoid potential warning */
EOF
	}

	if (defined(my $fixed = $on->{fixed})) {
		$temp .= "\t$fixed\n";
	}

	$temp .= $attr_init_code;
	if (defined($custom_init_attr_func)) {
		my $custominit = &$custom_init_attr_func($n, $on, "${arch}_$op");
		if ($custominit ne "") {
			$temp .= "\t$custominit\n";
		}
	}

	if (defined(my $init = $n->{init})) {
		$temp .= "\t$init\n";
	}

	$temp .= <<EOF;
$set_out_reqs
	verify_new_node(res);
	return optimize_node(res);
}
EOF

	$obst_constructor .= $temp;
}

my @node_attrs = (
	"arity",
	"attr",
	"comment",
	"in_reqs",
	"init",
	"ins",
	"irn_flags",
	"mode",
	"out_reqs",
	"outs",
);

$obst_enum_op .= "typedef enum ${arch}_opcodes {\n";
foreach my $op (sort(keys(%nodes))) {
	my %n = %{ $nodes{$op} };

	if (my $template = $n{template}) {
		foreach my $key (keys(%$template)) {
			if (!exists $n{$key}) {
				$n{$key} = $template->{$key};
			}
		}
	}

	my $ins  = $n{ins};
	my $outs = $n{outs};

	# determine arity
	my $arity = 0;
	if (my $in_reqs = $n{in_reqs}) {
		if ($in_reqs eq "...") {
			$arity = $ARITY_VARIABLE;
		} else {
			$arity = scalar(@$in_reqs);
		}
	} elsif ($ins) {
		$arity = scalar(@$ins);
	}

	# determine out arity
	my $out_arity = 0;
	if (my $out_reqs = $n{out_reqs}) {
		if ($out_reqs eq "...") {
			$out_arity = $ARITY_VARIABLE;
		} else {
			$out_arity = scalar(@$out_reqs);
		}
	} elsif ($outs) {
		$out_arity = scalar(@$outs);
	}

	$orig_op = $op;
	$op      = "${arch}_$op";

	# define proj numbers and in numbers
	if ($outs) {
		my $num_outs = scalar(@$outs);
		if ($out_arity >= 0 && $num_outs != $out_arity) {
			die "Fatal error: Op $op has different number of outs and out_arity\n";
		}

		if ($num_outs > 0) {
			$obst_proj .= "\ntypedef enum pn_$op {\n";

			for (my $idx = 0; $idx < $num_outs; $idx++) {
				$obst_proj .= "\tpn_${op}_$$outs[$idx] = $idx,\n";
			}

			$obst_proj .= "} pn_$op;\n";
		}
	}
	if ($ins) {
		my $num_ins = scalar(@$ins);
		if ($arity >= 0 && $num_ins != $arity) {
			die "Fatal error: Op $op has different number of ins and arity\n";
		}

		if ($num_ins > 0) {
			$obst_proj .= "\ntypedef enum n_$op {\n";
			for (my $idx = 0; $idx < $num_ins; $idx++) {
				$obst_proj .= "\tn_${op}_$$ins[$idx] = $idx,\n";
			}
			$obst_proj .= "} n_$op;\n";
		}
	}

	# Create opcode
	$obst_opvar .= "ir_op *op_$op = NULL;\n";

	$obst_header .= <<EOF;

extern ir_op *op_$op;

static inline bool is_$op(ir_node const *const n)
{
	return get_irn_op(n) == op_$op;
}

EOF

	my $attr_type = $n{attr_type} //= $default_attr_type;

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

	my $mem = undef;
	foreach my $constr (sort(keys(%constructors))) {
		my %cstr = %{ $constructors{$constr} };
		# Copy some values from outer node if they don't exists in the constr
		foreach my $a (@node_attrs) {
			if (!defined($cstr{$a}) && defined($n{$a})) {
				$cstr{$a} = $n{$a};
			}
		}
		create_constructor($orig_op, $constr, \%cstr, \%n);

		# Scan for mem input
		my $cons_mem = undef;
		if (defined(my $in_reqs = $cstr{in_reqs})) {
			if ($in_reqs eq "...") {
				$cons_mem = -1; # unknown whether there is mem input
			} else {
				my $idx = 0;
				for my $req (@$in_reqs) {
					if ($req eq "mem") {
						if (defined($cons_mem)) {
							die("Fatal error: constructor of \"$op\" has multiple mem inputs\n");
						}
						$cons_mem = $idx;
					}
					++$idx;
				}
			}
		}

		$cons_mem //= -2; # no mem input

		if (!defined($mem)) {
			$mem = $cons_mem;
		} elsif ($mem != $cons_mem) {
			die("Fatal error: inconsistent mem input slot for \"$op\"\n");
		}
	}

	my $op_flags = $n{op_flags};

	# Add flag 'uses_memory' if there is a known mem input
	if ($mem >= 0 && (!defined($op_flags) || !grep(/^uses_memory$/, @$op_flags))) {
		push(@$op_flags, "uses_memory");
	}

	my $is_fragile = 0;
	my $op_flags_joined;
	if (defined($op_flags)) {
		my %known_flags = map { $_ => 1 } (
			"commutative", "cfopcode", "unknown_jump", "fragile", "forking",
			"constlike", "keep", "start_block", "uses_memory", "dump_noblock",
		);
		foreach my $flag (@$op_flags) {
			if (not defined($known_flags{$flag})) {
				print STDERR "WARNING: Flag '$flag' in opcode $op is unknown\n";
			}
			if ($flag eq "fragile") {
				$is_fragile = 1;
			}
		}
		$op_flags_joined = join('|', map { "irop_flag_$_" } @$op_flags);
	} else {
		$op_flags_joined = "irop_flag_none";
	}

	my $state     = $n{state} // "floats";
	my $attr_size = $attr_type ne "" ? "sizeof($attr_type)" : "0";
	$obst_new_irop .= "\top = new_ir_op(cur_opcode + iro_$op, \"$op\", op_pin_state_$state, $op_flags_joined, oparity_any, -1, $attr_size);\n";

	if ($mem >= 0) {
		$obst_new_irop .= "\tir_op_set_memory_index(op, $mem);";
	}

	my $dump_func = $n{dump_func} // "${arch}_dump_node";
	$obst_new_irop .= "\tset_op_dump(op, $dump_func);\n";

	# determine compare function
	my $attrs_equal_func = $n{attrs_equal};
	if (!defined($attrs_equal_func) && $attr_type ne "") {
		$attrs_equal_func = $attr_type;
		$attrs_equal_func =~ s/_t$//;
		$attrs_equal_func .= "s_equal";
	}
	if (defined($attrs_equal_func)) {
		$obst_new_irop .= "\tset_op_attrs_equal(op, $attrs_equal_func);\n";
	}

	# don't set a copy_attr function if the node has no additional attributes.
	if ($attr_type ne "") {
		$obst_new_irop .= "\tset_op_copy_attr(op, be_copy_attr);\n";
	}

	# determine hash function
	if (defined(my $hash_func = $n{hash_func})) {
		$obst_new_irop .= "\tset_op_hash(op, $hash_func);\n";
	}

	if ($is_fragile) {
		$obst_new_irop .= "\tir_op_set_memory_index(op, n_${op}_mem);\n";
		$obst_new_irop .= "\tir_op_set_fragile_indices(op, pn_${op}_X_regular, pn_${op}_X_except);\n";
	}
	$obst_new_irop .= "\tset_op_tag(op, ${arch}_op_tag);\n";
	if (defined(my $op_attr_init = $n{op_attr_init})) {
		$obst_new_irop .= "\t$op_attr_init\n";
	}
	$obst_new_irop .= "\top_$op = op;\n";

	$obst_free_irop .= "\tfree_ir_op(op_$op); op_$op = NULL;\n";

	$obst_enum_op .= "\tiro_$op,\n";
}
$obst_enum_op .= "\tiro_${arch}_last\n";
$obst_enum_op .= "} ${arch}_opcodes;\n\n";

# build the FOURCC arguments from $arch
my @four = split("", $arch);
my ($a, $b, $c, $d) = @four;
$a //= '\0';
$b //= '\0';
$c //= '\0';
$d //= '\0';

# emit the code

open(my $out_c, ">", $target_c) // die("Fatal error: Could not open $target_c, reason: $!\n");
print $out_c <<EOF;
#include "gen_${arch}_new_nodes.h"

#include "benode.h"
#include "${arch}_bearch_t.h"
#include "gen_${arch}_regalloc_if.h"
#include "${arch}_new_nodes_t.h"
#include "fourcc.h"
#include "irgopt.h"
#include "ircons_t.h"

$obst_opvar

static int ${arch}_opcode_start = -1;

/** A tag for the $arch opcodes. */
#define ${arch}_op_tag FOURCC('$a', '$b', '$c', '$d')

/** Return 1 if the given opcode is a $arch machine op, 0 otherwise */
int is_${arch}_op(const ir_op *op)
{
	return get_op_tag(op) == $arch\_op_tag;
}

/** Return 1 if the given node is a $arch machine node, 0 otherwise */
int is_${arch}_irn(const ir_node *node)
{
	return is_${arch}_op(get_irn_op(node));
}

int get_${arch}_irn_opcode(const ir_node *node)
{
	assert(is_${arch}_irn(node));
	return get_irn_opcode(node) - ${arch}_opcode_start;
}

#undef BIT
#define BIT(x)  (1 << (x))

$obst_limit_func
$obst_reg_reqs
$obst_constructor

/**
 * Creates the $arch specific Firm machine operations
 * needed for the assembler irgs.
 */
void ${arch}_create_opcodes(void)
{
	ir_op *op;
	int    cur_opcode = get_next_ir_opcodes(iro_${arch}_last);

	${arch}_opcode_start = cur_opcode;
$obst_new_irop
}

void ${arch}_free_opcodes(void)
{
$obst_free_irop
}
EOF
close($out_c);

my $creation_time = localtime(time());
my $uarch = uc($arch);

open(my $out_h, ">", $target_h) // die("Fatal error: Could not open $target_h, reason: $!\n");
print $out_h <<EOF;
/**
 * \@file
 * \@brief Function prototypes for the new opcode functions.
 * \@note  DO NOT EDIT THIS FILE, your changes will be lost.
 *        Edit $specfile instead.
 *        created by: $0 $specfile $target_dir
 * \@date  $creation_time
 */
#ifndef FIRM_BE_${uarch}_GEN_${uarch}_NEW_NODES_H
#define FIRM_BE_${uarch}_GEN_${uarch}_NEW_NODES_H

#include "be_types.h"
#include "irnode_t.h"
#include "${arch}_nodes_attr.h"

$obst_enum_op
int is_${arch}_irn(const ir_node *node);
int is_${arch}_op(const ir_op *op);

int get_${arch}_irn_opcode(const ir_node *node);
$obst_header
$obst_proj

#endif
EOF
close($out_h);

sub mangle_requirements
{
	my ($reqs, $class, $flags) = @_;

	my @alternatives = split(/ /, $reqs);
	for (my $idx = 0; $idx < scalar(@alternatives); $idx++) {
		$alternatives[$idx] =~ s/!/not_/g;
	}

	@alternatives = sort @alternatives;

	my $name = "${class}_".join('_', @alternatives);
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
sub is_reg_class
{
    my ($name) = @_;
    return exists($reg_classes{$name});
}

sub is_reg
{
	my ($name) = @_;
	return exists($reg2class{$name});
}

###
# Returns the register class for a given register.
# @return class or undef
###
sub get_reg_class
{
    my ($reg) = @_;
    return $reg2class{$reg}{class};
}

###
# Returns the index of a given register within its register class.
# @return index or undef
###
sub get_reg_index
{
    my ($reg) = @_;
    return $reg2class{$reg}{index};
}

###
# Remember the register class for each index in the given requirements.
# We need this information for requirements like "in_sX" or "out_dX"
# @return array of classes corresponding to the requirement for each index
###
sub get_in_req_class
{
	my ($n, $idx) = @_;

	my $reqs = $n->{in_reqs};
	if ($reqs && $reqs ne "..." && $idx < scalar(@$reqs)) {
		my ($req,) = split(/:/, @$reqs[$idx]);
		if (is_reg_class($req)) {
			return $req;
		} else {
			foreach my $reg (split(/ /, $req)) {
				if (my $class = get_reg_class($reg)) {
					return $class;
				}
			}
		}
	}

	return undef;
}

###
# Generates the function for a given $op and a given IN-index
# which returns a subset of possible register from a register class
# @return classname from which the subset is derived or undef and
#         pos which corresponds to in/out reference position or undef
###
sub build_subset_class_func
{
	# build function header
	my ($node, $op, $idx, $is_in, $regs, $flags) = @_;

	my $neg           = undef;
	my $class         = undef;
	my $has_limit     = 0;
	my $same_pos      = 0;
	my $different_pos = 0;
	my @limit_array;
	my $limit_reqs;   #used for name mangling

	# set/unset registers
CHECK_REQS: foreach (split(/ /, $regs)) {
		if (!$is_in && /(!)?in_r(\d+)/) {
			my $idx     = $2;
			my $bit_pos = 1 << $idx;
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

			$class = get_in_req_class($node, $idx);
			if (!$class) {
				die("Fatal error: Could not get in_reqs register class for '$op' index $idx ... exiting.\n");
			}
			next CHECK_REQS;
		}

		# check for negate
		if (substr($_, 0, 1) eq "!") {
			if (defined($neg) && !$neg) {
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
			if (defined($neg) && $neg) {
				# we have seen a negative constraint as first one but this one is positive
				# this doesn't make sense
				print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
				return (undef, undef, undef, undef);
			}

			$has_limit = 1;
			$neg = 0;
		}

		# check if register belongs to one of the given classes
		my $temp = get_reg_class($_);
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

	my $limit_name;
	if ($has_limit) {
		$limit_name = "${arch}_limit_".mangle_requirements($limit_reqs, $class);

		if (defined($limit_bitsets{$limit_name})) {
			$limit_name = $limit_bitsets{$limit_name};
			return ($class, $limit_name, $same_pos, $different_pos);
		}

		$limit_bitsets{$limit_name} = $limit_name;

		$obst_limit_func  .= "static const unsigned ${limit_name}[] = { ";
		my $sep            = "";
		my $limitbitsetlen = $regclass2len{$class};
		my $limitarraylen  = ($limitbitsetlen+31) / 32;
		for (my $i = 0; $i < $limitarraylen; $i++) {
			$obst_limit_func .= $sep;
			$sep              = ", ";
			my $temp = $neg ? "0xFFFFFFFF" : undef;
			foreach my $reg (@{$limit_array[$i]}) {
				if ($neg) {
					$temp .= " & ~";
				} elsif (defined($temp)) {
					$temp .= " | ";
				}
				my $classuc = uc($class);
				my $reguc = uc($reg);
				$temp .= "BIT(REG_${classuc}_$reguc)";
			}
			$obst_limit_func .= $temp // "0";
		}
		$obst_limit_func .= " };\n";
	}

	return ($class, $limit_name, $same_pos, $different_pos);
}

###
# Generate register requirements structure
###
sub generate_requirements
{
	my ($reqs_flags, $node, $op, $idx, $is_in) = @_;

	my ($reqs, $flags) = split(/:/, $reqs_flags);

	my $width = 1;
	my $extra = "";
	if (defined($flags)) {
		foreach my $f (split(/\|/, $flags)) {
			if ($f eq "I") {
				$extra .= "\n\t.ignore = true,";
			} elsif ($f eq "K") {
				$extra .= "\n\t.kills_value = true,";
			} elsif ($f eq "2" or $f eq "4" or $f eq "8") {
				$width = int($f);
			} else {
				die("Fatal error: unknown flag '$f'")
			}
		}
	}

	my $is_cls = 0;
	my $is_reg = 0;
	if ($reqs =~ s/^reg-//) {
		$is_reg = 1;
	} elsif ($reqs =~ s/^cls-//) {
		$is_cls = 1;
	} else {
		$is_cls = is_reg_class($reqs);
		$is_reg = is_reg($reqs);
		if ($is_cls && $is_reg) {
			die("Fatal error: $reqs is ambiguous (try reg-$reqs or cls-$reqs) at node $op")
		}
	}

	my $class;
	my $result;
	if ($reqs eq "exec") {
		return "arch_exec_requirement";
	} elsif ($reqs eq "mem") {
		return "arch_memory_requirement";
	} elsif ($reqs eq "none") {
		return "arch_no_requirement";
	} elsif ($is_cls) {
		if (!defined($flags)) {
			return "${arch}_class_reg_req_$reqs";
		}
		$class  = $reqs;
		$result = <<EOF;
{
	.cls   = &${arch}_reg_classes[CLASS_${arch}_$class],
	.width = $width,$extra
};
EOF
	} elsif ($is_reg && !defined($flags)) {
		$class = get_reg_class($reqs);
		return "${arch}_single_reg_req_${class}_$reqs";
	} else {
		my ($regclass, $limit_bitset, $same_pos, $different_pos)
			= build_subset_class_func($node, $op, $idx, $is_in, $reqs, $flags);

		if (!defined($regclass)) {
			die("Fatal error: Could not build subset for requirements '$reqs' of '$op' pos $idx ... exiting.\n");
		}

		$limit_bitset //= "NULL";

		$class  = $regclass;
		$result = <<EOF;
{
	.cls               = &${arch}_reg_classes[CLASS_${arch}_$class],
	.limited           = $limit_bitset,
	.should_be_same    = $same_pos,
	.must_be_different = $different_pos,
	.width             = $width,$extra
};
EOF
	}

	my $name = "${arch}_requirements_".mangle_requirements($reqs, $class, $flags);
	if (!defined($requirements{$name})) {
		$requirements{$name} = $name;
		$obst_reg_reqs .= "static const arch_register_req_t $name = $result\n";
	}

	return $name;
}
