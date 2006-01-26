#!/usr/bin/perl -w

# This script generates C code which emits assembler code for the
# assembler ir nodes. It takes a "emit" key from the node specification
# and substitutes lines starting with . with a corresponding fprintf().
# Creation: 2005/11/14
# $Id$

use strict;
use Data::Dumper;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %reg_classes;
our %nodes;

# include spec file

my $return;

no strict "subs";
unless ($return = do $specfile) {
	warn "couldn't parse $specfile: $@" if $@;
	warn "couldn't do $specfile: $!"    unless defined $return;
	warn "couldn't run $specfile"       unless $return;
}
use strict "subs";

my $target_c   = $target_dir."/gen_".$arch."_regalloc_if.c";
my $target_h   = $target_dir."/gen_".$arch."_regalloc_if.h";
my $target_h_t = $target_dir."/gen_".$arch."_regalloc_if_t.h";

# helper function
my @rt = ("arch_register_type_none",
          "arch_register_type_write_invariant",
          "arch_register_type_caller_saved",
          "arch_register_type_callee_saved",
          "arch_register_type_ignore");

# stacks for output
my @obst_regtypes;     # stack for the register type variables
my @obst_regclasses;   # stack for the register class variables
my @obst_classdef;     # stack to define a name for a class index
my @obst_regdef;       # stack to define a name for a register index
my @obst_reginit;      # stack for the register type inits
my @obst_req;          # stack for the register requirements
my @obst_limit_func;   # stack for functions to return a subset of a register class
my @obst_defreq_head;  # stack for prototypes of default requirement function
my @obst_header_all;   # stack for some extern struct defs needed for bearch_$arch include
my @obst_projnum_map;  # stack for mapping register projnums to requirements

my $numregs;
my $class_ptr;
my $class_idx = 0;

my $tmp;

my %reg2class;

# there is a default NONE requirement
$tmp = "/* Default NONE register requirements */\n";
$tmp .= "const $arch\_register_req_t $arch\_default_req_none = {\n";
$tmp .= "  {\n";
$tmp .= "    arch_register_req_type_none,\n";
$tmp .= "    NULL,\n";
$tmp .= "    NULL,\n";
$tmp .= "    NULL,\n";
$tmp .= "    NULL\n";
$tmp .= "  },\n";
$tmp .= "  0\n";
$tmp .= "};\n\n";
push(@obst_req, $tmp);

push(@obst_header_all, "extern arch_register_class_t $arch\_reg_classes[N_CLASSES];\n\n");
push(@obst_header_all, "extern const $arch\_register_req_t $arch\_default_req_none;\n");

push(@obst_classdef, "#define N_CLASSES ".scalar(keys(%reg_classes))."\n");

my $global_projnum_idx = 0;

# generate register type and class variable, init function and default requirements
foreach my $class_name (keys(%reg_classes)) {
	my @class         = @{ $reg_classes{"$class_name"} };
	my $old_classname = $class_name;

	$class_name = $arch."_".$class_name;
	$numregs    = "N_".$class_name."_REGS";
	$class_ptr  = "&".$arch."_reg_classes[CLASS_".$class_name."]";

	push(@obst_regtypes, "#define $numregs ".($#class + 1)."\n");
	push(@obst_regtypes, "arch_register_t ".$class_name."_regs[$numregs];\n\n");

	push(@obst_classdef, "#define CLASS_$class_name $class_idx\n");
	push(@obst_regclasses, "{ \"$class_name\", $numregs, ".$class_name."_regs }");

	# there is a default NORMAL requirement for each class
	$tmp  = "/* Default NORMAL register requirements for class $class_name */\n";
	$tmp .= "const $arch\_register_req_t $arch\_default_req_$class_name = {\n";
	$tmp .= "  {\n";
	$tmp .= "    arch_register_req_type_normal,\n";
	$tmp .= "    $class_ptr,\n";
	$tmp .= "    NULL,\n";
	$tmp .= "    NULL,\n";
	$tmp .= "    NULL\n";
	$tmp .= "  },\n";
	$tmp .= "  0\n";
	$tmp .= "};\n\n";
	push(@obst_req, $tmp);

	push(@obst_header_all, "\nextern const $arch\_register_req_t $arch\_default_req_$class_name;\n");

	my $idx = 0;
	push(@obst_reginit, "  /* Init of all registers in class '$class_name' */\n\n");
	foreach (@class) {
		# For each class we build for each of it's member registers a limit function
		# which limits the class to this particular register. We also build the
		# corresponding requirement structs.
		# We need those functions to set register requirements on demand in transformation
		# esp. for Call and RegParams where we can mix int and float parameters.

		my $limit_func_name = $arch."_limit_".$class_name."_".$_->{"name"};

		# push the function prototype
		$tmp = "void $limit_func_name(void *_unused, bitset_t *bs)";
		push(@obst_defreq_head, $tmp.";\n");

		# push the function definition
		$tmp .= " {\n";
		$tmp .= "    bs = bitset_clear_all(bs);\n";
		$tmp .= "    bitset_set(bs, REG_".uc($_->{"name"}).");\n";  # REGISTER to index assignment is done some lines down
		$tmp .= "}\n\n";
		push(@obst_limit_func, $tmp);

		# push the default requirement struct
		$tmp  = "const $arch\_register_req_t $arch\_default_req_$class_name\_".$_->{"name"}." = {\n";
		$tmp .= "  {\n";
		$tmp .= "    arch_register_req_type_limited,\n";
		$tmp .= "    $class_ptr,\n";
		$tmp .= "    $limit_func_name,\n";
		$tmp .= "    NULL,\n";
		$tmp .= "    NULL\n";
		$tmp .= "  },\n";
		$tmp .= "  0\n";
		$tmp .= "};\n\n";
		push(@obst_req, $tmp);

		push(@obst_header_all, "extern const $arch\_register_req_t $arch\_default_req_$class_name\_".$_->{"name"}.";\n");

		$reg2class{$_->{"name"}} = { "class" => $old_classname, "index" => $idx }; # remember reg to class for later use
		push(@obst_regdef, "#define REG_".uc($_->{"name"})." $idx\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].name      = \"".$_->{"name"}."\";\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].reg_class = $class_ptr;\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].index     = $idx;\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].type      = ".$rt[$_->{"type"}].";\n");
		if ($_->{"type"} == 2) {
			# this is a caller saved register
			push(@obst_reginit, "  ia32_set_reg_projnum(&".$class_name."_regs[$idx], $global_projnum_idx, isa->reg_projnum_map);\n");
			push(@obst_projnum_map, "&$arch\_default_req_$class_name\_".$_->{"name"});
			$global_projnum_idx++;
		}
		push(@obst_reginit, "\n");
		$idx++;
	}

	$class_idx++;
}

push(@obst_regdef, "\n#define N_CALLER_SAVE_REGS ".scalar(@obst_projnum_map)."\n");

push(@obst_header_all, "\nextern const $arch\_register_req_t *$arch\_projnum_reg_req_map[N_CALLER_SAVE_REGS];\n\n");
push(@obst_header_all, "\n/* node specific requirements */\n");

# generate node-register constraints
foreach my $op (keys(%nodes)) {
	my %n = %{ $nodes{"$op"} };

	next if (!exists($n{"reg_req"}));

	$op = $arch."_".$op;

	push(@obst_req, "/* IN requirements for '$op' */\n");
	# check for argument requirements
	if (exists($n{"reg_req"}{"in"})) {
		generate_requirements(\%n, $op, "in");
	}

	push(@obst_req, "/* OUT requirements for '$op' */\n");
	# check for result requirements
	if (exists($n{"reg_req"}{"out"})) {
		generate_requirements(\%n, $op, "out");
	}
}



# generate header _t (internal usage) file
open(OUT, ">$target_h_t") || die("Could not open $target_h_t, reason: $!\n");

my $creation_time = localtime(time());

$tmp = uc($arch);

print OUT<<EOF;
#ifndef _GEN_$tmp\_REGALLOC_IF_T_H_
#define _GEN_$tmp\_REGALLOC_IF_T_H_

/**
 * Generated register classes from spec.
 *
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#include "../bearch.h"
#include "$arch\_nodes_attr.h"

EOF

print OUT @obst_regdef, "\n";

print OUT @obst_classdef, "\n";

print OUT @obst_regtypes, "\n";

print OUT @obst_defreq_head, "\n";

print OUT "void ".$arch."_register_init(void *isa_ptr);\n\n";

print OUT "\n#endif /* _GEN_$tmp\_REGALLOC_IF_T_H_ */\n";



# generate header (external usage) file
open(OUT, ">$target_h") || die("Could not open $target_h, reason: $!\n");

$creation_time = localtime(time());

print OUT<<EOF;
#ifndef _GEN_$tmp\_REGALLOC_IF_H_
#define _GEN_$tmp\_REGALLOC_IF_H_

/**
 * Contains additional external requirements defs for external includes.
 *
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#include "gen_$arch\_regalloc_if_t.h"

EOF

print OUT @obst_header_all;

print OUT "\n#endif /* _GEN_$tmp\_REGALLOC_IF_H_ */\n";

close(OUT);



# generate c inline file
open(OUT, ">$target_c") || die("Could not open $target_c, reason: $!\n");

$creation_time = localtime(time());

print OUT<<EOF;
/**
 * The generated interface for the register allocator.
 * Contains register classes and types and register constraints
 * for all nodes where constraints were given in spec.
 *
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#include "gen_$arch\_regalloc_if.h"
#include "bearch_ia32_t.h"   /* we need this to put the caller saved registers into the isa set */
#include "ia32_map_regs.h"

EOF

print OUT "arch_register_class_t $arch\_reg_classes[] = {\n  ".join(",\n  ", @obst_regclasses)."\n};\n\n";

print OUT "const $arch\_register_req_t *$arch\_projnum_reg_req_map[] = {\n  ".join(",\n  ", @obst_projnum_map)."\n};\n\n";

print OUT "void ".$arch."_register_init(void *isa_ptr) {\n";
print OUT "  ia32_isa_t *isa = (ia32_isa_t *)isa_ptr;\n\n";
print OUT @obst_reginit;
print OUT "}\n\n";

print OUT @obst_limit_func;
print OUT @obst_req;

close(OUT);

###
# Remember the register class for each index in the given requirements.
# We need this information for requirements like "in_sX" or "out_dX"
# @return array of classes corresponding to the requirement for each index
###
sub build_inout_idx_class {
	my $n     = shift;
	my $op    = shift;
	my $inout = shift;
	my @idx_class;

	if (exists($n->{"reg_req"}{"$inout"})) {
		my @reqs = @{ $n->{"reg_req"}{"$inout"} };

		for (my $idx = 0; $idx <= $#reqs; $idx++) {
			my $class = undef;

			if ($reqs[$idx] eq "none") {
				$class = "none";
			}
			elsif (is_reg_class($reqs[$idx])) {
				$class = $reqs[$idx];
			}
			else {
				my @regs = split(/ /, $reqs[$idx]);
GET_CLASS:		foreach my $reg (@regs) {
					if ($reg =~ /!?(in|out)\_r\d+/) {
						$class = "UNKNOWN_CLASS";
					}
					else {
						$class = get_reg_class($reg);
						if (!defined $class) {
							die("Could not get ".uc($inout)." register class for '$op' pos $idx (reg $reg) ... exiting.\n");
						}
						else {
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
# Generates the requirements for the given description
###
sub generate_requirements {
	my $n     = shift;
	my $op    = shift;
	my $inout = shift;

	# get classes for the complementary direction
	my $outin     = ($inout eq "in") ? "out" : "in";

	my @reqs = @{ $n->{"reg_req"}{"$inout"} };

	for (my $idx = 0; $idx <= $#reqs; $idx++) {
		my $class = undef;

		my $tmp2 = "const $arch\_register_req_t _".$op."_reg_req_$inout\_$idx = ";
		my $tmp  = "const $arch\_register_req_t *".$op."_reg_req_$inout\_$idx = ";

		push(@obst_header_all, "extern const $arch\_register_req_t *".$op."_reg_req_$inout\_$idx;\n");

		if ($reqs[$idx] eq "none") {
			$tmp .= "&$arch\_default_req_none;\n";
		}
		elsif (is_reg_class($reqs[$idx])) {
			$tmp .= "&$arch\_default_req_".$arch."_".$reqs[$idx].";\n";
		}
		else {
			my @req_type_mask;
			my ($class, $has_limit, $pos, $same) = build_subset_class_func($n, $op, $idx, (($inout eq "in") ? 1 : 0), $reqs[$idx]);
			if (!defined($class)) {
				die("Could not build subset for ".uc($inout)." requirements '$op' pos $idx ... exiting.\n");
			}
			if ($has_limit) {
				push(@req_type_mask, "arch_register_req_type_limited");
			}
			if (defined($pos)) {
				push(@req_type_mask, "arch_register_req_type_should_be_".($same ? "same" : "different"));
			}
			$tmp  .= "&_".$op."_reg_req_$inout\_$idx;\n";
			$tmp2 .= " {\n";
			$tmp2 .= "  {\n";
			$tmp2 .= "    ".join(" | ", @req_type_mask).",\n";
			$tmp2 .= "    &$arch\_reg_classes[CLASS_$arch\_".$class."],\n";
			$tmp2 .= "    ".($has_limit ? "limit_reg_".$op."_$inout\_".$idx : "NULL").",\n";
			$tmp2 .= "    NULL,\n";
			$tmp2 .= "    NULL\n";
			$tmp2 .= "  },\n";
			$tmp2 .= "  ".(defined($pos) ? $pos : "0")."\n};\n";

			$tmp   = $tmp2.$tmp;
		}

		push(@obst_req, $tmp."\n");
	}

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
	return $reg2class{"$reg"}{"class"} if (exists($reg2class{"$reg"}));
	return undef;
}

###
# Returns the index of a given register within it's register class.
# @return index or undef
###
sub get_reg_index {
	my $reg = shift;
	return $reg2class{"$reg"}{"index"} if (exists($reg2class{"$reg"}));
	return undef;
}

###
# Generates the function for a given $op and a given IN-index
# which returns a subset of possible register from a register class
# @return classname from which the subset is derived or undef and
#         pos which corresponds to in/out reference position or undef
###
sub build_subset_class_func {
	my $neg   = undef;
	my $class = undef;
	my $temp;
	my $has_limit = 0;

	# build function header
	my $n    = shift;
	my $op   = shift;
	my $idx  = shift;
	my $in   = shift;
	my $pos  = undef;
	my $same = 1;

	my @temp_obst;

	my $outin = $in ? "out" : "in";
	my @regs  = split(/ /, shift);

	my @idx_class = build_inout_idx_class($n, $op, $outin);

	# set/unset registers
CHECK_REQS: foreach (@regs) {
		if (/(!)?$outin\_r(\d+)/) {
			if (defined($pos)) {
				print STDERR "Multiple in/out references in one requirement not allowed.\n";
				return (undef, undef, undef, undef);
			}
			$same  = 0 if ($1);
			$class = $idx_class[$2 - 1];
			$pos   = $in ? -$2 : $2 - 1;
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
				push(@temp_obst, "  bs = bitset_set_all(bs);     /* allow all register (negative constraints given) */\n");
			}

			$_   = substr($_, 1); # skip '!'
			$neg = 1;
		}
		else {
			if (defined($neg) && $neg == 1) {
				# we have seen a negative constraint as first one but this one is positive
				# this doesn't make sense
				print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
				return (undef, undef, undef, undef);
			}

			if (!defined($neg)) {
				$has_limit = 1;
				push(@temp_obst, "  bs = bitset_clear_all(bs);   /* disallow all register (positive constraints given) */\n");
			}
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
		}
		elsif ($class ne $temp) {
			# all registers must belong to the same class
			print STDERR "Registerclass mismatch. '$_' is not member of class '$class'.\n";
			return (undef, undef, undef, undef);
		}

		if ($neg == 1) {
			$has_limit = 1;
			push(@temp_obst, "  bitset_clear(bs, ".get_reg_index($_).");         /* disallow $_ */\n");
		}
		else {
			$has_limit = 1;
			push(@temp_obst, "  bitset_set(bs, ".get_reg_index($_).");           /* allow $_ */\n");
		}
	}

	if ($has_limit == 1) {
		push(@obst_limit_func, "/* limit the possible registers for ".($in ? "IN" : "OUT")." $idx at op $op */\n");
		push(@obst_limit_func, "void limit_reg_".$op."_".($in ? "in" : "out")."_".$idx."(void *_unused, bitset_t *bs) {\n");
		push(@obst_limit_func, @temp_obst);
		push(@obst_limit_func, "}\n\n");
	}

	return ($class, $has_limit, $pos, $same);
}
