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
sub translate_reg_type {
	my $t = shift;

	if ($t == 0) {
		return "arch_register_type_none";
	}
	else {
		my @types;

		if ($t & 1) {
			push(@types, "arch_register_type_caller_save");
		}

		if ($t & 2) {
			push(@types, "arch_register_type_callee_save");
		}

		if ($t & 4) {
			push(@types, "arch_register_type_ignore");
		}

		return join(" | ", @types);
	}
}

# stacks for output
my @obst_regtypes_def; # stack for the register type variables definitions
my @obst_regtypes_decl;# stack for the register type variables declarations
my @obst_regclasses;   # stack for the register class variables
my @obst_classdef;     # stack to define a name for a class index
my @obst_regdef;       # stack to define a name for a register index
my @obst_reginit;      # stack for the register type inits
my @obst_req;          # stack for the register requirements
my @obst_limit_func;   # stack for functions to return a subset of a register class
my @obst_defreq_head;  # stack for prototypes of default requirement function
my @obst_header_all;   # stack for some extern struct defs needed for bearch_$arch include
my @obst_requirement_def;  # stack for requirement name defines

my $numregs;
my $class_ptr;
my $class_idx = 0;

my $tmp;

my %reg2class;

# there is a default NONE requirement
$tmp = "/* Default NONE register requirements */\n";
$tmp .= "const $arch\_register_req_t $arch\_default_req_none = {\n";
$tmp .= "  {\n";
$tmp .= "    arch_register_req_type_none,  /* register type  */\n";
$tmp .= "    NULL,                         /* register class */\n";
$tmp .= "    NULL,                         /* limit function */\n";
$tmp .= "    NULL,                         /* limit environment */\n";
$tmp .= "    NULL,                         /* node for same */\n";
$tmp .= "    NULL                          /* node for different */\n";
$tmp .= "  },\n";
$tmp .= "  0,                              /* same pos */\n";
$tmp .= "  0                               /* different pos */\n";
$tmp .= "};\n\n";
push(@obst_req, $tmp);
push(@obst_header_all, "extern const $arch\_register_req_t $arch\_default_req_none;\n");

push(@obst_classdef, "enum reg_classes {\n");

my $class_mode;

# generate register type and class variable, init function and default requirements
foreach my $class_name (keys(%reg_classes)) {
	my @class         = @{ $reg_classes{"$class_name"} };
	my $old_classname = $class_name;

	$class_name = $arch."_".$class_name;
	$numregs    = "N_".$class_name."_REGS";
	$class_ptr  = "&".$arch."_reg_classes[CLASS_".$class_name."]";
	$class_mode = pop(@class)->{"mode"};

	push(@obst_regtypes_decl, "extern arch_register_t ".$class_name."_regs[$numregs];\n");
	push(@obst_regtypes_def, "arch_register_t ".$class_name."_regs[$numregs];\n");

	push(@obst_classdef, "  CLASS_$class_name = $class_idx,\n");
	push(@obst_regclasses, "{ \"$class_name\", $numregs, NULL, ".$class_name."_regs }");

	# there is a default NORMAL requirement for each class
	$tmp  = "/* Default NORMAL register requirements for class $class_name */\n";
	$tmp .= "const $arch\_register_req_t $arch\_default_req_$class_name = {\n";
	$tmp .= "  {\n";
	$tmp .= "    arch_register_req_type_normal,\n";
	$tmp .= "    $class_ptr,\n";
	$tmp .= "    NULL,                    /* limit function */ \n";
	$tmp .= "    NULL,                    /* limit environment */\n";
	$tmp .= "    NULL,                    /* node for same */\n";
	$tmp .= "    NULL                     /* node for different */\n";
	$tmp .= "  },\n";
	$tmp .= "  0,                         /* same pos */\n";
	$tmp .= "  0                          /* different pos */\n";
	$tmp .= "};\n\n";
	push(@obst_req, $tmp);
	push(@obst_header_all, "extern const $arch\_register_req_t $arch\_default_req_$class_name;\n");

	my $idx = 0;
	push(@obst_reginit, "  /* Init of all registers in class '$class_name' */\n\n");
	push(@obst_reginit, "  /* set largest possible mode for '$class_name' */\n");
	push(@obst_reginit, "  $arch\_reg_classes[CLASS_".$class_name."].mode = $class_mode;\n\n");
	push(@obst_regdef, "enum reg_".$class_name."_values {\n");
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
		$tmp .= "    NULL,                     /* limit environment */\n";
		$tmp .= "    NULL,                     /* node for same */\n";
		$tmp .= "    NULL                      /* node for different */\n";
		$tmp .= "  },\n";
		$tmp .= "  0,                          /* same pos */\n";
		$tmp .= "  0                           /* different pos */\n";
		$tmp .= "};\n\n";
		push(@obst_req, $tmp);
		push(@obst_header_all,"extern const $arch\_register_req_t $arch\_default_req_$class_name\_".$_->{"name"}.";\n");

		$reg2class{$_->{"name"}} = { "class" => $old_classname, "index" => $idx }; # remember reg to class for later use
		push(@obst_regdef, "  REG_".uc($_->{"name"})." = $idx,\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].name      = \"".$_->{"name"}."\";\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].reg_class = $class_ptr;\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].index     = $idx;\n");
		push(@obst_reginit, "  ".$class_name."_regs[$idx].type      = ".translate_reg_type($_->{"type"}).";\n");
		push(@obst_reginit, "\n");
		$idx++;
	}
	push(@obst_regdef, "  $numregs = $idx\n");
	push(@obst_regdef, "};\n\n");

	$class_idx++;
}

push(@obst_classdef, "  N_CLASSES = ".scalar(keys(%reg_classes))."\n");
push(@obst_classdef, "};\n\n");

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

EOF

print OUT @obst_requirement_def;

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

#include "../bearch.h"
#include "$arch\_nodes_attr.h"

EOF

print OUT @obst_regdef, "\n";

print OUT @obst_classdef, "\n";

print OUT @obst_regtypes_decl, "\n";

print OUT "extern arch_register_class_t $arch\_reg_classes[N_CLASSES];\n\n";

print OUT "void ".$arch."_register_init(void *isa_ptr);\n\n";

print OUT @obst_header_all, "\n";

print OUT @obst_defreq_head, "\n";

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
#include "bearch_$arch\_t.h"   /* we need this to put the caller saved registers into the isa set */
#include "$arch\_map_regs.h"
#include "irmode.h"

EOF

print OUT "arch_register_class_t $arch\_reg_classes[] = {\n  ".join(",\n  ", @obst_regclasses)."\n};\n\n";

print OUT @obst_regtypes_def, "\n";

print OUT "void ".$arch."_register_init(void *isa_ptr) {\n";
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
		my $tmp  = "#define ".$op."_reg_req_$inout\_$idx ";

		if ($reqs[$idx] eq "none") {
			$tmp .= "&$arch\_default_req_none\n";
		}
		elsif ($reqs[$idx] =~ /^new_reg_(.*)$/) {
			if (is_reg_class($1)) {
				$tmp  .=  "&_".$op."_reg_req_$inout\_$idx\n";
				$tmp2 .= "{\n";
				$tmp2 .= "  {\n";
				$tmp2 .= "    arch_register_req_type_should_be_different_from_all,\n";
				$tmp2 .= "    &$arch\_reg_classes[CLASS_$arch\_".$1."],\n";
				$tmp2 .= "    NULL,        /* limit function */\n";
				$tmp2 .= "    NULL,        /* limit environment */\n";
				$tmp2 .= "    NULL,        /* same node */\n";
				$tmp2 .= "    NULL         /* different node */\n";
				$tmp2 .= "  },\n";
				$tmp2 .= "  0,\n";
				$tmp2 .= "  0\n";
				$tmp2 .= "};\n";

				push(@obst_req, $tmp2."\n");
				push(@obst_header_all, "extern const $arch\_register_req_t _".$op."_reg_req_$inout\_$idx;\n");
			}
			else {
				print STDERR "Invalid register class '$1' given in OUT requirement $idx for '$op'.\n";
			}
		}
		elsif (is_reg_class($reqs[$idx])) {
			$tmp .= "&$arch\_default_req_".$arch."_".$reqs[$idx]."\n";
		}
		else {
			my @req_type_mask;
			my ($class, $has_limit, $same_pos, $different_pos) = build_subset_class_func($n, $op, $idx, (($inout eq "in") ? 1 : 0), $reqs[$idx]);

			if (!defined($class)) {
				die("Could not build subset for ".uc($inout)." requirements '$op' pos $idx ... exiting.\n");
			}

			if ($has_limit) {
				push(@req_type_mask, "arch_register_req_type_limited");
			}
			if (defined($same_pos)) {
				push(@req_type_mask, "arch_register_req_type_should_be_same");
			}
			if (defined($different_pos)) {
				push(@req_type_mask, "arch_register_req_type_should_be_different");
			}

			$tmp  .= "&_".$op."_reg_req_$inout\_$idx\n";
			$tmp2 .= "{\n";
			$tmp2 .= "  {\n";
			$tmp2 .= "    ".join(" | ", @req_type_mask).",\n";
			$tmp2 .= "    &$arch\_reg_classes[CLASS_$arch\_".$class."],\n";
			$tmp2 .= "    ".($has_limit ? "limit_reg_".$op."_$inout\_".$idx : "NULL").",\n";
			$tmp2 .= "    NULL,        /* limit environment */\n";
			$tmp2 .= "    NULL,        /* same node */\n";
			$tmp2 .= "    NULL         /* different node */\n";
			$tmp2 .= "  },\n";
			$tmp2 .= "  ".(defined($same_pos) ? $same_pos : "0").",\n";
			$tmp2 .= "  ".(defined($different_pos) ? $different_pos : "0")."\n";
			$tmp2 .= "};\n";

			push(@obst_req, $tmp2."\n");
			push(@obst_header_all, "extern const $arch\_register_req_t _".$op."_reg_req_$inout\_$idx;\n");
		}

		push(@obst_requirement_def, $tmp);
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
	$reg = substr($reg, 1) if ($reg =~ /!.*/);
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
	my $neg           = undef;
	my $class         = undef;
	my $has_limit     = 0;
	my $same_pos      = undef;
	my $different_pos = undef;
	my $temp;
	my @temp_obst;


	# build function header
	my $n   = shift;
	my $op  = shift;
	my $idx = shift;
	my $in  = shift;

	my $outin = $in ? "out" : "in";
	my @regs  = split(/ /, shift);

	my @idx_class = build_inout_idx_class($n, $op, $outin);

	# set/unset registers
CHECK_REQS: foreach (@regs) {
		if (/(!)?$outin\_r(\d+)/) {
			if (($1 && defined($different_pos)) || (!$1 && defined($same_pos))) {
				print STDERR "Multiple in/out references of same type in one requirement not allowed.\n";
				return (undef, undef, undef, undef);
			}

			if ($1) {
				$different_pos = $in ? -$2 : $2 - 1;
			}
			else {
				$same_pos = $in ? -$2 : $2 - 1;
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

	my @cur_class = @{ $reg_classes{"$class"} };
	for (my $idx = 0; $idx <= $#cur_class; $idx++) {
		if (defined($cur_class[$idx]{"type"}) && ($cur_class[$idx]{"type"} & 4)) {
			push(@temp_obst, "  bitset_clear(bs, ".get_reg_index($cur_class[$idx]{"name"}).");");
			push(@temp_obst, "         /* disallow ignore reg ".$cur_class[$idx]{"name"}." */\n");
		}
	}

	if ($has_limit == 1) {
		push(@obst_header_all, "void limit_reg_".$op."_".($in ? "in" : "out")."_".$idx."(void *_unused, bitset_t *bs);\n");

		push(@obst_limit_func, "/* limit the possible registers for ".($in ? "IN" : "OUT")." $idx at op $op */\n");
		push(@obst_limit_func, "void limit_reg_".$op."_".($in ? "in" : "out")."_".$idx."(void *_unused, bitset_t *bs) {\n");
		push(@obst_limit_func, @temp_obst);
		push(@obst_limit_func, "}\n\n");
	}

	return ($class, $has_limit, $same_pos, $different_pos);
}
