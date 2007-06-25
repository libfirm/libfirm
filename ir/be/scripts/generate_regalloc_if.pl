#!/usr/bin/perl -w

#
# Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
#
# This file is part of libFirm.
#
# This file may be distributed and/or modified under the terms of the
# GNU General Public License version 2 as published by the Free Software
# Foundation and appearing in the file LICENSE.GPL included in the
# packaging of this file.
#
# Licensees holding valid libFirm Professional Edition licenses may use
# this file in accordance with the libFirm Commercial License.
# Agreement provided with the Software.
#
# This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
# WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.
#

# This script generates C code which creates ands sets up functions and
# data structures for the register allocator.
# Creation: 2005/11/14
# $Id$

use strict;
use Data::Dumper;
use integer;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %reg_classes;
our %nodes;
our %cpu;
our %flags = ();

# include spec file

my $return;

use strict "subs";
unless ($return = do $specfile) {
	die "Fatal error: couldn't parse $specfile: $@" if $@;
	die "Fatal error: couldn't do $specfile: $!"    unless defined $return;
	die "Fatal error: couldn't run $specfile"       unless $return;
}
use strict "subs";

my $target_c   = $target_dir."/gen_".$arch."_regalloc_if.c";
my $target_h   = $target_dir."/gen_".$arch."_regalloc_if.h";

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

		if ($t & 8) {
			push(@types, "arch_register_type_joker");
		}

		if ($t & 16) {
			push(@types, "arch_register_type_virtual");
		}

		if ($t & 32) {
			push(@types, "arch_register_type_state");
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
my @obst_header_all;   # stack for some extern struct defs needed for bearch_$arch include

my $numregs;
my $class_ptr;
my $class_idx = 0;

my $tmp;

my %reg2class;
my %regclass2len;

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

	push(@obst_regtypes_decl, "extern const arch_register_t ${class_name}_regs[$numregs];\n");

	push(@obst_classdef, "\tCLASS_$class_name = $class_idx,\n");
	push(@obst_regclasses, "{ \"$class_name\", $numregs, NULL, ".$class_name."_regs }");

	my $idx = 0;
	push(@obst_reginit, "\t/* set largest possible mode for '$class_name' */\n");
	push(@obst_reginit, "\t$arch\_reg_classes[CLASS_".$class_name."].mode = $class_mode;\n\n");
	push(@obst_regtypes_def, "const arch_register_t ${class_name}_regs[$numregs] = {\n");

	push(@obst_regdef, "enum reg_${class_name}_indices {\n");
	foreach (@class) {
		my $ucname = uc($_->{"name"});
		my $type = translate_reg_type($_->{"type"});
		# realname is name if not set by user
		$_->{"realname"} = $_->{"name"} if (! exists($_->{"realname"}));
		my $realname = $_->{realname};
		my $execunitvarname = get_execunit_variable_name($_->{"unit"});


		$reg2class{$_->{"name"}} = { "class" => $old_classname, "index" => $idx }; # remember reg to class for later use
		push(@obst_regdef, "\tREG_${ucname},\n");

		push(@obst_regtypes_def, "\t{\n");
		push(@obst_regtypes_def, "\t\t\"$realname\",\n");
		push(@obst_regtypes_def, "\t\t$class_ptr,\n");
		push(@obst_regtypes_def, "\t\tREG_${ucname},\n");
		push(@obst_regtypes_def, "\t\t$type,\n");
		push(@obst_regtypes_def, "\t\t$execunitvarname\n");
		push(@obst_regtypes_def, "\t},\n");

		$idx++;
	}
	push(@obst_regtypes_def, "};\n");

	$regclass2len{$old_classname} = $idx;
	push(@obst_regdef, "\t$numregs = $idx\n");
	push(@obst_regdef, "};\n\n");

	$class_idx++;
}

push(@obst_regdef, "enum flag_indices {\n");
foreach my $flag (keys(%flags)) {
	my %f = %{ $flags{$flag} };

	push(@obst_regdef, "\tFLAG_$flag,\n");
}
push(@obst_regdef, "\tFLAG_LAST\n");
push(@obst_regdef, "};\n");
push(@obst_regtypes_decl, "extern arch_flag_t ${arch}_flags[];\n");

push(@obst_classdef, "\tN_CLASSES = ".scalar(keys(%reg_classes))."\n");
push(@obst_classdef, "};\n\n");

$tmp = uc($arch);

# generate header (external usage) file
open(OUT, ">$target_h") || die("Fatal error: Could not open $target_h, reason: $!\n");

my $creation_time = localtime(time());

print OUT<<EOF;
/**
 * \@file
 * \@brief Contains additional external requirements defs for external includes.
 * \@note   DO NOT EDIT THIS FILE, your changes will be lost.
 *         Edit $specfile instead.
 *         created by: $0 $specfile $target_dir
 * \@date   $creation_time
 */
#ifndef FIRM_BE_${tmp}_GEN_${tmp}_REGALLOC_IF_H
#define FIRM_BE_${tmp}_GEN_${tmp}_REGALLOC_IF_H

#include "../bearch.h"
#include "${arch}_nodes_attr.h"

EOF

print OUT @obst_regdef, "\n";

print OUT @obst_classdef, "\n";

print OUT @obst_regtypes_decl, "\n";

print OUT "extern arch_register_class_t $arch\_reg_classes[N_CLASSES];\n\n";

print OUT "void ".$arch."_register_init(void);\n\n";

print OUT @obst_header_all, "\n";

print OUT "\n#endif\n";

close(OUT);



# generate c file
open(OUT, ">$target_c") || die("Fatal error: Could not open $target_c, reason: $!\n");

$creation_time = localtime(time());

print OUT<<EOF;
/**
 * \@file
 * \@brief  The generated interface for the register allocator.
 *          Contains register classes and types and register constraints
 *          for all nodes where constraints were given in spec.
 * \@note    DO NOT EDIT THIS FILE, your changes will be lost.
 *          Edit $specfile instead.
 *          created by: $0 $specfile $target_dir
 * \$date    $creation_time
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "gen_${arch}_regalloc_if.h"
#include "gen_${arch}_machine.h"
#include "bearch_${arch}_t.h"
#include "${arch}_map_regs.h"
#include "irmode.h"

EOF

print OUT "arch_register_class_t ${arch}_reg_classes[] = {\n\t".join(",\n\t", @obst_regclasses)."\n};\n\n";

print OUT @obst_regtypes_def, "\n";

print OUT "void ${arch}_register_init(void) {\n";
print OUT @obst_reginit;
print OUT "}\n\n";

print OUT @obst_limit_func;

print OUT @obst_req;

close(OUT);

###
# Gets the variable name for the execution unit assigned to this register.
###
sub get_execunit_variable_name {
	my $unit    = shift;
	my $name    = "NULL";
	my $uc_arch = uc($arch);

	if ($unit) {
		my $found = 0;
SRCH:	foreach my $cur_type (keys(%cpu)) {
			foreach my $cur_unit (@{ $cpu{"$cur_type"} }) {
				if ($unit eq $cur_unit) {
					my $tp_name   = "$arch\_execution_units_$cur_type";
					my $unit_name = "$uc_arch\_EXECUNIT_TP_$cur_type\_$unit";
					$name  = "&".$tp_name."[".$unit_name."]";
					$found = 1;
					last SRCH;
				}
			}
		}

		if (! $found) {
			print STDERR "Invalid execution unit $unit specified!\n";
		}
	}

	return $name;
}
