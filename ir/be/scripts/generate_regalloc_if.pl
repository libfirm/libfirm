#!/usr/bin/perl -w

#
# Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
our %cpu;

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
my $regtypes_def; # stack for the register type variables definitions
my $regtypes_decl;# stack for the register type variables declarations
my @regclasses;   # stack for the register class variables
my $classdef;     # stack to define a name for a class index
my $regdef;       # stack to define a name for a register index
my $regdef2;
my $regcounts;
my $reginit;      # stack for the register type inits
my $single_constraints_decls;
my $single_constraints;

my $class_ptr;
my $class_idx = 0;

my %regclass2len = ();
my %reg2class = ();

$classdef .= "enum reg_classes {\n";

my $class_mode;

foreach my $class_name (keys(%reg_classes)) {
	my @class = @{ $reg_classes{"$class_name"} };

	my $idx = 0;
	foreach (@class) {
		if (defined($_->{name})) {
			$reg2class{$_->{name}} = {
				"class" => $class_name,
				"index" => $idx
			};
		}
		$idx++;
	}
	$regclass2len{$class_name} = $idx;
}

sub get_limited_array {
	my $reg      = shift;
	my $regclass = $reg2class{"$reg"}{"class"};
	my $ucname   = uc($reg);
	my $result   = "{ ";

	my $limitedbitsetlen = $regclass2len{$regclass};
	my $arraylen         = ($limitedbitsetlen+31) / 32;
	my $firstreg         = uc($reg_classes{$regclass}[0]->{"name"});
	my $classuc          = uc($regclass);
	my $first            = 1;
	for (my $i = 0; $i < $arraylen; ++$i) {
		if ($first) {
			$first = 0;
		} else {
			$result .= ", ";
		}

		my $index = $reg2class{"$reg"}{"index"};
		if ($index >= $i*32 && $index < ($i+1)*32) {
			if ($i > 0) {
				$result .= "(1 << (REG_${classuc}_${ucname} % 32))";
			} else {
				$result .= "(1 << REG_${classuc}_${ucname})";
			}
		} else {
			$result .= "0";
		}
	}
	$result .= " }";
}

# generate register type and class variable, init function and default requirements
foreach my $class_name (keys(%reg_classes)) {
	my @class         = @{ $reg_classes{"$class_name"} };
	my $old_classname = $class_name;

	$class_name = $arch."_".$class_name;
	$class_ptr  = "&".$arch."_reg_classes[CLASS_".$class_name."]";
	my $flags = pop(@class);
	$class_mode  = $flags->{"mode"};
	my $class_flags = $flags->{"flags"};
	my $flags_prepared = "";

	if(defined($class_flags)) {
		my $first = 1;
		foreach my $flag (split(/\|/, $class_flags)) {
			if(!$first) {
				$flags_prepared .= "|";
			} else {
				$first = 0;
			}
			$flags_prepared .= "arch_register_class_flag_$flag";
		}
	} else {
		$flags_prepared = "0";
	}

	$single_constraints_decls .= <<EOF;
static const arch_register_req_t ${arch}_class_reg_req_${old_classname};
EOF

	$single_constraints .= <<EOF;
static const arch_register_req_t ${arch}_class_reg_req_${old_classname} = {
	arch_register_req_type_normal,
	&${arch}_reg_classes[CLASS_${arch}_${old_classname}],
	NULL,
	0,
	0,
	1
};
EOF

	$classdef .= "\tCLASS_$class_name = $class_idx,\n";
	my $numregs = @class;
	my $first_reg = "&${arch}_registers[REG_". uc($class[0]->{"name"}) . "]";
	push(@regclasses, "{ $class_idx, \"$class_name\", $numregs, NULL, $first_reg, $flags_prepared, &${arch}_class_reg_req_${old_classname} }");

	my $idx = 0;
	$reginit .= "\t$arch\_reg_classes[CLASS_".$class_name."].mode = $class_mode;\n";
	my $lastreg;
	foreach (@class) {
		my $name   = $_->{"name"};
		my $ucname = uc($name);
		my $type   = "arch_register_type_none";
		$type = translate_reg_type($_->{"type"}) if (exists($_->{"type"}));
		# realname is name if not set by user
		$_->{"realname"} = $_->{"name"} if (! exists($_->{"realname"}));
		my $realname = $_->{realname};
		my $classuc = uc($old_classname);

		$regdef  .= "\tREG_${ucname},\n";
		$regdef2 .= "\tREG_${classuc}_${ucname} = $idx,\n";

		$regtypes_def .= <<EOF;
	{
		"${realname}",
		${class_ptr},
		REG_${classuc}_${ucname},
		REG_${ucname},
		${type},
		&${arch}_single_reg_req_${old_classname}_${name}
	},
EOF

		my $limitedarray = get_limited_array($name);
		$single_constraints .= <<EOF;
static const unsigned ${arch}_limited_${old_classname}_${name} [] = ${limitedarray};
static const arch_register_req_t ${arch}_single_reg_req_${old_classname}_${name} = {
	arch_register_req_type_limited,
	${class_ptr},
	${arch}_limited_${old_classname}_${name},
	0,
	0,
	1
};
EOF

		$lastreg = $ucname;
		$idx++;
	}
	$regcounts .= "\tN_${class_name}_REGS = $numregs,\n";

	$class_idx++;
}

my $archuc = uc($arch);

$classdef .= "\tN_${archuc}_CLASSES = ".scalar(keys(%reg_classes))."\n";
$classdef .= "};\n\n";

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
#ifndef FIRM_BE_${archuc}_GEN_${archuc}_REGALLOC_IF_H
#define FIRM_BE_${archuc}_GEN_${archuc}_REGALLOC_IF_H

#include "../bearch.h"
#include "${arch}_nodes_attr.h"

enum reg_indices {
${regdef}
	N_${archuc}_REGISTERS
};
enum {
${regdef2}
};

enum {
${regcounts}
};
${classdef}

extern const arch_register_t ${arch}_registers[N_${archuc}_REGISTERS];

extern arch_register_class_t ${arch}_reg_classes[N_${archuc}_CLASSES];

void ${arch}_register_init(void);
unsigned ${arch}_get_n_regs(void);

#endif
EOF
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
#include "config.h"

#include "gen_${arch}_regalloc_if.h"
#include "gen_${arch}_machine.h"
#include "bearch_${arch}_t.h"
#include "irmode.h"

${single_constraints_decls}
EOF

print OUT "arch_register_class_t ${arch}_reg_classes[] = {\n\t".join(",\n\t", @regclasses)."\n};\n\n";

print OUT<<EOF;
${single_constraints}

const arch_register_t ${arch}_registers[] = {
${regtypes_def}
};

void ${arch}_register_init(void)
{
${reginit}
}
EOF
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
