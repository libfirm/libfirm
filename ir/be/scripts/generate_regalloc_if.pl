#!/usr/bin/perl -w

#
# This file is part of libFirm.
# Copyright (C) 2014 University of Karlsruhe.
#

# This script generates C code which creates ands sets up functions and
# data structures for the register allocator.
# Creation: 2005/11/14

use strict;
use Data::Dumper;
use integer;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %reg_classes;

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
sub map_flags {
	my $prefix = shift;
	my $flags  = shift || "none";
	return join(" | ", map { "$prefix$_" } split(/\s*\|\s*/, $flags));
}

# stacks for output
my $regtypes_def; # stack for the register type variables definitions
my @regclasses;   # stack for the register class variables
my $classdef;     # stack to define a name for a class index
my $regdef;       # stack to define a name for a register index
my $regdef2;
my $regcounts;
my $reginit;      # stack for the register type inits
my $single_constraints;

my $class_ptr;
my $class_idx = 0;

my %regclass2len = ();
my %reg2class = ();

$classdef .= "enum reg_classes {\n";

foreach my $class_name (sort(keys(%reg_classes))) {
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
foreach my $class_name (sort(keys(%reg_classes))) {
	my @class         = @{ $reg_classes{"$class_name"} };
	my $old_classname = $class_name;

	$class_name = $arch."_".$class_name;
	$class_ptr  = "&".$arch."_reg_classes[CLASS_".$class_name."]";
	my $flags = pop(@class);
	my $class_mode = $flags->{"mode"};
	my $flags_prepared = map_flags("arch_register_class_flag_", $flags->{"flags"});

	$single_constraints .= <<EOF;
static const arch_register_req_t ${arch}_class_reg_req_${old_classname} = {
	.cls             = &${arch}_reg_classes[CLASS_${arch}_${old_classname}],
	.limited         = NULL,
	.type            = arch_register_req_type_none,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};
EOF

	$classdef .= "\tCLASS_$class_name = $class_idx,\n";
	my $numregs = @class;
	my $first_reg = "&${arch}_registers[REG_". uc($class[0]->{"name"}) . "]";
	my $rcdef = <<EOF;
	{
		.name      = \"$class_name\",
		.mode      = NULL,
		.regs      = $first_reg,
		.class_req = &${arch}_class_reg_req_${old_classname},
		.index     = $class_idx,
		.n_regs    = $numregs,
		.flags     = $flags_prepared,
	},
EOF
	push(@regclasses, $rcdef);

	my $idx = 0;
	$reginit .= "\t$arch\_reg_classes[CLASS_".$class_name."].mode = $class_mode;\n";
	foreach (@class) {
		my $name   = $_->{"name"};
		my $ucname = uc($name);
		my $type   = map_flags("arch_register_type_", $_->{"type"});
		# realname is name if not set by user
		$_->{"realname"} = $_->{"name"} if (! exists($_->{"realname"}));
		my $realname = $_->{realname};
		my $classuc = uc($old_classname);

		$regdef  .= "\tREG_${ucname},\n";
		$regdef2 .= "\tREG_${classuc}_${ucname} = $idx,\n";
		my $dwarf_number = 0;
		if (defined($_->{dwarf})) {
			$dwarf_number = $_->{dwarf};
		}
		my $encoding = "REG_${classuc}_${ucname}";
		if (defined($_->{encoding})) {
			$encoding = $_->{encoding};
		}

		$regtypes_def .= <<EOF;
	{
		.name         = "${realname}",
		.cls          = ${class_ptr},
		.single_req   = &${arch}_single_reg_req_${old_classname}_${name},
		.type         = ${type},
		.index        = REG_${classuc}_${ucname},
		.global_index = REG_${ucname},
		.dwarf_number = ${dwarf_number},
		.encoding     = ${encoding},
	},
EOF

		my $limitedarray = get_limited_array($name);
		$single_constraints .= <<EOF;
static const unsigned ${arch}_limited_${old_classname}_${name} [] = ${limitedarray};
static const arch_register_req_t ${arch}_single_reg_req_${old_classname}_${name} = {
	.type            = arch_register_req_type_limited,
	.cls             = ${class_ptr},
	.limited         = ${arch}_limited_${old_classname}_${name},
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};
EOF

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

#include "${arch}_nodes_attr.h"

/** global register indices for ${arch} registers */
enum reg_indices {
${regdef}
	N_${archuc}_REGISTERS
};
/** local register indices for ${arch} registers */
enum {
${regdef2}
};

/** number of registers in ${arch} register classes. */
enum {
${regcounts}
};
${classdef}

extern const arch_register_t ${arch}_registers[N_${archuc}_REGISTERS];

extern arch_register_class_t ${arch}_reg_classes[N_${archuc}_CLASSES];

void ${arch}_register_init(void);

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
#include "gen_${arch}_regalloc_if.h"
#include "bearch_${arch}_t.h"

${single_constraints}
EOF

print OUT "arch_register_class_t ${arch}_reg_classes[] = {\n".join("",@regclasses)."\n};\n\n";

print OUT<<EOF;

/** The array of all registers in the ${arch} architecture, sorted by its global index.*/
const arch_register_t ${arch}_registers[] = {
${regtypes_def}
};

/**
 * Initializes ${arch} register classes.
 */
void ${arch}_register_init(void)
{
${reginit}
}
EOF
close(OUT);
