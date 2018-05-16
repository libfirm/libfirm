#! /usr/bin/env perl

#
# This file is part of libFirm.
# Copyright (C) 2014 University of Karlsruhe.
#

# This script generates C code which creates ands sets up functions and
# data structures for the register allocator.

use integer;
use strict;
use warnings;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
our %reg_classes;

# include spec file
unless (my $return = do "${specfile}") {
	die "Fatal error: couldn't parse $specfile: $@" if $@;
	die "Fatal error: couldn't do $specfile: $!"    unless defined $return;
	die "Fatal error: couldn't run $specfile"       unless $return;
}

# stacks for output
my $regtypes_def; # stack for the register type variables definitions
my $regclasses;   # stack for the register class variables
my $classdef;     # stack to define a name for a class index
my $reqdecls;
my $regdef;       # stack to define a name for a register index
my $regdef2;
my $regcounts;
my $reginit;      # stack for the register type inits
my $single_constraints;

my %regclass2len = ();
my %reg2class = ();

foreach my $class_name (sort(keys(%reg_classes))) {
	my $regs = $reg_classes{$class_name}{registers};

	my $idx = 0;
	foreach (@$regs) {
		if (defined(my $name = $_->{name})) {
			$reg2class{$name} = {
				"class" => $class_name,
				"index" => $idx
			};
		}
		$idx++;
	}
	$regclass2len{$class_name} = $idx;
}

sub get_limited_array
{
	my ($reg) = @_;

	my $result           = "{ ";
	my $sep              = "";
	my $regclass         = $reg2class{$reg}{class};
	my $classuc          = uc($regclass);
	my $ucname           = uc($reg);
	my $limitedbitsetlen = $regclass2len{$regclass};
	my $arraylen         = ($limitedbitsetlen + 31) / 32;
	for (my $i = 0; $i < $arraylen; ++$i) {
		$result .= $sep;
		$sep     = ", ";

		my $index = $reg2class{$reg}{index};
		if ($i * 32 <= $index && $index < ($i + 1) * 32) {
			if ($i > 0) {
				$result .= "(1U << (REG_${classuc}_${ucname} % 32))";
			} else {
				$result .= "(1U << REG_${classuc}_${ucname})";
			}
		} else {
			$result .= "0";
		}
	}
	$result .= " }";
}

sub has_flag
{
	my ($what, $list) = @_;
	return (defined($list) && grep { $_ eq $what } $list) ? "true" : "false";
}

# generate register type and class variable, init function and default requirements
foreach my $class_name (sort(keys(%reg_classes))) {
	my $class = $reg_classes{$class_name};

	my $arch_class_name = "${arch}_$class_name";
	my $class_req       = "${arch}_class_reg_req_$class_name";
	my $class_enum      = "CLASS_$arch_class_name";
	my $class_ptr       = "&${arch}_reg_classes[$class_enum]";
	my $class_mode      = $class->{mode};

	$single_constraints .= <<EOF;
const arch_register_req_t $class_req = {
	.cls   = &${arch}_reg_classes[$class_enum],
	.width = 1,
};
EOF
	$reqdecls .= "extern const arch_register_req_t $class_req;\n";

	$classdef .= "\t$class_enum,\n";

	my $regs    = $class->{registers};
	my $numregs = @$regs;
	my $uname   = uc($regs->[0]->{name});
	$regclasses .= <<EOF;
	{
		.name      = \"$arch_class_name\",
		.mode      = NULL,
		.regs      = &${arch}_registers[REG_$uname],
		.class_req = &$class_req,
		.index     = $class_enum,
		.n_regs    = $numregs,
EOF

	if (defined($class->{flags})) {
		foreach my $flag ($class->{flags}) {
			$regclasses .= "\t\t.$flag = true,\n";
		}
	}

	$regclasses .= "\t},\n";

	$reginit .= "\t$arch\_reg_classes[$class_enum].mode = $class_mode;\n";
	$regdef2 .= "enum {\n";
	foreach (@$regs) {
		my $name         = $_->{name};
		# realname is name if not set by user
		my $realname     = $_->{realname} // $name;
		my $single_req   = "${arch}_single_reg_req_${class_name}_${name}";
		my $classuc      = uc($class_name);
		my $ucname       = uc($name);
		my $local_idx    = "REG_${classuc}_$ucname";
		my $global_idx   = "REG_$ucname";
		my $dwarf_number = $_->{dwarf} // 0;
		my $encoding     = $_->{encoding} // $local_idx;
		my $is_virtual   = has_flag("virtual", $_->{type});

		$regdef  .= "\t$global_idx,\n";
		$regdef2 .= "\t$local_idx,\n";

		$regtypes_def .= <<EOF;
	{
		.name         = "$realname",
		.cls          = $class_ptr,
		.single_req   = &$single_req,
		.index        = $local_idx,
		.global_index = $global_idx,
		.dwarf_number = $dwarf_number,
		.encoding     = $encoding,
		.is_virtual   = $is_virtual,
	},
EOF

		my $limited_name = "${arch}_limited_${class_name}_${name}";
		my $limitedarray = get_limited_array($name);
		$single_constraints .= <<EOF;
static const unsigned ${limited_name}[] = $limitedarray;
const arch_register_req_t $single_req = {
	.cls     = $class_ptr,
	.limited = $limited_name,
	.width   = 1,
};
EOF
		$reqdecls .= "extern const arch_register_req_t $single_req;\n";
	}
	$regdef2 .= "};\n\n";
	$regcounts .= "\tN_${arch_class_name}_REGS = $numregs,\n";
}

my $archuc = uc($arch);

$classdef .= "\tN_${archuc}_CLASSES = ".scalar(keys(%reg_classes));

my $creation_time = localtime(time());

# generate header (external usage) file
my $target_h = "$target_dir/gen_${arch}_regalloc_if.h";
open(OUT, ">", $target_h) // die("Fatal error: Could not open $target_h, reason: $!\n");
print OUT <<EOF;
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

#include "bearch.h"

/** global register indices for ${arch} registers */
enum reg_indices {
${regdef}
	N_${archuc}_REGISTERS
};

/** local register indices for ${arch} registers */
${regdef2}
/** number of registers in ${arch} register classes. */
enum {
${regcounts}
};

enum {
${classdef}
};

${reqdecls}

extern const arch_register_t ${arch}_registers[N_${archuc}_REGISTERS];

extern arch_register_class_t ${arch}_reg_classes[N_${archuc}_CLASSES];

void ${arch}_register_init(void);

#endif
EOF
close(OUT);

# generate c file
my $target_c = "$target_dir/gen_${arch}_regalloc_if.c";
open(OUT, ">", $target_c) // die("Fatal error: Could not open $target_c, reason: $!\n");
print OUT <<EOF;
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

#include "${arch}_bearch_t.h"

${single_constraints}

arch_register_class_t ${arch}_reg_classes[] = {
$regclasses
};

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
