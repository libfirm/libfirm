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

# This script generates C code for the IR nodes specified
# in ../ir/ir/ir_spec.pl.
#
# $Id$

use strict;

my $specfile = $ARGV[0];
my $target_dir = $ARGV[1];
my $target_cons = $target_dir."/gen_ir_cons.c.inl";

our %nodes;
our $NONE;
our $MANUALFROMPARAM;

# include spec file

my $return;

no strict "subs";
unless ($return = do $specfile) {
	die "Fatal error: couldn't parse $specfile: $@" if $@;
	die "Fatal error: couldn't do $specfile: $!"    unless defined $return;
	die "Fatal error: couldn't run $specfile"       unless $return;
}
use strict "subs";

my @text_cons;
my $ARITY_DYNAMIC = -1;
my $ARITY_VARIABLE = "arity";
my $had_error = 0;

# generate IR node constructors

foreach my $nodename (keys(%nodes)) {
	my %curnode = %{ $nodes{"$nodename"} };

	my $op_name;
	if (exists($curnode{"op"})) {
		$op_name = $curnode{"op"};
	} else {
		$op_name = $nodename;
	}

	if ($op_name eq 0) {
		next;
	}

	print "${op_name}\n";

	# handle inheritance

	my @hierarchy;
	my %curop = %curnode;
	my $supername;
	push(@hierarchy, $nodename);
	while (exists($curop{"is_a"})) {
		$supername = $curop{"is_a"};
		push(@hierarchy, $supername);
		%curop = %{ $nodes{$supername} };
	}
	my %node;
	foreach my $cursupername (reverse @hierarchy) {
		# print " - $cursupername\n";
		my %supernode = %{ $nodes{$cursupername} };
		foreach my $keyname (keys(%supernode)) {
			# print " --- $keyname\n";
			if ($keyname eq "op") {
				next;
			}
			my $value = $supernode{$keyname};
			$node{$keyname} = $value;
		}
	}

	# check op_flags and state fields

	if (!exists($node{"op_flags"})) {
		$node{"op_flags"} = "N";
	}
	if (!exists($node{"state"})) {
		$node{"state"} = "floats";
	}

	# calculate arity

	my $arity = 0;

	if (exists($node{"arity"})) {
		$arity = $node{"arity"};
		if (exists($node{"ins"})) {
			print "ERROR: $nodename defines \"arity\" AND \"ins\" field\n";
			$had_error = 1;
		}
	} elsif (exists($node{"ins"})) {
		$arity = scalar(@{ $node{"ins"} });
	}

	if ($arity eq "dynamic") {
		$arity = $ARITY_DYNAMIC;
	} elsif ($arity eq "variable") {
		$arity = $ARITY_VARIABLE;
	}

	# build new_rd_$nodename function

	push(@text_cons, "ir_node *new_rd_$nodename(dbg_info *db, ir_graph *irg");

	my @text_paramdecls;
	my @text_paramuse;

	my $block_name;
	if (!exists($node{"block"})) {
		push(@text_cons, ", ir_node *block");
		$block_name = "block";
	} else {
		$block_name = $node{"block"};
	}

	if (exists($node{"ins"})) {
		my @ins = @{ $node{"ins"} };

		foreach my $inname (@ins) {
			push(@text_paramdecls, "ir_node *$inname");
			push(@text_paramuse, "$inname");
		}
	} elsif ($arity eq $ARITY_VARIABLE) {
		push(@text_paramdecls, "int arity, ir_node **in");
		push(@text_paramuse, "arity, in");
	}

	my $mode_name;
	if (exists($node{"mode"})) {
		$mode_name = $node{"mode"};
	} else {
		$mode_name = "mode";
		push(@text_paramdecls, "ir_mode *mode");
		push(@text_paramuse, "mode");
	}

	if (exists($node{"attrs"})) {
		my @attrs = @{ $node{"attrs"} };

		if (!exists($node{"attrs_name"})) {
			$node{"attrs_name"} = lcfirst($nodename);
		}

		foreach my $attritem (@attrs) {
			my %attr = %{ $attritem };
			if (!exists($attr{"init"}) || $attr{"init"} eq $MANUALFROMPARAM) {
				push(@text_paramdecls, $attr{"type"}. " ".$attr{"name"});
				push(@text_paramuse, $attr{"name"});
			}
		}
	}

	push(@text_cons, ", " . join(", ", @text_paramdecls)) if @text_paramdecls;
	push(@text_cons, ")\n{\n".
		"\tir_node *res;\n");

	my $in_array = "NULL";
	if ($arity eq $ARITY_VARIABLE) {
		$in_array = "in";
	} elsif ($arity > 0) {
		push(@text_cons, "\tir_node *in[$arity];\n");
		$in_array = "in";

		my @ins = @{ $node{"ins"} };
		for (my $idx = 0; $idx <= $#ins; $idx++) {
			push(@text_cons, "\tin[$idx] = ".$ins[$idx].";\n");
		}
	}

	push(@text_cons, "\tres = new_ir_node(db, irg, $block_name, op_$op_name, $mode_name, $arity, $in_array);\n");

	if (exists($node{"attrs"})) {
		my @attrs = @{ $node{"attrs"} };

		foreach my $attritem (@attrs) {
			my %attr = %{ $attritem };
			if (!exists($attr{"init"}) || $attr{"init"} ne $MANUALFROMPARAM &&
			                              $attr{"init"} ne $NONE) {
				my $initname = exists($attr{"initname"}) ? $attr{"initname"} : "." . $attr{"name"};
				my $initval  = exists($attr{"init"})     ? $attr{"init"}     : $attr{"name"};
				push(@text_cons, "\tres->attr.".$node{"attrs_name"}."$initname = $initval;\n");
			}
		}
	}

	if (exists($node{"init"})) {
		push(@text_cons, $node{"init"});
	}

	if (!exists($node{"optimize"}) || $node{"optimize"} eq 1) {
		push(@text_cons, "\tres = optimize_node(res);\n");
	}

	push(@text_cons, "\tIRN_VRFY_IRG(res, irg);\n".
		"\treturn res;\n".
		"}\n\n");

	# build new_r_$nodename function

	push(@text_cons, "ir_node *new_r_$nodename(ir_graph *irg");
	push(@text_cons, ", ir_node *block") unless exists($node{"block"});
	push(@text_cons, ", " . join(", ", @text_paramdecls)) if @text_paramdecls;
	push(@text_cons, ")\n"
		."{\n"
		."\treturn new_rd_$nodename(NULL, irg");
	push(@text_cons, ", block") unless exists($node{"block"});
	push(@text_cons, ", " . join(", ", @text_paramuse)) if @text_paramuse;
	push(@text_cons, ");\n"
		."}\n\n");

	# build new_d_$nodename function

	push(@text_cons, "ir_node *new_d_$nodename(dbg_info *db");
	push(@text_cons, ", " . join(", ", @text_paramdecls)) if @text_paramdecls;
	push(@text_cons, ")\n"
	                ."{\n"
	                ."\tir_node *res;\n");
	push(@text_cons, $node{"d_pre"}) if exists($node{"d_pre"});
	push(@text_cons, "\tres = new_rd_$nodename(db, current_ir_graph");
	push(@text_cons, ", current_ir_graph->current_block") unless exists($node{"block"});
	push(@text_cons, ", " . join(", ", @text_paramuse)) if @text_paramuse;
	push(@text_cons, ");\n");
	push(@text_cons, $node{"d_post"}) if exists($node{"d_post"});
	push(@text_cons, "\treturn res;\n"
	                ."}\n\n");

	# build new_$nodename function

	push(@text_cons, "ir_node *new_$nodename(");
	if (@text_paramdecls) {
		push(@text_cons, join(", ", @text_paramdecls));
	} else {
		push(@text_cons, "void");
	}
	push(@text_cons, ")\n"
		."{\n"
		."\treturn new_d_$nodename(NULL");
	push(@text_cons, ", " . join(", ", @text_paramuse)) if @text_paramuse;
	push(@text_cons, ");\n"
		."}\n\n");

# 	push(@text_cons, "ir_node *new_bd_$nodename(dbg_info *db");
# 	if (!exists($node{"block"})) {
# 		push(@text_cons, ", ir_node *block");
# 	}
# 	push(@text_cons, @text_paramdecls);
# 	push(@text_cons, ")\n"
# 		."{\n"
# 		."\treturn new_rd_$nodename(db, current_ir_graph");
# 	if (!exists($node{"block"})) {
# 		push(@text_cons, ", block");
# 	}
# 	push(@text_cons, @text_paramuse);
# 	push(@text_cons, ");\n"
# 		."}\n\n");
}

!$had_error || die;

# emit the code

print "Emitting code to $target_cons\n";

open(OUT, ">$target_cons") || die("Fatal error: Could not open $target_cons, reason: $!\n");

print OUT @text_cons;

close(OUT);

print "Done.\n"
