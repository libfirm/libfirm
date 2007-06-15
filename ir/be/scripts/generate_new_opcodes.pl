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

# This script generates the C code which creates the irop's and
# their coresponding node constructors for all operations in a given spec
# so they can be used as normal firm nodes.
# Creation: 2005/10/19
# $Id$

use strict;
use Data::Dumper;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];
my $state      = 1;
my $cur_op     = "";
my $line_nr    = 0;

our $arch;
our $additional_opcodes;
our %nodes;
our %cpu;
our $default_attr_type;
our $default_cmp_attr;
our %init_attr;
our %compare_attr;

# include spec file

my $return;

no strict "subs";
unless ($return = do $specfile) {
	die "couldn't parse $specfile: $@" if $@;
	die "couldn't do $specfile: $!"    unless defined $return;
	die "couldn't run $specfile"       unless $return;
}
use strict "subs";

my $target_c = $target_dir."/gen_".$arch."_new_nodes.c.inl";
my $target_h = $target_dir."/gen_".$arch."_new_nodes.h";

if(!defined($default_attr_type)) {
	$default_attr_type = "${arch}_attr_t";
}
if(!defined(%init_attr)) {
	%init_attr = (
		"$default_attr_type" => "\tinit_${arch}_attributes(res, flags, in_reqs, out_reqs, exec_units, n_res, latency);",
	);
}
if(!defined($default_cmp_attr)) {
	$default_cmp_attr = "${arch}_compare_attr";
}
if(!defined(%compare_attr)) {
	%compare_attr = (
		"${default_attr_type}" => "${default_cmp_attr}",
	);
}

#print Dumper(%nodes);
#print Dumper(%operands);

# create c code file from specs

my @obst_opvar;       # stack for the "ir_op *op_<arch>_<op-name> = NULL;" statements
my @obst_get_opvar;   # stack for the get_op_<arch>_<op-name>() functions
my @obst_constructor; # stack for node constructor functions
my @obst_new_irop;    # stack for the new_ir_op calls
my @obst_enum_op;     # stack for creating the <arch>_opcode enum
my @obst_header;      # stack for function prototypes
my @obst_is_archirn;  # stack for the is_$arch_irn() function
my @obst_cmp_attr;    # stack for the compare attribute functions
my @obst_proj;        # stack for the pn_ numbers
my $orig_op;
my $arity;
my $cmp_attr_func;
my $temp;
my $n_opcodes = 0;    # number of opcodes
my $ARITY_VARIABLE = -1;
my $ARITY_DYNAMIC  = -2;

# for registering additional opcodes
$n_opcodes += $additional_opcodes if (defined($additional_opcodes));

push(@obst_header, "void ".$arch."_create_opcodes(void);\n");

push(@obst_enum_op, "typedef enum _$arch\_opcodes {\n");
foreach my $op (keys(%nodes)) {
	my %n        = %{ $nodes{"$op"} };
	my $known_mode;
	my $num_outs = 0;
	my $out_arity;
	my @out_flags;

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

		@outs     = @{ $n{"outs"} };
		if($out_arity >= 0 && scalar(@outs) != $out_arity) {
			die "Op ${op} has different number of outs and out_arity\n";
		}

		$num_outs = $#outs + 1;

		push(@obst_proj, "\nenum pn_$op {\n");

		for (my $idx = 0; $idx <= $#outs; $idx++) {
			# check, if we have additional flags annotated to out
			if ($outs[$idx] =~ /:((S|I)(\|(S|I))*)/) {
				push(@out_flags, $1);
				$outs[$idx] =~ s/:((S|I)(\|(S|I))*)//;
			}
			push(@obst_proj, "\tpn_$op\_".$outs[$idx]." = $idx,\n");
		}

		push(@obst_proj, "};\n");
		# outs have names, it must be a mode_T node
		$known_mode = "mode_T";
	}
	if (exists($n{"ins"})) {
		undef my @ins;

		@ins = @{ $n{"ins"} };
		if($arity >= 0 && scalar(@ins) != $arity) {
			die "Op ${op} has different number of ins and arity\n";
		}

		push(@obst_proj, "\nenum n_$op {\n");

		for (my $idx = 0; $idx <= $#ins; $idx++) {
			push(@obst_proj, "\tn_${op}_".$ins[$idx]." = $idx,\n");
		}

		push(@obst_proj, "};\n");
	}

	# determine mode
	if (exists($n{"mode"})) {
		$known_mode = $n{"mode"};
	}

	push(@obst_opvar, "ir_op *op_$op = NULL;\n");
	push(@obst_get_opvar, "ir_op *get_op_$op(void)         { return op_$op; }\n");
	push(@obst_get_opvar, "int    is_$op(const ir_node *n) { return get_$arch\_irn_opcode(n) == iro_$op; }\n\n");

	push(@obst_is_archirn, "is_$op(node)");

	push(@obst_header, "extern ir_op *op_$op;\n");
	push(@obst_header, "ir_op *get_op_$op(void);\n");
	push(@obst_header, "int is_$op(const ir_node *n);\n");

	my $attr_type= $n{"attr_type"};
	if(!defined($attr_type)) {
		$attr_type = $default_attr_type;
	}

	# determine compare function
	my $cmp_attr_func;
	if (exists($n{"cmp_attr"})) {
		my $cmpcode = $n{"cmp_attr"};

		push(@obst_cmp_attr, "static int cmp_attr_$op(ir_node *a, ir_node *b) {\n");
		if($cmpcode =~ m/attr_a/) {
			push(@obst_cmp_attr, "\t${attr_type} *attr_a = get_irn_generic_attr(a);\n");
		}
		if($cmpcode =~ m/attr_b/) {
			push(@obst_cmp_attr, "\t${attr_type} *attr_b = get_irn_generic_attr(b);\n");
		}
		push(@obst_cmp_attr, "\t${cmpcode}\n");
		push(@obst_cmp_attr, "}\n\n");

		$cmp_attr_func = "cmp_attr_${op}";
	} else {
		if(defined($compare_attr{${attr_type}})) {
			$cmp_attr_func = $compare_attr{${attr_type}};
		} else {
			die "No compare function defined for ${attr_type} attributes.";
		}
	}

	if (exists($n{"rd_constructor"}) && $n{"rd_constructor"} =~ /^NONE$/i) {
		# we explicitly skip the constructor if the specification entry says NONE
	} else {
		my $comment = $n{"comment"};
		if(!exists($n{"comment"})) {
			$comment = "construct ${orig_op} node";
		}
		$comment =
			"/**\n".
			" * ${comment}\n".
			" */\n";

		push(@obst_constructor, $comment);

		# create constructor head
		my $complete_args = "";
		$temp             = "";

		$temp = "ir_node *new_rd_$op(dbg_info *db, ir_graph *irg, ir_node *block";
		if (!exists($n{"args"})) { # default args
			if ($arity == $ARITY_VARIABLE) {
				$complete_args = ", int arity, ir_node *in[]";
			} elsif ($arity == $ARITY_DYNAMIC) {
				$complete_args = "";
			} else {
				for (my $i = 0; $i < $arity; $i++) {
					my $opname = "op${i}";
					if (exists($n{"ins"})) {
						my @ins = @{ $n{"ins"} };
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
			for my $href (@{ $n{"args"} }) {
				$href->{"type"} .= " " if ($href->{"type"} !~ / [*]?$/); # put a space between name and type if there is none at the end
				$complete_args  .= ", ".$href->{"type"}.$href->{"name"};
			}
		}

		# we have additional attribute arguements
		if (exists($n{"attr"})) {
			$complete_args .= ", ".$n{"attr"};
		}

		$temp .= "$complete_args)";
		push(@obst_constructor, $temp."\n{\n");
		push(@obst_header, $comment);
		push(@obst_header, $temp.";\n");

		# emit constructor code
		if (!exists($n{"rd_constructor"})) { # default constructor
			$temp  = "\tir_node  *res;\n";
			$temp .= "\tir_op    *op      = op_${op};\n";
			$temp .= "\tint       flags   = 0;\n";

			if($arity == $ARITY_DYNAMIC) {
				$temp .= "\tint        arity   = -1;\n";
				$temp .= "\tir_node  **in      = NULL;\n";
			} elsif($arity == $ARITY_VARIABLE) {
			} else {
				$temp .= "\tint       arity   = $arity;\n";
				if($arity > 0) {
					$temp .= "\tir_node  *in[$arity];\n";
				} else {
					$temp .= "\tir_node **in    = NULL;\n";
				}
			}
			if($out_arity == $ARITY_DYNAMIC) {
				$temp .= "\tint       n_res   = -1;\n";
			} elsif($out_arity == $ARITY_VARIABLE) {
			} else {
				$temp .= "\tint       n_res   = ${out_arity};\n";
			}

			my $latency = $n{"latency"};
			if (!defined($latency)) {
				$latency = 1;
			}
			$temp .= "\tunsigned  latency = ${latency};\n";

			if (defined($known_mode)) {
				$temp .= "\tir_mode  *mode    = ${known_mode};\n";
			}

			# set up static variables for cpu execution unit assigments
			if (exists($n{"units"})) {
				$temp .= gen_execunit_list_initializer($n{"units"});
			} else {
				$temp .= "\tstatic const be_execution_unit_t ***exec_units = NULL;\n";
			}

			undef my $in_req_var;
			undef my $out_req_var;

			# set up static variables for requirements and registers
			if (exists($n{"reg_req"})) {
				my %req = %{ $n{"reg_req"} };
				my $idx;

				undef my @in;
				@in = @{ $req{"in"} } if (exists($req{"in"}));
				undef my @out;
				@out = @{ $req{"out"} } if exists(($req{"out"}));

				if (@in) {
					if($arity >= 0 && scalar(@in) != $arity) {
						die "Arity and number of in requirements don't match for ${op}\n";
					}

					$temp .= "\tstatic const arch_register_req_t *in_reqs[] =\n";
					$temp .= "\t{\n";
					for ($idx = 0; $idx <= $#in; $idx++) {
						$temp .= "\t\t&".$op."_reg_req_in_".$idx.",\n";
					}
					$temp .= "\t};\n";
				} else {
					if($arity > 0) {
						die "need in requirements for ${op}\n";
					}
					$temp .= "\tstatic const arch_register_req_t **in_reqs = NULL;\n";
				}

				if (@out) {
					if($out_arity >= 0 && scalar(@out) != $out_arity) {
						die "Out-Arity and number of out requirements don't match for ${op}\n";
					}

					$temp .= "\tstatic const arch_register_req_t *out_reqs[] =\n";
					$temp .= "\t{\n";
					for ($idx = 0; $idx <= $#out; $idx++) {
						$temp .= "\t\t&".$op."_reg_req_out_".$idx.",\n";
					}
					$temp .= "\t};\n";
				} else {
					if($out_arity > 0) {
						die "need out requirements for ${op}\n";
					}
					$temp .= "\tstatic const arch_register_req_t **out_reqs = NULL;\n";
				}
			} else {
				$temp .= "\tstatic const arch_register_req_t **in_reqs = NULL;\n";
				$temp .= "\tstatic const arch_register_req_t **out_reqs = NULL;\n";
			}
			if(exists($n{"init_attr"})) {
				$temp .= "\t${attr_type} *attr;\n";
			}

			$temp .= "\n";

			if($arity > 0) {
				$temp .= "\t/* construct in array */\n";
				for (my $i = 0; $i < $arity; $i++) {
					my $opname = "op${i}";
					if (exists($n{"ins"})) {
						my @ins = @{ $n{"ins"} };
						$opname = $ins[$i];
					}

					$temp .= "\tin[${i}] = ${opname};\n";
				}
				$temp .= "\n";
			}

			# set flags
			if (exists($n{"irn_flags"})) {
				$temp .= "\t/* flags */\n";
				foreach my $flag (split(/\|/, $n{"irn_flags"})) {
					if ($flag eq "R") {
						$temp .= "\tflags |= arch_irn_flags_rematerializable;\n";
					} elsif ($flag eq "N") {
						$temp .= "\tflags |= arch_irn_flags_dont_spill;\n";
					} elsif ($flag eq "I") {
						$temp .= "\tflags |= arch_irn_flags_ignore;\n";
					} elsif ($flag eq "S") {
						$temp .= "\tflags |= arch_irn_flags_modify_sp;\n";
					}
				}
				$temp .= "\n";
			}

			$temp .= "\t/* create node */\n";
			$temp .= "\tassert(op != NULL);\n";
			$temp .= "\tres = new_ir_node(db, irg, block, op, mode, arity, in);\n";
			$temp .= "\n";

			$temp .= "\t/* init node attributes */\n";
			# lookup init function
			my $attr_init_code = $init_attr{$attr_type};
			if(!defined($attr_init_code)) {
				die "Couldn't find attribute initialisation code for type '${attr_type}'";
			}
			$temp .= "${attr_init_code}\n";
			$temp .= "\n";

			# set flags for outs
			if ($#out_flags >= 0) {
				$temp .= "\t/* set flags for outs */\n";
				for (my $idx = 0; $idx <= $#out_flags; $idx++) {
					my $flags  = "";
					my $prefix = "";

					foreach my $flag (split(/\|/, $out_flags[$idx])) {
						if ($flag eq "I") {
							$flags .= $prefix."arch_irn_flags_ignore";
							$prefix = " | ";
						}
						elsif ($flag eq "S") {
							$flags .= $prefix."arch_irn_flags_modify_sp";
							$prefix = " | ";
						}
					}

					$temp .= "\tset_$arch\_out_flags(res, $flags, $idx);\n";
				}
				$temp .= "\n";
			}


			if (exists($n{"init_attr"})) {
				$temp .= "\tattr = get_irn_generic_attr(res);\n";
				$temp .= "\t".$n{"init_attr"}."\n";
			}

			$temp .= "\t/* optimize node */\n";
			$temp .= "\tres = optimize_node(res);\n";
			$temp .= "\tirn_vrfy_irg(res, irg);\n";
			$temp .= "\n";

			$temp .= "\treturn res;\n";

			push(@obst_constructor, $temp);
		}
		else { # user defined constructor
			push(@obst_constructor, $n{"rd_constructor"});
		}

		# close constructor function
		push(@obst_constructor, "}\n\n");
	} # constructor creation

	# set default values for state and flags if not given
	$n{"state"}    = "floats" if (! exists($n{"state"}));
	$n{"op_flags"} = "N"      if (! exists($n{"op_flags"}));


	push(@obst_new_irop, "\n\tmemset(&ops, 0, sizeof(ops));\n");
	push(@obst_new_irop, "\tops.dump_node     = $arch\_dump_node;\n");

	if (defined($cmp_attr_func)) {
		push(@obst_new_irop, "\tops.node_cmp_attr = ${cmp_attr_func};\n");
	}

	$n_opcodes++;
	my $n_res = $out_arity;
	if($n_res < 0) {
		$n_res = "20"; # hacky....
	}
	$temp  = "\top_$op = new_ir_op(cur_opcode + iro_$op, \"$op\", op_pin_state_".$n{"state"}.", ".$n{"op_flags"};
	$temp .= "|M, ".translate_arity($arity).", 0, sizeof(${attr_type}), &ops);\n";
	push(@obst_new_irop, $temp);
	push(@obst_new_irop, "\tset_op_tag(op_$op, &$arch\_op_tag);\n");
	push(@obst_enum_op, "\tiro_$op,\n");

	push(@obst_header, "\n");
}
push(@obst_enum_op, "\tiro_$arch\_last_generated,\n");
push(@obst_enum_op, "\tiro_$arch\_last = iro_$arch\_last_generated");
push(@obst_enum_op, " + $additional_opcodes") if (defined($additional_opcodes));
push(@obst_enum_op, "\n} $arch\_opcodes;\n\n");

# emit the code

open(OUT, ">$target_c") || die("Could not open $target_c, reason: $!\n");

print OUT "#include \"gen_$arch\_regalloc_if_t.h\"\n\n";
print OUT @obst_cmp_attr;
print OUT "\n";
print OUT @obst_opvar;
print OUT "\n";
print OUT @obst_get_opvar;
print OUT "\n";

print OUT<<EOF;

static int $arch\_opcode_start = -1;
static int $arch\_opcode_end   = -1;

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

print OUT<<ENDOFISIRN;

/** A tag for the $arch opcodes. Note that the address is used as a tag value, NOT the FOURCC code. */
static unsigned $arch\_op_tag = FOURCC('$a', '$b', '$c', '$d');

/** Return the opcode number of the first $arch opcode. */
int get_$arch\_opcode_first(void) {
	return $arch\_opcode_start;
}

/** Return the opcode number of the last $arch opcode + 1. */
int get_$arch\_opcode_last(void) {
	return $arch\_opcode_end;
}

/** Return 1 if the given opcode is a $arch machine op, 0 otherwise */
int is_$arch\_op(const ir_op *op) {
	return get_op_tag(op) == &$arch\_op_tag;
}

/** Return 1 if the given node is a $arch machine node, 0 otherwise */
int is_$arch\_irn(const ir_node *node) {
	return is_$arch\_op(get_irn_op(node));
}

int get_$arch\_irn_opcode(const ir_node *node) {
	if (is_$arch\_irn(node))
		return get_irn_opcode(node) - $arch\_opcode_start;
	return -1;
}

ENDOFISIRN

print OUT @obst_constructor;

print OUT<<ENDOFMAIN;
/**
 * Creates the $arch specific Firm machine operations
 * needed for the assembler irgs.
 */
void $arch\_create_opcodes(void) {
#define N   irop_flag_none
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking
#define H   irop_flag_highlevel
#define c   irop_flag_constlike
#define K   irop_flag_keep
#define M   irop_flag_machine
#define O   irop_flag_machine_op
#define R   (irop_flag_user << 0)

	ir_op_ops  ops;
	int        cur_opcode;
	static int run_once = 0;

	if (run_once)
		return;
	run_once = 1;

	cur_opcode = get_next_ir_opcodes(iro_$arch\_last);

	$arch\_opcode_start = cur_opcode;
ENDOFMAIN

print OUT @obst_new_irop;
print OUT "\n";
print OUT "\t$arch\_register_additional_opcodes(cur_opcode);\n" if (defined($additional_opcodes));
print OUT "\t$arch\_opcode_end = cur_opcode + iro_$arch\_last";
print OUT " + $additional_opcodes" if (defined($additional_opcodes));
print OUT ";\n";
print OUT "}\n";

close(OUT);

open(OUT, ">$target_h") || die("Could not open $target_h, reason: $!\n");

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

EOF

print OUT @obst_enum_op;
print OUT "int is_$arch\_irn(const ir_node *node);\n\n";
print OUT "int get_$arch\_opcode_first(void);\n";
print OUT "int get_$arch\_opcode_last(void);\n";
print OUT "int get_$arch\_irn_opcode(const ir_node *node);\n";
print OUT @obst_header;
print OUT @obst_proj;

print OUT <<EOF;
#endif
EOF

close(OUT);

###
# Translates numeric arity into string constant.
###
sub translate_arity {
	my $arity = shift;

	if ($arity =~ /^\d+$/) {
		if    ($arity == 0) {
			return "oparity_zero";
		}
		elsif ($arity == 1) {
			return "oparity_unary";
		}
		elsif ($arity == 2) {
			return "oparity_binary";
		}
		elsif ($arity == 3) {
			return "oparity_trinary";
		}
		else {
			return "oparity_any";
		}
	} elsif ($arity == $ARITY_VARIABLE) {
		return "oparity_variable";
	} elsif ($arity == $ARITY_DYNAMIC) {
		return "oparity_dynamic";
	} else {
		die "Unknown arity $arity";
	}
}

###
# Return the list of pointers for the given execution units.
###
sub gen_execunit_list_initializer {
	my $units   = shift;
	my $uc_arch = uc($arch);
	my $ret     = "";
	my $ret2    = "";
	my %init;

	foreach my $unit (@{ $units }) {
		if ($unit eq "DUMMY") {
			push(@{ $init{"DUMMY"} }, "\t\t&be_machine_execution_units_DUMMY[0]");
		}
		elsif (exists($cpu{"$unit"})) {
			# operation can be executed on all units of this type
			# -> add them all
			my $tp_name = "$arch\_execution_units_$unit";
			my $idx     = 0;
			foreach (@{ $cpu{"$unit"} }) {
				next if ($idx++ == 0);  # skip first element (it's not a unit)
				my $unit_name = "$uc_arch\_EXECUNIT_TP_$unit\_$_";
				push(@{ $init{"$unit"} }, "\t\t&".$tp_name."[".$unit_name."]");
			}
		}
		else {
			# operation can be executed only a certain unit
			# -> find corresponding unit type
			my $found = 0;
TP_SEARCH:	foreach my $cur_type (keys(%cpu)) {
				foreach my $cur_unit (@{ $cpu{"$cur_type"} }) {
					if ($unit eq $cur_unit) {
						my $tp_name   = "$arch\_execution_units_$cur_type";
						my $unit_name = "$uc_arch\_EXECUNIT_TP_$cur_type\_$unit";
						push(@{ $init{"$unit"} }, "\t\t&".$tp_name."[".$unit_name."]");
						$found = 1;
						last TP_SEARCH;
					}
				}
			}

			if (! $found) {
				print STDERR "Invalid execution unit $unit specified!\n";
			}
		}
	}

	# prepare the 2-dim array init
	foreach my $key (keys(%init)) {
		$ret .= "\tstatic const be_execution_unit_t *allowed_units_".$key."[] =\n";
		$ret .= "\t{\n";
		foreach (@{ $init{"$key"} }) {
			$ret .= "$_,\n";
		}
		$ret .= "\t\tNULL\n";
		$ret .= "\t};\n";
		$ret2 .= "\t\tallowed_units_$key,\n";
	}
	$ret2 .= "\t\tNULL\n";

	$ret .= "\tstatic const be_execution_unit_t **exec_units[] =\n";
	$ret .= "\t{\n";
	$ret .= $ret2;
	$ret .= "\t};\n";

	return $ret;
}
