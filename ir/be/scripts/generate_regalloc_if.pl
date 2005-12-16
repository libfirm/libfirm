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

my $numregs;
my $class_ptr;
my $class_idx = 0;

my $tmp;

my %reg2class;

# there is a default NONE requirement
$tmp = "/* Default NONE register requirements */\n";
$tmp .= "const arch_register_req_t ia32_default_req_none = {\n";
$tmp .= "  arch_register_req_type_none,\n";
$tmp .= "  NULL,\n";
$tmp .= "  { NULL }\n";
$tmp .= "};\n\n";
push(@obst_req, $tmp);

push(@obst_header_all, "extern arch_register_class_t $arch\_reg_classes[N_CLASSES];\n\n");
push(@obst_header_all, "extern const arch_register_req_t ia32_default_req_none;\n");

push(@obst_classdef, "#define N_CLASSES ".scalar(keys(%reg_classes))."\n");

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
  $tmp .= "const arch_register_req_t ia32_default_req_$class_name = {\n";
  $tmp .= "  arch_register_req_type_normal,\n";
  $tmp .= "  $class_ptr,\n";
  $tmp .= "  { NULL }\n";
  $tmp .= "};\n\n";
  push(@obst_req, $tmp);

  push(@obst_header_all, "\nextern const arch_register_req_t ia32_default_req_$class_name;\n");

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
    $tmp = "int $limit_func_name(const ir_node *irn, int pos, bitset_t *bs)";
    push(@obst_defreq_head, $tmp.";\n");

    # push the function definition
    $tmp .= " {\n";
    $tmp .= "    bs = bitset_clear_all(bs);\n";
    $tmp .= "    bitset_set(bs, REG_".uc($_->{"name"}).");\n";  # REGISTER to index assignment is done some lines down
    $tmp .= "    return 1;\n";
    $tmp .= "}\n\n";
    push(@obst_limit_func, $tmp);

    # push the default requirement struct
    $tmp  = "const arch_register_req_t ia32_default_req_$class_name\_".$_->{"name"}." = {\n";
    $tmp .= "  arch_register_req_type_limited,\n";
    $tmp .= "  $class_ptr,\n";
    $tmp .= "  { $limit_func_name }\n";
    $tmp .= "};\n\n";
    push(@obst_req, $tmp);

    push(@obst_header_all, "extern const arch_register_req_t ia32_default_req_$class_name\_".$_->{"name"}.";\n");

    $reg2class{$_->{"name"}} = { "class" => $old_classname, "index" => $idx }; # remember reg to class for later use
    push(@obst_regdef, "#define REG_".uc($_->{"name"})." $idx\n");
    push(@obst_reginit, "  ".$class_name."_regs[$idx].name      = \"".$_->{"name"}."\";\n");
    push(@obst_reginit, "  ".$class_name."_regs[$idx].reg_class = $class_ptr;\n");
    push(@obst_reginit, "  ".$class_name."_regs[$idx].index     = $idx;\n");
    push(@obst_reginit, "  ".$class_name."_regs[$idx].type      = ".$rt[$_->{"type"}].";\n\n");
    $idx++;
  }

  $class_idx++;
}

push(@obst_header_all, "\n/* node specific requirements */\n");

# generate node-register constraints
foreach my $op (keys(%nodes)) {
  my %n = %{ $nodes{"$op"} };

  next if (!exists($n{"reg_req"}));

  $op = $arch."_".$op;

  push(@obst_req, "/* IN requirements for '$op' */\n");

  # we need to remember the classes of the IN constraints for
  # OUT constraints like "in_s1"
  my @inidx_class;

  # check for argument requirements
  if (exists($n{"reg_req"}{"in"})) {
    my @in = @{ $n{"reg_req"}{"in"} };

    for (my $idx = 0; $idx <= $#in; $idx++) {
      my $class = undef;

      my $tmp2 = "const arch_register_req_t _".$op."_reg_req_in_$idx = ";

      $tmp = "const arch_register_req_t *".$op."_reg_req_in_$idx = ";

      push(@obst_header_all, "extern const arch_register_req_t *".$op."_reg_req_in_$idx;\n");

      if ($in[$idx] eq "none") {
        push(@inidx_class, "none");
        $tmp .= "&ia32_default_req_none;\n";
      }
      elsif (is_reg_class($in[$idx])) {
        push(@inidx_class, $in[$idx]);
        $tmp .= "&ia32_default_req_".$arch."_".$in[$idx].";\n";
      }
      else {
        $class = build_subset_class_func($op, $idx, 1, $in[$idx]);
        if (!defined $class) {
          die("Could not build subset for IN requirements '$op' pos $idx ... exiting.\n");
        }
        push(@inidx_class, $class);
        $tmp  .= "&_".$op."_reg_req_in_$idx;\n";
        $tmp2 .= " {\n  arch_register_req_type_limited,\n  &$arch\_reg_classes[CLASS_$arch\_".$class."],\n  { limit_reg_".$op."_in_".$idx." }\n};\n";

        $tmp   = $tmp2.$tmp;
      }

      push(@obst_req, $tmp."\n");
    }
  }

  push(@obst_req, "/* OUT requirements for '$op' */\n");

  # check for result requirements
  if (exists($n{"reg_req"}{"out"})) {
    my @out = @{ $n{"reg_req"}{"out"} };

    for (my $idx = 0; $idx <= $#out; $idx++) {
      my $class = undef;

      my $tmp2 = "const arch_register_req_t _".$op."_reg_req_out_$idx = ";

      $tmp = "const arch_register_req_t *".$op."_reg_req_out_$idx = ";

      push(@obst_header_all, "extern const arch_register_req_t *".$op."_reg_req_out_$idx;\n");

      if ($out[$idx] eq "none") {
        $tmp .= "&ia32_default_req_none;\n";
      }
      elsif (is_reg_class($out[$idx])) {
        $tmp .= "&ia32_default_req_".$arch."_".$out[$idx].";\n";
      }
      elsif ($out[$idx] =~ /^(!)?in_s(\d+)/) { # this is a "should be (un)equal to register at in_X"
        $tmp  .= "&_".$op."_reg_req_out_$idx;\n";
        $tmp2 .= " {\n";
        $tmp2 .= "  arch_register_req_type_should_be_".($1 ? "different" : "same").",\n";
        $tmp2 .= "  &$arch\_reg_classes[CLASS_$arch\_".$inidx_class[$2 - 1]."],\n";
        $tmp2 .= "  { ".($2 - 1)." }\n};\n";

        $tmp   = $tmp2.$tmp
      }
      else {
        $class = build_subset_class_func($op, $idx, 0, $out[$idx]);
        if (!defined $class) {
          die("Could not build subset for OUT requirements '$op' pos $idx ... exiting.\n");
        }
        $tmp  .= "&_".$op."_reg_req_out_$idx;\n";
        $tmp2 .= " {\n  arch_register_req_type_limited,\n  &$arch\_reg_classes[CLASS_$arch\_".$class."],\n  { limit_reg_".$op."_out_".$idx." }\n};\n";

        $tmp   = $tmp2.$tmp
      }

      push(@obst_req, $tmp."\n");
    }
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

EOF

print OUT @obst_regdef, "\n";

print OUT @obst_classdef, "\n";

print OUT @obst_regtypes, "\n";

print OUT @obst_defreq_head, "\n";

print OUT "void ".$arch."_register_init(void);\n\n";

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

#include "gen_$arch\_regalloc_if_t.h"

EOF

print OUT "arch_register_class_t $arch\_reg_classes[] = {\n  ".join(",\n  ", @obst_regclasses)."\n};\n\n";

print OUT "void ".$arch."_register_init(void) {\n";
print OUT @obst_reginit;
print OUT "}\n\n";

print OUT @obst_limit_func;
print OUT @obst_req;

close(OUT);



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
# @return classname from which the subset is derived or undef
###
sub build_subset_class_func {
  my $neg   = undef;
  my $class = "";
  my $temp;

  # build function header
  my $op  = shift;
  my $idx = shift;
  my $in  = shift;
  push(@obst_limit_func, "/* limit the possible registers for ".($in ? "IN" : "OUT")." $idx at op $op */\n");
  push(@obst_limit_func, "int limit_reg_".$op."_".($in ? "in" : "out")."_".$idx."(const ir_node *irn, int pos, bitset_t *bs) {\n");

  my @regs = split(/ /, shift);

  # set/unset registers
  foreach (@regs) {
    # check for negate

    if (substr($_, 0, 1) eq "!") {
      if (defined($neg) && $neg == 0) {
        # we have seen a positiv constraint as first one but this one is negative
        # this doesn't make sense
        print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
        return undef;
      }

      if (!defined($neg)) {
        push(@obst_limit_func, "  bs = bitset_set_all(bs);     /* allow all register (negative constraints given) */\n");
      }

      $_   = substr($_, 1); # skip '!'
      $neg = 1;
    }
    else {
      if (defined($neg) && $neg == 1) {
        # we have seen a negative constraint as first one but this one is positive
        # this doesn't make sense
        print STDERR "Mixed positive and negative constraints for the same slot are not allowed.\n";
        return undef;
      }

      if (!defined($neg)) {
        push(@obst_limit_func, "  bs = bitset_clear_all(bs);   /* disallow all register (positive constraints given) */\n");
      }
      $neg = 0;
    }

    # check if register belongs to one of the given classes
    $temp = get_reg_class($_);
    if (!defined($temp)) {
      print STDERR "Unknown register '$_'!\n";
      return undef;
    }

    # set class
    if (!$class) {
      $class = $temp;
    }
    elsif ($class ne $temp) {
      # all registers must belong to the same class
      print STDERR "Registerclass mismatch. '$_' is not member of class '$class'.\n";
      return undef;
    }

    if ($neg == 1) {
      push(@obst_limit_func, "  bitset_clear(bs, ".get_reg_index($_).");         /* disallow $_ */\n");
    }
    else {
      push(@obst_limit_func, "  bitset_set(bs, ".get_reg_index($_).");           /* allow $_ */\n");
    }
  }

  push(@obst_limit_func, "\n  return ".($neg ? scalar(@{ $reg_classes{"$class"} }) - scalar(@regs) : scalar(@regs)).";\n");
  push(@obst_limit_func, "}\n\n");

  return $class;
}
