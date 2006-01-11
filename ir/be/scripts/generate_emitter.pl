#!/usr/bin/perl -w

# This script generates C code which emits assembler code for the
# assembler ir nodes. It takes a "emit" key from the node specification
# and substitutes lines starting with . with a corresponding fprintf().
# Creation: 2005/11/07
# $Id$

use strict;
use Data::Dumper;

my $specfile   = $ARGV[0];
my $target_dir = $ARGV[1];

our $arch;
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

my $target_c = $target_dir."/gen_".$arch."_emitter.c";
my $target_h = $target_dir."/gen_".$arch."_emitter.h";

# stacks for output
my @obst_func;   # stack for the emit functions
my @obst_header;  # stack for the function prototypes
my $line;

# some default emitter functions (Copy, Perm)

$line  = "#undef is_ia32_Perm\n";
$line .= "#define is_ia32_Perm(irn) (arch_irn_classify(arch_env, irn) == arch_irn_class_perm && ! is_Proj(irn))\n";
$line .= "#undef is_ia32_Copy\n";
$line .= "#define is_ia32_Copy(irn) (arch_irn_classify(arch_env, irn) == arch_irn_class_copy)\n";
push(@obst_header, $line."\n");

$line = "void emit_".$arch."_Copy(ir_node *n, emit_env_t *env)";
push(@obst_header, $line.";\n");
$line .= " {\n  FILE *F = env->out;\n";
$line .= '  lc_efprintf(ia32_get_arg_env(), F, "\tmov %1s, %1d\t\t\t/* %+F */\n", n, n, n);'."\n}\n\n";
push(@obst_func, $line);

$line = "void emit_".$arch."_Perm(ir_node *n, emit_env_t *env)";
push(@obst_header, $line.";\n");
$line .= " {\n  FILE *F = env->out;\n";
$line .= '  lc_efprintf(ia32_get_arg_env(), F, "\txchg %1s, %1d\t\t\t/* %+F */\n", n, n, n);'."\n}\n\n";
push(@obst_func, $line);

foreach my $op (keys(%nodes)) {
  my %n = %{ $nodes{"$op"} };

  # skip this node description if no emit information is available
  next if (!$n{"emit"} || length($n{"emit"}) < 1);

  $line = "void emit_".$arch."_".$op."(ir_node *n, emit_env_t *env)";
  push(@obst_header, $line.";\n");
  push(@obst_func, $line." {\n  FILE *F = env->out;\n");

  my $cio = 0;
  # check in/out register if needed
  if (exists($n{"check_inout"}) && $n{"check_inout"} == 1) {
    push(@obst_func, "  equalize_dest_src(F, n);\n\n");
    $cio = 1;
  }

  my @emit = split(/\n/, $n{"emit"});

  foreach my $template (@emit) {
    # substitute only lines, starting with a '.'
    if ($template =~ /^(\d*)\.\s*/) {
      my @params;
      my $res    = "";
      my $res2   = "";
      my $indent = "  "; # default indent is 2 spaces

      $indent = " " x $1 if ($1 && $1 > 0);
      # remove indent, dot and trailing spaces
      $template =~ s/^\d*\.\s*//;
      # substitute all format parameter
      while ($template =~ /\%(([asd])(\d)|([com]))/) {
        $res  .= $`;      # get everything before the match
        $res2 .= $`;

        if ($4 && $4 eq "c") {
          push(@params, "n");
          $res  .= "\%c";
          $res2 .= "\%c";
        }
        elsif ($4 && $4 eq "o") {
          push(@params, "n");
          $res  .= "\%o";
          $res2 .= "\%o";
        }
        elsif ($4 && $4 eq "m") {
          push(@params, "n");
          $res  .= "\%m";
          $res2 .= "\%m";
        }
        elsif ($2 && $2 eq "s") {
          push(@params, "n");
          if ($cio && $3 == 2) {
            # check_in_out was set: if (s1 != d1) we
            # need to exchange s2 by s1
            $res2 .= "%1s"; # get name for first register
          }
          else {
            $res2 .= "%".$3."s"; # substitute %sx with %xs
          }
          $res .= "%".$3."s"; # substitute %sx with %xs
        }
        elsif ($2 && $2 eq "d") {
          push(@params, "n");
          $res  .= "%".$3."d"; # substitute %sx with %xs
          $res2 .= "%".$3."d"; # substitute %sx with %xs
        }
        elsif ($2 && $2 eq "a") {
          push(@params, "get_irn_n(n, ".($3 - 1).")");
          $res  .= "%+F";
          $res2 .= "%+F";
        }

        $template = $'; # scan everything after the match
      }
      $res  .= $template; # get the remaining string
      $res2 .= $template; # get the remaining string

      my $parm = "";
      $parm = ", ".join(", ", @params) if (@params);

      if ($cio) {
        push(@obst_func, $indent."if (get_irn_arity(n) > 1 && get_$arch\_reg_nr(n, 1, 1) == get_$arch\_reg_nr(n, 0, 0)) {\n");
        push(@obst_func, $indent.'  lc_efprintf(ia32_get_arg_env(), F, "\t'.$res2.'\n"'.$parm.');'."\n");
        push(@obst_func, $indent."}\n");
        push(@obst_func, $indent."else {\n");
        push(@obst_func, $indent.'  lc_efprintf(ia32_get_arg_env(), F, "\t'.$res.'\n"'.$parm.');'."\n");
        push(@obst_func, $indent."}\n");
      }
      else {
        push(@obst_func, $indent.'lc_efprintf(ia32_get_arg_env(), F, "\t'.$res.'\n"'.$parm.');'."\n");
      }
    }
    else {
      push(@obst_func, $template,"\n");
    }
  }
  push(@obst_func, "}\n\n");
}

open(OUT, ">$target_h") || die("Could not open $target_h, reason: $!\n");

my $creation_time = localtime(time());

my $tmp = uc($arch);

print OUT<<EOF;
#ifndef _GEN_$tmp\_EMITTER_H_
#define _GEN_$tmp\_EMITTER_H_

/**
 * Function prototypes for the emitter functions.
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#include "irnode.h"
#include "$arch\_emitter.h"

EOF

print OUT @obst_header;

print OUT "#endif /* _GEN_$tmp\_EMITTER_H_ */\n";

close(OUT);

open(OUT, ">$target_c") || die("Could not open $target_c, reason: $!\n");

$creation_time = localtime(time());

print OUT<<EOF;
/**
 * Generated functions to emit code for assembler ir nodes.
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#include <stdio.h>

#include "irnode.h"
#include "gen_$arch\_emitter.h"
#include "$arch\_new_nodes.h"

EOF

print OUT @obst_func;

close(OUT);
