#!/usr/bin/perl -w

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

my $target_c = $target_dir."/new_nodes.c";
my $target_h = $target_dir."/new_nodes.h";

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

#print Dumper(%nodes);

# create c code file from specs

my @obst_opvar;       # stack for the "ir_op *op_<arch>_<op-name> = NULL;" statements
my @obst_get_opvar;   # stack for the get_op_<arch>_<op-name>() functions
my @obst_constructor; # stack for node constructor functions
my @obst_new_irop;    # stack for the new_ir_op calls
my @obst_header;      # stack for function prototypes
my $temp;
my $orig_op;
my $arity;

push(@obst_header, "void create_bearch_asm_opcodes(void);");

foreach my $op (keys(%nodes)) {
  my %n = %{ $nodes{"$op"} };

  $orig_op = $op;
  $op      = $arch."_".$op;
  $arity   = $n{"arity"};

  push(@obst_opvar, "ir_op *op_$op = NULL;\n");
  push(@obst_get_opvar, "ir_op *get_op_$op(void)   { return op_$op; }\n");
  push(@obst_get_opvar, "int    is_$op(ir_node *n) { return get_irn_op(n) == op_$op ? 1 : 0; }\n\n");

  $n{"comment"} =~ s/^"|"$//g;
  $n{"comment"} = "/* ".$n{"comment"}." */\n";
  push(@obst_constructor, $n{"comment"});

  # create constructor head
  my $complete_args = "";
  my $arg_names     = "";
  $temp = "ir_node *new_rd_$op(dbg_info *db, ir_graph *irg, ir_node *block";
  if ($n{"args"} eq "DEFAULT") { # default args
    if ($n{"arity"} !~ /^[0-3]$/) {
      print "DEFAULT args require arity 0,1,2 or 3! Ignoring op $orig_op!\n";
      next;
    }
    for (my $i = 1; $i <= $n{"arity"}; $i++) {
      $complete_args .= ", ir_node *op".$i;
      $arg_names     .= ", op".$i;
    }
    $complete_args .= ", ir_mode *mode";
    $arg_names     .= ", mode";
  }
  else { # user defined args
    for my $href (@{ $n{"args"} }) {
      $complete_args .= ", ".$href->{"type"}.$href->{"name"};
      $arg_names     .= ", ".$href->{"name"};
    }
  }
  $complete_args = substr($complete_args, 2);
  $temp .= ", $complete_args)";
  push(@obst_constructor, $temp." {\n");
  push(@obst_header, $temp.";\n");

  # emit constructor code
  if ($n{"rd_constructor"} eq "DEFAULT") { # default constructor
    if ($n{"arity"} !~ /^[0-3]$/) {
      print "DEFAULT rd_constructor requires arity 0,1,2 or 3! Ignoring op $orig_op!\n";
      next;
    }
    $temp  = "  ir_node *res;\n";
    $temp .= "  ir_node *in[$arity];\n" if ($arity > 0);
    $temp .= "\n";
    $temp .= "  if (!op_$op) {\n";
    $temp .= "    assert(0);\n";
    $temp .= "    return NULL;\n";
    $temp .= "  }\n\n";
    for (my $i = 1; $i <= $arity; $i++) {
      $temp .= "  in[".($i - 1)."] = op".$i.";\n";
    }
    $temp .= "  res = new_ir_node(db, irg, block, op_$op, mode, $arity, ".($arity > 0 ? "in" : "NULL").");\n";
    $temp .= "  res = optimize_node(res);\n";
    $temp .= "  irn_vrfy_irg(res, irg);\n";
    $temp .= "  return res;\n";

    push(@obst_constructor, $temp);
  }
  else { # user defined constructor
    push(@obst_constructor, $n{"rd_constructor"});
  }

  # close constructor function
  push(@obst_constructor, "}\n\n");

  # create the _r and _d wrapper
  $temp  = "ir_node *new_r_$op(ir_graph *irg, ir_node *block, $complete_args)";
  push(@obst_header, $temp.";\n");
  $temp .= " {\n";
  $temp .= "  return new_rd_$op(NULL, irg, block".$arg_names.");\n";
  $temp .= "}\n\n";
  push(@obst_constructor, $temp);

  $temp  = "ir_node *new_d_$op(dbg_info *db, $complete_args)";
  push(@obst_header, $temp.";\n");
  $temp .= " {\n";
  $temp .= "  return new_rd_$op(db, current_ir_graph, current_ir_graph->current_block".$arg_names.");\n";
  $temp .= "}\n\n";
  push(@obst_constructor, $temp);

  $temp  = "ir_node *new_$op($complete_args)";
  push(@obst_header, $temp.";\n");
  $temp .= " {\n";
  $temp .= "  return new_d_$op(NULL".$arg_names.");\n";
  $temp .= "}\n\n";
  push(@obst_constructor, $temp);

  # construct the new_ir_op calls
  my $arity_str = $arity == 0 ? "zero" : ($arity == 1 ? "unary" : ($arity == 2 ? "binary" : ($arity == 3 ? "trinary" : $arity)));
  $temp  = "  op_$op = new_ir_op(get_next_ir_opcode(), \"$op\", op_pin_state_".$n{"state"}.", ".$n{"op_flags"};
  $temp .= ", oparity_".$arity_str.", 0, sizeof(asmop_attr), &ops);\n";
  push(@obst_new_irop, $temp);
}

# emit the code
my $creation_time = localtime(time());

open(OUT, ">$target_c") || die("Could not open $target_c, reason: $!\n");

print OUT<<ENDOFHEADER;
/**
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the $arch assembler irg.
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "firm_common_t.h"
#include "irvrfy_t.h"

#include "../firm2arch_nodes_attr.h"

#include "new_nodes.h"
#include "dump_support.inl"

tarval *get_Immop_tarval(ir_node *node) {
  asmop_attr *attr = (asmop_attr *)get_irn_generic_attr(node);
  return attr->tv;
}

ENDOFHEADER

print OUT @obst_opvar;
print OUT "\n";
print OUT @obst_get_opvar;
print OUT "\n";
print OUT @obst_constructor;

print OUT<<ENDOFMAIN;
/**
 * Creates the architecture specific firm operations
 * needed for the assembler irgs.
 */
void create_bearch_asm_opcodes(void) {
#define N   irop_flag_none
#define L   irop_flag_labeled
#define C   irop_flag_commutative
#define X   irop_flag_cfopcode
#define I   irop_flag_ip_cfopcode
#define F   irop_flag_fragile
#define Y   irop_flag_forking
#define H   irop_flag_highlevel
#define c   irop_flag_constlike

  ir_op_ops ops;

  memset(&ops, 0, sizeof(ops));

  /* enter our modified dumper */
  ops.dump_node = dump_node_ia32;

ENDOFMAIN

print OUT @obst_new_irop;
print OUT "}\n";

close(OUT);

$creation_time = localtime(time());

open(OUT, ">$target_h") || die("Could not open $target_h, reason: $!\n");
print OUT<<EOF;
#ifndef NEW_NODES_H
#define NEW_NODES_H

/**
 * Function prototypes for the assembler ir node constructors.
 * DO NOT EDIT THIS FILE, your changes will be lost.
 * Edit $specfile instead.
 * created by: $0 $specfile $target_dir
 * date:       $creation_time
 */

tarval *get_Immop_tarval(ir_node *node);

EOF

print OUT @obst_header;
print OUT "\n#endif\n";

close(OUT);
