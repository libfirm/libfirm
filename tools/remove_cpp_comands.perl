#!/usr/local/bin/perl

#
# Project:     libFIRM
# File name:   ir/tools/remove_cpp_commands.perl
# Purpose:
# Author:      Goetz Lindenmaier
# Modified by:
# Created:     8.2002
# CVS-ID:      $Id$
# Copyright:   (c) 2002-2003 Universität Karlsruhe
# Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
#

# This is necessary until CRECODER is integrated with the preprocessor.
#
# Take a C header file and remove all preprocessor commands.
# Wrap all typedefs with a preprocessor guard,
# add all possible typedefs in firm at the beginning of the file, also
# wrapped with a preprocessor guard
#
# Call
#  perl remove_cpp_comands.perl <file>
# for a file with name <file>.h.

# open files
$infile = $ARGV[0];

open(IN, $infile);
@lines = <IN>;
close(IN);

$outfile = $infile;
open(OUT, ">$outfile");

$typedeffile = "firm_typedefs.h";
open(TDF, ">>$typedeffile");

#dump headers
print OUT "\n#include \"firm_typedefs.h\"\n\n";


#Unresolved preprocessor commands
print TDF "#define INLINE\n";
print TDF "#define FILE int *\n";
print TDF "#ifndef MYTYPEDEFS\n#define MYTYPEDEFS\n";
#print TDF "#undef __cplusplus\n";
print TDF "typedef unsigned long size_t;\n";
#print TDF "typedef enum { false = 0, true = 1 } bool;\n";  geht nicht, false und true JAVA Schluesselwoerter
print TDF "typedef int bool;\n";
# Some typedefs we need because of wrond order resultion by this script
print TDF "#ifndef _ENTITY_TYPEDEF_\n#define _ENTITY_TYPEDEF_\ntypedef struct entity entity;\n#endif\n";
# Some typedefs we need because we do not include the according header files
print TDF "typedef struct dbg_info dbg_info;\n";
print TDF "#endif /* MYTYPEDEFS */ \n";

#to collect typedefs
$openbracket = 0;
$guardedtypedef = 0;

$scndlastline = "";
$lastline = "";

$eat = 0;
$multiline = 0;

foreach $line (@lines) {

    if ($line =~ /\#ifdef __cplusplus/) {
#       There is extern "C" in firm.h, guarded by #ifdef __cplusplus
#       crecoder does not grok the extern "C", so remove thses three lines.
	$eat = 2;
    } elsif ($eat > 0) {
	$eat = $eat -1;
    } elsif ($multiline > 0) {
        # this line connects a previous one, kill it
        if ($line =~ /\\$/) {
          $multiline = 1;
        } else {
          $multiline = 0;
        }
    } elsif ($line =~ /^\#/) {
	# eat the line
	$scndlastline = $lastline;
	$lastline = $line;
        if ($line =~ /\\$/) {
          $multiline = 1;
        } else {
          $multiline = 0;
        }
    } elsif ($openbracket == 1) {
	print TDF "$line";
	if ((index($line, "}") > -1)) {
	    $openbracket = 0;
	    if (($guardedtypedef == 1)) {
		print TDF "#endif\n";
		$guardedtypedef = 0;
	    }
	}
	$lastline = "";
    } elsif ($line =~ /typedef/) {
	# move the full typedef to firm_typedefs.h

	if (($lastline =~ /^\#/)   ) {
	    $guardedtypedef = 1;
	    print TDF "$scndlastline"; $scndlastline = "";
	    print TDF "$lastline"; 	$lastline = "";
	}
	print TDF "$line";
	if ((index($line, "{") > -1)) {
	    $openbracket = 1;
	} elsif (($guardedtypedef == 1)) {
	    print TDF "#endif\n";
	    $guardedtypedef = 0;
	}
	if ((index($line, "}") > -1)) {
	    $openbracket = 0;
	    if (($guardedtypedef == 1)) {
		print TDF "#endif\n";
		$guardedtypedef = 0;
	    }
	}
    } else {
	print OUT "$line";
	$scndlastline = $lastline;
	$lastline = "";
    }
}


close(TDF);
close(OUT);
