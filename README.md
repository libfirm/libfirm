libFirm -- A graph based SSA intermediate representation
========================================================

1. Introduction
---------------

The Firm library implements the Firm intermediate representation (ir). An old
description of Firm can be found in [TLB:99].

libFirm contains algorithms for construction of the SSA form directly from the
attributed syntax tree. A set of analyses and optimization phases is provided.
This version includes a complete backend for the IA32 and SPARC architecture,
as well as unfinished backends for MIPS, ARM, and AMD64.

1.1. Features
-------------

- works exclusively on a graph based SSA representation up to the code emission.
  Based on the work of C. Click and M. Trapp
- written in portable C99. Known to run on gcc on Linux, FreeBSD, Cygwin, clang,
  ICC on windows.
- includes doxygen documentation
- support for object oriented type hierarchies
- Analyses: dominance, loop tree, execution frequency, control dependencies,
            inter procedural call graph, rapid type, def-use, alias analysis,
            class hierarchy analysis, ...
- Optimizations: constant folding, local common subexpression elimination,
                 global common subexpression elimination, code placement,
                 operator strength reduction, scalar replacement, load/store,
                 control flow optimizations, if-conversion, partial condition
                 evaluation, reassociation, tail recursion elimination,
                 inlining, procedure cloning, dead code elimination, ...
- local common subexpression eliminiation, constant folding,
  constant propagatation, arithmetic identities happen implicitely
- extensive checkers
- enhanced debugging support: breakpoints on node creation, entity creation,
            graph dumping, visual studio debug extension
- lowering of intrinsics, double word arithmetics, bitfields
- backend with SSA based register allocation including several algorithms for
  spilling and copy coalescing. Instruction and block scheduling, support for
  ABI handling.
- working ia32 backend with support for x87 and SSE2 floating point
- unfinished backends for SPARC, ARM, AMD64


2. Building and Installation
----------------------------

Prerequisites for the build:

* python (2.6.x, 2.7.x or >=3.3 are supported)
* perl
* an ANSI C99 compiler (gcc, clang, icc are known to work)

Building on unix variants/cygwin:

1. Change into the directory containing libfirms source
2. Create a directory named build 'mkdir build'. Change into this directory
   and execute the configure script. 'cd build ; ../configure'
3. Type 'make' to compile the package
4. You may install libfirm as super user by typing 'make install'

3. Usage
--------

To generate FIRM code from a compiler frontend read the documentation
"libFIRM: A Library for Compiler Optimization Reaserch Implementing
Firm", UKA tech-report 2002-5. About Firm you can learn in UKA
tech-report 1999-14.

4. Contact
----------

You can contact us at
	<firm@ipd.info.uni-karlsruhe.de>

There's a mailing list here:
	<https://lists.sourceforge.net/lists/listinfo/libfirm-user>

We have a bugtracker at:
	<http://pp.info.uni-karlsruhe.de/~firm/bugs>
