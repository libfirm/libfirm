libFirm -- A graph based SSA intermediate representation
========================================================

1. Introduction
---------------

The Firm library implements the Firm intermediate representation (ir).
You can find an old description of Firm in [TLB:99].

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

* python (2.7.x or >=3.3 are supported)
* perl
* an ANSI C99 compiler (gcc, clang, icc are known to work)

2.1 Build using make
--------------------

Just type 'make' inside libfirms source directory. The results are put into a
directory called "build". You can override the existing preprocessor, compiler
and linker flags by creating a 'config.mak' file.

2.2 Using cmake
---------------

libfirm supports a cmake build system. CMake is a complexer build system than
the makefile based build and most libfirm developers do not use it. However it
can adapt the compiler and linker flags to build shared libraries for a wider
range of systems, provides an installation target and is often more familiar
for people preparing packages for distribution.

3. Further information and Contact
----------------------------------

Official website
	<http://libfirm.org/>

Contact E-Mail:
	<firm@ipd.info.uni-karlsruhe.de>

Mailing list:
	<https://lists.sourceforge.net/lists/listinfo/libfirm-user>

Bugtracker:
	<http://pp.ipd.kit.edu/~firm/bugs>

Internet relay chat:
	<irc://chat.freenode.net/#firm>
