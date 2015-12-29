libFirm -- A graph based SSA intermediate representation
========================================================

Introduction
------------

The Firm library implements the Firm intermediate representation (ir).
You can find an old description of Firm in [TLB:99].

libFirm contains algorithms for construction of the SSA form directly from the
attributed syntax tree. A set of analyses and optimization phases is provided.
This version includes a complete backend for the IA32 and SPARC architecture,
as well as unfinished backends for MIPS, ARM, and AMD64.

Building and Installation
-------------------------

Prerequisites for the build:

* python (2.7.x or >=3.3 are supported)
* perl
* an ANSI C99 compiler (gcc, clang, icc are known to work)

### Building with make

Just type 'make' in the source directory. The results are put into a
directory called "build". You can override the existing preprocessor, compiler
and linker flags by creating a 'config.mak' file.

### Building with cmake

libFirm has an additional cmake build system. CMake is a complexer build system
than the make based build and most libFirm developers do not use it.  However
it can adapt the compiler and linker flags to build shared libraries for a
wider range of systems, provides an installation target and is often more
familiar for people preparing packages for distribution.

Further Information and Contact
-------------------------------

Official website: http://libfirm.org/

Contact E-Mail: firm@ipd.info.uni-karlsruhe.de

Mailing list: https://lists.sourceforge.net/lists/listinfo/libfirm-user

Bugtracker: http://pp.ipd.kit.edu/~firm/bugs

Internet relay chat: irc://chat.freenode.net/#firm
