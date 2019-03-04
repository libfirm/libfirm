libFirm 1.22.1 (2016-01-07)
---------------------------
* make: Fix cmake/make build
* ia32: New just in time compilation mode which compiles into a memory buffer
* amd64: Support PIC with PLT for ELF
* ia32: Add `ia32-get_ip={pop,thunk}`
* ia32: Generate `mov $~1, %r; rol x, %r` for `~(1 << x)`
* ia32: Generate `mov $~0x80000000, %r; ror x, %r` for `~(0x80000000 >> x)`
* be: Stub support for the asm constraint modifier `%`
* amd64, ia32: Support the asm constraint `e`
* be: Support the asm modifier `c` in all backends
* ia32: Support JIT compilation
* be: Improve permutation moving for copy coalescing
* ir: Improve handling of negative overflow in float to int tarval conversion
* amd64: Improve matching of immediates during instruction selection
* amd64: Add peephole optimization `mov $0, %r` -> `xorl %r, %r`
* amd64: Add peephole optimization `lea c(%r), %r` -> `add $c, %r`
* amd64: Add peephole optimization `lea (%r1, %r2), %r1` -> `add %r2, %r1` and the commutated case
* ia32: Add peephole optimization `lea c(, %i, 2), %d` -> `lea c(%i, %i), %d`
* ia32: Add peephole optimization `lea (%b), %d` -> `mov %b, %d`
* ia32: Add peephole optimization `testl $0x0000XX00, %eRx` -> `testb $0xXX, %Rh`
* ia32: Generate slightly better code to load the floating-point constants `-0.0` and `-1.0`
* amd64: Reduce number of stack adjustments
* api: Set the length of an array type solely when creating the array type
* api: Set whether a function type is variadic and its calling convention and additional properties solely when creating the function type
* api: Automatically infer the mode when creating Add, And, Div, Eor, Minus, Mod, Mul, Mulh, Mux, Not, Or, Shl, Shr, Shrs and Sub nodes
* api: Remove the notion of atomic entities, use the initializer access functions instead
* api: Remove visibility from types
* api: Remove the type flag `tf_variable_size`, test the array size for 0 instead
* api: Remove `plist`, use `pdeq` instead
* api: Remove `get_{class,segment,struct,union}_{ident,name}()`, use `get_compound_{ident,name}()` instead
* ir: Improve IR graph verifier
* arm: Improve address mode use in instruction selection
* be, ir: Improve preservation of debug info during transformations
* amd64: Improve use of `lea` in instruction selection
* sparc: Support the asm constaint `K`
* sparc: Support computed goto
* arm: Support computed goto
* opt: Simplify computed goto with known destination to unconditional branch
* opt: Handle constant folding for the builtins `clz`, `ctz`, `ffs`, `parity` and `popcount`
* arm: Avoid redundant sign/zero extension right after a load
* amd64, arm: Avoid redundant sign/zero extension right before a store
* amd64: Avoid redundant sign/zero extension for truncation
* amd64: Mark floating point constants as candidates for rematerialization
* amd64: Improve block schedule by removing empty basic blocks
* amd64: Use cltd/cqto for sign extension before a division
* amd64: Use store with immediate for call arguments
* opt: Add local optimization `a * b [-1 <= b <= 0] -> -(a & b)`
* sparc: More strict checking of modifiers of placeholders in asm templates
* arm: Support inline asm with with constraints `I`, `J`, `K`, `L`, `M`, `Q`, `g`, `i`, `l`, `m`, `n` and `r` as well as modifiers `B`, `C` and `c`
* mips: Add new backend
* be: By default permutation lowering now uses copy instead swap
* amd64: Improve code generation for `v & 0xFF/0xFFFF/0xFFFFFFFF`
* amd64, ia32: Always use potentially smaller address mode c(x,x,1) instead of c(,x,2)
* opt: Add local optimization `a >>s b >>u (n - 1) -> a >>u (n - 1)`
* ia32: Improve code generation for 64 bit multiplication if the lower half of a factor is 0 or 1
* amd64: Add peephole optimization `cmp $0, %r` -> `test %r, %r`
* amd64, ia32: Support the asm operand modifiers `A`, `P`, `X` and `p`
* be: Accept `#` and `%` as register name prefix for gcc compatibility
* arm, mips, sparc: Handle clobbers in asm
* riscv: Add new backend
* be: Improve modelling of register demand
* amd64, ia32: Support all address modes in inline asm
* ir, be: Support `asm goto`; even allow output constraints, which gcc and clang do not
* Bugfixes

libFirm 1.22.0 (2015-12-31)
---------------------------
* Improved PIC support, linux/elf is now supported
* Many additional local optimization rules (instcombine)
* Inline assembly support for sparc/amd64
* Improved constant bit analysis
* Improved load/store optimization, featuring compound value optimizations
* Improved reassociation pass
* Improved amd64 (aka x86_64) backend, it is now able to bootstrap libfirm
* Improved arm backend (but still experimental)
* Improved inliner (can inline compound types)
* Compiletime performance improvements
* Cleanups and API simplifications
* Switch to C99 and corresponding code cleanup and simplification
* More details in dumped IR graphs
* Streamlined IR
* Improved IR graph verifiers
* Further unify backend infrastructure
* Support for arrays as parameter and return types
* Updated documentation
* Perform precise 80bit spilling for x87 floating point
* Improved permutation decomposition for copy coalescing
* Added bitwise 'don't care' analysis and use it for optimizing occult constants
* Support builtin 'saturating increment' (amd64, ia32, sparc)
* Improved generated code for division by constant
* Support builtin 'compare and swap' (amd64, ia32, sparc)
* Support alias entities
* Improved scalar conversion optimization
* Added store combine optimization
* Overhauled representation and handling of infinite loops
* Improved use of flag results generated as side effect of instructions (ia32)
* Improved calculation of execution frequency estimation for large graphs
* Improved instruction selection (amd64, arm, ia32, sparc)
* Improved x87 floating point code generation
* Support early clobbers in inline assembly
* Support x87 constraints in inline assembly (ia32)
* Support `__attribute__((visibility("hidden")))` and `__attribute__((visible("protected")))`
* Optionally use CMake as build system
* Support `make install` with the usual variables (INSTALLPREFIX, ...)
* Bugfixes

libFirm 1.21.0 (2012-11-16)
---------------------------
* Improvements of x86 backend (esp. x87 floatingpoint code)
* Improvements to sparc backend (better use of delay slots)
* Improved local optimization rules (esp. conversions)
* Make compiler more deterministic
* Bugfixes

libFirm 1.20.0 (2011-12-07)
---------------------------

* Further improvemens to sparc backend (SPEC2000 works with softfloat)
* Tuning of x86 backend
* Software floatingpoint lowerer
* Fixed firm profiling
* New pass management allowing to specify pre-/postconditions for passes
* Remove dependency on liblpp, add support for gurobi ILP solver
* Experimental dwarf debugging support
* Code cleanups, refactoring
* Restructured API documentation
* Bugfixes (we did a lot of csmith testing)

libFirm 1.19.1 (2011-05-17)
---------------------------

* Fix some set_XXX functions not being exported in the shared library

libFirm 1.19.0 (2011-03-15)
---------------------------

* Includes "SSA-Based Register Allocation with PBQP"
* Improved Sparc backend
* New (optimistic) fixpoint based value-range propagation/bit analysis
* Code cleanup and refactoring
* Bugfixes

libFirm 1.18.1 (2010-05-05)
---------------------------

* Fix bug where stackframe was not always setup for -fno-omit-frame-pointer
* bugfixes in Asm handling

libFirm 1.18.0 (2010-04-15)
---------------------------

* Includes "Preference Guided Register Assignment" algorithm
* Experimental Value Range Propagation
* Loop Inversion and experimental Loop Unrolling code
* Simplified construction interface. Most node constructors don't need graph/block arguments anymore.
* Reworked type interface. Type names are optional now. Support for additional linkage types that among others support C++ 'linkonce' semantics now.
* Small changes in constructors and node getters/setters (mostly adding 'const' to some getters)
* code cleanup, smaller improvements in API specification
* bugfixes

libFirm 1.17.0 (2009-05-15)
---------------------------

* bugfixes
* advanced load/store optimization which hoists loads out of loops
* Internal restruturing: Alot of node structures are automatically generated
   from a specification file now.
* Add support for multiple calling conventions
* New experimental support for reading and writing programgraphs to disk
* Support and optimizations for trampolines
* fix PIC support

libFirm 1.16.0 (2009-01-28)
---------------------------

* bugfixes
* support for builtin nodes

libFirm 1.15.0 (2008-12-01)
---------------------------
* bugfixes

libFirm 1.14.0 (2008-11-22)
---------------------------

* Implementation of Clicks Combined Analysis/Optimizations
* New switch lowering code
* support for global asm statements
* improved asm support
* PIC support for Mac OS X
* New register pressure minimizing scheduler
* Improvements to spill algorithm
* fix endless loop problems
* further improve inlining heuristics
* improve peephole optimizations for x86
* bugfixes

libFirm 1.13.0 (2008-07-31)
---------------------------

* VanDrunen's GVN-PRE fixed
* operator strength reduce fixed and improved
* fixed 64bit code generation for some rare compare cases
* better tailrecursion optimization: handles x * func() and x + func()
* improved inliner: better heuristics for inlining, can now inline recursive calls
* improved spiller
* lowering of CopyB nodes
* better memory disambiguator
* float->64bit conversion fixed for x87
* removed old verbosity level based debugging: all modules use the new debug facility
* Improved Confirm based optimization and conditional evaluation (using Confirm nodes)
* BugFixes: tail recursion, load/store optimization, lowering of structure return, conditional
  evaluation, removal of unused methods
* reduced numer of indirections for backend operation
* ia32 Backend: supports more CPU architectures
* ARM Backend: fixed frame access
* support for special segments (like constructors, destructors)

libFirm 1.12.1 (2008-02-18)
---------------------------

* bugfixes for new style initializers with bitfield types
* make lowerer look at constant initializers too

libFirm 1.12.0 (2008-02-14)
---------------------------

* dependency on libcore and libobstack dropped
* there's an alternative easier to use way to construct compound initializers
* bugfixes
* improved support for exceptions
* speed improvements
* optimization of known libc functions

libFirm 1.11.0 (2008-11-05)
---------------------------

* Lots of bugfixes
* Compilation speed improved
* Completely improved and rewritten handling of x86 address mode
* Optimized Mul -> Lea,Shift,Add transformation
* 64bit operations fixed and improved
* More local optimizations
* New backend peephole optimizations
* Explicit status flag modeling (only for x86 for now)
* Improvements of Load/Store optimization and alias analysis
* All C benchmarks from Spec CINT2000 work now (with our edg frontend)
