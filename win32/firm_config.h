#ifndef _FIRM_CONFIG_H
#define _FIRM_CONFIG_H
/* This file was automtically generated from libFirm's configure */

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to 1 if long double works and has more range or precision than
   double. */
#undef HAVE_LONG_DOUBLE

/* Define to 1 for heap analysis support */
/* #undef DO_HEAPANALYSIS */

/* Define the right volatile token */
/* #undef volatile */

/* Define the right inline token */
/* #undef inline */

/* Define the right const token */
/* #undef const */

/* define to 1 to enable debugging stuff. */
#ifndef DEBUG_libfirm
#define DEBUG_libfirm  1
#endif

/* define to 1 to have wchar_t support for identifiers */
#ifndef FIRM_ENABLE_WCHAR
#define FIRM_ENABLE_WCHAR 1
#endif

/* define to 1 to use the libcore */
#ifndef WITH_LIBCORE
#define WITH_LIBCORE 1
#endif

/* define to 1 to use the ILP solver */
#ifndef WITH_ILP
#define WITH_ILP 1
#endif

/* define to 1 to use JVM calling needed for the Java-based coalescer in firmbe */
/* #undef WITH_JVM */

/* Define to disable assertion checking.  */
/* #undef NDEBUG */

/* undef to disable inlining */
#ifndef USE_INLINING
#define USE_INLINING  1
#endif

/* Define to 1 if Firm statistics are activated */
#ifndef FIRM_STATISTICS
#define FIRM_STATISTICS  1
#endif

/* Define to 1 if Firm hooks are activated */
#ifndef FIRM_ENABLE_HOOKS
#define FIRM_ENABLE_HOOKS 1
#endif

#ifdef USE_INLINING
#ifndef INLINE
#define INLINE  __inline
#endif
#else
#ifndef INLINE
#define INLINE
#endif
#endif

/* enabled plugin: ir/be/ia32 */
#ifndef PLUGIN_IR_BE_IA
#define PLUGIN_IR_BE_IA 32 1
#endif
/* enabled plugin: ir/be/ppc32 */
#ifndef PLUGIN_IR_BE_PPC
#define PLUGIN_IR_BE_PPC 32 1
#endif
/* enabled plugin: ir/be/mips */
#ifndef PLUGIN_IR_BE_MIPS
#define PLUGIN_IR_BE_MIPS  1
#endif
/* enabled plugin: ir/be/arm */
#ifndef PLUGIN_IR_BE_ARM
#define PLUGIN_IR_BE_ARM  1
#endif
/* enabled plugin: ir/be/firm */
#ifndef PLUGIN_IR_BE_FIRM
#define PLUGIN_IR_BE_FIRM  1
#endif
/* enabled plugin: ir/be */
#ifndef PLUGIN_IR_BE
#define PLUGIN_IR_BE  1
#endif

#endif /* _FIRM_CONFIG_H */
