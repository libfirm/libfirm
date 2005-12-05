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
/* #undef WITH_LIBCORE */

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

/* Define to 1 if Firm inplace edges are activated */
#ifndef FIRM_EDGES_INPLACE
#define FIRM_EDGES_INPLACE 1
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

#endif /* _FIRM_CONFIG_H */
