/* Misc. declarations.
   Copyright (C) 1995, 1996 Markus Armbruster
   All rights reserved. */

/* $Id$ */

#ifndef _MISC_H_
#define _MISC_H_

/* Declare alloca() */

#ifdef __GNUC__
  /* @@@ on a true GNU system, this is defined by stdlib.h */
# undef alloca
# define alloca(size) __builtin_alloca (size)
#else
# if HAVE_ALLOCA_H
#   include <alloca.h>
# else
#   if defined(_AIX) && !defined(C_ALLOCA)
      /* if your version of AIX chokes on this, use gcc @@@ or alloca.o */
#pragma alloca
#   else
#     ifndef alloca /* predefined by HP cc +Olibcalls */
void *alloca ();
#     endif
#   endif
# endif
#endif


#include "host.h"
#include "bool.h"

/* Alignment of nodes, cf common/tag.h, a power of two.

   Most nodes are allocated from obstacks and therefore are aligned
   identically on the obstacks' alignment boundary.  We'd like gcc to
   align *all* nodes identically to avoid its cast-align warning and
   perhaps allow for better code.  Since the common node alignment is
   not known in time, we use an educated guess and check it against
   the correct value later.  */
#define NODE_ALIGN ALIGNOF (struct { void *p; int i; })


/* Miscellaneous */

#ifndef MAX
# define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
# define MIN(a,b) ((a) > (b) ? (b) : (a))
#endif

#define IS_POW2(n) ((((n)-1) & (n)) == 0)

typedef int (*cmp_fun) (const void *, const void *);


/* xmalloc() & friends.

   The macros set tmalloc_tag to __FILE__, the functions leave it
   alone.  Use the latter if you set it yourself.  See tmalloc.c for
   details.  */

extern void *xmalloc (size_t);
extern void *xrealloc (void *, size_t);
extern char *xstrdup (const char *);
extern void xnomem (void);

# define xmalloc(size) (XMALLOC_TRACE (xmalloc) ((size)))
# define xrealloc(ptr, size) (XMALLOC_TRACE (xrealloc) ((ptr), (size)))
# define xstrdup(str) (XMALLOC_TRACE (xstrdup) ((str)))
# define xfree(ptr) (XMALLOC_TRACE free ((ptr)))

#if defined(HAVE_GNU_MALLOC) && defined(DEBUG)
extern const char *tmalloc_tag;
# define XMALLOC_TRACE tmalloc_tag = __FILE__,
#else
# define XMALLOC_TRACE
#endif

#endif /* _MISC_H_ */
