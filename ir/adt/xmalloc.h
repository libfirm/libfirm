/*
 * Project:     libFIRM
 * File name:   ir/adt/xmalloc.h
 * Purpose:     More comfortable allocations.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1995, 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef _XMALLOC_H_
#define _XMALLOC_H_

#include "host.h"

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

/* xmalloc() & friends.

   The macros set tmalloc_tag to __FILE__, the functions leave it
   alone.  Use the latter if you set it yourself.  See tmalloc.c for
   details.  */

extern void *xmalloc (size_t);
extern void *xrealloc (void *, size_t);
extern char *xstrdup (const char *);
extern void  xnomem (void);
extern void  free (void *);

# define xmalloc(size)       (XMALLOC_TRACE (xmalloc) ((size)))
# define xrealloc(ptr, size) (XMALLOC_TRACE (xrealloc) ((ptr), (size)))
# define xstrdup(str)        (XMALLOC_TRACE (xstrdup) ((str)))
# define xfree(ptr)          (XMALLOC_TRACE free ((ptr)))

#if defined(HAVE_GNU_MALLOC) && defined(DEBUG)
extern const char *tmalloc_tag;
# define XMALLOC_TRACE tmalloc_tag = __FILE__,
#else
# define XMALLOC_TRACE
#endif

#endif /* _XMALLOC_H_ */
