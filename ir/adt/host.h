/* Declarations describing the host machine and C compiler.
   Copyright (C) 1995, 1996 Markus Armbruster
   All rights reserved.*/

/* $Id$ */

#ifndef _HOST_H
#define _HOST_H

#include <stddef.h>

/**
 * @file host.h
 */

/** A size handled efficiently by malloc(), at least 1K.  */
#define PREF_MALLOC_SIZE 2048


/** A wrapper around GNU C's __attribute__ */

/* According to the documentation, the attributes we are interested in
   work with 2.5, but we encountered trouble before 2.7.  */
#if defined (__GNUC__) && __GNUC__ >= 2 && __GNUC_MINOR__ >= 7
# define HAVE_ATTRIBUTE 1
# define ATTRIBUTE(attrs) __attribute__ (attrs)
#else
# define ATTRIBUTE(attrs)
#endif


/* Alignment */

/** A type that has most constrained alignment.  */
typedef union {
  long double d;
  void *p;
  long l;
} aligned_type ATTRIBUTE ((aligned));

/** Inquiring about the alignment of a type.  */
#ifdef __GNUC__
# define ALIGNOF(type) __alignof__ (type)
#else
# define ALIGNOF(type) offsetof (struct { char c; type d; }, d)
#endif

/** Maximal alignment required for any type.  */
#define MAX_ALIGN ALIGNOF (aligned_type)

#endif
