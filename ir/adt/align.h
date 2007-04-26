/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       macros for alignment.
 * @author      Markus Armbruster
 * @version     $Id$
 */
#ifndef FIRM_ADT_ALIGN_H
#define FIRM_ADT_ALIGN_H

#include <stddef.h>

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
