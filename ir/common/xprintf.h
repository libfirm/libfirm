/* Declarations for xprintf & friends.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

/* Parts of this file are adapted from the GNU C Library.
Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the, 1992 Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#ifndef _XPRINTF_H_
#define _XPRINTF_H_

#ifdef USE_PRINTF

/* This code is just an incomplete sketch how GNU libc could be used,
   if it provided the necessary functionality.  Problems:
   o  obstack_printf() is not yet available, it will be in version 2.
   o  User defined conversion specifiers cannot take struct arguments.
   Using GNU libc should be *significantly* faster.  */

# include <printf.h>

# define XP_PAR1 FILE *stream
# define XP_PARN const void **args

typedef struct printf_info xprintf_info;

/* @@@ GNU libc version 2's register_printf_function *requires*
   non-NULL 3rd argument */
# define xprintf_register(spec, func) \
    register_printf_function ((spec), (func), NULL)

# define xprintf printf
# define xvprintf vprintf
# define xfprintf fprintf
# define xvfprintf vfprintf
# define xoprintf obstack_printf
# define xvoprintf obstack_vprintf

#else /* !USE_PRINTF */

/* Emulate GNU libc functionality on top of standard libc */

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>


# define XP_PAR1 xgprintf_func *f, void *a
# define XP_PARN va_list *ap

/* Type of a generic print function */
typedef int xgprintf_func (void *, const char *, size_t);

typedef struct
{
  int prec;			/* Precision.  */
  int width;			/* Width.  */
  unsigned char spec;		/* Format letter.  */
  unsigned int is_long_double:1;/* L flag.  */
  unsigned int is_short:1;	/* h flag.  */
  unsigned int is_long:1;	/* l flag.  */
  unsigned int alt:1;		/* # flag.  */
  unsigned int space:1;		/* Space flag.  */
  unsigned int left:1;		/* - flag.  */
  unsigned int showsign:1;	/* + flag.  */
  char pad;			/* Padding character.  */
} xprintf_info;

/* Type of a printf specifier-handler function.
   `printer' is the generic print function to be called with first
   argument `out'.  `info' gives information about the format
   specification.  Arguments can be read from `args'.  The function
   shall return the number of characters written, or -1 for errors.  */
typedef int xprintf_function (xgprintf_func *printer, void *out,
			      const xprintf_info *info,
			      va_list *args);

void xprintf_register (char spec, xprintf_function *);

int xgprintf(xgprintf_func *, void *, const char *, ...);
int xvgprintf(xgprintf_func *, void *, const char *, va_list);

int xprintf (const char *, ...);
int xvprintf (const char *, va_list);
int xfprintf (FILE *, const char *, ...);
int xvfprintf (FILE *, const char *, va_list);

struct obstack;
int xoprintf (struct obstack *, const char *, ...);
int xvoprintf (struct obstack *, const char *, va_list);

#endif /* !USE_PRINTF */
#endif /* _XPRINTF_H_ */
