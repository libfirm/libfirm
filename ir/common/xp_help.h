/*
 * Project:     libFIRM
 * File name:   ir/common/xp_help.h
 * Purpose:     Macros to help writing output handlers.
 * Author:      Markus Armbruster
 * Modified by:
 * Created:     1999 by getting from fiasco
 * CVS-ID:      $Id$
 * Copyright:   (c) 1996 Markus Armbruster
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef _XP_HELP_H
#define _XP_HELP_H

# ifdef HAVE_STRING_H
#  include <string.h>
# endif
#ifdef USE_PRINTF
# define XP_ARG1 stream
# define XP_ARGN args
# define XP_GETARG(type, index) (*(type *)args[index])
# define XPMR(p,n) fwrite ((p), 1, (n), stream)
# define XPR(p) fputs ((p), stream)
# define XPF1R(fmt, arg) fprintf (stream, (fmt), (arg))
# define XPF3R(fmt, a1, a2, a3) fprintf (stream, (fmt), (a1), (a2), (a3))
# define XPCR(p) (putc (*(p), stream) == EOF ? -1 : 1)
#else /* !USE_PRINTF */
# define XP_ARG1 f, a
# define XP_ARGN ap
# define XP_GETARG(type, index) va_arg (*ap, type)
# define XPMR(p,n) f (a, (p), (n))
# define XPR(p) f (a, (p), strlen((p)))
# define XPF1R(fmt, arg) xgprintf (f, a, (fmt), (arg))
# define XPF3R(fmt, a1, a2, a3) xgprintf (f, a, (fmt), (a1), (a2), (a3))
# define XPCR(p) XPMR (p, 1)
#endif /* !USE_PRINTF */

#define XP(p) XP_CHK (XPR ((p)))
#define XPM(p,n) XP_CHK (XPMR ((p), (n)))
#define XPSR(p) XPMR ((p), sizeof(p)-1)
#define XPS(p) XPM ((p), sizeof(p)-1)
#define XPF1(fmt, arg) XP_CHK (XPF1R ((fmt), (arg)))
#define XPC(c) XP_CHK (XPCR ((c)))
#define XP_CHK(expr)				\
  do {						\
    int n = (expr);				\
    if (n < 0) return -1; else printed += n;	\
  } while (0)

#endif
