/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef _COMMON_T_H
#define _COMMON_T_H

#include <obstack.h>
#include <stdarg.h>
#include <stdio.h>

#define obstack_chunk_alloc malloc
#define obstack_chunk_free free

#define bcopy(src,dest,n) memcpy(dest,src,n)

#include <libcore/lc_config.h>

#define FUNCNAME     LC_FUNCNAME
#define UNUSED(x)    LC_UNUSED(x)
#define LONGLONG     long /* LC_LONGLONG */
#define LONGDOUBLE 	 double /* LC_LONGDOUBLE */

#ifdef LC_HAVE_C99
#define HAVE_C99     LC_HAVE_C99
#else /* LC_HAVE_C99 */

#ifdef _WIN32
/* Windows names for non-POSIX calls */
#define snprintf  _snprintf
#define vsnprintf _vsnprintf
#endif /* WIN32 */

/* These both are not posix or ansi c but almost everywhere existent */

/* Daniel: Why not just include stdio.h?
extern int snprintf(char *buf, size_t size, const char *fmt, ...);
extern int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);
*/

#endif /* LC_HAVE_C99 */

#endif /* _COMMON_T_H */
