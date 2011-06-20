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


/**
 * Central definitions for the libcore.
 * @author Sebastian Hack
 * @date 22.12.2004
 */
#ifndef _LC_CONFIG_H
#define _LC_CONFIG_H

#if defined(__GNUC__)

#define inline         __inline__
#define LC_FUNCNAME    __FUNCTION__
#define LC_PRINTF(m) __attribute__((format(printf,m,(m)+1)))

#ifdef __STRICT_ANSI__
#define LC_LONGLONG    long
#define LC_LONGDOUBLE  double
#else
#define LC_LONGLONG    long long
#define LC_LONGDOUBLE  long double
#endif

#elif defined(_MSC_VER)

#define LC_FUNCNAME    "<unknown>"
#define LC_PRINTF(m)

#define LC_LONGLONG    __int64
#define LC_LONGDOUBLE  long double

/* disable warning: 'foo' was declared deprecated, use 'bla' instead */
/* of course MS had to make 'bla' incompatible to 'foo', so a simple */
/* define will not work :-((( */
#pragma warning( disable : 4996 )

#ifdef _WIN32
#define snprintf _snprintf
#endif /* _WIN32 */

#if _MSC_VER <= 1200
typedef __int16            int16;
typedef __int32            int32;
typedef __int64            int64;
typedef unsigned __int16   uint16;
typedef unsigned __int32   uint32;
typedef unsigned __int64   uint64;
#endif /* _MSC_VER <= 1200 */

/* default definitions */
#else /* !defined(__GNUC__) && !defined(_MSC_VER) */

#define inline
#define LC_FUNCNAME "<unknown>"
#define LC_LONGLONG long
#define LC_LONGDOUBLE double
#define LC_PRINTF(m)

#endif

#endif /* _LC_CONFIG_H */
