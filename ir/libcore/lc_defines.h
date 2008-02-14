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
 * Some common defines.
 * @author Sebastian Hack
 * @date 22.12.2004
 */

#ifndef _LIBCORE_DEFINES_H
#define _LIBCORE_DEFINES_H

#define LC_ARRSIZE(x)             (sizeof(x) / sizeof(x[0]))

#define LC_MIN(x,y)			          ((x) < (y) ? (x) : (y))
#define LC_MAX(x,y)			          ((x) > (y) ? (x) : (y))

/** define a readable fourcc code */
#define LC_FOURCC(a,b,c,d)        ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))
#define LC_FOURCC_STR(str)			  LC_FOURCC(str[0], str[1], str[2], str[3])

#define LC_OFFSETOF(type,memb)	  ((char *) &((type *) 0)->memb - (char *) 0)

#ifdef __GNUC__
#define LC_ALIGNOF(type)				  __alignof__(type)
#else
#define LC_ALIGNOF(type)				  LC_OFFSETOF(struct { char c; type d; }, d)
#endif

#define LC_PTR2INT(x) (((char *)(x)) - (char *)0)
#define LC_INT2PTR(x) (((char *)(x)) + (char *)0)

#endif
