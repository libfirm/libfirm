/* IEEE754 fp format.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

/* This file was derived from the GNU C Library's ieee754.h which
   carried the following copyright notice:

Copyright (C) 1992 Free Software Foundation, Inc.

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
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/* @@@ This is completely non-portable!  ISO/IEC DIS 9899, section
   3.5.2.1: An implementation may allocate any addressable storage
   unit large enough to hold a bit-field.  If enough space remains, a
   bit-field that immediately follows another bit-field in a structure
   shall be packed into adjacent bits of the same unit.  If
   insufficient space remains, whether a bit-field that does not fit
   is put into the next unit or overlaps adjacent units is
   implementation-defined.  The order of allocation of bit-fields
   within a unit (high-order to low-order or low-order to high-order)
   is implementation-defined.  */

/* Floating point definitions in ieee standard number 754
   only used in target values (/libfirm/ir/tv/tv.c). */
#ifndef _IEEE754_H
#define _IEEE754_H

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


union ieee754_double
  {
    double d;

    /* This is the IEEE 754 double-precision format.  */
    struct
      {
#ifdef WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int mantissa0:20;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:20;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
      } ieee;
    struct
      {
#ifdef WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:11;
	unsigned int quiet_nan:1;
	unsigned int mantissa0:19;
	unsigned int mantissa1:32;
#else
	unsigned int mantissa1:32;
	unsigned int mantissa0:19;
	unsigned int quiet_nan:1;
	unsigned int exponent:11;
	unsigned int negative:1;
#endif
      } ieee_nan;
  };

/* bias added to exponent of ieee754_double */
#define _IEEE754_DOUBLE_BIAS 0x3ff


union ieee754_float
  {
    float f;

    /* This is the ieee754 single-precision format.  */
    struct
      {
#ifdef WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
#else
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif
      } ieee;
    /* This is for extracting information about NaNs.  */
    struct
      {
#ifdef WORDS_BIGENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int quiet_nan:1;
	unsigned int mantissa:22;
#else
	unsigned int mantissa:22;
	unsigned int quiet_nan:1;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif
      } ieee_nan;
  };

/* bias added to exponent of ieee_float */
#define _IEEE754_FLOAT_BIAS 0x7f

#endif	/* _IEEE754_H */
