/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @date   31.05.2005
 * @author Sebastian Hack
 * @brief  Miscellaneous utility macros.
 */
#ifndef FIRM_ADT_UTIL_H
#define FIRM_ADT_UTIL_H

#include <stddef.h>

/**
 * Make pointer to the struct from a pointer to a member of that struct.
 * @param ptr     The pointer to the member.
 * @param type    The type of the struct.
 * @param member  The name of the member.
 * @return        A pointer to the struct member is in.
 */
#define firm_container_of(ptr, type, member) \
	((type *) ((char *) (ptr) - offsetof(type, member)))

/**
 * Returns size of a static array. Warning: This returns invalid values for
 * dynamically allocated arrays.
 *
 * @param a    static array
 */
#define ARRAY_SIZE(a) (sizeof(a)/sizeof((a)[0]))

#undef MIN
#undef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

/**
 * Three valued compare as demanded by e.g. qsort(3)
 * @param c A number.
 * @param d Another number.
 * @return 0 if c == d, -1 if c < d, 1 if c > d.
 */
#define QSORT_CMP(c, d) (((c) > (d)) - ((c) < (d)))

/**
 * convert an integer into pointer
 */
#define INT_TO_PTR(v)   ((void *)((char *)0 + (v)))

/**
 * convert a pointer into an integer
 */
#define PTR_TO_INT(v)   (((char *)(v) - (char *)0))

#endif
