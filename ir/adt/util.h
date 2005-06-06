/**
 * @file   util.h
 * @date   31.05.2005
 * @author Sebastian Hack
 *
 * Some utility macros.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _UTIL_H
#define _UTIL_H

/**
 * Get the offset of a member of a struct.
 * @param type   The type of the struct member is in.
 * @param member The name of the member.
 * @return       The offset of member in type in bytes.
 */
#define offset_of(type, member) \
  ((char *) &(((type *) 0)->member) - (char *) 0)

/**
 * Make pointer to the struct from a pointer to a member of that struct.
 * @param ptr     The pointer to the member.
 * @param type    The type of the struct.
 * @param member  The name of the member.
 * @return        A pointer to the struct member is in.
 */
#define container_of(ptr, type, member) \
	((type *) ((char *) (ptr) - offset_of(type, member)))

/**
 * Get the number of elements of a static array.
 * @param arr The static array.
 * @return The number of elements in that array.
 */
#define array_size(arr) \
  (sizeof(arr) / sizeof((arr)[0]))

#endif /* _UTIL_H */
