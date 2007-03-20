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

/**
 * Asserts that the constant expression x is not zero at compiletime. name has
 * to be a unique identifier.
 *
 * @note This uses the fact, that double case labels are not allowed.
 */
#define COMPILETIME_ASSERT(x, name) \
    static __attribute__((unused)) void compiletime_assert_##name (int h) { \
        switch(h) { case 0: case (x): ; } \
    }

#ifdef __GNUC__
/**
 * Indicates to the compiler that the value of x is very likely 1
 * @note Only use this in speed critical code and when you are sure x is often 1
 */
#define LIKELY(x)   __builtin_expect((x), 1)

/**
 * Indicates to the compiler that it's very likely that x is 0
 * @note Only use this in speed critical code and when you are sure x is often 0
 */
#define UNLIKELY(x) __builtin_expect((x), 0)
#else
#define LIKELY(x)   x
#define UNLIKELY(x) x
#endif

#endif /* _UTIL_H */
