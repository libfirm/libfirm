/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date   04.06.2007
 * @author Matthias Braun, Sebastian Hack
 * @brief  Macros to instruct the compiler compiling libFirm.
 */

#ifndef FIRM_COMPILER_H
#define FIRM_COMPILER_H

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

/**
 * Tell the compiler, that a function is pure, i.e. it only
 * uses its parameters and never modifies the "state".
 * Add this macro after the return type.
 */
#define PURE        __attribute__((const))

/**
 * Tell the compiler, that a function is unused, no warning needed.
 */
#define UNUSED      __attribute__((unused))

/**
 * Use an enum type as the base type for a bitfield. This is more useful
 * than using the generic "unsigned" because the compiler can warn if the
 * range of enum elements exceeds the bitfield size now.
 */
#define ENUMBF(type)  __extension__ type

#else
#define LIKELY(x)   x
#define UNLIKELY(x) x
#define PURE
#define UNUSED
#define ENUMBF(type)  unsigned
#endif

/**
 * Asserts that the constant expression x is not zero at compiletime. name has
 * to be a unique identifier.
 *
 * @note This uses the fact, that double case labels are not allowed.
 */
#define COMPILETIME_ASSERT(x, name) \
    static UNUSED void compiletime_assert_##name (int h) { \
        switch (h) { case 0: case (x): {} } \
    }

#endif
