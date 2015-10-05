/*
 * This file is part of libFirm.
 * Copyright (C) 2014 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief   Macros for declaring extra function attributes
 * @author  Matthias Braun
 */
#ifndef FIRM_FUNCATTR_H
#define FIRM_FUNCATTR_H

/**
 * @def FIRM_NOTHROW
 * Tells that a function does not throw C++ exceptions. Currently this is only
 * necessary for obstack_printf to avoid nameclashes when linking with glibc
 * which has an obstack library with NOTHROW builtin.
 */
#ifdef __cplusplus
# define FIRM_NOTHROW throw ()
#else
# define FIRM_NOTHROW
#endif

/**
 * @def FIRM_PRINTF
 * Attribute to mark a function to have a printf style format string and
 * variadic arguments.
 */
#ifdef __GNUC__
# define FIRM_PRINTF(a,b) __attribute__((__format__(__printf__, a, b)))
#else
# define FIRM_PRINTF(a,b)
#endif

/**
 * @def FIRM_NORETURN
 * Attribute to mark a function which never returns.
 */
#if defined(__GNUC__) && __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70)
# define FIRM_NORETURN __attribute__((__noreturn__)) void
#elif defined(__STDC__) && (__STDC_VERSION__ >= 201112L)
# define FIRM_NORETURN _Noreturn void
#elif defined(_MSC_VER)
# define FIRM_NORETURN void __declspec(noreturn)
#else
# define FIRM_NORETURN void
#endif

/**
 * @def FIRM_NORETURN_FUNCPTR
 * Attribute to mark a function pointers target as noreturn.
 *
 * Unfortunately some platforms only allow noreturn on functions but not on
 * function pointer targets making it necessary to have FIRM_NORETURN and
 * FIRM_NORETURN_FUNCPTR.
 */
#if defined(__GNUC__) && __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70)
# define FIRM_NORETURN_FUNCPTR __attribute__((__noreturn__)) void
#else
# define FIRM_NORETURN_FUNCPTR void
#endif

#endif
