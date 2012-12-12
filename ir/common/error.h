/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Error handling for libFirm
 * @author   Michael Beck
 */
#ifndef FIRM_COMMON_ERROR_H
#define FIRM_COMMON_ERROR_H

/**
 * @file
 *
 * Error handling for libFirm.
 *
 * @author Michael Beck
 */

/* define a NORETURN attribute */
#ifndef NORETURN
# if defined(__GNUC__)
#  if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70)
#   define NORETURN void __attribute__ ((noreturn))
#  endif /* __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 70) */
# endif /* defined(__GNUC__) */

# if defined(_MSC_VER)
#  define NORETURN void __declspec(noreturn)
# endif /* defined(_MSC_VER) */

/* If not set above, use "void" for DOES_NOT_RETURN. */
# ifndef NORETURN
# define NORETURN void
# endif /* ifndef NORETURN */
#endif /* ifndef NORETURN */

/**
 * Prints a panic message to stderr and exits.
 */
NORETURN panic(char const *file, int line, char const *func, char const *fmt, ...);

#define panic(...) panic(__FILE__, __LINE__, __func__, __VA_ARGS__)

# endif
