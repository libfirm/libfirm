/*
 * Project:     libFIRM
 * File name:   ir/common/error.h
 * Purpose:     Error handling for libFirm
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (C) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _ERROR_H_
#define _ERROR_H_

/**
 * @file error.h
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
NORETURN panic(const char *fmt, ...);

# endif /*_ERROR_H_ */
