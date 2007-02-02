/*
 * Project:     libFIRM
 * File name:   ir/common/error.c
 * Purpose:     Error handling for libFirm
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (C) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>

#include <stdio.h>
#include <stdarg.h>
#include "error.h"

NORETURN panic(const char *fmt, ...) {
	va_list ap;

	fputs("libFirm panic: ", stderr);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	putc('\n', stderr);
	abort();
}
