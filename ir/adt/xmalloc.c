/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       implementation of xmalloc & friends
 * @author      Markus Armbruster
 * @version     $Id$
 */

/* @@@ ToDo: replace this file with the one from liberty.
   [reimplement xstrdup, ... ] */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "xmalloc.h"
#include "error.h"

static NORETURN xnomem(void) {
  panic("out of memory");
}

void *xmalloc(size_t size) {
  void *res = malloc(size);

  if (!res) xnomem();
  return res;
}

void *xcalloc(size_t num, size_t size) {
  void *res = calloc(num, size);

  if (!res) xnomem();
  return res;
}

void *xrealloc(void *ptr, size_t size) {
  /* ANSI blesses realloc (0, x) but SunOS chokes on it */
  void *res = ptr ? realloc (ptr, size) : malloc (size);

  if (!res) xnomem();
  return res;
}

char *xstrdup(const char *str) {
  size_t len = strlen (str) + 1;
  return memcpy((xmalloc) (len), str, len);
}
