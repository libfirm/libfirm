/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       A pointer hash-set (WARNING: deprecated!)
 * @author      Hubert Schmid
 * @date        09.06.2002
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "eset.h"
#include "set.h"
#include "hashptr.h"

struct eset {
  int dummy; /* dummy entry */
};


#define INITIAL_SLOTS 64

static int pcmp(const void *p1, const void *p2, size_t size) {
  const void **q1 = (const void **)p1;
  const void **q2 = (const void **)p2;
  (void) size;

  return *q1 != *q2;
}


eset * eset_create(void) {
  return (eset *) new_set(pcmp, INITIAL_SLOTS);
}


eset * eset_copy(eset *source) {
  eset * ret = eset_create();
  void * p;
  for (p = eset_first(source); p; p = eset_next(source)) {
    eset_insert(ret, p);
  }
  return ret;
}


void eset_destroy(eset *s) {
  del_set((set *)s);
}

/* Returns the number of elements in the set. */
int eset_count(eset *s) {
  return set_count((set *)s);
}

void eset_insert(eset *s, void *p) {
  if (!eset_contains(s, p)) {
    set_insert((set *)s, &p, sizeof(p), HASH_PTR(p));
  }
}


int eset_contains(eset *s, void *p) {
  return set_find((set *)s, &p, sizeof(p), HASH_PTR(p)) != NULL;
}


void * eset_first(eset *s) {
  void * p = set_first((set *) s);
  return p == NULL ? NULL : *((void **)p);
}


void * eset_next(eset *s) {
  void *p = set_next((set *) s);
  return p == NULL ? NULL : *((void **)p);
}


void eset_insert_all(eset *target, eset *source) {
  void *p;
  for (p = eset_first(source); p; p = eset_next(source)) {
    eset_insert(target, p);
  }
}
