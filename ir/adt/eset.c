/*
 * Project:     libFIRM
 * File name:   ir/adt/eset.c
 * Purpose:     Datentyp: Vereinfachte Menge (hash-set) zum Speichern von
 *              Zeigern/Adressen.
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

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
