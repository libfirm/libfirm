/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Datentyp: Vereinfachte Menge (hash-set) zum Speichern von
 * Zeigern/Adressen.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#include "eset.h"

#include "set.h"


struct eset {
  int dummy; /* dummy entry */
};


static const int INITIAL_SLOTS = 64;


static int pcmp(const void * * p1, const void * * p2, size_t size) {
  return *p1 == *p2 ? 0 : 1;
}


eset * eset_create(void) {
  return (eset *) new_set((set_cmp_fun) pcmp, INITIAL_SLOTS);
}


eset * eset_copy(eset * source) {
  eset * ret = eset_create();
  void * p;
  for (p = eset_first(source); p; p = eset_next(source)) {
    eset_insert(ret, p);
  }
  return ret;
}


void eset_destroy(eset * s) {
  del_set((set *) s);
}


void eset_insert(eset * s, void * p) {
  if (!eset_contains(s, p)) {
    set_insert((set *) s, &p, sizeof(void *), (unsigned) p);
  }
}


bool eset_contains(eset * s, void * p) {
  return set_find((set *) s, &p, sizeof(void *), (unsigned) p) != NULL;
}


void * eset_first(eset * s) {
  void * p = set_first((set *) s);
  return p == NULL ? NULL : *((void * *) p);
}


void * eset_next(eset *s) {
  void * p = set_next((set *) s);
  return p == NULL ? NULL : *((void * *) p);
}


void eset_insert_all(eset * target, eset * source) {
  void * p;
  for (p = eset_first(source); p; p = eset_next(source)) {
    eset_insert(target, p);
  }
}
