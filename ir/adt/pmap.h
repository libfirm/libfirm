/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Datentyp: Vereinfachte Map (hash-map) zum Speichern von
 * Zeigern/Adressen -> Zeigern/Adressen.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#ifndef _PMAP_H_
#define _PMAP_H_


#include "bool.h"


/* Map die Adressen auf Adressen abbildet. Der Vergleich und das
 * Hashen findet über die Adresse statt. */

typedef struct pmap pmap;

typedef struct pmap_entry {
  void * key;
  void * value;
} pmap_entry;


/* Erzeugt eine neue leere Map. */
pmap * pmap_create(void);

/* Löscht eine Map. */
void pmap_destroy(pmap *);

/* Fügt ein Paar (key,value) in die Map ein. Gibt es bereits einen
 * Eintrag mit "key" in er Map, so wird der entsprechende "value"
 * überschrieben. */
void pmap_insert(pmap *, void * key, void * value);

/* Prüft ob ein Eintrag zu "key" exisitiert. */
bool pmap_contains(pmap *, void * key);

/* Gibt den Eintrag zu "key" zurück. */
pmap_entry * pmap_find(pmap *, void * key);

/* Gibt für den Eintrag zu "key" den "value" zurück. */
void * pmap_get(pmap *, void * key);

/* Mit den Funktionen "pmap_first" und "pmap_next" kann man durch die
 * Map iterieren. Die Funktionen geben einen Zeiger auf einen Eintrag
 * zurück (key,value). Die Funktionen geben "NULL" zurück, wenn kein
 * weiterer Eintrag existiert. */
pmap_entry * pmap_first(pmap *);
pmap_entry * pmap_next(pmap *);


#endif /* _PMAP_H_ */
