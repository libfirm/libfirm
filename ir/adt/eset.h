/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Datentyp: Vereinfachte Menge (hash-set) zum Speichern von
 * Zeigern/Adressen.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#ifndef _ESET_H_
#define _ESET_H_


#include <stdbool.h>


/* "eset" ist eine Menge von Adressen. Der Vergleich und das Hashen
 * wird über die Adresse gemacht. "NULL" sollte nicht gespeichert
 * werden. */

typedef struct eset eset;


/* Erzeugt eine neue leere Menge. */
eset * eset_create(void);

/* Erzeugt eine Kopie der übergebenen Menge. Das Kopieren funktioniert
 * nur, wenn in der übergebenen Menge "NULL" nicht enthalten ist. */
eset * eset_copy(eset *);

/* Löscht die Menge. */
void eset_destroy(eset *);

/* Fügt ein Adresse in die Menge ein, wenn es nicht bereits in der
 * Menge enthalten ist. */
void eset_insert(eset *, void *);

/* Prüft ob eine Adresse in der Menge enthalten ist. */
bool eset_contains(eset *, void *);

/* Mit den Funktionen "eset_first" und "eset_next" kann man durch die
 * Menge iterieren. Die Funktion gibt jeweils die Adresse zurück. Wenn
 * keine weiteren Adressen in der Menge sind, geben die Funktionen
 * "NULL" zurück. Warnung: Man sollte deshalb "NULL" nicht in der
 * Menge speichern, weil man sonst nicht durch die Menge iterieren
 * kann. */
void * eset_first(eset *);
void * eset_next(eset *);

/* Fügt alle Elemente der Menge "source" der Menge "target"
 * hinzu. Diese Funktion funktioniert nur, wenn in der Menge "source"
 * die "NULL"-Adresse nicht enthalten ist. */
void eset_insert_all(eset * target, eset * source);


#endif /* _ESET_H_ */
