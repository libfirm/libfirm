/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Auf- und Abbau der interprozeduralen Darstellung (Explizite
 * interprozedurale Abhängigkeiten).
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#ifndef _CONSTRUCT_H_
#define _CONSTRUCT_H_


#include "entity.h"


/* Aufbau der interprozeduralen Darstellung. Das Analyseergebnis muss
 * in den Call-Operationen gespeichert sein. */
void cg_construct(int arr_len, entity ** free_methods_arr);


/* Abbau der interprozeduralen (Sichten-) Darstellung, in eine
 * gewöhnliche intraprozedurale Darstellung */
void cg_destruct(void);


#endif /* _CONSTRUCT_H_ */
