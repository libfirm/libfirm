/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Intraprozedurale Analyse zur Abschätzung der Aufrulrelation. Es
 * wird eine Menge von freien Methoden und anschließend die an den
 * Call-Operationen aufrufbaren Methoden bestimmt.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#ifndef _CGANA_H_
#define _CGANA_H_


#include "entity.h"


/* Methoden sind "frei", wenn ihr Funktionszeiger (potentiell)
 *"explizit" bekannt ist, d.h.:
 *
 * - die Methode ist von außen sichtbar (external_visible).
 *
 * - ihr Funktionszeiger ist "frei", d.h. der Funktionszeiger wurde
 *   nicht ausschließlich an den entsprechenden Eingang eines
 *   Call-Knotens weitergegeben, sondern z.B. in den Speicher
 *   geschrieben, als Parameter übergeben, ...
 *
 * Die main-Methode ist immer in der Menge enthalten.
 *
 * Die Links an den "ir_node"s werden gelöscht. */


/* Bestimmt für jede Call-Operation die Menge der aufrufbaren Methode
 * und speichert das Ergebnis in der Call-Operation. (siehe
 * "set_Call_callee"). Die Methode gibt die Menge der
 * "freien" Methoden zurück, die vom Aufrufer wieder freigegeben
 * werden muss (free).
 *
 * Performs some optimizations possible by the analysed information:
 *   - Replace SymConst nodes by Const nodes if possible,
 *   - Replace (Sel-method(Alloc)) by Const method,
 *   - Replaces unreachable Sel nodes by Bad  (@@@ was genau meint unreachable?)
 *   - Replaces Sel-method by Const if the Method is never overwritten */
void cgana(int *len, entity ***free_methods);

/* Performs only the optimizations done by cgana. */
/* @@@ move to irgopt ?! */
void opt_call_addrs(void);
#endif /* _CGANA_H_ */
