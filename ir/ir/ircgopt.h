/* -------------------------------------------------------------------
 * $Id$
 * -------------------------------------------------------------------
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Abschätzung der
 * Aufrufrelation bestimmt.
 *
 * Erstellt: Hubert Schmid, 09.06.2002
 * ---------------------------------------------------------------- */


#ifndef _GC_IRGS_H_
#define _GC_IRGS_H_


#include "entity.h"


/* Entfernt alle Methoden, die von den Methoden aus "keep_arr"
 * (bezgl. der Abschätzung get_Call_callee) nicht erreichbar sind. Die
 * Abschätzung der Aufrufrelation muss entsprechend an den
 * Call-Operationen gespeichert sein. Die "entity->link"s werden dabei
 * überschrieben. */
void gc_irgs(int n_keep, entity ** keep_arr);


#endif /* _GC_IRGS_H_ */
