/**
 * Analyse register pressure in loops.
 * @author Christian Wuerdig
 * @date 2006/02/20
 * @cvsid $Id$
 */

#ifndef _BELOOPANA_H_
#define _BELOOPANA_H_

#include "irloop.h"

#include "bearch.h"
#include "beirg.h"

typedef struct _be_loopana_t be_loopana_t;

/**
 * Compute the register pressure for a class of all loops in the birg.
 * @param birg  The backend irg object
 * @param cls   The register class to compute the pressure for
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure_cls(be_irg_t *birg, const arch_register_class_t *cls);

/**
 * Compute the register pressure for all classes of all loops in the birg.
 * @param birg  The backend irg object
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure(be_irg_t *birg);

/**
 * Returns the computed register pressure for the given class and loop.
 * @return The pressure or INT_MAX if not found
 */
unsigned be_get_loop_pressure(be_loopana_t *loop_ana, const arch_register_class_t *cls, ir_loop *loop);

/**
 * Frees loop analysis object.
 */
void be_free_loop_pressure(be_loopana_t *loop_ana);

#endif /* _BELOOPANA_H_ */
