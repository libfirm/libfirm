#ifndef IR_ANA_DCA_H
#define IR_ANA_DCA_H

#include "firm_types.h"

/**
 * Compute don't care bits.
 * The result is available via links to tarvals.
 */
void dca_analyze(ir_graph *irg);

#endif
