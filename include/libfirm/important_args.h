#ifndef FIRM_ANA_IMPORTANT_ARGS_H
#define FIRM_ANA_IMPORTANT_ARGS_H

#include "firm_types.h"
#include "bitset.h"
#include "pmap.h"
#include "begin.h"

/**
 * This provides an analysis for finding important parameters of functions.
 *
 * Here, "important" means that if the argument passed to the parameter was
 * known to be constant, profitable optimizations are likely to be possible
 * within the function under discussion.
 *
 * At the time of writing, this is used for finding profitable candidates for
 * procedure cloning.
 */

/**
 * Return a bitset indicating which parameters of given procedure are important
 * (set) and which are not (unset).
 *
 * This analysis only looks at proc and does no interprocedural analysis.
 * The user is responsible for freeing the bitset.
 *
 * @param  proc The irg for which to identify the important parameters
 * @return      A bitset identifying the important parameters of proc
 */
FIRM_API bitset_t *local_important_args(ir_graph *proc);


/**
 * Calculates a bitset with the same meaning as local_important_args for every
 * function in the program.
 *
 * This analysis does interprocedural analysis to also identify parameters that
 * are important because of argument passing to important parameters of
 * procedures that are called from the procedure under discussion.
 * The user is responsible for freeing the pmap and all bitsets contained in it.
 *
 * @return      A map from each irg to a bitset identifying its
 *              important parameters
 */
FIRM_API pmap *important_args_get(void);

#include "end.h"

#endif
