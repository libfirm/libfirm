#ifndef _BESTAT_H_
#define _BESTAT_H_

#include "be_t.h"
#include "benodesets.h"

/**
 * Collects statistics information about register pressure.
 * @param birg The be irg object containing the irg
 */
void be_do_stat_reg_pressure(be_irg_t *birg);

/**
 * Collect statistics about amount of redy nodes per block
 * @param block     The block
 * @param ready_set A set of ready nodes
 */
void be_do_stat_sched_ready(ir_node *block, nodeset *ready_set);

#endif /* _BESTAT_H_ */
