
#ifndef _BESCHED_H
#define _BESCHED_H

#include <stdio.h>

void be_sched_dump(FILE *f, const ir_graph *irg);

int (sched_get_time_step)(const ir_node *irn);
int (sched_has_succ)(const ir_node *irn);
int (sched_has_prev)(const ir_node *irn);
const ir_node *(sched_succ)(const ir_node *irn);
const ir_node *(sched_prev)(const ir_node *irn);
const ir_node *(sched_first)(const ir_node *block);
const ir_node *(sched_last)(const ir_node *block);
const ir_node *(sched_add)(ir_node *block, const ir_node *irn);

/**
 * A shorthand macro for iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach(block,irn) \
	for(irn = sched_first(block); irn; irn = sched_succ(irn))

/**
 * A shorthand macro for reversely iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach_reverse(block,irn) \
	for(irn = sched_last(block); irn; irn = sched_prev(irn))

#endif
