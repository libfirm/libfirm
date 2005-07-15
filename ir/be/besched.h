
#ifndef _BESCHED_H
#define _BESCHED_H

#include <stdio.h>
#include "irgraph.h"
#include "irnode.h"

void be_sched_dump(FILE *f, const ir_graph *irg);

int (sched_get_time_step)(const ir_node *irn);
int (sched_has_next)(const ir_node *irn);
int (sched_has_prev)(const ir_node *irn);
ir_node *(sched_next)(const ir_node *irn);
ir_node *(sched_prev)(const ir_node *irn);
ir_node *(sched_first)(const ir_node *block);
ir_node *(sched_last)(const ir_node *block);
ir_node *(sched_add_before)(ir_node *before, ir_node *irn);
ir_node *(sched_add_after)(ir_node *before, ir_node *irn);

#define sched_is_end(irn) is_Block(irn)
#define sched_is_begin(irn) is_Block(irn)

#define sched_foreach_from(from, irn) \
  for(irn = from; !sched_is_end(irn); irn = sched_next(irn))

#define sched_foreach_reverse_from(from, irn) \
  for(irn = from; !sched_is_begin(irn); irn = sched_prev(irn))

/**
 * A shorthand macro for iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach(block,irn) \
	sched_foreach_from(sched_first(block), irn)

/**
 * A shorthand macro for reversely iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach_reverse(block,irn) \
  sched_foreach_reverse_from(sched_last(block), irn)

#endif
