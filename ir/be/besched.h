/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Scheduling utilities for nodes in Blocks and Blocks.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BESCHED_H
#define FIRM_BE_BESCHED_H

#include <stdio.h>

#include "irgraph.h"
#include "irnode.h"
#include "beirg.h"

void be_sched_dump(FILE *f, ir_graph *irg);

int     sched_get_time_step(const ir_node *irn);
int     sched_has_next(const ir_node *irn);
int     sched_has_prev(const ir_node *irn);
int     sched_is_scheduled(const ir_node *irn);
ir_node *sched_next(const ir_node *irn);
ir_node *sched_prev(const ir_node *irn);
ir_node *sched_first(const ir_node *block);
ir_node *sched_last(const ir_node *block);
void    sched_add_before(ir_node *before, ir_node *irn);
void    sched_add_after(ir_node *before, ir_node *irn);
void    sched_init_block(ir_node *block);
void    sched_reset(ir_node *node);
void    sched_remove(ir_node *irn);

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

/**
 * Removes dead nodes from schedule
 * @param irg  the graph
 */
void be_remove_dead_nodes_from_schedule(be_irg_t *birg);

#endif /* FIRM_BE_BESCHED_H */
