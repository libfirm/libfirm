/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Trivial node selector.
 * @author      Sebastian Hack
 * @date        29.08.2006
 */
#include <stdlib.h>

#include "bearch.h"
#include "belistsched.h"
#include "bemodule.h"
#include "besched.h"
#include "irgwalk.h"
#include "irnode.h"
#include "irnodeset.h"

/**
 * The trivial selector:
 * Just assure that branches are executed last, otherwise select
 * the first node ready.
 */
static ir_node *trivial_select(ir_nodeset_t *ready_set)
{
	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (!is_cfop(irn)) {
			return irn;
		}
	}

	/* at last: schedule branches */
	return ir_nodeset_first(ready_set);
}

static void sched_block(ir_node *block, void *data)
{
	(void)data;
	ir_nodeset_t *cands = be_list_sched_begin_block(block);
	while (ir_nodeset_size(cands) > 0) {
		ir_node *node = trivial_select(cands);
		be_list_sched_schedule(node);
	}
	be_list_sched_end_block();
}

static void sched_trivial(ir_graph *irg)
{
	be_list_sched_begin(irg);
	irg_block_walk_graph(irg, sched_block, NULL, NULL);
	be_list_sched_finish();
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_trivial)
void be_init_sched_trivial(void)
{
	be_register_scheduler("trivial", sched_trivial);
}
