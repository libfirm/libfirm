/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Random node selector.
 * @author      Matthias Braun
 * @date        29.08.2006
 */
#include <stdlib.h>

#include "besched.h"
#include "belistsched.h"
#include "bemodule.h"
#include "irgwalk.h"

/**
 * The random selector:
 * Just assure that branches are executed last, otherwise select a random node
 */
static ir_node *random_select(ir_nodeset_t *ready_set)
{
	bool only_branches_left = true;

	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (!is_cfop(irn)) {
			only_branches_left = false;
			break;
		}
	}

	ir_node *rand_node;
	if (only_branches_left) {
		/* at last: schedule branches */
		rand_node = ir_nodeset_first(ready_set);
	} else {
		rand_node = NULL;
		do {
			/* take 1 random node */
			int n = rand() % ir_nodeset_size(ready_set);
			int i = 0;
			foreach_ir_nodeset(ready_set, irn, iter) {
				rand_node = irn;
				if (i == n) {
					break;
				}
				++i;
			}
		} while (is_cfop(rand_node));
	}

	return rand_node;
}

static void sched_block(ir_node *block, void *data)
{
	(void)data;
	ir_nodeset_t *cands = be_list_sched_begin_block(block);
	while (ir_nodeset_size(cands) > 0) {
		ir_node *node = random_select(cands);
		be_list_sched_schedule(node);
	}
	be_list_sched_end_block();
}

static void sched_random(ir_graph *irg)
{
	/* TODO: add commandline option for the seed */
	srand(0x4711);
	be_list_sched_begin(irg);
	irg_block_walk_graph(irg, sched_block, NULL, NULL);
	be_list_sched_finish();
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_rand)
void be_init_sched_rand(void)
{
	be_register_scheduler("random", sched_random);
}
