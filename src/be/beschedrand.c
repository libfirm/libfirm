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
#include "belistsched.h"
#include "bemodule.h"
#include "besched.h"
#include "irgwalk.h"
#include <stdlib.h>

/**
 * The random selector:
 * Just assure that branches are executed last, otherwise select a random node
 */
static ir_node *random_select(ir_nodeset_t *ready_set)
{
	ir_node *rand_node = NULL;
	/* take 1 random node */
	int n = rand() % ir_nodeset_size(ready_set);
	int i = 0;
	foreach_ir_nodeset(ready_set, irn, iter) {
		rand_node = irn;
		if (i == n)
			break;
		++i;
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
