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

/**
 * The random selector:
 * Just assure that branches are executed last, otherwise select a random node
 */
static ir_node *random_select(void *block_env, ir_nodeset_t *ready_set)
{
	(void)block_env;
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

static void *random_init_graph(ir_graph *irg)
{
	(void)irg;
	/* TODO: add commandline option for the seed */
	srand(0x4711);
	return NULL;
}

static void *random_init_block(void *graph_env, ir_node *block)
{
	(void)graph_env;
	(void)block;
	return NULL;
}

static void sched_random(ir_graph *irg)
{
	static const list_sched_selector_t random_selector = {
		random_init_graph,
		random_init_block,
		random_select,
		NULL,                /* node_ready */
		NULL,                /* node_selected */
		NULL,                /* finish_block */
		NULL                 /* finish_graph */
	};
	be_list_sched_graph(irg, &random_selector);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_rand)
void be_init_sched_rand(void)
{
	be_register_scheduler("random", sched_random);
}
