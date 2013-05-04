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

#include "irnode.h"
#include "irnodeset.h"

#include "bearch.h"
#include "belistsched.h"
#include "bemodule.h"
#include "besched.h"

/**
 * The trivial selector:
 * Just assure that branches are executed last, otherwise select
 * the first node ready.
 */
static ir_node *trivial_select(void *block_env, ir_nodeset_t *ready_set)
{
	(void)block_env;

	/* assure that branches and constants are executed last */
	foreach_ir_nodeset(ready_set, irn, iter) {
		if (!is_cfop(irn)) {
			return irn;
		}
	}

	/* at last: schedule branches */
	return ir_nodeset_first(ready_set);
}

static void *trivial_init_graph(ir_graph *irg)
{
	(void)irg;
	return NULL;
}

static void *trivial_init_block(void *graph_env, ir_node *block)
{
	(void)graph_env;
	(void)block;
	return NULL;
}

static void sched_trivial(ir_graph *irg)
{
	static const list_sched_selector_t trivial_selector = {
		trivial_init_graph,
		trivial_init_block,
		trivial_select,
		NULL,                /* node_ready */
		NULL,                /* node_selected */
		NULL,                /* finish_block */
		NULL                 /* finish_graph */
	};
	be_list_sched_graph(irg, &trivial_selector);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_sched_trivial)
void be_init_sched_trivial(void)
{
	be_register_scheduler("trivial", sched_trivial);
}
