/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Primitive list scheduling with different node selectors.
 * @author      Sebastian Hack, Matthias Braun
 * @date        20.10.2004
 */
#include "belistsched.h"

#include "array.h"
#include "be_t.h"
#include "belive.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bestat.h"
#include "debug.h"
#include "irdump.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "lc_opts.h"
#include "lc_opts_enum.h"
#include "list.h"
#include "obst.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_node     *current_block;
static unsigned    *available;
static ir_node     *ready_cfop;
/** Set of ready nodes (nodes where all dependencies are already fulfilled).
 * Does not contain cfops. */
static ir_nodeset_t ready_set;

/**
 * Returns non-zero if the node is already available
 */
static bool is_available(const ir_node *const node)
{
	unsigned idx = get_irn_idx(node);
	return rbitset_is_set(available, idx);
}

static void make_available(ir_node *node);
static void add_to_sched(ir_node *node);

/**
 * Put a node in the ready set, or make it available immediately if it doesn't
 * need to be scheduled
 */
static void node_ready(ir_node *node)
{
	if (arch_is_irn_not_scheduled(node)) {
		/* not_scheduled nodes are already available */
		DB((dbg, LEVEL_3, "\tmaking available: %+F\n", node));
		make_available(node);
	} else if (arch_irn_is(node, schedule_first)) {
		/* schedule schedule_first nodes immediately */
		DB((dbg, LEVEL_3, "\tschedule immediately: %+F\n", node));
		add_to_sched(node);
	} else {
		/* all other nodes go into the ready set */
		DB((dbg, LEVEL_2, "\tmaking ready: %+F\n", node));
		if (is_cfop(node)) {
			/* we must not have multiple cfops in a block */
			assert(ready_cfop == NULL || ready_cfop == node);
			ready_cfop = node;
		} else {
			ir_nodeset_insert(&ready_set, node);
		}
	}
}

/**
 * Try to put a node in the ready set.
 * @param node   The node to make ready.
 */
static void try_make_ready(ir_node *node)
{
	/* ensure that all operands are either in another block or already
	 * scheduled, if not abort */
	if (!is_Phi(node)) {
		foreach_irn_in(node, i, op) {
			if (!is_Block(op) && get_nodes_block(op) == current_block
			    && !is_available(op))
				return;
		}
	}
	node_ready(node);
}

static void make_available(ir_node *node)
{
    /* Insert the node in the set of all available nodes. */
	unsigned idx = get_irn_idx(node);
	rbitset_set(available, idx);

    /* check users, they might be ready now */
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (!is_Block(user) && get_nodes_block(user) == current_block
		    && !is_Phi(user))
			try_make_ready(user);
	}
}

static void maybe_add_cfop(void)
{
	/* if we ran out of other nodes we can finally take the cfop */
	if (ir_nodeset_size(&ready_set) == 0 && ready_cfop != NULL) {
		ir_nodeset_insert(&ready_set, ready_cfop);
		ready_cfop = NULL;
	}
}

/**
 * Append an instruction to a schedule.
 * @param env The block scheduling environment.
 * @param irn The node to add to the schedule.
 * @return    The given node.
 */
static void add_to_sched(ir_node *node)
{
	/* append node to schedule */
	sched_add_before(current_block, node);

	ir_nodeset_remove(&ready_set, node);
	make_available(node);
}

void be_list_sched_schedule(ir_node *node)
{
	DB((dbg, LEVEL_1, "\tpicked %+F\n", node));
	add_to_sched(node);
	maybe_add_cfop();
}

ir_nodeset_t *be_list_sched_begin_block(ir_node *block)
{
	assert(current_block == NULL);
	current_block = block;

	sched_init_block(block);
	ir_nodeset_init(&ready_set);
	ready_cfop = NULL;
	DB((dbg, LEVEL_1, "scheduling %+F\n", block));

	/* fill ready set */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		try_make_ready(node);
	}
	maybe_add_cfop();

	return &ready_set;
}

void be_list_sched_end_block(void)
{
	assert(ir_nodeset_size(&ready_set) == 0);
	ir_nodeset_destroy(&ready_set);
	current_block = NULL;
}

void be_list_sched_begin(ir_graph *irg)
{
	/* make sure we have no danging out-edges. TODO: avoid this in the future */
	edges_deactivate(irg);
	edges_activate(irg);

	unsigned last_idx = get_irg_last_idx(irg);
	available = rbitset_malloc(last_idx);
}

void be_list_sched_finish(void)
{
	free(available);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_listsched)
void be_init_listsched(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.listsched");
}
