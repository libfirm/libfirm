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
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#include "benode.h"
#include "be_t.h"

#include "obst.h"
#include "list.h"

#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "irdump.h"
#include "array.h"
#include "debug.h"
#include "bemodule.h"
#include "besched.h"
#include "beutil.h"
#include "belive.h"
#include "belistsched.h"
#include "bearch.h"
#include "bestat.h"

#include "lc_opts.h"
#include "lc_opts_enum.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_node     *current_block;
static unsigned    *available;
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
	if (is_Proj(node) || arch_irn_is(node, not_scheduled)) {
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
		ir_nodeset_insert(&ready_set, node);
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
		for (int i = 0, n_deps = get_irn_n_deps(node); i < n_deps; ++i) {
			ir_node *op = get_irn_dep(node, i);
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
	foreach_out_edge_kind(node, edge, EDGE_KIND_DEP) {
		ir_node *user = get_edge_src_irn(edge);
		if (!is_Block(user) && get_nodes_block(user) == current_block
		    && !is_Phi(user))
			try_make_ready(user);
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
}

ir_nodeset_t *be_list_sched_begin_block(ir_node *block)
{
	assert(current_block == NULL);
	current_block = block;

	sched_init_block(block);
	ir_nodeset_init(&ready_set);
	DB((dbg, LEVEL_1, "scheduling %+F\n", block));

	/* fill ready set */
	foreach_out_edge(block, edge) {
		ir_node *node = get_edge_src_irn(edge);
		try_make_ready(node);
	}

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
