/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Common functions for creating listscheduling algorithms
 * @author      Sebastian Hack
 * @date        20.10.2004
 */
#ifndef FIRM_BE_BELISTSCHED_H
#define FIRM_BE_BELISTSCHED_H

#include "firm_types.h"
#include "irnodeset.h"

#include "be.h"
#include "be_types.h"
#include "bearch.h"

/**
 * A selector interface which is used by the list schedule framework.
 * You can implement your own list scheduler by implementing these
 * functions.
 */
typedef struct list_sched_selector_t {

	/**
	 * Called before a graph is being scheduled.
	 * May be NULL.
	 *
	 * @param irg      The backend graph.
	 * @return         The environment pointer that is passed to all other
	 *                 functions in this struct.
	 */
	void *(*init_graph)(ir_graph *irg);

	/**
	 * Called before scheduling starts on a block.
	 * May be NULL.
	 *
	 * @param graph_env   The environment.
	 * @param block       The block which is to be scheduled.
	 * @return A per-block pointer that is additionally passed to select.
	 */
	void *(*init_block)(void *graph_env, ir_node *block);

	/**
	 * The selection function.
	 * It picks one node out of the ready list to be scheduled next.
	 * The function does not have to delete the node from the ready set.
	 * MUST be implemented.
	 *
	 * @param block_env   Some private information as returned by init_block().
	 * @param sched_head  The schedule so far.
	 * @param ready_set   A set containing all ready nodes. Pick one of these nodes.
	 * @return The chosen node.
	 */
	ir_node *(*select)(void *block_env, ir_nodeset_t *ready_set);

	/**
	 * This function gets executed after a node finally has been made ready.
	 * May be NULL.
	 *
	 * @param block_env The block environment.
	 * @param irn       The node made ready.
	 * @param pred      The previously scheduled node.
	 */
	void (*node_ready)(void *block_env, ir_node *irn, ir_node *pred);

	/**
	 * This function gets executed after a node finally has been selected.
	 * May be NULL.
	 *
	 * @param block_env The block environment.
	 * @param irn       The selected node.
	 */
	void (*node_selected)(void *block_env, ir_node *irn);

	/**
	 * Called after a block has been scheduled.
	 * May be NULL.
	 *
	 * @param env The environment.
	 * @param block_env The per block environment as returned by init_block().
	 */
	void (*finish_block)(void *block_env);

	/**
	 * Called after a whole graph has been scheduled.
	 * May be NULL.
	 *
	 * @param env The environment.
	 */
	void (*finish_graph)(void *env);
} list_sched_selector_t;

/**
 * List schedule a graph.
 * Each block in the graph gets a list head to its link field being the
 * head of the schedule. You can walk this list using the functions in
 * list.h.
 *
 * @param irg     The backend irg.
 */
void be_list_sched_graph(ir_graph *irg, const list_sched_selector_t *selector);

#endif
