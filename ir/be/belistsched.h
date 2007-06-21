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
 * @brief       Primitive list scheduling with different node selectors.
 * @author      Sebastian Hack
 * @date        20.10.2004
 * @version     $Id$
 */
#ifndef FIRM_BE_BELISTSCHED_H
#define FIRM_BE_BELISTSCHED_H

#include "firm_types.h"
#include "irnodeset.h"

#include "be.h"
#include "bearch.h"
#include "beirg.h"

typedef struct _list_sched_selector_t list_sched_selector_t;

/**
 * A selector interface which is used by the list schedule framework.
 * You can implement your own list scheduler by implementing these
 * functions.
 */
struct _list_sched_selector_t {

	/**
	 * Called before a graph is being scheduled.
	 * @param arch_env The architecture environment.
	 * @param irg      The graph.
	 * @return         The environment pointer that is passed to all other functions in this struct.
	 */
	void *(*init_graph)(const list_sched_selector_t *vtab, const arch_env_t *arch_env, ir_graph *irg);

	/**
	 * Called before scheduling starts on a block.
	 * @param graph_env   The environment.
	 * @param block       The block which is to be scheduled.
	 * @return A per-block pointer that is additionally passed to select.
	 */
	void *(*init_block)(void *graph_env, ir_node *block);

	/**
	 * The selection function.
	 * It picks one node out of the ready list to be scheduled next.
	 * The function does not have to delete the node from the ready set.
	 *
	 * @param block_env   Some private information as returned by init_block().
	 * @param sched_head  The schedule so far.
	 * @param ready_set   A set containing all ready nodes. Pick one of these nodes.
	 * @param live_set    A set containing all nodes currently alive.
	 * @return The chosen node.
	 */
	ir_node *(*select)(void *block_env, ir_nodeset_t *ready_set,
                       ir_nodeset_t *live_set);

	/**
	 * This function decides, if a node should appear in a schedule.
	 * @param block_env The block environment.
	 * @param irn       The node.
	 * @return 1, if the node should be scheduled, 0 if not.
	 */
	int (*to_appear_in_schedule)(void *block_env, const ir_node *irn);

	/**
	 * This function gets executed after a node finally has been made ready.
	 * @param block_env The block environment.
	 * @param irn       The node made ready.
	 * @param pred      The previously scheduled node.
	 */
	void (*node_ready)(void *block_env, ir_node *irn, ir_node *pred);

	/**
	 * This function gets executed after a node finally has been selected.
	 * @param block_env The block environment.
	 * @param irn       The selected node.
	 */
	void (*node_selected)(void *block_env, ir_node *irn);

	/**
	 * Returns the execution time of node irn.
	 */
	unsigned (*exectime)(void *block_env, const ir_node *irn);

	/**
	 * Calculates the latency of executing cycle curr_cycle of node curr in cycle pred_cycle
	 * of node pred.
	 *
	 * @param block_env   The block environment.
	 * @param pred        The previous node.
	 * @param pred_cycle  The previous node execution cycle.
	 * @param curr        The current node.
	 * @param curr_cycle  The current node execution cycle.
	 */
	unsigned (*latency)(void *block_env, const ir_node *pred, int pred_cycle, const ir_node *curr, int curr_cycle);

	/**
	 * Called after a block has been scheduled.
	 * @param env The environment.
	 * @param block_env The per block environment as returned by init_block().
	 */
	void (*finish_block)(void *block_env);

	/**
	 * Called after a whole graph has been scheduled.
	 * @param env The environment.
	 */
	void (*finish_graph)(void *env);

};


/**
 * A trivial selector, that just selects the first ready node.
 */
extern const list_sched_selector_t *trivial_selector;

extern const list_sched_selector_t *random_selector;

/**
 * A selector that tries to minimize the register pressure.
 * @note Not really operational yet.
 */
extern const list_sched_selector_t *reg_pressure_selector;

/**
 * A selector based on trace scheduling as introduced by Muchnik[TM]
 */
extern const list_sched_selector_t *muchnik_selector;

/**
 * A selector based on trace scheduling as introduced by Muchnik[TM]
 * but using the mueller heuristic selector.
 */
extern const list_sched_selector_t *heuristic_selector;

/**
 * List schedule a graph.
 * Each block in the graph gets a list head to its link field being the
 * head of the schedule. You can walk this list using the functions in
 * list.h.
 *
 * @param birg    The backend irg.
 * @param be_opts The backend options
 */
void list_sched(be_irg_t *birg, be_options_t *be_opts);

/**
 * List schedule a block.
 * Same as list_sched but only for a certain block (needed for ILP fallback).
 */
void list_sched_single_block(const be_irg_t *birg, ir_node *block, be_options_t *be_opts);

#endif /* FIRM_BE_BELISTSCHED_H */
