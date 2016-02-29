/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       data structures for scheduling nodes in basic blocks.
 * @author      Sebastian Hack, Matthias Braun
 */
#ifndef FIRM_BE_BESCHED_H
#define FIRM_BE_BESCHED_H

#include <stdbool.h>

#include "beinfo.h"
#include "irdom.h"

static sched_info_t *get_irn_sched_info(const ir_node *node)
{
	return &be_get_info(skip_Proj_const(node))->sched_info;
}

/**
 * Check, if the node is scheduled.
 * Block nodes are reported as scheduled as they mark the begin and end
 * of the scheduling list.
 * @param irn The node.
 * @return 1, if the node is scheduled, 0 if not.
 */
static inline bool sched_is_scheduled(const ir_node *irn)
{
	return get_irn_sched_info(irn)->next != NULL;
}

/**
 * Returns the time step of a node. Each node in a block has a timestep
 * unique to that block. A node schedule before another node has a lower
 * timestep than this node.
 * @param irn The node.
 * @return The time step in the schedule.
 */
static inline sched_timestep_t sched_get_time_step(const ir_node *irn)
{
	assert(sched_is_scheduled(irn));
	return get_irn_sched_info(irn)->time_step;
}

static inline bool sched_is_end(const ir_node *node)
{
	return is_Block(node);
}

static inline bool sched_is_begin(const ir_node *node)
{
	return is_Block(node);
}

/**
 * Get the scheduling successor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or the block, if the node has no next node.
 */
static inline ir_node *sched_next(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return info->next;
}

/**
 * Get the scheduling predecessor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or the block, if the node has no predecessor.
 * predecessor.
 */
static inline ir_node *sched_prev(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return info->prev;
}

/**
 * Get the first node in a block schedule.
 * @param block The block of which to get the schedule.
 * @return The first node in the schedule or the block itself
 *         if there is no node in the schedule.
 */
static inline ir_node *sched_first(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return sched_next(block);
}

/**
 * Get the last node in a schedule.
 * @param  block The block to get the schedule for.
 * @return The last ir node in a schedule, or the block itself
 *         if there is no node in the schedule.
 */
static inline ir_node *sched_last(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return sched_prev(block);
}

/**
 * Add a node to a block schedule.
 * @param irn The node to add.
 * @return The given node.
 */
void sched_add_before(ir_node *before, ir_node *irn);


/**
 * Add a node to a block schedule.
 * @param irn The node to add.
 * @return The given node.
 */
void sched_add_after(ir_node *after, ir_node *irn);

static inline void sched_init_block(ir_node *block)
{
	sched_info_t *info = get_irn_sched_info(block);
	assert(info->next == NULL && info->time_step == 0);
	info->next = block;
	info->prev = block;
}

/**
 * Remove a node from the scheduled.
 * @param irn The node.
 */
void sched_remove(ir_node *irn);

/**
 * Remove @p old from the schedule and put @p irn in its place.
 */
void sched_replace(ir_node *old, ir_node *irn);

/**
 * Checks, if node @p a comes before node @p b.
 * @param a   A node.  May be a block, which represents the start of the
 *            schedule.
 * @param b   Another node. Must not be a block.
 * @return    true, if a is in front of b in the schedule, false else.
 * @note      Both nodes must be in the same block.
 */
static inline bool sched_comes_before(const ir_node *a, const ir_node *b)
{
	assert(get_block_const(a) == get_nodes_block(b));
	sched_timestep_t const as = sched_get_time_step(a);
	sched_timestep_t const bs = sched_get_time_step(b);
	return as < bs;
}

/**
 * Check, if one value dominates the other.
 * The dominance is not strict here.
 * @param a The first node.
 * @param b The second node.
 * @return true if a dominates b or if a == b.
 */
static inline bool value_strictly_dominates(const ir_node *a,
                                            const ir_node *b)
{
	/* if a and b are not in the same block, dominance is determined by the
	 * dominance of the blocks. */
	const ir_node *block_a = get_block_const(a);
	const ir_node *block_b = get_block_const(b);
	if (block_a != block_b)
		return block_dominates(block_a, block_b);

	/* Dominance is determined by schedule. */
	return sched_comes_before(a, b);
}

#define sched_foreach_after(after, irn) \
	for (ir_node *irn = (after); !sched_is_end(irn = sched_next(irn));)

#define sched_foreach_reverse_before(before, irn) \
	for (ir_node *irn = (before); !sched_is_begin(irn = sched_prev(irn));)

#define sched_foreach_non_phi_reverse_before(before, irn) \
	for (ir_node *irn = (before); !sched_is_begin(irn = sched_prev(irn));) \
		if (is_Phi(irn)) break; else

/**
 * A shorthand macro for iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach(block,irn) \
	sched_foreach_after((assert(is_Block(block)), block), irn)

#define sched_foreach_phi(block, irn) \
	sched_foreach(block, irn) \
		if (!is_Phi(irn)) break; else

#define sched_foreach_non_phi(block, irn) \
	sched_foreach(block, irn) \
		if (is_Phi(irn)) continue; else

/**
 * A shorthand macro for reversely iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach_reverse(block,irn) \
	sched_foreach_reverse_before((assert(is_Block(block)), block), irn)

#define sched_foreach_non_phi_reverse(block, irn) \
	sched_foreach_non_phi_reverse_before((assert(is_Block(block)), block), irn)

/**
 * A shorthand macro for iterating over a schedule while the current node may be
 * removed or replaced.
 *
 * @param block  The block.
 * @param irn    A ir node pointer used as an iterator.
 */
#define sched_foreach_safe(block, irn) \
	for (ir_node *irn, *irn##__next = sched_first(block); !sched_is_end(irn = irn##__next) ? irn##__next = sched_next(irn), 1 : 0;)

/**
 * A shorthand macro for reversely iterating over a schedule while the current
 * node may be removed or replaced.
 *
 * @param block  The block.
 * @param irn    A ir node pointer used as an iterator.
 */
#define sched_foreach_reverse_safe(block, irn) \
	for (ir_node *irn, *irn##__prev = sched_last(block); !sched_is_begin(irn = irn##__prev) ? irn##__prev = sched_prev(irn), 1 : 0;)

/**
 * Type for a function scheduling a graph
 */
typedef void (*schedule_func) (ir_graph *irg);

/**
 * Register new scheduling algorithm
 */
void be_register_scheduler(const char *name, schedule_func func);

/**
 * schedule a graph with the currently selected scheduler.
 */
void be_schedule_graph(ir_graph *irg);

/**
 * Return the last schedule_first node following node, if there is any, node
 * otherwise.
 */
ir_node *be_move_after_schedule_first(ir_node *node);

#endif
