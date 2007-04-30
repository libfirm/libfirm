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
 * @brief       Scheduling utilities for nodes in Blocks and Blocks.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BESCHED_T_H
#define FIRM_BE_BESCHED_T_H

#define SCHED_INITIAL_GRANULARITY (1 << 14)

#include "list.h"
#include "irnode_t.h"
#include "irgraph_t.h"

#include "beutil.h"
#include "besched.h"

typedef unsigned int sched_timestep_t;

extern size_t sched_irn_data_offset;

/**
 * The schedule structure which is present at each ir node.
 *
 * Currently, only basic blocks are scheduled. The list head of
 * every block schedule list is the Block list.
 */
typedef struct _sched_info_t {
	struct list_head list;         /**< The list head to list the nodes in a schedule. */
	sched_timestep_t time_step;    /**< If a is after b in a schedule, its time step is
                                        larger than b's. */
	unsigned scheduled : 1;        /**< 1, if the node is in the schedule of the block, 0 else. */
} sched_info_t;

#define _sched_entry(list_head) (list_entry(list_head, sched_info_t, list))

#define get_irn_sched_info(irn) get_irn_data(irn, sched_info_t, sched_irn_data_offset)
#define get_sched_info_irn(sched_info) get_irn_data_base(sched_info, sched_irn_data_offset)

/**
 * Check, if the node is scheduled.
 * @param irn The node.
 * @return 1, if the node is scheduled, 0 if not.
 */
static INLINE int _sched_is_scheduled(const ir_node *irn)
{
  return get_irn_sched_info(irn)->scheduled;
}

/**
 * Get the time step of an irn in a schedule.
 * @param irn The node.
 * @return The time step in the schedule.
 */
static INLINE int _sched_get_time_step(const ir_node *irn)
{
	assert(_sched_is_scheduled(irn));
	return get_irn_sched_info(irn)->time_step;
}

/**
 * Checks, if a node is to appear in a schedule. Such nodes either
 * consume real data (mode datab) or produce such.
 * @param irn The node to check for.
 * @return 1, if the node consumes/produces data, false if not.
 */
static INLINE int to_appear_in_schedule(const ir_node *irn)
{
	switch(get_irn_opcode(irn)) {
		case iro_Start:
		case iro_Jmp:
		case iro_Break:
			return 1;
		default:
			return is_data_node(irn);
	}
}

/**
 * Check, if an ir_node has a scheduling successor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling successor, 0 if not.
 */
static INLINE int _sched_has_next(const ir_node *irn)
{
	const ir_node *block = is_Block(irn) ? irn : get_nodes_block(irn);
	const sched_info_t *info = get_irn_sched_info(irn);
	const sched_info_t *block_info = get_irn_sched_info(block);
	return info->list.next != &block_info->list;
}

/**
 * Check, if an ir_node has a scheduling predecessor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling predecessor, 0 if not.
 */
static INLINE int _sched_has_prev(const ir_node *irn)
{
   	const ir_node *block = is_Block(irn) ? irn : get_nodes_block(irn);
	const sched_info_t *info = get_irn_sched_info(irn);
	const sched_info_t *block_info = get_irn_sched_info(block);
	return info->list.prev != &block_info->list;
}

/**
 * Get the scheduling successor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or the block, if the node has no next node.
 */
static INLINE ir_node *_sched_next(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return get_sched_info_irn(_sched_entry(info->list.next));
}

/**
 * Get the scheduling predecessor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or the block, if the node has no predecessor.
 * predecessor.
 */
static INLINE ir_node *_sched_prev(const ir_node *irn)
{
	const sched_info_t *info = get_irn_sched_info(irn);
	return get_sched_info_irn(_sched_entry(info->list.prev));
}

/**
 * Get the first node in a block schedule.
 * @param block The block of which to get the schedule.
 * @return The first node in the schedule or the block itself
 *         if there is no node in the schedule.
 */
static INLINE ir_node *_sched_first(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return _sched_next(block);
}

/**
 * Get the last node in a schedule.
 * @param  block The block to get the schedule for.
 * @return The last ir node in a schedule, or the block itself
 *         if there is no node in the schedule.
 */
static INLINE ir_node *_sched_last(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return _sched_prev(block);
}

/**
 * Reassign the time steps in the schedule.
 * @param block The schedule to update.
 */
void sched_renumber(const ir_node *block);

static INLINE void _sched_set_time_stamp(ir_node *irn)
{
	sched_info_t *inf = get_irn_sched_info(irn);
	sched_timestep_t before_ts = _sched_entry(inf->list.prev)->time_step;
	sched_timestep_t after_ts = _sched_entry(inf->list.next)->time_step;

	/*
	 * If we are the last, we can give us a big time step,
	 * else we have to compute our time step from our
	 * neighbours.
	 */
	if(before_ts >= after_ts)
		inf->time_step = before_ts + SCHED_INITIAL_GRANULARITY;
	else {
		sched_timestep_t ts = (before_ts + after_ts) / 2;

		/*
		 * If the resolution went out, we have to renumber
		 * this block.
		 */
		if(ts == before_ts || ts == after_ts)
			sched_renumber(get_nodes_block(irn));
		else
			inf->time_step = ts;
	}
}

/**
 * Add a node to a block schedule.
 * @param block The block to whose schedule the node shall be added to.
 * @param irn The node to add.
 * @return The given node.
 */
static INLINE ir_node *_sched_add_before(ir_node *before, ir_node *irn)
{
	sched_info_t *info = get_irn_sched_info(irn);
	assert(_sched_is_scheduled(before) && !_sched_is_scheduled(irn));
	list_add_tail(&info->list, &get_irn_sched_info(before)->list);
	_sched_set_time_stamp(irn);
	info->scheduled = 1;
	return irn;
}

/**
 * Add a node to a block schedule.
 * @param block The block to whose schedule the node shall be added to.
 * @param irn The node to add.
 * @return The given node.
 */
static INLINE ir_node *_sched_add_after(ir_node *after, ir_node *irn)
{
	sched_info_t *info = get_irn_sched_info(irn);
	assert(_sched_is_scheduled(after) && !_sched_is_scheduled(irn));
	list_add(&info->list, &get_irn_sched_info(after)->list);
	_sched_set_time_stamp(irn);
	info->scheduled = 1;
	return irn;
}

static INLINE void _sched_init_block(ir_node *block)
{
	sched_info_t *info = get_irn_sched_info(block);
	assert(info->scheduled == 0 && info->time_step == 0);
	INIT_LIST_HEAD(&info->list);
	info->scheduled = 1;
}

static INLINE void _sched_reset(ir_node *node)
{
	sched_info_t *info = get_irn_sched_info(node);
	info->scheduled = 0;
}

/**
 * Remove a node from the scheduled.
 * @param irn The node.
 */
static INLINE void _sched_remove(ir_node *irn)
{
  sched_info_t *info = get_irn_sched_info(irn);
  list_del(&info->list);
	INIT_LIST_HEAD(&info->list);
  info->scheduled = 0;
}

/**
 * Compare two nodes according to their position in the schedule.
 * @param a The first node.
 * @param b The second node.
 * @return A number smaller, equals to or larger than 0, if a is
 *         before, the same, or after b in the schedule.
 */
static INLINE int _sched_cmp(const ir_node *a, const ir_node *b)
{
  assert(_sched_is_scheduled(a) && _sched_is_scheduled(b));
  assert(get_nodes_block(a) == get_nodes_block(b));

  return get_irn_sched_info(a)->time_step - get_irn_sched_info(b)->time_step;
}

/**
 * Checks, if one node is scheduled before another.
 * @param n1   A node.
 * @param n2   Another node.
 * @return     1, if n1 is in front of n2 in the schedule, 0 else.
 * @note       Both nodes must be in the same block.
 */
static INLINE int _sched_comes_after(const ir_node *n1, const ir_node *n2)
{
	assert(_sched_is_scheduled(n1));
	assert(_sched_is_scheduled(n2));
	assert(get_nodes_block(n1) == get_nodes_block(n2));
	return _sched_get_time_step(n1) < _sched_get_time_step(n2);
}

/**
 * A predicate for a node.
 * @param irn The node.
 * @param data The custom data.
 * @return 1 if irn should be skipped. Else 0.
 */
typedef int (sched_predicator_t)(const ir_node *irn, void *data);

/**
 * Predicate for sched_skip(), returns non-zero if irn is a control flow changing node.
 *
 * @param irn   the node to evaluate
 * @param data  an arch_env_t * used to determine if irn is a cf
 *              node for the given architecture
 */
int sched_skip_cf_predicator(const ir_node *irn, void *data);

/**
 * Predicate for sched_skip(), returns non-zero if irn is a Phi node.
 *
 * Used with sched_skip().
 *
 * @param irn  the node to evaluate
 * @param data unused
 */
int sched_skip_phi_predicator(const ir_node *irn, void *data);

/**
 * Skip nodes in a schedule.
 * @param from        The node to start from.
 * @param forward     The direction (1 for forward, 0 for backward).
 * @param predicator  The predicator function which decides what is skipped.
 * @param data        context parameter for the predicator.
 *
 * @return The first node not rejected by the predicator or the block
 *         itself if all nodes were rejected.
 */
ir_node *sched_skip(ir_node *from, int forward,
    sched_predicator_t *predicator, void *data);

#define sched_get_time_step(irn)        _sched_get_time_step(irn)
#define sched_has_next(irn)             _sched_has_next(irn)
#define sched_has_prev(irn)             _sched_has_prev(irn)
#define sched_next(irn)                 _sched_next(irn)
#define sched_prev(irn)                 _sched_prev(irn)
#define sched_first(irn)                _sched_first(irn)
#define sched_last(irn)                 _sched_last(irn)
#define sched_add_before(before, irn)   _sched_add_before(before, irn)
#define sched_add_after(after, irn)     _sched_add_after(after, irn)
#define sched_remove(irn)               _sched_remove(irn)
#define sched_is_scheduled(irn)         _sched_is_scheduled(irn)
#define sched_comes_after(n1, n2)       _sched_comes_after(n1, n2)
#define sched_cmp(a, b)                 _sched_cmp(a, b)

#endif /* FIRM_BE_BESCHED_T_H */
