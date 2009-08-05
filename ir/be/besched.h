/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
#ifndef FIRM_BE_BESCHED_H
#define FIRM_BE_BESCHED_H

#include <stdio.h>

#include "irgraph.h"
#include "irnode.h"
#include "beirg.h"
#include "beinfo.h"
#include "beutil.h"

void be_sched_dump(FILE *f, ir_graph *irg);

/**
 * returns the time step of a node. Each node in a block has a timestep
 * unique to that block. a node schedule before another node has a lower
 * timestep than this node.
 */
int     have_sched_info(const ir_graph *irg);
int     sched_get_time_step(const ir_node *irn);
int     sched_has_next(const ir_node *irn);
int     sched_has_prev(const ir_node *irn);
int     sched_is_scheduled(const ir_node *irn);
ir_node *sched_next(const ir_node *irn);
ir_node *sched_prev(const ir_node *irn);
ir_node *sched_first(const ir_node *block);
ir_node *sched_last(const ir_node *block);
void    sched_add_before(ir_node *before, ir_node *irn);
void    sched_add_after(ir_node *after, ir_node *irn);
void    sched_init_block(ir_node *block);
void    sched_reset(ir_node *node);
void    sched_remove(ir_node *irn);

#define sched_is_end(irn)   is_Block(irn)
#define sched_is_begin(irn) is_Block(irn)

#define sched_foreach_from(from, irn) \
  for(irn = from; !sched_is_end(irn); irn = sched_next(irn))

#define sched_foreach_reverse_from(from, irn) \
  for(irn = from; !sched_is_begin(irn); irn = sched_prev(irn))

/**
 * A shorthand macro for iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach(block,irn) \
	sched_foreach_from(sched_first(block), irn)

/**
 * A shorthand macro for reversely iterating over a schedule.
 * @param block The block.
 * @param irn A ir node pointer used as an iterator.
 */
#define sched_foreach_reverse(block,irn) \
  sched_foreach_reverse_from(sched_last(block), irn)

/**
 * A shorthand macro for iterating over all Phi nodes of a schedule.
 * @param block The block.
 * @param phi A ir node pointer used as an iterator.
 */
#define sched_foreach_Phi(block,phi) \
	for (phi = sched_first(block); is_Phi(phi); phi = sched_next(phi))

/**
 * Removes dead nodes from schedule
 * @param irg  the graph
 */
void be_remove_dead_nodes_from_schedule(be_irg_t *birg);

#define SCHED_INITIAL_GRANULARITY (1 << 14)
#define get_irn_sched_info(irn)             (&be_get_info(skip_Proj_const(irn))->sched_info)

/**
 * Returns non-zero if schedule information is available
 * for a given graph.
 * @param irg  The graph.
 */
static inline int _have_sched_info(const ir_graph *irg)
{
	return be_info_initialized(irg);
}

/**
 * Check, if the node is scheduled.
 * @param irn The node.
 * @return 1, if the node is scheduled, 0 if not.
 */
static inline int _sched_is_scheduled(const ir_node *irn)
{
	return get_irn_sched_info(irn)->next != NULL;
}

/**
 * Get the time step of an irn in a schedule.
 * @param irn The node.
 * @return The time step in the schedule.
 */
static inline int _sched_get_time_step(const ir_node *irn)
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
static inline int to_appear_in_schedule(const ir_node *irn)
{
	switch(get_irn_opcode(irn)) {
		case iro_Start:
		case iro_Jmp:
		case iro_Break:
			return 1;
		case iro_Proj:
			return 0;
		default:
			return is_data_node(irn);
	}
}

/**
 * Check, if an ir_node has a scheduling successor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling successor, 0 if not.
 */
static inline int _sched_has_next(const ir_node *irn)
{
	const sched_info_t *info  = get_irn_sched_info(irn);
	const ir_node      *block = is_Block(irn) ? irn : get_nodes_block(irn);
	return info->next != block;
}

/**
 * Check, if an ir_node has a scheduling predecessor.
 * @param irn The ir node.
 * @return 1, if the node has a scheduling predecessor, 0 if not.
 */
static inline int _sched_has_prev(const ir_node *irn)
{
	const sched_info_t *info  = get_irn_sched_info(irn);
   	const ir_node      *block = is_Block(irn) ? irn : get_nodes_block(irn);
	return info->prev != block;
}

/**
 * Get the scheduling successor of a node.
 * @param irn The node.
 * @return The next ir node in the schedule or the block, if the node has no next node.
 */
static inline ir_node *_sched_next(const ir_node *irn)
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
static inline ir_node *_sched_prev(const ir_node *irn)
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
static inline ir_node *_sched_first(const ir_node *block)
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
static inline ir_node *_sched_last(const ir_node *block)
{
	assert(is_Block(block) && "Need a block here");
	return _sched_prev(block);
}

/**
 * Reassign the time steps in the schedule.
 * @param block The schedule to update.
 */
void sched_renumber(const ir_node *block);

static inline void _sched_set_time_stamp(const ir_node *irn)
{
	sched_info_t       *info      = get_irn_sched_info(irn);
	const sched_info_t *prev_info = get_irn_sched_info(info->prev);
	const sched_info_t *next_info = get_irn_sched_info(info->next);
	sched_timestep_t    before_ts = prev_info->time_step;
	sched_timestep_t    after_ts  = next_info->time_step;

	/*
	 * If we are the last, we can give us a big time step,
	 * else we have to compute our time step from our
	 * neighbours.
	 */
	if(before_ts >= after_ts) {
		info->time_step = before_ts + SCHED_INITIAL_GRANULARITY;
		/* overflow? */
		if (info->time_step <= before_ts) {
			sched_renumber(get_nodes_block(irn));
		}
	} else {
		sched_timestep_t ts = (before_ts + after_ts) / 2;

		/*
		 * If the resolution went out, we have to renumber
		 * this block.
		 */
		if(ts == before_ts || ts == after_ts)
			sched_renumber(get_nodes_block(irn));
		else
			info->time_step = ts;
	}
}

/**
 * Add a node to a block schedule.
 * @param block The block to whose schedule the node shall be added to.
 * @param irn The node to add.
 * @return The given node.
 */
static inline void _sched_add_before(ir_node *before, ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *next      = before;
	sched_info_t *next_info = get_irn_sched_info(next);
	ir_node      *prev      = next_info->prev;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	assert(_sched_is_scheduled(before));
	assert(!_sched_is_scheduled(irn));
	assert(!is_Proj(before));
	assert(!is_Proj(irn));

	info->prev = prev;
	info->next = next;
	prev_info->next = irn;
	next_info->prev = irn;
	_sched_set_time_stamp(irn);
}

/**
 * Add a node to a block schedule.
 * @param block The block to whose schedule the node shall be added to.
 * @param irn The node to add.
 * @return The given node.
 */
static inline void _sched_add_after(ir_node *after, ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *prev      = after;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	ir_node      *next      = prev_info->next;
	sched_info_t *next_info = get_irn_sched_info(next);
	assert(_sched_is_scheduled(after));
	assert(!_sched_is_scheduled(irn));
	assert(!is_Proj(after));
	assert(!is_Proj(irn));

	info->prev = prev;
	info->next = next;
	prev_info->next = irn;
	next_info->prev = irn;
	_sched_set_time_stamp(irn);
}

static inline void _sched_init_block(ir_node *block)
{
	sched_info_t *info = get_irn_sched_info(block);
	assert(info->next == NULL && info->time_step == 0);
	info->next = block;
	info->prev = block;
}

static inline void _sched_reset(ir_node *node)
{
	sched_info_t *info = get_irn_sched_info(node);
	info->next = NULL;
	info->prev = NULL;
}

/**
 * Remove a node from the scheduled.
 * @param irn The node.
 */
static inline void _sched_remove(ir_node *irn)
{
	sched_info_t *info      = get_irn_sched_info(irn);
	ir_node      *prev      = info->prev;
	ir_node      *next      = info->next;
	sched_info_t *prev_info = get_irn_sched_info(prev);
	sched_info_t *next_info = get_irn_sched_info(next);
	assert(_sched_is_scheduled(irn));

	prev_info->next = next;
	next_info->prev = prev;
	info->next      = NULL;
	info->prev      = NULL;
}

/**
 * Compare two nodes according to their position in the schedule.
 * @param a The first node.
 * @param b The second node.
 * @return A number smaller, equals to or larger than 0, if a is
 *         before, the same, or after b in the schedule.
 */
static inline int _sched_cmp(const ir_node *a, const ir_node *b)
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
static inline int _sched_comes_after(const ir_node *n1, const ir_node *n2)
{
	assert(_sched_is_scheduled(n1));
	assert(_sched_is_scheduled(n2));
	assert((is_Block(n1) ? n1 : get_nodes_block(n1)) == (is_Block(n2) ? n2 : get_nodes_block(n2)));
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
 * @param data  unused
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
ir_node *sched_skip(ir_node *from, int forward, sched_predicator_t *predicator, void *data);

#define have_sched_info(irg)            _have_sched_info(irg)
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



#endif
