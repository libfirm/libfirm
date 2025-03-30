/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       peephole optimization framework
 * @author      Matthias Braun
 */
#ifndef BEPEEPHOLE_H
#define BEPEEPHOLE_H

#include "bearch.h"

extern ir_node **register_values;

static inline ir_node *be_peephole_get_value(unsigned register_idx)
{
	return register_values[register_idx];
}

static inline ir_node *be_peephole_get_reg_value(const arch_register_t *reg)
{
	unsigned register_idx = reg->global_index;
	return be_peephole_get_value(register_idx);
}

/**
 * Datatype of the generic op handler for optimization.
 */
typedef void (*peephole_opt_func) (ir_node *node);

/**
 * When doing peephole optimization use this function instead of plain
 * exchange(), so it can update its internal state.  This function also removes
 * the old node from the schedule.
 */
void be_peephole_exchange(ir_node *old, ir_node *nw);

/**
 * Same as be_peephole_exchange(), but this function should be used for exchanging a Proj node with a new node, as it
 * updates the internal state correctly for this case.
 * This function removes the proj operand of old_proj from the schedule.
 */
void be_peephole_exchange_using_proj(ir_node *old_proj, ir_node *nw);

/**
 * Same as be_peephole_exchange(), but also replace @p old in the schedule
 * by @p nw.
 */
void be_peephole_replace(ir_node *old, ir_node *nw);

ir_node *be_peephole_to_tuple(ir_node *node);

/**
 * Tries to optimize a beIncSp node with its previous IncSP node.
 * Must be run from a be_peephole_opt() context.
 *
 * @param node  a be_IncSP node
 *
 * @return whether @p node was removed
 */
bool be_peephole_IncSP_IncSP(ir_node *node);

bool be_has_only_one_user(ir_node *node);

typedef ir_entity *(*get_frame_entity_func)(const ir_node *node);

/**
 * In a scheduled program with registers assigned,
 * checks whether @p node can be moved before @p before without changing program
 * semantics.
 *
 * Note: It is allowed to use this function without being in a peephole
 * optimization phase.
 */
bool be_can_move_down(ir_heights_t *heights, const ir_node *node,
                      const ir_node *before,
                      get_frame_entity_func get_frame_entity);

bool be_can_move_up(ir_heights_t *heights, const ir_node *node,
                    const ir_node *after);

/**
 * Register a peephole optimization function.
 */
static inline void register_peephole_optimization(ir_op *const op, peephole_opt_func const func)
{
	assert(!op->ops.generic);
	op->ops.generic = (op_func)func;
}

/**
 * Do peephole optimizations. It traverses the schedule of all blocks in
 * backward direction. The register_values variable indicates which (live)
 * values are stored in which register.
 * The generic op handler is called for each node if it exists. That's where
 * backend specific optimizations should be performed based on the
 * register-liveness information.
 */
void be_peephole_opt(ir_graph *irg);

#endif
