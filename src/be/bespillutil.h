/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       higher level abstraction for the creation of spill and reload
 *              instructions and rematerialisation of values.
 * @author      Daniel Grund, Sebastian Hack, Matthias Braun
 * @date        29.09.2005
 */
#ifndef FIRM_BE_BESPILLUTIL_H
#define FIRM_BE_BESPILLUTIL_H

#include <stdbool.h>
#include "firm_types.h"
#include "bera.h"

typedef struct spill_env_t spill_env_t;

/**
 * Creates a new spill environment.
 */
spill_env_t *be_new_spill_env(ir_graph *irg, const regalloc_if_t *regif);

/**
 * Deletes a spill environment.
 */
void be_delete_spill_env(spill_env_t *senv);

/**
 * Return the last control flow node of a block.
 */
ir_node *be_get_end_of_block_insertion_point(const ir_node *block);

/**
 * Marks a point until which a node must be spilled.
 */
void be_add_spill(spill_env_t *senv, ir_node *to_spill, ir_node *after);

/**
 * Inserts a new entry into the list of reloads to place (the real nodes will
 * be created when be_insert_spills_reloads is run). You don't have to
 * explicitly create spill nodes, they will be created automatically after
 * the definition of a value as soon as a reload is created. (we should add a
 * possibility for explicit spill placement in the future)
 *
 * @param senv        The spill environment
 * @param to_spill    The node which is about to be spilled
 * @param before      The node before the reload should be added
 */
void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before);

/**
 * Analog to be_add_reload, but places the reload "on an edge" between 2 blocks
 * @see be_add_reload
 */
void be_add_reload_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl,
                           int pos);

/**
 * The main function that places real spills/reloads (or rematerializes values)
 * for all values where be_add_reload was called. It then rebuilds the
 * SSA-form and updates liveness information
 */
void be_insert_spills_reloads(spill_env_t *senv);

/**
 * There are 2 possibilities to spill a phi node: Only its value, or replacing
 * the whole phi-node with a memory phi. Normally only the value of a phi will
 * be spilled unless you mark the phi with be_spill_phi.
 * (Remember that each phi needs a register, so you have to spill phis when
 *  there are more phis than registers in a block)
 */
void be_spill_phi(spill_env_t *env, ir_node *phi);

/**
 * Returns the estimated costs if a node would ge spilled. This does only return
 * the costs for the spill instructions, not the costs for needed reload
 * instructions. The value is weighted by the estimated execution frequency of
 * the spill.
 */
double be_get_spill_costs(spill_env_t *env, ir_node *to_spill, ir_node *before);

/**
 * Returns the estimated costs if a node would get reloaded at a specific place
 * This returns the costs for a reload instructions, or when possible the costs
 * for a rematerialisation. The value is weighted by the estimated execution
 * frequency of the reload/rematerialisation.
 */
double be_get_reload_costs(spill_env_t *env, ir_node *to_spill,
                           ir_node *before);

unsigned be_get_reload_costs_no_weight(spill_env_t *env, const ir_node *to_spill,
                                       const ir_node *before);


/**
 * Analog to be_get_reload_costs but returns the cost if the reload would be
 * placed "on an edge" between 2 blocks
 */
double be_get_reload_costs_on_edge(spill_env_t *env, ir_node *to_spill,
                                   ir_node *block, int pos);

typedef struct {
	unsigned n_spills;
	unsigned n_reloads;
	double   spill_costs;
	double   reload_costs;
} be_total_spill_costs_t;

/**
 * Collect spill/reload cost statistics for a graph.
 * @param irg    The graph.
 * @param costs  A struct which will be filled with the costs.
 */
void be_get_total_spill_costs(ir_graph *irg, be_total_spill_costs_t *costs);

/**
 * Create a be_Reload node. This function is compatible to the
 * arch_env->new_reload interface.
 */
ir_node *be_new_reload(ir_node *value, ir_node *spilled, ir_node *before);

/**
 * Prepare graph for spilling: This adds explicit copies where this is
 * unavoidable because of register constraints. This also makes the real
 * register pressure explicit which is necessary for correct spilling.
 * @param irg  The graph
 */
void be_spill_prepare_for_constraints(ir_graph *irg);

#endif
