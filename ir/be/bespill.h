/*
 * Author:      Daniel Grund, Sebastian Hack, Matthias Braun
 * Date:		29.09.2005
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BESPILL_H_
#define BESPILL_H_

#include "firm_types.h"
#include "set.h"
#include "pset.h"
#include "debug.h"

#include "bechordal.h"
#include "be_t.h"

#include "bearch.h"

typedef struct _spill_env_t spill_env_t;

/**
 * Creates a new spill environment.
 */
spill_env_t *be_new_spill_env(be_irg_t *birg);

/**
 * Deletes a spill environment.
 */
void be_delete_spill_env(spill_env_t *senv);

/**
 * Sets the debug module of a spill environment.
 */
DEBUG_ONLY(void be_set_spill_env_dbg_module(spill_env_t *env, firm_dbg_module_t *dbg));

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
 * @param reload_cls  The register class the reloaded value will be put into
 * @param allow_remat Set to 1 if the node may be rematerialized instead of reloaded
 */
void be_add_reload(spill_env_t *senv, ir_node *to_spill, ir_node *before,
	const arch_register_class_t *reload_cls, int allow_remat);

/**
 * Analog to be_add_reload, but places the reload "on an edge" between 2 blocks
 * @see be_add_reload
 */
void be_add_reload_on_edge(spill_env_t *senv, ir_node *to_spill, ir_node *bl, int pos,
	const arch_register_class_t *reload_cls, int allow_remat);

/**
 * Analog to be_add_reload but adds an already created rematerialized node.
 */
void be_add_remat(spill_env_t *env, ir_node *to_spill, ir_node *before, ir_node *rematted_node);

/**
 * The main function that places real spills/reloads (or rematerializes values)
 * for all values where be_add_reload was called. It then rebuilds the
 * SSA-form and updates liveness information
 */
void be_insert_spills_reloads(spill_env_t *senv);

/**
 * There are 2 possibilities to spill a phi node: Only it's value, or replacing
 * the whole phi-node with a memory phi. Normally only the value of a phi will
 * be spilled unless you mark the phi with be_spill_phi.
 * (Remember that each phi needs a register, so you have to spill phis when
 *  there are more phis than registers in a block)
 */
void be_spill_phi(spill_env_t *env, ir_node *node);

/**
 * Returns the estimated costs if a node would get reloaded at a specific place
 * This usually returns the cost of spill + reload operation. But might return
 * smaller values if the value has already been spilled in a former run or
 * when it is possible to rematerialize the value.
 */
int be_get_reload_costs(spill_env_t *env, ir_node *to_spill, ir_node *before);

/**
 * Analog to be_get_reload_costs but returns the cost if the reload would be
 * placed "on an edge" between 2 blocks
 */
int be_get_reload_costs_on_edge(spill_env_t *env, ir_node *to_spill, ir_node *block, int pos);

#endif
