/**
 * @file
 * @brief       Handles state switching. This is basically the belady spill
 *              algorithm optimized for the 1-register case.
 * @author      Matthias Braun
 * @date        26.03.2007
 * @version     $Id$
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef FIRM_BE_BESTATE_H
#define FIRM_BE_BESTATE_H

#include "irnode.h"
#include "beirg.h"
#include "bearch.h"

/**
 * Callback that should create a spill for a certain value. Can return NULL
 * if @p force == 0 and the value can be easily rematerialized
 */
typedef ir_node* (*create_spill_func) (void *env, ir_node *value, int force,
                                       ir_node *after);

/**
 * Callback that should create a reload for a certain value
 */
typedef ir_node* (*create_reload_func) (void *env, ir_node *value,
                                        ir_node *spill, ir_node *before,
                                        ir_node *last_value);

/**
 * Some state is expressed as a register. nodes defining a value for this
 * register are known states. You can connect these to nodes to express that a
 * node needs the processor to be in a certain state.
 * This functions asserts that the state is switched to fullfill all state
 * requirements of nodes.
 */
void be_assure_state(be_irg_t *birg, const arch_register_t *reg, void *func_env,
                     create_spill_func spill_func,
                     create_reload_func reload_func);

#endif
