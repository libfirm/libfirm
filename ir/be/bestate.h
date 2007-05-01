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
 * @brief       Handles state switching. This is basically the belady spill
 *              algorithm optimized for the 1-register case.
 * @author      Matthias Braun
 * @date        26.03.2007
 * @version     $Id$
 */
#ifndef FIRM_BE_BESTATE_H
#define FIRM_BE_BESTATE_H

#include "firm_types.h"
#include "beirg.h"
#include "bearch_t.h"

/**
 * Callback that should create a spill for a certain value. Can return NULL
 * if @p force == 0 and the value can be easily rematerialized
 */
typedef ir_node *(*create_spill_func) (void *env, ir_node *value, int force, ir_node *after);

/**
 * Callback that should create a reload for a certain value
 */
typedef ir_node *(*create_reload_func) (void *env, ir_node *value,
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

#endif /* FIRM_BE_BESTATE_H */
