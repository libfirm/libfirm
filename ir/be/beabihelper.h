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
 * @brief       Helper functions for handling ABI constraints in the code
 *              selection phase.
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_BEABI_HELPER_H
#define FIRM_BE_BEABI_HELPER_H

#include "firm_types.h"
#include "be_types.h"
#include "bearch.h"

typedef struct beabi_helper_env_t beabi_helper_env_t;

/**
 * Creates a helper object for the ABI constraint handling.
 */
beabi_helper_env_t *be_abihelper_prepare(ir_graph *irg);

/**
 * Terminates a helper object for the ABI constraint handling.
 */
void be_abihelper_finish(beabi_helper_env_t *env);

/**
 * Mark a registers value at the beginning of the function as significant.
 * This is necessary for things like:
 *  - Callee-Save registers (we need to restore that value at the end)
 *  - Parameters passed in registers
 *  - stack pointer, base pointer, ...
 * It is possible to specify additional irn flags (useful to mark a value
 * as ignore or produces_sp).
 */
void be_prolog_add_reg(beabi_helper_env_t *env, const arch_register_t *reg,
                       arch_register_req_type_t flags);

/**
 * Creates a start node.
 * Must be called after all prolog_add_reg calls
 */
ir_node *be_prolog_create_start(beabi_helper_env_t *env, dbg_info *dbgi,
                                ir_node *block);

/**
 * Creates a barrier node which lets all registers specified by prolog_add_reg
 * pass through
 */
ir_node *be_prolog_create_barrier(beabi_helper_env_t *env, ir_node *block);

/**
 * Get "value" of a register.
 * This usually creates a Proj node for the start-node or barrier-node.
 * Or returns the value set by a abi_helper_set_reg_value call
 */
ir_node *be_prolog_get_reg_value(beabi_helper_env_t *env,
                                 const arch_register_t *reg);

ir_node *be_prolog_get_memory(beabi_helper_env_t *env);

/**
 * Set current register value.
 */
void be_prolog_set_reg_value(beabi_helper_env_t *env,
                             const arch_register_t *reg, ir_node *value);

void be_prolog_set_memory(beabi_helper_env_t *env, ir_node *value);

/**
 * Set value of register at the end of the function. Necessary for:
 *  - Callee-save registers
 *  - Return values in registers
 *  - stack pointer, base pointer
 */
void be_epilog_add_reg(beabi_helper_env_t *env, const arch_register_t *reg,
                       arch_register_req_type_t flags, ir_node *value);

void be_epilog_set_reg_value(beabi_helper_env_t *env,
                             const arch_register_t *reg, ir_node *value);

ir_node *be_epilog_get_reg_value(beabi_helper_env_t *env,
                             const arch_register_t *reg);

void be_epilog_set_memory(beabi_helper_env_t *env, ir_node *value);

ir_node *be_epilog_get_memory(beabi_helper_env_t *env);

void be_epilog_begin(beabi_helper_env_t *env);

ir_node *be_epilog_create_barrier(beabi_helper_env_t *env, ir_node *block);

/**
 * Create return node and finishes epilog handling
 */
ir_node *be_epilog_create_return(beabi_helper_env_t *env, dbg_info *dbgi,
                                 ir_node *block);

/**
 * Adds a X->Proj->Keep for each output value of X which has no Proj yet
 */
void be_add_missing_keeps(ir_graph *irg);

/**
 * Collect firm nodes that will probably modify the stack.
 * Put them into an order that respects all their dependencies.
 */
void be_collect_stacknodes(beabi_helper_env_t *env);

/**
 * return node that should produce the predecessor stack node in a block.
 * returns NULL if there's no predecessor in the current block.
 */
ir_node *be_get_stack_pred(const beabi_helper_env_t *env, const ir_node *node);

#endif
