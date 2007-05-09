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
 * @brief   declarations for arm emitter
 * @author  Oliver Richter, Tobias Gneis
 * @version $Id$
 */
#ifndef FIRM_BE_ARM_ARM_EMITTER_H
#define FIRM_BE_ARM_ARM_EMITTER_H

#include "firm_types.h"
#include "pmap.h"
#include "irargs_t.h"
#include "debug.h"

#include "../bearch_t.h"

#include "bearch_arm_t.h"

/**
 * The ARM emitter environment.
 */
typedef struct _arm_emit_env_t {
	be_emit_env_t             *emit;     /**< environment for the generic GAS emitter. */
	const arch_env_t          *arch_env; /**< the architecture environment */
	const arm_code_gen_t      *cg;       /**< the code generator object */
	pmap                      *symbols;  /**< map containing all indirect symbols */
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} arm_emit_env_t;

void arm_emit_mode(arm_emit_env_t *env, const ir_node *node);
void arm_emit_source_register(arm_emit_env_t *env, const ir_node *node, int pos);
void arm_emit_dest_register(arm_emit_env_t *env, const ir_node *node, int pos);
void arm_emit_offset(arm_emit_env_t *env, const ir_node *node);
void arm_emit_immediate(arm_emit_env_t *env, const ir_node *node);
void arm_emit_shift(arm_emit_env_t *env, const ir_node *node);

void arm_gen_routine(const arm_code_gen_t *cg, ir_graph *irg);

#endif
