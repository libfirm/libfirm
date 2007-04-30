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
 * @brief    declarations for emit functions
 * @version  $Id$
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H
#define FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch_t.h"
#include "../beemitter.h"

#include "bearch_TEMPLATE_t.h"

typedef struct _TEMPLATE_emit_env_t {
	be_emit_env_t             *emit;
	const arch_env_t          *arch_env;
	const TEMPLATE_code_gen_t *cg;
	TEMPLATE_isa_t            *isa;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} TEMPLATE_emit_env_t;

void TEMPLATE_emit_source_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos);
void TEMPLATE_emit_dest_register(TEMPLATE_emit_env_t *env, const ir_node *node, int pos);
void TEMPLATE_emit_immediate(TEMPLATE_emit_env_t *env, const ir_node *node);

int get_TEMPLATE_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_TEMPLATE_in_reg_name(ir_node *irn, int pos);

void TEMPLATE_gen_routine(const TEMPLATE_code_gen_t *cg, ir_graph *irg);

#endif
