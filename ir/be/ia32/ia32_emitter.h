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
 * @brief       This file implements the ia32 node emitter.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_EMITTER_H
#define FIRM_BE_IA32_IA32_EMITTER_H

#include "irnode.h"

#include "../bearch.h"
#include "../beemitter.h"

#include "bearch_ia32_t.h"

typedef struct ia32_emit_env_t {
	be_emit_env_t         *emit;
	const arch_env_t      *arch_env;
	const ia32_code_gen_t *cg;
	ia32_isa_t            *isa;
} ia32_emit_env_t;

void ia32_emit_source_register(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_dest_register(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_x87_name(ia32_emit_env_t *env, const ir_node *node, int pos);
void ia32_emit_immediate(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_mode_suffix(ia32_emit_env_t *env, const ir_mode *mode);
void ia32_emit_x87_mode_suffix(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_xmm_mode_suffix(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_xmm_mode_suffix_s(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_extend_suffix(ia32_emit_env_t *env, const ir_mode *mode);
void ia32_emit_binop(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_unop(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_am(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_adr(ia32_emit_env_t *env, const ir_node *node);
void ia32_emit_x87_binop(ia32_emit_env_t *env, const ir_node *node);

void ia32_gen_routine(ia32_code_gen_t *cg, ir_graph *irg);

#endif /* FIRM_BE_IA32_IA32_EMITTER_H */
