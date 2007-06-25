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
 * @brief   declarations for the mips assembler emitter
 * @author  Matthias Braun, Mehdi
 * @version $Id$
 */
#ifndef FIRM_BE_MIPS_MIPS_EMITTER_H
#define FIRM_BE_MIPS_MIPS_EMITTER_H

#include "irnode.h"

#include "../bearch.h"
#include "../beemitter.h"

#include "bearch_mips_t.h"

typedef struct mips_emit_env_t   mips_emit_env_t;

struct mips_emit_env_t {
	be_emit_env_t         *emit;
	const arch_env_t      *arch_env;
	const mips_code_gen_t *cg;
	mips_isa_t            *isa;
};

void mips_emit_source_register(mips_emit_env_t *env, const ir_node *node,
                               int pos);
void mips_emit_dest_register(mips_emit_env_t *env, const ir_node *node,
                             int pos);
void mips_emit_source_register_or_immediate(mips_emit_env_t *env,
                                            const ir_node *node, int pos);
void mips_emit_immediate(mips_emit_env_t *env, const ir_node *node);
void mips_emit_immediate_suffix(mips_emit_env_t *env, const ir_node *node,
                                int pos);
void mips_emit_load_store_address(mips_emit_env_t *env, const ir_node *node,
                                  int pos);
void mips_emit_jump_target(mips_emit_env_t *env, const ir_node *node);
void mips_emit_jump_target_proj(mips_emit_env_t *env, const ir_node *node,
                                long pn);
void mips_emit_jump_or_fallthrough(mips_emit_env_t *env, const ir_node *node,
                                   long pn);

void mips_register_emitters(void);
ir_node *mips_get_jump_block(const ir_node* node, long projn);

/** returns the label used for a block */
const char* mips_get_block_label(const ir_node* block);
/** returns the label for the jumptable */
const char* mips_get_jumptbl_label(const ir_node* switchjmp);

void mips_gen_routine(mips_code_gen_t *cg, ir_graph *irg);

#endif
