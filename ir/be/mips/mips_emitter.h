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

#ifndef _mips_EMITTER_H_
#define _mips_EMITTER_H_

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch_t.h"

#include "bearch_mips_t.h"

typedef struct _mips_emit_env_t {
	FILE                      *out;
	const arch_env_t          *arch_env;
	const mips_code_gen_t *cg;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} mips_emit_env_t;

const lc_arg_env_t *mips_get_arg_env(void);

void equalize_dest_src(FILE *F, ir_node *n);

int get_mips_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_mips_in_reg_name(ir_node *irn, int pos);

void mips_gen_routine(FILE *F, ir_graph *irg, const mips_code_gen_t *cg);
void mips_register_emitters(void);
ir_node *mips_get_jump_block(const ir_node* node, int projn);

/** returns the label used for a block */
const char* mips_get_block_label(const ir_node* block);
/** returns the label for the jumptable */
const char* mips_get_jumptbl_label(const ir_node* switchjmp);

#endif /* _mips_EMITTER_H_ */
