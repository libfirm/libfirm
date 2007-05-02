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
 * @brief   declarations for ppc32 emitter
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifndef FIRM_BE_PPC32_PPC32_EMITTER_H
#define FIRM_BE_PPC32_PPC32_EMITTER_H

#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "irnode.h"
#include "debug.h"

#include "../bearch_t.h"

#include "bearch_ppc32_t.h"

typedef struct _emit_env_t {
	FILE                      *out;
	const arch_env_t          *arch_env;
	const ppc32_code_gen_t    *cg;
	DEBUG_ONLY(firm_dbg_module_t *mod;)
} ppc32_emit_env_t;

const lc_arg_env_t *ppc32_get_arg_env(void);

void equalize_dest_src(FILE *F, ir_node *n);

int get_ppc32_reg_nr(ir_node *irn, int posi, int in_out);
const char *get_ppc32_in_reg_name(ir_node *irn, int pos);

void ppc32_gen_routine(FILE *F, ir_graph *irg, const ppc32_code_gen_t *cg);

const char *ppc32_rlwimi_emit_helper(const ir_node *n, ppc32_emit_env_t *env);


#endif
