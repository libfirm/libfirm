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

#include "firm_types.h"
#include "irargs_t.h"  // this also inlucdes <libcore/lc_print.h>
#include "debug.h"

#include "../bearch_t.h"

#include "bearch_ppc32_t.h"

void ppc32_gen_routine(const ppc32_code_gen_t *cg, ir_graph *irg);


void ppc32_emit_source_register(const ir_node *node, int pos);
void ppc32_emit_dest_register(const ir_node *node, int pos);
void ppc32_emit_offset(const ir_node *n);
void ppc32_emit_immediate(const ir_node *n);
void ppc32_emit_rlwimi_helper(const ir_node *n);

#endif
