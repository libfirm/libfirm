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

#include "bearch_ia32_t.h"

void ia32_emit_source_register(const ir_node *node, int pos);
void ia32_emit_dest_register(const ir_node *node, int pos);
void ia32_emit_8bit_dest_register(const ir_node *node, int pos);
void ia32_emit_x87_register(const ir_node *node, int pos);
void ia32_emit_source_register_or_immediate(const ir_node *node, int pos);
void ia32_emit_8bit_source_register_or_immediate(const ir_node *node, int pos);
void ia32_emit_mode_suffix(const ir_node *node);
void ia32_emit_x87_mode_suffix(const ir_node *node);
void ia32_emit_xmm_mode_suffix(const ir_node *node);
void ia32_emit_xmm_mode_suffix_s(const ir_node *node);
void ia32_emit_extend_suffix(const ir_mode *mode);
void ia32_emit_cmp_suffix_node(const ir_node *node, int flags_pos);
void ia32_emit_binop(const ir_node *node);
void ia32_emit_am_or_dest_register(const ir_node *node, int pos);
void ia32_emit_unop(const ir_node *node, int pos);
void ia32_emit_am(const ir_node *node);
void ia32_emit_x87_binop(const ir_node *node);

void ia32_gen_routine(ia32_code_gen_t *cg, ir_graph *irg);
void ia32_emit_exc_label(const ir_node *node);

#endif
