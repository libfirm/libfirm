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
 * @brief    declarations for emit functions
 * @version  $Id$
 */
#ifndef FIRM_BE_SPARC_EMITTER_H
#define FIRM_BE_SPARC_EMITTER_H

#include "irargs_t.h"
#include "irnode.h"
#include "debug.h"

#include "../bearch.h"
#include "../beemitter.h"

#include "bearch_sparc_t.h"

void sparc_emit_immediate(const ir_node *node);
void sparc_emit_high_immediate(const ir_node *node);
void sparc_emit_mode(const ir_node *node);
void sparc_emit_source_register(const ir_node *node, int pos);
void sparc_emit_reg_or_imm(const ir_node *node, int pos);
void sparc_emit_dest_register(const ir_node *node, int pos);
void sparc_emit_offset(const ir_node *node, int offset_node_pos);
void sparc_emit_load_mode(const ir_node *node);
void sparc_emit_store_mode(const ir_node *node);
void sparc_emit_float_load_store_mode(const ir_node *node);
void sparc_emit_mode_sign_prefix(const ir_node *node);
void sparc_emit_fp_mode_suffix(const ir_node *node);
void sparc_emit_fp_conv_source(const ir_node *node);
void sparc_emit_fp_conv_destination(const ir_node *node);

void sparc_emit_routine(ir_graph *irg);

void sparc_init_emitter(void);

#endif
