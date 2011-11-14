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
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H
#define FIRM_BE_TEMPLATE_TEMPLATE_EMITTER_H

#include "irargs_t.h"
#include "irnode.h"
#include "debug.h"

#include "bearch.h"
#include "beemitter.h"

#include "bearch_TEMPLATE_t.h"

void TEMPLATE_emit_source_register(const ir_node *node, int pos);
void TEMPLATE_emit_dest_register(const ir_node *node, int pos);
void TEMPLATE_emit_immediate(const ir_node *node);

void TEMPLATE_emit_routine(ir_graph *irg);

#endif
