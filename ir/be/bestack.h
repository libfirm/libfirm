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
 * @brief       Helper functions for handling offsets into stack frames/
 *              activation records.
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_BESTACK_H
#define FIRM_BE_BESTACK_H

#include "firm_types.h"
#include "be_types.h"

/**
 * Rewire all stack modifying nodes and their users to assure SSA property.
 * @param env   The abi
 */
void be_abi_fix_stack_nodes(ir_graph *irg);

/**
 * Fix the stack bias for all nodes accessing the stack frame using the
 * stack pointer.
 */
void be_abi_fix_stack_bias(ir_graph *irg);

int be_get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent,
                               int bias);

#endif
