/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief       lower mode_b operations to something the backend can handle
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id$
 *
 * Most machines can't really manipulate mode_b values (which are usually
 * modeled as cpu flags). So you often have to convert them into machine words
 * with the values 0/1 and operate on them instead.
 *
 * After this pass the following holds:
 *   - The only inputs with mode_b are for the Cond node and the Sel input of
 *     a Mux node.
 *   - The only nodes producing mode_b are: Proj(Cmp)
 */
#ifndef FIRM_LOWER_MODE_B_H
#define FIRM_LOWER_MODE_B_H

#include "firm_types.h"

/**
 * Function which creates a "set" instraction. A "set" instruction takes a
 * condition value (a value with mode_b) as input and produces a value in a
 * general purpose integer mode.
 * Most architectures have special intrinsics for this. But if all else fails
 * you can just produces the an if-like construct.
 */
typedef ir_node* (*create_set_func)(ir_node *cond);

/**
 * implementation of create_set_func which produces a cond with control
 * flow
 */
ir_node *ir_create_cond_set(ir_node *cond, ir_mode *dest_mode);

typedef struct lower_mode_b_config_t {
	/* mode that is used to transport 0/1 values */
	ir_mode *lowered_mode;
	/* callback for creating set-like instructions */
	create_set_func create_set;
} lower_mode_b_config_t;

/**
 * Lowers mode_b operations to integer arithmetic. After the lowering the only
 * operations with mode_b are the Projs of Cmps; the only nodes with mode_b
 * inputs are Cond and Psi nodes.
 *
 * Example: Psi(a < 0, 1, 0) => a >> 31
 *
 * @param irg      the firm graph to lower
 * @param config   configuration for mode_b lowerer
 */
void ir_lower_mode_b(ir_graph *irg, const lower_mode_b_config_t *config);

#endif
