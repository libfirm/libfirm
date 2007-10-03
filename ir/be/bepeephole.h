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
 * @brief       peephole optimisation framework
 * @author      Matthias Braun
 * @version     $Id$
 */

#ifndef BEPEEPHOLE_H
#define BEPEEPHOLE_H

#include "beirg.h"
#include "bearch_t.h"

extern ir_node ***register_values;

static INLINE ir_node *be_peephole_get_value(unsigned regclass_idx,
                                             unsigned register_idx)
{
	return register_values[regclass_idx][register_idx];
}

static INLINE ir_node *be_peephole_get_reg_value(const arch_register_t *reg)
{
	unsigned regclass_idx = arch_register_class_index(arch_register_get_class(reg));
	unsigned register_idx = arch_register_get_index(reg);

	return be_peephole_get_value(regclass_idx, register_idx);
}

/**
 * Datatype of the generic op handler for optimisation. If it exchanged node
 * with something else, then it must return the new node, otherwise NULL.
 */
typedef ir_node*(*peephole_opt_func) (ir_node *node);

/**
 * Do peephole optimisations. It traverses the schedule of all blocks in
 * backward direction. The register_values variable indicates which (live)
 * values are stored in which register.
 * The generic op handler is called for each node if it exists. That's where
 * backedn specific optimisations should be performed based on the
 * register-liveness information.
 */
void be_peephole_opt(be_irg_t *birg);

#endif
