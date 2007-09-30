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

static inline ir_node *be_peephole_get_value(unsigned regclass_idx,
                                             unsigned register_idx)
{
	return register_values[regclass_idx][register_idx];
}

static inline ir_node *be_peephole_get_reg_value(const arch_register_t *reg)
{
	unsigned regclass_idx = arch_register_class_index(arch_register_get_class(reg));
	unsigned register_idx = arch_register_get_index(reg);

	return be_peephole_get_value(regclass_idx, register_idx);
}

typedef ir_node*(*peephole_opt_func) (ir_node *node);

/**
 * Do peephole optimisations, works backwards over blockschedules and calls the generic op handler function
 * which should be of type peephole_opt_func. The values of the values in the registers are availble
 * in the register_values variable during the optimisation functions.
 */
void be_peephole_opt(be_irg_t *birg);

#endif
