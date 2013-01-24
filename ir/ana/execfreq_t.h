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
 * @brief       Compute an estimate of basic block executions.
 * @author      Adam M. Szalkowski
 * @date        28.05.2006
 */
#ifndef FIRM_ANA_EXECFREQ_T_H
#define FIRM_ANA_EXECFREQ_T_H

#include "execfreq.h"

void init_execfreq(void);

void exit_execfreq(void);

void set_block_execfreq(ir_node *block, double freq);

typedef struct ir_execfreq_int_factors {
	double min_non_zero;
	double m, b;
} ir_execfreq_int_factors;

void ir_calculate_execfreq_int_factors(ir_execfreq_int_factors *factors,
                                       ir_graph *irg);

int get_block_execfreq_int(const ir_execfreq_int_factors *factors,
                           const ir_node *block);

#endif
