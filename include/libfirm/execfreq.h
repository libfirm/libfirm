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
 * @version     $Id$
 */
#ifndef FIRM_ANA_EXECFREQ_H
#define FIRM_ANA_EXECFREQ_H

#include "firm_types.h"
#include "begin.h"

struct ir_exec_freq;

/**
 * Create execfreq structure (to be used with set_execfreq)
 */
FIRM_API ir_exec_freq *create_execfreq(ir_graph *irg);

/**
 * Set execution frequency of a basic block
 */
FIRM_API void set_execfreq(ir_exec_freq *ef, const ir_node *block, double freq);

/**
 * Create execfreq structure and initialize with estimated frequencies
 */
FIRM_API ir_exec_freq *compute_execfreq(ir_graph *irg, double loop_weight);

FIRM_API void free_execfreq(ir_exec_freq *ef);

FIRM_API double get_block_execfreq(const ir_exec_freq *ef,
                                   const ir_node *block);

FIRM_API unsigned long get_block_execfreq_ulong(const ir_exec_freq *ef,
                                                const ir_node *block);

#include "end.h"

#endif
