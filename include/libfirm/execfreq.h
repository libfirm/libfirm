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
#ifndef FIRM_ANA_EXECFREQ_H
#define FIRM_ANA_EXECFREQ_H

#include "firm_types.h"
#include "begin.h"

/**
 * @ingroup irana
 * @defgroup execfreq Basic Block Execution Frequency
 * @{
 */

/** Creates execfreq structure (to be used with set_execfreq) */
FIRM_API ir_exec_freq *create_execfreq(ir_graph *irg);

/**
 * Sets execution frequency of a basic block
 */
FIRM_API void set_execfreq(ir_exec_freq *ef, const ir_node *block, double freq);

/** Creates execfreq structure and initialize with estimated frequencies. */
FIRM_API ir_exec_freq *compute_execfreq(ir_graph *irg, double loop_weight);

/** Frees memory occupied by execution frequency structure @p ef. */
FIRM_API void free_execfreq(ir_exec_freq *ef);

/** Returns execution frequency of block @p block. */
FIRM_API double get_block_execfreq(const ir_exec_freq *ef,
                                   const ir_node *block);

/** Returns execution frequency of block @p block, scaled into the range
 * of an unsigned long type. */
FIRM_API unsigned long get_block_execfreq_ulong(const ir_exec_freq *ef,
                                                const ir_node *block);

/** @} */

#include "end.h"

#endif
