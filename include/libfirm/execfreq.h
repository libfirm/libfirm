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
 *
 * Execution frequencies specify how often a basic block is expected to get
 * executed during execution of a function.
 * For example the start block has a natural execution frequency of 1.0, the
 * two branches of a simple if 0.5, nodes in a simple loop 10.0 ...
 * Execution frequencies can either get estimated based on the structure of the
 * control flow graph or can be calculated based on profile information.
 * @{
 */

/** Estimates execution frequency of a graph.
 * You can query the frequencies with get_block_execfreq().
 */
FIRM_API void ir_estimate_execfreq(ir_graph *irg);

/** Returns execution frequency of block @p block. */
FIRM_API double get_block_execfreq(const ir_node *block);

/** @} */

#include "end.h"

#endif
