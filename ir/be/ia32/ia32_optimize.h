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
 * @brief       Implements several optimizations for IA32.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_OPTIMIZE_H
#define FIRM_BE_IA32_IA32_OPTIMIZE_H

#include "irgraph.h"
#include "bearch_ia32_t.h"

/**
 * Prepares irg for codegeneration. Places consts and transform reference mode
 * nodes into mode_Iu nodes.
 */
void ia32_pre_transform_phase(ir_graph *irg);

/**
 * Performs conv and address mode optimizations.
 * @param cg  The ia32 codegenerator object
 */
void ia32_optimize_graph(ir_graph *irg);

/**
 * Performs Peephole Optimizations an a graph.
 *
 * @param irg   the graph
 * @param cg    the code generator object
 */
void ia32_peephole_optimization(ir_graph *irg);

/** Initialize the ia32 address mode optimizer. */
void ia32_init_optimize(void);

/**
 * Creates an immediate node.
 */
ir_node *ia32_immediate_from_long(long val);

#endif /* FIRM_BE_IA32_IA32_OPTIMIZE_H */
