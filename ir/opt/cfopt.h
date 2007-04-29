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
 * @brief   Control flow optimizations.
 * @author  Goetz Lindenmaier, Michael Beck, Sebastian Hack
 * @version $Id$
 */
#ifndef FIRM_OPT_CFOPT_H
#define FIRM_OPT_CFOPT_H

#include "irgraph.h"

/** Control flow optimization.
 *
 * Removes empty blocks doing if simplifications and loop simplifications.
 * A block is empty if it contains only a Jmp node and Phi nodes.
 * Merges single entry single exit blocks with their predecessor
 * and propagates dead control flow by calling equivalent_node().
 * Independent of compiler flag it removes Tuples from cf edges,
 * Bad predecessors from Blocks and Phis, and unnecessary predecessors of End.
 *
 * @bug So far destroys backedge information.
 * @bug Chokes on Id nodes if called in a certain order with other
 *      optimizations.  Call local_optimize_graph() before to remove
 *      Ids.
 */
void optimize_cf(ir_graph *irg);

#endif /* FIRM_OPT_CFOPT_H */
