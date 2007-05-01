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
 * @brief       Backend IRG modification routines.
 * @author      Sebastian Hack, Daniel Grund, Matthias Braun, Christian Wuerdig
 * @date        04.05.2005
 * @version     $Id$
 *
 * This file contains the following IRG modifications for be routines:
 * - insertion of Perm nodes
 * - empty block elimination
 * - a simple dead node elimination (set inputs of unreachable nodes to BAD)
 */
#ifndef FIRM_BE_BEIRGMOD_H
#define FIRM_BE_BEIRGMOD_H

#include "firm_types.h"
#include "beirg.h"

/**
 * Insert a Perm which permutes all (non-ignore) live values of a given register class
 * after a certain instruction.
 * @param lv        Liveness Information.
 * @param irn       The node to insert the Perm after.
 * @return          The Perm or NULL if nothing was live before @p irn.
 */
ir_node *insert_Perm_after(be_irg_t *birg, const arch_register_class_t *cls,
						   ir_node *irn);

/**
 * Removes basic blocks that only contain a jump instruction
 * (this will potentially create critical edges).
 *
 * @param irg  the graph that will be changed
 *
 * @return non-zero if the graph was changed, zero else
 */
int be_remove_empty_blocks(ir_graph *irg);

#endif /* FIRM_BE_BEIRGMOD_H */
