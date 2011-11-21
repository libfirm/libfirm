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
 * @brief       Performs lowering of perm nodes. Inserts copies to assure
 *              register constraints.
 * @author      Christian Wuerdig
 * @date        14.12.2005
 */
#ifndef FIRM_BE_BELOWER_H
#define FIRM_BE_BELOWER_H

#include "beirg.h"

/**
 * Walks over all nodes to assure register constraints.
 *
 * @param irg  The graph
 */
void assure_constraints(ir_graph *irg);

/**
 * Push nodes that do not need to be permed through the Perm.
 * This is commonly a reload cascade at block ends.
 * @note This routine needs interference.
 * @note We can probably implement it a little more efficient.
 *       Especially searching the frontier lazily might be better.
 *
 * @param perm The perm
 * @param env  The lowerer environment
 *
 * @return     1 if there is something left to perm over.
 *             0 if removed the complete perm.
 */
int push_through_perm(ir_node *perm);

/**
 * Walks over all blocks in an irg and performs lowering need to be
 * done after register allocation (e.g. perm lowering).
 *
 * @param irg       The graph
 */
void lower_nodes_after_ra(ir_graph *irg);

#endif
