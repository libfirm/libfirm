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
 * @brief     Various irnode constructors.  Automatic construction
 *            of SSA representation. Private Header
 * @author    Martin Trapp, Christian Schaefer, Michael Beck
 * @version   $Id$
 */
#ifndef FIRM_IR_IRCONS_T_H
#define FIRM_IR_IRCONS_T_H

#include "ircons.h"
#include "irgraph_t.h"

/**
 * Initializes the graph construction.
 *
 * @param func  callback that is called if a uninitialized
 *              variable is detected
 *
 * @see uninitialized_local_variable_func_t
 */
void firm_init_cons(uninitialized_local_variable_func_t *func);

/**
 * Creates a new Anchor node.
 */
ir_node *new_Anchor(ir_graph *irg);

/**
 * Allocate a frag array for a node if the current graph state is phase_building.
 *
 * @param irn         the node for which the frag array should be allocated
 * @param op          the opcode of the (original) node, if does not match opcode of irn,
 *                    nothing is done
 * @param frag_store  the address of the frag store in irn attributes, if this
 *                    address contains a value != NULL, does nothing
 */
void firm_alloc_frag_arr(ir_node *irn, ir_op *op, ir_node ***frag_store);

/**
 * Restarts SSA construction on the given graph with n_loc
 * new values.
 *
 * @param irg    the graph on which the SSA construction is restarted
 * @param n_loc  number of new variables
 *
 * After this function is complete, the graph is in phase_building
 * again and set_value()/get_value() and mature_block() can be used
 * to construct new values.
 *
 * @note do not use get_mem()/set_mem() they will build a new memory
 *       instead of modifying the old one which might be not what you expect...
 */
void ssa_cons_start(ir_graph *irg, int n_loc);

/**
 * Finalize the (restarted) SSA construction. Matures all blocks that are
 * not matured yet and reset the graph state to phase_high.
 *
 * @param irg    the graph on which the SSA construction was restarted
 */
void ssa_cons_finish(ir_graph *irg);

#endif
