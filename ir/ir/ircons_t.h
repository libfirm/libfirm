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
void init_cons(uninitialized_local_variable_func_t *func);

/**
 * Creates a new Anchor node.
 */
ir_node *new_Anchor(ir_graph *irg);

/* inline functions */

static INLINE ir_node *
_new_d_Bad(void) {
	return get_irg_bad(current_ir_graph);
}

static INLINE ir_node *
_new_d_NoMem(void) {
	return get_irg_no_mem(current_ir_graph);
}


#define new_d_Bad()               _new_d_Bad()
#define new_d_NoMem()             _new_d_NoMem()

#endif
