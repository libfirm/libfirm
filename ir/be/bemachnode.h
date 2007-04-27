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
 * Support for machine nodes with machine operands.
 *
 * @author Michael Beck
 */
#ifndef _BEMACHNODES_H
#define _BEMACHNODES_H

#include "firm_types.h"

/*
 * Machine nodes can have machine operands as inputs.
 * Machine operands are trees of other machine operands.
 * The leafs of machine operands are normal Firm nodes
 * or a machine operand with arity 0.
 *
 * The arity of a machine node is the number of leafs
 * that are NOT machine operands.
 *
 * For instance Add(x, Const) have the arity 1 if Const is a
 * machine operand, Add(x, ADR(b, i)) have arity 3.
 *
 * We simulate the the normal concept of get_irn_n/set_irn_n.
 * Instead of the node machine handle are used which are just
 * caches for the leaf positions of a machine node.
 * This means, it is only possible to manipulate the leafs!
 * Do not mix _mirn_ and _irn_ calls or terrible things can happen :-)
 *
 * Note further that currently the number of ins for a machine
 * instruction is limited to 8 + arity of Firm node which means
 * up to 8 registers can be added due to machine operands.
 *
 * Moreover machine handles must be destroyed yet...
 *
 * The convenience functions create the handle and drop it at the end.
 * as operands are seldom, they have a complexity of O(#predecessors)
 * which is near to O(1) for most node but use them seldom ...
 */

/**
 * The machine handle. A cache to access and set
 * the predecessors (leafs) of a machine node.
 */
typedef ir_node *** mirn_handle;

/**
 * Returns the machine handle for a machine node with machine operands.
 * The order of the predecessors in this handle is not guaranteed, except that
 * lists of operands as predecessors of Block or arguments of a Call are
 * consecutive.
 */
mirn_handle get_mirn_in(ir_node *n);

/** Frees a machine handle. */
void free_mirn_in(mirn_handle h);

/** Get the pos-th predecessor of a machine node represented by it's
    handle.. */
ir_node *get_mirn_n(mirn_handle h, int pos);

/** Convenience: Get the pos-th predecessor of a machine node. */
ir_node *_get_mirn_n(ir_node *n, int pos);

/** Set the n-th predecessor of a machine node represented by it's
    handle.. */
void set_mirn_n(mirn_handle h, int pos, ir_node *n);

/* Convenience: Set the pos-th predecessor of a machine node. */
void _set_mirn_n(ir_node *irn, int pos, ir_node *n);

/** Returns the arity (in terms of inputs) of a machine node represented by it's
    handle. */
int get_mirn_arity(mirn_handle h);

/* Convenience: Returns the arity of a machine node. */
int _get_mirn_arity(ir_node *n);

#endif /* _BEMACHNODES_H */
