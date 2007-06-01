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
 * @brief    Run most simple type analyses.
 * @author   Goetz Lindenmaier
 * @date     22.8.2003
 * @version  $Id$
 * @summary
 *  We compute type information for each node.  It is derived from the
 *  types of the origines of values, e.g. parameter types can be derived
 *  from the method type.
 *  The type information so far is saved in the link field.
 */
#ifndef FIRM_ANA_IRSIMPLETYPE_H
#define FIRM_ANA_IRSIMPLETYPE_H

#include "irgraph.h"
#include "irnode.h"
#include "typerep.h"



/* ------------ Building and Removing the type information  ----------- */

/** Computes type information for each node in all ir graphs.
 *
 * Computes type information for each node.  Stores the information in the
 * field defined in irtypeinfo.h. Sets typestate in irg to irg_typeinfo_consistent.
 *
 * Derives the information from nodes/patterns that give hints about the
 * type, as projecting an argument from the Start node.  Deletes all previous
 * type information.
 *
 * If a non-pointer type is specified for a pointer value (as the Alloc node does:
 * it contains the type allocated, but to type the result we need the type that
 * describes a pointer to the allocated type) searches for a corresponding pointer
 * type.  If several exist, uses one of them.  If none exists, uses unknown_type.
 *
 * Uses the link field of types.  Sets this field of each type to point to a
 * pointer type that points to the type (Got it? ;-)).
 */
void simple_analyse_types(void);

/** Frees all type information datastructures.  Sets the flag in irg to "???". */
void free_simple_type_information(void);

/** Computes type information for a node.
 *
 *  Computes type information for a node.  Does not remark this type information
 *  in the ir.  Computes the type information by analysing the predecessors.
 *  Calls the same basic analyses routine for a node as simple_analyse_types,
 *  but returns unknown_type for Phi nodes.
 *  Each call is theoretically O(n).
 *
 *  Not yet implemented, but I guess we want this for iropt, to find the
 *  type for newly allocated constants.
 */
/* type *analyse_irn_type(ir_node *node); */

#endif
