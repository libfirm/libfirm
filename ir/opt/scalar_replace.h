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
 * @brief   Scalar replacement of compounds.
 * @author  Beyhan Veliev, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_SCALAR_REPLACE_H
#define FIRM_OPT_SCALAR_REPLACE_H

#include "firm_types.h"

/**
 * Returns non-zero, if the address of an entity
 * represented by a Sel node (or it's successor Sels) is taken.
 *
 * @param sel  the Sel node
 */
int is_address_taken(ir_node *sel);

/**
 * Do the scalar replacement optimization.
 * Replace local compound entities (like structures and arrays)
 * with atomic values if possible. Does not handle classes yet.
 *
 * @param irg  the graph which should be optimized
 */
void scalar_replacement_opt(ir_graph *irg);

#endif /* FIRM_OPT_SCALAR_REPLACE_H */
