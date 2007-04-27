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

/*
 * Project:     libFIRM
 * File name:   ir/opt/scalar_replace.h
 * Purpose:     scalar replacement of compounds
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 */

#ifndef _SCALAR_REPLACE_H_
#define _SCALAR_REPLACE_H_

#include "irgraph.h"

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

#endif /* _SCALAR_REPLACE_H_ */
