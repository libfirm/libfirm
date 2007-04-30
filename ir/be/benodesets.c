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
 * @brief       A lightweight wrapper around pset to store IR nodes.
 * @author      Michael Beck
 * @version     $Id$
 *
 * In some algorithms we want a more deterministic behavior
 * which the pset_ptr did not guarantee due to it's hash function
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "benodesets.h"
#include "irnode_t.h"

/*
 * Calculates a hash value for a node.
 *
 * Use its node number
 */
unsigned nodeset_hash(const ir_node *n) {
	return (unsigned)get_irn_idx(n);
}
