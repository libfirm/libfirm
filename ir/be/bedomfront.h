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
 * @brief       Algorithms for computing normal and iterated dominance frontiers
 * @author      Sebastian Hack, Daniel Grund
 * @date:       04.05.2005
 * @version     $Id$
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _FIRM_BE_DOMFRONT_H_
#define _FIRM_BE_DOMFRONT_H_

#include "irnodeset.h"

/*
 * Forward type declaration.
 */
typedef struct _be_dom_front_info_t be_dom_front_info_t;

/**
 * Compute the dominance frontiers for a given graph.
 * @param  irg The graphs.
 * @return A pointer to the dominance frontier information.
 */
be_dom_front_info_t *be_compute_dominance_frontiers(ir_graph *irg);

/**
 * Free some dominance frontier information.
 * @param info Some dominance frontier information.
 */
void be_free_dominance_frontiers(be_dom_front_info_t *info);

/**
 * Get the dominance frontier of a block.
 * @param info 	A pointer to the dominance frontier information.
 * @param block The block whose dominance frontier you want.
 * @return A list containing the all blocks in the dominance frontier of @p block.
 */
ir_node **be_get_dominance_frontier(const be_dom_front_info_t *info,
                                    ir_node *block);

#endif
