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
 * @brief       Performs lowering of perm nodes. Inserts copies to assure register constraints.
 * @author      Christian Wuerdig
 * @date        14.12.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BELOWER_H
#define FIRM_BE_BELOWER_H

#include "beirg.h"

void assure_constraints(be_irg_t *birg);
void lower_nodes_after_ra(be_irg_t *birg, int do_copy);

#endif /* FIRM_BE_BELOWER_H */
