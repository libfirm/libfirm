/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Special iCore Perm lowering
 * @author   Manuel Mohr
 */

#ifndef FIRM_BE_SPARC_ICORE_LOWERPERM_H
#define FIRM_BE_SPARC_ICORE_LOWERPERM_H

#include "beirg.h"

/**
 * Walks over all blocks in an irg and performs lowering that needs to be
 * done after register allocation (e.g. perm lowering).  Differs from the
 * default implementation in belower.c in that it creates bigger Perm
 * nodes because the iCore can handle them using one special instruction.
 *
 * @param irg       The graph
 */
void icore_lower_nodes_after_ra(ir_graph *irg);

#endif
