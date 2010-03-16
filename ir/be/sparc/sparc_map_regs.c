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
 * @brief   Register mapping for firm nodes. Stolen from bearch_firm :)
 * @author  Hannes Rapp
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "sparc_map_regs.h"
#include "sparc_new_nodes.h"

#include "gen_sparc_regalloc_if.h"


static const arch_register_t *gp_param_regs[] = {
	&sparc_gp_regs[REG_O0],
	&sparc_gp_regs[REG_O1],
	&sparc_gp_regs[REG_O2],
	&sparc_gp_regs[REG_O3],
	&sparc_gp_regs[REG_O4],
	&sparc_gp_regs[REG_O5],
};


const arch_register_t *sparc_get_RegParam_reg(int n)
{
	assert(n < 6 && n >=0 && "trying to get register for param >= 6");
	return gp_param_regs[n];
}
