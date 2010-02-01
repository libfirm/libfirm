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
 * @brief  declarations for SPARC register allocation
 * @author Hannes Rapp
 * @version $Id: sparc_map_regs.h 26317 2009-08-05 10:53:46Z matze $
 */
#ifndef FIRM_BE_SPARC_SPARC_MAP_REGS_H
#define FIRM_BE_SPARC_SPARC_MAP_REGS_H

#include "irnode.h"
#include "set.h"

#include "../bearch.h"
#include "sparc_nodes_attr.h"

const arch_register_t *sparc_get_RegParam_reg(int n);

#endif
