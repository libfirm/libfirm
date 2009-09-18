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
 * @brief       Register param constraints and some other register handling tools.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_MAP_REGS_H
#define FIRM_BE_IA32_IA32_MAP_REGS_H

#include "irnode.h"
#include "irmode.h"
#include "set.h"

#include "../bearch.h"
#include "bearch_ia32_t.h"
#include "gen_ia32_regalloc_if.h"

/**
 * Enters for each general purpose register the corresponding 16bit
 * name into a pmap.
 */
void ia32_build_16bit_reg_map(pmap *reg_map);

/**
 * Enters for each general purpose register the corresponding 8bit
 * name into a pmap.
 */
void ia32_build_8bit_reg_map(pmap *reg_map);
void ia32_build_8bit_reg_map_high(pmap *reg_map);

/**
 * Returns the corresponding mapped name for a register.
 */
const char *ia32_get_mapped_reg_name(pmap *reg_map, const arch_register_t *reg);

#endif
