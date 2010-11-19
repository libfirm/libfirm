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
#include "config.h"

#include <stdlib.h>

#include "pmap.h"
#include "error.h"

#include "ia32_map_regs.h"
#include "ia32_new_nodes.h"
#include "ia32_architecture.h"
#include "gen_ia32_regalloc_if.h"
#include "bearch_ia32_t.h"

/* this is the order of the assigned registers used for parameter passing */


void ia32_build_16bit_reg_map(const char *reg_map[])
{
	memset(reg_map, 0, sizeof(reg_map[0]) * N_ia32_gp_REGS);
	reg_map[REG_GP_EAX] = "ax";
	reg_map[REG_GP_EBX] = "bx";
	reg_map[REG_GP_ECX] = "cx";
	reg_map[REG_GP_EDX] = "dx";
	reg_map[REG_GP_ESI] = "si";
	reg_map[REG_GP_EDI] = "di";
	reg_map[REG_GP_EBP] = "bp";
	reg_map[REG_GP_ESP] = "sp";
}

void ia32_build_8bit_reg_map(const char *reg_map[])
{
	memset(reg_map, 0, sizeof(reg_map[0]) * N_ia32_gp_REGS);
	reg_map[REG_GP_EAX] = "al";
	reg_map[REG_GP_EBX] = "bl";
	reg_map[REG_GP_ECX] = "cl";
	reg_map[REG_GP_EDX] = "dl";
}

void ia32_build_8bit_reg_map_high(const char *reg_map[])
{
	memset(reg_map, 0, sizeof(reg_map[0]) * N_ia32_gp_REGS);
	reg_map[REG_GP_EAX], "ah";
	reg_map[REG_GP_EBX], "bh";
	reg_map[REG_GP_ECX], "ch";
	reg_map[REG_GP_EDX], "dh";
}

const char *ia32_get_mapped_reg_name(const char *reg_map[], const arch_register_t *reg)
{
	const char *name = reg_map[reg->index];
	assert(reg->reg_class->index == CLASS_ia32_gp);

	//assert(name && "missing map init?");
	if (! name) {
		printf("FIXME: ia32map_regs.c:122: returning fake register name for ia32 with 32 register\n");
		return reg->name;
	}

	return name;
}
