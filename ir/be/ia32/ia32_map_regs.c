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


void ia32_build_16bit_reg_map(pmap *reg_map) {
	pmap_insert(reg_map, &ia32_gp_regs[REG_EAX], "ax");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EBX], "bx");
	pmap_insert(reg_map, &ia32_gp_regs[REG_ECX], "cx");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EDX], "dx");
	pmap_insert(reg_map, &ia32_gp_regs[REG_ESI], "si");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EDI], "di");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EBP], "bp");
	pmap_insert(reg_map, &ia32_gp_regs[REG_ESP], "sp");
}

void ia32_build_8bit_reg_map(pmap *reg_map) {
	pmap_insert(reg_map, &ia32_gp_regs[REG_EAX], "al");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EBX], "bl");
	pmap_insert(reg_map, &ia32_gp_regs[REG_ECX], "cl");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EDX], "dl");
}

void ia32_build_8bit_reg_map_high(pmap *reg_map) {
	pmap_insert(reg_map, &ia32_gp_regs[REG_EAX], "ah");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EBX], "bh");
	pmap_insert(reg_map, &ia32_gp_regs[REG_ECX], "ch");
	pmap_insert(reg_map, &ia32_gp_regs[REG_EDX], "dh");
}

const char *ia32_get_mapped_reg_name(pmap *reg_map, const arch_register_t *reg) {
	pmap_entry *e = pmap_find(reg_map, (void *)reg);

	//assert(e && "missing map init?");
	if (! e) {
		printf("FIXME: ia32map_regs.c:122: returning fake register name for ia32 with 32 register\n");
		return reg->name;
	}

	return e->value;
}
