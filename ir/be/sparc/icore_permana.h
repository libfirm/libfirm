/*
 * Copyright (C) 1995-2012 University of Karlsruhe.  All right reserved.
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
 * @brief    Analyze Perm node to identify cycle and chain components
 * @author   Manuel Mohr
 */

#ifndef FIRM_BE_SPARC_ICORE_PERMANA_H
#define FIRM_BE_SPARC_ICORE_PERMANA_H

#include "irnode.h"
#include "gen_sparc_regalloc_if.h"

#define NUM_REGISTERS N_sparc_gp_REGS

/* The type of a Perm (sub-)operation. */
typedef enum perm_op_type_t {
	PERM_OP_CHAIN,
	PERM_OP_CYCLE
} perm_op_type_t;

/* A Perm operation. */
typedef struct perm_op_t {
	perm_op_type_t type;
	unsigned       regs[NUM_REGISTERS];
	unsigned       length;
} perm_op_t;


void print_perm_op(const perm_op_t *op);
unsigned analyze_perm(const ir_node *perm, perm_op_t *ops);

#endif
