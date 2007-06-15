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
 * @brief   Register mapping for firm nodes. Stolen from bearch_firm :)
 * @author  Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "arm_map_regs.h"
#include "arm_new_nodes.h"

#include "gen_arm_regalloc_if.h"


static const arch_register_t *gpreg_param_reg_std[] = {
	&arm_gp_regs[REG_R0],
	&arm_gp_regs[REG_R1],
	&arm_gp_regs[REG_R2],
	&arm_gp_regs[REG_R3],
};

const arch_register_t *arm_get_RegParam_reg(int n) {
	assert(n < 4 && n >=0 && "register param > 3 angefordert");
	return gpreg_param_reg_std[n];
}

/* Mapping to store registers in firm nodes */

struct arm_irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int arm_cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct arm_irn_reg_assoc *x = a;
	const struct arm_irn_reg_assoc *y = b;

	return x->irn != y->irn;
}

static struct arm_irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct arm_irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = HASH_PTR(irn);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void arm_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set) {
	struct arm_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	assoc->reg = reg;
}

const arch_register_t *arm_get_firm_reg(const ir_node *irn, set *reg_set) {
	const struct arm_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}
