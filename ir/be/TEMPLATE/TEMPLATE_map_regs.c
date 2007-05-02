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
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "TEMPLATE_map_regs.h"
#include "TEMPLATE_new_nodes.h"

/* Mapping to store registers in firm nodes */

struct TEMPLATE_irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int TEMPLATE_cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct TEMPLATE_irn_reg_assoc *x = a;
	const struct TEMPLATE_irn_reg_assoc *y = b;

	return x->irn != y->irn;
}

static struct TEMPLATE_irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct TEMPLATE_irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = HASH_PTR(irn);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void TEMPLATE_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set) {
	struct TEMPLATE_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	assoc->reg = reg;
}

const arch_register_t *TEMPLATE_get_firm_reg(const ir_node *irn, set *reg_set) {
	struct TEMPLATE_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}



/**
 * Translates the projnum into a "real" argument position for register
 * requirements dependend on the predecessor.
 */
long TEMPLATE_translate_proj_pos(const ir_node *proj) {
	ir_node *pred = get_Proj_pred(proj);
	long nr       = get_Proj_proj(proj);

	if (is_TEMPLATE_Load(pred)) {
		if (nr == pn_Load_res)
			return 0;
		assert(0 && "unsupported Proj(Load) number");
	}
	else if (is_TEMPLATE_Store(pred)) {
		return 0;
	}
	else if (is_TEMPLATE_fDiv(pred)) {
		if (nr == pn_Quot_res)
			return 0;
		else
			assert(0 && "there should be no more Projs for a fDiv");
	}

//	assert(0 && "unsupported Proj(X)");
	return nr;
}
