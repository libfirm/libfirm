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
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "ppc32_map_regs.h"
#include "ppc32_new_nodes.h"

/* Mapping to store registers in firm nodes */

struct ppc32_irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int ppc32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct ppc32_irn_reg_assoc *x = a;
	const struct ppc32_irn_reg_assoc *y = b;
	(void) len;

	return x->irn != y->irn;
}

static struct ppc32_irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct ppc32_irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = HASH_PTR(irn);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void ppc32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set) {
	struct ppc32_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	assoc->reg = reg;
}

const arch_register_t *ppc32_get_firm_reg(const ir_node *irn, set *reg_set) {
	struct ppc32_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}


int is_ppc32_Load(const ir_node *n)
{
	ir_op *op = get_irn_op(n);
	if(op == op_ppc32_Lbz) return 1;
	if(op == op_ppc32_Lhz) return 1;
	if(op == op_ppc32_Lha) return 1;
	if(op == op_ppc32_Lwz) return 1;
	if(op == op_ppc32_Lfd) return 1;
	if(op == op_ppc32_Lfs) return 1;

	return 0;
}

int is_ppc32_Store(const ir_node *n)
{
	ir_op *op = get_irn_op(n);
	if(op == op_ppc32_Stb) return 1;
	if(op == op_ppc32_Sth) return 1;
	if(op == op_ppc32_Stw) return 1;
	if(op == op_ppc32_Stfd) return 1;
	if(op == op_ppc32_Stfs) return 1;

	return 0;
}


/**
 * Translates the projnum into a "real" argument position for register
 * requirements dependend on the predecessor.
 */
long ppc32_translate_proj_pos(const ir_node *proj) {
	ir_node *pred = get_Proj_pred(proj);
	long nr       = get_Proj_proj(proj);


	if (is_ppc32_Load(pred)) {
		if (nr == pn_Load_res)
			return 0;
		assert(0 && "unsupported Proj(Load) number");
	}
	else if (is_ppc32_Store(pred)) {
		return 0;
	}
	else if (is_ppc32_fDiv(pred)) {
		if (nr == pn_Quot_res)
			return 0;
		else
			assert(0 && "there should be no more Projs for a fDiv");
	}
	else if (is_ppc32_Divw(pred) || is_ppc32_Divwu(pred)) {
		if (nr == pn_DivMod_res_div)
			return 0;
		else
			assert(0 && "there should be no more Projs for a ppc32_Divw or ppc32_Divwu");
	}

	else if(is_ppc32_Cmp(pred))
		return 0;
	else if(is_ppc32_Cmpi(pred))
		return 0;
	else if(is_ppc32_Cmpl(pred))
		return 0;
	else if(is_ppc32_Cmpli(pred))
		return 0;



//	assert(0 && "unsupported Proj(X)");
	return nr;
}
