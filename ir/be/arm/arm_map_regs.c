/**
 * Register mapping for firm nodes. Stolen from bearch_firm :)
 * $Id$
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
	struct arm_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}



/**
 * Translates the projnum into a "real" argument position for register
 * requirements dependend on the predecessor.
 */
long arm_translate_proj_pos(const ir_node *proj) {
	ir_node *pred = get_Proj_pred(proj);
	long nr       = get_Proj_proj(proj);

	if (is_arm_Load(pred) || is_arm_fLoad(pred)) {
		if (nr == pn_Load_res)
			return 0;
		assert(0 && "unsupported Proj(Load) number");
	}
	else if (is_arm_Store(pred) || is_arm_fStore(pred)) {
		return 0;
	}
	else if (is_arm_fDiv(pred)) {
		if (nr == pn_Quot_res)
			return 0;
		else
			assert(0 && "there should be no more Projs for a fDiv");
	}

//	assert(0 && "unsupported Proj(X)");
	return nr;
}
