/**
 * Register mapping for firm nodes. Stolen from bearch_firm :)
 * $Id$
 */

#include <stdlib.h>

#include "ia32_map_regs.h"
#include "ia32_new_nodes.h"

struct irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct irn_reg_assoc *x = a;
	const struct irn_reg_assoc *y = b;

	return !(x->irn == y->irn);
}

static struct irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = HASH_PTR(irn);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void ia32_set_firm_reg(const arch_irn_ops_t *self, ir_node *irn,
    const arch_register_t *reg, set *reg_set)
{
	struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	assoc->reg = reg;
}

const arch_register_t *ia32_get_firm_reg(const arch_irn_ops_t *self,
    const ir_node *irn, set *reg_set)
{
	struct irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}


/**
 * Translates the projnum into a "real" argument position for register
 * requirements dependend on the predecessor.
 */
long translate_proj_pos(const ir_node *proj) {
	ir_node *first;
	ir_node *pred = get_Proj_pred(proj);
	long nr       = get_Proj_proj(proj);

	if (is_ia32_Load(pred)) {
		if (nr == pn_Load_res)
			return 0;
		assert(0 && "unsupported Proj(Load) number");
	}
	else if (is_ia32_Store(pred)) {
		return 0;
	}
	else if (is_ia32_CondJmp(pred) || is_ia32_CondJmp_i(pred)) {
		return 0;
	}
	else if (is_ia32_SwitchJmp(pred)) {
		return 0;
	}
	else if (is_ia32_Cltd(pred) || is_ia32_Mul(pred)) {
		if (nr == pn_EAX)
			return 0;
		if (nr == pn_EDX)
			return 1;
	}
	else if (is_ia32_DivMod(pred)) {
		if (nr == pn_DivMod_res_div || pn_Div_res)
			return 0;
		if (nr == pn_DivMod_res_mod || pn_Mod_res)
			return 1;
	}
	else if (is_ia32_Call(pred)) {
		return 0;
	}
	else if (get_irn_mode(proj) == mode_X && nr == pn_Start_X_initial_exec) {
		return 0;
	}
	else if (is_Proj(pred)) {
//		return nr + translate_proj_pos(pred);
		first = get_Proj_pred(pred);

		if (is_ia32_Call(first))
			return 0;

		assert(0 && "unsupported proj-pos translation Proj(Proj)");
		return -1;
	}
	else if (get_irn_opcode(pred) == iro_Start) {
		return nr;
	}

//	assert(0 && "unsupported Proj(X)");
	return nr;
}
