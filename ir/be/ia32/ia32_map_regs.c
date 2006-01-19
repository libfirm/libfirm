/**
 * Register mapping for firm nodes. Stolen from bearch_firm :)
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "ia32_map_regs.h"
#include "ia32_new_nodes.h"


/* Mapping to store registers in firm nodes */

struct ia32_irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int ia32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct ia32_irn_reg_assoc *x = a;
	const struct ia32_irn_reg_assoc *y = b;

	return !(x->irn == y->irn);
}

static struct ia32_irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct ia32_irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = HASH_PTR(irn);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void ia32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set) {
	struct ia32_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	assoc->reg = reg;
}

const arch_register_t *ia32_get_firm_reg(const ir_node *irn, set *reg_set) {
	struct ia32_irn_reg_assoc *assoc = get_irn_reg_assoc(irn, reg_set);
	return assoc->reg;
}



/* Mapping to store proj numbers for registers */

struct ia32_reg_projnum_assoc {
	const arch_register_t *reg;
	long                   proj_num;
};

int ia32_cmp_reg_projnum_assoc(const void *a, const void *b, size_t len) {
	const struct ia32_reg_projnum_assoc *x = a;
	const struct ia32_reg_projnum_assoc *y = b;

	return !(x->reg == y->reg);
}

static struct ia32_reg_projnum_assoc *get_reg_projnum_assoc(const arch_register_t *reg, set *reg_set) {
	struct ia32_reg_projnum_assoc templ;
	unsigned int hash;

	templ.reg      = reg;
	templ.proj_num = -1;
	hash = HASH_PTR(reg);

	return set_insert(reg_set, &templ, sizeof(templ), hash);
}

void ia32_set_reg_projnum(const arch_register_t *reg, long proj_num, set *reg_set) {
	struct ia32_reg_projnum_assoc *assoc = get_reg_projnum_assoc(reg, reg_set);
	assoc->proj_num = proj_num;
}

long ia32_get_reg_projnum(const arch_register_t *reg, set *reg_set) {
	struct ia32_reg_projnum_assoc *assoc = get_reg_projnum_assoc(reg, reg_set);
	return assoc->proj_num;
}



/**
 * Translates the projnum into a "real" argument position for register
 * requirements dependend on the predecessor.
 */
long ia32_translate_proj_pos(const ir_node *proj) {
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
