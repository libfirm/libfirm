/**
 * Register mapping for firm nodes. Stolen from bearch_firm :)
 * Calculate requirements for register parameter.
 * @author Christian Wuerdig
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "pmap.h"

#include "ia32_map_regs.h"
#include "ia32_new_nodes.h"
#include "gen_ia32_regalloc_if.h"
#include "../benodesets.h"

static int maxnum_gpreg_args = 3;   /* maximum number of int arguments passed in registers; default 3 */
static int maxnum_fpreg_args = 5;   /* maximum number of float arguments passed in registers; default 5 */

/* this is the order of the assigned registers usesd for parameter passing */

const arch_register_t *gpreg_param_reg_std[] = {
	&ia32_gp_regs[REG_EAX],
	&ia32_gp_regs[REG_EDX],
	&ia32_gp_regs[REG_ECX],
	&ia32_gp_regs[REG_EBX],
	&ia32_gp_regs[REG_EDI],
	&ia32_gp_regs[REG_ESI]
};

const arch_register_t *gpreg_param_reg_this[] = {
	&ia32_gp_regs[REG_ECX],
	&ia32_gp_regs[REG_EAX],
	&ia32_gp_regs[REG_EDX],
	&ia32_gp_regs[REG_EBX],
	&ia32_gp_regs[REG_EDI],
	&ia32_gp_regs[REG_ESI]
};

const arch_register_t *fpreg_param_reg_std[] = {
	&ia32_xmm_regs[REG_XMM0],
	&ia32_xmm_regs[REG_XMM1],
	&ia32_xmm_regs[REG_XMM2],
	&ia32_xmm_regs[REG_XMM3],
	&ia32_xmm_regs[REG_XMM4],
	&ia32_xmm_regs[REG_XMM5],
	&ia32_xmm_regs[REG_XMM6],
	&ia32_xmm_regs[REG_XMM7]
};

const arch_register_t *fpreg_param_reg_this[] = {
	NULL,  /* in case of a "this" pointer, the first parameter must not be a float */
	&ia32_xmm_regs[REG_XMM0],
	&ia32_xmm_regs[REG_XMM1],
	&ia32_xmm_regs[REG_XMM2],
	&ia32_xmm_regs[REG_XMM3],
	&ia32_xmm_regs[REG_XMM4],
	&ia32_xmm_regs[REG_XMM5],
	&ia32_xmm_regs[REG_XMM6],
	&ia32_xmm_regs[REG_XMM7]
};



/* Mapping to store registers in firm nodes */

struct ia32_irn_reg_assoc {
	const ir_node *irn;
	const arch_register_t *reg;
};

int ia32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len) {
	const struct ia32_irn_reg_assoc *x = a;
	const struct ia32_irn_reg_assoc *y = b;

	return x->irn != y->irn;
}

static struct ia32_irn_reg_assoc *get_irn_reg_assoc(const ir_node *irn, set *reg_set) {
	struct ia32_irn_reg_assoc templ;
	unsigned int hash;

	templ.irn = irn;
	templ.reg = NULL;
	hash = nodeset_hash(irn);

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

const char *ia32_get_mapped_reg_name(pmap *reg_map, const arch_register_t *reg) {
	pmap_entry *e = pmap_find(reg_map, (void *)reg);

	//assert(e && "missing map init?");
	if (! e) {
		printf("FIXME: ia32map_regs.c:122: returning fake register name for ia32 with 32 register\n");
		return reg->name;
	}

	return e->value;
}

/**
 * Check all parameters and determine the maximum number of parameters
 * to pass in gp regs resp. in fp regs.
 *
 * @param n       The number of parameters
 * @param modes   The list of the parameter modes
 * @param n_int   Holds the number of int parameters to be passed in regs after the call
 * @param n_float Holds the number of float parameters to be passed in regs after the call
 * @return        The number of the last parameter to be passed in register
 */
int ia32_get_n_regparam_class(int n, ir_mode **modes, int *n_int, int *n_float) {
	int i, finished = 0;

	*n_int   = 0;
	*n_float = 0;

	for (i = 0; i < n && !finished; i++) {
		if (mode_is_int(modes[i]) || mode_is_reference(modes[i])) {
			*n_int = *n_int + 1;
		}
		else if (mode_is_float(modes[i])) {
			*n_float = *n_float + 1;
		}
		else {
			finished = 1;
		}

		/* test for maximum */
		if (*n_int == maxnum_gpreg_args || *n_float == maxnum_fpreg_args) {
			finished = 1;
		}
	}

	return i - 1;
}


/**
 * Returns the register for parameter nr.
 *
 * @param n     The number of parameters
 * @param modes The list of the parameter modes
 * @param nr    The number of the parameter to return the requirements for
 * @param cc    The calling convention
 * @return      The register
 */
const arch_register_t *ia32_get_RegParam_reg(int n, ir_mode **modes, long nr, unsigned cc) {
	const arch_register_t **current_gpreg_param_reg;
	const arch_register_t **current_fpreg_param_reg;
	const arch_register_t  *param_reg = NULL;
	int n_gpregparam = 0;
	int n_fpregparam = 0;
	int i, done      = 0;
	int cur_gp_idx   = 0;
	int cur_fp_idx   = 0;
	int biggest_n    = ia32_get_n_regparam_class(n, modes, &n_gpregparam, &n_fpregparam);

	/* Check if parameter #nr is in range for passing in register */
	if (nr <= biggest_n) {
		current_gpreg_param_reg = gpreg_param_reg_std;
		current_fpreg_param_reg = fpreg_param_reg_std;

		if (cc & cc_this_call) {
			current_gpreg_param_reg = gpreg_param_reg_this;
			current_fpreg_param_reg = fpreg_param_reg_this;
		}

		/* loop over all parameters and determine whether its a int or float register parameter */
		for (i = 0; i < nr && !done && (cc & cc_reg_param); i++) {
			if ((mode_is_int(modes[i]) || mode_is_reference(modes[i])) && cur_gp_idx < maxnum_gpreg_args) {
				/* param can be passed in general purpose register and we have some registers left */
				cur_gp_idx++;
			}
			else if (mode_is_float(modes[i]) && cur_fp_idx < maxnum_fpreg_args) {
				/* param can be passed in floating point register and we have some registers left */
				assert(current_gpreg_param_reg[cur_fp_idx] && "'this' pointer cannot be passed as float");
				cur_fp_idx++;
			}
		}

		/* now: i == nr, that's the parameter requirement we want */
		if ((mode_is_int(modes[i]) || mode_is_reference(modes[i])) && cur_gp_idx < maxnum_gpreg_args) {
			/* parameter #nr can be passed in general purpose register */
			param_reg = current_gpreg_param_reg[i];
		}
		else if (mode_is_float(modes[i]) && cur_fp_idx < maxnum_fpreg_args) {
			/* parameter #nr can be passed in floating point register */
			param_reg = current_fpreg_param_reg[i];
		}
		else {
			assert(0 && "This should not happen!");
		}
	}

	return param_reg;
}
