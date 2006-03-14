#ifndef _IA32_MAP_REGS_H_
#define _IA32_MAP_REGS_H_

#include "irnode.h"
#include "set.h"

#include "../bearch.h"
#include "ia32_nodes_attr.h"

/**
 * Convenience macro to check if register <code>out<\code>
 * and register <code>in<\code> are equal.
 */
#define REGS_ARE_EQUAL(out, in) \
	((arch_register_get_class(out) == arch_register_get_class(in)) && \
	(arch_register_get_index(out) == arch_register_get_index(in)))


int  ia32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void ia32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *ia32_get_firm_reg(const ir_node *irn, set *reg_set);

int ia32_get_n_regparam_class(int n, ir_mode **modes, int *n_int, int *n_float);

const arch_register_t *ia32_get_RegParam_reg(int n, ir_mode **modes, long nr, unsigned cc);

long ia32_translate_proj_pos(const ir_node *proj);

#endif /* _IA32_MAP_REGS_H_ */
