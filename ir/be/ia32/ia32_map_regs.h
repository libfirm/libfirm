#ifndef _IA32_MAP_REGS_H_
#define _IA32_MAP_REGS_H_

#include "irnode.h"
#include "set.h"

#include "../bearch.h"

void ia32_set_firm_reg(const arch_irn_ops_t *self, ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *ia32_get_firm_reg(const arch_irn_ops_t *self, const ir_node *irn, set *reg_set);
int cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
long translate_proj_pos(const ir_node *proj);

#endif /* _IA32_MAP_REGS_H_ */
