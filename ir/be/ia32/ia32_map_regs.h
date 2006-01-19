#ifndef _IA32_MAP_REGS_H_
#define _IA32_MAP_REGS_H_

#include "irnode.h"
#include "set.h"

#include "../bearch.h"

int  ia32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void ia32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *ia32_get_firm_reg(const ir_node *irn, set *reg_set);

int  ia32_cmp_reg_projnum_assoc(const void *a, const void *b, size_t len);
void ia32_set_reg_projnum(const arch_register_t *reg, long proj_num, set *reg_set);
long ia32_get_reg_projnum(const arch_register_t *reg, set *reg_set);

long ia32_translate_proj_pos(const ir_node *proj);

#endif /* _IA32_MAP_REGS_H_ */
