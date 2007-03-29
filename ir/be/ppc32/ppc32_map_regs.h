#ifndef _PPC32_MAP_REGS_H_
#define _PPC32_MAP_REGS_H_

#include "irnode.h"
#include "set.h"

#include "../bearch.h"
#include "ppc32_nodes_attr.h"

int  ppc32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void ppc32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *ppc32_get_firm_reg(const ir_node *irn, set *reg_set);

long ppc32_translate_proj_pos(const ir_node *proj);

#endif /* _PPC32_MAP_REGS_H_ */
