#ifndef _mips_MAP_REGS_H_
#define _mips_MAP_REGS_H_

#include "irnode.h"
#include "set.h"

#include "../bearch.h"
#include "mips_nodes_attr.h"

int  mips_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void mips_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *mips_get_firm_reg(const ir_node *irn, set *reg_set);

long mips_translate_proj_pos(const ir_node *proj);

#endif /* _mips_MAP_REGS_H_ */
