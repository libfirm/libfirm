#ifndef _arm_MAP_REGS_H_

#define _arm_MAP_REGS_H_



#include "irnode.h"

#include "set.h"



#include "../bearch_t.h"

#include "arm_nodes_attr.h"



const arch_register_t *arm_get_RegParam_reg(int n);



int  arm_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);

void arm_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);

const arch_register_t *arm_get_firm_reg(const ir_node *irn, set *reg_set);



long arm_translate_proj_pos(const ir_node *proj);



#endif /* _arm_MAP_REGS_H_ */
