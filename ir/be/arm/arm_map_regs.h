/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief  declarations for ARM register allocation
 * @author Oliver Richter, Tobias Gneist, Michael Beck
 */
#ifndef FIRM_BE_ARM_ARM_MAP_REGS_H
#define FIRM_BE_ARM_ARM_MAP_REGS_H

#include "irnode.h"
#include "set.h"

#include "bearch.h"
#include "arm_nodes_attr.h"

const arch_register_t *arm_get_RegParam_reg(int n);

int  arm_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void arm_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *arm_get_firm_reg(const ir_node *irn, set *reg_set);

#endif
