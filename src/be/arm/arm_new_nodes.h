/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Function prototypes for the assembler ir node constructors.
 * @author  Oliver Richter, Tobias Gneist
 */
#ifndef FIRM_BE_ARM_ARM_NEW_NODES_H
#define FIRM_BE_ARM_ARM_NEW_NODES_H

#include "arm_nodes_attr.h"

/**
 * Returns the attributes of a generic Arm node.
 */
arm_attr_t *get_arm_attr(ir_node *node);
const arm_attr_t *get_arm_attr_const(const ir_node *node);

/**
 * Returns the attributes of an ARM Address node.
 */
arm_Address_attr_t *get_arm_Address_attr(ir_node *node);
const arm_Address_attr_t *get_arm_Address_attr_const(const ir_node *node);

/**
 * Returns the attributes of an ARM CondJmp node.
 */
arm_CondJmp_attr_t *get_arm_CondJmp_attr(ir_node *node);
const arm_CondJmp_attr_t *get_arm_CondJmp_attr_const(const ir_node *node);

/**
 * Returns the attributes of an ARM SwitchJmp node.
 */
arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr(ir_node *node);
const arm_SwitchJmp_attr_t *get_arm_SwitchJmp_attr_const(const ir_node *node);

arm_load_store_attr_t *get_arm_load_store_attr(ir_node *node);
const arm_load_store_attr_t *get_arm_load_store_attr_const(const ir_node *node);

arm_shifter_operand_t *get_arm_shifter_operand_attr(ir_node *node);
const arm_shifter_operand_t *get_arm_shifter_operand_attr_const(const ir_node *node);

arm_cmp_attr_t *get_arm_cmp_attr(ir_node *node);
const arm_cmp_attr_t *get_arm_cmp_attr_const(const ir_node *node);

arm_farith_attr_t *get_arm_farith_attr(ir_node *node);
const arm_farith_attr_t *get_arm_farith_attr_const(const ir_node *node);

arm_CopyB_attr_t *get_arm_CopyB_attr(ir_node *node);
const arm_CopyB_attr_t *get_arm_CopyB_attr_const(const ir_node *node);

/**
 * Return the tarval of a fConst
 */
ir_tarval *get_fConst_value(const ir_node *node);

/**
 * Sets the tarval of a fConst
 */
void set_fConst_value(ir_node *node, ir_tarval *tv);

/**
 * Returns the compare kind
 */
ir_relation get_arm_CondJmp_relation(const ir_node *node);

/**
 * Set compare type
 */
void set_arm_CondJmp_relation(ir_node *node, ir_relation relation);

/* Include the generated headers */
#include "gen_arm_new_nodes.h"

#endif
