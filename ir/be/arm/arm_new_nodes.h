/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Function prototypes for the assembler ir node constructors.
 * @author  Oliver Richter, Tobias Gneist
 * @version $Id$
 */
#ifndef FIRM_BE_ARM_ARM_NEW_NODES_H
#define FIRM_BE_ARM_ARM_NEW_NODES_H

#include "arm_nodes_attr.h"
#include "bearch_arm_t.h"

/**
 * Returns the attributes of a generic Arm node.
 */
arm_attr_t *get_arm_attr(ir_node *node);
const arm_attr_t *get_arm_attr_const(const ir_node *node);

/**
 * Returns the attributes of an ARM SymConst node.
 */
arm_SymConst_attr_t *get_arm_SymConst_attr(ir_node *node);
const arm_SymConst_attr_t *get_arm_SymConst_attr_const(const ir_node *node);

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

ir_node *new_r_arm_StoreStackMInc(ir_graph *irg, ir_node *block, ir_node *mem, ir_node *sp,
							      int n_regs, ir_node **regs, ir_mode *mode);

/* Include the generated headers */
#include "gen_arm_new_nodes.h"

#endif
