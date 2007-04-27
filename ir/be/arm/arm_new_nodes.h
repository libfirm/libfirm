/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

#ifndef _ARM_NEW_NODES_H_
#define _ARM_NEW_NODES_H_

/**
 * Function prototypes for the assembler ir node constructors.
 * $Id$
 */

#include "arm_nodes_attr.h"
#include "bearch_arm_t.h"

/***************************************************************************************************
 *        _   _                   _       __        _                    _   _               _
 *       | | | |                 | |     / /       | |                  | | | |             | |
 *   __ _| |_| |_ _ __   ___  ___| |_   / /_ _  ___| |_   _ __ ___   ___| |_| |__   ___   __| |___
 *  / _` | __| __| '__| / __|/ _ \ __| / / _` |/ _ \ __| | '_ ` _ \ / _ \ __| '_ \ / _ \ / _` / __|
 * | (_| | |_| |_| |    \__ \  __/ |_ / / (_| |  __/ |_  | | | | | |  __/ |_| | | | (_) | (_| \__ \
 *  \__,_|\__|\__|_|    |___/\___|\__/_/ \__, |\___|\__| |_| |_| |_|\___|\__|_| |_|\___/ \__,_|___/
 *                                        __/ |
 *                                       |___/
 ***************************************************************************************************/

/**
 * Returns the attributes of an arm node.
 */
arm_attr_t *get_arm_attr(const ir_node *node);

/**
 * Returns the argument register requirements of an arm node.
 */
const arch_register_req_t **get_arm_in_req_all(const ir_node *node);

/**
 * Returns the result register requirements of an arm node.
 */
const arch_register_req_t **get_arm_out_req_all(const ir_node *node);

/**
 * Returns the argument register requirements of an arm node.
 */
const arch_register_req_t *get_arm_in_req(const ir_node *node, int pos);

/**
 * Returns the result register requirements of an arm node.
 */
const arch_register_req_t *get_arm_out_req(const ir_node *node, int pos);

/**
 * Sets the OUT register requirements at position pos.
 */
void set_arm_req_out(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Sets the complete OUT requirements of node.
 */
void set_arm_req_out_all(ir_node *node, const arch_register_req_t **reqs);

/**
 * Sets the IN register requirements at position pos.
 */
void set_arm_req_in(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Returns the register flag of an arm node.
 */
arch_irn_flags_t get_arm_flags(const ir_node *node);

/**
 * Sets the register flag of an arm node.
 */
void set_arm_flags(const ir_node *node, arch_irn_flags_t flags);

/**
 * Returns the result register slots of an arm node.
 */
const arch_register_t **get_arm_slots(const ir_node *node);

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_arm_out_reg_name(const ir_node *node, int pos);

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_arm_out_regnr(const ir_node *node, int pos);

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_arm_out_reg(const ir_node *node, int pos);

/**
 * Sets the number of results.
 */
void set_arm_n_res(ir_node *node, int n_res);

/**
 * Returns the number of results.
 */
int get_arm_n_res(const ir_node *node);

/**
 * Set the ARM machine node attributes to default values.
 */
void init_arm_attributes(ir_node *node, int flags, const arch_register_req_t ** in_reqs,
                         const arch_register_req_t ** out_reqs, const be_execution_unit_t ***execution_units, int n_res, unsigned latency);

/**
 * Returns the tarval
 */
tarval *get_arm_value(const ir_node *node);

/**
 * Sets the tarval
 */
void set_arm_value(ir_node *node, tarval *tv);

/**
 * Returns the proj num
 */
int get_arm_proj_num(const ir_node *node);

/**
 * Sets the proj num
 */
void set_arm_proj_num(ir_node *node, int proj_num);

const char *get_arm_symconst_label(ir_node *node);
void set_arm_symconst_label(ir_node *node, const char *symconst_label);

ir_node *new_r_arm_StoreStackMInc(ir_graph *irg, ir_node *block, ir_node *mem, ir_node *sp,
							      int n_regs, ir_node **regs, ir_mode *mode);

/**
 * Returns the number of projs.
 */
int get_arm_n_projs(ir_node *node);

/**
 * Sets the number of projs.
 */
void set_arm_n_projs(ir_node *node, int n_projs);

/**
 * Returns the default_proj_num.
 */
long get_arm_default_proj_num(ir_node *node);

/**
 * Sets the default_proj_num.
 */
void set_arm_default_proj_num(ir_node *node, long default_proj_num);

/**
 * Gets the shift modifier attribute.
 */
arm_shift_modifier get_arm_shift_modifier(ir_node *node);

/**
 * Decode an immediate with shifter operand
 */
unsigned int arm_decode_imm_w_shift(tarval *tv);

/* Include the generated headers */
#include "gen_arm_new_nodes.h"

#endif /* _ARM_NEW_NODES_H_ */
