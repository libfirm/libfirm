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

/**
 * @file
 * @brief   Function prototypes for the ppc32 assembler ir node constructors.
 * @author  Moritz Kroll, Jens Mueller
 * @version $Id$
 */
#ifndef FIRM_BE_PPC32_PPC32_NEW_NODES_H
#define FIRM_BE_PPC32_PPC32_NEW_NODES_H

#include "ppc32_nodes_attr.h"

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
 * Returns the attributes of an ppc node.
 */
ppc32_attr_t *get_ppc32_attr(const ir_node *node);

/**
 * Returns the argument register requirements of an ppc node.
 */
const arch_register_req_t **get_ppc32_in_req_all(const ir_node *node);

/**
 * Returns the result register requirements of an ppc node.
 */
const arch_register_req_t **get_ppc32_out_req_all(const ir_node *node);

/**
 * Returns the argument register requirements of an ppc node.
 */
const arch_register_req_t *get_ppc32_in_req(const ir_node *node, int pos);

/**
 * Returns the result register requirements of an ppc node.
 */
const arch_register_req_t *get_ppc32_out_req(const ir_node *node, int pos);

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ppc32_req_out(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Sets the IN register requirements at position pos.
 */
void set_ppc32_req_in(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Returns the register flag of an ppc node.
 */
arch_irn_flags_t get_ppc32_flags(const ir_node *node);

/**
 * Sets the register flag of an ppc node.
 */
void set_ppc32_flags(const ir_node *node, arch_irn_flags_t flags);

/**
 * Returns the result register slots of an ppc node.
 */
const arch_register_t **get_ppc32_slots(const ir_node *node);

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ppc32_out_reg_name(const ir_node *node, int pos);

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ppc32_out_regnr(const ir_node *node, int pos);

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ppc32_out_reg(const ir_node *node, int pos);

/**
 * Sets the number of results.
 */
void set_ppc32_n_res(ir_node *node, int n_res);

/**
 * Returns the number of results.
 */
int get_ppc32_n_res(const ir_node *node);

ppc32_attr_content_type get_ppc32_type(const ir_node *node);

void set_ppc32_constant_tarval(const ir_node *node, tarval *const_tarval);
tarval *get_ppc32_constant_tarval(const ir_node *node);

void set_ppc32_symconst_ident(const ir_node *node, ident *symconst_ident);
ident *get_ppc32_symconst_ident(const ir_node *node);

void set_ppc32_frame_entity(const ir_node *node, ir_entity *ent);
ir_entity *get_ppc32_frame_entity(const ir_node *node);

void set_ppc32_rlwimi_const(const ir_node *node, unsigned shift, unsigned maskA, unsigned maskB);
rlwimi_const_t *get_ppc32_rlwimi_const(const ir_node *node);

void set_ppc32_proj_nr(const ir_node *node, int proj_nr);
int get_ppc32_proj_nr(const ir_node *node);

void set_ppc32_offset(const ir_node *node, int offset);
int get_ppc32_offset(const ir_node *node);

void set_ppc32_offset_mode(const ir_node *node, ppc32_attr_offset_mode mode);
ppc32_attr_offset_mode get_ppc32_offset_mode(const ir_node *node);

void init_ppc32_attributes(ir_node *node, int flags,
						 const arch_register_req_t **in_reqs, const arch_register_req_t **out_reqs,
						 const be_execution_unit_t ***execution_units,
						 int n_res, unsigned latency);

void ppc32_register_additional_opcodes(int opcode_num);

/* Include the generated headers */
#include "gen_ppc32_new_nodes.h"

#endif
