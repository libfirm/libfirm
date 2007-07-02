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
 * @brief       Handling of ia32 specific firm opcodes.
 * @author      Christian Wuerdig
 * @version     $Id$
 *
 * This file implements the creation of the achitecture specific firm opcodes
 * and the coresponding node constructors for the ia32 assembler irg.
 */
#ifndef FIRM_BE_IA32_IA32_NEW_NODES_H
#define FIRM_BE_IA32_IA32_NEW_NODES_H

#include "firm_config.h"
#include "ia32_nodes_attr.h"

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
 * returns true if a node has x87 registers
 */
int ia32_has_x87_register(const ir_node *n);

/**
 * Returns the attributes of an ia32 node.
 */
ia32_attr_t *get_ia32_attr(ir_node *node);
const ia32_attr_t *get_ia32_attr_const(const ir_node *node);

ia32_x87_attr_t *get_ia32_x87_attr(ir_node *node);
const ia32_x87_attr_t *get_ia32_x87_attr_const(const ir_node *node);

const ia32_immediate_attr_t *get_ia32_immediate_attr_const(const ir_node *node);

/**
 * Gets the type of an ia32 node.
 */
ia32_op_type_t get_ia32_op_type(const ir_node *node);

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp);

/**
 * Gets the immediate op type of an ia32 node.
 */
ia32_immop_type_t get_ia32_immop_type(const ir_node *node);

/**
 * Sets the immediate op type of an ia32 node.
 */
void set_ia32_immop_type(ir_node *node, ia32_immop_type_t tp);

/**
 * Gets the supported addrmode of an ia32 node
 */
ia32_am_type_t get_ia32_am_support(const ir_node *node);

ia32_am_arity_t get_ia32_am_arity(const ir_node *node);

/**
 * Sets the supported addrmode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t am_tp,
                         ia32_am_arity_t am_arity);

/**
 * Gets the addrmode flavour of an ia32 node
 */
ia32_am_flavour_t get_ia32_am_flavour(const ir_node *node);

/**
 * Sets the addrmode flavour of an ia32 node
 */
void set_ia32_am_flavour(ir_node *node, ia32_am_flavour_t am_flavour);

/**
 * Gets the addressmode offset as long.
 */
int get_ia32_am_offs_int(const ir_node *node);

/**
 * Sets the addressmode offset
 */
void set_ia32_am_offs_int(ir_node *node, int offset);

void add_ia32_am_offs_int(ir_node *node, int offset);

/**
 * Returns the symconst entity associated to addrmode.
 */
ir_entity *get_ia32_am_sc(const ir_node *node);

/**
 * Sets the symconst entity associated to addrmode.
 */
void set_ia32_am_sc(ir_node *node, ir_entity *sc);

/**
 * Sets the sign bit for address mode symconst.
 */
void set_ia32_am_sc_sign(ir_node *node);

/**
 * Clears the sign bit for address mode symconst.
 */
void clear_ia32_am_sc_sign(ir_node *node);

/**
 * Returns the sign bit for address mode symconst.
 */
int is_ia32_am_sc_sign(const ir_node *node);

/**
 * Gets the addr mode const.
 */
int get_ia32_am_scale(const ir_node *node);

/**
 * Sets the const for addr mode.
 */
void set_ia32_am_scale(ir_node *node, int scale);

/**
 * Return the tarval of an immediate operation or NULL if none set
 */
tarval *get_ia32_Immop_tarval(const ir_node *node);

/**
 * Return the symconst entity of an immediate operation or NULL if none set
 */
ir_entity* get_ia32_Immop_symconst(const ir_node *node);

/**
 * Sets the attributes of an immediate operation to the specified tarval
 */
void set_ia32_Immop_tarval(ir_node *node, tarval *tv);

/**
 * Sets the attributes of an immediate operation to the specified SymConst
 */
void set_ia32_Immop_symconst(ir_node *node, ir_entity *entity);

/**
 * Sets the uses_frame flag.
 */
void set_ia32_use_frame(ir_node *node);

/**
 * Clears the uses_frame flag.
 */
void clear_ia32_use_frame(ir_node *node);

/**
 * Gets the uses_frame flag.
 */
int is_ia32_use_frame(const ir_node *node);

/**
 * Sets node to commutative.
 */
void set_ia32_commutative(ir_node *node);

/**
 * Sets node to non-commutative.
 */
void clear_ia32_commutative(ir_node *node);

/**
 * Checks if node is commutative.
 */
int is_ia32_commutative(const ir_node *node);

/**
 * Sets node emit_cl.
 */
void set_ia32_emit_cl(ir_node *node);

/**
 * Clears node emit_cl.
 */
void clear_ia32_emit_cl(ir_node *node);

/**
 * Checks if node is commutative.
 */
int is_ia32_emit_cl(const ir_node *node);

/**
 * Sets node got_lea.
 */
void set_ia32_got_lea(ir_node *node);

/**
 * Clears node got_lea.
 */
void clear_ia32_got_lea(ir_node *node);

/**
 * Checks if node got lea.
 */
int is_ia32_got_lea(const ir_node *node);

/**
 * Sets node needs_stackent
 */
void set_ia32_need_stackent(ir_node *node);

/**
 * Clears node needs_stackent
 */
void clear_ia32_need_stackent(ir_node *node);

/**
 * Checks if node needs a stackentity assigned
 */
int is_ia32_need_stackent(const ir_node *node);

/**
 * Gets the mode of the stored/loaded value (only set for Store/Load)
 */
ir_mode *get_ia32_ls_mode(const ir_node *node);

/**
 * Sets the mode of the stored/loaded value (only set for Store/Load)
 */
void set_ia32_ls_mode(ir_node *node, ir_mode *mode);

/**
 * Gets the frame entity assigned to this node;
 */
ir_entity *get_ia32_frame_ent(const ir_node *node);

/**
 * Sets the frame entity for this node;
 */
void set_ia32_frame_ent(ir_node *node, ir_entity *ent);

/**
 * Returns the argument register requirements of an ia32 node.
 */
const arch_register_req_t **get_ia32_in_req_all(const ir_node *node);

/**
 * Sets the argument register requirements of an ia32 node.
 */
void set_ia32_in_req_all(ir_node *node, const arch_register_req_t **reqs);

/**
 * Returns the result register requirements of an ia32 node.
 */
const arch_register_req_t **get_ia32_out_req_all(const ir_node *node);

/**
 * Sets the result register requirements of an ia32 node.
 */
void set_ia32_out_req_all(ir_node *node, const arch_register_req_t **reqs);

/**
 * Returns the argument register requirements of an ia32 node.
 */
const arch_register_req_t *get_ia32_in_req(const ir_node *node, int pos);

/**
 * Returns the result register requirements of an ia32 node.
 */
const arch_register_req_t *get_ia32_out_req(const ir_node *node, int pos);

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ia32_req_out(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Sets the IN register requirements at position pos.
 */
void set_ia32_req_in(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Returns the register flag of an ia32 node.
 */
arch_irn_flags_t get_ia32_flags(const ir_node *node);

/**
 * Sets the register flag of an ia32 node.
 */
void set_ia32_flags(ir_node *node, arch_irn_flags_t flags);

/**
 * Returns the result register slots of an ia32 node.
 */
const arch_register_t **get_ia32_slots(const ir_node *node);

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ia32_out_reg_name(const ir_node *node, int pos);

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ia32_out_regnr(const ir_node *node, int pos);

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ia32_out_reg(const ir_node *node, int pos);

/**
 * Returns the number of results.
 */
int get_ia32_n_res(const ir_node *node);

/**
 * Returns the flavour of an ia32 node,
 */
ia32_op_flavour_t get_ia32_flavour(const ir_node *node);

/**
 * Sets the flavour of an ia32 node to flavour_Div/Mod/DivMod/Mul/Mulh.
 */
void set_ia32_flavour(ir_node *node, ia32_op_flavour_t op_flav);

/**
 * Returns the projnum code.
 */
long get_ia32_pncode(const ir_node *node);

/**
 * Sets the projnum code
 */
void set_ia32_pncode(ir_node *node, long code);

/**
 * Gets the instruction latency.
 */
unsigned get_ia32_latency(const ir_node *node);

/**
 * Sets the instruction latency.
 */
void set_ia32_latency(ir_node *node, unsigned latency);


/**
 * Sets the flags for the n'th out.
 */
void set_ia32_out_flags(ir_node *node, arch_irn_flags_t flags, int pos);

/**
 * Gets the flags for the n'th out.
 */
arch_irn_flags_t get_ia32_out_flags(const ir_node *node, int pos);

/**
 * Get the list of available execution units.
 */
const be_execution_unit_t ***get_ia32_exec_units(const ir_node *node);

/**
 * Get the exception label attribute.
 */
unsigned get_ia32_exc_label(const ir_node *node);

/**
 * Set the exception label attribute.
 */
void set_ia32_exc_label(ir_node *node, unsigned flag);

#ifndef NDEBUG

/**
 * Returns the name of the original ir node.
 */
const char *get_ia32_orig_node(const ir_node *node);

/**
 * Sets the name of the original ir node.
 */
void set_ia32_orig_node(ir_node *node, const char *name);

#endif /* NDEBUG */

/******************************************************************************************************
 *                      _       _         _   _           __                  _   _
 *                     (_)     | |       | | | |         / _|                | | (_)
 *  ___ _ __   ___  ___ _  __ _| |   __ _| |_| |_ _ __  | |_ _   _ _ __   ___| |_ _  ___  _ __    ___
 * / __| '_ \ / _ \/ __| |/ _` | |  / _` | __| __| '__| |  _| | | | '_ \ / __| __| |/ _ \| '_ \  / __|
 * \__ \ |_) |  __/ (__| | (_| | | | (_| | |_| |_| |    | | | |_| | | | | (__| |_| | (_) | | | | \__ \
 * |___/ .__/ \___|\___|_|\__,_|_|  \__,_|\__|\__|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_| |___/
 *     | |
 *     |_|
 ******************************************************************************************************/

/**
 * Returns the ident of an entity
 * @param ent The entity
 * @return The ident of the entity
 */
ident *ia32_get_ent_ident(ir_entity *ent);

/**
 * Copy the attributes from Immop to an Immop
 */
void copy_ia32_Immop_attr(ir_node *node, ir_node *src);

/**
 * Returns the proj of the result value for nodes that have the usual
 * (res, Mem) result tuple
 */
ir_node *get_ia32_result_proj(const ir_node *node);

/**
 * Copy the attributes from a Const to an ia32_Const
 */
void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst);

void set_ia32_Const_tarval(ir_node *node, tarval *tv);

/**
 * Sets the AddrMode attribute
 * @param direction The "direction" of AM ('S' source or 'D' destination)
 */
void set_ia32_AddrMode(ir_node *node, char direction);

/**
 * Returns whether or not the node is an immediate operation with Const.
 */
int is_ia32_ImmConst(const ir_node *node);

/**
 * Returns whether or not the node is an immediate operation with SymConst.
 */
int is_ia32_ImmSymConst(const ir_node *node);

/**
 * Returns whether or not the node is an AddrModeS node.
 */
int is_ia32_AddrModeS(const ir_node *node);

/**
 * Returns whether or not the node is an AddrModeD node.
 */
int is_ia32_AddrModeD(const ir_node *node);

/**
 * Checks if node is a Load or fLoad.
 */
int is_ia32_Ld(const ir_node *node);

/**
 * Checks if node is a Store or fStore.
 */
int is_ia32_St(const ir_node *node);

/**
 * Checks if node is a Const or fConst.
 */
int is_ia32_Cnst(const ir_node *node);

/**
 * Swaps left/right input of a node (and adjusts pnc if needed)
 */
void ia32_swap_left_right(ir_node *node);

/**
 * Initializes the nodes attributes.
 */
void init_ia32_attributes(ir_node *node, arch_irn_flags_t flags,
                          const arch_register_req_t **in_reqs,
                          const arch_register_req_t **out_reqs,
                          const be_execution_unit_t ***execution_units,
                          int n_res, unsigned latency);

void init_ia32_x87_attributes(ir_node *node);
void init_ia32_asm_attributes(ir_node *node);
void init_ia32_immediate_attributes(ir_node *node, ir_entity *symconst,
                                    int symconst_sign, long offset);

/* Include the generated headers */
#include "gen_ia32_new_nodes.h"

#endif /* FIRM_BE_IA32_IA32_NEW_NODES_H */
