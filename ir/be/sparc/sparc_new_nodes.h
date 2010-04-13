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
 * @version $Id$
 */
#ifndef FIRM_BE_SPARC_SPARC_NEW_NODES_H
#define FIRM_BE_SPARC_SPARC_NEW_NODES_H

#include "sparc_nodes_attr.h"

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
 * Returns the attributes of an sparc node.
 */
sparc_attr_t *get_sparc_attr(ir_node *node);
const sparc_attr_t *get_sparc_attr_const(const ir_node *node);

sparc_load_store_attr_t *get_sparc_load_store_attr(ir_node *node);
const sparc_load_store_attr_t *get_sparc_load_store_attr_const(const ir_node *node);

sparc_symconst_attr_t *get_sparc_symconst_attr(ir_node *node);
const sparc_symconst_attr_t *get_sparc_symconst_attr_const(const ir_node *node);

sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr(ir_node *node);
const sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr_const(const ir_node *node);

sparc_jmp_switch_attr_t *get_sparc_jmp_switch_attr(ir_node *node);
const sparc_jmp_switch_attr_t *get_sparc_jmp_switch_attr_const(const ir_node *node);

sparc_cmp_attr_t *get_sparc_cmp_attr(ir_node *node);
const sparc_cmp_attr_t *get_sparc_cmp_attr_const(const ir_node *node);

sparc_save_attr_t *get_sparc_save_attr(ir_node *node);
const sparc_save_attr_t *get_sparc_save_attr_const(const ir_node *node);


/**
 * Returns the argument register requirements of an sparc node.
 */
const arch_register_req_t **get_sparc_in_req_all(const ir_node *node);

/**
 * Returns the argument register requirements of an sparc node.
 */
const arch_register_req_t *get_sparc_in_req(const ir_node *node, int pos);

/**
 * Sets the IN register requirements at position pos.
 */
void set_sparc_req_in(ir_node *node, const arch_register_req_t *req, int pos);


/**
 * Returns the proj num
 */
int get_sparc_jmp_cond_proj_num(const ir_node *node);

/**
 * Sets the proj num
 */
void set_sparc_jmp_cond_proj_num(ir_node *node, int proj_num);


/**
 * Returns the number of projs of a SwitchJmp.
 */
int get_sparc_jmp_switch_n_projs(const ir_node *node);

/**
 * Sets the number of projs of a SwitchJmp.
 */
void set_sparc_jmp_switch_n_projs(ir_node *node, int n_projs);

/**
 * Returns the default_proj_num.
 */
long get_sparc_jmp_switch_default_proj_num(const ir_node *node);

/**
 * Sets the default_proj_num.
 */
void set_sparc_jmp_switch_default_proj_num(ir_node *node, long default_proj_num);

/* Include the generated headers */
#include "gen_sparc_new_nodes.h"

#endif
