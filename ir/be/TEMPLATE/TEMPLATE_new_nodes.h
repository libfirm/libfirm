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
#ifndef FIRM_BE_TEMPALTE_TEMPLATE_NEW_NODES_H
#define FIRM_BE_TEMPLATE_TEMPLATE_NEW_NODES_H

#include "TEMPLATE_nodes_attr.h"

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
 * Returns the attributes of an TEMPLATE node.
 */
TEMPLATE_attr_t *get_TEMPLATE_attr(ir_node *node);

const TEMPLATE_attr_t *get_TEMPLATE_attr_const(const ir_node *node);

/**
 * Returns the argument register requirements of an TEMPLATE node.
 */
const arch_register_req_t **get_TEMPLATE_in_req_all(const ir_node *node);

/**
 * Returns the result register requirements of an TEMPLATE node.
 */
const arch_register_req_t **get_TEMPLATE_out_req_all(const ir_node *node);

/**
 * Returns the argument register requirements of an TEMPLATE node.
 */
const arch_register_req_t *get_TEMPLATE_in_req(const ir_node *node, int pos);

/**
 * Returns the result register requirements of an TEMPLATE node.
 */
const arch_register_req_t *get_TEMPLATE_out_req(const ir_node *node, int pos);

/**
 * Sets the OUT register requirements at position pos.
 */
void set_TEMPLATE_req_out(ir_node *node, const arch_register_req_t *req, int pos);

/**
 * Sets the IN register requirements at position pos.
 */
void set_TEMPLATE_req_in(ir_node *node, const arch_register_req_t *req, int pos);

/* Include the generated headers */
#include "gen_TEMPLATE_new_nodes.h"

#endif
