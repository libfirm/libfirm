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
 * @version $Id: amd64_new_nodes.h 26549 2009-09-18 17:48:23Z matze $
 */
#ifndef FIRM_BE_TEMPALTE_amd64_NEW_NODES_H
#define FIRM_BE_amd64_amd64_NEW_NODES_H

#include "amd64_nodes_attr.h"

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
 * Sets the input mode of the node.
 */
void set_amd64_ls_mode(ir_node *n, ir_mode *mode);

/**
 * Returns the attributes of an amd64 node.
 */
amd64_attr_t *get_amd64_attr(ir_node *node);

const amd64_attr_t *get_amd64_attr_const(const ir_node *node);
const amd64_SymConst_attr_t *get_amd64_SymConst_attr_const(const ir_node *node);

/* Include the generated headers */
#include "gen_amd64_new_nodes.h"

#endif
