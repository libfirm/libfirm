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
 */
#ifndef FIRM_BE_TEMPALTE_TEMPLATE_NEW_NODES_H
#define FIRM_BE_TEMPLATE_TEMPLATE_NEW_NODES_H

#include "TEMPLATE_nodes_attr.h"

/**
 * Returns the attributes of an TEMPLATE node.
 */
TEMPLATE_attr_t *get_TEMPLATE_attr(ir_node *node);

const TEMPLATE_attr_t *get_TEMPLATE_attr_const(const ir_node *node);

/* Include the generated headers */
#include "gen_TEMPLATE_new_nodes.h"

#endif
