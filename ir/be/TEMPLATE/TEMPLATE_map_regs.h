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
 * @brief   declarations for the register allocation interface
 * @version $Id$
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_MAP_REGS_H
#define FIRM_BE_TEMPLATE_TEMPLATE_MAP_REGS_H

#include "irnode.h"
#include "set.h"

#include "../bearch_t.h"
#include "TEMPLATE_nodes_attr.h"

int  TEMPLATE_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);
void TEMPLATE_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);
const arch_register_t *TEMPLATE_get_firm_reg(const ir_node *irn, set *reg_set);

long TEMPLATE_translate_proj_pos(const ir_node *proj);

#endif
