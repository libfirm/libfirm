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
 * @file   bearch_firm.h
 * @date   11.05.2005
 * @author Sebastian Hack
 * @brief  An instruction set architecture made up of firm nodes.
 */
#ifndef FIRM_BE_BEARCH_FIRM_H
#define FIRM_BE_BEARCH_FIRM_H

#include "../bearch_t.h"

extern const arch_isa_if_t firm_isa;
extern const arch_irn_handler_t firm_irn_handler;

/* TODO UGLY*/
int is_Imm(const ir_node *irn);

#endif
