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
 * @brief       Base routines for register allocation.
 * @author      Sebastian Hack
 * @date        13.01.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BERA_H
#define FIRM_BE_BERA_H

#include "irnode.h"

#include "belive.h"
#include "beirg.h"

typedef struct be_ra_t {
	void (*allocate)(be_irg_t *bi);   /**< allocate registers on a graph */
} be_ra_t;

void be_register_allocator(const char *name, be_ra_t *allocator);

/**
 * Do register allocation with currently selected register allocator
 */
void be_allocate_registers(be_irg_t *birg);

int (values_interfere)(const be_irg_t *birg, const ir_node *a, const ir_node *b);

#endif /* FIRM_BE_BERA_H */
