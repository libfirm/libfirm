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
 */
#ifndef FIRM_BE_BERA_H
#define FIRM_BE_BERA_H

#include "firm_types.h"

typedef struct be_ra_t {
	void (*allocate)(ir_graph *irg);   /**< allocate registers on a graph */
} be_ra_t;

void be_register_allocator(const char *name, be_ra_t *allocator);

/**
 * Do register allocation with currently selected register allocator
 */
void be_allocate_registers(ir_graph *irg);

#endif
