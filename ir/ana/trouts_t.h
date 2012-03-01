/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Reverse edges that reference types/entities.
 * @author   Goetz Lindenmaier
 * @date     29.10.2004
 */
#ifndef FIRM_ANA_TROUTS_T_H
#define FIRM_ANA_TROUTS_T_H

#include "trouts.h"

void add_type_cast(const ir_type *tp, ir_node *cast);
void add_type_pointertype_to(const ir_type *tp, ir_type *ptp);
void add_type_arraytype_of(const ir_type *tp, ir_type *atp);

#endif
