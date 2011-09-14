/*
 * Copyright (C) 2011 University of Karlsruhe.  All right reserved.
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
 * @brief   Lower (stack-) Alloc nodes to allocate an aligned number of bytes
 * @author  Matthias Braun
 */
#ifndef FIRM_LOWER_ALLOC_H
#define FIRM_LOWER_ALLOC_H

#include <stdbool.h>
#include "firm_types.h"

/**
 * Lower Alloc/Free nodes: This changes them to allocate bytes instead of
 * objects of a certain type. It can also make sure that the resulting
 * size is aligned.
 */
void lower_alloc(ir_graph *irg, unsigned stack_alignment,
                 bool align_constant_sizes,
                 long addr_delta);

#endif
