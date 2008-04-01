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
 * @brief       This file implements the IR transformation from firm into ia32-Firm with PBQP magic
 * @author      Sebastian Buchwald, Andreas Zwinkau
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_PBQP_TRANSFORM_H
#define FIRM_BE_IA32_IA32_PBQP_TRANSFORM_H

#include "firm_config.h"
#include "bearch_ia32_t.h"

/**
 * Transform firm nodes to x86 assembler nodes
 */
void ia32_transform_graph_by_pbqp(ia32_code_gen_t *cg);

#endif /* FIRM_BE_IA32_IA32_PBQP_TRANSFORM_H */
