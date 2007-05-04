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
 * @brief       Implements several optimizations for IA32.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_OPTIMIZE_H
#define FIRM_BE_IA32_IA32_OPTIMIZE_H

#include "irgraph.h"
#include "bearch_ia32_t.h"

/**
 * Prepares irg for codegeneration. Places consts and transform reference mode
 * nodes into mode_Iu nodes.
 * @param cg  The ia32 codegenerator object
 */
void ia32_pre_transform_phase(ia32_code_gen_t *cg);

/**
 * Performs conv and address mode optimizations.
 * @param cg  The ia32 codegenerator object
 */
void ia32_optimize_graph(ia32_code_gen_t *cg);

/**
 * Performs Peephole Optimizations
 */
void ia32_peephole_optimization(ir_graph *irg, ia32_code_gen_t *cg);

#endif /* FIRM_BE_IA32_IA32_OPTIMIZE_H */
