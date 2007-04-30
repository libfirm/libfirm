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
 * @brief   This file implements functions to finalize the irg for emit.
 * @author  Christian Wuerdig
 * @version $Id$
 */
#ifndef FIRM_BE_IA32_IA32_FINISH_H
#define FIRM_BE_IA32_IA32_FINISH_H

#include "irgraph.h"
#include "bearch_ia32_t.h"

/**
 * Check 2-Addresscode constraints and call peephole optimizations.
 * @param irg  The irg to finish
 * @param cg   The codegenerator object for the irg
 */
void ia32_finish_irg(ir_graph *irg, ia32_code_gen_t *cg);

#endif /* FIRM_BE_IA32_IA32_FINISH_H */
