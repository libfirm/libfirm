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
 * @brief   Handles fpu rounding modes
 * @author  Matthias Braun
 * @version $Id$
 */
#ifndef FIRM_BE_IA32_IA32_FPU_H
#define FIRM_BE_IA32_IA32_FPU_H

#include "bearch_ia32_t.h"

/**
 * Handle switching of fpu mode
 */
void ia32_setup_fpu_mode(ia32_code_gen_t *cg);

#endif /* FIRM_BE_IA32_IA32_FPU_H */
