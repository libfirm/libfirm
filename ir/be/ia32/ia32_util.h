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
 * @brief       Contains implementation of some useful functions for ia32 backend.
 * @author      Christian Wuerdig, Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_UTIL_H
#define FIRM_BE_IA32_IA32_UTIL_H

#include "irnode.h"
#include "irmode.h"

/**
 * Returns the first Proj with given mode connected to irn.
 * @param irn  The irn
 * @param First proj with mode == mode or NULL if none found
 */
ir_node *ia32_get_proj_for_mode(const ir_node *irn, ir_mode *mode);

/**
 * Returns the first Proj with mode != mode_M connected to irn.
 * @param irn  The irn
 * @param First proj with mode != mode_M or NULL if none found
 */
ir_node *ia32_get_res_proj(const ir_node *irn);

#endif /* FIRM_BE_IA32_IA32_UTIL_H */
