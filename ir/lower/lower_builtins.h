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
 * @brief   Lowering of builtins to compiler-lib calls
 * @author  Matthias Braun
 */
#ifndef FIRM_LOWER_BUILTINS_H
#define FIRM_LOWER_BUILTINS_H

#include "firm_types.h"
#include <stddef.h>

void lower_builtins(size_t n_exceptions, ir_builtin_kind *exceptions);

#endif
