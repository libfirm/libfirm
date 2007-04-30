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
 * @brief       Compute register pressure in loops.
 * @author      Christian Wuerdig
 * @date        20.02.2007
 * @version     $Id$
 */
#ifndef FIRM_BE_BELOOPANA_H
#define FIRM_BE_BELOOPANA_H

#include "irloop.h"

#include "bearch.h"
#include "beirg.h"

typedef struct _be_loopana_t be_loopana_t;

/**
 * Compute the register pressure for a class of all loops in the birg.
 * @param birg  The backend irg object
 * @param cls   The register class to compute the pressure for
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure_cls(be_irg_t *birg, const arch_register_class_t *cls);

/**
 * Compute the register pressure for all classes of all loops in the birg.
 * @param birg  The backend irg object
 * @return The loop analysis object.
 */
be_loopana_t *be_new_loop_pressure(be_irg_t *birg);

/**
 * Returns the computed register pressure for the given class and loop.
 * @return The pressure or INT_MAX if not found
 */
unsigned be_get_loop_pressure(be_loopana_t *loop_ana, const arch_register_class_t *cls, ir_loop *loop);

/**
 * Frees loop analysis object.
 */
void be_free_loop_pressure(be_loopana_t *loop_ana);

#endif /* FIRM_BE_BELOOPANA_H */
