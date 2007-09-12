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
 * @brief       modifies schedule so flags dependencies are respected.
 * @author      Matthias Braun, Christoph Mallon
 * @version     $Id: besched.h 14693 2007-06-21 15:35:49Z beck $
 */
#ifndef FIRM_BE_BEFLAGS_H
#define FIRM_BE_BEFLAGS_H

#include "bearch.h"
#include "beirg.h"

typedef ir_node * (*func_rematerialize) (ir_node *node, ir_node *after);

/**
 * Walks the schedule and ensures that flags aren't destroyed between producer
 * and consumer of flags. It does so by moving down/rematerialising of the
 * nodes. This does not work across blocks.
 */
void be_sched_fix_flags(be_irg_t *birg, const arch_register_class_t *flag_cls,
                        func_rematerialize remat_func);

#endif
