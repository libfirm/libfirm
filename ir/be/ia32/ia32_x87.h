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
 * This file implements the x87 support and virtual to stack
 * register translation.
 *
 * $Id$
 */
#ifndef _IA32_X87_H_
#define _IA32_X87_H_

#include "../bearch_t.h"

/**
 * Run a simulation and fix all virtual instructions for a graph.
 * Replaces all virtual floating point instructions and registers
 * by real ones.
 *
 * @param env       architecture environment
 * @param birg      the graph to simulate and patch
 *
 * Registers must be allocated.		Needs a block-schedule.
 */
void x87_simulate_graph(const arch_env_t *env, be_irg_t *birg);

#endif /* _IA32_X87_H_ */
