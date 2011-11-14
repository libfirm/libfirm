/*
 * Copyright (C) 2011 Karlsruhe Institute of Technology.  All right reserved.
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
 * @brief   Optimization wrapper for specifying requirements
 * @author  Andreas Zwinkau
 */
#ifndef FIRM_OPT_MANAGE_H
#define FIRM_OPT_MANAGE_H

#include "config.h"

#include <stdbool.h>

#include "irgraph_t.h"

typedef struct optdesc_t {
	/**
	 * The short name of the optimization
	 *
	 * Should not contain spaces, since it is used for the dumper filenames.
	 */
	const char * const name;

	/**
	 * required irg_state for this optimization
	 */
	ir_graph_state_t requirements;

	/**
	 * The optimization function itself
	 *
	 * @returns  zero by default; set some flags, if you guarantee some irg_state properties
	 **/
	ir_graph_state_t (*const optimization)(ir_graph *irg);
} optdesc_t;

/** Apply an optimization to an ir graph.
 *  Assures preconditions, invalidates afterwards, and runs the verifier.
 *  Might also dump the irg.
 */
void perform_irg_optimization(ir_graph *irg, optdesc_t *opt);

#endif
