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
 * @brief    Remove all Tuple nodes from ir graph
 * @author   Andreas Zwinkau
 */
#include "config.h"

#include "irnode_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irtools.h"
#include "irgopt.h"

/** Transforms:
 *    a
 *    |
 *   Tuple
 *    |        =>
 *   Proj x          a
 */
static void exchange_tuple_projs(ir_node *node, void *env)
{
	ir_node *pred;
	int proj;
	(void)env;

	if (!is_Proj(node)) return;

	pred = get_Proj_pred(node);
	proj = get_Proj_proj(node);

	if (!is_Tuple(pred)) return;

	pred = get_Tuple_pred(pred, proj);
	exchange(node, pred);
}

void remove_tuples(ir_graph *irg)
{
	irg_walk_graph(irg, exchange_tuple_projs, NULL, NULL);

	ir_node *end          = get_irg_end(irg);
	int      n_keepalives = get_End_n_keepalives(end);
	int      i;

	for (i = n_keepalives - 1; i >= 0; --i) {
		ir_node *irn = get_End_keepalive(end, i);

		if (is_Tuple(irn)) {
			remove_End_keepalive(end, irn);
		}
	}

	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES);
}
