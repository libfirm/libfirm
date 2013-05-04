/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Karlsruhe Institute of Technology.
 */

/**
 * @brief    Remove all Tuple nodes from ir graph
 * @author   Andreas Zwinkau
 */
#include "irnode_t.h"
#include "irgopt.h"
#include "irgmod.h"
#include "irgwalk.h"
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
