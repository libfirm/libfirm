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
	(void)env;
	if (!is_Proj(node))
		return;

	ir_node *pred = get_Proj_pred(node);
	if (!is_Tuple(pred))
		return;

	int      pn         = get_Proj_proj(node);
	ir_node *tuple_pred = get_Tuple_pred(pred, pn);
	exchange(node, tuple_pred);
}

void remove_tuples(ir_graph *irg)
{
	irg_walk_graph(irg, NULL, exchange_tuple_projs, NULL);

	/* remove Tuples only held by keep-alive edges */
	ir_node *end = get_irg_end(irg);
	for (int i = get_End_n_keepalives(end); i-- > 0; ) {
		ir_node *irn = get_End_keepalive(end, i);
		if (is_Tuple(irn))
			remove_End_n(end, i);
	}

	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES);
}
