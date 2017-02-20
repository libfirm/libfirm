/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Karlsruhe Institute of Technology.
 */

/**
 * @brief    Remove all Tuple nodes from ir graph
 * @author   Andreas Zwinkau
 */
#include "irgmod.h"
#include "irgopt.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "irnode_t.h"

/** Transforms:
 *    a
 *    |
 *   Tuple
 *    |        =>
 *   Proj x          a
 */
static void exchange_tuple_projs(ir_node *node, void *env)
{
	bool *changed = (bool*)env;
	if (!is_Proj(node))
		return;

	/* Handle Tuple(Tuple,...) case. */
	exchange_tuple_projs(get_Proj_pred(node), env);

	ir_node *pred = get_Proj_pred(node);
	if (!is_Tuple(pred))
		return;

	unsigned pn         = get_Proj_num(node);
	ir_node *tuple_pred = get_Tuple_pred(pred, pn);
	exchange(node, tuple_pred);
	*changed = true;
}

void remove_tuples(ir_graph *irg)
{
	bool changed = false;
	irg_walk_graph(irg, exchange_tuple_projs, NULL, &changed);

	/* remove Tuples only held by keep-alive edges */
	ir_node *end = get_irg_end(irg);
	for (int i = get_End_n_keepalives(end); i-- > 0; ) {
		ir_node *irn = get_End_keepalive(end, i);
		if (is_Tuple(irn)) {
			remove_End_n(end, i);
			changed = true;
		}
	}

	confirm_irg_properties(irg, changed
	                       ? IR_GRAPH_PROPERTIES_CONTROL_FLOW | IR_GRAPH_PROPERTY_ONE_RETURN
	                         | IR_GRAPH_PROPERTY_MANY_RETURNS | IR_GRAPH_PROPERTY_NO_BADS
	                       : IR_GRAPH_PROPERTIES_ALL);
	add_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES);
}
