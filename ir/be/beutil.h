
#ifndef _BEUTIL_H
#define _BEUTIL_H

#include <stdio.h>

#include "irnode.h"
#include "config.h"

/** Undefine this to disable debugging mode. */
#define BE_DEBUG 1

/**
 * Check, if a node produces or consumes a data value.
 * If it does, it is significant for scheduling and register allocation.
 * A node produces/consumes a data value, if one of its operands is of
 * mode datab, or his retuning mode is of mode datab.
 * @param irn The node to check for.
 * @return 1, if the node is a data node, 0 if not.
 */
static INLINE int is_data_node(const ir_node *irn)
{
	int i, n;

	/* If the node produces a data value, return immediately. */
	if(mode_is_datab(get_irn_mode(irn)))
		return 1;

	/* else check, if it takes a data value, if that is so, return */
	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);
		if(mode_is_datab(get_irn_mode(op)))
			return 1;
	}

	/* Else the node does not produce/consume a data value */
	return 0;
}


void dump_allocated_irg(ir_graph *irg);

#endif
